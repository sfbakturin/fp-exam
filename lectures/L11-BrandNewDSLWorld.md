# Разверзнись, новый DSL мир

**Main Specific Language** - это определенный набор команд функций и тех или иных функциональностей, которые позволяют работать с определенной доменной областью. Грубо говоря, это задачи, которые решают те или иные языки программирования: **GSL** (*general specific language*) - обычно про Python/Java/C++ - те языки программирования, которые умеют решать множество задач, а **DSL** - про Verilog, например.

## GADT

Попробуем привести пример, где мы бы хотели заиметь что-то новое от Haskell в плане типизации. Хотим сделать объекты для математических выражений (сложение, оператор-БОЛЬШЕ и логический-И). Воспользуемся старым добрым Haskell'ким ADT.

```haskell
data ArithExpr =
    AENum Int
  | AEPlus ArithExpr ArithExpr
  | AEAnd ArithExpr ArithExpr
  | AEGt ArithExpr ArithExpr
```

И создадим пример-выражение.

```haskell
-- (23 + 12) > 170 && (35 > 47)
myExpr =
  ((AENum 23 `AEPlus` AENum 12) `AEGt` AENum 170)
  `AEAnd` (AENum 35 `AEGt` AENum 47)
```

В чем может быть проблема? В том, что мы можем подать в качестве выражения в импровизированную функцию `eval` выражение вида `12 && 23`.

```haskell
badExpr = AENum 12 `AEAnd` AENum 23
```

Для решения этой проблемы используется другой (заместо ADT) механизм типов - **GADT** - *Generalized algebraic data types*. В чём именно проблема? Заметим, что `AENum` не хранит информацию о том, что внутри него лежит какое-то число.

```haskell
ghci> :t AENum
AENum  :: Int -> ArithExpr
-- что там дальше в `ArithExpr` мы тоже не знаем
```

Синтаксис интересуемого GADT выглядит следующим образом - он ничем не отличается от развёрнутого синтаксиса `data`, поэтому введём сразу типовой аргумент:

```haskell
-- `a` - типовой аргумент
data ArithExpr a where
  AENum  :: Int -> ArithExpr
  AEPlus :: ArithExpr Int -> ArithExpr Int -> ArithExpr Int
  -- нас появился типовой параметр, пропишем его везде, где он возможен
  -- в данном случае: мы хотим складывать Int'ы
  AEAnd  :: ArithExpr Bool -> ArithExpr Bool -> ArithExpr Bool
  -- в данном случае: мы хотим использовать только на Bool'ах
  AEGt   :: ArithExpr Int -> ArithExpr Int -> ArithExpr Bool
  -- в данном случае: мы принимаем Int'ы, но возвращаем Bool
```

Теперь, если мы хотим что-то посчитать, нужно протягивать типы. Зато, мы сможем избавиться от необходимости проверять на правильность типов `Int`/`Int`, `Bool`/`Bool`. Что ещё хорошего? То, что было верно - вообще никак не меняется, а то, что не было - будет считаться неверным выражением. Причем, происходит это на этапе компиляции - типы не совпадут.

Однако, у этой штуки есть явная проблема: а именно парсинг. Мы помним, что [парсер имеет полиморфный тип](L5-Parsers.md), а тут - GADT - нам нужен явный возвращаемый тип. Причем, если вы пытаетесь вывести тип, а Haskell не поймёт выльется интересная ошибка следующего вида: вы подали тип `Int`, например, а ожидался вообще-то `a`.

```haskell
parse
  :: String -> Maybe (ArithExpr a)
parse "1" = Just (AENum 1)
parse _ = Nothing
-- казалось бы, мы хотим, чтобы тип был `Int`...

ghci> :l DSL.hs
error:
    • Couldn't match type ‘a’ with ‘Int’
      Expected type: Maybe (ArithExpr a)
        Actual type: Maybe (ArithExpr Int)
```

Обратим внимание, что тип `parse` функции говорит от том, что для каждого типа `a` существует вызов `parse`, который вернёт `ArithExpr a`. Но на самом деле мы хотим сказать, что *есть некоторая верная строка*, которая бы спарсилась в `ArithExpr`...

## Экзистенциальные типы

В продолжение к предыдущей проблемой мы вводим так называемый *экзистенциальный тип* `SomeAE`, который выглядит следующим образом.

```haskell
data SomeAE where
  SomeAE :: Show a => ArithExpr a -> SomeAE
```

Что здесь такого особенного? Мы помещаем объект `a` внутри `SomeAE` и требуем от него (от типа `a`), чтобы тот имел constraint от `Show` (была возможность воспользоваться `toString`-аналогом). Однако, тогда, мы получаем другую проблему: определив таким образом тип, мы вообще никаким образом не сможем интерактироваться с внутренним миром - есть только `show` (так как тип `a` от `Show`). А как мы избавляемся от проблемы? Во первых, оно компилируется, во вторых, теперь мы сами в функции берём *полный контроль* над типами.

```haskell
parse :: String -> Maybe SomeAE
parse "1" = Just (SomeAE $ AENum 1)
parse "1+2" = Just $ SomeAE $
  AENum 1 `AEPlus` AENum 2
parse _ = Nothing

interpretShow :: SomeAE -> String
interpretShow (SomeAE expr) =
  show (interpret expr)

-- пример использования в GHCi
ghci> interpretShow <$> parse "1+2"
Just "3"
```

Альтернативный вариант грамматики такого типа - использования прагмы, включающий в себя возможность писать `forall`.

```haskell
{-# LANGUAGE ExistentialQuantification #-}

data SomeAE =
  forall a. Show a => SomeAE (ArithExpr a)
-- классификатор `forall` - что-то вроде лямбда-абстракция на типах
```

Возвращаясь к вопросу, как нам получить что-то внутри завёрнутое? Есть такой класс `Typeable`, он позволяет работать с типами на уровне runtime (как reflection в Java), в том числе - проверить, равны ли типы через функцию `eqT`.

```haskell
eqT
  :: forall a b. (Typeable a, Typeable b)
  => Maybe (a :~: b)
-- возвращает `Just <что-то>`, если типы равны, иначе - `Nothing`
```

Тогда, мы можем применить её, подобно [`SomeException`](L6-RealWorld.md#исключительные-ситуации) будем перебирать все возможные варианты.

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

parseInt
  :: String -> Maybe (ArithExpr Int)
parseInt s = parse s >>=
  \(SomeAE (expr :: ArithExpr t)) ->
    do
      Refl <- eqT @t @Int
      -- если `Refl` вернётся как `Nothing`, то всё вычисление вернёт `Nothing`
      pure expr
      -- если типы `t` и `Int` совпали, значит, можем протаскивать дальше
      -- выражение с известным типом и считать его
```

Пример использования.

```haskell
ghci> let aE = parseInt "1+2"
ghci> aE
Just 1 + 2

ghci> (^3) . interpret <$> aE
Just 27
```

### `forall`

Почему `forall`, а не `exists`? Потому что, из `forall` мы можем легко вывести этот `exists`: $\forall x.\phi\to\psi\implies\exists x.\phi\to\psi$. А что под капотом? Давайте посмотрим на `length`.

```haskell
length :: [a] -> Int
-- мы утверждаем, что нам будет все равно, какой тип у `a`, так как для этой
-- функции в целом вообще неважно
```

В компиляторе, если компилируется, обычно превращается в то, что мы ожидаем в целом.

```haskell
length :: forall a . [a] -> Int
-- "для каждого типа `a` данная функция может иметь тип `[a] -> Int`"
```

Но когда это - плохо? Когда компилятору невозможно догадаться, ошиблись вы или это была идея.

```haskell
applyToTuple :: ([a] -> Int) -> ([b], [c]) -> (Int, Int)
applyToTuple f x y = (f x, f y)
-- компилятор ругается вполне справедливо: вы хотим `[a]` на вход, а получаем
-- `[b]` или `[c]` на вход - функция будет рассмотрена до вызова её в контексте
```

Для исправления - нам нужно поставить `forall` в **нужном** месте.

```haskell
{-# LANGUAGE RankNTypes #-}

applyToTuple :: (forall a. [a] -> Int) -> ([b], [c]) -> (Int, Int)
applyToTuple f (x, y) = (f x, f y)

-- можем использовать
ghci> applyToTuple length ("hello", [1,2,3])
(5, 3)
```

#### Rank Types

То, что мы описали выше - имеет, так называемый, ранг типа 2. А что это такое? Тип функции ранг 0 - это та, в которой вообще нету полиморфных неизвестных типов; ранга 1 - имеет тип вида: `forall a . a -> Int`; ранга 2 - `(forall a . a -> Int) -> Int`; и, наконец, ранга 3 (может достигать до $n$) - `((forall a . a -> Int) -> Int) -> Int`.

**Ранг типов** показывает глубину, на которой `forall`-классификатор стоит в *контравариантной* позиции, то есть, левее от функциональной стрелки. Функция имеет ранг $n + 1$, когда его аргумент имеет ранг $n$.

Примеры.

```haskell
Int -> Int
-- ранг 0
```

```haskell
forall a . a -> a
-- ранг 1
```

```haskell
(forall a . a -> a) -> Int
-- ранг 2
```

```haskell
forall a b . a -> b -> a
-- ранг 1
```

```haskell
Int -> (forall a . a -> a)
-- ранг 1, потому что мы можем преобразовать его в
forall a . Int -> a -> a
```

```haskell
forall a . a -> (forall b . b -> a)
-- ранг 1, потому что мы можем преобразовать его в
forall a b . a -> b -> a
```

```haskell
(a -> a) -> (forall b . b -> b) -> (c -> c)
-- ранг 2
```

Продолжая разбираться с `forall`, мы можем также запихнуть его в типы данных, если вдруг это понадобится - делаем также и получаем ровно такую же работоспособность, причём, мы можем спокойно навешивать контекст на поданный `m`.

```haskell
data Ctx = Ctx { modulus :: Int }

newtype Action a = Action
  { runAction
      :: forall m .
        (MonadReader Ctx m, MonadCatch m)
      => m a }
```

```haskell
expOrDefault
  :: Int -> Int -> Int -> Action Int
expOrDefault base pow def = Action $ do
  m <- asks modulus
  let r = base ^ pow
  (r `seq` pure (r `mod` m))
    `catchAny` \_ -> pure def

runPrint = runAction >=> print

main :: IO ()
main = flip runReaderT (Ctx 17) $ do
  runPrint $ expOrDefault 2 5  (-100)
  runPrint $ expOrDefault 2 (-1) (-100)
```

**Важно**: не следует путать с **экзистенциальными типами**, посмотрим на пример и увидим разницу.

```haskell
data Action a = 
 forall m .
  (MonadReader Ctx m, MonadCatch m)
 => Action (m a)
-- здесь `m` вынесен ЗА ПРЕДЕЛЫ конструктора в отличии от вышеописанного варианта
-- то есть, теперь МЫ УПРАВЛЯЕМ типом извне, а не внутри функций
```

## Final Tagless

Возвращаясь к теме о GADT, давайте немного поменяем наш тип данных `data ArithExpr`, которы выглядел следующим образом.

```haskell
data ArithExpr a where
  AENum  :: Int -> ArithExpr Int
  AEPlus :: ArithExpr Int -> ArithExpr Int -> ArithExpr Int
  AEAnd  :: ArithExpr Bool -> ArithExpr Bool -> ArithExpr Bool
  AEGt   :: ArithExpr Int -> ArithExpr Int -> ArithExpr Bool

-- пример использования - составление выражения
myExpr = ((AENum 23 `AEPlus` AENum 12) `AEGt` AENum 170)
           `AEAnd` (AENum 35 `AEGt` AENum 47)
```

На такую технику *final tagless* - теперь у нас тип данных превращается type class (для последующих instance'ов) и заменим `ArithExpr` на типовой параметр `expr`.

```haskell
class ArithExpr expr where
  aeNum  :: Int -> expr Int
  aePlus :: expr Int -> expr Int -> expr Int
  aeAnd  :: expr Bool -> expr Bool -> expr Bool
  aeGt   :: expr Int -> expr Int -> expr Bool

-- пример использования - составление выражения
myExpr :: ArithExpr expr => expr Bool
myExpr = ((aeNum 23 `aePlus` aeNum 12) `aeGt` aeNum 170)
           `aeAnd` (aeNum 35 `aeGt` aeNum 47)
```

Для чего это нужно? Теперь мы можем менять *то*, над чем мы *проводим вычисления* - то есть, был `expr`, а в каком-то месте мы сказали, что это некий тип `A` - у которого есть какие-то методы для работы над ним с доступными данными внутри `expr`.

Например, `toString` можно задать через тип `ToS`.

```haskell
newtype ToS a = ToS { toString :: String }
  deriving (Show, Semigroup)

castTS :: ToS a -> ToS b
castTS (ToS s) = ToS s
-- каст, который явным образом говорит, что мы хотим другой тип
-- разворачиваем и заворачиваем обратно в `ToS`

instance ArithExpr ToS where
  aeNum = ToS . show
  -- число перевели в строку и завернули в `ToS`
  aePlus a b = a <> (ToS " + ") <> b
  aeAnd a b = a <> (ToS " && ") <> b
  aeGt a b =
    castTS a <> (ToS " > ") <> castTS b
  -- здесь на приходится делать `castTS`, так как
  -- возвращаемое значение - это `ToS Bool`, а не `ToS Int`
```

Для вычисления значений мы можем также задать тип, с функцией `interpret :: a` (где `a` - возвращаемое значение).

```haskell
newtype Interpret a =
  Interpret { interpret :: a }

instance ArithExpr Interpret where
  aeNum = Interpret
  aePlus a b = Interpret $
    interpret a + interpret b
  aeAnd a b = Interpret $
    interpret a && interpret b
  aeGt a b = Interpret $
    interpret a > interpret b
```

Примеры использования.

```haskell
myExpr :: ArithExpr expr => expr Bool
myExpr = ((aeNum 23 `aePlus` aeNum 12)
           `aeGt` aeNum 170) `aeAnd`
           (aeNum 35 `aeGt` aeNum 47)
```

```haskell
ghci> toString myExpr
"23 + 12 > 170 && 35 > 47"
-- вывод строки через `ToS`

ghci> interpret myExpr
False
-- вычисление значения через `Interpret`
```
