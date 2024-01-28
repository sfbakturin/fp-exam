# Монады

## Введение в монады

Поймем, что такое монада. Мы можем точно сказать, что **переменная** (или: константа) - это контейнер для *данных*, **монада** же - контейнер для *вычислений*. Своего рода, идея монады в том, что мы хотим комбинировать разные вычисления таким образом, что последующие вычисления будут зависеть от предыдущих - как бы "изменяются". В чем вообще особенность монады от `Functor`, `Applicative`? Мы знаем, что `Functor` работает с некоторыми объектами, которые завернуты в контекст, мы меняем внутренность с помощью функции и возвращаем тот же контекст, но с новым значением внутри. А `Applicative` ещё более узконаправленный: теперь и функция отображения `a -> b` завернут в некотором контексте.

```haskell
(<$>) :: Functor t     => (a -> b) -> t a -> t b
(<*>) :: Applicative t => t (a -> b) -> t a -> t b
```

А теперь перейдем к ещё одному частному случаю: скажем, что мы все ещё будем работать с объектами, завернутые в контекст, но теперь наша функция будет возвращать контекст `b` и принимать тип `a`.

```haskell
????  :: Monad t       => (a -> t b) -> t a -> t b
```

Утверждается, что несмотря на тип `a` вне контекста, `Monad` всё равно будет более частным случаем, чем `Applicative`.

Теперь рассмотрим его в техническом плане. В Haskell `Monad` type class - это подкласс `Applicative`, у него есть две основные функции.

```haskell
class (Applicative m) => Monad m where
  -- заметим, что родом `m` всё ещё является `Type -> Type`

  return :: a -> m a
  -- заметим, что `return` подозрительно похож на `pure`... и это
  -- действительно так, если мы не хотим прописывать новый `return`,
  -- то будет правильно просто сказать, что `return = pure`
  (>>=)  :: m a -> (a -> m b) -> m b
  -- та самая функция подмены значения `a` в контекст `b`, мы
  -- именуем её обычно "bind"
```

### Monad Maybe

Рассмотрим в качестве примера монаду от `Maybe` - как самый простой способ описать контекст.

```haskell
instance Monad Maybe where
  return :: a -> Maybe a
  return = Just
  -- ничего не меняется, мы оборачиваем значение в контекст

  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= _ = Nothing
  -- нам подали пустое значение `Nothing`, значения типа `a` не существует,
  -- значит, мы не можем применить функцию для получения нового контекста,
  -- а значит, мы возвращаем пустоту
  Just a >>= f = f a
  -- нам подали непустое значение `Just`, есть значение типа `a`,
  -- значит, мы можем применить функцию замены и обёртки в контекст
  -- и, тем самым, вернуть контекст `b`
```

Некоторые примеры использования.

```haskell
ghci> Just 5 >>= (\x -> Just (x + 3))
Just 8
```

```haskell
ghci> Just 5 >>= (\x -> return (x + 3))
Just 8
-- данный пример эквивалентен предыдущему, так как в `Monad Maybe`
-- `return` определён как обёртка значения в `Just`
```

```haskell
ghci> Nothing >>= (\x -> return (x + 3))
Nothing
-- схожее поведение с `Applicative`
```

```haskell
ghci> Just 3 >>= \x -> Just 4 >>= \y -> Just (x + y)
Just 7
-- давайте расставим скобки: лямбда сжирает всё, что справа, поэтому
ghci> Just 3 >>= (\x -> (Just 4 >>= \y -> Just (x + y)))
Just 7
-- будет эквивалентно и мы видим, что `\x -> (Just 4 >>= \y -> Just (x + y))`
-- это в точности `Int -> Maybe Int`
-- тоже самое проделаем с внутренней частью
ghci> Just 3 >>= (\x -> (Just 4 >>= (\y -> (Just (x + y)))))
Just 7
-- опять же будет эквивалентно и убеждаемся, что `\y -> (Just (x + y))`
-- это тоже в точности `Int -> Maybe Int`, наконец, после редуцирования
-- мы получаем `Just (3 + 4)`
-- это мы можем переписать через известные нам способы: `(<*>)`, `fmap`
```

А теперь давайте создадим функцию, которая бы принимала два типа `Maybe` и возвращала был `Maybe` от пары их двух значений.

1. Первая имплементация через pattern matching.

    ```haskell
    maybePair :: Maybe a -> Maybe b -> Maybe (a, b)
    maybePair Nothing  _        = Nothing
    maybePair _        Nothing  = Nothing
    maybePair (Just a) (Just b) = Just (a, b)
    ```

2. Заметим, что на любой из `Nothing` мы всегда возвращаем `Nothing`, но ведь так же делает и bind для `Maybe`: если где-то в вычисление что-то пошло не так, мы вернем `Nothing`.

    ```haskell
    maybePair :: Maybe a -> Maybe b -> Maybe (a, b)
    maybePair ma mb = ma >>= \a -> mb >>= \b -> Just (a, b)
    ```

    Для лучшего понимания, что здесь происходит, расставим скобки, как в прошлом примере.

    ```haskell
    maybePair :: Maybe a -> Maybe b -> Maybe (a, b)
    maybePair ma mb = ma >>= (\a -> (mb >>= (\b -> Just (a, b))))
    ```

    Здесь мы создаем ещё две функции `a -> Maybe b` и `b -> Maybe (a, b)`.

3. Наконец, посмотрим внимательно на `Just`: с точки зрения монады - это просто `return`. Тогда, нам и не нужен какой-то особый instance для этой функции, а просто потребуем монады в качестве constraint и заменим на `return`.

    ```haskell
    monadPair :: Monad m => m a -> m b -> m (a, b)
    monadPair ma mb = ma >>= \a -> mb >>= \b -> return (a, b)
    ```

Для закрепления посмотрим на ещё один пример. Пусть для простоты у нас определены две функции следующим образом.

```haskell
stripUsername :: String -> Maybe String
stripUsername ""          = Nothing
stripUsername name@(n:ns) = case isSpace n || isPunctuation n of
    True  -> stripUsername ns
    False -> Just name
```

```haskell
validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if length s > maxLen
                          then Nothing
                          else Just s
```

Скажем, что у нас есть обёртка `newtype Username = Username String deriving (Eq, Show)` и создадим функцию `mkUser`. Опять же, начнем с самой простой реализации.

1. Самая простая реализация.

    ```haskell
    mkUser :: String -> Maybe Username
    mkUser name = case stripUsername name of
        Nothing    -> Nothing
        Just name' -> case validateLength 15 name' of
            Nothing     -> Nothing
            Just name'' -> Just $ Username name''
    ```

2. Видим здесь `case` по возвращаемому значению `stripUsername name`, но ведь выражение возвращает `Maybe`, а он монада, поэтому давайте воспользуемся bind и заменим первый `case` на более простую форму (где `name'`).

    ```haskell
    mkUser :: String -> Maybe Username
    mkUser name = stripUsername name >>= \name' ->

                      case validateLength 15 name' of
            Nothing     -> Nothing
            Just name'' -> Just $ Username name''
    ```

3. Опять видим `case`, снова вспоминаем, что `Maybe` - это монада и заменяем на оператор bind.

    ```haskell
    mkUser :: String -> Maybe Username
    mkUser name = stripUsername name >>= \name' ->

                          validateLength 15 name' >>= \name'' ->

                          Just $ Username name''
    ```

4. Наконец, посмотрим на `Just`, который мы можем получить стандартным образом обернув нечто в контекст, то есть воспользоваться `return`.

    ```haskell
    mkUser :: String -> Maybe Username
    mkUser name = stripUsername name >>= \name' ->

                          validateLength 15 name' >>= \name'' ->

                          return $ Username name''
    ```

5. Последнее, что мы можем здесь сделать - это применить $\eta$-редукцию.

    ```haskell
    mkUser :: String -> Maybe Username
    mkUser name = stripUsername name >>= validateLength 15 >>= return . Username
    ```

### Identity Monad

Самая тривиальная реализация монады - это тождество, где наш тип `newtype Identity a = Identity { runIdentity :: a }` - это просто обёртка над `a`.

```haskell
instance Monad Identity where
  return = Identity
  i >> f = ...
```

### Either Monad

Мы знаем, что `Either` - это по сути дизъюнкция Карри-Хорворда, они принимает два типа `e` и `a`, где `e` принято считать *типом ошибки*. Напомним, что его родом является `Type -> Type -> Type`, так как сам по себе `Either` - это тип, а для его определения нам нужно ещё два типа. Но также напомним, что `Either Int` или `Either String` (то есть, с одним типом из двух) выдаёт род `Type -> Type`, что подходит под определение монады/`Applicative`/функтора.

```haskell
instance Monad (Either e) where
  -- здесь мы фиксируем тип ошибки `e`

  return :: a -> Either e a
  return = Right
  -- создали новый контекст как обёртку нефиксированного типа успеха

  (>>=) :: Either e a -> (a -> Either e b) -> Either e b
  -- внимание: мы здесь требуем тот же тип ошибки, что и зафиксированный
  Left e >>= f = Left e
  -- если нам подали ошибку, значит у нас нету значения `a`, значит,
  -- единственное, что мы можем вернуть, это - ту же ошибку
  Right a >>= f = f a
  -- нам подали значения типа `a`, значит, мы можем применить функцию
  -- применяем и получаем тип `b`
```

Попробуем расширить пример `mkUser` с Monad Maybe до Either Monad и будем возвращать теперь более осмысленную ошибку.

```haskell
data ValidationError = InvalidStrip | TooBigLength
```

Две вспомогательные функции проверки почти никаким образом не меняются - разве что `Just` меняется на `Right`, а `Nothing` на `Left` с какой-то ошибкой.

```haskell
stripUsername :: String -> Either ValidationError String
stripUsername ""          = Left InvalidStrip
stripUsername name@(n:ns) = case isSpace n || isPunctuation n of
    True  -> stripUsername ns
    False -> Right name
```

```haskell
validateLength :: Int -> String -> Either ValidationError String
validateLength maxLen s = if length s > maxLen
                          then Left TooBigLength
                          else Right s
```

А теперь ещё раз посмотрим на самый первый вариант `mkUser`, если бы мы делали с Monad Maybe.

```haskell
mkUser :: String -> Maybe Username
mkUser name = case stripUsername name of
    Nothing    -> Nothing
    Just name' -> case validateLength 15 name' of
        Nothing     -> Nothing
        Just name'' -> Just $ Username name''
```

Сначала, мы убрали первый `case` от `stripUsername name`. потому что на ошибку (то есть, на `Nothing`) мы тут же выходим из вычислений и возвращаем эту ошибку. С Either Monad мы получаем ровно такую же картину: с ошибкой (то есть, с `Left`) мы возвращаем эту же самую ошибку - ничего в сравнении с Monad Maybe не меняется. Тоже самое можно сказать и про второй `case` от `validateLength 15 name'`. Наконец, мы выяснили, что `Just` - это просто обёртка (то есть, `return`) над значением, тоже самое валидно для Either Monad.

То есть, наш `mkUser`, за исключением top-level типа, *никаким образом не меняется*.

```haskell
mkUser :: String -> Either ValidationError Username
mkUser name = stripUsername name >>= validateLength 15 >>= return . Username
```

Примеры использования.

```haskell
ghci> mkUser "   "
Left InvalidStrip

ghci> mkUser " ...  I Am The Greatest Hero Of All Times ... "
Left TooBigLength

ghci> mkUser "JustSenia..."
Right ( Username "JustSenia..." )
```

### List Monad

Список также является монадой. Посмотрим на него поближе.

```haskell
instance Monad [] where
    return :: a -> [a]
    return x = [x]
    -- заворачиваем в singleton
    
    (>>=) :: [a] -> (a -> [b]) -> [b]
    l >>= f  = concat (map f l) -- или: `concatMap`
    -- мы каждый элемент `a` из списка сначала превращаем в список
    -- получаем список списка, то есть `[[b]]`, затем мы редуцируем
    -- (или: конкатенируем) в один список
```

Некоторые примеры использования List Monad.

```haskell
ghci> [10, 5, 7] >>= replicate 3
[10, 10, 10, 5, 5, 5, 7, 7, 7]
-- на первом шаге мы получаем [[10, 10, 10], [5, 5, 5], [7, 7, 7]]
-- на следующем, то есть на `concat`, получаем
-- [10, 10, 10, 5, 5, 5, 7, 7, 7]
```

```haskell
ghci> [1..5] >>= \x -> replicate x x
[1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5]
-- на первом шаге мы получаем
-- [[1], [2, 2], [3, 3, 3], [4, 4, 4, 4], [5, 5, 5, 5, 5]]
-- каждый элемент мы повторили ровно столько, сколько он сам по себе,
-- то есть 1 - 1 раз, 2 - 2 раза и так далее
-- затем, всё это собрали в один список
```

```haskell
ghci> let step x = [x - 1, x + 1]
ghci> [0] >>= step
[-1, 1]

ghci> [0] >>= step >>= step
[-2, 0, 0, 2]

ghci> [0] >>= step >>= step >>= step
[-3, -1, -1, 1, -1, 1, 1, 3]

ghci> [0] >>= step >>= step >>= step >>= step
[-4, -2, -2, 0, -2, 0, 0, 2, -2, 0, 0, 2, 0, 2, 2, 4]
```

## Композиция монад

Мы все время использовали функцию преобразования и заворачивания в контекст `a -> m b` в монаде. Это, так называемая, **стрелка Клеисли**.

Вспомним про композицию функций - это оператор `(.)` - сначала применяем одну, затем вторую функцию и получаем новую функцию, чей top-level определен следующим образом:

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```

А теперь представим, что вместо стандартных возвращений функций, мы будем использовать стрелку Клеисли. Мы получаем *композицию Монад*.

```haskell
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
```

Как работает `(<=<)`? Пусть у нас есть аргумент `x` типа `a`. Тогда, давайте применим `f`, который имеет тип `a -> m b`, к нашему аргументу и получим `m b` - значение в контексте. Но нам нужно само значение типа `b`, так как у нас таким образом определен `g`, имеющий тип `b -> m c`. Применим bind (или: оператор `(>>=)`), который имеет сигнатуру вида `m a -> (a -> m b) -> m b`, и получим `m c`.

```haskell
(<=<) f g x = f x >>= \x' -> g x'
```

Обратим внимание, что `(>=>)`, который называют как *прямая композиция*, - это в точности `flip` от оператора `(<=<)`.

Отсюда мы выводим одно из правил монады *ассоциативность*:

```haskell
(f >=> g) >=> h = f >=> (g >=> h)
```

## Zen

Мы бы хотели заиметь такой оператор, который бы сохранял эффект контекста от первой монады, в случае если он закончился *неудачно*, в ином случае - вернуть контекст второго.

```haskell
(>>) :: Monad m => m a -> m b -> m b
m >> k = m >>= \_ -> k
```

Почему мы не можем сделать как "вернуть второе значение" (`_ >> k = k`)? Потому что если бы первое, рассматривая Monad Maybe, было `Nothing`, то мы очень хотели бы вернуть `Nothing`, в ином случае - второе.

```haskell
ghci> Nothing >> Just 5
Nothing
```

Теперь посмотрим на пример с двумя массивами `[Bool]` и `[Int]`. Если мы ничего бы не делали, то вернулся бы второй без лишних действий.

```haskell
ghci> [True, False] >> [1, 2, 3]
[1, 2, 3, 1, 2, 3]
-- \_ -> k игнорирует элементы `True` и `False`
```

Теперь зададим функцию, которая бы на `True` возвращала непустой список (список с `Unit`, если точнее), а на `False` - пустой. Тогда применение `zen` на singleton возвращал бы этот самый массив, а на пустой, так как там нету элементов, ничего (пустой массив).

```haskell
guard :: Bool -> [()]
guard True  = [()]
guard False = []
```

```haskell
ghci> [True, False, True] >> [1, 2]
[1, 2, 1, 2, 1, 2]

ghci> [True, False, True] >>= \b -> guard b >> [1, 2]
[1, 2, 1, 2]
```

## Joining Monad

Функция `join` формально складывает все слои в один и получает тот же контекст.

```haskell
join :: Monad m => m (m a) -> m a
```

Несколько примеров с списками и `Maybe`.

```haskell
ghci> join [[3, 4], [7, 10]]
[3, 4, 7, 10]
```

```haskell
ghci> join Just (Just 3)
Just 3
```

**Важно**: наши слои **должны быть одинакового типа**, то есть, мы не можем, например, сделать `join (Just [1, 2, 3])`, так как `Maybe` и `[]` - это два разных типа.

А что будет, если в `join` подать функцию? По определению, функция - это `Type -> Type -> Type`, где первый и второй аргументы это аргументы и возвращаемое значение. Тогда `join` будет просто дублировать поданный аргумент.

```haskell
ghci> join (,) 1
(1, 1)
```

```haskell
ghci> join replicate 3
[3, 3, 3]
```

Возникает вопрос: можем ли мы определить `extract`, который был избавлялся от всех слоев и возвращал бы значение вне контекста?

```haskell
extract :: Monad m => m a -> a
```

Нет, так как это работает далеко не со всеми типами. Например, мы не можем такое определить для `Maybe`, так как там может лежать `Nothing`.

## Функции на монадах

Напомним, что монада - это и `Applicative`, и `Functor`, а значит, мы можем определить легко некоторые функции для монады с тем же top-level, что и у `Applicative`/`Functor`. Например, `liftM`.

```haskell
liftM :: Monad m => (a -> b) -> m a -> m b
```

Работает так же, как и знакомый `fmap`, только здесь мы требуем в качестве контекста - монаду.

```haskell
ghci> liftM (+1) (Just 3)
Just 4

ghci> liftM (+1) Nothing
Nothing
```

Можно так же определить знакомую тернарную функцию на монадах `liftM2`.

```haskell
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
```

Из примечательного: с помощью этой функции, мы можем легко реализовать уже сверху реализованный `monadPair` и он будет работать ожидаемым образом.

```haskell
ghci> let monadPair = liftM2 (,)
monadPair :: Monad m => m a -> m b -> m (a, b)
```

```haskell
ghci> monadPair (Just 3) (Just 5)
Just (3, 5)

ghci> monadPair (Just 3) Nothing
Nothing

ghci> monadPair [1..3] [5, 10]
[(1, 5), (1, 10), (2, 5), (2, 10), (3, 5), (3, 10)]
```

Всё это находится в модулей [Control.Monad](https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Monad.html). Из интересного оттуда можно найти модуль [Control.Monad.Extra](https://hackage.haskell.org/package/extra-1.6/docs/Control-Monad-Extra.html) с функциями на контекстах в виде монад с ленивой моделью вычисления.

```haskell
(||^) :: Monad m => m Bool -> m Bool -> m Bool -- ленивый ||
(&&^) :: Monad m => m Bool -> m Bool -> m Bool -- ленивый &&
```

```haskell
ghci> Just False ||^ Just True
Just True

ghci> Just False &&^ Just True
Just False
-- if (false && true) сразу вычисляется в C++ в false, так как `&&`

ghci> Just False &&^ Nothing
Just False
-- if (false && true) сразу вычисляется в C++ в false, так как `&&`
-- обратим внимание на то, что `Nothing` не был вычислен

ghci> Just True  &&^ Nothing
Nothing
-- if (true && predicate) сразу вычисляется в C++ в predicate, так как `&&`
-- обратим внимание на то, что на сей раз `Nothing` был вычислен,
-- так как нужно было понять, что вернёт predicate

ghci> Nothing    &&^ Just True
Nothing
-- `Nothing` был вычислен первым и остановился, потому это - `Nothing`
```

## Monad Laws

Вспомним композицию Клейсли `(<=<)`, имеющая следующую сигнатуру:

```haskell
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
```

Утверждается, что мы хотим, чтобы данная композиция была ассоциативна. Заметим, что `return` у монады - это её *нейтральный элемент* относительно операции композиции Клейсли. Тогда мы требуем работы следующих равенств:

```haskell
(h <=< g) <=< f === h <=< (g <=< f)
return <=< f    === f
f <=< return    === f
```

Если раскрыть по определению `(<=<)` в данных выше выражениях, мы получим три основных закона монады:

1. Левое тождество:

    ```haskell
    return a >>= f == f a
    ```

2. Правое тождество:

    ```haskell
    m >>= return == m
    ```

3. Ассоциативность:

    ```haskell
    (m >>= f) >>= g == m >>= (\x -> f x >>= g)
    ```

## Writer Monad

Начнём с некоторого прелюдия: рассмотрим некоторую функцию, которая что-то долго считает и причем делает разные действия во время вычисления. Нам хочется узнать, а какие именно действия делала функция во время подсчёта значения. Для этого можно завести пару из `Int` (или иного другого вычисляемого типа) и `String` - сам по себе логгер, содержащее длинное сообщение, разделенное новыми строками (как бы разными вызовами функции).

```haskell
type IntWithLog = (Int, String)

binPow :: Int -> Int -> IntWithLog
binPow 0 _      = (1, "")
binPow n a
    | even n    = let (b, msg) = binPow (n `div` 2) a 
                  in (b * b, msg ++ "Square " ++ show b ++ "\n")
    | otherwise = let (b, msg) = binPow (n - 1) a
                  in (a * b, msg ++ "Mul " ++ show a ++ " and " ++ show b ++ "\n")
```

Как ни странно, именно этим и занимается `Writer`. Он позволяет инкапсулировать две вещи: значения типа `a` - наше актуальное значение, и значение типа `w` - наш непосредственный логгер, который может быть почти кем-угодно.

```haskell
newtype Writer w a = Writer { runWriter :: (a, w) }
```

В типе `Writer` рекомендуется обратить внимание на последовательность типов: `a` *обязательно стоит после* `w`, так как мы им будем сильно варьировать.

```haskell
instance Monoid w => Monad (Writer w) where
  return :: a -> Writer w a
  return a = Writer (a, mempty)

  (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
  Writer (a, oldLog) >>= f = let Writer (b, newLog) = f a 
                              in Writer (b, oldLog <> newLog)
```

В чем интерес использовать именно `Writer`? До этого мы пользовались `String`, у неё есть что-то вроде пустого состояния (пустая строка). В `Writer` у нас тоже самое: не зря мы `w` поставили первым типом - он у нас моноид, потому что мы хотим получать объект из типа из `a`, а в качестве нейтрального элемента из `Monoid` мы используем `mempty`. Также обратим внимание на bind: с помощью функции мы создаем новый лог типа `Writer w b`, а затем нам нужно (в общем случае) сконкатенировать старый и новый лог - сделать это мы сможем опять же из свойства моноида - оператор `(<>)`.

Определим также несколько утилит-функций.

```haskell
tell       :: w -> Writer w ()
-- возвращает новый лог, нужен для эффекта от `then`

execWriter :: Writer w a -> w
-- "правая проекция" для `Writer`, возвращает сам логгер

writer     :: (a, w) -> Writer w a
-- а-ля конструктор из значения и логгера в `Writer`
```

Попробуем переписать пример с `binPow`, используя `Writer`.

```haskell
binPow :: Int -> Int -> Writer String Int
binPow 0 _      = return 1
binPow n a
    | even n    = binPow (n `div` 2) a >>= \b ->
                  -- `binPow` возвращает `Writer`, далее bind, где `b` - это `Int`
                  tell ("Square " ++ show b ++ "\n") >>
                  -- здесь мы создаем `Writer String Int`, но с `Unit` значением
                  -- вместо `Int`, далее мы прокидываем его в `then`
                  return (b * b)
                  -- `return` возвращает пустой логгер с новым значением `Int`
                  -- из `then` к нам прилетает `Writer String Int`, который
                  -- затем конкатенируется по оператору `(<>)`
    | otherwise = binPow (n - 1) a >>= \b -> 
                  tell ("Mul " ++ show a ++ " and " ++ show b ++ "\n") >>
                  return (a * b)
```

На самом деле, сложную махинацию с `tell` и `return` можно было избежать, используя конструктор `writer`.

```haskell
binPow :: Int -> Int -> Writer String Int
binPow 0 _      = return 1
binPow n a
    | even n    = binPow (n `div` 2) a >>= \b -> 
                  writer (b * b, "Square " ++ show b ++ "\n")
    | otherwise = binPow (n - 1) a >>= \b -> 
                  writer (a * b, "Mul " ++ show a ++ " and " ++ show b ++ "\n")
```

В чем главные особенности вот такого класса от обычной пары с сообщением и значением?

1. Деление логики. Благодаря `tell` и `return` мы можем без проблем разделить логику логгирования и подсчёта чего-то. `tell` отвечает за сообщения, `return`, соответственно, за счёт.
2. Конкатенация сообщений. Благодаря `then` мы можем без проблем конкатенировать два лога: `binPow 2 3 >> binPow 2 3`.

В чем проблемы такого подхода? Она из проблем: это занимаемое место, из-за постоянно растущего лога проблема нехватки может достаточно быстро настигнуть. Поэтому почти никто и никогда не использует `Writer` в том виде, в котором он представлен. Однако, он может быть достаточно полезен для чистого логирования без исключений и дебага.

## Reader Monad

Начнем с небольшой мотивационной части про те или иные "глобальные переменные", которых на самом деле в Haskell нету. Для этого нам приходится их передавать постоянно в функции. Приведём искусственный пример.

```haskell
data Environment = Environment { ids  :: [Int]
                               , name :: Int -> String
                               , near :: Int -> (Int, Int) }
```

Пусть будет функция `inEnv`, которая бы возвращала бы `True`, если в нашей среде есть некоторый элемент `Int`.

```haskell
inEnv :: Environment -> Int -> Bool
inEnv env i = i `elem` ids env 
```

Также заведём ещё функцию, которая бы спрашивала есть ли один из элементов в среде.

```haskell
anyInEnv :: Environment -> (Int, Int) -> Bool
anyInEnv env (i, j) = inEnv env i || inEnv env j
```

Наконец, ещё одна: проверка соседей.

```haskell
checkNeighbours :: Environment -> Int -> Maybe String
checkNeighbours env i = if anyInEnv env (near env i)
                        then Just (name env i)
                        else Nothing
```

Это очень простой пример на понимание проблемы. У нас всё иммутабельно, `env` никогда не изменяется, но при этом нам всё равно приходится тащить вообще во все функции.

`Reader` решает эту проблему тем, что это - просто обёртка из условной среды в нужное нам значение. Причем, как и с `Writer`, мы тип определяем таким образом, что мы могли легко варьировать `a`, но никогда не меняли `e`.

```haskell
newtype Reader e a = Reader { runReader :: e -> a }
```

Теперь определим его класс.

```haskell
instance Monad (Reader e) where
  return :: a -> Reader e a
  return a = Reader (\_ -> a) -- или: `Reader (const a)`
  -- здесь есть только значения типа `a`, но нет никакой среды
  -- давайте тогда вернем функцию, которая бы игнорировала среду и возвращала `a`

  (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
  m >>= f = Reader (\e -> runReader (f (runReader m e)) e)
  -- `(runReader m)` - возвращает `e -> a`, передавая ей среду мы получаем значение типа `a`
  -- `f (runReader m e)` - это в точности `Reader e b` - на нём мы запустим `runReader` и передадим туда `e`
  -- наконец, получаем `b`, которые мы оборачиваем в лямбду и в `Reader`
```

Для нашего `Reader` мы можем создать несколько полезных для работы функций.

```haskell
ask :: Reader e e
-- по сути, она возвращает всю переданную ей среду

asks :: (e -> a) -> Reader e a
-- можно воспринимать данную функцию в двух разных контекстах:
-- либо мы берем функцию и оборачиваем её в `Reader`
-- либо как выделение части из переданной ей среды, какой-то релевантной части

local :: (e -> b) -> Reader b a -> Reader e a
-- если говорить про практическую часть, то у нас здесь на самом деле композиция `Reader`'ов
```

Перепишем наш мотивационный пример с помощью `Reader`.

```haskell
inEnv :: Int -> Reader Environment Bool
inEnv i = asks (elem i . ids)
```

```haskell
anyInyEnv :: (Int, Int) -> Reader Environment Bool
anyInyEnv (i, j) = inEnv i || inEnv j
```

```haskell
checkNeighbours :: Int -> Reader Environment (Maybe String)
checkNeighbours i = 
  asks (`near` i) >>= \pair ->
  anyInEnv pair   >>= \res  ->
  if res 
  then Just <$> asks (`name` i)
  else pure Nothing
```

В чем главные особенности `Reader` в жизни?

1. Нам нет необходимости передавать аргументы среды или конфигурации явным образом.
2. Мы не можем случайно изменить среду обитания, так как мы не имеем явного доступа к нему.
3. Наш код может быть полиморфен и может работать с разными частями конфигурации/среды.

## State Monad

Начнем с некоторой мотивации, почему мы хотим этим овладеть. Представим себе структуру `Stack`: в функциональных языках мы не умеем менять локальные переменные, а значит нам приходится пересоздавать новый объект с обновленными данными.

```haskell
type Stack = [Int]

pop  :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> Stack
push x s = x:s

stackOps :: Stack -> (Int, Stack)
stackOps s = let (x, xs) = pop s
                 s'      = push 5 xs
                 res     = push 10 s'
             in (x, res)
```

В чем суть `State`? По сути, это прямая композиция `Reader` и `Writer`.

```haskell
newtype State s a = State { runState :: s -> (a, s) }
```

Она инкапсулирует функцию, которая принимает *текущее* состояние `s` и возвращает пару из какого-то значения типа `a` и *нового* состояния `s`. Попробуем сделать instance для монады.

```haskell
instance Monad (State s) where
  return :: a -> State s a
  return a = State (\s -> (a, s))
  -- принимаем элемент и возвращаем состояние,
  -- заметим, что мы никаким образом не меняем состояния, поэтому мы просто
  -- пропихиваем наше значение типа `a` в функцию, никак не меняя состояние

  (>>=) :: State s a -> (a -> State s b) -> State s b
  oldState >>= f = State (\s ->
    let (a, newState) = runState oldState s
    in runState (f a) newState
  )
  -- объяснение, почему это так ниже
```

Попробуем понять, почему у нас именно такой bind, а никакой другой. Скажем для простоты, что `State'` - это alias `type State' s a = s -> (a, s)`. Тогда, наш bind это по простому:

```haskell
bindState
  :: State' s a        -- (s -> (a, s))
  -> (a -> State' s b) -- (a -> s -> (b, s))
  -> State' sb         -- (s -> (b, s))
                       -- мы должны вернуть функцию из (s -> (b, s))
bindState m f = \s ->  -- примем в качестве `\s` наше ПЕРВОЕ состояние
  let (a, s') = m s    -- с помощью `m` (то есть, `s -> (a, s)`), мы получим
                       -- значение `a` и НОВОЕ состояние s'
    in f a s'          -- передадим в f (который и возвращает НУЖНОЕ состояние)
                       -- значение `a` и НОВОЕ состояние s'
```

С помощью `State` мы можем переписать стек более красиво. Здесь функция `state` - это конструктор состояния, принимает `s -> (a, s)` и возвращает `State s a`.

```haskell
type Stack = [Int]

pop :: State Stack Int
pop = state (\(x:xs) -> (x, xs))
-- `(x:xs)` - наш стек (без случая с отсутствие значений)
-- `(x, xs)` - возвращает голову и хвост

push :: Int -> State Stack ()
push x = state (\xs -> ((), x:xs))
-- заметим, что `push` возвращает `Unit`
-- с `x:xs` наше состояние стека изменилось
```

Тогда, операции становится чуть проще:

```haskell
stackOps :: State Stack Int
stackOps = pop >>= \x -> push 5 >> push 10 >> return x
-- 1. сделал `pop` на стеке
-- 2. достали значение, которое мы вытащили оттуда `x`
-- 3. положили `push 5` значение (эффект - СОСТОЯНИЕ ИЗМЕНИЛОСЬ)
-- 4. положили `push 10` значение (эффект - СОСТОЯНИЕ ИЗМЕНИЛОСЬ)
-- 5. вернули вынутое значение `x` <-- важно, что наше состояние НЕ ИЗМЕНИЛОСЬ
```

На самом деле, у `State` может быть куча разных полезных и интересных функций. Рассмотрим некоторые из них.

```haskell
get :: State s s
-- возвращает в качестве значения само по себе состояние
```

```haskell
put :: s -> State s ()
-- работает как `tell` про логгер, перезаписывает состояние
-- нам плевать на значение, поэтому там `Unit`
```

```haskell
modify :: (s -> s) -> State s ()
-- изменяет состояния, значение не трогаем (поэтому `Unit`)
```

```haskell
gets :: (s -> a) -> State s a
-- принимает модифицирующую функцию, и `State` принимает к состоянию и возвращает значение
```

```haskell
withState :: (s -> s) -> State s a -> State s a
```

```haskell
evalState :: State s a -> s -> a
-- работает как `runState`, только здесь мы возвращаем только значение
```

```haskell
execState :: State s a -> s -> s
-- работает как `runState`, только здесь мы возвращаем только состояния
```

Теперь мы бы хотели заиметь функцию, которая бы делала нечто $n$ раз с монадическими объектами. На этот случай есть функция `replicateM`.

```haskell
replicateM :: Monad m => Int -> m a -> m [a]
-- принимает значение типа `a` в контексте `m`
-- как обычный `replicate`: если `Int` - это условный n, тогда
-- на выходе мы получаем список, в котором ровно n штук значений типа `a` в контексте `m`
```

А если мы хотим пройтись по списку элементов и что-то с ними сделать? На этот случай есть `forM_`.

```haskell
forM_ :: (Monad m, Foldable t) => t a -> (a -> m b) -> m ()
-- заметим, что данная функция похожа на `Traversable`, а также у нас в конце стоит `m ()`,
-- то есть, нас не интересует аргумент, но интересует само действие, но поскольку нас
-- не интересует результат, то нам предостаточно заиметь `Foldable` (то есть, `forM_` можно определить через `foldr`)
-- данная функция применяет функцию `a -> m b` к каждому элементу `t a`, получается `m b`, у которого есть эффект,
-- которые имеют место где-то внутри, но в конце имеет место `m ()` (то есть, никуда эффекты не применяются)
```

Тогда, для множества операций достаточно сделать так: пусть есть тип операции `type StackOperation = Pop | Push Int`, тогда множественное действие будет похоже на следующее.

```haskell
doOperations :: [StackOperation] -> State Stack ()
doOperations ops = forM_ ops (\case -- XLambdaCase
    -- для каждой операции в нашем списке мы делаем Pattern Matching по операции
    -- затем, выполняем какое-то действие над стеком
    Pop    -> pop >> return ()
    Push n -> push n
)
```

## Типизированная дыра

Возьмем некоторое определение функции и посмотрим на символ `_` - когда мы его пишем слева, это значит, что нам все равно, что там может попасть под образец, но если мы напишем справа - то это нечто, называемое как **typed hole**. Если в компилятор найдет хотя бы одну дыру, то он упадет с ошибкой о найденной дыре.

Для чего нам такое может пригодится? Например, для выведения типа.

```haskell
join :: Monad m => m (m a) -> m a
join m = _

    • Found hole: _ :: m a
```

Попробуем добавить что-нибудь. Попробуем добавить именованную дыру с bind.

```haskell
join :: Monad m => m (m a) -> m a
join m = _k >>= _f

    • Found hole: _k :: m a0
    • Found hole: _f :: a0 -> m a
```

Из этого сообщения выходит, что `m` - это не функция, попробуем поставить вместо `_k` её.

```haskell
join :: Monad m => m (m a) -> m a
join m = m >>= _f

    • Found hole: _f :: m a -> m a
```

Здесь нужна функция, которая бы вывела из `m a` в `m a` - это и есть знакомый нам давно `id`.

```haskell
join :: Monad m => m (m a) -> m a
join m = m >>= id
```

## Cont Monad

Для начала разберемся с таким стилем программирования как *Continuation Passing Style*. Посмотрим на код на JavaScript `add`:

```javascript
function add(a, b) {
    return a + b;
}
```

Мы можем сделать тоже самое, но только с некоторыми callback'ом. В общем случае это выглядит так:

```javascript
function addCPS(a, b, callback) {
    callback(a + b);
}
```

А для создания таких callback'ов мы, в общем случае, сделать следующее:

```javascript
addCPS(1, 2, function (result) {
    // use result here
});
```

В Haskell по простому мы можем сделать следующее:

```haskell
add :: Int -> Int -> Int
add x y = x + y
-- оригинальная `add` функция

addCPS :: Int -> Int -> (Int -> r) -> r
addCPS x y cont = cont (x + y)
-- здесь мы не говорим, какой тип `r`, потому что нам все равно
```

Рассмотрим ещё один пример с, так называемым, *анонимным callback*'ом.

```haskell
square :: Int -> Int
square x = x * x
-- оригинальная `square` функция

squareCPS :: Int -> ((Int -> r) -> r)
squareCPS x = \k -> k (square x)
-- функция `square` с callback'ом
```

Сами же continuation'ы могут быть выстроены как ряд лямбд-функций. Иногда бывает это сложно.

```haskell
doubleSquareCPS :: Int -> ((Int -> r) -> r)
doubleSquareCPS x = \k ->
  -- есть функция из `Int` в `r` - это наш continuation
  squareCPS x  $ \x2 ->
  -- вызываем squareCPS от поданного значения, поскольку этот тоже CPS, то
  -- мы ему подаем ещё один большой callback, `x2` результат функции `squareCPS`
  squareCPS x2 $ \x4 ->
  -- возвращает предыдущий ещё один вызов `squareCPS` только от `x2`
  -- у него тоже есть CPS, поэтому мы ему также передаем в виде лямбды
  k x4
  -- в конце мы вызываем `k` от `x4`
```

Как перевести из CPS-функции в обычную? Давайте просто передадим `id`: он сделает тип из `Int` в `Int`, тогда мы получим сам `Int` на выходе.

```haskell
ghci> doubleSquareCPS 2 id
16
```

Перейдем, наконец, к типу `Cont`. Его идея в том, чтобы инкапсулировать отложенное вычисление.

```haskell
newtype Cont r a = Cont ( runCont :: (a -> r) -> r )
-- тип `r` - это результат continuation
-- тип `a` - это соответственное вычисляемое значение
```

Рассмотрим instance от монады.

```haskell
instance Monad (Cont r) where
  return :: a -> Cont r a
  return x = Cont (\c -> c x)
  -- принимает `x` типа `a` и возвращает отложенное вычисление `x`, где `c`
  -- это и есть вычисление или continuation - передаем в него `x` и получаем `r`

  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  s >>= f = Cont (\c -> runCont s (\a -> runCont (f a) c))
  -- объяснение, почему это так ниже
```

Для понимания bind, а он довольно сложный, мы введем тип `type Cont' r a = (a -> r) -> r` и создадим для него развернутый bind.

```haskell
bindCont
  :: Cont' r a        -- (a -> r) -> r
  -- взяли в качестве первого аргумента, в котором `a` мы хотим поменять на `b`
  -> (a -> Cont' r b) -- (a -> (b -> r) -> r)
  -- соответствующая функция из bind
  -> Cont' r b        -- (b -> r) -> r
bindCont s1 f = \c ->
    -- `s1` - это `(a -> r) -> r`
    -- `f`  - это `(a -> (b -> r) -> r)`
    -- теперь к реализации функции...
    -- `c`  - это continuation `b -> r`
  s1 $ \a ->
    -- начиная с s1 $ ... мы начинаем работать с первым Cont'
    -- ему нужно передать функцию `a -> r`, тогда `\a` имеет тип `a`
    let s2 = f a
    -- после применения `f a`, переменная `s2` будет иметь тип `(b -> r) -> r`
    in s2 c
    -- здесь переменная `c` - это `b -> r`, а `s2` - это `(b -> r) -> r`
    -- передаём и убираем `b -> r` - оставляем только `r`
```

Наконец, перепишем наши примеры через Cont Monad.

```haskell
addCPS :: Int -> Int -> Cont r Int
addCPS x y = return $ x + y
```

```haskell
squareCPS :: Int -> Cont r Int
squareCPS = return . square
```
