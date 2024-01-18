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
