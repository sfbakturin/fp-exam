# Monad Transformers

## Монады как эффект

Мы уже знакомы с монадами как способ репрезентации некоторого эффекта или даже большого действия, вычисления или работы с внешним миром. Ниже представлена таблица того, на что мы посмотрели и кто каким эффектом обладает.

| Монада | Эффект |
|:-------|:-------|
| Maybe  | Вычисление может закончится неудачно |
| Either | Вычисление может закончится неудачно с конкретной ошибкой |
| []     | Вычисление имеет множество вычисленных значений |
| Writer | Вычисление имеет моноидальный аккумулятор |
| Reader | Вычисление имеет доступ к некоторому неизменяемому контексту |
| State  | Вычисление имеет доступ к некотором изменяемому состоянию |
| IO     | Вычисление может проводить I/O действия |

Однако теперь мы хотим применить несколько эффектов к нашей функции, то есть, грубо говоря, скомбинировать несколько монад.

Посмотрим ещё раз на итерацию создания монад для состояния и придём к какому-ту ответу.

1. Итак, изначально у нас была функция, которая принимала непосредственно нашу рабочую среду, использовала ещё и возвращала какое-то значение.

    ```haskell
    foo :: String -> Env -> Int
    ```

2. Затем, мы подумали и придумали способ запихнуть возвращаемое значение и рабочую среду в один объект-монаду.

    ```haskell
    foo :: String -> Reader Env Int
    ```

    Здесь эффект вычисляется автоматически.

3. Теперь попробуем соединить `Reader` и `State`, скажем, что у нас есть вот такой мнемонический код.

    ```haskell
    foo :: UnknownType
    foo i = do
      baseCounter <- ask
      let newCounter = baseCounter + i
      put [baseCounter, newCounter]
      return newCounter  
    ```

    Здесь появляются две новые функции `ask` и `put`. Договоримся о следующем:

      * У `Reader` есть определение `ask`, но нету `put`.
      * У `State` есть определение `put`, но нету `ask`.

4. Казалось бы, мы могли бы остановиться и сказать - у нас же есть монада `RWS`, можем её воспользоваться - действительно, в ней сочетаются `Reader` и `State`, но зачем нам `Writer`?
5. Мы могли бы плюнуть и сказать, что у нас будет только `State`, в котором мы будем работать так, как мы захотим.

    ```haskell
    foo :: State (Int, [Int]) Int
    foo i = do
      x <- gets fst
      let xi = x + i
      put (x, [x, xi])
      return xi
    ```

    Понятное дело, что это - небезопасно. Мы имеем доступ к значениям, а значит, мы можем в любой момент поменять среду.

6. Наконец, единственное и правильно решение - использовать *трансформеры*-монады.

    ```haskell
    foo :: Int -> ReaderT Int (State [Int]) Int -- альтернатива: `StateT [Int] (Reader Int) Int`
    foo i = do
      baseCounter <- ask
      let newCounter = baseCounter + i
      put [baseCounter, newCounter]
      return newCounter
    ```

    Заметим, что мы не просто поместили `State` в `ReaderT`, мы сказали, какой тип у `State` хранится.

## Реализация

Почему нам вообще придётся изворачиваться таким образом? Потому что у нас есть одна проблема: `Functor`, `Applicative`, `Alternative` - все они композируемы, а `Monad` - нет. А что значит вообще "композируемый"? Представим себе тип `Compose` следующим образом.

```haskell
newtype Compose f g a = Compose { getCompose :: f (g a) }
```

Представим себе композицию вида "функтор от чего-то", так вот этот тип говорит следующее: если $f$ - это функтор и $g$ - это функтор, тогда композиция $f$ и $g$ также является функтором (в техническом аспекте: `Compose f g` - это тоже функтор). Такое же правило распространяется и для `Applicative`, и для `Alternative`, и для `Foldable`, и для `Traversable`. Более того, мы можем это прописать явным образом эти правила на языке Haskell.

```haskell
instance (Functor     f, Functor     g) => Functor     (Compose f g)
instance (Foldable    f, Foldable    g) => Foldable    (Compose f g)
instance (Traversable f, Traversable g) => Traversable (Compose f g)
instance (Applicative f, Applicative g) => Applicative (Compose f g)
instance (Alternative f, Applicative g) => Alternative (Compose f g)
```

С точки зрения теории категории прописать тоже самое для монад невозможно, а поэтому для реализации придётся прописывать все instance'ы ручками.

### Maybe

Попробуем скомбинировать `Maybe` и `IO` в одну монаду. Скажем, что у нас есть тип `MaybeIO`, где под контекстом завернуто `Maybe` (ещё один контекст).

```haskell
newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }
```

Тогда, в принципе, нам никто не мешает создать instance монады. Функция `return` будет возвращать завернутый в `Just` и `IO` новый контекст по некоторому значению `a`. Оператор `(>>=)` немного посложнее: во первых, нам нужно произвести `IO`-действие и вернуть `Maybe` объект, где, очевидно, есть два варианта: либо это `Nothing`, либо целостный `Just`, к которому нам нужно применить функцию и завернуть обратно в `MaybeIO`.

```haskell
instance Monad MaybeIO where
  return x = MaybeIO (return (Just x))

  MaybeIO action >>= f = MaybeIO $ do
      result <- action
      case result of
          Nothing -> return Nothing
          Just x  -> runMaybeIO (f x)
```

Отлично, теперь мы можем, например, избавиться от бесконечного числа `case`'ов в каком-нибудь коде, который бы дергал наш instance. Появляется новая проблема? Мы не можем просто так взять и вставить внутрь `IO` какое-то `IO`-действие, не возвращающее результата (то есть, `IO ()`), например:

```haskell
result <- runMaybeIO $ do
  c1 <- MaybeIO $ tryConnect "host1"
  c2 <- MaybeIO $ tryConnect "host2"
  ...
-- работает

result <- runMaybeIO $ do
  c1 <- MaybeIO $ tryConnect "host1"
  print "Hello" -- так нельзя, а хочется
  c2 <- MaybeIO $ tryConnect "host2"
  ...
-- не работает, не сходится по типам
```

Для таких случаев давайте создадим небольшую функцию, которая бы заворачивала любой результат `IO`-действия в `MaybeIO`. Тогда, подставив везде данную функцию, где мы хотим то, что хочется, мы получим корректный тип и всё скомпилируется.

```haskell
transformIO2MaybeIO :: IO a -> MaybeIO a
transformIO2MaybeIO action = MaybeIO $ do
  result <- action
  return (Just result)

result <- runMaybeIO $ do
  c1 <- MaybeIO $ tryConnect "host1"
  transformIO2MaybeIO $ print "Hello"
  c2 <- MaybeIO $ tryConnect "host2"
  ...
-- работает
```

По сути, мы реализовали то, что хотели - трансформер для `Maybe`, а именно тип `MaybeT`. Выглядит он абсолютно также, только вместо `IO` у нас указывается явно, что `m` - это монада.

```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
```

```haskell
instance Monad m => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return x = MaybeT (return (Just x))

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT action >>= f = MaybeT $ do
      result <- action
      case result of
          Nothing -> return Nothing
          Just x  -> runMaybeT (f x)
```

Также, нам понадобится какой-нибудь простой код, который бы смог взять функтор и превратить его в `MaybeT`.

```haskell
transformToMaybeT :: Functor m => m a -> MaybeT m a
transformToMaybeT = MaybeT . fmap Just  
```

А можем ли мы описать данный код в общем случае? Для этого мы посмотрим на сигнатуры функций для `MaybeT` и ещё не рассмотренного `EitherT`.

```haskell
transformToMaybeT  :: Monad m => m a -> MaybeT    m a
transformToEitherT :: Monad m => m a -> EitherT l m a
```

Видим схожие детали, а значит явно есть что-то, что объединит их. Пусть есть монада `m`, мы хотим, чтобы и `t m` был монадой. Тогда заведем некоторую функцию `lift`, которая бы трансформировала бы `m` в `t m` (как и делают функции `transformTo`) и, опять же на плечах программиста, должна была следовать следующим правилам:

```haskell
lift . return  = return
lift (m >>= f) = lift >>= (lift . f)
```

В Haskell такой type class существует, называют его `MonadTrans`, который и нужен для трансформации из монады в монаду.

```haskell
class MonadTrans t where  -- РОД t :: (* -> *) -> * -> *
  lift :: Monad m => m a -> t m a

  {-# LAWS

      1. lift . return  ≡ return
      2. lift (m >>= f) ≡ lift m >>= (lift . f)

  #-}
```

Тогда instance для `MaybeT` выглядел бы следующим образом (уже пользуясь вышеописанной функцией).

```haskell
instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift = transformToMaybeT
```

### Reader

Так же, как и с `Maybe`, мы попробуем применить нашу среду в `IO` контексте и поймем, что, на самом деле, `ReaderT` почти ничем примечательным не отличается от `MaybeT`. Итак, приведем пример с средой + IO.

```haskell
newtype LoggerName = LoggerName { getLoggerName :: Text }
-- наша основная среда

logMessage :: LoggerName -> Text -> IO ()
-- скажем, что это какая-та функция, которая записывает в файл .log
```

```haskell
-- вот здесь протягиваем среду
readFileWithLog :: LoggerName -> FilePath -> IO Text
readFileWithLog loggerName path = do
  logMessage loggerName $ "Reading file: " <> T.pack (show path)
  readFile path

-- вот здесь
writeFileWithLog :: LoggerName -> FilePath -> Text -> IO ()
writeFileWithLog loggerName path content = do
   logMessage loggerName $ "Writing to file: " <> T.pack (show path)
   writeFile path content

-- и здесь....
prettifyFileContent :: LoggerName -> FilePath -> IO ()
prettifyFileContent loggerName path = do
  content <- readFileWithLog loggerName path
  writeFileWithLog loggerName path (format content)
```

Придумаем тип `ReaderT`, который бы отвечал как `Reader`, но при этом нёс в себе монаду.

```haskell
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
```

Заметим, что ему также приходится иметь подобную функцию для получения доступа к элементу `a` (в данном случае, `r` - это среда обитания).

Тогда, мы легко подменяем `LoggerName` на alias в виде `LoggerIO`.

```haskell
type LoggerIO a = ReaderT LoggerName IO a
-- `LoggerName` на протяжении всей жизни остаётся неизменным
-- `IO a` - это контекст для задачи
```

Самое приятное, что его instance почти такой же, как и [`Reader`](L5-Monads.md#reader-monad). Обратим внимание, что здесь мы используем свойство функции `lift` из `MonadTrans` instance'а.

```haskell
instance Monad m => Monad (ReaderT r m) where
  return = lift . return
  -- пользуемся правилом: "lift . return = return"

  m >>= f = ReaderT (\r -> do
      a <- runReaderT m r
      runReaderT (f a) r
    )
```

```haskell
instance MonadTrans (ReaderT r) where
  lift :: m a -> ReaderT r m a
  lift = ReaderT . const
  -- мы всё ещё только делаем обёртку над значением
```

### IO

Мы не будем рассматривать все остальные случаи, так как они очень друг на друг похожи, что не может не радовать. Однако, реальность такова, мы не можем определить трансформер для `IO`. Это сделано по той причине, что создание для неё трансформера приведёт к погубным последствиям.

Единственное, что можно создать под него, так это - `MonadIO`, принимающий монаду `m`. Он позволяет производить `lift` специальным образом `IO`-действие в другую, выбранную, монаду.

```haskell
class Monad m => MonadIO m where
  liftIO :: IO a -> m a
  -- может показаться, что она стирает нечистоту IO, но, на самом деле,
  -- она нужна только потому, что обычно у вас IO в самом конце цепочки трансформеров,
  -- поэтому мы здесь будем поднимать монаду с самого низа, в самый вверх
```

Заметим, что данную функцию несложно определить в разных instance'ах `MonadIO`.

```haskell
instance MonadIO IO where
  liftIO = id
  -- ничего не делаем, так как мы уже и есть IO

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO
```

### `mtl`

`mtl` пакет состоит из множество трансформеров и type class'ов, там лежат все реализации тех, которые мы уже рассмотрели выше. Они позволяют не писать $N$ лифтов (для взятия элемента из монады, потом ещё раз, и ещё раз) для поднятия в контекст, а предоставляют функции из коробки чтобы убрать эти лифты.

```haskell
class Monad m => MonadReader r m | m -> r where
  ask    :: m r
  -- возвращает монадную среду контекста

  local  :: (r -> r) -> m a -> m a
  -- выполняет вычисления на измененной среде

  reader :: (r -> a) -> m a
  -- возвращает функцию из данной среды
```

Что означает `| m -> r` в объявлении класса типов? Эта часть, объединённая с оператором `(|)`, представляет собой *функциональную зависимость* между типом `m` (монадой) и типом `r` (контекстом), то есть говорит нам, что при фиксированном типе `m`, тип `r` также фиксирован. Это означает, что *для каждой конкретной монады `m` существует единственный контекст `r`*.

## MonadThrow

Можем ли мы кинуть исключение из чистой функции? На самом деле, да, посмотрим на функцию `throw` - он не требует монады в качестве аргументов. Заметим, что мы не можем сделать `catch` из чистой функции, ловить исключения можно только в `IO`-контексте. Почему вообще не хочется пользоваться функцией `throw`? Потому что она не `IO`, а значит её результат пробрасываться через `IO` функции, тем самым полностью ломая вычисления, чего не хочется на самом деле.

Для решения это проблемы вы либо всё делаете в `IO`, тогда пользуетесь `throwIO`/`catch` для обработки исключений и тем самым вы не ломаете ничего, либо вы накладываете на всё монадический контекст (пример: `ExceptT` - после ошибки он заменяет результат вычисления на ошибку, продолжая вычисления, при этом не производя их). Для реализации были придуман класс `MonadThrow`.

```haskell
class Monad m => MonadThrow m where
  throwM :: Exception e => e -> m a
```

```haskell
instance MonadThrow Maybe where
  throwM _ = Nothing
  -- игнорируем `Exception` и возвращаем `Nothing`

instance MonadThrow IO where
  throwM = Control.Exception.throwIO
  -- остаётся тот же `throwIO`

instance MonadThrow m => MonadThrow (StateT s m) where
  throwM = lift . throwM
  -- для внутренних монад мы применяем `lift`, что означает, что мы
  -- можем кинуть исключение уже оттуда
```

Для ловли - соответствующий `MonadCatch`. Если вы определили нечто для бросания - то следует сразу определить и для ловли.

```haskell
class MonadThrow m => MonadCatch m where
  catch :: Exception e => m a -> (e -> m a) -> m a
```

```haskell
instance MonadCatch IO where
  catch = Control.Exception.catch
```

На вопрос, есть ли комбинированный, который умеет и кидать, и ловить, ответ - да, опять же `mtl` - класс `MonadError`.

```haskell
class (Monad m) => MonadError e m | m -> e where
-- замечаем, что как и выше, в главе про монады, есть | m -> e, только здесь мы
-- определяем тип ошибки именно для монады,
-- то есть, ОДНА МОНАДА может кидать только ОДНУ ОШИБКУ
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a
```

В чём вообще идиллия использовать именно `mtl` для многих задач? В том, что мы хотим избавиться от бесконечного количества `lift` в нашем коде для вытаскивания из монад монады и значения, поэтому следует использовать данный пакет. С другой стороны, если у нас есть $n$ монад и $m$ type class'ов, тогда нам нужно написать $n \cdot m$ монадных instance'ев, хотя, большинство из них, достаточно тривиальны.
