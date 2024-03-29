# Базовый синтаксис

## Введение

Из основных особенностей языка Haskell можно выделить;

* Статическая типизация
* Иммутабельность (или: неизменяемость объектов)
* Чистота (см. *чистая функция*)
* Отсутствие `null`
* Ленивая модель вычислений (вычисляется тогда и только тогда, когда нужно). Одна из особенностей: можно создавать *бесконечные списки*.

**Чистая функция** - эта та функция, которая зависит только от своих аргументов и на одних и тех же аргументов она всегда выдаёт один и тот же результат. Компилятор может на уровне типов увидеть, насколько чиста данная функция (например, тип `IO` говорит, что функция нечиста).

## Арифметика и типы

Некоторые базовые арифметические выражения, логика и сравнения:

```haskell
ghci> 1 + 2 + 3
7
ghci> 3 / 5 * (7 + 1)
4.8
ghci> 2 ^ 16
65536
ghci> 1 + 1 == 3
False
ghci> 2 + 2 /= 5
True
ghci> 21 * 22 <= 20 * 23
False
ghci> 1 < 2 && 2 < 3
True
ghci> 0 > 0 || 10 >= 10
True
```

Вызов функций: все аргументы разделены через пробельный символы и не обособляются скобками.

```haskell
ghci> not True
False
ghci> div 7 3 -- integral division
2
ghci> max 3 5
5
ghci> min 6 (10 * 2)
6
```

Иногда стоит задумываться о группировках аргументов: в Haskell у применения функции приоритет всегда выше, чем у любого другого оператора.

```haskell
ghci> div 7 3 + 1
3
ghci> div 7 (3 + 1)
1
ghci> div 7 + 1 3
-- *** Compilation error!
ghci> div (7 + 1) 3
2
```

И функции, и операторы мы можем использовать как в *префиксной* записи, так и в *инфиксной*:

* оператор в префиксной записи необходимо обернуть в `()`
* функцию в инфиксной записи необходимо обернуть в `` (кавычки)

```haskell
ghci> 3 + 4
7
ghci> (+) 3 4
7
ghci> mod 7 3
1
ghci> 7 `mod` 3
1
```

Определение переменных (*констант*, так как они неизменяемы). В новых нынешних версиях:

```haskell
ghci> x = 7 + 8
ghci> x * 2
30
```

Помимо целочисленных/нецелочисленных чисел и логических типов в Haskell есть также строковый тип `String`.

```haskell
ghci> greeting = "Hello"
ghci> greeting ++ " world!" -- use `++` instead of `+` to concatenate strings
"Hello world!"
ghci> "DONE: say \"" ++ greeting ++ " world!\""
"DONE: say "Hello world!""
```

В Haskell большое внимание выделяется *типам*. Обычно говорят, что если программа *компилируется*, то она работает правильно. Для определения типа выражения в GHCi используется команда `:t`.

```haskell
ghci> :t 'x'
'x' :: Char
ghci> :t False
False :: Bool
ghci> :t not
not :: Bool -> Bool
ghci> :t (&&)
(&&) :: Bool -> Bool -> Bool
ghci> :t 42
42 :: Num t => t -- числовые константы полиморфны
```

В последнем примере есть интересная деталь: мы не знаем, какой тип `t` в выражении `42 :: Num t => t` до тех пор, пока нам неизвестен *контекст* выражения, то есть, если бы мы передали `42` в функцию, которая принимает `Int`, тогда `t` был бы выведен в `Int`. Это и называется *полиморфизм*.

## Функции

Определим функцию `addMul`, которая будет принимать три числа, умножать второе на третье суммировать с первым.

```haskell
addMul :: Int -> Int -> Int -> Int
addMul x y z = x + y * z
```

Как правильно, для определения функции мы пишем сначала её сигнатуру (или: её тип) - `addMul :: Int -> Int -> Int -> Int` - здесь `::` можно читать как "имеет тип", принимает три `Int` и возвращает тип `Int`; далее само определение: название функции, имена параметров с соответствующем порядке типов, после `=` пишем тело функции.

Или другой пример, функция `greet`: принимаем строку, возвращаем строку, в качестве параметра принимаем `name :: String`.

```haskell
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"
```

Для загрузки модуля в GHCi мы используем команду `:l` для первоначальной и `:r` для перезагрузок, если модуль был изменен.

```haskell
ghci> :l Lecture.hs
ghci> addMul 1 2 3
7
ghci> greet "Haskell"
"Hello, Haskell!"
```

## Операторы

В Haskell можно создавать свои собственные операторы с своими собственными приоритетами (от 0 до 10, сам приоритет в порядке возрастания). Они могут быть как левоассоциативными, так и правоассоциативными, так и не те, и не те (и то другое).

```haskell
-- где-то в пакете `base`
infixr 2 ||
(||) :: Bool -> Bool -> Bool
(||) left right = ...
```

Для определения оператора мы сначала пишем одно из трех ключевых слов `infix`/`infixl`/`infixr`, определяющий *ассоциативность оператора*, затем его *приоритет*, далее *название оператора*, наконец - мы пишем его *тип* и *тело оператора*.

Попробуем задать оператор импликации, у него приоритет ниже, чем у `||`, она правоассоциативна и с названием `==>`. Получаем:

```haskell
infixr 1 ==> -- если оставить только это (убрать тип и тело),
             -- то будет ошибка компиляции
(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b -- `a ==> b` эквивалентно `(==>) a b`
```

Оператор **не может быть не-бинарным**, оператор, по определению, это **бинарная операция**. Также, приоритет **не может быть нецелым числом**, всегда **целое**; максимальный задаваемый приоритет - это **`9`**, оператор `пробел` - *применение функции* - имеет наивысший приоритет **`10`**.

Итого, по ассоцитивности оператор в Haskell получается следующее:

$$
\begin{aligned}
\mathtt{infixl} : a \circ b \circ c \circ d &\equiv ((a \circ b) \circ c) \circ d \\
\mathtt{infixr} : a \circ b \circ c \circ d &\equiv a \circ (b \circ (c \circ d)) \\
\mathtt{infix} : a \circ b \circ c \circ d &\equiv \textit{Compilation error}
\end{aligned}
$$

Для последнего нам придется сами расставлять скобки.

### Тип `List`

Определение списка осуществляется через перечисление его аргументов через запятую.

```haskell
ghci> [1 + 2, 3 + 4, 5 * 6]
[3, 7, 10]
```

Некоторые полезные операторы при работе с списками.

```haskell
ghci> list = [2, 1, 3] -- создали `list` переменную
ghci> [5, 10] ++ list  -- конкатенация двух списков с помощью оператора `++``
[5, 10, 2, 1, 3]       -- напоминание: Haskell иммутабельный,
                       -- переменная `list` не была изменена
ghci> 10 : list        -- второй метод конкатенации,
                       -- когда мы присоединяем к голове списка один элемент
[10, 2, 1, 3]
```

Под капотом при конкатенации списков мы не полностью копируем списки, внутри мы используем древовидный вид списков для более быстрого изменения (как персистентные структуры данных). Работают почти также, как в Clojure.

Ещё некоторые примеры работы для понимания, что `List` иммутабельный.

```haskell
ghci> list
[2, 1, 3]
ghci> reverse list -- не изменяет переменную `list`
[3, 1, 2]
ghci> list
[2, 1, 3]
ghci> anotherList = 5 : list -- новая переменная
ghci> anotherList
[5, 2, 1, 3]
ghci> list
[2, 1, 3]
```

Рассмотрим ещё некоторые примеры при работе с списками.

```haskell
emptyList :: [Int] -- если у функции 0 аргументов, значит, перед нами константа
emptyList = [] -- пустой список

singletonList :: [Int]
singletonList = 1 : emptyList -- или: [1]

listExample :: [Int]
listExample = [2, 1, 3] -- или: 2:1:3:[], на самом деле `[2, 1, 3]`
                        -- это синтаксический сахар

listExample' :: [Int]
listExample' = 5:10:listExample -- или: [5, 10, 2, 1, 3]

twoLists :: [Int]
twoLists = singletonList ++ listExample -- [1, 2, 1, 3]

trinity :: [Int]
trinity = listExample ++ [7] ++ twoLists -- [2, 1, 3, 7, 1, 2, 1, 3]
```

Обратим внимание на знакомый тип `String` и сравним его с `List`.

```haskell
string :: [Char] -- `String` - на самом деле, это [Char]
string = "str" -- ['s', 't', 'r']

otherString :: String -- [Char]
otherString = "other" ++ " " ++ string -- "other str"
```

Поскольку, `String` - это *alias* над списком символом, то довольно очевидно, почему это так:

```haskell
ghci> "" == []
True
-- `""` - это пустой [Char]
-- `[]` - это полиморфный тип, который выводится в [Char]
```

У списков в Java и в других языках программирования можно задать *range*.

```java
IntStream.range(0, 5).toArray(); // {0, 1, 2, 3, 4};
IntStream.rangeClosed(0, 5).toArray(); // {0, 1, 2, 3, 4, 5};
IntStream.iterate(0, x -> x + 2).limit(5).toArray(); // {0, 2, 4, 6, 8};
```

Тоже самое можно задать в Haskell.

```haskell
[0 .. 5]     -- [0, 1, 2, 3, 4, 5], правая граница всегда включена
[1, 3 .. 5]  -- [1, 3, 5, 5], для указания шага достаточно первых двух элементов

[0..]        -- [0, 1, 2, 3, ...] - бесконечный список,
             -- не будет вычисляться честным образом
[0, 2 ..]    -- [0, 2, 4, 6, ...] - все четные числа,
             -- также не будет вычисляться честным образом

[5, 4, .. 1] -- [5, 4, 3, 2, 1], важно, что
[5 .. 1]     -- [] - пустой список
```

Для списков определены некоторые стандартные функции. Рассмотрим для получения доступа к элемента, пусть `list = [2, 1, 3]`.

```haskell
ghci> head list -- возвращает первый элемент из списка; работает за O(1)
2

ghci> tail list -- возвращает все остальные, кроме первого элемента;
                -- работает за O(1)
[1, 3]

ghci> last list -- возвращает последний элемент; работает за O(n)
3

ghci> init list -- возвращает все остальные, кроме последнего; работает за O(n)
[2, 1]
```

**Дисклеймер**: лучше не использовать данные функции, так как они небезопасны в чистых функциях. Например, при пустом списке `head` и `tail` могут бросить исключение.

Рассмотрим функции из модуля `Prelude`.

```haskell
ghci> drop 2 [2, 1, 3] -- если вместо 2 поставить 1000, вернётся пустой список;
                       -- альтернативная реализация `tail`: `drop 1`
[3]

ghci> take 1 [2, 1, 3] -- если вместо 1 поставить 1000, вернётся весь список
[2]

ghci> replicate 3 [1..5] -- может принимать не только `List`
                         -- в качестве второго аргумента
[[1, 2, 3, 4, 5], [1, 2, 3, 4, 5], [1, 2, 3, 4, 5]]

ghci> zip [1, 2, 3] "abc" -- составление соответствующих пар из двух списков, если
                          -- какой-то из них оказался короче, то
                          -- длина результирующего списка = длина короткого
[(1, 'a'), (2, 'b'), (3, 'c')] -- `(1, 'a')` - это пара типа Num t => (t, Char)

ghci> unzip [(5, True), (10, False)] -- делает ровно наоборот в отличии от `zip`
([5, 10], [True, False])

ghci> words "Hello,     Haskell   \t\n\n  world!" -- принимает `String` и делает
                                                  -- разделение по
                                                  -- пробельным символам
["Hello,", "Haskell", "world!"]

ghci> unwords ["Hello,", "Haskell", "world!"] -- делает ровно наоборот в отличии от
                                              -- `words`, в качестве разделителя
                                              -- используется пробел
"Hello, Haskell world!"

ghci> [2, 1, 3] !! 2 -- l !! i = l[i], работает за O(i); не стоит использовать
3 
```

## Конструкции и выражения

**Конструкция** во многих языках программирования - это управляющая конструкция языка, которая не имеет возвращаемого значения и типа. **Выражение** - наоборот - имеет возвращаемое значение. Например, `2 + 3` - это выражение, результат которого `5` арифметического типа; а, например, `if` в C++ - это конструкция, управляющая часть языка, оно не имеет возвращаемого типа и значения.

```c++
void f (); // определение функции - это конструкция

int random() {
    return 4; // `return` - это конструкция, `4` - это выражение
}

int main() {
    int a = random(); // определение переменной - это конструкция,
    // `random()` - это выражение
    if (a > 0) { // `if` - это конструкция, `a > 0` - это выражение
        printf("C++"); // вызов функции без явного контекста может быть как
        // конструкцией, так и выражением
    }
}
```

Большинство конструкций в императивных языках программирования - это выражения в Haskell.

### `let`

Предположим, что у нас есть функция $pythagoras(x, y) = x^2 + y ^2$, которая принимает два целочисленных значения и возвращает сумму их квадратов.

```haskell
pythagoras :: Int -> Int -> Int
pythagoras x y = x^2 + y^2
```

С помощью конструкции вида $\texttt{let}\text{ }bindings\text{ }\texttt{int}\text{ }expression$ мы можем объявить после `let` какие-то промежуточные индификаторы, а после `in` записать выражение, где мы можем применять данные bind'инги.

```haskell
pythagoras :: Int -> Int -> Int
pythagoras x y = let x2 = x ^ 2 -- важное замечание про форматирование:
                                -- только пробелы и только верное их число
                     y2 = y ^ 2
                 in x2 + y2
```

### `where`

Рассмотрим максимально нагруженный пример на Java.

```java
double pythagoras(double a, double b) {
    class Squarer {
        double eval(double x) {
            return x * x;
        }
    }

    final double a2 = new Squarer().eval(a);
    final double b2 = new Squarer().eval(b);

    return a2 + b2;
}
```

Тоже самое мы можем написать в Haskell, используя конструкцию `where`.

```haskell
pythagoras :: Double -> Double -> Double
pythagoras a b = a2 + b2
  where
    square x = x ^ 2
    a2 = square a
    b2 = square b
```

Заметим, что (как и в `let`) в части *binding* мы можем писать не только константы (индификаторы), но и целые функции. То есть, мы объявили функцию `square`, которая возвращает квадрат значения, а затем две константы как вызов функции.

Заметим, что мы не объявляли (хотя, нам никто не запрещает) тип у функции `square` - компилятор сможет сам догадаться по входным значениям. Также, иногда, мы можем опускать типы у самих определений функций (но лучше так не надо делать).

**Важно**: конструкция `where` **не является выражением** - это набор определений, которые относятся к нашим функциям, в отличии от `let`-`in`.

### `if`

Конструкция `if` **является выражением** в отличие от C++ и имеет следующую сигнатуру записи: $\texttt{if}\text{ }predicate\text{ }\texttt{then}\text{ }experession\text{ }\texttt{else}\text{ }expression$. Мы можем использовать его в функциях.

```haskell
factorial :: Integer -> Integer -- `Integer` это неограниченно большое число,
                                -- в отличии от `Int` - платформозависмый
factorial n = if n <= 1
              then 1
              else n * factorial (n - 1)
```

В каком-то плане такой `if` - это *тернарный оператор* как в других языках программирования.

**Важно**: выражения, находящиеся после `then` и `else` всегда **должны быть одинаковых типов**, так как у нас строгая типизация.

### guards

Рассмотрим максимально нагруженный пример на Java.

```java
public int collatzSum(int n) {
    if (n < 0) {
        return 0;
    } else if (n == 1) {
        return 1;
    } else if (n % 2 == 0) {
        return n + collatzSum(n / 2);
    } else {
        return n + collatzSum(3 * n + 1);
    }
}
```

В Haskell мы можем также написать, но для таких лучше использовать так называемые *guard* выражения.

```haskell
collatzSum :: Natural -> Natural
collatzSum n
  | n == 1 = 1
  | even n = n + collatzSum (n `div` 2)
  | otherwise = n + collatzSum(3 * n + 1)
```

Мы также пишем определение функции `collatzSum`, пишем аргумент `n`, однако, вместо `=` и привычного тела мы пишем условия через `|` (вертикальная черта):

* в случае, если `n == 1`, тогда нашим телом будет `1`
* в случае, если `n` четное (`even` - стандартная функция), тогда телом будет `n + collatzSum (div n 2)`
* наконец, в любом другом случае, `n + collatzSum(3 * n + 1)`; `otherwise` - это константа `True`

Проверка условий идет всегда сверху-вниз, то есть если после `otherwise` будет ещё какое-то условие, то мы получим недостижимое тело функции.

### `case`

Рассмотрим максимально нагруженный пример на Java.

```java
public String getFont(int fontConstant) {
    String font = null;
    switch (fontConstant) {
        case 0: font = "PLAIN"; break;
        case 1: font = "BOLD"; break;
        case 2: font = "ITALIC"; break;
        default: font = "UNKNOWN";
    }
    return font;
}
```

В Haskell также есть конструкция `case`, которая на самом деле также **является выражением**. Записывается она вот так: $\texttt{case}\text{ }expression\text{ }\texttt{of}\text{ }\left[pattern\text{ }\texttt{->}\text{ }expression\right]$, то есть мы сопоставляем нечто (первый *expression*) с некоторым из *pattern*'ов и вернуть новый *expression*.

```haskell
getFont :: Int -> String
getFont n = case n of
              0 -> "PLAIN"
              1 -> "BOLD"
              2 -> "ITALIC"
              _ -> "UNKNOWN" -- аналог default, `underscore` - это один из способов
                             -- наименования значения
```

Принципиальная разница между `case` и guard выражениями: guard проверяет *логическую истинность* `Bool`-выражения, в то время как `case` проверяет *соответствие значению-паттернов*. Можем переписать на guard-стиль.

```haskell
getFont :: Int -> String
getFont n
  | n == 0 = "PLAIN"
  | n == 1 = "BOLD"
  | n == 2 = "ITALIC"
  | otherwise = "UNKNOWN"
```

## Функции высшего порядка

В Haskell любая функция - это, так называемый, объект первого класса, то есть мы можем функцию присвоить какому-ту индификатору (или: переменной).

Рассмотрим для примера две простые функции инкремента и декремента.

```haskell
inc, dec :: Int -> Int
inc x = x + 1
dec x = x - 1
```

А теперь введем вот такую функцию.

```haskell
changeTwiceBy :: (Int -> Int) -> Int -> Int
changeTwiceBy operation value = operation (operation value)
```

Заметим, что `(Int -> Int)` в сигнатуре функции - это *тоже функция*, принимающая `Int` и возвращающая `Int`, то есть в качестве аргумента мы будем подавать функцию. Таким образом, функция `changeTwiceBy` становится *функцией высшего порядка*.

**Функция высшего порядка** - это функция, принимающая в качестве аргумента другую функцию.

Пример использования `changeTwiceBy`.

```haskell
ghci> changeTwiceBy inc 5
7

ghci> changeTwiceBy dec 5
3
```

## Анонимные функции

Для тривиальных функций мы можем "на месте" создавать анонимные или, по другому, лямбда функции. Для них определен следующий синтаксис: $\backslash arg_1\text{ }arg_2\text{ }\ldots\text{ }arg_N\text{ }\texttt{->}\text{ }expression$.

```haskell
ghci> changeTwiceBy (\x -> x + 1) 5
7

ghci> changeTwiceBy (\x -> x * 2) 5
20
```

## Полиморфизм

### Параметрический полиморфизм

Как уже было сказано, свойства *полиморфизма* - это, в первую очередь, вывод типа по некоторому контексту. В Java такой полиморфизм называют Generic.

```java
public <T> first(T x, T y) {
    return x;
}
```

Важно отметить, что в данном случае у нас используется так называемый *параметрический полиморфизм*, то есть когда на разных типах (в данном случае: Generic'ах) мы получаем *одинаковое* поведение. В Haskell реализовано из коробки и называют обычно *типовой переменной*.

```haskell
first :: a -> a -> a
first x y = x
```

Заметим, что в Java у нас есть также интерфейсы, которые гарантирует некоторую работу классов, реализующие их, например `ArrayList<T>` по одному методу кладёт элемент через метод `add(T)`, в то время как `LinkedList<T>` - по другому. В общем случае мы называем это *специальным полиморфизмом*. В Haskell специальный полиморфизм представлен механизмом *классы типов* или *type class*.

Примеры параметрических полиморфных функций.

```haskell
id :: a -> a
id x = x

fst :: (a, b) -> a
snd :: (a, b) -> b

emptyList :: [a]
emptyList = []

repeatThree :: a -> [a]
repeatThree x = [x, x, x]

tripleApply :: (a -> a -> a) -> a -> a
tripleApply f x = f (f x x) (f x x)
```

### Полиморфные функции высшего порядка

Рассмотрим функцию из других языков программирования.

```haskell
map :: (a -> b) -> [a] -> [b]
```

Почему она - функция высшего порядка? Потому что она принимает в качестве аргумента другую функцию. Почему она полиморфна? Потому что мы имеем полиморфизм в виде типов `a` и `b`.

Другие примеры.

```haskell
filter :: (a -> Bool) -> [a] -> [a]
-- фильтрация по предикату

foldr1 :: (a -> a -> a) -> [a] -> a
-- свёртка без начального аккумулирующего элемента

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- более общая функция в отличии от `zip`
-- (где в качестве параметра передано составление пар)
```

Некоторые примеры использования.

```haskell
ghci> map negate [2, 1, 3]
[-2, -1, -3]

ghci> filter odd [1, 2, 3, 4, 5]
[1, 3, 5]

ghci> foldr1 (+) [1, 2, 4]  -- sum [1, 2, 4]
7

ghci> takeWhile isUpper "HTMLXml"
"HTMLX"

ghci> zipWith max [1..5] [5, 4 .. 1]
[5, 4, 3, 4, 5]
```

Рассмотрим ещё полиморфные функции высшего порядка с кареткой.

```haskell
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f p = f (fst p) (snd p)
-- принимает бинарную функцию, пару и возвращает тип `c`
-- мы сначала распаковываем элементы пары, затем их кормим в бинарную функцию

curry :: ((a, b) -> c) -> a -> b -> c
curry f l r = f (l, r)
-- делаем ровно наоборот: мы запаковываем в пару, а затем применяем на ней функцию
```

## Каррирование, частичное применение

На самом деле, функции в Haskell бывают только двух видов:

* ноль аргументов - это константа
* не ноль аргументов - это функция с одним аргументом, которая либо возвращает функцию/функцию от функции, либо константу

Рассмотрим функцию `foo` в двух нотациях. Помним, что оператор `->` - правоассоциативен.

```haskell
foo :: Int -> Char -> Double -> String
-- расставим скобки
foo :: Int -> (Char -> (Double -> (String)))
-- получили функцию `foo`, которая принимает `Int` и возвращает функцию от `Char`
```

Это также работает для функций высшего порядка.

```haskell
bar :: (Int -> Int) -> Int -> Int
-- что эквивалентно...
bar :: (Int -> Int) -> (Int -> (Int))
```

Рассмотрим стандартную функцию `div`, на самом деле её сигнатура для `Int` типа будет `div :: Int -> (Int -> Int)` - принимает `Int` и возвращает функцию. Тогда, заведем `div7By`.

```haskell
ghci> div7By = div 7
-- `div7By` - это частично применённое `div`
div7By :: Int -> Int
-- теперь это унарная функцию, аргументом которого на самом деле служит
-- вторым аргументом функции `div`, первый - это 7

ghci> div7By' = div (7 :: Int)
-- для полиморфных типов в Haskell мы можем явно указать нужный тип
-- `div7By'` полностью эквивалентен `div7By`
```

Некоторые примеры применения `div7By`.

```haskell
ghci> div7By 2
3

ghci> div7By 3
2
-- эквивалентно...
ghci> (div 7) 3
2
```

Частичное применение также позволяет работать с операторами, если один из аргументов закреплён и известен на этапе компиляции.

```haskell
ghci> map (div 7) [1..7]
[7, 3, 2, 1, 1, 1, 1]

ghci> map (+2) [2, 1, 3]
[4, 3, 5]

ghci> filter (<3) [1..5]
[1, 2]

ghci> filter (3<) [1..5]
[4, 5]
```

Замечание по поводу оператора `-` в карринге.

```haskell
ghci> map (5-) [1..5]
[4, 3, 2, 1, 0]

-- парсер компилятора не может понять, что это - функция,
-- считая это - константой `-5`
ghci> map (-5) [1..5]

    • Non type-variable argument in the constraint: Num (a -> b)
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall a b. (Enum a, Num (a -> b), Num a) => [b]
-- на этот случай люди придумали `subtract`
ghci> subtract 5 3
-2

ghci> map (subtract 5) [1..5]
[-4, -3, -2, -1, 0]
```

## Функция `flip`

Данная функция принимает функцию от двух аргументов и возвращает функцию, с переставленными аргументами.

```haskell
flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b
```

Например, выше описанный `subtract` - это `flip` с минусом: мы получаем ожидаемое поведение от `(-5)`, то есть мы передаем, например, `5`, тогда после после применения с вторым аргументом мы получаем `<значение> - 5`.

## Сопоставление с образцом, или Pattern Matching

Научимся писать функцию факториала новым способом через *pattern matching*. Суть в pattern matching заключается в том, что для каждого образца под который подходит наш элемент применяется своё тело функции - как с guard, только там мы проверяли логическую истинность.

```haskell
fact :: Integer -> Integer
fact 0 = 1 -- явное сравнение на равенство
fact n = n * fact (n - 1) -- почти тот самый otherwise

stringLit :: String -> String
stringLit "such" = "pattern"
stringLit "much" = "amaze"
stringLit _ = "wow" -- здесь underscore, так как мы не используем переменную
```

Более сложная махинация с списком, сложная для воссоздания в guard-стиле.

```haskell
sumList3 :: [Int] -> Int
sumList3 [x, y, z] = x + y + z -- слева именно образец под трех-элементный список
sumList3 _ = 0

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs -- [2, 1, 3] == 2 : 1 : 3 : []
-- применили к x :: a и присоединили голову к рекурсивному вызову map (a -> b) [a]

dropWhile :: (a -> Bool) -> [a] -> [b]
dropWhile _ [] = []
dropWhile p l@(x:xs) = if p x then dropWhile p xs else l
-- здесь появляется новый элемент синтаксиса - @, на самом деле,
-- мы можем задать ему имя, если мы хотим вернуть тот же список,
-- при этом не повторяя x:xs
-- так называемый alias
```

Заметим, что pattern matching очень схож на конструкцию `case` и, на самом деле, под капотом первое транслируется во второе.

## Расширение языка

Ещё одно отличие языка Haskell от других - это возможность расширяться на месте в синтаксическом плане, то есть то, что раньше не компилировалось, потому что это было не верно с точки зрения парсера, мы можем сделать верным.

Для подключения *прагмы* в GHCi мы используем `:set -X<NameOfPragma>`, в модулях же - `{-# LANGUAGE NameOfPragma #-}`.

### `TupleSections`

Вместо...

```haskell
ghci> map (\x -> (42, x)) [1..5]
[(42,1), (42,2), (42,3), (42,4), (42,5)]

ghci> map (\x -> (x, 42)) [1..5]
[(1,42), (2,42), (3,42), (4,42), (5,42)]
```

... мы можем писать...

```haskell
ghci> map (42,) [1..5]
[(42,1), (42,2), (42,3), (42,4), (42,5)]

ghci> map (,42) [1..5]
[(1,42), (2,42), (3,42), (4,42), (5,42)]
```

### `LambdaCase`

Для функций с длинными именами бывает не очень удобно сопоставлять pattern matching.

```haskell
veryLongFunctionName :: Int -> String
veryLongFunctionName 0 = foo
veryLongFunctionName 1 = bar
veryLongFunctionName n = baz n
```

Вместо этого мы можем написать специальную форму `case` выражения, которая транслируется в ту же принятую нами `case <expression> of`.

```haskell
veryLongFunctionName :: Int -> String
veryLongFunctionName = \case
    0 -> foo
    1 -> bar
    n -> baz n
```

### `ViewPatterns`

Очень полезно для функций, где мы работаем с списками после применения функций.

```haskell
exactTwoWords :: String -> Bool
exactTwoWords s = case words s of
                    [_, _] -> True
                    _      -> False
```

Можно записать так.

```haskell
exactTwoWords :: String -> Bool
exactTwoWords (words -> [_, _]) = True
exactTwoWords _                 = False
```

## Применение функций

Как уже обсуждалось, в Haskell - всё есть функция, даже пробельный символ - это функция применения функции. Давайте заведём оператор `$` следующим образом.

```haskell
infixr 0 $
($) :: (a -> b) -> a -> b
f $ x = f x
```

У него самый низкий приоритет, в качестве аргумента он берёт функцию, аргумент и возвращает функцию - тоже самое, что и несуществующие на уровне конструкций или объект пробельный символ. Однако, здесь отличие в том, что у `$` *самый низкий приоритет*, то есть он будет применяться *позже всех*, благодаря чему мы можем избавиться от Clojure-стиля, убрав скобки в каких-то местах и подставив туда `$`.

```haskell
foo, bar :: [Int] -> Int
foo list = length (filter odd (map (div 2) (filter even (map (div 7) list))))
bar list = length $ filter odd $ map (div 2) $ filter even $ map (div 7) list
```

Когда это ещё полезно? Например, давайте заведем список функций (скажем для простоты `Integer -> Integer`) и список целочисленных чисел, мы хотим к каждому элементу применить соответствующую функцию - вспоминаем про `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`. Здесь нам здорово поможет оператор `$`.

```haskell
ghci> let funs = [(+2), div 7, (*5)]
funs :: [Integer -> Integer]

ghci> let vals = [20, 30, 40]
vals :: [Integer]

ghci> :t zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

ghci> zipWith ($) funs vals
[22, 0, 200]
```

А если хотим как в любом другом языке программирования? То есть, мы хотим видеть данные и как в C++ через точку указывать необходимые методы. Для этого в `Data.Function` завезли оператор `&`, который по сути является `flip ($)`.

```haskell
infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x
```

Полезно для тех, кто читает слева направо, а не наоборот.

```haskell
ghci> (\l -> l ++ reverse l) [2, 1, 3]
[2, 1, 3, 3, 1, 2]

ghci> [2, 1, 3] & \l -> l ++ reverse l
[2, 1, 3, 3, 1, 2]
```

## Оператор композиции

Для выведения композиции двух функции используется готовый оператор `.` с наименьшим приоритетом.

```haskell
infixr 9 .
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)
```

Про данный оператор можно думать в две стороны: с одной стороны они принимает две функции и возвращает функцию, с другой - принимает две функции и аргумент `a`, тогда мы бы последовательно применяли функции.

## Генерация списков

Подобно Python мы умеем генерировать списки довольно декларативным способом.

```haskell
ghci> [x | x <- [1..10], even x]
[2, 4, 6, 7, 8, 10]
```

То есть, мы говорим, что у нас есть список из элементов `x`, где (вертикальная черта) `x` пробегает по элементам `[1..10]`, если `even x`.

Другие примеры генератора списков.

```haskell
ghci> [if even x then "!" else "?" | x <- [1 .. 5]] 
["?", "!", "?", "!", "?"]

ghci> [ x * y | x <- [1, 3, 5], y <- [2, 4, 6], x * y >= 10]
[12, 18, 10, 20, 30]

ghci> [13 | even 13]  -- conditionally create singleton list
[]

ghci> [14 | even 14]
[14]

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) 
    = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]
```

## Ленивое вычисление

### Порядок вычисления

Haskell вычисляет свои аргументы/выражения только тогда, когда это нужно. По сути, все выражения мы выстраиваем в виде графа, где мы точно знаем, какой и где тип выражения, как операция и так далее - здесь мы можем легко сравнить одно поддерево с другим - если они одинаковы, тогда мы *редуцируем* наше двойное вычисление в одинарное - просто похороним ссылку на ту же вычисленное значение в памяти и будем его использовать. Например, `(1 + 2) * (1 + 2)` - здесь один раз посчитается выражение `(1 + 2)` и затем его результат, а он где-то лежит в памяти, мы будем переиспользовать чтобы умножить на самого себя. Тоже самое и для списка, например, `1:2:3:[]` - это применение `:` к `1` и вершине, содержащий применение `:` к `2` и вершине... Такие подвешенные части мы обычно называем *thunk*, те части, где мы можем вычислить (то есть, не является листом), мы называем *redex* (или: *reducible expression*).

### Формы выражений

В деревьях вычисления нам могут попадаться разные виды выражений, в том числе и *формы*, либо требующие, либо не требующие отдельного вычисления.

* *Normal form*. Все подвыражения вычислены на этапе построения дерева.

    ```haskell
    42
    ```

    ```haskell
    (2, "Hello")
    ```

    ```haskell
    \x -> (x + 1) -- здесь мы смотрим на весь объект, а не на `x + 1`
    ```

* *No NF*. Одно или несколько подвыражений требуют вычисления.

    ```haskell
    1 + 2
    ```

    ```haskell
    (\x -> x + 1) 2 -- вот здесь мы явно применяем `x + 1` к `2`
    ```

    ```haskell
    "he" ++ "llo"
    ```

    ```haskell
    (1 + 1, 2 + 2)
    ```

Говорят, что выражение находится в **слабой головной нормальной форме**, если это - либо *конструктор*, либо *лямбда*. Любая NF - это WHNF.

* *WHNF*.

    ```haskell
    10
    ```

    ```haskell
    2 : [1, 3]
    ```

    ```haskell
    'h' : ("e" ++ "llo")
    ```

    ```haskell
    [4, length undefined]
    ```

    ```haskell
    Just (2 + 7)
    ```

    ```haskell
    (1 + 2, 3 + 4)
    ```

    ```haskell
    \x -> x + 10
    ```

    ```haskell
    \xx -> foldr (+) zx [1, 2, 3]
    ```

* *No WHNF*.

    ```haskell
    1 + 1
    ```

    ```haskell
    1 + (2 + 3)
    ```

    ```haskell
    (\x -> x + 10) 3
    ```

    ```haskell
    length [1, 2, 3]
    ```

    ```haskell
    Just (2 + 7) >>= \n -> Just (n * 2)
    ```

**Важно**: когда мы делал pattern matching по значению, Haskell **вычисляет его до WHNF**.
