# Функциональное программирование

Подготовительные билеты к экзамену по лекционным материалам курса **Функциональное программирование** (5 семестр). В билетах встречаются ошибки и недосказанности, а также имеются проблемы с согласованностью общего стиля ввиду разного виденья мира разных летописцев.

## Структура проекта

В репозитории используются две основные ветки: `main` и `dev`.

* `main` — ветка с версиями выпущенных редакцией билетов, рекомендуется для чтения.
* `dev` — ветка с версиями до сих пор находящихся в разработке и доработке билетов, используется для [контрибуции](#о-контрибуции) и написания билетов.

## Билеты

Все билеты написаны в формате [Markdown](https://en.wikipedia.org/wiki/Markdown), которые можно скомпилировать в PDF формат с помощью [`pandoc`](https://github.com/jgm/pandoc). Через двоеточие указан автор данного билета.

### Основные билеты

* [Билет №1-2: Basic Syntax](sources/L2-BasicSyntax.md): [@sfbakturin](https://github.com/sfbakturin)
* [Билет №3: Datas, Classes, Instances](sources/L3-DatasClassesInstances.md): [@Nomad192](https://github.com/Nomad192)
* [Билет №4: Basic typeclasses: Monoid. Functor. Applicative](sources/L4-MonoidFunctorApplicative.md): [@sfbakturin](https://github.com/sfbakturin)
* [Билет №5: Monads](sources/L5-Monads.md): [@sfbakturin](https://github.com/sfbakturin)
* [Билет №6: RealWorld](sources/L6-RealWorld.md): [@sfbakturin](https://github.com/sfbakturin)
* [Билет №7: Monad Transformers](sources/L7-MonadTransformers.md): [@sfbakturin](https://github.com/sfbakturin)
* [Билет №8: Speeding up Haskell](sources/L8-SpeedingUpHaskell.md): [@sfbakturin](https://github.com/sfbakturin)
* [Билет №9: Parallel and Concurrent Haskell](sources/L9-ParallelConcurrent.md): [@Nomad192](https://github.com/Nomad192)
* [Билет №11: Brand new DSL world](sources/L11-BrandNewDSLWorld.md): [@sfbakturin](https://github.com/sfbakturin)
* [Билет №12: Some fun with kinds](sources/L12-Kinds.md): [@SotnikovMaksim](https://github.com/SotnikovMaksim)
* [Билет №13: Comonads](sources/L13-Comonads.md): [@sfbakturin](https://github.com/sfbakturin)

### Дополнительные билеты

* [Билет №5.5: Parsers Combinators](sources/L5-Parsers.md): [@sfbakturin](https://github.com/sfbakturin)

*Обратите внимание*: в силу особенностей отображения $\LaTeX$ в веб-версии GitHub, некоторые части билетов могут выглядеть странно или нечитаемо. Рекомендуется [прочитать про сборку](#сборка).

## Сборка

Необходимые зависимости.

* Транслятор разметки Pandoc, не ниже 2.9.2.1
* Компилятор системы сборки XeTeX, из TeX Live 2022

Сборка на Linux/Mac OS.

```bash
bash build.sh # сборка и трансляция Markdown документов в PDF формат
              # после, в директории `sources` должны появиться PDF билеты
```
