# Функциональное программирование

Подготовительные билеты к экзамену по лекционным материалам курса **Функциональное программирование** (5 семестр). В билетах встречаются ошибки и недосказанности, а также имеются проблемы с согласованностью общего стиля ввиду разного виденья мира разных летописцев.

## Структура проекта

В репозитории используются две основные ветки: `main` и `dev`.

* `main` — ветка с версиями выпущенных редакцией билетов, рекомендуется для чтения.
* `dev` — ветка с версиями до сих пор находящихся в разработке и доработке билетов, используется для [контрибуции](#о-контрибуции) и написания билетов.

## Билеты

Все билеты написаны в формате [Markdown](https://en.wikipedia.org/wiki/Markdown), которые можно скомпилировать в PDF формат с помощью [`pandoc`](https://github.com/jgm/pandoc). Через двоеточие указан автор данного билета.

### Основные билеты

* [Билет №1-2: Basic Syntax](lectures/L2-BasicSyntax.md): [@sfbakturin](https://github.com/sfbakturin)
* [Билет №3: Datas, Classes, Instances](lectures/L3-DatasClassesInstances.md): [@Nomad192](https://github.com/Nomad192)
* [Билет №4: Basic typeclasses: Monoid. Functor. Applicative](lectures/L4-MonoidFunctorApplicative.md): [@sfbakturin](https://github.com/sfbakturin)
* [Билет №5: Monads](lectures/L5-Monads.md): [@sfbakturin](https://github.com/sfbakturin)
* [Билет №6: RealWorld](lectures/L6-RealWorld.md): [@sfbakturin](https://github.com/sfbakturin)
* [Билет №7: Monad Transformers](lectures/L7-MonadTransformers.md): [@sfbakturin](https://github.com/sfbakturin)
* [Билет №8: Speeding up Haskell](lectures/L8-SpeedingUpHaskell.md): [@sfbakturin](https://github.com/sfbakturin)
* [Билет №9: Parallel and Concurrent Haskell](lectures/L9-ParallelConcurrent.md): [@Nomad192](https://github.com/Nomad192)
* [Билет №11: Brand new DSL world](lectures/L11-BrandNewDSLWorld.md): [@sfbakturin](https://github.com/sfbakturin)
* [Билет №12: Some fun with kinds](lectures/L12-Kinds.md): [@SotnikovMaksim](https://github.com/SotnikovMaksim)
* [Билет №13: Comonads](lectures/L13-Comonads.md): [@sfbakturin](https://github.com/sfbakturin)

### Дополнительные билеты

* [Билет №5.5: Parsers Combinators](lectures/L5-Parsers.md): [@sfbakturin](https://github.com/sfbakturin)

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

## О контрибуции

Вы также можете дополнить/исправить тот или иной билет, в случае обнаружения ошибок.

Инструкция.

1. Создать [fork](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/working-with-forks/fork-a-repo) репозитория.
2. Закоммитить изменения билета/билетов на ветку **`dev`**.
3. Создать Pull Request с описанием того, что было изменено, и дожидаться ответа и/или комментариев от других пользователей (все Pull Request'ы, предлагающие сделать объединение на ветку `main` не будут рассматриваться).
