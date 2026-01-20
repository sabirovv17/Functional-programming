//Problem 29: Distinct Powers

let minA, maxA = 2, 100
let minB, maxB = 2, 100

//Хвостовая рекурсия
let solveWithTailRecursion () =
    //Set для хранения уникальных значений
    let rec loopA a acc =
        if a > maxA then acc
        else
            let rec loopB b innerAcc =
                if b > maxB then innerAcc
                else
                    let value = pown (bigint a) b
                    loopB (b + 1) (Set.add value innerAcc)
            loopA (a + 1) (loopB minB acc)
    loopA minA Set.empty |> Set.count

//Обычная рекурсия
// После рекурсивного вызова выполняется операция объединения множеств
let solveWithRecursion () =
    let rec getPowersForA a =
        if a > maxA then Set.empty
        else
            let rec getPowersForB b =
                if b > maxB then Set.empty
                else
                    let value = pown (bigint a) b
                    Set.add value (getPowersForB (b + 1))
            Set.union (getPowersForB minB) (getPowersForA (a + 1))
    getPowersForA minA |> Set.count

// Альтернативная версия - рекурсивно собираем список
let solveWithRecursion2 () =
    let rec collectPowers a b =
        if a > maxA then []
        elif b > maxB then collectPowers (a + 1) minB
        else (pown (bigint a) b) :: collectPowers a (b + 1)
    
    collectPowers minA minB
    |> Set.ofList
    |> Set.count

// 3. Модульная реализация (генерация, фильтрация, свёртка)
let solveModular () =
    // Генерация всех пар (a, b)
    let generatePairs () : (int * int) list =
        [ for a in minA..maxA do
            for b in minB..maxB do
                yield (a, b) ]
    
    // Отображение пар в значения степеней
    let computePowers (pairs: (int * int) list) =
        pairs |> List.map (fun (a, b) -> pown (bigint a) b)
    
    // "Фильтрация" уникальных значений (через Set)
    let filterDistinct values =
        values |> Set.ofList
    
    // Свёртка - подсчёт количества
    let countElements set =
        Set.fold (fun acc _ -> acc + 1) 0 set
    
    // Композиция
    generatePairs ()
    |> computePowers
    |> filterDistinct
    |> countElements

//Генерация последовательности при помощи отображения (map)
let solveWithMap () =
    //список оснований
    let bases = [minA..maxA]
    
    //список показателей
    let exponents = [minB..maxB]
    
    //отображаем в список степеней
    let allPowers =
        bases
        |> List.map (fun a ->
            exponents
            |> List.map (fun b -> pown (bigint a) b))
        |> List.concat
    
    allPowers |> Set.ofList |> Set.count

// Альтернативная версия с List.collect (flatMap)
let solveWithMap2 () =
    [minA..maxA]
    |> List.collect (fun a ->
        [minB..maxB]
        |> List.map (fun b -> pown (bigint a) b))
    |> Set.ofList
    |> Set.count

//Цикл
let solveWithLoop () =
    [minA..maxA]
    |> List.collect (fun a ->
        [minB..maxB]
        |> List.map (fun b -> pown (bigint a) b))
    |> Set.ofList
    |> Set.count

//List comprehension
let solveWithComprehension () =
    let values = 
        [ for a in minA..maxA do
            for b in minB..maxB do
                yield pown (bigint a) b ]
    values |> Set.ofList |> Set.count

//Ленивая последовательность
let solveWithLazySequence () =
    // Бесконечная последовательность всех пар (a, b) начиная с (2, 2)
    let allPairs = seq {
        for a in Seq.initInfinite (fun i -> i + minA) do
            for b in minB..maxB do
                yield (a, b)
    }
    
    // Берём только те пары, где a <= maxA
    allPairs
    |> Seq.takeWhile (fun (a, _) -> a <= maxA)
    |> Seq.map (fun (a, b) -> pown (bigint a) b)
    |> Set.ofSeq
    |> Set.count

//ленивая генерация степеней
let solveWithLazySequence2 () =
    let powersOf (a: int) =
        Seq.init (maxB - minB + 1) (fun i -> pown (bigint a) (i + minB))
    
    // Генерируем все степени для всех оснований
    seq { minA..maxA }
    |> Seq.collect (fun a ->
        powersOf a)
    |> Set.ofSeq
    |> Set.count

//полностью ленивое вычисление
let solveWithLazySequence3 () =
    let lazyPowers = seq {
        for a in minA..maxA do
            for b in minB..maxB do
                yield pown (bigint a) b
    }
    lazyPowers |> Set.ofSeq |> Set.count


printfn "Project Euler - Problem 29: Distinct Powers"
printfn "============================================"
printfn "Range: a ∈ [%d, %d], b ∈ [%d, %d]" minA maxA minB maxB
printfn ""

printfn "1. Хвостовая рекурсия:           %d" (solveWithTailRecursion ())
printfn "2. Обычная рекурсия:             %d" (solveWithRecursion ())
printfn "2a. Обычная рекурсия (альт.):    %d" (solveWithRecursion2 ())
printfn "3. Модульная реализация:         %d" (solveModular ())
printfn "4. С использованием map:         %d" (solveWithMap ())
printfn "4a. Map (альтернативная):        %d" (solveWithMap2 ())
printfn "5. С циклом for:                 %d" (solveWithLoop ())
printfn "5a. List comprehension:          %d" (solveWithComprehension ())
printfn "6. Ленивая последовательность:   %d" (solveWithLazySequence ())
printfn "6a. Ленивая (альтернативная):    %d" (solveWithLazySequence2 ())
printfn "6b. Ленивая (seq expression):    %d" (solveWithLazySequence3 ())
