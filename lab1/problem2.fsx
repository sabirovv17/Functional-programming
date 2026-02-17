//Even Fibonacci Numbers

let limit = 4_000_000

//хвостоявая рекурсия
let solveWithTailRecursion () =
    let rec loop a b acc =
        if a > limit then acc
        elif a % 2 = 0 then loop b (a + b) (acc + a)
        else loop b (a + b) acc
    loop 1 2 0

//обычная рекурсия
let solveWithRecursion () =
    let rec fib n =
        if n <= 1 then n
        else fib (n - 1) + fib (n - 2)
    
    //находим числа фибоначчи до лимита и + чётные
    let rec sumEvenFib n acc =
        let f = fib n
        if f > limit then acc
        elif f % 2 = 0 then sumEvenFib (n + 1) (acc + f)
        else sumEvenFib (n + 1) acc
    sumEvenFib 1 0

//нехвостовая рекурсия
let solveWithRecursion2 () =
    let rec sumEvenFibFrom a b =
        if a > limit then 0
        elif a % 2 = 0 then a + sumEvenFibFrom b (a + b)
        else sumEvenFibFrom b (a + b)
    sumEvenFibFrom 1 2

//генерация -> фильтрация -> свёртка
let solveModular () =
    let generateFibonacci () =
        let rec gen a b acc =
            if a > limit then List.rev acc
            else gen b (a + b) (a :: acc)
        gen 1 2 []
    
    let filterEven = List.filter (fun x -> x % 2 = 0)
    
    let sumAll = List.fold (+) 0
    
    generateFibonacci ()
    |> filterEven
    |> sumAll

//генерация последовательности при помощи мапы
//unfold для генерации и map для преобразования
let solveWithMap () =
    let fibSequence =
        Seq.unfold (fun (a, b) -> 
            if a > limit then None 
            else Some(a, (b, a + b))) (1, 2)
        |> Seq.toList
    
    // map: чётные -> значение; нечётные -> 0
    fibSequence
    |> List.map (fun x -> if x % 2 = 0 then x else 0)
    |> List.sum

//использование map для индексации
let solveWithMap2 () =
    let indices = [1..50]
    
    //вычисление нного числа Фибоначчи
    let fibAt n =
        let rec loop i a b =
            if i = n then a
            else loop (i + 1) b (a + b)
        loop 1 1 2
    
    //индексы -> Фибоначчи
    indices
    |> List.map fibAt
    |> List.takeWhile (fun x -> x <= limit)
    |> List.filter (fun x -> x % 2 = 0)
    |> List.sum

//циклы
let solveWithLoop () =
    let fibs =
        Seq.unfold (fun (a, b) ->
            if a > limit then None
            else Some(a, (b, a + b))) (1, 2)

    fibs
    |> Seq.filter (fun x -> x % 2 = 0)
    |> Seq.sum

//for с sequence expression
let solveWithForLoop () =
    let fibs =
        Seq.unfold (fun (a, b) ->
            if a > limit then None
            else Some(a, (b, a + b))) (1, 2)

    fibs
    |> Seq.fold (fun acc x -> if x % 2 = 0 then acc + x else acc) 0

//ленивые последовательности (Seq)
let solveWithLazySequence () =
    //бесконечная последовательность
    let fibonacciSeq =
        Seq.unfold (fun (a, b) -> Some(a, (b, a + b))) (1, 2)
    
    fibonacciSeq
    |> Seq.takeWhile (fun x -> x <= limit)
    |> Seq.filter (fun x -> x % 2 = 0)
    |> Seq.sum

//версия с sequence expression
let solveWithLazySequence2 () =
    let rec fibSeq a b = seq {
        yield a
        yield! fibSeq b (a + b)
    }
    
    fibSeq 1 2
    |> Seq.takeWhile (fun x -> x <= limit)
    |> Seq.filter (fun x -> x % 2 = 0)
    |> Seq.sum


printfn "Project Euler - Problem 2: Even Fibonacci Numbers"
printfn "=================================================="
printfn "Limit: %d" limit
printfn ""

printfn "1. Хвостовая рекурсия:           %d" (solveWithTailRecursion ())
printfn "2. Обычная рекурсия:             %d" (solveWithRecursion ())
printfn "2a. Обычная рекурсия (альт.):    %d" (solveWithRecursion2 ())
printfn "3. Модульная реализация:         %d" (solveModular ())
printfn "4. С использованием map:         %d" (solveWithMap ())
printfn "4a. Map (альтернативная):        %d" (solveWithMap2 ())
printfn "5. С циклом while:               %d" (solveWithLoop ())
printfn "5a. С циклом for:                %d" (solveWithForLoop ())
printfn "6. Ленивая последовательность:   %d" (solveWithLazySequence ())
printfn "6a. Ленивая (альтернативная):    %d" (solveWithLazySequence2 ())
