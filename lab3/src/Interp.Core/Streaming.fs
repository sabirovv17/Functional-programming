namespace Interp.Core

open System

module Streaming =
    let private epsilon = 1e-9

    let private generateOutputs nextX endX step eval =
        let rec loop x acc =
            if x > endX + epsilon then
                (List.rev acc, x)
            else
                loop (x + step) ({ X = x; Y = eval x } :: acc)
        loop nextX []

    let private alignNextX nextX startX =
        if nextX < startX - epsilon then startX else nextX

    type LinearState = { Prev: Point option; NextX: float option; Step: float }
    type NewtonState = { Window: Point list; NextX: float option; Step: float; WindowSize: int }

    let initialLinear step = { Prev = None; NextX = None; Step = step }
    let initialNewton step windowSize = { Window = []; NextX = None; Step = step; WindowSize = windowSize }

    let updateLinear state (point: Point) : LinearState * Point list =
        match state.Prev with
        | None ->
            ({ state with Prev = Some point; NextX = Some point.X }, [])
        | Some p0 ->
            if point.X < p0.X - epsilon then
                invalidOp "Input points must be sorted by X"
            let startX = p0.X
            let endX = point.X
            let nextX = alignNextX (state.NextX |> Option.defaultValue startX) startX
            let eval x = Interpolation.linear p0 point x
            let outputs, newNextX = generateOutputs nextX endX state.Step eval
            ({ state with Prev = Some point; NextX = Some newNextX }, outputs)

    let updateNewton state (point: Point) : NewtonState * Point list =
        let window' =
            let updated = state.Window @ [ point ]
            if List.length updated > state.WindowSize then updated |> List.tail else updated
        let nextX =
            match state.NextX with
            | Some value -> value
            | None ->
                match window' with
                | h :: _ -> h.X
                | [] -> point.X
        if List.length window' >= state.WindowSize then
            let startX = window'.Head.X
            let endX = window'.[window'.Length - 1].X
            let alignedNextX = alignNextX nextX startX
            let eval x = Interpolation.newton window' x
            let outputs, newNextX = generateOutputs alignedNextX endX state.Step eval
            ({ state with Window = window'; NextX = Some newNextX }, outputs)
        else
            ({ state with Window = window'; NextX = Some nextX }, [])

    let linearStream (step: float) (points: seq<Point>) : seq<Point> =
        points
        |> Seq.scan (fun (state, _) point -> updateLinear state point) (initialLinear step, [])
        |> Seq.collect snd

    let newtonStream (step: float) (windowSize: int) (points: seq<Point>) : seq<Point> =
        points
        |> Seq.scan (fun (state, _) point -> updateNewton state point) (initialNewton step windowSize, [])
        |> Seq.collect snd
