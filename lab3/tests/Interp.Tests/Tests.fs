module Tests

open System
open Interp.Core
open global.FsCheck
open global.Xunit

[<Fact>]
let ``linear interpolation hits endpoints`` () =
    let p0 = { X = 0.0; Y = 2.0 }
    let p1 = { X = 4.0; Y = 10.0 }
    let y0 = Interpolation.linear p0 p1 p0.X
    let y1 = Interpolation.linear p0 p1 p1.X
    Assert.Equal(p0.Y, y0, 8)
    Assert.Equal(p1.Y, y1, 8)

[<Fact>]
let ``newton interpolation hits all points`` () =
    let points = [ { X = 0.0; Y = 1.0 }; { X = 1.0; Y = 2.0 }; { X = 2.0; Y = 5.0 } ]
    points
    |> List.iter (fun p ->
        let y = Interpolation.newton points p.X
        Assert.Equal(p.Y, y, 8))

[<Fact>]
let ``linear streaming uses global step`` () =
    let points = [ { X = 0.0; Y = 0.0 }; { X = 1.0; Y = 1.0 }; { X = 2.0; Y = 2.0 }; { X = 3.0; Y = 3.0 } ]
    let outputs =
        Streaming.linearStream 0.7 points
        |> Seq.map (fun p -> Math.Round(p.X, 1))
        |> Seq.toList
    Assert.Equal<float list>([ 0.0; 0.7; 1.4; 2.1; 2.8 ], outputs)

[<FsCheck.Xunit.Property>]
let ``linear endpoints property`` (x0: float) (x1: float) (y0: float) (y1: float) =
    if Double.IsNaN x0 || Double.IsNaN x1 || Double.IsNaN y0 || Double.IsNaN y1 then true
    elif Double.IsInfinity x0 || Double.IsInfinity x1 || Double.IsInfinity y0 || Double.IsInfinity y1 then true
    else
    let clamp v = max -1e6 (min 1e6 v)
    let x0' = clamp x0
    let x1' = clamp x1
    let y0' = clamp y0
    let y1' = clamp y1
    let valid =
        x0' <> x1'
    if not valid then true
    else
        let p0 = { X = x0'; Y = y0' }
        let p1 = { X = x1'; Y = y1' }
        let yStart = Interpolation.linear p0 p1 x0'
        let yEnd = Interpolation.linear p0 p1 x1'
        abs (yStart - y0') < 1e-7 && abs (yEnd - y1') < 1e-7

[<FsCheck.Xunit.Property>]
let ``newton interpolation reproduces data`` (points: (float * float) list) =
    let clamp v = max -1e3 (min 1e3 v)
    let clean =
        points
        |> List.filter (fun (x, y) ->
            not (Double.IsNaN x || Double.IsNaN y || Double.IsInfinity x || Double.IsInfinity y))
        |> List.map (fun (x, y) -> clamp x, clamp y)
        |> List.distinctBy fst
        |> List.sortBy fst
        |> List.truncate 4
    let spaced =
        clean
        |> List.fold (fun acc (x, y) ->
            match acc with
            | [] -> [ (x, y) ]
            | (prevX, _) :: _ when abs (x - prevX) < 1e-3 -> acc
            | _ -> (x, y) :: acc) []
        |> List.rev
    if List.length spaced < 2 then true
    else
        let pts = spaced |> List.map (fun (x, y) -> { X = x; Y = y })
        pts
        |> List.forall (fun p -> abs (Interpolation.newton pts p.X - p.Y) < 1e-4)
