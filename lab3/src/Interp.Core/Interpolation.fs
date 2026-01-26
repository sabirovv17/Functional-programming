namespace Interp.Core

module Interpolation =
    let linear (p0: Point) (p1: Point) (x: float) =
        let t = (x - p0.X) / (p1.X - p0.X)
        p0.Y + t * (p1.Y - p0.Y)

    let newtonCoefficients (points: Point list) =
        let xs = points |> List.map (fun p -> p.X)
        let ys = points |> List.map (fun p -> p.Y)

        let rec build level values acc =
            match values with
            | [] -> List.rev acc
            | v :: _ ->
                let next =
                    values
                    |> List.pairwise
                    |> List.mapi (fun i (a, b) ->
                        let denom = xs[i + level] - xs[i]
                        (b - a) / denom)
                build (level + 1) next (v :: acc)

        build 1 ys []

    let evalNewton (xs: float list) (coeffs: float list) (x: float) =
        match coeffs with
        | [] -> 0.0
        | _ ->
            let xsArray = xs |> List.toArray
            let coeffsArray = coeffs |> List.toArray
            let rec loop i acc =
                if i < 0 then acc
                else loop (i - 1) (coeffsArray[i] + (x - xsArray[i]) * acc)
            loop (coeffsArray.Length - 2) coeffsArray[coeffsArray.Length - 1]

    let newton (points: Point list) (x: float) =
        let xs = points |> List.map (fun p -> p.X)
        let coeffs = newtonCoefficients points
        evalNewton xs coeffs x
