namespace Interp.Core

type Point = { X: float; Y: float }

type Algorithm =
    | Linear
    | Newton of int

type Output = { Algorithm: string; X: float; Y: float }
