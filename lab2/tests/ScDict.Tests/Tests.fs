module Tests

open global.FsCheck
open ScDict
open global.Xunit

[<Fact>]
let ``empty is empty`` () =
    let dict = ScDict.empty
    Assert.True(ScDict.isEmpty dict)
    Assert.Equal(0, ScDict.size dict)

[<Fact>]
let ``add and find returns value`` () =
    let dict = ScDict.empty |> ScDict.add "a" 10
    Assert.Equal(Some 10, ScDict.tryFind "a" dict)

[<Fact>]
let ``remove deletes key`` () =
    let dict =
        ScDict.empty
        |> ScDict.add "a" 1
        |> ScDict.add "b" 2
        |> ScDict.remove "a"
    Assert.Equal(None, ScDict.tryFind "a" dict)
    Assert.Equal(Some 2, ScDict.tryFind "b" dict)

[<Fact>]
let ``map transforms values`` () =
    let dict = ScDict.empty |> ScDict.add "a" 1 |> ScDict.add "b" 2
    let mapped = ScDict.map (fun _ v -> v * 10) dict
    Assert.Equal(Some 10, ScDict.tryFind "a" mapped)
    Assert.Equal(Some 20, ScDict.tryFind "b" mapped)

[<Fact>]
let ``filter keeps matching pairs`` () =
    let dict = ScDict.empty |> ScDict.add "a" 1 |> ScDict.add "b" 2
    let filtered = ScDict.filter (fun _ v -> v = 2) dict
    Assert.Equal(None, ScDict.tryFind "a" filtered)
    Assert.Equal(Some 2, ScDict.tryFind "b" filtered)

[<Fact>]
let ``append prefers right value`` () =
    let left = ScDict.empty |> ScDict.add "x" 1
    let right = ScDict.empty |> ScDict.add "x" 2
    let merged = ScDict.append left right
    Assert.Equal(Some 2, ScDict.tryFind "x" merged)

[<FsCheck.Xunit.Property>]
let ``monoid identity left`` (pairs: (int * int) list) =
    let dict = ScDict.ofSeq pairs
    ScDict.equals dict (ScDict.append ScDict.empty dict)

[<FsCheck.Xunit.Property>]
let ``monoid identity right`` (pairs: (int * int) list) =
    let dict = ScDict.ofSeq pairs
    ScDict.equals dict (ScDict.append dict ScDict.empty)

[<FsCheck.Xunit.Property>]
let ``monoid associativity`` (aPairs: (int * int) list) (bPairs: (int * int) list) (cPairs: (int * int) list) =
    let a = ScDict.ofSeq aPairs
    let b = ScDict.ofSeq bPairs
    let c = ScDict.ofSeq cPairs
    let left = ScDict.append (ScDict.append a b) c
    let right = ScDict.append a (ScDict.append b c)
    ScDict.equals left right

[<FsCheck.Xunit.Property>]
let ``add then find returns value`` (pairs: (int * int) list) (key: int) (value: int) =
    let dict = ScDict.ofSeq pairs
    ScDict.tryFind key (ScDict.add key value dict) = Some value
