module Frimsort.Tests.TopoSortTests

open Xunit
open Frimsort.Core
open Frimsort.Tests.TestHelpers

[<Fact>]
let ``toposort empty graph returns empty`` () =
    let graph: DependencyGraph = Map.empty
    let result = TopoSort.toposort graph
    Assert.Empty(result)

[<Fact>]
let ``toposort single node no dependencies`` () =
    let graph = Map.ofList [("a", Set.empty)]
    let result = TopoSort.toposort graph
    Assert.Equal(1, result.Length)
    Assert.Contains("a", result.[0])

[<Fact>]
let ``toposort linear chain`` () =
    // c depends on b, b depends on a
    let graph = Map.ofList [
        ("a", Set.empty)
        ("b", set ["a"])
        ("c", set ["b"])
    ]
    let result = TopoSort.toposort graph
    // Flatten levels to get order
    let flat = result |> List.collect Set.toList
    let idxA = flat |> List.findIndex (fun x -> x = "a")
    let idxB = flat |> List.findIndex (fun x -> x = "b")
    let idxC = flat |> List.findIndex (fun x -> x = "c")
    Assert.True(idxA < idxB)
    Assert.True(idxB < idxC)

[<Fact>]
let ``toposort diamond dependency`` () =
    // d depends on b and c; b and c depend on a
    let graph = Map.ofList [
        ("a", Set.empty)
        ("b", set ["a"])
        ("c", set ["a"])
        ("d", set ["b"; "c"])
    ]
    let result = TopoSort.toposort graph
    let flat = result |> List.collect Set.toList
    let idxA = flat |> List.findIndex (fun x -> x = "a")
    let idxB = flat |> List.findIndex (fun x -> x = "b")
    let idxC = flat |> List.findIndex (fun x -> x = "c")
    let idxD = flat |> List.findIndex (fun x -> x = "d")
    Assert.True(idxA < idxB)
    Assert.True(idxA < idxC)
    Assert.True(idxB < idxD)
    Assert.True(idxC < idxD)

[<Fact>]
let ``toposort multiple independent nodes at same level`` () =
    let graph = Map.ofList [
        ("a", Set.empty)
        ("b", Set.empty)
        ("c", Set.empty)
    ]
    let result = TopoSort.toposort graph
    // All should be in the same level
    Assert.Equal(1, result.Length)
    Assert.Equal(3, result.[0].Count)

[<Fact>]
let ``toposort circular dependency raises exception`` () =
    // a depends on b, b depends on a
    let graph = Map.ofList [
        ("a", set ["b"])
        ("b", set ["a"])
    ]
    Assert.Throws<TopoSort.CircularDependencyException>(fun () ->
        TopoSort.toposort graph |> ignore)

[<Fact>]
let ``toposort complex circular dependency raises exception`` () =
    // a -> b -> c -> a
    let graph = Map.ofList [
        ("a", set ["c"])
        ("b", set ["a"])
        ("c", set ["b"])
    ]
    Assert.Throws<TopoSort.CircularDependencyException>(fun () ->
        TopoSort.toposort graph |> ignore)

[<Fact>]
let ``toposort nodes appearing only as dependencies`` () =
    // b depends on a, but a is not a key in the graph
    let graph = Map.ofList [
        ("b", set ["a"])
    ]
    let result = TopoSort.toposort graph
    let flat = result |> List.collect Set.toList
    Assert.Contains("a", flat)
    Assert.Contains("b", flat)
    let idxA = flat |> List.findIndex (fun x -> x = "a")
    let idxB = flat |> List.findIndex (fun x -> x = "b")
    Assert.True(idxA < idxB)

[<Fact>]
let ``doTopoSort returns UUIDs in dependency order`` () =
    let mods = buildModsMap [
        ("uuid-a", makeMod "mod.a" "Alpha Mod" [])
        ("uuid-b", makeMod "mod.b" "Beta Mod" [("mod.a", true)])
        ("uuid-c", makeMod "mod.c" "Charlie Mod" [("mod.b", true)])
    ]
    let graph = Map.ofList [
        ("mod.a", Set.empty)
        ("mod.b", set ["mod.a"])
        ("mod.c", set ["mod.b"])
    ]
    let result = TopoSort.doTopoSort graph mods
    Assert.Equal(3, result.Length)
    Assert.Equal("uuid-a", result.[0])
    Assert.Equal("uuid-b", result.[1])
    Assert.Equal("uuid-c", result.[2])

[<Fact>]
let ``doTopoSort sorts same level alphabetically by name`` () =
    let mods = buildModsMap [
        ("uuid-z", makeMod "mod.z" "Zebra" [])
        ("uuid-a", makeMod "mod.a" "Apple" [])
        ("uuid-m", makeMod "mod.m" "Mango" [])
    ]
    let graph = Map.ofList [
        ("mod.z", Set.empty)
        ("mod.a", Set.empty)
        ("mod.m", Set.empty)
    ]
    let result = TopoSort.doTopoSort graph mods
    Assert.Equal(3, result.Length)
    Assert.Equal("uuid-a", result.[0])  // Apple
    Assert.Equal("uuid-m", result.[1])  // Mango
    Assert.Equal("uuid-z", result.[2])  // Zebra

[<Fact>]
let ``doTopoSort handles mods not in graph`` () =
    // Mod C has packageId not in graph
    let mods = buildModsMap [
        ("uuid-a", makeMod "mod.a" "Alpha" [])
        ("uuid-c", makeMod "mod.c" "Charlie" [])
    ]
    let graph = Map.ofList [
        ("mod.a", Set.empty)
    ]
    let result = TopoSort.doTopoSort graph mods
    // Only mod.a should be in the result
    Assert.Equal(1, result.Length)
    Assert.Equal("uuid-a", result.[0])

[<Fact>]
let ``doTopoSort mixed levels with alphabetical within each`` () =
    let mods = buildModsMap [
        ("uuid-1", makeMod "mod.1" "Framework" [])
        ("uuid-2", makeMod "mod.2" "Zebra Patch" [("mod.1", true)])
        ("uuid-3", makeMod "mod.3" "Alpha Patch" [("mod.1", true)])
    ]
    let graph = Map.ofList [
        ("mod.1", Set.empty)
        ("mod.2", set ["mod.1"])
        ("mod.3", set ["mod.1"])
    ]
    let result = TopoSort.doTopoSort graph mods
    Assert.Equal(3, result.Length)
    Assert.Equal("uuid-1", result.[0])  // Framework (level 0)
    Assert.Equal("uuid-3", result.[1])  // Alpha Patch (level 1, alphabetically first)
    Assert.Equal("uuid-2", result.[2])  // Zebra Patch (level 1, alphabetically second)

[<Fact>]
let ``findCycles detects simple cycle`` () =
    let graph = Map.ofList [
        ("a", set ["b"])
        ("b", set ["a"])
    ]
    let cycles = TopoSort.findCycles graph
    Assert.NotEmpty(cycles)

[<Fact>]
let ``findCycles returns empty for acyclic graph`` () =
    let graph = Map.ofList [
        ("a", Set.empty)
        ("b", set ["a"])
    ]
    let cycles = TopoSort.findCycles graph
    Assert.Empty(cycles)
