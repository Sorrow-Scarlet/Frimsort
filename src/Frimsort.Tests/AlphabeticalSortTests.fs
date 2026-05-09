module Frimsort.Tests.AlphabeticalSortTests

open Xunit
open Frimsort.Core
open Frimsort.Tests.TestHelpers

[<Fact>]
let ``alphabetical sort empty graph returns empty`` () =
    let mods: Map<string, ModMetadata> = Map.empty
    let graph: DependencyGraph = Map.empty
    let result = AlphabeticalSort.doAlphabeticalSort graph mods
    Assert.Empty(result)

[<Fact>]
let ``alphabetical sort single mod`` () =
    let mods = buildModsMap [
        ("uuid-a", makeMod "mod.a" "Alpha" [])
    ]
    let graph = Map.ofList [("mod.a", Set.empty)]
    let result = AlphabeticalSort.doAlphabeticalSort graph mods
    Assert.Equal(1, result.Length)
    Assert.Equal("uuid-a", result.[0])

[<Fact>]
let ``alphabetical sort multiple mods no dependencies`` () =
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
    let result = AlphabeticalSort.doAlphabeticalSort graph mods
    Assert.Equal(3, result.Length)
    Assert.Equal("uuid-a", result.[0])  // Apple
    Assert.Equal("uuid-m", result.[1])  // Mango
    Assert.Equal("uuid-z", result.[2])  // Zebra

[<Fact>]
let ``alphabetical sort respects dependencies`` () =
    // Apple depends on Zebra, so Zebra should come before Apple
    let mods = buildModsMap [
        ("uuid-z", makeMod "mod.z" "Zebra" [])
        ("uuid-a", makeMod "mod.a" "Apple" [("mod.z", true)])
    ]
    let graph = Map.ofList [
        ("mod.z", Set.empty)
        ("mod.a", set ["mod.z"])
    ]
    let result = AlphabeticalSort.doAlphabeticalSort graph mods
    Assert.Equal(2, result.Length)
    let idxZ = result |> List.findIndex (fun x -> x = "uuid-z")
    let idxA = result |> List.findIndex (fun x -> x = "uuid-a")
    Assert.True(idxZ < idxA)

[<Fact>]
let ``alphabetical sort chain dependency`` () =
    // C depends on B, B depends on A
    let mods = buildModsMap [
        ("uuid-c", makeMod "mod.c" "Alpha" [("mod.b", true)])
        ("uuid-b", makeMod "mod.b" "Beta" [("mod.a", true)])
        ("uuid-a", makeMod "mod.a" "Charlie" [])
    ]
    let graph = Map.ofList [
        ("mod.c", set ["mod.b"])
        ("mod.b", set ["mod.a"])
        ("mod.a", Set.empty)
    ]
    let result = AlphabeticalSort.doAlphabeticalSort graph mods
    Assert.Equal(3, result.Length)
    let idxA = result |> List.findIndex (fun x -> x = "uuid-a")
    let idxB = result |> List.findIndex (fun x -> x = "uuid-b")
    let idxC = result |> List.findIndex (fun x -> x = "uuid-c")
    // A must come before B, B must come before C (dependency order)
    Assert.True(idxA < idxB)
    Assert.True(idxB < idxC)

[<Fact>]
let ``alphabetical sort case insensitive`` () =
    let mods = buildModsMap [
        ("uuid-1", makeMod "mod.1" "ZEBRA" [])
        ("uuid-2", makeMod "mod.2" "apple" [])
        ("uuid-3", makeMod "mod.3" "Mango" [])
    ]
    let graph = Map.ofList [
        ("mod.1", Set.empty)
        ("mod.2", Set.empty)
        ("mod.3", Set.empty)
    ]
    let result = AlphabeticalSort.doAlphabeticalSort graph mods
    Assert.Equal(3, result.Length)
    Assert.Equal("uuid-2", result.[0])  // apple
    Assert.Equal("uuid-3", result.[1])  // Mango
    Assert.Equal("uuid-1", result.[2])  // ZEBRA

[<Fact>]
let ``alphabetical sort with diamond dependency`` () =
    // D depends on B and C; B and C depend on A
    let mods = buildModsMap [
        ("uuid-d", makeMod "mod.d" "Delta" [("mod.b", true); ("mod.c", true)])
        ("uuid-b", makeMod "mod.b" "Beta" [("mod.a", true)])
        ("uuid-c", makeMod "mod.c" "Charlie" [("mod.a", true)])
        ("uuid-a", makeMod "mod.a" "Alpha" [])
    ]
    let graph = Map.ofList [
        ("mod.d", set ["mod.b"; "mod.c"])
        ("mod.b", set ["mod.a"])
        ("mod.c", set ["mod.a"])
        ("mod.a", Set.empty)
    ]
    let result = AlphabeticalSort.doAlphabeticalSort graph mods
    Assert.Equal(4, result.Length)
    let idxA = result |> List.findIndex (fun x -> x = "uuid-a")
    let idxB = result |> List.findIndex (fun x -> x = "uuid-b")
    let idxC = result |> List.findIndex (fun x -> x = "uuid-c")
    let idxD = result |> List.findIndex (fun x -> x = "uuid-d")
    Assert.True(idxA < idxB)
    Assert.True(idxA < idxC)
    Assert.True(idxB < idxD)
    Assert.True(idxC < idxD)

[<Fact>]
let ``alphabetical sort only includes mods in graph`` () =
    let mods = buildModsMap [
        ("uuid-a", makeMod "mod.a" "Alpha" [])
        ("uuid-b", makeMod "mod.b" "Beta" [])
    ]
    // Only mod.a is in the graph
    let graph = Map.ofList [("mod.a", Set.empty)]
    let result = AlphabeticalSort.doAlphabeticalSort graph mods
    Assert.Equal(1, result.Length)
    Assert.Equal("uuid-a", result.[0])

[<Fact>]
let ``alphabetical sort dependency not in graph is ignored`` () =
    // mod.a depends on mod.x which is not in the graph
    let mods = buildModsMap [
        ("uuid-a", makeMod "mod.a" "Alpha" [("mod.x", true)])
    ]
    let graph = Map.ofList [("mod.a", set ["mod.x"])]
    let result = AlphabeticalSort.doAlphabeticalSort graph mods
    // mod.a should still appear
    Assert.Equal(1, result.Length)
    Assert.Equal("uuid-a", result.[0])
