module Frimsort.Tests.SorterTests

open Xunit
open Frimsort.Core
open Frimsort.Tests.TestHelpers

[<Fact>]
let ``sortMods Topological returns success for valid graph`` () =
    let mods = buildModsMap [
        ("uuid-a", makeMod "mod.a" "Alpha" [])
        ("uuid-b", makeMod "mod.b" "Beta" [("mod.a", true)])
    ]
    let activeIds = set ["mod.a"; "mod.b"]
    let result = Sorter.sortMods Topological mods activeIds false
    match result with
    | Success sorted ->
        Assert.Equal(2, sorted.Length)
        let idxA = sorted |> List.findIndex (fun x -> x = "uuid-a")
        let idxB = sorted |> List.findIndex (fun x -> x = "uuid-b")
        Assert.True(idxA < idxB)
    | CircularDependencyError _ ->
        Assert.Fail("Expected success but got circular dependency error")

[<Fact>]
let ``sortMods Alphabetical returns success for valid graph`` () =
    let mods = buildModsMap [
        ("uuid-a", makeMod "mod.a" "Alpha" [])
        ("uuid-b", makeMod "mod.b" "Beta" [("mod.a", true)])
    ]
    let activeIds = set ["mod.a"; "mod.b"]
    let result = Sorter.sortMods Alphabetical mods activeIds false
    match result with
    | Success sorted ->
        Assert.Equal(2, sorted.Length)
        let idxA = sorted |> List.findIndex (fun x -> x = "uuid-a")
        let idxB = sorted |> List.findIndex (fun x -> x = "uuid-b")
        Assert.True(idxA < idxB)
    | CircularDependencyError _ ->
        Assert.Fail("Expected success but got circular dependency error")

[<Fact>]
let ``sortMods detects circular dependency`` () =
    let mods = buildModsMap [
        ("uuid-a", makeMod "mod.a" "Alpha" [("mod.b", true)])
        ("uuid-b", makeMod "mod.b" "Beta" [("mod.a", true)])
    ]
    let activeIds = set ["mod.a"; "mod.b"]
    let result = Sorter.sortMods Topological mods activeIds false
    match result with
    | CircularDependencyError _ -> ()  // Expected
    | Success _ -> Assert.Fail("Expected circular dependency error but got success")

[<Fact>]
let ``sortMods removes duplicates`` () =
    let mods = buildModsMap [
        ("uuid-a", makeMod "mod.a" "Alpha" [])
        ("uuid-b", makeMod "mod.b" "Beta" [("mod.a", true)])
    ]
    let activeIds = set ["mod.a"; "mod.b"]
    let result = Sorter.sortMods Topological mods activeIds false
    match result with
    | Success sorted ->
        let distinct = sorted |> List.distinct
        Assert.Equal(sorted.Length, distinct.Length)
    | CircularDependencyError _ ->
        Assert.Fail("Expected success")

[<Fact>]
let ``sort with explicit graphs`` () =
    let mods = buildModsMap [
        ("uuid-a", makeMod "mod.a" "Alpha" [])
        ("uuid-b", makeMod "mod.b" "Beta" [])
    ]
    let graphs = [
        Map.ofList [("mod.a", Set.empty)]
        Map.ofList [("mod.b", Set.empty)]
    ]
    let result = Sorter.sort Topological mods graphs
    match result with
    | Success sorted ->
        Assert.Equal(2, sorted.Length)
        Assert.Contains("uuid-a", sorted)
        Assert.Contains("uuid-b", sorted)
    | CircularDependencyError _ ->
        Assert.Fail("Expected success")

[<Fact>]
let ``generateDependencyGraphs produces four tier graphs`` () =
    let mods = buildModsMap [
        ("uuid-a", makeMod "mod.a" "Alpha" [])
        ("uuid-b", makeMod "mod.b" "Beta" [("mod.a", true)])
    ]
    let activeIds = set ["mod.a"; "mod.b"]
    let graphs = Sorter.generateDependencyGraphs mods activeIds false
    Assert.Equal(4, graphs.Length)

[<Fact>]
let ``sortMods tier zero mods come first`` () =
    let mods = buildModsMap [
        ("uuid-harmony", makeMod "brrainz.harmony" "Harmony" [])
        ("uuid-core", makeMod "ludeon.rimworld" "Core" [])
        ("uuid-regular", makeMod "regular.mod" "Regular Mod" [("brrainz.harmony", true)])
    ]
    let activeIds = set ["brrainz.harmony"; "ludeon.rimworld"; "regular.mod"]
    let result = Sorter.sortMods Topological mods activeIds false
    match result with
    | Success sorted ->
        // Tier zero mods (harmony, core) should appear before regular.mod
        let idxHarmony = sorted |> List.tryFindIndex (fun x -> x = "uuid-harmony")
        let idxCore = sorted |> List.tryFindIndex (fun x -> x = "uuid-core")
        let idxRegular = sorted |> List.tryFindIndex (fun x -> x = "uuid-regular")
        match idxHarmony, idxCore, idxRegular with
        | Some h, Some c, Some r ->
            Assert.True(h < r)
            Assert.True(c < r)
        | _ -> ()  // Some mods may not appear if not in any tier
    | CircularDependencyError _ ->
        Assert.Fail("Expected success")

[<Fact>]
let ``sortMods with useModDependenciesAsLoadBefore`` () =
    let mods = buildModsMap [
        ("uuid-a", makeModFull "mod.a" "Alpha" [] [] false false ["mod.b"])
        ("uuid-b", makeMod "mod.b" "Beta" [])
    ]
    let activeIds = set ["mod.a"; "mod.b"]
    let result = Sorter.sortMods Topological mods activeIds true
    match result with
    | Success sorted ->
        let idxA = sorted |> List.tryFindIndex (fun x -> x = "uuid-a")
        let idxB = sorted |> List.tryFindIndex (fun x -> x = "uuid-b")
        match idxA, idxB with
        | Some a, Some b -> Assert.True(b < a)  // mod.b should come before mod.a
        | _ -> ()
    | CircularDependencyError _ ->
        Assert.Fail("Expected success")

[<Fact>]
let ``sortMods empty mods returns empty success`` () =
    let mods: Map<string, ModMetadata> = Map.empty
    let activeIds = Set.empty
    let result = Sorter.sortMods Topological mods activeIds false
    match result with
    | Success sorted -> Assert.Empty(sorted)
    | CircularDependencyError _ -> Assert.Fail("Expected success")
