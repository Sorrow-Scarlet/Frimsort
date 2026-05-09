module Frimsort.Tests.DependenciesTests

open Xunit
open Frimsort.Core
open Frimsort.Tests.TestHelpers

[<Fact>]
let ``genDepsGraph empty mods returns empty graph`` () =
    let mods: Map<string, ModMetadata> = Map.empty
    let activeIds = Set.empty
    let graph = Dependencies.genDepsGraph mods activeIds
    Assert.True(graph.IsEmpty)

[<Fact>]
let ``genDepsGraph builds correct dependency graph`` () =
    let mods = buildModsMap [
        ("uuid-a", makeMod "mod.a" "Alpha" [])
        ("uuid-b", makeMod "mod.b" "Beta" [("mod.a", true)])
        ("uuid-c", makeMod "mod.c" "Charlie" [("mod.a", true); ("mod.b", true)])
    ]
    let activeIds = set ["mod.a"; "mod.b"; "mod.c"]
    let graph = Dependencies.genDepsGraph mods activeIds
    Assert.Equal<Set<string>>(Set.empty, graph.["mod.a"])
    Assert.Equal<Set<string>>(set ["mod.a"], graph.["mod.b"])
    Assert.Equal<Set<string>>(set ["mod.a"; "mod.b"], graph.["mod.c"])

[<Fact>]
let ``genDepsGraph filters out inactive dependencies`` () =
    let mods = buildModsMap [
        ("uuid-a", makeMod "mod.a" "Alpha" [("mod.x", true)])  // mod.x is not active
        ("uuid-b", makeMod "mod.b" "Beta" [("mod.a", true)])
    ]
    let activeIds = set ["mod.a"; "mod.b"]  // mod.x is not here
    let graph = Dependencies.genDepsGraph mods activeIds
    Assert.Equal<Set<string>>(Set.empty, graph.["mod.a"])  // mod.x filtered out
    Assert.Equal<Set<string>>(set ["mod.a"], graph.["mod.b"])

[<Fact>]
let ``genRevDepsGraph builds reverse dependency graph`` () =
    let mods = buildModsMap [
        ("uuid-a", makeModWithAfter "mod.a" "Alpha" [("mod.b", true)])
        ("uuid-b", makeModWithAfter "mod.b" "Beta" [])
    ]
    let activeIds = set ["mod.a"; "mod.b"]
    let graph = Dependencies.genRevDepsGraph mods activeIds
    Assert.Equal<Set<string>>(set ["mod.b"], graph.["mod.a"])
    Assert.Equal<Set<string>>(Set.empty, graph.["mod.b"])

[<Fact>]
let ``getDependenciesRecursive finds transitive dependencies`` () =
    let graph = Map.ofList [
        ("a", Set.empty)
        ("b", set ["a"])
        ("c", set ["b"])
        ("d", set ["c"])
    ]
    let processed = ref Set.empty
    let deps = Dependencies.getDependenciesRecursive "d" graph processed
    Assert.Contains("c", deps)
    Assert.Contains("b", deps)
    Assert.Contains("a", deps)

[<Fact>]
let ``getDependenciesRecursive handles circular reference without infinite loop`` () =
    let graph = Map.ofList [
        ("a", set ["b"])
        ("b", set ["a"])
    ]
    let processed = ref Set.empty
    let deps = Dependencies.getDependenciesRecursive "a" graph processed
    // Should not loop infinitely, should return b (and not loop back to a again)
    Assert.Contains("b", deps)

[<Fact>]
let ``genTierZeroDepsGraph extracts known tier zero mods`` () =
    let graph = Map.ofList [
        ("brrainz.harmony", Set.empty)
        ("ludeon.rimworld", Set.empty)
        ("some.other.mod", set ["brrainz.harmony"])
    ]
    let tierZeroGraph, tierZeroMods = Dependencies.genTierZeroDepsGraph graph
    Assert.Contains("brrainz.harmony", tierZeroMods)
    Assert.Contains("ludeon.rimworld", tierZeroMods)
    Assert.DoesNotContain("some.other.mod", tierZeroMods)

[<Fact>]
let ``genTierZeroDepsGraph includes dependencies of tier zero mods`` () =
    // Harmony depends on prepatcher (both are tier zero)
    let graph = Map.ofList [
        ("zetrith.prepatcher", Set.empty)
        ("brrainz.harmony", set ["zetrith.prepatcher"])
        ("some.mod", set ["brrainz.harmony"])
    ]
    let _, tierZeroMods = Dependencies.genTierZeroDepsGraph graph
    Assert.Contains("zetrith.prepatcher", tierZeroMods)
    Assert.Contains("brrainz.harmony", tierZeroMods)
    Assert.DoesNotContain("some.mod", tierZeroMods)

[<Fact>]
let ``genTierOneDepsGraph includes loadTop mods`` () =
    let mods = buildModsMap [
        ("uuid-fw", makeModFull "my.framework" "My Framework" [] [] true false [])
        ("uuid-x", makeMod "other.mod" "Other" [])
    ]
    let graph = Map.ofList [
        ("my.framework", Set.empty)
        ("other.mod", set ["my.framework"])
    ]
    let _, tierOneMods = Dependencies.genTierOneDepsGraph graph mods
    Assert.Contains("my.framework", tierOneMods)
    Assert.DoesNotContain("other.mod", tierOneMods)

[<Fact>]
let ``genTierOneDepsGraph includes known tier one mods`` () =
    let mods = buildModsMap [
        ("uuid-hub", makeMod "unlimitedhugs.hugslib" "HugsLib" [])
    ]
    let graph = Map.ofList [
        ("unlimitedhugs.hugslib", Set.empty)
    ]
    let _, tierOneMods = Dependencies.genTierOneDepsGraph graph mods
    Assert.Contains("unlimitedhugs.hugslib", tierOneMods)

[<Fact>]
let ``genTierThreeDepsGraph includes loadBottom mods`` () =
    let mods = buildModsMap [
        ("uuid-rm", makeModFull "my.bottom.mod" "Bottom Mod" [] [] false true [])
        ("uuid-x", makeMod "other.mod" "Other" [])
    ]
    let graph = Map.ofList [
        ("my.bottom.mod", Set.empty)
        ("other.mod", Set.empty)
    ]
    let revGraph = Map.ofList [
        ("my.bottom.mod", Set.empty)
        ("other.mod", Set.empty)
    ]
    let _, tierThreeMods = Dependencies.genTierThreeDepsGraph graph revGraph mods
    Assert.Contains("my.bottom.mod", tierThreeMods)
    Assert.DoesNotContain("other.mod", tierThreeMods)

[<Fact>]
let ``genTierTwoDepsGraph excludes tier one and tier three mods`` () =
    let mods = buildModsMap [
        ("uuid-1", makeMod "tier.one.mod" "Tier One" [])
        ("uuid-2", makeMod "regular.mod" "Regular" [("tier.one.mod", true)])
        ("uuid-3", makeMod "tier.three.mod" "Tier Three" [])
    ]
    let activeIds = set ["tier.one.mod"; "regular.mod"; "tier.three.mod"]
    let tierOneMods = set ["tier.one.mod"]
    let tierThreeMods = set ["tier.three.mod"]
    let graph = Dependencies.genTierTwoDepsGraph mods activeIds tierOneMods tierThreeMods false
    Assert.True(graph.ContainsKey("regular.mod"))
    Assert.False(graph.ContainsKey("tier.one.mod"))
    Assert.False(graph.ContainsKey("tier.three.mod"))

[<Fact>]
let ``genTierTwoDepsGraph strips references to tier one mods from dependencies`` () =
    let mods = buildModsMap [
        ("uuid-1", makeMod "tier.one.mod" "Tier One" [])
        ("uuid-2", makeMod "regular.mod" "Regular" [("tier.one.mod", true); ("another.regular", true)])
        ("uuid-3", makeMod "another.regular" "Another" [])
    ]
    let activeIds = set ["tier.one.mod"; "regular.mod"; "another.regular"]
    let tierOneMods = set ["tier.one.mod"]
    let tierThreeMods = Set.empty
    let graph = Dependencies.genTierTwoDepsGraph mods activeIds tierOneMods tierThreeMods false
    // regular.mod's dependency on tier.one.mod should be stripped
    Assert.DoesNotContain("tier.one.mod", graph.["regular.mod"])
    Assert.Contains("another.regular", graph.["regular.mod"])

[<Fact>]
let ``genTierTwoDepsGraph with useModDependenciesAsLoadBefore`` () =
    let mods = buildModsMap [
        ("uuid-a", makeModFull "mod.a" "Alpha" [] [] false false ["mod.b"])
        ("uuid-b", makeMod "mod.b" "Beta" [])
    ]
    let activeIds = set ["mod.a"; "mod.b"]
    let graph = Dependencies.genTierTwoDepsGraph mods activeIds Set.empty Set.empty true
    // mod.a should have mod.b as a dependency (inferred from About.xml deps)
    Assert.Contains("mod.b", graph.["mod.a"])

[<Fact>]
let ``genTierTwoDepsGraph explicit rules override inferred`` () =
    // mod.a has explicit loadBefore pointing to mod.b
    // mod.b has About.xml dependency on mod.a (which would create a conflict)
    let mods = buildModsMap [
        ("uuid-a", makeMod "mod.a" "Alpha" [("mod.b", true)])
        ("uuid-b", makeModFull "mod.b" "Beta" [] [] false false ["mod.a"])
    ]
    let activeIds = set ["mod.a"; "mod.b"]
    let graph = Dependencies.genTierTwoDepsGraph mods activeIds Set.empty Set.empty true
    // mod.a's explicit rule says mod.b loads before mod.a
    Assert.Contains("mod.b", graph.["mod.a"])
    // mod.b's inferred rule (mod.a as dep) should be IGNORED due to conflict
    Assert.DoesNotContain("mod.a", graph.["mod.b"])

[<Fact>]
let ``getReverseDependenciesRecursive finds all dependents`` () =
    let revGraph = Map.ofList [
        ("a", set ["b"; "c"])
        ("b", set ["d"])
        ("c", Set.empty)
        ("d", Set.empty)
    ]
    let deps = Dependencies.getReverseDependenciesRecursive "a" revGraph
    Assert.Contains("b", deps)
    Assert.Contains("c", deps)
    Assert.Contains("d", deps)
