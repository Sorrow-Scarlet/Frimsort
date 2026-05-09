namespace Frimsort.Core

/// High-level sorter that orchestrates tiered sorting.
module Sorter =

    /// Generate all four tier dependency graphs from mod metadata.
    let generateDependencyGraphs
        (mods: Map<string, ModMetadata>)
        (activeModIds: Set<string>)
        (useModDependenciesAsLoadBefore: bool)
        : DependencyGraph list =

        let depsGraph = Dependencies.genDepsGraph mods activeModIds
        let revDepsGraph = Dependencies.genRevDepsGraph mods activeModIds

        let tierZeroGraph, _tierZeroMods = Dependencies.genTierZeroDepsGraph depsGraph
        let tierOneGraph, tierOneMods = Dependencies.genTierOneDepsGraph depsGraph mods
        let tierThreeGraph, tierThreeMods = Dependencies.genTierThreeDepsGraph depsGraph revDepsGraph mods

        let tierTwoGraph =
            Dependencies.genTierTwoDepsGraph mods activeModIds tierOneMods tierThreeMods useModDependenciesAsLoadBefore

        [tierZeroGraph; tierOneGraph; tierTwoGraph; tierThreeGraph]

    /// Sort mods using the specified method and dependency graphs.
    let sort
        (method: SortMethod)
        (mods: Map<string, ModMetadata>)
        (graphs: DependencyGraph list)
        : SortResult =

        let sortFn =
            match method with
            | Alphabetical -> AlphabeticalSort.doAlphabeticalSort
            | Topological -> TopoSort.doTopoSort

        try
            let sortedUuids =
                graphs
                |> List.collect (fun graph -> sortFn graph mods)
                // Remove duplicates while preserving order
                |> List.fold (fun (seen, result) uuid ->
                    if Set.contains uuid seen then (seen, result)
                    else (Set.add uuid seen, uuid :: result)
                ) (Set.empty, [])
                |> snd
                |> List.rev

            Success sortedUuids
        with
        | TopoSort.CircularDependencyException cycles ->
            CircularDependencyError cycles

    /// Convenience function: generate graphs and sort in one step.
    let sortMods
        (method: SortMethod)
        (mods: Map<string, ModMetadata>)
        (activeModIds: Set<string>)
        (useModDependenciesAsLoadBefore: bool)
        : SortResult =

        let graphs = generateDependencyGraphs mods activeModIds useModDependenciesAsLoadBefore
        sort method mods graphs
