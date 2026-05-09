namespace Frimsort.Core

/// Module for generating dependency graphs from mod metadata.
module Dependencies =

    /// Known tier zero mods (Core, DLCs, essential low-level patches).
    let knownTierZeroMods =
        set
            [ "zetrith.prepatcher"
              "brrainz.harmony"
              "brrainz.visualexceptions"
              "ludeon.rimworld"
              "ludeon.rimworld.royalty"
              "ludeon.rimworld.ideology"
              "ludeon.rimworld.biotech"
              "ludeon.rimworld.anomaly"
              "ludeon.rimworld.odyssey" ]

    /// Known tier one mods (Frameworks that many mods depend on).
    let knownTierOneMods =
        set
            [ "adaptive.storage.framework"
              "aoba.framework"
              "aoba.exosuit.framework"
              "ebsg.framework"
              "imranfish.xmlextensions"
              "thesepeople.ritualattachableoutcomes"
              "ohno.asf.ab.local"
              "oskarpotocki.vanillafactionsexpanded.core"
              "owlchemist.cherrypicker"
              "redmattis.betterprerequisites"
              "smashphil.vehicleframework"
              "unlimitedhugs.hugslib"
              "vanillaexpanded.backgrounds" ]

    /// Build a dependency graph from mod metadata.
    /// The graph maps each active mod's packageId to the set of packageIds that must load before it.
    let genDepsGraph (mods: Map<string, ModMetadata>) (activeModIds: Set<string>) : DependencyGraph =
        mods
        |> Map.fold
            (fun (graph: Map<string, Set<string>>) _uuid meta ->
                let pid = meta.PackageId

                let deps =
                    meta.LoadTheseBefore
                    |> List.choose (fun (depId, _explicit) ->
                        if activeModIds.Contains(depId) then Some depId else None)
                    |> Set.ofList

                graph |> Map.add pid deps)
            Map.empty

    /// Build a reverse dependency graph.
    /// Maps each mod to the set of mods that should load after it (i.e., dependents).
    let genRevDepsGraph (mods: Map<string, ModMetadata>) (activeModIds: Set<string>) : DependencyGraph =
        mods
        |> Map.fold
            (fun (graph: Map<string, Set<string>>) _uuid meta ->
                let pid = meta.PackageId

                let deps =
                    meta.LoadTheseAfter
                    |> List.choose (fun (depId, _explicit) ->
                        if activeModIds.Contains(depId) then Some depId else None)
                    |> Set.ofList

                graph |> Map.add pid deps)
            Map.empty

    /// Recursively get all dependencies of a package.
    let rec getDependenciesRecursive
        (packageId: string)
        (graph: DependencyGraph)
        (processed: Set<string> ref)
        : Set<string> =
        match Map.tryFind packageId graph with
        | None -> Set.empty
        | Some deps ->
            deps
            |> Set.fold
                (fun acc depId ->
                    if processed.Value.Contains(depId) then
                        acc
                    else
                        processed.Value <- processed.Value.Add depId
                        let subDeps = getDependenciesRecursive depId graph processed
                        acc |> Set.add depId |> Set.union subDeps)
                Set.empty

    /// Recursively get all reverse dependencies (dependents) of a package.
    let rec getReverseDependenciesRecursive (packageId: string) (revGraph: DependencyGraph) : Set<string> =
        match Map.tryFind packageId revGraph with
        | None -> Set.empty
        | Some dependents ->
            dependents
            |> Set.fold
                (fun acc depId ->
                    let subDeps = getReverseDependenciesRecursive depId revGraph
                    acc |> Set.add depId |> Set.union subDeps)
                Set.empty

    /// Generate the tier zero dependency graph (Core, DLCs, Harmony, etc.).
    let genTierZeroDepsGraph (depsGraph: DependencyGraph) : DependencyGraph * Set<string> =
        let processed = ref Set.empty

        let tierZeroMods =
            knownTierZeroMods
            |> Set.fold
                (fun acc modId ->
                    if depsGraph.ContainsKey(modId) then
                        let depsSet = getDependenciesRecursive modId depsGraph processed
                        acc |> Set.add modId |> Set.union depsSet
                    else
                        acc)
                Set.empty

        let tierZeroGraph =
            tierZeroMods
            |> Set.fold
                (fun (graph: Map<string, Set<string>>) modId ->
                    match Map.tryFind modId depsGraph with
                    | Some deps -> graph |> Map.add modId deps
                    | None -> graph)
                Map.empty

        (tierZeroGraph, tierZeroMods)

    /// Generate the tier one dependency graph (Frameworks).
    let genTierOneDepsGraph
        (depsGraph: DependencyGraph)
        (mods: Map<string, ModMetadata>)
        : DependencyGraph * Set<string> =
        // Collect known tier one mods plus any mods with loadTop = true
        let allTierOneCandidates =
            let loadTopMods =
                mods
                |> Map.fold
                    (fun acc _uuid meta -> if meta.LoadTop then acc |> Set.add meta.PackageId else acc)
                    Set.empty

            Set.union knownTierOneMods loadTopMods

        let processed = ref Set.empty

        let tierOneMods =
            allTierOneCandidates
            |> Set.fold
                (fun acc modId ->
                    if depsGraph.ContainsKey(modId) then
                        let depsSet = getDependenciesRecursive modId depsGraph processed
                        acc |> Set.add modId |> Set.union depsSet
                    else
                        acc)
                Set.empty

        let tierOneGraph =
            tierOneMods
            |> Set.fold
                (fun (graph: Map<string, Set<string>>) modId ->
                    match Map.tryFind modId depsGraph with
                    | Some deps -> graph |> Map.add modId deps
                    | None -> graph)
                Map.empty

        tierOneGraph, tierOneMods

    /// Generate the tier three dependency graph (bottom-loaded mods).
    let genTierThreeDepsGraph
        (depsGraph: DependencyGraph)
        (revDepsGraph: DependencyGraph)
        (mods: Map<string, ModMetadata>)
        : DependencyGraph * Set<string> =
        let knownTierThreeMods =
            let loadBottomMods =
                mods
                |> Map.fold
                    (fun acc _uuid meta ->
                        if meta.LoadBottom then
                            acc |> Set.add meta.PackageId
                        else
                            acc)
                    Set.empty

            loadBottomMods |> Set.add "krkr.rocketman"

        let tierThreeMods =
            knownTierThreeMods
            |> Set.fold
                (fun acc modId ->
                    if depsGraph.ContainsKey(modId) then
                        let revDepsSet = getReverseDependenciesRecursive modId revDepsGraph
                        acc |> Set.add modId |> Set.union revDepsSet
                    else
                        acc)
                Set.empty

        let tierThreeGraph =
            tierThreeMods
            |> Set.fold
                (fun (graph: Map<string, Set<string>>) modId ->
                    match Map.tryFind modId depsGraph with
                    | Some deps ->
                        // Trim dependencies to only reference other tier three mods
                        let trimmedDeps = Set.intersect deps tierThreeMods
                        graph |> Map.add modId trimmedDeps
                    | None -> graph)
                Map.empty

        (tierThreeGraph, tierThreeMods)

    /// Generate the tier two dependency graph (regular mods, excluding tier one and tier three).
    let genTierTwoDepsGraph
        (mods: Map<string, ModMetadata>)
        (activeModIds: Set<string>)
        (tierOneMods: Set<string>)
        (tierThreeMods: Set<string>)
        (useModDependenciesAsLoadBefore: bool)
        : DependencyGraph =

        // First pass: collect explicit loadTheseBefore rules
        let explicitRules =
            mods
            |> Map.fold
                (fun acc _uuid meta ->
                    let pid = meta.PackageId

                    if not (tierOneMods.Contains(pid)) && not (tierThreeMods.Contains(pid)) then
                        let deps =
                            meta.LoadTheseBefore
                            |> List.choose (fun (depId, _) ->
                                if
                                    not (tierOneMods.Contains(depId))
                                    && not (tierThreeMods.Contains(depId))
                                    && activeModIds.Contains(depId)
                                then
                                    Some depId
                                else
                                    None)
                            |> Set.ofList

                        acc |> Map.add pid deps
                    else
                        acc)
                Map.empty

        // Second pass: collect inferred rules from About.xml dependencies
        let inferredRules =
            if useModDependenciesAsLoadBefore then
                mods
                |> Map.fold
                    (fun acc _uuid meta ->
                        let pid = meta.PackageId

                        if not (tierOneMods.Contains(pid)) && not (tierThreeMods.Contains(pid)) then
                            let deps =
                                meta.Dependencies
                                |> List.choose (fun depId ->
                                    if
                                        activeModIds.Contains(depId)
                                        && not (tierOneMods.Contains(depId))
                                        && not (tierThreeMods.Contains(depId))
                                    then
                                        Some depId
                                    else
                                        None)
                                |> Set.ofList

                            acc |> Map.add pid deps
                        else
                            acc)
                    Map.empty
            else
                Map.empty

        // Resolve conflicts: explicit rules take precedence
        explicitRules
        |> Map.map (fun packageId explicitDeps ->
            let inferredDeps =
                inferredRules |> Map.tryFind packageId |> Option.defaultValue Set.empty

            let nonConflicting =
                inferredDeps
                |> Set.filter (fun inferredDep ->
                    // Check for conflict: does inferredDep have an explicit rule saying packageId should load before it?
                    match Map.tryFind inferredDep explicitRules with
                    | Some depExplicitDeps -> not (depExplicitDeps.Contains(packageId))
                    | None -> true)

            Set.union explicitDeps nonConflicting)
