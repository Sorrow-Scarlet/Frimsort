namespace Frimsort.Core

/// Module implementing alphabetical sort with dependency respect.
module AlphabeticalSort =

    /// Recursively insert dependencies before their dependents in the load order.
    let rec private recursivelyForceInsert
        (modsLoadOrder: ResizeArray<string>)
        (graph: DependencyGraph)
        (packageId: string)
        (mods: Map<string, ModMetadata>)
        (indexJustAppended: int)
        : unit =

        let packageIdToName =
            mods
            |> Map.fold (fun acc _uuid meta -> acc |> Map.add meta.PackageId meta.Name) Map.empty

        // Get the alphabetized list of the current mod's dependencies
        let depsOfPackage =
            graph |> Map.tryFind packageId |> Option.defaultValue Set.empty

        let depsAlphabetized =
            depsOfPackage
            |> Set.toList
            |> List.choose (fun depId ->
                match Map.tryFind depId packageIdToName with
                | Some name -> Some (depId, name)
                | None -> None)
            |> List.sortBy (fun (_, name) -> name.ToLowerInvariant())

        for (depId, _) in depsAlphabetized do
            if not (modsLoadOrder.Contains(depId)) then
                // Find the correct insertion index
                let currentPackageIdx = modsLoadOrder.IndexOf(packageId)
                let mutable indexToInsertAt = indexJustAppended

                // Check existing deps between indexJustAppended and currentPackageIdx
                if currentPackageIdx > indexJustAppended then
                    let subList = modsLoadOrder.GetRange(indexJustAppended, currentPackageIdx - indexJustAppended)
                    for i in (subList.Count - 1) .. -1 .. 0 do
                        let e = subList.[i]
                        let depDeps = graph |> Map.tryFind depId |> Option.defaultValue Set.empty
                        if depDeps.Contains(e) then
                            indexToInsertAt <- modsLoadOrder.IndexOf(e) + 1
                            // Break equivalent - use mutable flag
                            ()

                // Actually implement the break logic properly
                let mutable found = false
                let currentPackageIdx2 = modsLoadOrder.IndexOf(packageId)
                if currentPackageIdx2 > indexJustAppended then
                    let mutable i = currentPackageIdx2 - 1
                    while i >= indexJustAppended && not found do
                        let e = modsLoadOrder.[i]
                        let depDeps = graph |> Map.tryFind depId |> Option.defaultValue Set.empty
                        if depDeps.Contains(e) then
                            indexToInsertAt <- modsLoadOrder.IndexOf(e) + 1
                            found <- true
                        i <- i - 1

                modsLoadOrder.Insert(indexToInsertAt, depId)
                let newIdx = modsLoadOrder.IndexOf(depId)
                recursivelyForceInsert modsLoadOrder graph depId mods newIdx

    /// Perform alphabetical sort respecting dependencies.
    let doAlphabeticalSort (graph: DependencyGraph) (mods: Map<string, ModMetadata>) : string list =
        let packageIdToUuid =
            mods |> Map.fold (fun acc uuid meta -> acc |> Map.add meta.PackageId uuid) Map.empty

        let activeModIdToName =
            mods |> Map.fold (fun acc _uuid meta -> acc |> Map.add meta.PackageId meta.Name) Map.empty

        let safeName (name: string) =
            if System.String.IsNullOrEmpty(name) then "name error in mod about.xml"
            else name.ToLowerInvariant()

        // Sort mods alphabetically by name
        let alphabetized =
            activeModIdToName
            |> Map.toList
            |> List.sortBy (fun (_, name) -> safeName name)

        // Filter to only include mods present in the dependency graph
        let alphabetizedInGraph =
            alphabetized
            |> List.filter (fun (pid, _) -> graph.ContainsKey(pid))

        let modsLoadOrder = ResizeArray<string>()

        for (packageId, _) in alphabetizedInGraph do
            if not (modsLoadOrder.Contains(packageId)) then
                modsLoadOrder.Add(packageId)
                let indexJustAppended = modsLoadOrder.IndexOf(packageId)
                recursivelyForceInsert modsLoadOrder graph packageId mods indexJustAppended

        // Convert package IDs back to UUIDs
        modsLoadOrder
        |> Seq.toList
        |> List.choose (fun pid -> Map.tryFind pid packageIdToUuid)
