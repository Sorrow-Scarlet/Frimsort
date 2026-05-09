namespace Frimsort.Core

/// Module implementing topological sort with Kahn's algorithm.
module TopoSort =

    /// Exception raised when circular dependencies are detected.
    exception CircularDependencyException of cycles: string list list

    /// Find cycles in a dependency graph (for error reporting).
    let findCycles (graph: DependencyGraph) : string list list =
        let mutable visited = Set.empty
        let mutable recStack = Set.empty
        let mutable cycles = []

        let rec dfs (node: string) (path: string list) =
            visited <- visited |> Set.add node
            recStack <- recStack |> Set.add node

            let deps = graph |> Map.tryFind node |> Option.defaultValue Set.empty
            for dep in deps do
                if not (visited.Contains(dep)) then
                    dfs dep (dep :: path)
                elif recStack.Contains(dep) then
                    // Found a cycle - extract it from path
                    let cycleStart = path |> List.tryFindIndex (fun x -> x = dep)
                    match cycleStart with
                    | Some idx ->
                        let cycle = dep :: (path |> List.take (idx + 1) |> List.rev)
                        cycles <- cycle :: cycles
                    | None ->
                        cycles <- [dep; node] :: cycles

            recStack <- recStack |> Set.remove node

        for node in graph |> Map.toSeq |> Seq.map fst do
            if not (visited.Contains(node)) then
                dfs node [node]

        cycles

    /// Perform topological sort on a dependency graph using Kahn's algorithm.
    /// Returns groups of items at each topological level.
    let toposort (graph: DependencyGraph) : Set<string> list =
        // Build full set of nodes (including those only appearing as dependencies)
        let allNodes =
            graph
            |> Map.fold (fun acc key deps ->
                acc |> Set.add key |> Set.union deps
            ) Set.empty

        // Use iterative removal (Kahn's algorithm)
        // graph[X] = set of items X depends on, i.e., X must come AFTER all items in graph[X].
        // Items with empty dependency sets (no unresolved dependencies) come first.
        let mutable remaining =
            allNodes
            |> Set.fold (fun (m: Map<string, Set<string>>) node ->
                if m.ContainsKey(node) then m
                else m |> Map.add node Set.empty
            ) (graph |> Map.map (fun _ deps -> deps))

        let mutable result = []

        while not remaining.IsEmpty do
            // Find all nodes with no remaining dependencies
            let ready =
                remaining
                |> Map.filter (fun _ deps -> deps.IsEmpty)
                |> Map.toSeq
                |> Seq.map fst
                |> Set.ofSeq

            if ready.IsEmpty then
                // Circular dependency detected - find cycles
                let cycles = findCycles remaining
                raise (CircularDependencyException cycles)

            result <- result @ [ready]

            // Remove processed nodes from remaining graph
            remaining <-
                remaining
                |> Map.filter (fun key _ -> not (ready.Contains(key)))
                |> Map.map (fun _ deps -> Set.difference deps ready)

        result

    /// Perform topological sort and return UUIDs sorted by dependency order.
    /// Within each topological level, mods are sorted alphabetically by name.
    let doTopoSort (graph: DependencyGraph) (mods: Map<string, ModMetadata>) : string list =
        let packageIdToUuid =
            mods |> Map.fold (fun acc uuid meta -> acc |> Map.add meta.PackageId uuid) Map.empty

        let uuidToName =
            mods |> Map.map (fun _ meta ->
                match meta.Name with
                | null | "" -> "name error in mod about.xml"
                | n -> n)

        let safeName (name: string) =
            if System.String.IsNullOrEmpty(name) then "name error in mod about.xml"
            else name.ToLowerInvariant()

        let sortedLevels = toposort graph

        sortedLevels
        |> List.collect (fun level ->
            level
            |> Set.toList
            |> List.choose (fun pid -> Map.tryFind pid packageIdToUuid)
            |> List.sortBy (fun uuid ->
                let name = uuidToName |> Map.tryFind uuid |> Option.defaultValue "name error in mod about.xml"
                safeName name))
