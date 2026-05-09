module Frimsort.Program

open System
open System.IO
open Frimsort.Core

// ============ Constants ============
let private Version = "1.0.0"
let private AppName = "Frimsort"
let private ConfigFileName = ".frimsort-config"

// ============ Config File ============
type FrimsortConfig =
    { ModsFolders: string list      // Directories containing mod folders
      ConfigFolderPath: string      // Path to RimWorld Config folder
      SortMethod: SortMethod }      // Alphabetical or Topological

let private getConfigFilePath () =
    Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ConfigFileName)

let private loadConfig () : FrimsortConfig option =
    try
        let path = getConfigFilePath ()
        if not (File.Exists(path)) then
            None
        else
            let lines = File.ReadAllLines(path)
            let mutable modsFolders = []
            let mutable configFolderPath = ""
            let mutable sortMethod = Topological

            for line in lines do
                let trimmed = line.Trim()
                if trimmed.StartsWith("mods_folder=") then
                    modsFolders <- trimmed.Substring("mods_folder=".Length) :: modsFolders
                elif trimmed.StartsWith("config_folder=") then
                    configFolderPath <- trimmed.Substring("config_folder=".Length)
                elif trimmed.StartsWith("sort_method=") then
                    match trimmed.Substring("sort_method=".Length) with
                    | "alphabetical" -> sortMethod <- Alphabetical
                    | _ -> sortMethod <- Topological

            Some {
                ModsFolders = List.rev modsFolders
                ConfigFolderPath = configFolderPath
                SortMethod = sortMethod
            }
    with _ ->
        None

let private saveConfig (config: FrimsortConfig) : unit =
    let path = getConfigFilePath ()
    let lines =
        [
            yield "# Frimsort configuration file"
            yield "# Mod folders: directories containing mod folders (local mods, workshop)"
            for folder in config.ModsFolders do
                yield $"mods_folder={folder}"
            yield $"# RimWorld config folder path"
            yield $"config_folder={config.ConfigFolderPath}"
            yield $"# Sort method: alphabetical or topological"
            let sortMethodStr = match config.SortMethod with Alphabetical -> "alphabetical" | Topological -> "topological"
            yield $"sort_method={sortMethodStr}"
        ]
    File.WriteAllLines(path, lines)

// ============ Color Helpers ============
let private setColor (color: ConsoleColor) =
    Console.ForegroundColor <- color

let private resetColor () =
    Console.ForegroundColor <- ConsoleColor.Gray

let private printColor (text: string) (color: ConsoleColor) =
    setColor color
    printf "%s" text
    resetColor ()

let private printLineColor (text: string) (color: ConsoleColor) =
    setColor color
    printfn "%s" text
    resetColor ()

// ============ Help Command ============
let private executeHelp () =
    printfn "%s v%s - F# RimWorld Mod Sorter" AppName Version
    printfn ""
    printLineColor "USAGE:" ConsoleColor.Cyan
    printfn "  frimsort <command> [options]"
    printfn ""
    printLineColor "COMMANDS:" ConsoleColor.Cyan
    printfn "  help              Show this help message"
    printfn "  path              Configure mod folder and config folder paths"
    printfn "  sort [method]     Sort mods and save result. method: alphabetical|topological (default: topological)"
    printfn "  auto              Auto-detect RimWorld paths, then ask to sort"
    printfn "  sync              Download/update community rules database from GitHub"
    printfn ""
    printLineColor "DESCRIPTIONS:" ConsoleColor.Cyan
    printfn "  help"
    printfn "    Displays this usage information."
    printfn ""
    printfn "  path"
    printfn "    Interactively configure the paths for mod folders and RimWorld config folder."
    printfn "    You will be prompted to paste absolute paths for:"
    printfn "      - Mod folder(s): directories containing your local mod folders"
    printfn "      - Config folder: the RimWorld Config folder containing ModsConfig.xml"
    printfn "    Paths are saved to %s in your user profile." ConfigFileName
    printfn ""
    printfn "  sort"
    printfn "    Sorts all mods found in configured mod folders and saves the result to ModsConfig.xml."
    printfn "    Before running, you must configure paths using 'frimsort path'."
    printfn "    Sort methods:"
    printfn "      alphabetical - Sort alphabetically by mod name, respecting dependencies"
    printfn "      topological  - Sort by dependency order, alphabetical within each tier (default)"
    printfn ""
    printfn "  auto"
    printfn "    Automatically searches for RimWorld installation and config folders."
    printfn "    If found, displays the paths and asks if you want to sort."
    printfn ""
    printfn "  sync"
    printfn "    Downloads the latest communityRules.json from the RimSort community rules database."
    printfn "    If the file exists, it will be overwritten. Otherwise, a new file is created."
    printfn "    Saved to %%APPDATA%%\\Frimsort\\communityRules.json."
    printfn ""
    printLineColor "EXAMPLES:" ConsoleColor.Cyan
    printfn "  frimsort help"
    printfn "  frimsort path"
    printfn "  frimsort sort"
    printfn "  frimsort sort alphabetical"
    printfn "  frimsort sort topological"
    printfn "  frimsort auto"
    printfn "  frimsort sync"

// ============ Path Command ============
let private executePath () =
    let existingConfig = loadConfig ()

    printfn "%s - Configure Paths" AppName
    printfn ""

    if existingConfig.IsSome then
        let cfg = existingConfig.Value
        printLineColor "Current configuration:" ConsoleColor.Yellow
        if cfg.ModsFolders.IsEmpty then
            printfn "  Mod folders:    (not configured)"
        else
            printfn "  Mod folders:"
            for folder in cfg.ModsFolders do
                printfn "    - %s" folder
        printfn "  Config folder:  %s" (if String.IsNullOrEmpty(cfg.ConfigFolderPath) then "(not configured)" else cfg.ConfigFolderPath)
        printfn "  Sort method:    %s" (match cfg.SortMethod with Alphabetical -> "alphabetical" | Topological -> "topological")
        printfn ""

    // Collect mod folders
    let rec collectModFolders (current: string list) : string list =
        printfn "Enter mod folder path (e.g., D:\\RimWorld\\Mods), or type 'done' to finish:"
        printf "> "
        let input = Console.ReadLine().Trim()
        if input.ToLowerInvariant() = "done" then
            current
        elif input = "" then
            collectModFolders current
        elif not (Directory.Exists(input)) then
            printLineColor (sprintf "Warning: Directory '%s' does not exist." input) ConsoleColor.Red
            printfn "Type 'y' to add anyway, or press Enter to re-enter:"
            printf "> "
            let confirm = Console.ReadLine().Trim()
            if confirm.ToLowerInvariant() = "y" then
                collectModFolders (input :: current)
            else
                collectModFolders current
        else
            printfn "  Added: %s" input
            collectModFolders (input :: current)

    let modFolders = collectModFolders []

    // Collect config folder
    let rec getConfigFolder () : string =
        printfn ""
        printfn "Enter RimWorld Config folder path (containing ModsConfig.xml):"
        printf "  e.g., C:\\Users\\%s\\AppData\\LocalLow\\Ludeon Studios\\RimWorld by Ludeon Studios\\Config" (Environment.UserName)
        printfn ""
        printf "> "
        let input = Console.ReadLine().Trim()
        if input = "" then
            match existingConfig with
            | Some cfg when not (String.IsNullOrEmpty(cfg.ConfigFolderPath)) ->
                cfg.ConfigFolderPath
            | _ ->
                printfn "Config folder path cannot be empty. Please enter a path."
                getConfigFolder ()
        elif not (Directory.Exists(input)) then
            printLineColor (sprintf "Warning: Directory '%s' does not exist." input) ConsoleColor.Red
            printfn "Type 'y' to use anyway, or press Enter to re-enter:"
            printf "> "
            let confirm = Console.ReadLine().Trim()
            if confirm.ToLowerInvariant() = "y" then
                input
            else
                getConfigFolder ()
        else
            input

    let configFolder = getConfigFolder ()

    // Save config
    let newConfig = {
        ModsFolders = List.rev modFolders
        ConfigFolderPath = configFolder
        SortMethod = Topological
    }
    saveConfig newConfig
    printfn ""
    printLineColor "Configuration saved!" ConsoleColor.Green
    printfn "  Mod folders: %d configured" newConfig.ModsFolders.Length
    printfn "  Config folder: %s" newConfig.ConfigFolderPath

// ============ Sort Command ============
let private executeSort (methodArg: string option) =
    let config =
        match loadConfig () with
        | Some cfg -> cfg
        | None ->
            printLineColor "Error: No configuration found." ConsoleColor.Red
            printfn "Please run 'frimsort path' first to configure your mod folders and config folder."
            exit 1

    // Validate config
    if config.ModsFolders.IsEmpty then
        printLineColor "Error: No mod folders configured." ConsoleColor.Red
        printfn "Please run 'frimsort path' first to configure your mod folders."
        exit 1

    if String.IsNullOrEmpty(config.ConfigFolderPath) then
        printLineColor "Error: No config folder configured." ConsoleColor.Red
        printfn "Please run 'frimsort path' first to configure your config folder."
        exit 1

    // Determine sort method
    let sortMethod =
        match methodArg with
        | Some "alphabetical" -> Alphabetical
        | Some "topological" -> Topological
        | Some other ->
            printLineColor (sprintf "Error: Unknown sort method '%s'." other) ConsoleColor.Red
            printfn "Valid methods: alphabetical, topological"
            exit 1
        | None -> config.SortMethod

    // Scan mods
    printfn "Scanning mod folders..."
    let mods = ModScanner.scanAllMods config.ModsFolders
    if mods.IsEmpty then
        printLineColor "Warning: No mods found in configured folders." ConsoleColor.Yellow
        printfn "Please verify your mod folder paths contain mod folders with About.xml files."
        exit 0

    printfn "  Found %d mod(s)" mods.Length

    // Build mods map and active package IDs
    let modsMap = Map.ofList mods
    let activeModIds = mods |> List.map (fun (_, m) -> m.PackageId) |> Set.ofList

    // Sort
    printfn "Sorting with %s method..." (match sortMethod with Alphabetical -> "alphabetical" | Topological -> "topological")
    let result = Sorter.sortMods sortMethod modsMap activeModIds false

    match result with
    | CircularDependencyError cycles ->
        printLineColor "Error: Circular dependencies detected!" ConsoleColor.Red
        for cycle in cycles do
            printfn "  Cycle: %s" (String.Join(" -> ", cycle |> List.toArray))
        exit 1

    | Success sortedUuids ->
        // Convert UUIDs back to package IDs
        let uuidToPkgId = mods |> Map.ofList |> Map.map (fun _ meta -> meta.PackageId)
        let sortedPackageIds =
            sortedUuids
            |> List.choose (fun uuid -> Map.tryFind uuid uuidToPkgId)

        printfn "  Sorted %d mod(s)" sortedPackageIds.Length

        // Write ModsConfig.xml
        let configPath = Path.Combine(config.ConfigFolderPath, "ModsConfig.xml")
        let gameVersion =
            match ModsConfigXml.readGameVersion configPath with
            | Some v -> v
            | None -> "1.5.0"  // Default fallback

        printfn "Saving to %s..." configPath
        let success = ModsConfigXml.writeModsConfig configPath gameVersion sortedPackageIds

        if success then
            printLineColor "Successfully saved sorted mod list!" ConsoleColor.Green
            // Show first 10 mods
            let preview =
                sortedPackageIds
                |> List.take (min 10 sortedPackageIds.Length)
            for pid in preview do
                printfn "  - %s" pid
            if sortedPackageIds.Length > 10 then
                printfn "  ... and %d more" (sortedPackageIds.Length - 10)
        else
            printLineColor "Error: Failed to write ModsConfig.xml." ConsoleColor.Red
            exit 1

// ============ Auto Command ============
let private executeAuto () =
    printfn "%s - Auto-Detect RimWorld Paths" AppName
    printfn ""

    let mutable found = false

    // Try detecting config folder based on OS
    let detectedConfigPath =
        if Environment.OSVersion.Platform = PlatformID.Win32NT then
            ModsConfigXml.defaultConfigPath ()
        else
            ModsConfigXml.defaultConfigPath ()

    // On Windows, also try to detect Steam + game folder
    let detectedModsFolders =
        if Environment.OSVersion.Platform = PlatformID.Win32NT then
            // Try common Steam paths
            let steamPath = Path.Combine("C:", "Program Files (x86)", "Steam", "steamapps", "workshop", "content", "294100")
            let localModsPath = Path.Combine("C:", "Program Files (x86)", "Steam", "steamapps", "common", "RimWorld", "Mods")
            [
                if Directory.Exists(steamPath) then yield steamPath
                if Directory.Exists(localModsPath) then yield localModsPath
            ]
        else
            []

    if Directory.Exists(detectedConfigPath) then
        found <- true
        printLineColor "Detected RimWorld config folder:" ConsoleColor.Green
        printfn "  %s" detectedConfigPath

        let modsConfigPath = Path.Combine(detectedConfigPath, "ModsConfig.xml")
        if File.Exists(modsConfigPath) then
            printfn "  ModsConfig.xml found"
        else
            printLineColor "  Warning: ModsConfig.xml not found in config folder." ConsoleColor.Yellow
    else
        printLineColor "Could not auto-detect RimWorld config folder." ConsoleColor.Red
        printfn "  Default path checked: %s" detectedConfigPath

    if not detectedModsFolders.IsEmpty then
        found <- true
        printLineColor "Detected mod folders:" ConsoleColor.Green
        for folder in detectedModsFolders do
            printfn "  - %s" folder
    else
        printLineColor "Could not auto-detect mod folders." ConsoleColor.Red

    printfn ""

    if found then
        printfn "Auto-detected paths:"
        printfn "  Config folder: %s" detectedConfigPath
        for folder in detectedModsFolders do
            printfn "  Mod folder:      %s" folder
        printfn ""
        printfn "Do you want to save these paths and sort now? (Y/n): "
        let answer = Console.ReadLine().Trim()
        if answer.ToLowerInvariant() <> "n" then
            // Save config
            let config = {
                ModsFolders = detectedModsFolders
                ConfigFolderPath = detectedConfigPath
                SortMethod = Topological
            }
            saveConfig config
            printLineColor "Paths saved. Now sorting..." ConsoleColor.Green
            executeSort(None)
        else
            printfn "Aborted."
    else
        printLineColor "Auto-detection failed." ConsoleColor.Red
        printfn "Please run 'frimsort path' to manually configure your paths."

// ============ Sync Command ============
let private executeSync () =
    let destPath = CommunityRules.defaultLocalPath ()
    printfn "%s - Sync Community Rules Database" AppName
    printfn ""
    printfn "Source: %s" CommunityRules.rawJsonUrl
    printfn "Destination: %s" destPath
    printfn ""

    let exists = File.Exists(destPath)
    if exists then
        printfn "Existing file found. It will be overwritten."
    else
        printfn "No existing file. A new file will be created."
    printfn ""

    printfn "Downloading communityRules.json..."
    let task = CommunityRules.downloadAndSave destPath
    task.Wait()

    if task.Result then
        printLineColor "Successfully downloaded and saved communityRules.json!" ConsoleColor.Green

        // Show summary from parsed data
        try
            match CommunityRules.parseFromFile destPath with
            | Some db ->
                printfn "  Timestamp: %d" db.Timestamp
                printfn "  Rules count: %d mods" db.Rules.Count
                let preview = db.Rules |> Map.toSeq |> Seq.take (min 5 db.Rules.Count) |> Seq.toList
                for (pid, rules) in preview do
                    let beforeCount = rules.LoadBefore.Count
                    let afterCount = rules.LoadAfter.Count
                    let flags =
                        [ if rules.LoadTop then yield "loadTop"
                          if rules.LoadBottom then yield "loadBottom" ]
                    let flagStr = if flags |> List.isEmpty then "" else sprintf " [%s]" (String.Join(", ", flags |> List.toArray))
                    printfn "    - %s (%d loadBefore, %d loadAfter)%s" pid beforeCount afterCount flagStr
                if db.Rules.Count > 5 then
                    printfn "    ... and %d more" (db.Rules.Count - 5)
            | None ->
                printLineColor "Warning: Failed to parse the downloaded file." ConsoleColor.Yellow
        with ex ->
            printfn "  Parse exception details: %s" ex.Message
    else
        printLineColor "Error: Failed to download communityRules.json." ConsoleColor.Red
        printfn "Please check your internet connection and try again."

// ============ Main Entry Point ============
[<EntryPoint>]
let main args =
    Console.ForegroundColor <- ConsoleColor.Gray

    if args.Length = 0 then
        executeHelp ()
        0
    else
        match args.[0].ToLowerInvariant() with
        | "help" | "--help" | "-h" | "/?" ->
            executeHelp ()
            0
        | "path" ->
            executePath ()
            0
        | "sort" ->
            let methodArg = if args.Length > 1 then Some(args.[1].ToLowerInvariant()) else None
            executeSort methodArg
            0
        | "auto" ->
            executeAuto ()
            0
        | "sync" ->
            executeSync ()
            0
        | _ ->
            printfn "Unknown command: %s" args.[0]
            printfn "Run 'frimsort help' for usage information."
            1
