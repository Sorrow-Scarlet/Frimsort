namespace Frimsort.Core

open System
open System.IO
open System.Xml

/// Module for scanning mod directories and reading About.xml metadata.
module ModScanner =

    /// Parse an About.xml file and return mod metadata.
    /// The uuid is derived from the full path of the mod folder.
    let parseAboutXml (path: string) : ModMetadata option =
        try
            let doc = XmlDocument()
            doc.Load(path)

            // Helper: get inner text of a node path like ModMetaData/name
            let getNodeText (xpath: string) =
                let node = doc.SelectSingleNode(xpath)
                if node <> null && not (String.IsNullOrEmpty(node.InnerText)) then
                    Some(node.InnerText.Trim())
                else
                    None

            let packageId =
                match getNodeText "ModMetaData/packageId" with
                | Some id -> id.ToLowerInvariant()
                | None -> ""

            let name =
                match getNodeText "ModMetaData/name" with
                | Some n -> n
                | None -> "Unknown Mod"

            let loadBefore =
                let loadBeforeNode = doc.SelectSingleNode("ModMetaData/loadBefore")
                match loadBeforeNode with
                | null -> []
                | node ->
                    let liNodes = node.SelectNodes("li")
                    if liNodes <> null then
                        [ for i in 0 .. liNodes.Count - 1 ->
                            let text = liNodes.Item(i).InnerText.Trim().ToLowerInvariant()
                            (text, true) ]
                    else
                        []

            let loadAfter =
                let loadAfterNode = doc.SelectSingleNode("ModMetaData/loadAfter")
                match loadAfterNode with
                | null -> []
                | node ->
                    let liNodes = node.SelectNodes("li")
                    if liNodes <> null then
                        [ for i in 0 .. liNodes.Count - 1 ->
                            let text = liNodes.Item(i).InnerText.Trim().ToLowerInvariant()
                            (text, true) ]
                    else
                        []

            let loadTop =
                match getNodeText "ModMetaData/loadTop" with
                | Some v -> v.Equals("true", StringComparison.OrdinalIgnoreCase)
                | None -> false

            let loadBottom =
                match getNodeText "ModMetaData/loadBottom" with
                | Some v -> v.Equals("true", StringComparison.OrdinalIgnoreCase)
                | None -> false

            let dependencies =
                let depsNode = doc.SelectSingleNode("ModMetaData/modDependencies")
                match depsNode with
                | null -> []
                | node ->
                    let liNodes = node.SelectNodes("li")
                    if liNodes <> null then
                        [ for i in 0 .. liNodes.Count - 1 do
                            let li = liNodes.Item(i)
                            let pkgNode = li.SelectSingleNode("packageId")
                            if pkgNode <> null && not (String.IsNullOrEmpty(pkgNode.InnerText.Trim())) then
                                yield pkgNode.InnerText.Trim().ToLowerInvariant() ]
                    else
                        []

            if String.IsNullOrEmpty(packageId) then
                None
            else
                Some {
                    PackageId = packageId
                    Name = name
                    LoadTheseBefore = loadBefore
                    LoadTheseAfter = loadAfter
                    LoadTop = loadTop
                    LoadBottom = loadBottom
                    Dependencies = dependencies
                }
        with _ ->
            None

    /// Find all About.xml files in a directory tree (one level deep for mod folders).
    let findModsInDirectory (rootPath: string) : (string * ModMetadata) list =
        if not (Directory.Exists(rootPath)) then
            []
        else
            let modFolders =
                try
                    Directory.GetDirectories(rootPath)
                with _ ->
                    [||]

            modFolders
            |> Array.choose (fun modFolder ->
                let aboutPath = Path.Combine(modFolder, "About")
                let aboutFile = Path.Combine(aboutPath, "About.xml")
                if File.Exists(aboutFile) then
                    match parseAboutXml(aboutFile) with
                    | Some meta ->
                        let uuid = modFolder  // Use folder path as UUID
                        Some (uuid, meta)
                    | None -> None
                else
                    None)
            |> Array.toList

    /// Scan multiple directories (local mods + steam workshop) for mods.
    let scanAllMods (paths: string list) : (string * ModMetadata) list =
        paths
        |> List.collect (fun p -> findModsInDirectory p)
        |> List.distinctBy fst  // Deduplicate by UUID
