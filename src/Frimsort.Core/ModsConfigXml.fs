namespace Frimsort.Core

open System
open System.IO
open System.Xml

/// Module for reading and writing ModsConfig.xml files.
module ModsConfigXml =

    /// Default DLC/Expansion package IDs.
    let private knownExpansions =
        [
            "ludeon.rimworld.royalty"
            "ludeon.rimworld.ideology"
            "ludeon.rimworld.biotech"
            "ludeon.rimworld.anomaly"
            "ludeon.rimworld.odyssey"
        ]

    /// Read a ModsConfig.xml file and return the list of active mod package IDs.
    let readModsConfig (path: string) : string list option =
        try
            if not (File.Exists(path)) then
                None
            else
                let doc = XmlDocument()
                doc.Load(path)

                // Try ModsConfigData/activeMods/li format (standard)
                let activeModsNode = doc.SelectSingleNode("ModsConfigData/activeMods")
                if activeModsNode <> null then
                    let liNodes = activeModsNode.SelectNodes("li")
                    if liNodes <> null && liNodes.Count > 0 then
                        [ for i in 0 .. liNodes.Count - 1 ->
                            liNodes.Item(i).InnerText.Trim() ]
                        |> Some
                    else
                        Some []
                else
                    // Try savegame/meta/modIds/li format (.rws save)
                    let modIdsNode = doc.SelectSingleNode("savegame/meta/modIds")
                    if modIdsNode <> null then
                        let liNodes = modIdsNode.SelectNodes("li")
                        if liNodes <> null && liNodes.Count > 0 then
                            [ for i in 0 .. liNodes.Count - 1 ->
                                liNodes.Item(i).InnerText.Trim() ]
                            |> Some
                        else
                            Some []
                    else
                        // Try savedModList/meta/modIds/li format (.rml modlist)
                        let savedModListNode = doc.SelectSingleNode("savedModList/meta/modIds")
                        if savedModListNode <> null then
                            let liNodes = savedModListNode.SelectNodes("li")
                            if liNodes <> null && liNodes.Count > 0 then
                                [ for i in 0 .. liNodes.Count - 1 ->
                                    liNodes.Item(i).InnerText.Trim() ]
                                |> Some
                            else
                                Some []
                        else
                            None
        with _ ->
            None

    /// Get the game version from a ModsConfig.xml file.
    let readGameVersion (path: string) : string option =
        try
            if not (File.Exists(path)) then
                None
            else
                let doc = XmlDocument()
                doc.Load(path)
                let versionNode = doc.SelectSingleNode("ModsConfigData/version")
                if versionNode <> null && not (String.IsNullOrEmpty(versionNode.InnerText)) then
                    Some (versionNode.InnerText.Trim())
                else
                    None
        with _ ->
            None

    /// Write a ModsConfig.xml file with the given sorted package IDs.
    let writeModsConfig (path: string) (gameVersion: string) (packageIds: string list) : bool =
        try
            // Ensure parent directory exists
            let dir = Path.GetDirectoryName(path)
            if not (String.IsNullOrEmpty(dir)) && not (Directory.Exists(dir)) then
                Directory.CreateDirectory(dir) |> ignore

            let doc = XmlDocument()

            // Create XML declaration
            let decl = doc.CreateXmlDeclaration("1.0", "utf-8", null)
            doc.AppendChild(decl) |> ignore

            // Create ModsConfigData root
            let modsConfigData = doc.CreateElement("ModsConfigData")
            doc.AppendChild(modsConfigData) |> ignore

            // Add version
            let versionElem = doc.CreateElement("version")
            versionElem.InnerText <- gameVersion
            modsConfigData.AppendChild(versionElem) |> ignore

            // Add activeMods
            let activeModsElem = doc.CreateElement("activeMods")
            for pid in packageIds do
                let liElem = doc.CreateElement("li")
                liElem.InnerText <- pid
                activeModsElem.AppendChild(liElem) |> ignore
            modsConfigData.AppendChild(activeModsElem) |> ignore

            // Add knownExpansions
            let knownExpElem = doc.CreateElement("knownExpansions")
            for eid in knownExpansions do
                let liElem = doc.CreateElement("li")
                liElem.InnerText <- eid
                knownExpElem.AppendChild(liElem) |> ignore
            modsConfigData.AppendChild(knownExpElem) |> ignore

            let settings = XmlWriterSettings(Indent = true)
            use writer = XmlWriter.Create(path, settings)
            doc.Save(writer)
            true
        with _ ->
            false

    /// Default Windows config folder path.
    let defaultWindowsConfigPath () =
        let userHome = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
        Path.Combine(userHome, "AppData", "LocalLow", "Ludeon Studios", "RimWorld by Ludeon Studios", "Config")

    /// Default Linux config folder path.
    let defaultLinuxConfigPath () =
        let userHome = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
        Path.Combine(userHome, ".config", "unity3d", "Ludeon Studios", "RimWorld by Ludeon Studios", "Config")

    /// Default macOS config folder path.
    let defaultMacOsConfigPath () =
        let userHome = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
        Path.Combine(userHome, "Library", "Application Support", "Rimworld", "Config")

    /// Get the default config folder path for the current OS.
    let defaultConfigPath () =
        if Environment.OSVersion.Platform = PlatformID.Win32NT then
            defaultWindowsConfigPath ()
        elif Environment.OSVersion.Platform = PlatformID.Unix || Environment.OSVersion.Platform = PlatformID.MacOSX then
            // Simple heuristic: check if /home exists for Linux
            if Directory.Exists("/home") then
                defaultLinuxConfigPath ()
            else
                defaultMacOsConfigPath ()
        else
            defaultWindowsConfigPath ()
