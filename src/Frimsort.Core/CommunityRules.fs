namespace Frimsort.Core

open System
open System.IO
open System.Net.Http
open System.Text.Json
open System.Threading.Tasks

/// Represents a single community rule entry for loadBefore/loadAfter.
type CommunityRuleEntry =
    { Comment: string
      Name: string option }

/// Represents community rules for a single mod.
type ModCommunityRules =
    { LoadBefore: Map<string, CommunityRuleEntry>
      LoadAfter: Map<string, CommunityRuleEntry>
      LoadTop: bool
      LoadBottom: bool }

/// Represents the full community rules database.
type CommunityRulesDb =
    { Timestamp: int64
      Rules: Map<string, ModCommunityRules> }

/// Module for downloading and parsing communityRules.json.
module CommunityRules =

    /// Default URL for the community rules database.
    let defaultRepoUrl = "https://github.com/RimSort/Community-Rules-Database"

    /// Raw JSON file URL on GitHub.
    let rawJsonUrl = "https://raw.githubusercontent.com/RimSort/Community-Rules-Database/refs/heads/main/communityRules.json"

    /// Default local file path for storing communityRules.json.
    let defaultLocalPath () =
        let appData = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)
        Path.Combine(appData, "Frimsort", "communityRules.json")

    /// Helper: get first string from a JSON element that could be a string or array of strings.
    let private getFirstString (elem: JsonElement) : string =
        match elem.ValueKind with
        | JsonValueKind.Array ->
            elem.EnumerateArray()
            |> Seq.tryHead
            |> Option.bind (fun e -> e.GetString() |> Option.ofObj)
            |> Option.defaultValue ""
        | JsonValueKind.String ->
            elem.GetString() |> Option.ofObj |> Option.defaultValue ""
        | _ -> ""

    /// Parse a CommunityRuleEntry from a JsonElement.
    let private parseRuleEntry (elem: JsonElement) : CommunityRuleEntry =
        let comment =
            match elem.TryGetProperty("comment") with
            | true, v -> getFirstString v
            | false, _ -> ""
        let name =
            match elem.TryGetProperty("name") with
            | true, v -> getFirstString v |> Option.ofObj
            | false, _ -> None
        { Comment = comment; Name = name }

    /// Parse loadBefore/loadAfter entries from a JsonElement.
    let private parseEntries (elem: JsonElement) : Map<string, CommunityRuleEntry> =
        if elem.ValueKind <> JsonValueKind.Object then
            Map.empty
        else
            elem.EnumerateObject()
            |> Seq.fold (fun acc prop ->
                let entry = parseRuleEntry prop.Value
                acc |> Map.add (prop.Name.ToLowerInvariant()) entry
            ) Map.empty

    /// Parse a single mod's community rules from a JsonElement.
    let private parseModRules (elem: JsonElement) : ModCommunityRules =
        let loadBefore =
            match elem.TryGetProperty("loadBefore") with
            | true, v -> parseEntries v
            | false, _ -> Map.empty

        let loadAfter =
            match elem.TryGetProperty("loadAfter") with
            | true, v -> parseEntries v
            | false, _ -> Map.empty

        // loadTop can be a boolean or an object with "value" field
        let loadTop =
            match elem.TryGetProperty("loadTop") with
            | true, v when v.ValueKind = JsonValueKind.Object ->
                match v.TryGetProperty("value") with
                | true, bv -> bv.GetBoolean()
                | false, _ -> false
            | true, v ->
                try v.GetBoolean() with _ -> false
            | false, _ -> false

        // loadBottom can be a boolean or an object with "value" field
        let loadBottom =
            match elem.TryGetProperty("loadBottom") with
            | true, v when v.ValueKind = JsonValueKind.Object ->
                match v.TryGetProperty("value") with
                | true, bv -> bv.GetBoolean()
                | false, _ -> false
            | true, v ->
                try v.GetBoolean() with _ -> false
            | false, _ -> false

        { LoadBefore = loadBefore
          LoadAfter = loadAfter
          LoadTop = loadTop
          LoadBottom = loadBottom }

    /// Parse the full community rules database from JSON text.
    let parseJson (jsonText: string) : CommunityRulesDb option =
        try
            use doc = JsonDocument.Parse(jsonText)
            let root = doc.RootElement

            let timestamp =
                match root.TryGetProperty("timestamp") with
                | true, v -> v.GetInt64()
                | false, _ -> 0L

            let rules =
                match root.TryGetProperty("rules") with
                | true, v when v.ValueKind = JsonValueKind.Object ->
                    v.EnumerateObject()
                    |> Seq.fold (fun acc prop ->
                        try
                            let modRules = parseModRules prop.Value
                            acc |> Map.add (prop.Name.ToLowerInvariant()) modRules
                        with _ ->
                            // Skip rules that fail to parse
                            acc
                    ) Map.empty
                | _ -> Map.empty

            Some { Timestamp = timestamp; Rules = rules }
        with _ ->
            None

    /// Parse community rules from a local file path.
    let parseFromFile (filePath: string) : CommunityRulesDb option =
        try
            if not (File.Exists(filePath)) then
                None
            else
                let jsonText = File.ReadAllText(filePath)
                parseJson jsonText
        with _ ->
            None

    /// Download communityRules.json from the raw GitHub URL and save it to the specified path.
    /// Returns true on success, false on failure.
    let downloadAndSave (destinationPath: string) : Task<bool> =
        task {
            try
                // Ensure parent directory exists
                let dir = Path.GetDirectoryName(destinationPath)
                if not (String.IsNullOrEmpty(dir)) && not (Directory.Exists(dir)) then
                    Directory.CreateDirectory(dir) |> ignore

                use client = new HttpClient()
                client.Timeout <- TimeSpan.FromSeconds(float 30)
                let! content = client.GetStringAsync(rawJsonUrl)
                File.WriteAllText(destinationPath, content)
                return true
            with _ ->
                return false
        }

    /// Download and parse community rules in one step.
    /// Downloads to the specified path and returns the parsed database.
    let downloadAndParse (destinationPath: string) : Task<CommunityRulesDb option> =
        task {
            let! success = downloadAndSave destinationPath
            if success then
                return parseFromFile destinationPath
            else
                return None
        }
