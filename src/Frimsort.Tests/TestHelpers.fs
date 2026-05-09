module Frimsort.Tests.TestHelpers

open Frimsort.Core

/// Create a simple ModMetadata with defaults.
let makeMod (packageId: string) (name: string) (loadBefore: (string * bool) list) : ModMetadata =
    { PackageId = packageId
      Name = name
      LoadTheseBefore = loadBefore
      LoadTheseAfter = []
      LoadTop = false
      LoadBottom = false
      Dependencies = [] }

/// Create a ModMetadata with loadAfter rules.
let makeModWithAfter (packageId: string) (name: string) (loadAfter: (string * bool) list) : ModMetadata =
    { PackageId = packageId
      Name = name
      LoadTheseBefore = []
      LoadTheseAfter = loadAfter
      LoadTop = false
      LoadBottom = false
      Dependencies = [] }

/// Create a ModMetadata with all options.
let makeModFull
    (packageId: string)
    (name: string)
    (loadBefore: (string * bool) list)
    (loadAfter: (string * bool) list)
    (loadTop: bool)
    (loadBottom: bool)
    (deps: string list)
    : ModMetadata =
    { PackageId = packageId
      Name = name
      LoadTheseBefore = loadBefore
      LoadTheseAfter = loadAfter
      LoadTop = loadTop
      LoadBottom = loadBottom
      Dependencies = deps }

/// Build a mods map from a list of (uuid, metadata) pairs.
let buildModsMap (items: (string * ModMetadata) list) : Map<string, ModMetadata> =
    items |> Map.ofList
