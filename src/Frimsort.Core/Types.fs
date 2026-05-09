namespace Frimsort.Core

/// Represents mod metadata needed for sorting.
type ModMetadata =
    { PackageId: string
      Name: string
      /// Package IDs that should be loaded before this mod.
      LoadTheseBefore: (string * bool) list
      /// Package IDs that should be loaded after this mod.
      LoadTheseAfter: (string * bool) list
      /// Whether this mod should load at the top of the load order.
      LoadTop: bool
      /// Whether this mod should load at the bottom of the load order.
      LoadBottom: bool
      /// About.xml dependencies (package IDs with optional alternatives).
      Dependencies: string list }

/// Represents a dependency graph where each key maps to a set of package IDs
/// that must be loaded before it.
type DependencyGraph = Map<string, Set<string>>

/// Sort method enumeration.
type SortMethod =
    | Alphabetical
    | Topological

/// Result of a sort operation.
type SortResult =
    | Success of string list
    | CircularDependencyError of cycles: string list list
