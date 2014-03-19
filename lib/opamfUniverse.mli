
type repo_ref = [
| `Opam
| `Local of string
| `Path of string
]

type pred =
| Tag of string
| Depopt
| Not of pred
| Repo of string
| Pkg of string

type pred_dnf = pred OpamFormula.dnf

exception Local_remote_not_found of string
exception Opam_file_not_found of OpamFilename.t
exception Opam_file_parse_error of OpamFilename.t

val pred_sep : char

val repo_ref_ns_sep : char

val repo_ref_of_string : string -> repo_ref
val string_of_repo_ref : repo_ref -> string

val href : ?href_base:Uri.t -> string -> string -> Uri.t

val is_base_package : OpamPackage.t -> bool
val remove_base_packages : 'a OpamPackage.Map.t -> 'a OpamPackage.Map.t

val versions_of_packages :
  OpamPackage.Set.t -> OpamPackage.Version.Set.t OpamPackage.Name.Map.t
val max_versions_of_versions :
  OpamPackage.Version.Set.t OpamPackage.Name.Map.t ->
  OpamTypes.version OpamPackage.Name.Map.t

val reverse_deps :
  OpamFormula.t OpamPackage.Map.t ->
  OpamfuFormula.version_set OpamPackage.Name.Map.t ->
  OpamfuFormula.version_dnf OpamPackage.Name.Map.t OpamPackage.Map.t

val satisfies :
  pred list list ->
  < name : OpamRepositoryName.t; .. > ->
  < name : string; tags : string list; .. > -> bool

class ['pkg] repo :
  repo:OpamTypes.repository ->
  new_pkg:(opam:OpamFile.OPAM.t -> 'pkg) ->
object ('self)
  method filter : ('self -> 'pkg -> bool) -> 'self
  method map    : ('self -> 'pkg -> 'pkg) -> 'self

  method with_name     : string -> 'self
  method with_priority : int -> 'self

  method packages : 'pkg OpamPackage.Map.t
  method prefix   : OpamTypes.package -> string option

  method name     : string
  method priority : int
  method opam     : OpamTypes.repository
  method kind     : OpamTypes.repository_kind
  method address  : OpamTypes.address
  method root     : OpamTypes.repository_root
end

class virtual linked_repo : repo:OpamTypes.repository ->
object
  method links : OpamFile.Repo.t
end

class opam_package : opam:OpamFile.OPAM.t ->
object
  method name    : string
  method opam    : OpamFile.OPAM.t
  method package : OpamPackage.t

  method depends : OpamTypes.formula
  method depopts : OpamTypes.formula

  method href    : Uri.t
  method tags    : string list
  method title   : string
  method version : string
end

val package_set_of_map : 'a OpamPackage.Map.t -> OpamPackage.Set.t
val opam_universe :
  ('a *
   < conflicts : OpamTypes.formula;
     depends   : OpamTypes.formula;
     depopts   : OpamTypes.formula;
     ..
   >) OpamPackage.Map.t -> OpamTypes.universe

class ['repo, 'pkg] universe :
object ('self)
  constraint 'repo =
  < filter        : ('repo -> 'pkg -> bool) -> 'repo;
    map           : ('repo -> 'pkg -> 'pkg) -> 'repo;
    name          : string;
    opam          : OpamTypes.repository;
    packages      : 'pkg OpamPackage.Map.t;
    priority      : int;
    with_priority : int -> 'repo;
    ..
  >
  constraint 'pkg =
  < package   : OpamTypes.package;
    conflicts : OpamTypes.formula;
    depends   : OpamTypes.formula;
    depopts   : OpamTypes.formula;
    ..
  >

  method load_repos   : 'repo OpamRepositoryName.Map.t -> 'self
  method add_repo     : 'repo -> 'self
  method push_repo    : 'repo -> 'self

  method map_repos    : ('repo -> 'repo) -> 'self
  method map          : ('repo -> 'pkg -> 'pkg) -> 'self

  method filter_repos : ('repo -> bool) -> 'self
  method filter       : ?depopts:bool -> ('repo -> 'pkg -> bool) -> 'self

  method repos        : 'repo OpamRepositoryName.Map.t
  method package      : OpamTypes.package -> 'repo * 'pkg
  method packages     : ('repo * 'pkg)        OpamPackage.Map.t
  method package_set  : OpamPackage.Set.t
  method versions     : OpamTypes.version_set OpamPackage.Name.Map.t
  method max_versions : OpamTypes.version     OpamPackage.Name.Map.t
end
