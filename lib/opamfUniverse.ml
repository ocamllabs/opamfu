(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012 INRIA                                                *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2013 David Sheets                                         *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

type pkg_idx = (OpamTypes.repository_name * string option) OpamTypes.package_map

type repo_ref = [
| `Path of string
| `Local of string
| `Opam
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

let pred_sep = ':'
let repo_ref_ns_sep = ':'

let repo_ref_of_string s = match OpamMisc.split s repo_ref_ns_sep with
  | "path"::r -> `Path String.(concat (make 1 repo_ref_ns_sep) r)
  | "local"::r -> `Local String.(concat (make 1 repo_ref_ns_sep) r)
  | "opam"::r -> `Opam
  | _::_ | [] -> raise Not_found

let string_of_repo_ref = function
  | `Path p  -> Printf.sprintf "path%c%s" repo_ref_ns_sep p
  | `Local l -> Printf.sprintf "local%c%s" repo_ref_ns_sep l
  | `Opam -> "opam"

let href ?href_base name version =
  let base = Printf.sprintf "%s/%s.%s/" name name version in
  let base = Uri.of_string base in
  match href_base with
  | None   -> base
  | Some p -> Uri.resolve "http" p base

let is_base_package pkg =
  List.mem (OpamPackage.name pkg) OpamState.base_packages

let remove_base_packages pkg_idx =
  OpamPackage.Map.filter (fun pkg _ -> not (is_base_package pkg)) pkg_idx

let versions_of_packages pkg_set =
  OpamPackage.Set.fold (fun nv map ->
    let name = OpamPackage.name nv in
    let versions =
      try OpamPackage.Name.Map.find name map
      with Not_found -> OpamPackage.Version.Set.empty
    in
    let versions = OpamPackage.Version.Set.add
      (OpamPackage.version nv) versions in
    OpamPackage.Name.Map.add name versions map
  ) pkg_set OpamPackage.Name.Map.empty

let max_versions_of_versions versions =
  OpamPackage.Name.Map.map (fun versions ->
    OpamPackage.Version.Set.max_elt versions
  ) versions

(* Create a reverse version constraint map
   (package -> package_name -> version_constraint) *)
let reverse_deps formulas versions =
  let open OpamPackage in
  let add_version map pkg revdep =
    let name = name revdep in
    let version = version revdep in
    let revmap =
      try Map.find pkg map
      with Not_found -> Name.Map.empty
    in
    let revdepvs =
      try Name.Map.find name revmap
      with Not_found -> Version.Set.empty
    in
    Map.add pkg
      (Name.Map.add name
         Version.Set.(add version revdepvs)
         revmap)
      map
  in
  let depnames_of_formula f = List.fold_left (fun depset (name,_) ->
    if is_base_package (create name Version.pinned)
    then depset
    else Name.Set.add name depset
  ) Name.Set.empty (OpamFormula.atoms f)
  in
  let add_satisfiers pkg f = Name.Set.fold (fun name map ->
    Version.Set.fold (fun v map ->
      let nvsetmap = Name.Map.singleton name (Version.Set.singleton v) in
      if OpamfuFormula.(could_satisfy nvsetmap (of_opam_formula f))
      then add_version map (create name v) pkg
      else map
    ) (Name.Map.find name versions) map
  ) in
  let revdeps = Map.fold (fun pkg f deps ->
    add_satisfiers pkg f (depnames_of_formula f) deps
  ) formulas Map.empty in
  Map.map (Name.Map.mapi (fun name subset ->
    OpamfuFormula.dnf_of_version_subset (Name.Map.find name versions) subset
  )) revdeps

let satisfies preds repo package =
  let rec is_satisfied = function
    | Tag t  -> List.mem t package#tags
    | Repo r -> r = (OpamRepositoryName.to_string repo#name)
    | Not p  -> not (is_satisfied p)
    | Depopt -> false (* TODO: correct? *)
    | Pkg p  -> p = package#name
  in
  let rec aux = function
    | [] -> false
    | pred::rest ->
      if List.for_all is_satisfied pred then true else aux rest
  in
  if preds = [] then true else aux preds

class ['pkg] repo ~repo ~new_pkg =
  let pkg_prefixes = OpamRepository.packages_with_prefixes repo in
  let packages = OpamPackage.Map.mapi (fun package prefix ->
    let opam =
      let file = OpamPath.Repository.opam repo prefix package in
      try OpamFile.OPAM.read file with
      | Not_found -> raise (Opam_file_not_found file)
    (*Printf.printf "Cannot find an OPAM file for %s, skipping.\n"
      (OpamPackage.to_string nv);*)
      | Parsing.Parse_error | OpamSystem.Internal_error _ ->
        raise (Opam_file_parse_error file)
  (*Printf.printf "Errors while parsing %s OPAM file, skipping.\n"
    (OpamPackage.to_string nv);*)
    in
    new_pkg ~opam
  ) pkg_prefixes in
object (self : 'self)
  val packages = packages
  val name = repo.repo_name
  val priority = repo.repo_priority
  val repo = repo

  method filter (p : 'self -> 'pkg -> bool) =
    let packages = OpamPackage.Map.filter (fun _k -> p self) packages in
    {< packages = packages >}

  method map f =
    let packages = OpamPackage.Map.map (f self) packages in
    {< packages = packages >}

  method with_name repo_name =
    let repo_name = OpamRepositoryName.of_string repo_name in
    {< repo = { repo with repo_name }; name = repo_name >}
  method with_priority priority =
    {< repo = { repo with repo_priority=priority }; priority = priority >}

  method prefix package = OpamPackage.Map.find package pkg_prefixes

  method packages = packages
  method opam     = repo

  method name     = OpamRepositoryName.to_string name
  method priority = priority
  method kind     = repo.repo_kind
  method address  = repo.repo_address
  method root     = repo.repo_root
end

class virtual linked_repo ~repo = object (self)
  val links = lazy (
    OpamFile.Repo.safe_read (OpamPath.Repository.repo repo)
  )

  method links = Lazy.force links (* TODO: fields? *)
end

class opam_package ~opam =
  let name = OpamFile.OPAM.name opam in
  let version = OpamFile.OPAM.version opam in
  let package = OpamPackage.create name version in
  let name = OpamPackage.Name.to_string name in
  let version = OpamPackage.Version.to_string version in
  let href = href ~href_base:Uri.(of_string "packages/") name version in
  let title = Printf.sprintf "%s %s" name version in
  let depends = OpamFile.OPAM.depends opam in
  let depopts = OpamFile.OPAM.depopts opam in
  let tags = OpamFile.OPAM.tags opam in
object
  method opam        = opam

  method depends     = depends
  method depopts     = depopts

  method package     = package
  method name        = name
  method version     = version
  method href        = href
  method title       = title
  method tags        = tags
end

let package_set_of_map m = OpamPackage.(Set.of_list (Map.keys m))

let opam_universe packages =
  let package_set = package_set_of_map packages in
  { OpamSolver.empty_universe with
    u_packages  = package_set;
    u_action    = Depends;
    u_available = package_set; (* TODO: ok? check opam's semantics *)
    u_depends   = OpamPackage.Map.map (fun (_,p) -> p#depends)   packages;
    u_depopts   = OpamPackage.Map.map (fun (_,p) -> p#depopts)   packages;
    u_conflicts = OpamPackage.Map.map (fun (_,p) -> p#conflicts) packages;
  }

class ['repo,'pkg] universe = object (self)
  val repos        = OpamRepositoryName.Map.empty
  val packages : ('repo * 'pkg) OpamPackage.Map.t Lazy.t =
    lazy OpamPackage.Map.empty
  val package_set  = lazy OpamPackage.Set.empty
  val versions     = lazy OpamPackage.Name.Map.empty
  val max_versions = lazy OpamPackage.Name.Map.empty

  method load_repos repos =
    let repo_map = OpamRepositoryName.Map.map (fun r -> r#opam) repos in
    let pkg_idx = OpamRepository.package_index repo_map in
    let pkgs_by_repo = OpamPackage.Map.fold (fun pkg (repo,_) map ->
      let pkg_set =
        try OpamRepositoryName.Map.find repo map
        with Not_found -> OpamPackage.Set.empty
      in
      OpamRepositoryName.Map.add repo (OpamPackage.Set.add pkg pkg_set) map
    ) pkg_idx OpamRepositoryName.Map.empty
    in

    let repos = OpamRepositoryName.(Map.(map (fun r ->
      let pset = find (of_string r#name) pkgs_by_repo in
      r#filter (fun _r p -> OpamPackage.Set.mem p#package pset)
    ) repos)) in
    let packages = lazy (OpamRepositoryName.Map.fold (fun _ repo m ->
      OpamPackage.Map.(fold (fun k v a -> add k (repo,v) a) repo#packages m)
    ) repos OpamPackage.Map.empty) in
    let package_set = lazy (package_set_of_map (Lazy.force packages)) in
    let versions = lazy (versions_of_packages (Lazy.force package_set)) in
    let max_versions = lazy (max_versions_of_versions (Lazy.force versions)) in

    {<
      repos        = repos;
      packages     = packages;
      package_set  = package_set;
      versions     = versions;
      max_versions = max_versions;
    >}

  method add_repo (repo : 'repo) = self#load_repos
    OpamRepositoryName.(Map.add (of_string repo#name) repo repos)

  method push_repo (repo : 'repo) =
    let priority = 10 + OpamRepositoryName.Map.fold (fun _ r a ->
      max r#priority a
    ) repos 0 in
    self#add_repo (repo#with_priority priority)

  method map_repos f = self#load_repos (OpamRepositoryName.Map.map f repos)
  method map f =
    self#load_repos (OpamRepositoryName.Map.map (fun r -> r#map f) repos)

  method filter_repos p =
    self#load_repos (OpamRepositoryName.Map.filter (fun _k -> p) repos)

  method filter ?(depopts=false) p =
    let packages = Lazy.force packages in
    let package_set = Lazy.force package_set in
    let universe = opam_universe packages in
    let dep_closure = OpamSolver.dependencies ~depopts ~installed:false universe
      (OpamPackage.Set.filter (fun pkg ->
        let repo, pkg = OpamPackage.Map.find pkg packages in
        p repo pkg
       ) package_set) in
    let package_set = OpamPackage.Set.of_list dep_closure in
    self#map_repos (fun r -> r#filter (fun _r p ->
      OpamPackage.Set.mem p#package package_set
    ))

  method repos        = repos
  method package pkg  = OpamPackage.Map.find pkg self#packages
  method packages     = Lazy.force packages
  method package_set  = Lazy.force package_set
  method versions     = Lazy.force versions
  method max_versions = Lazy.force max_versions
end

(*
let universe_of_repo_stack repo_stack =
  let t = OpamState.load_state "opamfu" in
  let opam_repos = t.OpamState.Types.repositories in
  let repos,_ = List.fold_left
    (fun (rmap,repo_priority) repo -> match repo with
    | `Path path ->
      let repo_name = OpamRepositoryName.of_string (string_of_repository repo) in
      OpamRepository.Map.add repo_name
        (repo,{
          (OpamRepository.local (OpamFilename.Dir.of_string path))
               with repo_priority; repo_name;
        }) rmap,
      repo_priority - 1
    | `Local remote ->
      let repo_name = OpamRepositoryName.of_string (string_of_repository repo) in
      begin
        try
          let repo = OpamRepository.Map.find
            (OpamRepositoryName.of_string remote) opam_repos in
          OpamRepository.Map.add repo_name
            (repo,{ repo with repo_priority; repo_name }) rmap,
          repo_priority - 1
        with Not_found -> raise (Local_remote_not_found remote)
      (*Printf.printf "Local opam remote '%s' not found, skipping.\n%!" remote;
        Printf.printf "Maybe you wanted the 'path' namespace?\n%!";
        rmap, repo_priority
      *)
      end
    | `Opam ->
      List.fold_left (fun (m,i) r ->
        let k = OpamRepositoryName.to_string r.repo_name in
        let repo_name = Printf.sprintf "%s%c%s"
          (string_of_repository repo) repository_ns_sep k in
        let repo_name = OpamRepositoryName.of_string repo_name in
        RepoMap.add repo_name
          (repo,{ r with repo_priority = i; repo_name }) m, i - 1
      ) (rmap, repo_priority) (OpamRepository.sort opam_repos)
    ) (RepoMap.empty,64) repo_stack
  in
*)
(*
class virtual published_repo = object
  method virtual packages : string option OpamPackage.Map.t
  method publication pkg =
end
*)
(*
class virtual published_package = object (self : 'self)
  method virtual pkg  : OpamTypes.package
  method virtual repo : [< publication : OpamTypes.package -> float; ..>,'self] universe
  method published = self#repo#publication self#pkg
end
*)
(*
class md_text : string -> object
  method md : string
end
*)
(*
class ['text] described_package : synopsis : string -> body : string -> object
  method synopsis : 'text
  method body : 'text
end
*)
(*
class downloadable_package : opam_url : OpamFile.URL.t -> object
  method url      : Uri.t
  method mirrors  : Uri.t list
  method kind     : string (* TODO: variant? *)
  method checksum : string option
end
*)
(*
class standard_package = object
  inherit opam_package
  inherit [md_text] described_package
  inherit downloadable_package
end
*)
(*
module Pkg = struct
  (* Build a record representing information about a package *)
  let get_info ~dates repo prefix pkg =
    let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
    let version = OpamPackage.Version.to_string (OpamPackage.version pkg) in
    let href = href ~href_base:Uri.(of_string "packages/") name version in
    let descr = OpamFile.Descr.safe_read
      (OpamPath.Repository.descr repo prefix pkg) in
    let synopsis = OpamFile.Descr.synopsis descr in
    let descr_markdown = OpamFile.Descr.full descr in
    let descr =
      match OpamMisc.cut_at descr_markdown '\n' with
      | None       -> descr_markdown, ""
      | Some (s,d) -> s, d in
    let url =
      let file = OpamPath.Repository.url repo prefix pkg in
      if OpamFilename.exists file then
        Some (OpamFile.URL.read file)
      else
        None in
    try
      let published = OpamPackage.Map.find pkg dates in
      Some {
        name;
        version;
        descr;
        synopsis;
        href;
        title;
        published;
        url;
      }
    with Not_found -> (* TODO: Really? No date = no info? *)
      None

end
*)
(*
let infos repos dates pkg_idx =
  let n = OpamPackage.Map.cardinal pkg_idx in
  let c = ref 1 in
  let msg () =
    Printf.printf "\r++ Building the package infos: %-5d/%d%!" !c n;
    incr c in
  let result = OpamPackage.Map.fold (fun pkg (repo,prefix) map ->
    msg ();
    let repo = RepoMap.find repo repos in
    let info = Pkg.get_info ~dates repo prefix pkg in
    OpamPackage.Map.add pkg info map
  ) pkg_idx OpamPackage.Map.empty in
  Printf.printf "\n%!";
  result
*)
(*
let dates repos pkg_idx =
  let any_opam_path = "*/opam" in
  let parse_git_commit_times =
    let rec read_time pkgs found = function
      | [] -> pkgs,found
      | ln::rest -> read_file pkgs found (float_of_string ln) rest
    and read_file pkgs found time = function
      | [] -> pkgs,found
      | ""::rest -> read_time pkgs found rest
      | path::rest ->
        let suff = String.length any_opam_path - 1 in
        let path = String.(sub path 0 (length path - suff)) in
        let slash = try String.rindex path '/' + 1 with Not_found -> 0 in
        let pkg = String.(sub path slash (length path - slash)) in
        match OpamPackage.of_string_opt pkg with
        | Some pkg ->
          if OpamPackage.Map.mem pkg pkgs
          then read_file
            (OpamPackage.Map.remove pkg pkgs)
            (OpamPackage.Map.add pkg time found)
            time rest
          else read_file pkgs found time rest
        | None -> read_file pkgs found time rest
    in read_time
  in
  let repo_idx = index_by_repo pkg_idx in
  let missing, found = RepoMap.fold (fun repo pkg_map (missing,found) ->
    let command = [
      "git"; "log"; "--name-only"; "--diff-filter=A"; "--reverse";
      "--pretty=format:%ct"; "--"; any_opam_path;
    ] in
    let repo_name = OpamRepositoryName.to_string repo in
    let repo = RepoMap.find repo repos in
    try
      let times = OpamFilename.in_dir
        (OpamFilename.dirname_dir (OpamPath.Repository.packages_dir repo))
        (fun () -> OpamSystem.read_command_output command)
      in
      let unmatched,found = parse_git_commit_times pkg_map found times in
      OpamPackage.Map.(fold add unmatched missing),found
    with (OpamSystem.Process_error _ | Failure "float_of_string" as e) ->
      OpamGlobals.warning "Date retrieval for %s using" repo_name;
      OpamGlobals.warning "%s" (String.concat " " command);
      OpamGlobals.warning "failed with:\n%s" (Printexc.to_string e);
      (OpamPackage.Map.(fold add pkg_map missing),found)
  ) repo_idx OpamPackage.Map.(empty, empty) in
  if OpamPackage.Map.cardinal missing > 0
  then begin
    OpamGlobals.warning "Couldn't retrieve creation date for:";
    OpamPackage.Map.fold (fun pkg prefix map ->
      OpamGlobals.warning "%s" (OpamPackage.to_string pkg);
      let (repo,prefix) = OpamPackage.Map.find pkg pkg_idx in
      let repo = RepoMap.find repo repos in
      let opam_filename = OpamPath.Repository.opam repo prefix pkg in
      (* TODO: errors? *)
      let opam_stat = Unix.stat (OpamFilename.to_string opam_filename) in
      OpamPackage.Map.add pkg opam_stat.Unix.st_mtime map
    ) missing found
  end
  else found
*)
