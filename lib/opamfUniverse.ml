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

module RepoMap = OpamRepositoryName.Map

type 'a pkg = {
  name     : string;
  version  : string;
  descr    : 'a;
  synopsis : string;
  href     : Uri.t;
  title    : string;
  update   : float;
  url      : OpamFile.URL.t option;
}

type repository = Path of string | Local of string | Opam

type pred =
| Tag of string
| Depopt
| Not of pred
| Repo of string
| Pkg of string

type index = Index_pred | Index_all

type 'a t = {
  repos       : OpamTypes.repository repository_name_map;
  preds       : pred list list;
  index       : index;
  pkg_idx     : (repository_name * string option) package_map;
  versions    : version_set name_map;
  max_packages: package_set;
  max_versions: version name_map;
  reverse_deps: name_set name_map;
  pkgs_infos  : 'a pkg option package_map;
  pkgs_opams  : OpamFile.OPAM.t package_map;
  pkgs_dates  : float package_map;
}

module Repo = struct
  (* Get the repository opam file corresponding to a repo *)
  let links repo =
    let repo_file = OpamPath.Repository.repo repo in
    OpamFile.Repo.safe_read repo_file    
end

module Pkg = struct
  (* Get the repository corresponding to a package in a universe *)
  let to_repo universe pkg =
    let { pkg_idx; repos } = universe in
    let repo_name, _ = OpamPackage.Map.find pkg pkg_idx in
    let repo = OpamRepositoryName.Map.find repo_name repos in
    repo

  let are_preds_satisfied universe pkg =
    try
      let pkg_opam = OpamPackage.Map.find pkg universe.pkgs_opams in
      let tags = OpamFile.OPAM.tags pkg_opam in
      let rec is_satisfied = function
        | Tag t -> List.mem t tags
        | Repo r ->
          let (rn,_) = OpamPackage.Map.find pkg universe.pkg_idx in
          r = (OpamRepositoryName.to_string rn)
        | Not p -> not (is_satisfied p)
        | Depopt-> false
        | Pkg p ->
          let name = OpamPackage.(Name.to_string (name pkg)) in
          p = name
      in
      let rec aux = function
        | [] -> false
        | pred::rest ->
          if List.for_all is_satisfied pred then true else aux rest
      in
      if universe.preds = [] then true else aux universe.preds
    with Not_found -> false

  let href ?href_base name version =
    let name = OpamPackage.Name.to_string name in
    let version = OpamPackage.Version.to_string version in
    let base = Printf.sprintf "%s/%s.%s/" name name version in
    let base = Uri.of_string base in
    match href_base with
    | None   -> base
    | Some p -> Uri.resolve "http" p base

  (* Build a record representing information about a package *)
  let get_info ~dates repo prefix pkg =
    let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
    let version = OpamPackage.Version.to_string (OpamPackage.version pkg) in
    let href = href ~href_base:Uri.(of_string "pkg/")
      (OpamPackage.name pkg) (OpamPackage.version pkg) in
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
    let title = Printf.sprintf "%s %s" name version in
    try
      let update =
        OpamPackage.Map.find pkg dates in
      Some {
        name;
        version;
        descr;
        synopsis;
        href;
        title;
        update;
        url;
      }
    with Not_found ->
      None
end

let remove_base_packages pkg_idx =
  OpamPackage.Map.filter (fun pkg _ ->
    let name = OpamPackage.name pkg in
    not (OpamMisc.starts_with ~prefix:"base" (OpamPackage.Name.to_string name))
  ) pkg_idx

let versions pkg_idx =
  OpamPackage.Map.fold (fun nv _ map ->
    let name = OpamPackage.name nv in
    let versions, map =
      try
        let versions = OpamPackage.Name.Map.find name map in
        let map = OpamPackage.Name.Map.remove name map in
        versions, map
      with Not_found ->
        OpamPackage.Version.Set.empty, map in
    let versions = OpamPackage.Version.Set.add
      (OpamPackage.version nv) versions in
    OpamPackage.Name.Map.add name versions map
  ) pkg_idx OpamPackage.Name.Map.empty

let max_versions versions =
  OpamPackage.Name.Map.map (fun versions ->
    OpamPackage.Version.Set.max_elt versions
  ) versions

let max_packages max_versions =
  OpamPackage.Name.Map.fold (fun name version set ->
    OpamPackage.Set.add (OpamPackage.create name version) set
  ) max_versions OpamPackage.Set.empty

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

(* Get the last update timestamp of a package in a given repository *)
let last_update repo prefix package =
  let opam_filename = OpamPath.Repository.opam repo prefix package in
  try
    let command =
      [ "git"; "log"; "--reverse"; "--pretty=format:%ct"; "--";
        "*/" ^ OpamPackage.to_string package ^ "/opam" ] in
    let return =
      OpamFilename.in_dir
        (OpamFilename.dirname_dir (OpamPath.Repository.packages_dir repo))
        (fun () -> OpamSystem.read_command_output command) in
    match return with
    | ts::_ -> float_of_string ts
    | [] -> raise Not_found
  with
  | (OpamSystem.Process_error _ | Failure "float_of_string" | Not_found as e) ->
    OpamGlobals.warning "last_update of %s failed with %s\n"
      (OpamPackage.to_string package) (Printexc.to_string e);
    let opam_stat = Unix.stat (OpamFilename.to_string opam_filename) in
    opam_stat.Unix.st_mtime

let dates repos pkg_idx =
  OpamPackage.Map.fold (fun pkg (repo,prefix) map ->
    let last_update = last_update (RepoMap.find repo repos) prefix pkg in
    OpamPackage.Map.add pkg last_update map
  ) pkg_idx OpamPackage.Map.empty

(* Create an association list (package_name -> reverse_dependencies) *)
let reverse_dependencies opams =
  let revdeps_tbl: (name, name) Hashtbl.t = Hashtbl.create 300 in
  (* Fill a hash table with reverse dependencies (required by...) *)
  OpamPackage.Map.iter (fun pkg opam ->
    let depends = OpamFile.OPAM.depends opam in
    List.iter (fun (depname,_) ->
      Hashtbl.add revdeps_tbl depname (OpamPackage.name pkg)
    ) (OpamFormula.atoms depends)
  ) opams;
  let names =
    Hashtbl.fold (fun name _ acc -> name :: acc) revdeps_tbl [] in
  (* Build the association list *)
  List.fold_left (fun acc name ->
    let revdeps = Hashtbl.find_all revdeps_tbl name in
    let names = OpamPackage.Name.Set.of_list revdeps in
    OpamPackage.Name.Map.add name names acc
  ) OpamPackage.Name.Map.empty names

let mk_universe_info preds index repos pkg_idx opams =
  let pkg_idx = remove_base_packages pkg_idx in
  let versions = versions pkg_idx in
  let max_versions = max_versions versions in
  let max_packages = max_packages max_versions in
  let reverse_deps = reverse_dependencies opams in
  let pkgs_dates = dates repos pkg_idx in
  let pkgs_infos = infos repos pkgs_dates pkg_idx in
  { repos; preds; index; versions; pkg_idx; max_versions; max_packages;
    reverse_deps; pkgs_infos; pkgs_opams=opams; pkgs_dates }

(* Generate a universe from a stack of repositories *)
let of_repositories ?(preds=[]) index repo_stack =
  let t = OpamState.load_state "opam2web" in
  let opam_repos = t.OpamState.Types.repositories in
  let repos,_ = List.fold_left
    (fun (rmap,repo_priority) -> function
    | Path path ->
      let repo_name = OpamRepositoryName.of_string ("path:"^path) in
      RepoMap.add repo_name
        { (OpamRepository.local (OpamFilename.Dir.of_string path))
        with repo_priority; repo_name } rmap,
      repo_priority - 1
    | Local remote ->
      let repo_name = OpamRepositoryName.of_string ("local:"^remote) in
      begin
        try
          let repo = RepoMap.find
            (OpamRepositoryName.of_string remote) opam_repos in
          RepoMap.add repo_name
            { repo with repo_priority; repo_name } rmap,
          repo_priority - 1
        with Not_found ->
          Printf.printf "Local opam remote '%s' not found, skipping.\n%!" remote;
          Printf.printf "Maybe you wanted the 'path' namespace?\n%!";
          rmap, repo_priority
      end
    | Opam ->
      List.fold_left (fun (m,i) r ->
        let k = r.repo_name in
        let repo_name = OpamRepositoryName.(of_string ("opam:"^(to_string k))) in
        RepoMap.add repo_name { r with repo_priority = i; repo_name } m, i - 1
      ) (rmap, repo_priority) (OpamRepository.sort opam_repos)
    ) (RepoMap.empty,256) repo_stack
  in
  let pkg_idx = OpamRepository.package_index repos in
  let packages = OpamPackage.Set.of_list (OpamPackage.Map.keys pkg_idx) in
  let opams = OpamPackage.Set.fold (fun nv map ->
    try
      let repo, prefix = OpamPackage.Map.find nv pkg_idx in
      let repo = OpamRepositoryName.Map.find repo repos in
      let file = OpamPath.Repository.opam repo prefix nv in
      let opam = OpamFile.OPAM.read file in
      OpamPackage.Map.add nv opam map
    with
    | Not_found ->
      Printf.printf "Cannot find an OPAM file for %s, skipping.\n"
        (OpamPackage.to_string nv);
      map
    | Parsing.Parse_error | OpamSystem.Internal_error _ ->
      Printf.printf "Errors while parsing %s OPAM file, skipping.\n"
        (OpamPackage.to_string nv);
      map
  ) packages OpamPackage.Map.empty
  in
  let universe_info = mk_universe_info preds index repos pkg_idx opams in
  let universe = {
    u_packages  = packages;
    u_action    = Depends;
    u_installed = OpamPackage.Set.empty;
    u_available = packages; (* TODO: ok? check opam's semantics *)
    u_depends   = OpamPackage.Map.map OpamFile.OPAM.depends opams;
    u_depopts   = OpamPackage.Map.map OpamFile.OPAM.depopts opams;
    u_conflicts = OpamPackage.Map.map OpamFile.OPAM.conflicts opams;
    u_installed_roots = OpamPackage.Set.empty;
    u_pinned    = OpamPackage.Name.Map.empty;
  } in
  let dep_closure = OpamSolver.dependencies
    ~depopts:(List.mem [Depopt] preds) ~installed:false universe
    (OpamPackage.Set.filter
       (Pkg.are_preds_satisfied universe_info) packages)
  in
  let packages = OpamPackage.Set.of_list dep_closure in
  let pkg_idx = OpamPackage.Map.filter
    (fun k _ -> OpamPackage.Set.mem k packages) pkg_idx in
  mk_universe_info preds index repos pkg_idx opams

let map f u = 
  { u with pkgs_infos=OpamPackage.Map.map (function
  | None -> None
  | Some x -> Some { x with descr = f x.descr }
  ) u.pkgs_infos
  }
