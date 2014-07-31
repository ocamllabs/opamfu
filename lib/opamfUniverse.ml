(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open OpamfuPackage
open OpamfuRepo

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

let package_set_of_map m = OpamPackage.(Set.of_list (Map.keys m))

let opam_universe packages =
  let package_set = package_set_of_map packages in
  OpamTypes.({
    OpamSolver.empty_universe with
      u_packages  = package_set;
      u_action    = Depends;
      u_available = package_set; (* TODO: ok? check opam's semantics *)
      u_depends   = OpamPackage.Map.map (fun (_,p) -> p#depends)   packages;
      u_depopts   = OpamPackage.Map.map (fun (_,p) -> p#depopts)   packages;
      u_conflicts = OpamPackage.Map.map (fun (_,p) -> p#conflicts) packages;
  })

class ['repo,'pkg] universe = object (self)
  constraint 'repo = 'pkg #universal_repo
  constraint 'pkg  = #universal_package
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

class virtual ['repo,'pkg] reverse_universe = object (self)
  val mutable rev_depends = lazy OpamPackage.Map.empty
  val mutable rev_depopts = lazy OpamPackage.Map.empty

  method virtual packages : ('repo * 'pkg) OpamPackage.Map.t
  method virtual versions : OpamTypes.version_set OpamPackage.Name.Map.t

  (* TODO: fix clones and caches *)
  initializer begin
    rev_depends <- lazy (
      reverse_deps
        (OpamPackage.Map.map (fun (_r, p) -> p#depends) self#packages)
        self#versions
    );
    rev_depopts <- lazy (
      reverse_deps
        (OpamPackage.Map.map (fun (_r, p) -> p#depopts) self#packages)
        self#versions
    );
  end

  method rev_depends pkg = OpamPackage.Map.find pkg (Lazy.force rev_depends)
  method rev_depopts pkg = OpamPackage.Map.find pkg (Lazy.force rev_depopts)
end

let of_repo_ref_stack ~new_universe ~new_repo repo_stack =
  let open OpamTypes in
  let opam_repos = current_opam_repos () in
  let repo_stack = List.fold_right (fun repo_ref orl ->
    (opam_repos_of_repo_ref ~opam_repos ~repo_ref) @ orl
  ) repo_stack [] in
  let sz = List.length repo_stack in
  let repo_map, _ = List.fold_left (fun (repo_map, repo_priority) repo ->
    let repo = { repo with repo_priority } in
    OpamRepositoryName.Map.add repo.repo_name (new_repo ~repo) repo_map,
    repo_priority - 10
  ) (OpamRepositoryName.Map.empty, sz*10) repo_stack in
  (new_universe ())#load_repos repo_map  

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
