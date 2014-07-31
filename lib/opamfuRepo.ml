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

open OpamTypes

type repo_ref = [
| `Opam
| `Local of string
| `Path of string
]

exception Local_remote_not_found of string

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

class type ['pkg] universal_repo =
object ('self)
  method filter        : ('self -> 'pkg -> bool) -> 'self
  method map           : ('self -> 'pkg -> 'pkg) -> 'self
  method name          : string
  method opam          : repository
  method packages      : 'pkg OpamPackage.Map.t
  method priority      : int
  method with_priority : int -> 'self
end

class ['pkg] repo ~new_pkg ~repo =
  let pkg_prefixes = OpamRepository.packages_with_prefixes repo in
  let packages = OpamPackage.Map.mapi (fun package prefix ->
    let opam_path = OpamPath.Repository.opam repo prefix package in
    new_pkg ~opam_path
  ) pkg_prefixes in
object (self : 'self)
  constraint 'self = 'pkg #universal_repo

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

class virtual dated_repo ~repo = object (self)
  val f
end

let current_opam_repos () =
  let t = OpamState.load_state "opamfu" in
  t.OpamState.Types.repositories

let opam_repos_of_repo_ref ~opam_repos ~repo_ref =
  let repo_name = string_of_repo_ref repo_ref in
  match repo_ref with
  | `Path path ->
    let repo_name = OpamRepositoryName.of_string repo_name in
    [{ (OpamRepository.local (OpamFilename.Dir.of_string path)) with repo_name }]
  | `Local remote -> begin
    let repo_name = OpamRepositoryName.of_string repo_name in
    try
      let remote = OpamRepositoryName.of_string remote in
      [{ (OpamRepositoryName.Map.find remote opam_repos) with repo_name }]
    with Not_found -> raise (Local_remote_not_found remote)
      (*Printf.printf "Local opam remote '%s' not found, skipping.\n%!" remote;
        Printf.printf "Maybe you wanted the 'path' namespace?\n%!";
        rmap, repo_priority
      *)
  end
  | `Opam ->
    List.fold_right (fun r rl ->
      let k = OpamRepositoryName.to_string r.repo_name in
      let repo_name = Printf.sprintf "%s%c%s" repo_name repo_ref_ns_sep k in
      let repo_name = OpamRepositoryName.of_string repo_name in
      { r with repo_name }::rl
    ) (OpamRepository.sort opam_repos) []
      
