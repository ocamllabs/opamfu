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

val versions_of_packages :
  OpamPackage.Set.t -> OpamPackage.Version.Set.t OpamPackage.Name.Map.t
val max_versions_of_versions :
  OpamPackage.Version.Set.t OpamPackage.Name.Map.t ->
  OpamTypes.version OpamPackage.Name.Map.t

val reverse_deps :
  OpamFormula.t OpamPackage.Map.t ->
  OpamfuFormula.version_set OpamPackage.Name.Map.t ->
  OpamfuFormula.version_dnf OpamPackage.Name.Map.t OpamPackage.Map.t

val opam_universe :
  ('a * #universal_package) OpamPackage.Map.t -> OpamTypes.universe

class ['repo, 'pkg] universe :
object ('self)
  constraint 'repo = 'pkg #universal_repo
  constraint 'pkg = #universal_package

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
