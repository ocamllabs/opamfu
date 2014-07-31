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

open OpamFile

exception Opam_file_not_found of OpamFilename.t
exception Opam_file_parse_error of OpamFilename.t

class type universal_package =
object
  method package   : OpamTypes.package
  method conflicts : OpamTypes.formula
  method depends   : OpamTypes.formula
  method depopts   : OpamTypes.formula
end

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

class package ~opam_path =
  let opam =
    try OPAM.read opam_path with
    | Not_found -> raise (Opam_file_not_found opam_path)
    (*Printf.printf "Cannot find an OPAM file for %s, skipping.\n"
      (OpamPackage.to_string nv);*)
    | Parsing.Parse_error | OpamSystem.Internal_error _ ->
      raise (Opam_file_parse_error opam_path)
  (*Printf.printf "Errors while parsing %s OPAM file, skipping.\n"
    (OpamPackage.to_string nv);*)
  in
  let name = OPAM.name opam in
  let version = OPAM.version opam in
  let package = OpamPackage.create name version in
  let name = OpamPackage.Name.to_string name in
  let version = OpamPackage.Version.to_string version in
  let href = href ~href_base:Uri.(of_string "packages/") name version in
  let title = Printf.sprintf "%s %s" name version in
  let depends = OPAM.depends opam in
  let depopts = OPAM.depopts opam in
  let conflicts = OPAM.conflicts opam in
  let tags = OPAM.tags opam in
object (_ : 'self)
  constraint 'self = #universal_package

  method opam        = opam (* TODO: more fields? *)

  method depends     = depends
  method depopts     = depopts
  method conflicts   = conflicts

  method package     = package
  method name        = name
  method version     = version
  method href        = href
  method title       = title
  method tags        = tags
end

class md_text (md : string) = object
  method md = md
end

class ['text] described_package ~new_text ~descr_path =
  let descr = Descr.safe_read descr_path in
  let synopsis = Descr.synopsis descr in
  let body = Descr.body descr in
object
  constraint 'text = #md_text

  method synopsis : 'text = new_text synopsis
  method body     : 'text = new_text body
end

class downloadable_package ~url_path =
  let url_file = URL.read url_path in
  let url = Uri.of_string (OpamTypes.string_of_address (URL.url url_file)) in
  let kind = URL.kind url_file in
  let checksum = URL.checksum url_file in
object
  (* TODO: mirrors? *)
  method url      = url
  method kind     = kind
  method checksum = checksum
end
