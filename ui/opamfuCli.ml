(*
* Copyright (c) 2013 David Sheets <sheets@alum.mit.edu>
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

open Cmdliner
open OpamfUniverse

let map f x = Term.(pure f $ x)

let rec parse_pred = function
  | "not"::more -> Not (parse_pred more)
  | "tag"::more -> Tag (String.concat ":" more)
  | "repo"::more -> Repo (String.concat ":" more)
  | "pkg"::more -> Pkg (String.concat ":" more)
  | ["depopt"]  -> Depopt
  | []   -> failwith "filter predicate empty"
  | p::_ -> failwith ("unknown predicate "^p)

let parse_pred preds =
  List.rev_map (fun pred ->
    List.rev_map (fun pred ->
      parse_pred Re_str.(split (regexp_string ":") pred)
    ) pred
  ) preds

let pred = map parse_pred Arg.(
  value & opt_all (list string) [] & info ["where"]
    ~docv:"WHERE_OR"
    ~doc:"Satisfaction of all of the predicates in any comma-separated list implies inclusion")

let index = Arg.(
  value & opt (enum [
    "all", Index_all;
    "where", Index_pred;
  ]) Index_pred & info ["index"]
  ~docv:"INDEX"
  ~doc:"Changes the set of packages for which indices are generated: 'all' or 'where'")

let repositories =
  map (List.map repository_of_string) Arg.(
    value & pos_all string ["opam"] & info []
      ~docv:"REPOSITORY"
      ~doc:"The repositories to consider as the universe. Available namespaces are 'path' for local directories, 'local' for named opam remotes, and 'opam' for the current local opam universe.")
