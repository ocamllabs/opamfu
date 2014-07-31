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

type pred =
| Tag of string
| Depopt
| Not of pred
| Repo of string
| Pkg of string

type pred_dnf = pred OpamFormula.dnf

let pred_sep = ':'

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
