(******************************************************************************)
(*                                                                            *)
(*                                  Patricia                                  *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2025--2025 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

include Map.Make(Int)

type map =
  int t

let is_singleton m =
  cardinal m = 1

let find_and_remove x m =
  let v = find x m in
  v, remove x m

let fine_union d m1 m2 =
  union (fun _k x y -> Some (d x y)) m1 m2

let union m1 m2 =
  fine_union (fun _x y -> y) m1 m2

(* [choose] is nondeterministic. *)

open Monolith
open Monolith.Print
let string, (^^), utf8format = PPrint.(string, (^^), utf8format)

let expected_some_result () =
  (* The map is nonempty, yet the candidate returned nothing. *)
  Invalid (fun observed ->
    assert_ (observed ^^ string " <> None") ^^
    comment (string "The map is supposed to be nonempty.")
      (* If desired, we could print a list of the keys which
         we expect to exist in the map. *)
  )

let nonexistent_key k =
  (* The key [k] does not exist in the map. *)
  Invalid (fun _ -> comment (utf8format
    "The key %d is not supposed to exist in the map." k))

let incorrect_value k rv cv =
  Invalid (fun _ -> comment (utf8format
    "The key %d is supposed to be associated with %d, not %d."
    k rv cv))

let candidate_has_returned candidate m k cv =
  match find_opt k m with
  | None ->
      nonexistent_key k
  | Some rv ->
      if rv = cv then
        Valid candidate
      else
        incorrect_value k rv cv

(* [choose] is nondeterministic. *)

let choose (m : int t) (candidate : (key * int, exn) result)
: (key * int, exn) result diagnostic =
  match is_empty m, candidate with
  | false, Error Not_found ->
      expected_some_result()
  | true, Error Not_found ->
      Valid candidate
  | _, Error e ->
      (* The candidate has raised some exception other than [Not_found]. *)
      Invalid (fun _ -> comment (utf8format
        "Candidate has raised %s." (Printexc.to_string e)))
  | _, Ok (k, cv) ->
      candidate_has_returned candidate m k cv
