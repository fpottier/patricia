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

open Monolith

(* This is the reference implementation. *)
module R = Reference

(* -------------------------------------------------------------------------- *)

(* This is the candidate implementation. *)

(* We wrap it with extra tests of the sharing properties that we expect. *)

module C = struct

  include Patricia

  let () =
    dprintf "          open Patricia;;\n"

  let add x v m =
    let m' = add x v m in
    if equal (=) m m' then assert (m == m');
    m'

  let remove x m =
    let m' = remove x m in
    if equal (=) m m' then assert (m == m');
    m'

  let union m1 m2 =
    let m' = union m1 m2 in
    if equal (=) m' m2 then assert (m2 == m');
    m'

end

(* -------------------------------------------------------------------------- *)

(* The abstract type of maps. *)

let check _model =
  C.check, constant "check"

let map =
  declare_abstract_type ~check ()

(* -------------------------------------------------------------------------- *)

(* Propose several key generators. *)

(* [arbitrary_key] produces an arbitrary key within a certain interval.
   The interval must be reasonably small, otherwise the fuzzer wastes
   time trying lots of different keys. *)

let arbitrary_key =
  Gen.closed_interval (-32) 32

(* [extreme_key] produces a key near [min_int] and [max_int]. *)

let extreme_key () =
  if Gen.bool() then
    min_int + Gen.lt 16 ()
  else
    max_int - Gen.lt 16 ()

(* [present_key m] produces a key that is present in the map [m]. Its
   implementation is not efficient, but we will likely be working with
   very small maps, so this should be acceptable. *)

let present_key (m : R.map) () =
  let n = R.cardinal m in
  let i = Gen.int n () in
  let key, _value = List.nth (R.bindings m) i in
  key

(* [key m] combines the above generators. *)

let key m () =
  if Gen.bool() then
    arbitrary_key()
  else if Gen.bool() then
    extreme_key()
  else
    present_key m ()

(* Declare concrete types. *)

let key_maybe_in_map m =
  int_within (key m)

let key =
  key_maybe_in_map R.empty

(* -------------------------------------------------------------------------- *)

(* Declare the type [value] as an alias for [int]. *)

(* We generate values within a restricted range, because we do not expect
   that a wide range of values is required in order to expose bugs. *)

let value =
  lt 16

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = map in
  declare "empty" spec R.empty C.empty;

  let spec = map ^> bool in
  declare "is_empty" spec R.is_empty C.is_empty;

  let spec = key ^> value ^> map in
  declare "singleton" spec R.singleton C.singleton;

  let spec = map ^> bool in
  declare "is_singleton" spec R.is_singleton C.is_singleton;

  let spec = rot3 (map ^>> fun m -> key_maybe_in_map m ^> value ^> map) in
  declare "add" spec R.add C.add;

  let spec = rot2 (map ^>> fun m -> key_maybe_in_map m ^> bool) in
  declare "mem" spec R.mem C.mem;

  let spec = rot2 (map ^>> fun m -> key_maybe_in_map m ^!> value) in
  declare "find" spec R.find C.find;

  let spec = rot2 (map ^>> fun m -> key_maybe_in_map m ^> map) in
  declare "remove" spec R.remove C.remove;

  let spec = key ^> map ^!> key *** map in
  declare "find_and_remove" spec R.find_and_remove C.find_and_remove;

  let spec = map ^> map ^> map in
  declare "union" spec R.union C.union;

  let spec = map ^!?> int *** value in
  declare "choose" spec R.choose C.choose;

  let spec = map ^> int in
  declare "cardinal" spec R.cardinal C.cardinal;

  (* [iter], [fold], [fold_rev], [map], [mapi] are not tested *)

  let spec = map ^> map ^> bool in
  declare "equal (=)" spec (R.equal (=)) (C.equal (=));

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 16 in
  main fuel
