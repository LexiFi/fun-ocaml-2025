(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Sebastien Hinderer, Tarides, Paris                   *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Infrastructure to support user-defined printers in toplevels and debugger *)

type printer_type = Types.type_expr -> Types.type_expr

val type_arrow : Types.type_expr -> Types.type_expr -> Types.type_expr

val printer_type_new : printer_type
val printer_type_old : printer_type

type kind =
  | Old of Types.type_expr
  (* 'a -> unit *)
  | Simple of Types.type_expr
  (* Format.formatter -> 'a -> unit *)
  | Generic of { ty_path: Path.t; arity: int; }
  (* (formatter -> 'a1 -> unit) ->
     (formatter -> 'a2 -> unit) ->
     ... ->
     (formatter -> 'an -> unit) ->
     formatter -> ('a1, 'a2, ..., 'an) t -> unit
  *)

type error = [
  | `Unbound_identifier of Longident.t
  | `Wrong_type of Longident.t
  | `No_active_printer of Path.t
]

val find_printer : Env.t -> Longident.t -> (Path.t * kind, error) result

val report_error : Format.formatter -> error -> unit
