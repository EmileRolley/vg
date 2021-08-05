(*---------------------------------------------------------------------------
   Copyright (c) 2014 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bigarray
open Gg
open Vg
open Vgr
module P = Private

type render_fun_input = [ `End | `Image of size2 * box2 * P.Data.image ]

type r_state = {
  (* Current path being built. *)
  path : v2 list list;
  (* Current cursor position. *)
  curr : v2;
  (* Stores the rendered {{!Vg.image}} *)
  bitmap : (float, float32_elt, c_layout) Array1.t;
}
(** Render state. NOTE: this should not be exposed. *)

let render_target (_ : P.renderer) (_ : [< dst ]) : bool * P.render_fun =
  let render v k r =
    match v with
    | `End ->
        Printf.printf "In `End\n";
        k r
    | `Image (size, view, i) ->
        Printf.printf "In `Image\n";
        I.pp (Format.formatter_of_out_channel stdout) (P.I.of_data i);
        k r
  in
  (false, render)

let target () = Vgr.Private.create_target render_target

(*---------------------------------------------------------------------------
   Copyright (c) 2014 The vg programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
