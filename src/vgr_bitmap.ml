(*---------------------------------------------------------------------------
   Copyright (c) 2014 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vgr
module P = Private

module type BitmapType = sig
  type t

  val create : int -> int -> t

  val get : t -> float -> float -> Gg.Color.t

  val set : t -> float -> float -> Gg.Color.t -> unit

  val width : t -> int

  val height : t -> int
end

module F32_ba : BitmapType = struct
  open Ba

  type t =
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
    * int
    * int

  let stride = 4

  let get_i x y h =
    let x' = int_of_float x in
    let y' = int_of_float y in
    ((x' * h) + y') * stride

  let create w h = (Ba.create Ba.Float32 (w * h * stride), w, h)

  let get (b, _, h) x y =
    let v_rgba = get_i x y h |> Ba.get_v4 b in
    V4.(Color.v (x v_rgba) (y v_rgba) (z v_rgba) (w v_rgba)) |> Color.of_srgb

  let set (b, _, h) x y c = Ba.set_v4 b (get_i x y h) c

  let width (_, w, _) = w

  let height (_, _, h) = h
end

module type S = sig
  type bitmap

  val target : bitmap -> [ `Other ] Vg.Vgr.target
end

module Make (Bitmap : BitmapType) = struct
  type bitmap = Bitmap.t

  type r_state = {
    (* Current path being built. *)
    path : v2 list list;
    (* Current cursor position. *)
    curr : v2;
    (* Stores the rendered {{!Vg.image}} *)
    bitmap : bitmap;
  }

  let render_target (_bitmap : bitmap) (_ : P.renderer) (_ : [< dst ]) :
      bool * P.render_fun =
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

  let target bitmap = Vgr.Private.create_target (render_target bitmap)
end

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
