(*---------------------------------------------------------------------------
   Copyright (c) 2014 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Vg bitmap renderer dependency free. *)

(** {1 Generic bitmap interface} *)

(**
   [BitmapType] is a generic interface allowing to use custom [bitmap]
   implementations.
   (For provided one, see {!providedtype}).
*)
module type BitmapType = sig
  type t
  (** [t] is the type of the bitmap implementation used to store a rasterized
      {!Vg.image}*)

  val create : int -> int -> t
  (** [create w h] must return an initialized bitmap corresponding to an image
      with a width of [w] and a height of [h]. *)

  val get : t -> float -> float -> Gg.Color.t
  (** [get bitmap x y] must return the color of the stored pixel with
      coordinates ([x], [y]). *)

  val set : t -> float -> float -> Gg.Color.t -> unit
  (** [set bitmap x y c] must update the [bitmap] by associated the [c] color
      to the pixel with coordinates ([x], [y]). *)

  val width : t -> int
  (** [width bitmap] must return the width of the represented image by the [bitmap]. *)

  val height : t -> int
  (** [width bitmap] must return the height of the represented image by the [bitmap]. *)
end

(** {2:providedtype Provided bitmap implementations} *)

module F32_ba : BitmapType
(** Default {!BitmapType} implementation using a {!Gg.Ba} (linear {!Bigarray}). *)

(**
   RGBa color channels of a pixel are stored in a row:
{v
 0        1        2        3        4           (x*w+y)*c
 +--------+--------+--------+--------+--------+-------+--------+---
 | (0,0)  | (0,0)  | (0,0)  | (0,0)  | (0,1)  |  ...  | (x,y)  |
 |      r |      g |      b |      a |      r |       |      r |
 +--------+--------+--------+--------+--------+-------+--------+---
v}

where:
    {ul
    {- [w] is the image width}
    {- [h] is the image height}
    {- [c] is the number of color channel}}
*)

(** {1:target Bitmap render targets} *)

module type S = sig
  type bitmap

  val target : bitmap -> [ `Other ] Vg.Vgr.target
  (** [target state]. *)
end

(** Functor building an implementation of the bitmap given a {!BitmapType}. *)
module Make (Bitmap : BitmapType) : S with type bitmap = Bitmap.t

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
