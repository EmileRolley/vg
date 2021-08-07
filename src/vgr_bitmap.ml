(*---------------------------------------------------------------------------
   Copyright (c) 2014 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vgr
module Pv = Private

module type BitmapType = sig
  type t

  val create : int -> int -> t

  val get : t -> float -> float -> Gg.Color.t

  val set : t -> float -> float -> Gg.Color.t -> unit

  val w : t -> int

  val h : t -> int
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

  let create w h =
    let ba = Ba.create Ba.Float32 (w * h * stride) in
    Ba.fill ba 1.;
    (ba, w, h)

  let get (b, _, h) x y =
    let v_rgba = get_i x y h |> Ba.get_v4 b in
    V4.(Color.v (x v_rgba) (y v_rgba) (z v_rgba) (w v_rgba)) |> Color.of_srgb

  let set (b, _, h) x y c = Ba.set_v4 b (get_i x y h) c

  let w (_, w, _) = w

  let h (_, _, h) = h
end

module type S = sig
  type bitmap

  val target : bitmap -> float -> [ `Other ] Vg.Vgr.target
end

module Make (Bitmap : BitmapType) = struct
  module B = Bitmap

  type bitmap = B.t

  type gstate = {
    mutable g_tr : M3.t;
    mutable g_outline : P.outline;
    mutable g_stroke : Color.t;
    mutable g_fill : Color.t;
  }

  type cmd = Set of gstate | Draw of Pv.Data.image

  (* TODO: fill the doc. *)
  type state = {
    r : Pv.renderer;
    (* Stores the rendered {{!Vg.image}} *)
    bitmap : bitmap;
    (* Current path being built. *)
    size : size2;
    scaling : float;
    res : float;
    mutable path : p2 list list;
    (* Current cursor position. *)
    mutable curr : p2;
    mutable cost : int;
    mutable view : box2;
    mutable todo : cmd list;
    mutable gstate : gstate;
  }

  (* Convenient functions. TODO: this could be factorized with the other renderers. *)

  let partial = Pv.partial

  let limit s = Pv.limit s.r

  let warn s w = Vgr.Private.warn s.r w

  let image i = Vgr.Private.I.of_data i

  (* Temporary debug functions. *)

  let pp_img i = Printf.printf "Image: %s\n" @@ I.to_string @@ Pv.I.of_data i

  let pp_path p = Printf.printf "Path: %s\n" @@ P.to_string @@ Pv.P.of_data p

  let pp_segs segs =
    Printf.printf "Seg: ";
    List.iter (fun pt -> Printf.printf "(%f, %f); " (P2.x pt) (P2.y pt)) segs;
    Printf.printf "\n"

  (** image view rect in current coordinate system. *)
  let view_rect s =
    let tr = M3.inv s.gstate.g_tr in
    Pv.Data.of_path (P.empty |> P.rect (Box2.tr tr s.view))

  (* Render functions.

     They follow the same design that for other renderers such as [Vgr_cairo]
     or [Vgr_htmlc] in order to stay consistent. *)

  (* let flip_y (y : float) (h : int) : float = float_of_int h -. y *)

  let move_to (s : state) (x : float) (y : float) : unit = s.curr <- P2.v x y

  (* FIXME: lineplotting + yflipping. *)
  (* NOTE: yflipping is not the priority for now I think. *)
  let line_to (s : state) (x : float) (y : float) : unit =
    let plot_line segs x0 y0 x1 y1 =
      (* Algorithm from: https://en.wikipedia.org/wiki/Bresenham's_line_algorithm *)
      let open Int in
      let dx = abs (x1 - x0) in
      let sx = if x0 < x1 then 1 else -1 in
      let dy = -1 * abs (y1 - y0) in
      let sy = if y0 < y1 then 1 else -1 in
      let err = dx + dy in

      let rec loop segs x y err =
        Printf.printf "plot: (%d, %d)\n" x y;

        if x = x1 && y = y1 then segs
        else
          let e2 = 2 * err in
          let err = if e2 >= dy then err + dy else err in
          let x = if e2 >= dy then x + sx else x in
          let err = if e2 <= dx then err + dx else err in
          let y = if e2 <= dx then y + sy else y in
          loop (P2.v (float_of_int x) (float_of_int y) :: segs) x y err
      in
      loop segs x0 y0 err
    in
    (* NOTE: find a way to reduce int <-> float convesions. *)
    let x0 = P2.x s.curr |> int_of_float in
    let y0 = P2.y s.curr |> int_of_float in
    let x1 = int_of_float (x *. s.scaling) in
    let y1 = int_of_float (y *. s.scaling) in
    s.path <- plot_line [ s.curr ] x0 y0 x1 y1 :: s.path

  (* TODO: need to find out how to manage the [view] and the [size]. *)
  let is_in_view (_x : float) (_y : float) (_view : box2) : bool =
    (* Box2.(x >= minx view && x <= maxx view && y >= miny view && y <= maxy view) *)
    true

  (** [stroke s] fills the [s.bitmap] according to the current [s.gstate]. *)
  let stroke (s : state) : unit =
    let draw (pt : p2) : unit =
      let x = P2.x pt in
      let y = P2.y pt in
      let c = s.gstate.g_stroke in
      if Color.void <> c && is_in_view x y s.view then (
        Printf.printf "stroke: (%f, %f)\n" x y;
        B.set s.bitmap x y c)
    in
    Printf.printf "view (w: %f, h: %f)\n" (Box2.w s.view) (Box2.h s.view);
    List.iter
      (fun s ->
        pp_segs s;
        Printf.printf "\n")
      s.path;
    List.iter (List.iter draw) s.path

  let set_path (s : state) (p : Pv.Data.path) : unit =
    let open P2 in
    s.path <- [ [] ];
    let add_segment : Pv.Data.segment -> unit = function
      | `Sub pt -> move_to s (x pt) (y pt)
      | `Line pt -> line_to s (x pt) (y pt)
      | `Qcurve (c, pt) ->
          failwith "quadratic_curve_to (x c) (y c) (x pt) (y pt))"
      | `Ccurve (c, c', pt) ->
          failwith "bezier_curve_to (x c) (y c) (x c') (y c') (x pt) (y pt)"
      | `Earc (large, cw, r, a, pt) ->
          (*( match Vgr.Private.P.earc_params last large cw r a pt with
                  | None -> line_to (x pt) (y pt)
                  | Some (c, m, a, a') ->
                      (* This part needs to be developed. *)
                      let s = save s in
                      let c = V2.ltr (M2.inv m) c in
                      M2.(transform s (e00 m) (e10 m) (e01 m) (e11 m) (0.) (0.)));
                      arc s (x c) (y c) ~r:1.0 ~a1:a ~a2:a'
                      |> restore
                  )*)
          ()
      | `Close -> failwith "close_path s"
    in
    pp_path p;
    List.iter add_segment p

  let set_stroke s = function
    | Pv.Data.Const c -> s.gstate.g_stroke <- c
    | Axial _ | Radial _ | Raster _ -> failwith "TODO"

  let rec r_cut (s : state) (a : P.area) : Pv.Data.image -> unit = function
    | Primitive (Raster _) -> assert false
    | Primitive p -> (
        match a with
        | `O o ->
            s.gstate.g_outline <- o;
            set_stroke s p;
            stroke s
        | `Aeo | `Anz -> failwith "TODO")
    | _ -> failwith "TODO"

  let rec r_image s k r =
    if s.cost > limit s then (
      s.cost <- 0;
      partial (r_image s k) r)
    else
      match s.todo with
      | [] -> k r
      | Set _gs :: _todo -> failwith "TODO"
      | Draw i :: todo -> (
          s.cost <- s.cost + 1;
          match i with
          | Primitive _ as i ->
              (* Uncut primitive, just cut to view. *)
              let p = view_rect s in
              s.todo <- Draw (Cut (`Anz, p, i)) :: todo;
              r_image s k r
          | Cut (a, p, i) ->
              s.todo <- todo;
              set_path s p;
              r_cut s a i;
              r_image s k r
          | Cut_glyphs (a, _run, i) ->
              s.todo <- todo;
              warn s (`Unsupported_glyph_cut (a, image i));
              r_image s k r
          | Blend (_, _, i, i') ->
              (* NOTE: seems like this operation is avoided. *)
              s.todo <- Draw i' :: Draw i :: todo;
              r_image s k r
          | Tr (_tr, i) ->
              s.todo <- todo;
              warn s (`Other "TODO: support transformations.");
              r_image s k r)

  let create_state
      (b : bitmap) (res : float) (s : size2) (view : box2) (r : Pv.renderer) :
      state =
    {
      r;
      res;
      view;
      bitmap = b;
      (* NOTE: need to find out why this needs to be the height instead of the
         minimum between the height and the width or just the width.
         + This probably will be replace by a transformation matrix. *)
      scaling = B.h b |> float_of_int;
      size = s;
      path = [ [] ];
      curr = P2.o;
      cost = 0;
      todo = [];
      gstate =
        {
          g_tr = M3.id;
          g_outline = P.o;
          g_stroke = Color.void;
          g_fill = Color.void;
        };
    }

  let render_target
      (bitmap : bitmap) (res : float) (_ : Pv.renderer) (_ : [< dst ]) :
      bool * Pv.render_fun =
    let render v k r =
      match v with
      | `End ->
          Printf.printf "The rendering is over.\n";
          k r
      | `Image (size, view, i) ->
          Printf.printf "Start to render:\n";
          pp_img i;
          let s = create_state bitmap res size view r in
          s.todo <- [ Draw i ];
          r_image s k r
    in
    (false, render)

  (* NOTE: Maybe needs to get the res too. *)
  let target bitmap res = Pv.create_target (render_target bitmap res)
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
