(*---------------------------------------------------------------------------
   Copyright (c) 2014 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vgr
module Pv = Private

(** Temporary debugging functions. *)
module Debug = struct
  let pp_img i = Printf.printf "Image: %s\n" @@ I.to_string @@ Pv.I.of_data i

  let pp_path p = Printf.printf "Path: %s\n" @@ P.to_string @@ Pv.P.of_data p

  let pp_segs segs =
    Printf.printf "Seg: ";
    List.iter (fun pt -> Printf.printf "(%f, %f); " (P2.x pt) (P2.y pt)) segs;
    Printf.printf "\n"

  let spf = Printf.sprintf

  let log ?(s = "LOG") = Printf.printf "\t[%s] %s\n" s
end

module D = Debug

module type BitmapType = sig
  type t

  val create : int -> int -> t

  val get : t -> float -> float -> Gg.color

  val set : t -> float -> float -> Gg.color -> unit

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
    let x' = int_of_float x and y' = int_of_float y in
    ((x' * h) + y') * stride

  let create w h =
    let ba = Ba.create Ba.Float32 (w * h * stride) in
    Ba.fill ba 1.;
    (ba, w, h)

  let get (b, _, h) x y = Ba.get_v4 b (get_i x y h)

  let set (b, _, h) x y c = Ba.set_v4 b (get_i x y h) c

  let w (_, w, _) = w

  let h (_, _, h) = h
end

module type S = sig
  type bitmap

  val target : bitmap -> [ `Other ] Vg.Vgr.target
end

(* TODO: find a better location. *)
let to_int_coords (x : float) (y : float) : int * int =
  (int_of_float x, int_of_float y)

let to_float_coords (x : int) (y : int) : float * float =
  (float_of_int x, float_of_int y)

(** Extends the [Box2] module by adding a folding function. *)
module Box2 = struct
  include Box2

  (** [fold f acc b] is a classical left folding function on [box2]. *)
  let fold (f : 'a -> int -> int -> 'a) (acc : 'a) (b : box2) : 'a =
    let minx = Box2.minx b |> int_of_float
    and miny = Box2.miny b |> int_of_float
    and maxx = Box2.maxx b |> int_of_float |> ( + ) ~-1
    and maxy = Box2.maxy b |> int_of_float |> ( + ) ~-1 in
    let rec loop acc x y =
      if x = maxx && y = maxy then acc
      else
        let acc = f acc x y in
        if x = maxx then loop acc minx (y + 1) else loop acc (x + 1) y
    in
    loop acc minx miny

  (** [iter f b] is a classical itering function on [box2]. *)
  let iter (f : 'int -> int -> unit) (b : box2) : 'a =
    let minx = Box2.minx b |> int_of_float
    and miny = Box2.miny b |> int_of_float
    and maxx = Box2.maxx b |> int_of_float |> ( + ) ~-1
    and maxy = Box2.maxy b |> int_of_float |> ( + ) ~-1 in
    let rec loop x y =
      if x = maxx && y = maxy then ()
      else (
        f x y;
        if x = maxx then loop minx (y + 1) else loop (x + 1) y)
    in
    loop minx miny
end

(** [Stroker] contains all the algorithm implementations in order to calculates
    coordinates of points of mathematical 2D graphics primitives shuch as lines
    or Bézier curves.

    All point coordinates used by the following functions are assumed to be
    scaled (see {!state.scaling}). ) *)
module Stroker = struct
  (** [bresenham_line x0 y0 x1 y1] adds all the calculated points of the line
      from ([x0], [y0]) to ([x1], [y1]) to [pts].

      The Bresenham's line algorithm is used (see
      https://en.wikipedia.org/wiki/Bresenham's_line_algorithm) *)
  let bresenham_line (x0 : int) (y0 : int) (x1 : int) (y1 : int) : p2 list =
    let dx = abs (x1 - x0)
    and sx = if x0 < x1 then 1 else -1
    and dy = -1 * abs (y1 - y0)
    and sy = if y0 < y1 then 1 else -1 in
    let err = dx + dy in

    let rec loop pts x y err =
      if x = x1 && y = y1 then pts
      else
        let e2 = 2 * err in
        let err = if e2 >= dy then err + dy else err in
        let x = if e2 >= dy then x + sx else x in
        let err = if e2 <= dx then err + dx else err in
        let y = if e2 <= dx then y + sy else y in
        loop (P2.v (float_of_int x) (float_of_int y) :: pts) x y err
    in
    loop [] x0 y0 err

  (** [cubic_bezier ?nb_line p1x p1y c1x c1y c2x c2y p2x p2y] returns all the
      points needed to be connected by a line in order to approach a Bézier
      curve.

      [nb_line] determines in how many lines the curve is approximated.

      Algorithm from here:
      https://rosettacode.org/wiki/Bitmap/B%C3%A9zier_curves/Cubic#C *)
  let cubic_bezier
      ?(nb_line = 20.)
      (p1x : float)
      (p1y : float)
      (c1x : float)
      (c1y : float)
      (c2x : float)
      (c2y : float)
      (p2x : float)
      (p2y : float) : p2 list =
    let rec loop acc t =
      if t > nb_line then acc
      else
        let t' = t /. nb_line in
        let a = (1. -. t') ** 3.
        and b = 3. *. t' *. ((1. -. t') ** 2.)
        and c = 3. *. (t' ** 2.) *. (1. -. t')
        and d = t' ** 3. in
        let x, y =
          ( (a *. p1x) +. (b *. c1x) +. (c *. c2x) +. (d *. p2x),
            (a *. p1y) +. (b *. c1y) +. (c *. c2y) +. (d *. p2y) )
        in
        loop (P2.v x y :: acc) (t +. 1.)
    in
    loop [] 0.
end

(** [Filler_rule] contains all the algorithm implementations to determines
    coordinates of points inside a path.

    All point coordinates used by the following functions are assumed to be
    scaled (see {!state.scaling}). ) *)
module Filler_rule = struct
  (** [even_odd x y pts] is the implementation of the even-odd rule algorithm.

      PERF: very basic algorithm which needs to be seriously improved to be
      really functional. -> using Lwt_list? *)
  let even_odd (x : int) (y : int) (pts : p2 list list list) : bool =
    let open List in
    let is_in_pts x y =
      pts
      |> exists @@ exists
         @@ exists (fun pt ->
                let ptx', pty' = to_int_coords (P2.x pt) (P2.y pt) in
                ptx' = x && pty' = y)
    in
    let is_crossing =
      exists (fun pt ->
          let ptx, pty = to_int_coords (P2.x pt) (P2.y pt) in
          ptx < x && pty = y && not (is_in_pts x y))
    in
    let count =
      fold_left
        (fold_left (fun acc sp -> if is_crossing sp then acc + 1 else acc))
        0 pts
    in
    count mod 2 = 1

  (** [non_zero x y pts] is the implementation of the non-zero rule algorithm. *)
  let non_zero (x : int) (y : int) (pts : p2 list list list) : bool =
    failwith "TODO"
end

module Make (Bitmap : BitmapType) = struct
  module B = Bitmap

  type bitmap = B.t

  (** Represents all points of a sub-path which must be drawn. *)
  type subpath = {
    (* All points that needs to be drawn in the bitmap.

       NOTE: A dimension should be removed to be coherent with the semantics.
       Do we really need to store distinctly segments of sub-paths?
       PERF: To test with 'big' images and compare different data structures
       such as Hashtbl..
    *)
    segs : p2 list list;
    (* Beginning of the sub-path. *)
    start : p2 option;
    (* Tracks the current state of the sub-path: is it closed or not? *)
    closed : bool;
  }

  (** Graphical state of the renderer. *)
  type gstate = {
    mutable g_tr : M3.t;
    (* Current path outline. *)
    mutable g_outline : P.outline;
    (* Current stroking color.  *)
    mutable g_stroke : Gg.color;
    (* Current filling color.  *)
    mutable g_fill : Gg.color;
  }

  (** Commands to perform. *)
  type cmd = Set of gstate | Draw of Pv.Data.image

  (** State of the renderer. *)
  type state = {
    r : Pv.renderer;
    (* Stores the rendered {{!Vg.image}} *)
    bitmap : bitmap;
    (* Current path being built. *)
    size : size2;
    (* Constant used to convert Vg point coordinates into rasterized ones. *)
    scaling : float;
    (* Points of the current path being calculated. *)
    mutable path : subpath list;
    (* Current cursor position. *)
    mutable curr : p2;
    mutable cost : int;
    mutable view : box2;
    (* List of remaining commands to perform. *)
    mutable todo : cmd list;
    (* Graphical state. *)
    mutable gstate : gstate;
  }
  (* Convenient functions. TODO: this could be factorized with the other renderers. *)

  let partial = Pv.partial

  let limit s = Pv.limit s.r

  let warn s w = Vgr.Private.warn s.r w

  let image i = Vgr.Private.I.of_data i

  (** image view rect in current coordinate system. *)
  let view_rect s =
    let tr = M3.inv s.gstate.g_tr in
    Pv.Data.of_path (P.empty |> P.rect (Box2.tr tr s.view))

  (* Convenient functions for coordinate conversions. *)

  let get_curr_int_coords (s : state) : int * int =
    to_int_coords (P2.x s.curr) (P2.y s.curr)

  let get_scaled_coords (s : state) (x : float) (y : float) : float * float =
    (s.scaling *. x, s.scaling *. y)

  let get_int_scaled_coords (s : state) (x : float) (y : float) : int * int =
    to_int_coords (s.scaling *. x) (s.scaling *. y)

  (* Render functions.

     They follow the same design that for other renderers such as [Vgr_cairo]
     or [Vgr_htmlc] in order to stay consistent. *)

  let empty_subpath : subpath = { segs = []; start = None; closed = false }

  let get_current_subpath (s : state) : subpath option = List.nth_opt s.path 0

  (** [move_to s x y] updates the current position to ([x], [y]) and opan a new
      sub-path starting at ([x], [y]). *)
  let move_to (s : state) (x : float) (y : float) : unit =
    s.curr <- P2.v x y;
    s.path <- { empty_subpath with start = Some s.curr } :: s.path

  (** [add_path_points s pts] add [pts] to the current path and if it's empty,
      begins a new one starting at [s.curr]. *)
  let add_path_points (s : state) (pts : p2 list) : unit =
    s.path <-
      (match s.path with
      | [] -> [ { empty_subpath with segs = [ pts ]; start = Some s.curr } ]
      | sp :: tl -> { sp with segs = pts :: sp.segs } :: tl)

  (** [close_path s] Adds a line segment to the current path being built from
      the current point to the beginning of the current sub-path before closing
      it. After this call the current point will be at the joined endpoint of
      the sub-path.

      If there is no current point before the call to [close_path], this
      function will have no effect. *)
  let close_path (s : state) : unit =
    let close curr_sp start =
      let x0, y0 = get_curr_int_coords s
      and x1, y1 = to_int_coords (P2.x start) (P2.y start) in
      let r_line_pts = Stroker.bresenham_line x0 y0 x1 y1 in
      s.curr <- start;
      add_path_points s r_line_pts
    in
    Option.iter
      (fun curr_sp -> Option.iter (close curr_sp) curr_sp.start)
      (get_current_subpath s)

  (** [line_to s x y] adds a line to the path from the current point to position
      ([x], [y]) scaled by [s.scaling]. After this call the current point will
      be ([x], [y]). *)
  let line_to (s : state) (x : float) (y : float) : unit =
    let x0, y0 = get_curr_int_coords s in
    let x1, y1 = get_scaled_coords s x y in
    let x1', y1' = to_int_coords x1 y1 in
    Stroker.bresenham_line x0 y0 x1' y1' |> add_path_points s;
    s.curr <- P2.v x1 y1

  let bezier_curve_to
      (s : state)
      (cx : float)
      (cy : float)
      (cx' : float)
      (cy' : float)
      (ptx : float)
      (pty : float) : unit =
    let p1x, p1y = (P2.x s.curr, P2.y s.curr)
    and cx, cy = get_scaled_coords s cx cy
    and cx', cy' = get_scaled_coords s cx' cy'
    and p2x, p2y = get_scaled_coords s ptx pty in
    let to_line =
      Stroker.cubic_bezier p1x p1y cx cy cx' cy' p2x p2y |> List.rev
    in
    let r_line_pts = ref [] in
    ignore
      (List.fold_left
         (fun prev pt ->
           if prev = pt then pt
           else
             let x0, y0 = to_int_coords (P2.x prev) (P2.y prev)
             and x1, y1 = to_int_coords (P2.x pt) (P2.y pt) in
             r_line_pts := Stroker.bresenham_line x0 y0 x1 y1 @ !r_line_pts;
             pt)
         (List.hd to_line) to_line);
    s.curr <- P2.v p2x p2y;
    add_path_points s !r_line_pts

  (** [set_path s p] calculates points to draw according to a given [p]. *)
  let set_path (s : state) (p : Pv.Data.path) : unit =
    let open P2 in
    List.rev p
    |> List.iter (function
         | `Sub pt ->
             let x, y = get_scaled_coords s (x pt) (y pt) in
             move_to s x y
         | `Line pt -> line_to s (x pt) (y pt)
         | `Qcurve (c, pt) ->
             failwith "quadratic_curve_to (x c) (y c) (x pt) (y pt))"
         | `Ccurve (c, c', pt) ->
             bezier_curve_to s (x c) (y c) (x c') (y c') (x pt) (y pt)
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
         | `Close -> close_path s)

  let get_primitive : Pv.Data.primitive -> color = function
    | Pv.Data.Const c -> c
    | Axial _ | Radial _ | Raster _ -> failwith "TODO"

  (** [set_stroke s] updates [s.gstate] according to a given Vg primitive. *)
  let set_stroke (s : state) (p : Pv.Data.primitive) : unit =
    s.gstate.g_stroke <- get_primitive p

  (** [set_fill s] updates [s.gstate] according to a given Vg primitive. *)
  let set_fill (s : state) (p : Pv.Data.primitive) : unit =
    s.gstate.g_fill <- get_primitive p

  (** [is_in_view s x y] for now, verifies that (x, y) are valid coordinates for
      the [s.bitmap]. TODO: need to find out how to manage the [view] and the
      [size].

      PERF: This test should be done before adding points the [s.path] instead
      of checking before drawing. => less memory usage. *)
  let is_in_view (s : state) (x : float) (y : float) : bool =
    let w, h = to_float_coords (B.w s.bitmap) (B.h s.bitmap) in
    x >= 0. && x < w && y >= 0. && y < h

  (** [stroke s] fills the [s.bitmap] according to the current [s.gstate]. *)
  let r_stroke (s : state) : unit =
    let draw_point pt =
      let x = P2.x pt and y = P2.y pt and c = s.gstate.g_stroke in
      if Color.void <> c && is_in_view s x y then B.set s.bitmap x y c
    in
    let draw_subpath sp = List.iter (List.iter draw_point) sp.segs in
    List.iter draw_subpath s.path

  (** [r_fill r s] fills all the points inside [s.path] according to the given
      filling rule [r]. *)
  let r_fill (r : [< `Aeo | `Anz ]) (s : state) : unit =
    let c = s.gstate.g_fill in
    if Color.void <> c then
      let view =
        Box2.v P2.o
          (P2.v (float_of_int (B.w s.bitmap)) (float_of_int (B.h s.bitmap)))
      in
      let pts = List.fold_left (fun acc s -> s.segs :: acc) [] s.path in
      let is_inside =
        match r with
        | `Anz -> Filler_rule.non_zero
        | `Aeo -> Filler_rule.even_odd
      in
      Box2.iter
        (fun x y ->
          if is_inside x y pts then
            let x, y = to_float_coords x y in
            B.set s.bitmap x y c)
        view

  (** [r_cut s a] renders a cut image. *)
  let rec r_cut (s : state) (a : P.area) : Pv.Data.image -> unit = function
    | Primitive (Raster _) -> assert false
    | Primitive p -> (
        match a with
        | `O o ->
            s.gstate.g_outline <- o;
            set_stroke s p;
            r_stroke s
        | (`Anz | `Aeo) as a ->
            set_fill s p;
            r_fill a s)
    | _ -> failwith "TODO"

  (** [r_image s k r] renders a Vg image. *)
  let rec r_image (s : state) (k : Pv.k) (r : Pv.renderer) : [ `Ok | `Partial ]
      =
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

  (** [create_state b s v r] creates a initial state. *)
  let create_state (b : bitmap) (s : size2) (v : box2) (r : Pv.renderer) : state
      =
    {
      r;
      view = v;
      bitmap = b;
      (* NOTE: need to find out why this needs to be the height instead of the
         minimum between the height and the width or just the width.
         + This probably should be replaced by a transformation matrix. *)
      scaling = B.h b |> float_of_int;
      size = s;
      path = [];
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

  let render_target (bitmap : bitmap) (_ : Pv.renderer) (_ : [< dst ]) :
      bool * Pv.render_fun =
    let render v k r =
      match v with
      | `End -> k r
      | `Image (size, view, i) ->
          D.pp_img i;
          let s = create_state bitmap size view r in
          s.todo <- [ Draw i ];
          r_image s k r
    in
    (false, render)

  let target bitmap = Pv.create_target (render_target bitmap)
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
