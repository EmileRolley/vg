(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vgr.Private.Data

(* Renderer *)

type gstate = 
  { g_alpha : float; 
    g_blender : I.blender; 
    g_outline : P.outline; } 
    
type svg_prim = Gradient of int | Color of string * string

type cmd = Pop of unit | Draw of Vgr.Private.Data.image
type state = 
  { r : Vgr.Private.renderer;                    (* corresponding renderer. *)
    buf : Buffer.t;                                   (* formatting buffer. *)
    mutable cost : int;                          (* cost counter for limit. *)
    mutable view : Gg.box2;           (* current renderable view rectangle. *)
    mutable todo : cmd list;                        (* commands to perform. *)
    mutable id : int;                                     (* uid generator. *)
    prims :                                           (* cached primitives. *)
      (Vgr.Private.Data.primitive, svg_prim) Hashtbl.t; 
    paths : (path, int) Hashtbl.t;                         (* cached paths. *)
    mutable s_alpha : float;                       (* current global alpha. *)
    mutable s_blender : I.blender;                (* current blending mode. *)
    mutable s_outline : P.outline; }       (* current outline stroke state. *)
 
let partial = Vgr.Private.partial
let limit s = Vgr.Private.limit s.r
let warn s w = Vgr.Private.warn s.r w
let image i = Vgr.Private.image i
let pop_gstate s = Pop ()
let set_gstate s gs = () 
let new_id s = s.id <- s.id + 1; s.id

let cap_str = function 
| `Butt -> "butt" | `Round -> "round" | `Square -> "square"

let join_str = function 
| `Miter -> "miter" | `Bevel -> "bevel" | `Round -> "round"

let area_str = function 
| `Aeo -> "evenodd" | `Anz -> "nonzero"

let w_str str k r = Vgr.Private.writes str 0 (String.length str) k r
let w_buf s k r = 
  let clear k r = Buffer.clear s.buf; k r in
  Vgr.Private.writebuf s.buf 0 (Buffer.length s.buf) (clear k) r

let badd_fmt s fmt = Printf.bprintf s.buf fmt
let badd_str s str = Buffer.add_string s.buf str 
let badd_esc_str s str = 
  let len = String.length str in
  let start = ref 0 in 
  let last = ref 0 in 
  let escape e = 
    Buffer.add_substring s.buf str !start (!last - !start);
    Buffer.add_string s.buf e; 
    incr last; 
    start := !last
  in
  while (!last < len) do match String.get str !last with 
  | '<' -> escape "&lt;"         (* Escape markup delimiters. *)
  | '>' -> escape "&gt;"
  | '&' -> escape "&amp;"
  (* | '\'' -> escape "&apos;" *) (* Not needed we use \x22 for attributes. *)
  | '\x22' -> escape "&quot;"
  | _ -> incr last
  done;
  Buffer.add_substring s.buf str !start (!last - !start)
      
let badd_title s = function
| None -> () 
| Some t -> badd_str s "<title>"; badd_esc_str s t; badd_str s "</title>"

let badd_descr s = function
| None -> () 
| Some d -> badd_str s "<descr>"; badd_esc_str s d; badd_str s "</descr>"

let badd_svg s xml_decl size =
  badd_fmt s
     "%s\
     <svg xmlns=\"http://www.w3.org/2000/svg\" \
        xmlns:l=\"http://www.w3.org/1999/xlink\" \
        version=\"1.1\" \
        width=\"%gmm\" \
        height=\"%gmm\" \
        viewBox=\"0 0 %g %g\" \
        color-profile=\"auto\" \
        color-interpolation=\"linearRGB\" \
        color-interpolation-filters=\"linearRGB\" \
    >"
    (if xml_decl then "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" else "") 
    (Size2.w size) (Size2.h size) (Size2.w size) (Size2.h size)

let badd_init s size view = 
  let sx = Size2.w size /. Box2.w view in 
  let sy = Size2.h size /. Box2.h view in 
  let dx = -. Box2.ox view *. sx in 
  let dy = Size2.h size +. Box2.oy view *. sy in
   (* stroke-width is 1. initially, so is P.o *)
   (* stroke-linecap is butt initially, so is P.o *) 
   (* stroke-linejoin is miter initially, so is P.o *)
   (* stroke-dasharray is none initially, so is P.o *)
   (* fill is black initially, we set it to none *)
  badd_fmt s "<g fill=\"none\" stroke-miterlimit=\"%g\" \
                 transform=\"matrix(%g %g %g %g %g %g)\">"
    (Vgr.Private.P.miter_limit P.o)
    sx 0. 0. (-. sy) dx dy (* map view rect -> viewport *)

let badd_transform s = function 
| Move v -> badd_fmt s "translate(%g %g)" (V2.x v) (V2.y v)
| Rot a -> badd_fmt s "rotate(%g)" (Float.deg_of_rad a)
| Scale sv -> badd_fmt s "scale(%g %g)" (V2.x sv) (V2.y sv)
| Matrix m -> 
    badd_fmt s "transform(%g %g %g %g %g %g)"
      (M3.e00 m) (M3.e10 m) (M3.e01 m) (M3.e11 m) (M3.e02 m) (M3.e12 m)

let w_path s p k r = try k (Hashtbl.find s.paths p) r with
| Not_found -> 
    let id = new_id s in
    let rec w_data p k r = match p with
    | [] -> w_str "\"/></defs>" (k id) r 
    | seg :: p -> 
        match seg with 
        | `Sub pt -> 
            badd_fmt s "M%g %g" (V2.x pt) (V2.y pt); 
            w_buf s (w_data p k) r
        | `Line pt -> 
            badd_fmt s "L%g %g" (V2.x pt) (V2.y pt);
            w_buf s (w_data p k) r
        | `Qcurve (c, pt) -> 
            badd_fmt s "Q%g %g %g %g" (V2.x c) (V2.y c) (V2.x pt) (V2.y pt); 
            w_buf s (w_data p k) r               
        | `Ccurve (c, c', pt) ->
            badd_fmt s "C%g %g %g %g %g %g" 
              (V2.x c) (V2.y c) (V2.x c') (V2.y c') (V2.x pt) (V2.y pt); 
            w_buf s (w_data p k) r
        | `Earc (large, cw, a, radii, pt) -> 
            let large = if large then 1 else 0 in
            let sweep = if cw then 0 else 1 in
            badd_fmt s "A %g %g %g %d %d %g %g"  
              (V2.x radii) (V2.y radii) (Float.deg_of_rad a) large sweep 
              (V2.x pt) (V2.y pt);
            w_buf s (w_data p k) r
        | `Close -> 
            w_str "Z" (w_data p k) r
    in
    Hashtbl.add s.paths p id;
    badd_fmt s "<defs><path id=\"i%d\" d=\"" id; 
    w_buf s (w_data (List.rev p) k) r

let badd_rgb_color s c = 
  let srgba = Color.to_srgba c in
  let r = Float.int_of_round (Color.r srgba *. 255.) in 
  let g = Float.int_of_round (Color.g srgba *. 255.) in 
  let b = Float.int_of_round (Color.b srgba *. 255.) in
  badd_fmt s "#%02X%02X%02X" r g b

let badd_svg_prim s op = function
| Color (c, "") -> badd_fmt s " %s=\"%s\"/>" op c
| Color (c, a) -> badd_fmt s " %s=\"%s\" %s-opacity=\"%s\"/>" op c op a
| Gradient id -> badd_fmt s " %s=\"url(#i%d)\"/>" op id

let badd_dashes s = function
| None -> () 
| Some (offset, dashes) -> 
    let rec array = function
    | [] -> badd_fmt s "\""
    | d :: ds -> 
        if ds = [] then badd_fmt s "%g\"" d else (badd_fmt s "%g," d; array ds)
    in
    if offset <> 0. then badd_fmt s " stroke-dashoffset=\"%g\"" offset;
    badd_fmt s " stroke-dasharray=\""; 
    array dashes

let badd_stop s (t, c) =
  badd_fmt s "<stop offset=\"%g\" stop-color=\"" t;
  badd_rgb_color s c; 
  badd_fmt s "\"/>"

let w_primitive s p k r = try k (Hashtbl.find s.prims p) r with 
| Not_found ->
    let create = function 
    | Const c -> 
        let get () = let c = Buffer.contents s.buf in Buffer.clear s.buf; c in
        let a = Color.a c in
        let cstr = (badd_rgb_color s c; get ()) in
        let astr = if a = 1.0 then "" else (badd_fmt s "%g" a; get ()) in 
        Color (cstr, astr)
    | Axial (stops, p1, p2) ->
        let id = new_id s in
        badd_fmt s "<defs><linearGradient gradientUnits=\"userSpaceOnUse\" \
                    id=\"i%d\" x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\">" 
          id (V2.x p1) (V2.y p1) (V2.x p2) (V2.y p2); 
        List.iter (badd_stop s) stops; 
        badd_fmt s "</linearGradient></defs>";
        Gradient id
    | Radial (stops, f, c, r) ->
        let id = new_id s in 
        badd_fmt s "<defs><radialGradient gradientUnits=\"userSpaceOnUse\" \
                    id=\"i%d\" fx=\"%g\" fy=\"%g\" cx=\"%g\" cy=\"%g\" \
                    r=\"%g\">"
          id (V2.x f) (V2.y f) (V2.x c) (V2.y c) r;
        List.iter (badd_stop s) stops; 
        badd_fmt s "</radialGradient></defs>";
        Gradient id
    | Raster _ -> assert false 
    in
    let svg_prim = create p in 
    Hashtbl.add s.prims p svg_prim; w_buf s (k svg_prim) r

let w_primitive_cut s a path_id k svg_prim r = match a with 
| `O o ->
    let w = o.P.width in 
    let c = o.P.cap in 
    let j = o.P.join in
    let ma = o.P.miter_angle in
    badd_fmt s "<use l:href=\"#i%d\"" path_id;
    if w <> P.o.P.width then badd_fmt s " stroke-width=\"%g\"" w; 
    if c <> P.o.P.cap then badd_fmt s " stroke-linecap=\"%s\"" (cap_str c); 
    if j <> P.o.P.join then badd_fmt s " stroke-linejoin=\"%s\"" (join_str j);
    if ma <> P.o.P.miter_angle then 
      badd_fmt s " stroke-miterlimit=\"%g\"" (Vgr.Private.P.miter_limit o);
    badd_dashes s o.P.dashes;
    badd_svg_prim s "stroke" svg_prim;
    w_buf s k r
| `Anz | `Aeo ->
    let rule = if a = `Anz then "" else " fill-rule=\"evenodd\"" in 
    badd_fmt s "<use l:href=\"#i%d\"%s" path_id rule; 
    badd_svg_prim s "fill" svg_prim;
    w_buf s k r

let rec w_cut s a i k path_id r = match i with 
| Primitive (Raster _) -> 
    begin match a with 
    | `O _ -> warn s (`Unsupported_cut (a, image i)); k r
    | `Aeo | `Anz -> 
        warn s (`Other "TODO raster unimplemented"); 
        k r 
    end
| Primitive p -> w_primitive s p (w_primitive_cut s a path_id k) r
| Tr (tr, i) -> (* TODO *) k r
| Blend _ | Cut _ as i ->
    let astr = match a with 
    | `O _ -> warn s (`Unsupported_cut (a, image i)); area_str `Anz 
    | a -> area_str `Anz 
    in
    s.todo <- (Draw i) :: (pop_gstate s) :: s.todo;
    badd_fmt s "<g clip-path=\"url(#i%d)\" clip-rule=\"%s\">" path_id astr;
    w_buf s k r
| Meta (_, i) -> w_cut s a i k path_id r

let rec w_transforms s tr i k r =     (* collapses nested Tr in single <g>. *)
  if s.cost > limit s then (s.cost <- 0; partial (w_transforms s tr i k) r) else
  begin
    s.cost <- s.cost + 1;
    match i with
    | Tr (tr', i') -> badd_transform s tr; w_buf s (w_transforms s tr' i' k) r
    | Meta (_, i) -> w_transforms s tr i k r
    | i ->
        badd_transform s tr;
        badd_str s "\">"; 
        s.todo <- (Draw i) :: pop_gstate s :: s.todo;
        w_buf s k r
  end

let rec w_image s k r =
  if s.cost > limit s then (s.cost <- 0; partial (w_image s k) r) else
  match s.todo with 
  | [] -> Hashtbl.reset s.prims; Hashtbl.reset s.paths; k r
  | Pop gs :: todo -> 
      set_gstate s gs; 
      s.todo <- todo; 
      w_str "</g>" (w_image s k) r
  | (Draw i) :: todo -> 
      s.cost <- s.cost + 1; 
      match i with 
      | Primitive _ -> 
          (* Uncut primitive, just cut to view. Need CTM *) 
          warn s (`Other "TODO, uncut primitive not implemented");
          s.todo <- todo;
          w_image s k r
      | Cut (a, p, i) -> 
          s.todo <- todo;
          w_path s p (w_cut s a i (w_image s k)) r
      | Blend (blender, alpha, i, i') -> 
          (* TODO blender and alpha *) 
          s.todo <- (Draw i') :: (Draw i) :: todo; 
          if blender = `Over && alpha = None then w_image s k r else
          begin
            warn s (`Other "TODO, blend mode and group opacity");
            w_image s k r
          end
      | Tr (tr, i) ->
          s.todo <- todo;
          badd_str s "<g transform=\""; 
          w_transforms s tr i (w_image s k) r
      | Meta (m, i) -> 
          s.todo <- (Draw i) :: todo; 
          w_image s k r

let render xml_decl s v k r = match v with 
| `End -> w_str "</g></svg>" (Vgr.Private.flush k) r
| `Image (size, view, i) -> 
    let m = Vgr.Private.meta r in
    badd_svg s xml_decl size;
    badd_title s (Vgm.find m Vgm.title); 
    badd_descr s (Vgm.find m Vgm.description);
    badd_init s size view;
    s.todo <- [Draw i];
    w_buf s (w_image s k) r

let target ?(xml_decl = true) () = 
  let target r _ = 
    false, 
    render xml_decl { r;
                      buf = Buffer.create 2048;
                      cost = 0;
                      view = Box2.empty; 
                      todo = [];
                      id = 0; 
                      prims = Hashtbl.create 241; 
                      paths = Hashtbl.create 241;
                      s_alpha = 1.; 
                      s_blender = `Over; 
                      s_outline = P.o; }
  in
  Vgr.Private.create_target target
  
(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
