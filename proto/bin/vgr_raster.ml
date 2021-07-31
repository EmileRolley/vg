open Vg
open Vgr
open Gg
open Bigarray

let red = 255 * 65536

let w = ref 0

let plot raster color = Array1.set raster color

let plot_red raster = plot raster red

(** Algorithm from:

        https://en.wikipedia.org/wiki/Bresenham's_line_algorithm

*)
let plot_line raster x0 y0 x1 y1 =
  let open Int in
  let dx = abs (x1 - x0) in
  let sx = if x0 < x1 then 1 else -1 in
  let dy = -1 * abs (y1 - y0) in
  let sy = if y0 < y1 then 1 else -1 in
  let err = dx + dy in

  let rec loop x y err =
    Printf.printf "(%d, %d)\n" x y;
    (* FIXME: index out of bounds. *)
    plot_red raster ((x * !w) + y);

    if x = x1 && y = y1 then ()
    else
      let e2 = 2 * err in
      let err = if e2 >= dy then err + dy else err in
      let x = if e2 >= dy then x + sx else x in
      let err = if e2 <= dx then err + dx else err in
      let y = if e2 <= dx then y + sy else y in
      loop x y err
  in
  loop x0 y0 err

let rec render_img raster vg_img =
  let open Vgr.Private.Data in
  let open Gg in
  let render_segment : segment -> unit = function
    | `Line v2 ->
        let x0 = int_of_float (V2.x P2.o) in
        let x = int_of_float (V2.x v2) in
        let y = int_of_float (V2.y v2) in
        Printf.printf "  ->`Line (%d, %d)\n" x y;
        plot_line raster x0 x0 x y
    | `Sub _ -> Printf.printf "  ->`Sub\n"
    | `Qcurve _ -> Printf.printf "  ->`Qcurve\n"
    | `Ccurve _ -> Printf.printf "  ->`Ccurve\n"
    | `Earc _ -> Printf.printf "  ->`Earc\n"
    | `Close -> Printf.printf "  ->`Close\n"
  in
  (* NOTE: I think the continuation passing style (or at least using states) is needed
     to go further. *)
  Printf.printf "Image: %s\n" @@ I.to_string @@ Private.I.of_data vg_img;
  match vg_img with
  | Primitive _ ->
      Printf.printf "Primitive\n";
      `Ok
  | Cut (`O outline, p, i) ->
      Printf.printf "Cut\n";
      (* List.iter render_segment p; *)
      (* render_img raster i *)
      `Ok
  | Cut _ ->
      Printf.printf "Cut without outline\n";
      `Partial
  | Cut_glyphs _ ->
      Printf.printf "Cut_glyphs\n";
      `Partial
  | Blend _ ->
      Printf.printf "Blend\n";
      `Partial
  | Tr _ ->
      Printf.printf "Tr\n";
      `Partial

let render_fun _r raster = function
  | `End -> fun k -> k
  | `Image (s, _v, i) ->
      w := int_of_float (Gg.V2.x s);
      ignore (render_img raster i);
      fun k -> k

let render_target raster r _dst = (false, render_fun r raster)

let target raster : dst target =
  let target = render_target raster in
  Private.create_target target
