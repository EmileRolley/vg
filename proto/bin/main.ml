open Bigarray
open Bimage
open Gg
open Vg

let dimx = 100

let dimy = 100

(* Bigarray representation. *)

module Pixmap = struct
  let get_pixel (pxmp, dimx, _) x y = Array1.get pxmp ((x * dimx) + y)
end

(* To PNG functions (from school project). *)

(** Converts a Graphics.color [c] into its rgb code. *)
let color_to_rgb c =
  let r = c / 65536 and g = c / 256 mod 256 and b = c mod 256 in
  (r, g, b)

(** Colors the pixel at ([x], [y]) of [img] with the color [c] *)
let color_pixel img x y c =
  let r, g, b = color_to_rgb c in
  Image.set img x y 0 r;
  Image.set img x y 1 g;
  Image.set img x y 2 b

(** Save the drawing as a PNG in [path] *)
let save path pxmp =
  let getpx = Pixmap.get_pixel pxmp in
  let img = Image.create u8 Bimage.rgb dimx dimy in
  ignore (Image.for_each (fun x y _px -> color_pixel img x y (getpx x y)) img);
  Bimage_unix.Magick.write path img;
  Printf.printf "PNG file saved here: %s\n" path

let red = 255 * 65536

let white = (255 * 65536) + (255 * 256) + 255

(* 2. Render *)

let plot (pxmp, dimx, _) x y = Array1.set pxmp ((x * dimx) + y) red

(** Algorithm from:

        https://en.wikipedia.org/wiki/Bresenham's_line_algorithm

*)
let plot_line pxmp x0 y0 x1 y1 =
  let open Int in
  let dx = abs (x1 - x0) in
  let sx = if x0 < x1 then 1 else -1 in
  let dy = -1 * abs (y1 - y0) in
  let sy = if y0 < y1 then 1 else -1 in
  let err = dx + dy in

  let rec loop x y err =
    plot pxmp x y;

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

let rec render pxmp vg_img =
  let open Vgr.Private.Data in
  let render_segment : segment -> unit = function
    | `Line v2 ->
        Printf.printf "  ->`Line\n";
        let x = int_of_float (V2.x v2) in
        let y = int_of_float (V2.y v2) in
        plot_line pxmp 0 0 x y
    | `Sub _ -> Printf.printf "  ->`Sub\n"
    | `Qcurve _ -> Printf.printf "  ->`Qcurve\n"
    | `Ccurve _ -> Printf.printf "  ->`Ccurve\n"
    | `Earc _ -> Printf.printf "  ->`Earc\n"
    | `Close -> Printf.printf "  ->`Close\n"
  in
  (* NOTE: I think the continuation passing style (or at least using states) is needed
     to go further. *)
  match vg_img with
  | Primitive _ -> Printf.printf "Primitive\n"
  | Cut (_, p, i) ->
      Printf.printf "Cut\n";
      List.iter ~f:render_segment p;
      render pxmp i
  | Cut_glyphs _ -> Printf.printf "Cut_glyphs\n"
  | Blend _ -> Printf.printf "Blend\n"
  | Tr _ -> Printf.printf "Tr\n"

let () =
  let vg_img =
    let line = P.empty |> P.line (P2.v 5. 25.) |> P.line (P2.v 25. 50.) in
    (* let circle = P.empty |> P.circle (P2.v 0.5 0.5) 0.4 in *)
    (* let area = `O { P.o with P.width = 0.04 } in *)
    let black = I.const Color.black in
    I.cut line black
  in

  let pxmp = Array1.create int c_layout (dimx * dimy) in
  Array1.fill pxmp white;
  (* plot_line (pxmp, dimx, dimy) 10 10 90 90; *)
  render (pxmp, dimx, dimy) (Vgr.Private.Data.of_image vg_img);
  save "testoutput.png" (pxmp, dimx, dimy)
