open Bigarray
open Bimage
open Gg
open Vg

(* To PNG functions (from school project). *)

(** Converts a Graphics.color [c] into its rgb code. *)
let color_to_rgb (rgb : float) =
  (* let rgb = Int32.to_int rgb in *)
  (* Printf.printf "rgb: %dd\n" rgb; *)
  let rgb = Int.of_float rgb in
  let r = (rgb lsr 16) land 0xFF in
  (* Printf.printf "r: %d\n" r; *)
  let g = (rgb lsr 8) land 0xFF in
  (* Printf.printf "g: %d\n" g; *)
  let b = rgb land 0xFF in
  Printf.printf "b: %d\n" b;
  (r, g, b)

let to_255_int_rgb v = Int.of_float (255. *. v)

let rgba_to_int rgb =
  let open Gg.V4 in
  let sa res = Stdlib.( + ) (res lsl 8) in
  let res = x rgb |> to_255_int_rgb in
  Printf.printf "res1: %d\n" res;
  let res = y rgb |> to_255_int_rgb |> sa res in
  Printf.printf "res2: %d\n" res;
  let res = z rgb |> to_255_int_rgb |> sa res in
  Printf.printf "res3: %d\n" res;
  res

(** Colors the pixel at ([x], [y]) of [img] with the color [c] *)
let color_pixel (img : (float, Bimage.f32, Bimage.rgb) Bimage.Image.t) x y c =
  let r, g, b = color_to_rgb c in
  Image.set img x y 0 (Int.to_float (r / 255));
  Image.set img x y 1 (Int.to_float (g / 255));
  Image.set img x y 2 (Int.to_float (b / 255))

(** Save the drawing as a PNG in [path] *)
let save path (ba, w, h) =
  (* NOTE: why I need to use the height instead of the width? *)
  let img = Image.create f32 Bimage.rgb w h in
  ignore
    (Image.for_each (fun x y _px -> color_pixel img x y ba.{(x * h) + y}) img);
  Bimage_unix.Magick.write path img;
  Printf.printf "PNG file saved here: %s\n" path

let white = Color.white

let () =
  let aspect = 1.618 in
  let size = Size2.v (aspect *. 10.) 10. (* mm *) in
  let view = Box2.v P2.o (Size2.v aspect 1.) in
  (* let circle = P.empty |> P.circle (P2.v 0.5 0.5) 0.4 in *)
  (* let image = I.cut circle (I.const (Color.v_srgb 0.314 0.784 0.471)) in *)
  let image = I.const Color.red in

  (* How the renderer should be used. *)
  let res = 300. /. 25.4 in
  let w = int_of_float (res *. Size2.w size) in
  let h = int_of_float (res *. Size2.h size) in
  Printf.printf "w: %d, h: %d\n" w h;

  (* let raster =  in *)
  let buff = Ba.create UInt8 (w * h) in

  (* let raster = *)
  (*   Gg.Raster.v (`D2 size) *)
  (*     (Raster.Sample.format (`Color (Color.p_rgb_l, true)) `Int8) *)
  (*     (`Int8 buff) *)
  (* in *)

  (* FIXME: not white. *)
  let blue = rgba_to_int (Color.v_srgb 0.5 0.5 1.) in
  Printf.printf "blue %d\n" blue;
  Ba.fill buff (Int.to_float blue);
  Printf.printf "dim: %d\n" (Array1.dim buff);
  let target = Vgr_raster.target buff in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn target `Other in
  ignore (Vgr.render r (`Image (size, view, image)));
  ignore (Vgr.render r `End);
  save "testoutput.png" (buff, w, h)
