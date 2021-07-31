open Bigarray
open Bimage
open Gg
open Vg

let get_pixel pxmp h x y = Array1.get pxmp ((x * h) + y)

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
let save path (pxmp, w, h) =
  let getpx = get_pixel pxmp w in
  let img = Image.create u8 Bimage.rgb w h in
  ignore (Image.for_each (fun x y _px -> color_pixel img x y (getpx x y)) img);
  Bimage_unix.Magick.write path img;
  Printf.printf "PNG file saved here: %s\n" path

let white = (255 * 65536) + (255 * 256) + 255

let () =
  let aspect = 1.0 in
  let size = Size2.v (aspect *. 100.) 100. in
  let view = Box2.v P2.o (Size2.v aspect 1.) in
  let image =
    let line = P.empty |> P.line (P2.v 10. 10.) in
    let black = I.const Color.black in
    let lines = `O { P.o with P.width = 0.01 } in
    I.cut ~area:lines line black
  in

  (* How the renderer should be used. *)
  let res = 300. /. 25.4 in
  let w = int_of_float (res *. Size2.w size) in
  let h = int_of_float (res *. Size2.h size) in
  Printf.printf "w: %d, h: %d\n" w h;

  let raster = Gg.Ba.create Int16 (w * h) in

  (* FIXME: not white. *)
  Bigarray.Array1.fill raster white;

  let target = Vgr_raster.target raster in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn target `Other in
  ignore (Vgr.render r (`Image (size, view, image)));
  ignore (Vgr.render r `End);
  save "testoutput.png" (raster, w, h)
