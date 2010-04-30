(** Ml_plot_gtk - Holds all of the gtk display functionality for easy
    removal - Jordan*)

(* Need to set up gtk for use *)
ignore (GtkMain.Main.init())

let init_size = 400 (* default window size *)

let draw_plot_to_gtk_area plot area =
  (** [draw_plot plot area] draws the plot to a GTK drawing area. *)
  let ctx = Cairo_lablgtk.create area#misc#window in
  let { Gtk.width = width ; Gtk.height = height } = area#misc#allocation in
  let sizef = float (min width height) in
    Drawing.fill_rectangle ctx ~color:Drawing.white
      (Geometry.rectangle 0. (float width) 0. (float height));
    (* Scale so that drawing can take place between 0. and 1. *)
    Cairo.scale ctx sizef sizef;
    plot#draw ctx


let create_display plot title =
  (** [create_display plot title] spawns a window with the given title
      showing the specified plot *)
  let width = init_size and height = init_size in
  let w = GWindow.window ~title ~width ~height () in
  let area = GMisc.drawing_area ~width ~height ~packing:w#add() in
  let draw = draw_plot_to_gtk_area plot in
    area#misc#realize ();
    ignore (area#event#connect#expose (fun _ -> draw area; true));
    ignore (w#connect#destroy GMain.quit);
    ignore (draw area);
    w#show();
    GMain.main ()

(* EOF *)
