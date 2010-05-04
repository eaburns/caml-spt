(** Ml_plot_gtk - Holds all of the gtk display functionality for easy
    removal - Jordan*)

(* Need to set up gtk for use *)
ignore (GtkMain.Main.init())

let init_size = 400 (* default window size *)

let file_dialog ~title ~callback () =
  let sel =
    GWindow.file_selection ~title ~modal:true (*?filename*) () in
  ignore (sel#cancel_button#connect#clicked ~callback:sel#destroy);
  ignore (sel#ok_button#connect#clicked ~callback:
	    begin fun () ->
	      let name = sel#filename in
		sel#destroy ();
		callback name
	    end);
  sel#show ()

let save_dialog plot =
    file_dialog ~title:"Save"
      ~callback:(fun file -> Spt_cairo.save plot file) ()

let draw_plot_to_gtk_area plot area =
  (** [draw_plot plot area] draws the plot to a GTK drawing area. *)
  let ctx = Cairo_lablgtk.create area#misc#window in
  let { Gtk.width = width ; Gtk.height = height } = area#misc#allocation in
  let sizef = float (min width height) in
    Drawing.fill_rectangle ctx
      ~color:(Drawing.color ~a:1. ~r:0.8 ~g:0.8 ~b:0.8)
      (Geometry.rectangle 0. (float width) 0. (float height));
    Drawing.fill_rectangle ctx ~color:Drawing.white
      (Geometry.rectangle 0. (sizef *. plot#width) 0. (sizef *. plot#height));
    (* Scale so that drawing can take place between 0. and 1. *)
    Drawing.scale ctx sizef sizef;
    plot#draw ctx


open GdkKeysyms

let create_display plot title =
  (** [create_display plot title] spawns a window with the given title
      showing the specified plot *)
  let width = init_size and height = init_size in
  let w = GWindow.window ~title ~width ~height:(height + 40) () in
  let vbox = GPack.vbox ~packing:w#add () in
  let menu_bar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory ~accel_path:"<MLPLOT>/" menu_bar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in
  let factory = new GMenu.factory ~accel_path:"<MLPLOT File>///" file_menu
    ~accel_group in
  let area = GMisc.drawing_area ~width ~height
    ~packing:(vbox#pack ~expand:true) () in
  let draw = draw_plot_to_gtk_area plot in
    ignore (factory#add_item "Save as..." ~key:_S ~callback:
	      (fun _ -> (save_dialog plot)));
    area#misc#realize ();
    ignore (factory#add_item "Quit" ~key:_Q ~callback:w#destroy);
    ignore (area#event#connect#expose (fun _ -> draw area; true));
    ignore (w#connect#destroy GMain.quit);
    ignore (draw area);
    w#show();
    GMain.main ()

(* EOF *)
