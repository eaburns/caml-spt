(**

    @author eaburns
    @since 2010-05-04
*)

let normalize ~width ~height =
  (** [normalized ~width ~height] gets the normalized aspect ratio
      from the given width and height.  This is a width, height pair
      where the larger dimension is 1.0 and the smaller dimension is
      the proper portion of 1.0. *)
  if width > height then 1., height /. width else width /. height, 1.
