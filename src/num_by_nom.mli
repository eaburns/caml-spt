val y_axis_padding : Length.t
val x_axis_padding : Length.t
val data_dimensions :
  y_min:float option ->
  y_max:float option ->
  < dimensions : Geometry.range; .. > list -> Geometry.range
class plot :
  ?label_text_style:Drawing.text_style ->
  ?legend_text_style:Drawing.text_style ->
  ?tick_text_style:Drawing.text_style ->
  ?title:string ->
  ?ylabel:string ->
  ?y_min:float ->
  ?y_max:float ->
  < dimensions : Geometry.range;
    draw : Drawing.context ->
           src:Geometry.range ->
           dst:Geometry.range -> width:float -> x:float -> 'a;
    draw_x_label : Drawing.context ->
                   x:float ->
                   y:float -> Drawing.text_style -> width:float -> 'b;
    x_label_height : Drawing.context -> Drawing.text_style -> float -> float;
    .. >
  list ->
  object
    val mutable height : Length.t
    val src : Geometry.range
    val mutable width : Length.t
    method display : unit
    method draw : Drawing.context -> unit
    method private draw_x_axis :
      Drawing.context ->
      y:float -> xrange:Geometry.range -> text_width:float -> unit
    method private draw_y_axis :
      Drawing.context -> dst:Geometry.range -> Numeric_axis.axis -> unit
    method private dst_y_range :
      Drawing.context ->
      y_min:float option ->
      y_max:float option -> text_width:float -> Geometry.range
    method private fill_background : Drawing.context -> unit
    method height : Length.t
    method output : string -> unit
    method set_size : w:Length.t -> h:Length.t -> unit
    method private size : Drawing.context -> float * float
    method private title : string
    method width : Length.t
    method private x_axis_dimensions :
      Drawing.context -> Numeric_axis.axis -> Geometry.range * float
    method private yaxis : Numeric_axis.axis
  end
class virtual dataset :
  string ->
  object
    val name : string
    method virtual dimensions : Geometry.range
    method virtual draw :
      Drawing.context ->
      src:Geometry.range ->
      dst:Geometry.range -> width:float -> x:float -> unit
    method draw_x_label :
      Drawing.context ->
      x:float -> y:float -> Drawing.text_style -> width:float -> unit
    method virtual residual :
      Drawing.context ->
      src:Geometry.range ->
      dst:Geometry.range -> width:float -> x:float -> Geometry.range
    method x_label_height :
      Drawing.context -> Drawing.text_style -> float -> float
  end
val between_padding : Length.t
class dataset_group :
  string ->
  < dimensions : Geometry.range;
    draw : Drawing.context ->
           src:Geometry.range ->
           dst:Geometry.range -> width:float -> x:float -> 'a;
    draw_x_label : Drawing.context ->
                   x:float ->
                   y:float -> Drawing.text_style -> width:float -> 'b;
    residual : Drawing.context ->
               src:Geometry.range ->
               dst:Geometry.range -> width:float -> x:float -> Geometry.range;
    x_label_height : Drawing.context -> Drawing.text_style -> float -> float;
    .. >
  list ->
  object
    val name : string
    method private dataset_name_height :
      Drawing.context -> Drawing.text_style -> float -> float
    method private dataset_width : Drawing.context -> float -> float
    method dimensions : Geometry.range
    method draw :
      Drawing.context ->
      src:Geometry.range ->
      dst:Geometry.range -> width:float -> x:float -> unit
    method draw_x_label :
      Drawing.context ->
      x:float -> y:float -> Drawing.text_style -> width:float -> unit
    method residual :
      Drawing.context ->
      src:Geometry.range ->
      dst:Geometry.range -> width:float -> x:float -> Geometry.range
    method x_label_height :
      Drawing.context -> Drawing.text_style -> float -> float
  end
val minf : float -> float -> float
val maxf : float -> float -> float
val line_style : Drawing.line_style
val draw_mean_line :
  Drawing.context ->
  Drawing.line_style ->
  Geometry.range ->
  (float -> float) -> x0:float -> x1:float -> mean:float -> unit
val draw_box :
  Drawing.context ->
  Drawing.line_style ->
  Geometry.range ->
  (float -> float) -> x0:float -> x1:float -> q1:float -> q3:float -> unit
val fill_ci_box :
  Drawing.context ->
  Drawing.color ->
  Geometry.range ->
  (float -> float) ->
  x0:float -> x1:float -> lower:float -> upper:float -> unit
class boxplot_dataset :
  ?point_radius:Length.t ->
  string ->
  float array ->
  object
    val conf_lower : float
    val conf_upper : float
    val data : float array
    val mean : float
    val name : string
    val outliers : float array
    val q1 : float
    val q2 : float
    val q3 : float
    method dimensions : Geometry.range
    method draw :
      Drawing.context ->
      src:Geometry.range ->
      dst:Geometry.range -> width:float -> x:float -> unit
    method draw_x_label :
      Drawing.context ->
      x:float -> y:float -> Drawing.text_style -> width:float -> unit
    method residual :
      Drawing.context ->
      src:Geometry.range ->
      dst:Geometry.range -> width:float -> x:float -> Geometry.range
    method x_label_height :
      Drawing.context -> Drawing.text_style -> float -> float
  end
val boxplot_dataset :
  ?point_radius:Length.t -> string -> float array -> boxplot_dataset
val boxplot_datasets :
  ?point_radius:Length.t ->
  (string * float array) list -> boxplot_dataset list
class barchart_dataset :
  Drawing.fill_pattern ->
  ?line_width:Length.t ->
  string ->
  float ->
  object
    val name : string
    val style : Drawing.line_style
    method dimensions : Geometry.range
    method draw :
      Drawing.context ->
      src:Geometry.range ->
      dst:Geometry.range -> width:float -> x:float -> unit
    method draw_x_label :
      Drawing.context ->
      x:float -> y:float -> Drawing.text_style -> width:float -> unit
    method residual :
      Drawing.context ->
      src:Geometry.range ->
      dst:Geometry.range -> width:float -> x:float -> Geometry.range
    method x_label_height :
      Drawing.context -> Drawing.text_style -> float -> float
  end
val barchart_dataset :
  Drawing.fill_pattern ->
  ?line_width:Length.t -> string -> float -> barchart_dataset
val barchart_datasets :
  ?use_color:bool ->
  ?line_width:Length.t ->
  ?group:string ->
  (string * float) list -> Num_by_nom_dataset.dataset_group list
class barchart_errbar_dataset :
  Drawing.fill_pattern ->
  ?line_width:Length.t ->
  string ->
  float array ->
  object
    val name : string
    val style : Drawing.line_style
    method dimensions : Geometry.range
    method draw :
      Drawing.context ->
      src:Geometry.range ->
      dst:Geometry.range -> width:float -> x:float -> unit
    method draw_x_label :
      Drawing.context ->
      x:float -> y:float -> Drawing.text_style -> width:float -> unit
    method residual :
      Drawing.context ->
      src:Geometry.range ->
      dst:Geometry.range -> width:float -> x:float -> Geometry.range
    method x_label_height :
      Drawing.context -> Drawing.text_style -> float -> float
  end
val barchart_errbar_dataset :
  Drawing.fill_pattern ->
  ?line_width:Length.t -> string -> float array -> barchart_errbar_dataset
val barchart_errbar_datasets :
  ?use_color:bool ->
  ?line_width:Length.t ->
  ?group:string ->
  (string * float array) list -> Num_by_nom_dataset.dataset_group list
class stacked_barchart_dataset :
  (unit -> Drawing.fill_pattern) ->
  ?name:string ->
  ?line_width:Length.t ->
  (string * float) array ->
  object
    val name : string
    val style : Drawing.line_style
    method dimensions : Geometry.range
    method draw :
      Drawing.context ->
      src:Geometry.range ->
      dst:Geometry.range -> width:float -> x:float -> unit
    method draw_x_label :
      Drawing.context ->
      x:float -> y:float -> Drawing.text_style -> width:float -> unit
    method residual :
      Drawing.context ->
      src:Geometry.range ->
      dst:Geometry.range -> width:float -> x:float -> Geometry.range
    method x_label_height :
      Drawing.context -> Drawing.text_style -> float -> float
  end
val stacked_barchart_dataset :
  ?line_width:Length.t ->
  ?name:string ->
  ?fill_factory:(unit -> Drawing.fill_pattern) ->
  (string * float) array -> stacked_barchart_dataset
val stacked_barchart_datasets :
  ?line_width:Length.t ->
  ?group:string ->
  ?fill_factory:(unit -> Drawing.fill_pattern) ->
  (string option * (string * float) array) list ->
  Num_by_nom_dataset.dataset_group list
class layered_barchart_dataset :
  (unit -> Drawing.fill_pattern) ->
  ?name:string ->
  ?line_width:Length.t ->
  (string * float) array ->
  object
    val name : string
    val style : Drawing.line_style
    method dimensions : Geometry.range
    method draw :
      Drawing.context ->
      src:Geometry.range ->
      dst:Geometry.range -> width:float -> x:float -> unit
    method draw_x_label :
      Drawing.context ->
      x:float -> y:float -> Drawing.text_style -> width:float -> unit
    method residual :
      Drawing.context ->
      src:Geometry.range ->
      dst:Geometry.range -> width:float -> x:float -> Geometry.range
    method x_label_height :
      Drawing.context -> Drawing.text_style -> float -> float
  end
val layered_barchart_dataset :
  ?line_width:Length.t ->
  ?name:string ->
  ?fill_factory:(unit -> Drawing.fill_pattern) ->
  (string * float) array -> layered_barchart_dataset
val layered_barchart_datasets :
  ?line_width:Length.t ->
  ?group:string ->
  ?fill_factory:(unit -> Drawing.fill_pattern) ->
  (string option * (string * float) array) list ->
  Num_by_nom_dataset.dataset_group list
