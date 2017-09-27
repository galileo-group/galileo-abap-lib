class /GAL/DYNPRO_LAYOUT definition
  public
  create public .

public section.
  type-pools ABAP .
  type-pools CNTL .

  constants DOCK_AT_BOTTOM type I value CL_GUI_DOCKING_CONTAINER=>DOCK_AT_BOTTOM. "#EC NOTEXT
  constants DOCK_AT_LEFT type I value CL_GUI_DOCKING_CONTAINER=>DOCK_AT_LEFT. "#EC NOTEXT
  constants DOCK_AT_RIGHT type I value CL_GUI_DOCKING_CONTAINER=>DOCK_AT_RIGHT. "#EC NOTEXT
  constants DOCK_AT_TOP type I value CL_GUI_DOCKING_CONTAINER=>DOCK_AT_TOP. "#EC NOTEXT
  constants METRIC_DYNPRO type I value CL_GUI_CONTAINER=>METRIC_DEFAULT. "#EC NOTEXT
  constants METRIC_MM type I value CL_GUI_CONTAINER=>METRIC_MM. "#EC NOTEXT
  constants METRIC_PERCENT type I value -2. "#EC NOTEXT
  constants METRIC_PIXEL type I value CL_GUI_CONTAINER=>METRIC_PIXEL. "#EC NOTEXT
  constants METRIC_UNDEFINED type I value -1. "#EC NOTEXT

  methods ADD_CUSTOM_CONTAINER
    importing
      !CONTAINER_NAME type CSEQUENCE
      !NAME type CSEQUENCE optional
      !STYLE type I optional
    raising
      /GAL/CX_DYNP_LAYOUT_EXCEPTION .
  methods ADD_DOCKING_CONTAINER
    importing
      !NAME type CSEQUENCE
      !MODE type I default DOCK_AT_LEFT
      !SIZE type CSEQUENCE
      !STYLE type I default -1
      !LIFETIME type I default CNTL_LIFETIME_DEFAULT
    raising
      /GAL/CX_DYNP_LAYOUT_EXCEPTION .
  methods ADD_NAMED_CONTAINER
    importing
      !NAME type CSEQUENCE
      !CONTAINER type ref to CL_GUI_CONTAINER
    raising
      /GAL/CX_DYNP_LAYOUT_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !ROOT type ref to CL_GUI_CONTAINER optional
      !LAYOUT_DEFINITION type STRING optional
    raising
      /GAL/CX_DYNP_LAYOUT_EXCEPTION .
  methods FREE .
  methods GET_LAYOUT_DEFINITION
    returning
      value(LAYOUT_DEFINITION) type STRING
    raising
      /GAL/CX_DYNP_LAYOUT_EXCEPTION .
  methods GET_NAMED_CONTAINER
    importing
      !NAME type CSEQUENCE
    returning
      value(CONTAINER) type ref to CL_GUI_CONTAINER
    raising
      /GAL/CX_DYNP_LAYOUT_EXCEPTION .
  methods SPLIT_CONTAINER
    importing
      !PARENT_NAME type CSEQUENCE
      !SPLITTER_NAME type CSEQUENCE optional
      !CHILD_NAMES type CSEQUENCE
      !ROWS type I default 1
      !ROW_HEIGHT type CSEQUENCE optional
      !COLUMNS type I default 1
      !COLUMN_WIDTH type CSEQUENCE optional
      !BORDER type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_DYNP_LAYOUT_EXCEPTION .
protected section.

  methods APPLY_LAYOUT_DEFINITION
    importing
      !LAYOUT_DEFINITION type STRING
    raising
      /GAL/CX_DYNP_LAYOUT_EXCEPTION .
  methods PARSE_SIZE
    importing
      !SIZE type CSEQUENCE
    exporting
      !METRIC type I
      !VALUE type I
      !FIXED type ABAP_BOOL
    raising
      /GAL/CX_DYNP_LAYOUT_EXCEPTION .
private section.

  types:
    BEGIN OF t_container_dictionary_entry.
  TYPES   name      TYPE string.
  TYPES   container TYPE REF TO cl_gui_container.
  TYPES END OF t_container_dictionary_entry .
  types:
    t_container_dictionary TYPE HASHED TABLE OF t_container_dictionary_entry
                               WITH UNIQUE KEY name .

  data CONTAINER_DICTIONARY type T_CONTAINER_DICTIONARY .
ENDCLASS.



CLASS /GAL/DYNPRO_LAYOUT IMPLEMENTATION.


METHOD add_custom_container.
  DATA l_custom_container   TYPE REF TO cl_gui_custom_container.
  DATA l_container_name(50) TYPE c.
  DATA l_name               TYPE string.

  l_container_name = container_name.

  CREATE OBJECT l_custom_container
    EXPORTING
      container_name = l_container_name
      style          = style
    EXCEPTIONS
      OTHERS         = 1.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gal/cx_dynp_layout_exception
      EXPORTING
        textid = /gal/cx_dynp_layout_exception=>container_creation_failed.
  ENDIF.

  IF name IS INITIAL.
    l_name = container_name.
  ELSE.
    l_name = name.
  ENDIF.

  add_named_container( name      = l_name
                       container = l_custom_container ).
ENDMETHOD.


METHOD add_docking_container.
  DATA l_docking_container TYPE REF TO cl_gui_docking_container.

  DATA l_size              TYPE string.
  DATA l_size_metric       TYPE i.
  DATA l_size_value        TYPE i.

  DATA l_style             TYPE i.

  DATA l_fixed             TYPE abap_bool.

  parse_size( EXPORTING size   = size
              IMPORTING metric = l_size_metric
                        value  = l_size_value
                        fixed  = l_fixed ).

  IF l_size_metric = metric_undefined.
    l_size = size.

    RAISE EXCEPTION TYPE /gal/cx_dynp_layout_exception
      EXPORTING
        textid = /gal/cx_dynp_layout_exception=>size_required
        var1   = l_size.
  ENDIF.

* Determine container style
  IF style = -1.
    IF l_fixed = abap_true.
      l_style = cl_gui_control=>ws_child + cl_gui_control=>ws_visible.
    ELSE.
      l_style = cl_gui_control=>ws_child + cl_gui_control=>ws_visible + cl_gui_control=>ws_thickframe.
    ENDIF.
  ELSE.
    l_style = style.
  ENDIF.

  IF l_size_metric <> metric_percent.
    CREATE OBJECT l_docking_container
      EXPORTING
        side      = mode
        metric    = l_size_metric
        extension = l_size_value
        lifetime  = lifetime
        style     = l_style
      EXCEPTIONS
        OTHERS    = 1.
  ELSE.
    CREATE OBJECT l_docking_container
      EXPORTING
        side     = mode
        ratio    = l_size_value
        lifetime = lifetime
        style    = l_style
      EXCEPTIONS
        OTHERS   = 1.
  ENDIF.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gal/cx_dynp_layout_exception
      EXPORTING
        textid = /gal/cx_dynp_layout_exception=>container_creation_failed.
  ENDIF.

  add_named_container( name      = name
                       container = l_docking_container ).
ENDMETHOD.


METHOD add_named_container.
  DATA l_wa_container_dictionary LIKE LINE OF container_dictionary.

  l_wa_container_dictionary-name      = name.
  l_wa_container_dictionary-container = container.
  INSERT l_wa_container_dictionary INTO TABLE container_dictionary.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gal/cx_dynp_layout_exception
      EXPORTING
        textid = /gal/cx_dynp_layout_exception=>container_name_already_exists
        var1   = l_wa_container_dictionary-name.
  ENDIF.
ENDMETHOD.


METHOD apply_layout_definition.
  RAISE EXCEPTION TYPE /gal/cx_dynp_layout_exception
    EXPORTING
      textid = /gal/cx_dynp_layout_exception=>not_implemented.
ENDMETHOD.


METHOD constructor.
  IF root IS NOT INITIAL.
    add_named_container( name      = `ROOT`
                         container = root ).
  ELSE.
    add_named_container( name      = `ROOT`
                         container = cl_gui_container=>default_screen ).
  ENDIF.

  IF layout_definition IS NOT INITIAL.
    apply_layout_definition( layout_definition = layout_definition ).
  ENDIF.
ENDMETHOD.


METHOD free.
  FIELD-SYMBOLS <l_container_dictionary> LIKE LINE OF container_dictionary.

  WHILE container_dictionary IS NOT INITIAL.
    LOOP AT container_dictionary ASSIGNING <l_container_dictionary>.
      AT LAST.
        IF <l_container_dictionary>-container <> cl_gui_container=>default_screen.
          <l_container_dictionary>-container->free( EXCEPTIONS OTHERS = 0 ).
        ENDIF.

        DELETE TABLE container_dictionary WITH TABLE KEY name = <l_container_dictionary>-name.
      ENDAT.
    ENDLOOP.
  ENDWHILE.
ENDMETHOD.


METHOD get_layout_definition.
  RAISE EXCEPTION TYPE /gal/cx_dynp_layout_exception
    EXPORTING
      textid = /gal/cx_dynp_layout_exception=>not_implemented.
ENDMETHOD.


METHOD get_named_container.
  DATA l_name TYPE string.

  FIELD-SYMBOLS  <l_container_dictionary> LIKE LINE OF container_dictionary.

  l_name = name.

  READ TABLE container_dictionary
        WITH TABLE KEY name = l_name
             ASSIGNING <l_container_dictionary>.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gal/cx_dynp_layout_exception
      EXPORTING
        textid = /gal/cx_dynp_layout_exception=>unknown_container_name
        var1   = l_name.
  ENDIF.

  container = <l_container_dictionary>-container.
ENDMETHOD.


METHOD parse_size.
  DATA l_size TYPE string.
  DATA l_unit TYPE string.

  l_size = /gal/string=>trim( size ).

  metric = metric_undefined.
  fixed  = /gal/string=>ends_with( input = size
                                   part  = `!` ).

  IF fixed = abap_true.
    SHIFT l_size RIGHT CIRCULAR.
    SHIFT l_size LEFT.
  ENDIF.

  IF l_size IS INITIAL OR l_size = `*`.
    value  = 0.
    RETURN.
  ENDIF.

  IF l_size CN '0123456789'.
    l_unit = l_size+sy-fdpos.
    l_size = l_size(sy-fdpos).

    l_unit = /gal/string=>trim( l_unit ).

    CASE l_unit.

      WHEN `mm`.
        metric = metric_mm.

      WHEN `px`.
        metric = metric_pixel.

      WHEN `%`.
        metric = metric_percent.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE /gal/cx_dynp_layout_exception
          EXPORTING
            textid = /gal/cx_dynp_layout_exception=>unknown_unit_for_size
            var1   = l_unit.

    ENDCASE.
  ELSE.
    metric = metric_dynpro.
  ENDIF.

  value = l_size.
ENDMETHOD.


METHOD split_container.
  DATA l_parent_container   TYPE REF TO cl_gui_container.

  DATA l_splitter_container TYPE REF TO cl_gui_splitter_container.
  DATA l_splitter_name      TYPE string.

  DATA l_child_names        TYPE STANDARD TABLE OF string.
  DATA l_child_name         TYPE string.
  DATA l_child_container    TYPE REF TO cl_gui_container.

  DATA l_row_height         TYPE STANDARD TABLE OF string.
  DATA l_column_width       TYPE STANDARD TABLE OF string.

  DATA l_count              TYPE i.
  DATA l_row                TYPE i.
  DATA l_column             TYPE i.

  DATA l_string             TYPE string.

  DATA l_col_size_metric    TYPE i.
  DATA l_col_size_value     TYPE i.
  DATA l_col_metric         TYPE i.
  DATA l_col_fixed          TYPE abap_bool.

  DATA l_row_size_metric    TYPE i.
  DATA l_row_size_value     TYPE i.
  DATA l_row_metric         TYPE i.
  DATA l_row_fixed          TYPE abap_bool.


* Check parameters
  IF rows    < 1 OR  rows    > 16 OR
     columns < 1 OR  columns > 16 OR
     rows    = 1 AND columns = 1.

    RAISE EXCEPTION TYPE /gal/cx_dynp_layout_exception
      EXPORTING
        textid = /gal/cx_dynp_layout_exception=>invalid_no_of_rows_or_columns.
  ENDIF.


* Create container names
  IF l_splitter_name IS NOT INITIAL.
    l_splitter_name = splitter_name.
  ELSE.
    CONCATENATE parent_name `SPLITTER` INTO l_splitter_name SEPARATED BY `.`.
  ENDIF.

  l_count = rows * columns.

  SPLIT child_names AT `;` INTO TABLE l_child_names.

  DESCRIBE TABLE l_child_names LINES sy-tfill.

  IF sy-tfill <> l_count.
    RAISE EXCEPTION TYPE /gal/cx_dynp_layout_exception
      EXPORTING
        textid = /gal/cx_dynp_layout_exception=>invalid_no_of_child_names.
  ENDIF.

* Get size of cells and check values

  SPLIT row_height   AT `;` INTO TABLE l_row_height.
  SPLIT column_width AT `;` INTO TABLE l_column_width.

  l_row_metric = metric_undefined.

  LOOP AT l_row_height INTO l_string.
    parse_size( EXPORTING size   = l_string
                IMPORTING metric = l_row_size_metric
                          value  = l_row_size_value ).

    CHECK l_row_size_metric <> metric_undefined.

    IF l_row_metric = metric_undefined.
      l_row_metric = l_row_size_metric.
    ELSEIF l_row_metric <> l_row_size_metric.
      RAISE EXCEPTION TYPE /gal/cx_dynp_layout_exception
        EXPORTING
          textid = /gal/cx_dynp_layout_exception=>mixed_row_metric.
    ENDIF.
  ENDLOOP.

  IF l_row_metric = metric_mm.
    RAISE EXCEPTION TYPE /gal/cx_dynp_layout_exception
      EXPORTING
        textid = /gal/cx_dynp_layout_exception=>metric_not_supported
        var1   = `mm`. "#EC NOTEXT
  ENDIF.

  l_col_metric = metric_undefined.

  LOOP AT l_column_width INTO l_string.
    parse_size( EXPORTING size   = l_string
                IMPORTING metric = l_col_size_metric
                          value  = l_col_size_value ).

    CHECK l_col_size_metric <> metric_undefined.

    IF l_col_metric = metric_undefined.
      l_col_metric = l_col_size_metric.
    ELSEIF l_col_metric <> l_col_size_metric.
      RAISE EXCEPTION TYPE /gal/cx_dynp_layout_exception
        EXPORTING
          textid = /gal/cx_dynp_layout_exception=>mixed_column_metric.
    ENDIF.
  ENDLOOP.

  IF l_col_metric = metric_mm.
    RAISE EXCEPTION TYPE /gal/cx_dynp_layout_exception
      EXPORTING
        textid = /gal/cx_dynp_layout_exception=>metric_not_supported
        var1   = `mm`. "#EC NOTEXT
  ENDIF.

* Create splitter container
  l_parent_container = get_named_container( name = parent_name ).

  CREATE OBJECT l_splitter_container
    EXPORTING
      parent  = l_parent_container
      rows    = rows
      columns = columns
    EXCEPTIONS
      OTHERS  = 1.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gal/cx_dynp_layout_exception
      EXPORTING
        textid = /gal/cx_dynp_layout_exception=>container_creation_failed.
  ENDIF.

  add_named_container( name      = l_splitter_name
                       container = l_splitter_container ).

* Set container properties
  l_splitter_container->set_border( border = border ).

  IF l_row_metric = metric_percent.
    l_splitter_container->set_row_mode( mode = cl_gui_splitter_container=>mode_relative ).
  ELSE.
    l_splitter_container->set_row_mode( mode = cl_gui_splitter_container=>mode_absolute ).
  ENDIF.

  IF l_col_metric = metric_percent.
    l_splitter_container->set_column_mode( mode = cl_gui_splitter_container=>mode_relative ).
  ELSE.
    l_splitter_container->set_column_mode( mode = cl_gui_splitter_container=>mode_absolute ).
  ENDIF.

* Set cell size
  LOOP AT l_row_height INTO l_string.
    l_row = sy-tabix.

    parse_size( EXPORTING size   = l_string
                IMPORTING metric = l_row_size_metric
                          value  = l_row_size_value
                          fixed  = l_row_fixed ).

    IF l_row_size_metric <> metric_undefined.
      l_splitter_container->set_row_height( id     = l_row
                                            height = l_row_size_value ).
    ENDIF.

    AT LAST.
      IF l_row_fixed = abap_true.
        RAISE EXCEPTION TYPE /gal/cx_dynp_layout_exception
          EXPORTING
            textid = /gal/cx_dynp_layout_exception=>last_row_size_fixed.
      ENDIF.
    ENDAT.

    IF l_row_fixed = abap_true.
      l_splitter_container->set_row_sash( id    = l_row
                                          type  = cl_gui_splitter_container=>type_movable
                                          value = cl_gui_splitter_container=>false ).
    ENDIF.
  ENDLOOP.

  LOOP AT l_column_width INTO l_string.
    l_column = sy-tabix.

    parse_size( EXPORTING size   = l_string
                IMPORTING metric = l_col_size_metric
                          value  = l_col_size_value
                          fixed  = l_col_fixed ).

    IF l_col_size_metric <> metric_undefined.
      l_splitter_container->set_column_width( id    = l_column
                                              width = l_col_size_value ).
    ENDIF.

    AT LAST.
      IF l_row_fixed = abap_true.
        RAISE EXCEPTION TYPE /gal/cx_dynp_layout_exception
          EXPORTING
            textid = /gal/cx_dynp_layout_exception=>last_column_size_fixed.
      ENDIF.
    ENDAT.

    IF l_col_fixed = abap_true.
      l_splitter_container->set_column_sash( id    = l_column
                                             type  = cl_gui_splitter_container=>type_movable
                                             value = cl_gui_splitter_container=>false ).
    ENDIF.
  ENDLOOP.

* Process child containers
  CLEAR l_count.

  DO rows TIMES.
    l_row = sy-index.

    DO columns TIMES.
      l_column = sy-index.
      l_count  = l_count + 1.

      l_child_container = l_splitter_container->get_container( row    = l_row
                                                               column = l_column ).

      READ TABLE l_child_names INDEX l_count INTO l_child_name.

      add_named_container( name      = l_child_name
                           container = l_child_container ).
    ENDDO.
  ENDDO.
ENDMETHOD.
ENDCLASS.
