*----------------------------------------------------------------------*
*       CLASS /GAL/BACKEND_FILE_BROWSER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class /GAL/BACKEND_FILE_BROWSER definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  methods CONSTRUCTOR
    importing
      !CONTAINER type ref to CL_GUI_CONTAINER
      !RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO
      !BASE_DIRECTORY type STRING optional
      !INITIAL_DIRECTORY type STRING optional
      !INITIAL_FILE type STRING optional
      !INITIAL_FILTER type STRING optional
      !FILE_DOUBLE_CLICK_FCODE type SYUCOMM optional
      !FILE_CLICK_FCODE type SYUCOMM optional
      !DIRECTORY_CLICK_FCODE type SYUCOMM optional
    raising
      /GAL/CX_CONTROL_EXCEPTION
      /GAL/CX_IO_EXCEPTION .
  methods FREE .
  methods GET_SELECTED_FILE
    returning
      value(SELECTED_FILE) type STRING
    raising
      /GAL/CX_IO_EXCEPTION .
  methods SET_FILTER
    importing
      !FILTER type STRING .
  methods SET_SELECTED_FILE
    importing
      !SELECTED_FILE type STRING
    exporting
      value(IS_EXISTING_FILE) type ABAP_BOOL
      value(IS_EXISTING_DIRECTORY) type ABAP_BOOL
    raising
      /GAL/CX_IO_EXCEPTION
      /GAL/CX_CONTROL_EXCEPTION .
protected section.
private section.

  data BASE_DIRECTORY type STRING .
  data CASE_SENSISTIVE_PATH type ABAP_BOOL .
  data DIRECTORY_CLICK_FCODE type SYUCOMM .
  data FILE_CLICK_FCODE type SYUCOMM .
  data FILE_DOUBLE_CLICK_FCODE type SYUCOMM .
  data FILTER type STRING .
  data NODE_DATA type /GAL/DIRECTORY_CONTENT .
  data PATH_SEPARATOR type STRING .
  data RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO .
  data ROOT_PATH type STRING .
  data TREE type ref to CL_GUI_ALV_TREE .

  methods CREATE_COLUMN_LAYOUT
    exporting
      !IT_COLUMN_LAYOUT type LVC_T_FCAT .
  methods EXPAND_DIRECTORY_NODE
    importing
      !NODE_KEY type LVC_NKEY optional .
  methods GET_NODE_DATA
    importing
      !NODE_KEY type LVC_NKEY
    returning
      value(NODE_DATA) type /GAL/DIRECTORY_CONTENT_ENTRY .
  methods GET_NODE_KEY
    importing
      !NODE_PATH type STRING
    returning
      value(NODE_KEY) type LVC_NKEY .
  methods GET_NODE_PATH
    importing
      !NODE_KEY type LVC_NKEY
    returning
      value(NODE_PATH) type STRING .
  methods HANDLE_EXPAND_NO_CHILDREN
    for event EXPAND_NC of CL_GUI_ALV_TREE
    importing
      !NODE_KEY .
  methods HANDLE_NODE_DOUBLE_CLICK
    for event NODE_DOUBLE_CLICK of CL_GUI_ALV_TREE
    importing
      !NODE_KEY .
  methods HANDLE_SELECTION_CHANGED
    for event SELECTION_CHANGED of CL_GUI_ALV_TREE
    importing
      !NODE_KEY .
ENDCLASS.



CLASS /GAL/BACKEND_FILE_BROWSER IMPLEMENTATION.


METHOD constructor.
  DATA l_initial_path     TYPE string.

  DATA l_it_column_layout TYPE lvc_t_fcat.
  DATA l_hierarchy_header TYPE treev_hhdr.

  DATA l_it_events        TYPE cntl_simple_events.
  DATA l_event            TYPE cntl_simple_event.

  DATA l_message          TYPE string.

  me->base_directory          = base_directory.
  me->rfc_route_info          = rfc_route_info.
  me->file_double_click_fcode = file_double_click_fcode.
  me->file_click_fcode        = file_click_fcode.
  me->directory_click_fcode   = directory_click_fcode.

* Get path separtator of target system
  CALL FUNCTION '/GAL/FILE_GET_DIR_SEPARATOR'
    EXPORTING
      rfc_route_info       = rfc_route_info
    IMPORTING
      separator            = path_separator
      case_sensistive_path = case_sensistive_path
    EXCEPTIONS
      OTHERS               = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    RAISE EXCEPTION TYPE /gal/cx_control_exception
      EXPORTING
        textid = /gal/cx_control_exception=>communication_error
        var1   = l_message.
  ENDIF.

* Set base path
  IF base_directory IS NOT INITIAL.
    root_path = /gal/path=>append_separator( path      = base_directory
                                             separator = path_separator ).
  ELSE.
    root_path = path_separator.
  ENDIF.

* Create tree control
  CREATE OBJECT tree
    EXPORTING
      parent              = container
      node_selection_mode = cl_gui_column_tree=>node_sel_mode_single
      item_selection      = abap_false
      no_html_header      = abap_true
      no_toolbar          = abap_true
    EXCEPTIONS
      others              = 1.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gal/cx_control_exception
      EXPORTING
        textid = /gal/cx_control_exception=>cannot_create_control
        var1   = `CL_GUI_ALV_TREE`.
  ENDIF.

  l_hierarchy_header-heading   = text-008.
  l_hierarchy_header-tooltip   = text-008.
  l_hierarchy_header-width     = 63.

* Hide columns and mark complete column for icon display
  create_column_layout( IMPORTING it_column_layout = l_it_column_layout ).

  tree->set_table_for_first_display( EXPORTING is_hierarchy_header = l_hierarchy_header
                                     CHANGING  it_fieldcatalog     = l_it_column_layout
                                               it_outtab           = node_data ).

* Set filter (triggers tree update)
  IF initial_filter IS NOT INITIAL.
    set_filter( initial_filter ).
  ELSE.
    set_filter( `*` ).
  ENDIF.

* Make sure that base directory exists
  IF base_directory IS NOT INITIAL AND node_data IS INITIAL.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>path_does_not_exist
        var1   = base_directory.
  ENDIF.

* Set initial path
  IF initial_directory IS NOT INITIAL OR initial_file IS NOT INITIAL.
    IF initial_directory IS NOT INITIAL.
      l_initial_path = initial_directory.
    ELSE.
      l_initial_path = base_directory.
    ENDIF.

    IF initial_file IS NOT INITIAL.
      l_initial_path = /gal/path=>combine( path1     = l_initial_path
                                           path2     = initial_file
                                           separator = path_separator ).
    ENDIF.

    TRY.
        set_selected_file( selected_file = l_initial_path ).

      CATCH /gal/cx_io_exception.                       "#EC NO_HANDLER
        "Nothing needs to be done here -> no file will be selected

    ENDTRY.
  ENDIF.

* Register events
  tree->get_registered_events( IMPORTING  events = l_it_events
                               EXCEPTIONS OTHERS = 1 ).
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gal/cx_control_exception
      EXPORTING
        textid = /gal/cx_control_exception=>cannot_get_registered_events.
  ENDIF.

  l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
  INSERT l_event INTO TABLE l_it_events.

  IF file_double_click_fcode IS NOT INITIAL.
    l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
    INSERT l_event INTO TABLE l_it_events.
  ENDIF.

  IF file_click_fcode IS NOT INITIAL OR directory_click_fcode IS NOT INITIAL.
    l_event-eventid = cl_gui_column_tree=>eventid_selection_changed.
    INSERT l_event INTO TABLE l_it_events.
  ENDIF.

  tree->set_registered_events( EXPORTING  events = l_it_events
                               EXCEPTIONS OTHERS = 1 ).
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gal/cx_control_exception
      EXPORTING
        textid = /gal/cx_control_exception=>cannot_set_registered_events.
  ENDIF.

  SET HANDLER handle_expand_no_children FOR tree.
  SET HANDLER handle_node_double_click FOR tree.
  SET HANDLER handle_selection_changed FOR tree.

* Send data to frontend.
  tree->frontend_update( ).
ENDMETHOD.                    "constructor


METHOD create_column_layout.
  DATA l_el_column_layout TYPE lvc_s_fcat.

  l_el_column_layout-fieldname = 'OWNER'.
  l_el_column_layout-inttype   = 'g'.
  l_el_column_layout-outputlen = 20.
  l_el_column_layout-coltext   = text-004.
  l_el_column_layout-seltext   = text-004.
  INSERT l_el_column_layout INTO TABLE it_column_layout.

  l_el_column_layout-fieldname = 'LENGTH'.
  l_el_column_layout-inttype   = 'P'.
  l_el_column_layout-outputlen = 20.
  l_el_column_layout-coltext   = text-003.
  l_el_column_layout-seltext   = text-003.
  INSERT l_el_column_layout INTO TABLE it_column_layout.

  l_el_column_layout-fieldname = 'MOD_DATE'.
  l_el_column_layout-inttype   = 'D'.
  l_el_column_layout-outputlen = 15.
  l_el_column_layout-coltext   = text-005.
  l_el_column_layout-seltext   = text-005.
  INSERT l_el_column_layout INTO TABLE it_column_layout.

  l_el_column_layout-fieldname = 'MOD_TIME'.
  l_el_column_layout-inttype   = 'T'.
  l_el_column_layout-outputlen = 12.
  l_el_column_layout-coltext   = text-006.
  l_el_column_layout-seltext   = text-006.
  INSERT l_el_column_layout INTO TABLE it_column_layout.
ENDMETHOD.                    "CREATE_COLUMN_LAYOUT


METHOD expand_directory_node.
  DATA l_path        TYPE string.
  DATA l_content     TYPE /gal/directory_content.

  DATA l_dir_path    TYPE string.
  DATA l_dir_content TYPE /gal/directory_content.

  DATA l_layout_node TYPE lvc_s_layn.
  DATA l_node_text   TYPE lvc_value.
  DATA l_node_key    TYPE lvc_nkey.

  FIELD-SYMBOLS <l_content>   LIKE LINE OF node_data.
  FIELD-SYMBOLS <l_node_data> LIKE LINE OF node_data.

  IF node_key IS INITIAL.
    CLEAR node_data.

    tree->delete_all_nodes( ).

    l_path = root_path.
  ELSE.
    l_path = get_node_path( node_key ).
  ENDIF.

  CALL FUNCTION '/GAL/FILE_GET_DIR_CONTENT'
    EXPORTING
      rfc_route_info = rfc_route_info
      path           = l_path
      filter         = filter
    IMPORTING
      content        = l_content
    EXCEPTIONS
      OTHERS         = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

* Process directory content
  LOOP AT l_content ASSIGNING <l_content>.

* Do not show . and .. in tree
    CHECK <l_content>-name <> '.'
      AND <l_content>-name <> '..'.

* Handle file / directory
    CLEAR l_layout_node.

    l_node_text = <l_content>-name.

    IF <l_content>-is_directory = abap_true. "Handle directory
      l_layout_node-isfolder = abap_true.

* Check if directory contains objects (node must be expandable in this case)
      l_dir_path = /gal/path=>combine( path1     = l_path
                                       path2     = <l_content>-name
                                       separator = path_separator ).

      CALL FUNCTION '/GAL/FILE_GET_DIR_CONTENT'
        EXPORTING
          rfc_route_info = rfc_route_info
          path           = l_dir_path
          filter         = filter
          max_entries    = 3
        IMPORTING
          content        = l_dir_content
        EXCEPTIONS
          io_exception   = 1
          rfc_exception  = 2.
      IF sy-subrc = 0.
        DELETE l_dir_content WHERE name = '.' OR name = '..'. "#EC CI_STDSEQ

        IF l_dir_content IS INITIAL.
          l_layout_node-expander = abap_false.
        ELSE.
          l_layout_node-expander = abap_true.
        ENDIF.
      ELSEIF sy-subrc = 1.
        l_layout_node-expander = abap_false.
      ELSE.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

        l_layout_node-expander = abap_true.
      ENDIF.
    ELSEIF l_node_text CP filter. "Handle file
      l_layout_node-n_image = 'ICON_VIEW_LIST'.
    ENDIF.

* Create tree node and store node key
    tree->add_node( EXPORTING i_relat_node_key = node_key
                              i_relationship   = cl_gui_column_tree=>relat_last_child
                              i_node_text      = l_node_text
                              is_outtab_line   = <l_content>
                              is_node_layout   = l_layout_node
                    IMPORTING e_new_node_key   = l_node_key ).

    DESCRIBE TABLE node_data LINES sy-tfill.

    READ TABLE node_data INDEX sy-tfill ASSIGNING <l_node_data>.

    <l_node_data>-tag = l_node_key.
  ENDLOOP.

  tree->frontend_update( ).
ENDMETHOD.                    "display_content


METHOD free.
  tree->free( EXCEPTIONS others = 0 ).
ENDMETHOD.


METHOD get_node_data.
  tree->get_outtab_line( EXPORTING  i_node_key    = node_key
                         IMPORTING  e_outtab_line = node_data
                         EXCEPTIONS OTHERS        = 1 ).
  IF sy-subrc <> 0.
    CLEAR node_data.
  ENDIF.
ENDMETHOD.


METHOD get_node_key.
  DATA l_path_upper TYPE string.
  DATA l_path_comp  TYPE string.

  FIELD-SYMBOLS <l_node_data> LIKE LINE OF node_data.

  READ TABLE node_data
        WITH KEY path = node_path
             ASSIGNING <l_node_data>.                    "#EC CI_STDSEQ
  IF sy-subrc = 0.
    node_key = <l_node_data>-tag.
    RETURN.
  ENDIF.

  IF case_sensistive_path = abap_false.
    l_path_upper = node_path.
    TRANSLATE l_path_upper TO UPPER CASE.

    LOOP AT node_data ASSIGNING <l_node_data>.
      l_path_comp = <l_node_data>-path.
      TRANSLATE l_path_comp TO UPPER CASE.

      IF l_path_upper = l_path_comp.
        node_key = <l_node_data>-tag.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMETHOD.


METHOD get_node_path.
  DATA l_node_data TYPE /gal/directory_content_entry.

  l_node_data = get_node_data( node_key ).
  node_path   = l_node_data-path.
ENDMETHOD.


METHOD get_selected_file.
  DATA l_node_keys       TYPE lvc_t_nkey.
  DATA l_node_key        TYPE lvc_nkey.

* Get selected item(s)
  tree->get_selected_nodes( CHANGING   ct_selected_nodes = l_node_keys
                            EXCEPTIONS OTHERS            = 1 ).
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>control_error.
  ENDIF.

  IF l_node_keys IS INITIAL.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>no_file_selected.
  ENDIF.

  READ TABLE l_node_keys INTO l_node_key INDEX 1.

  selected_file = get_node_path( l_node_key ).
ENDMETHOD.


METHOD handle_expand_no_children.
  expand_directory_node( node_key ).
ENDMETHOD.                    "handle_expand_nc


METHOD handle_node_double_click.
  DATA l_node_data          TYPE /gal/directory_content_entry.

  DATA l_expanded_node_keys TYPE lvc_t_nkey.
  DATA l_child_node_key     TYPE lvc_nkey.

  l_node_data = get_node_data( node_key ).

  IF l_node_data-is_directory = abap_true.
    tree->get_expanded_nodes( CHANGING   ct_expanded_nodes = l_expanded_node_keys
                              EXCEPTIONS OTHERS            = 1 ).
    IF l_node_data-is_directory = abap_true.
      READ TABLE l_expanded_node_keys
            WITH TABLE KEY table_line = node_key
                 TRANSPORTING NO FIELDS.                 "#EC CI_STDSEQ
    ENDIF.

    IF sy-subrc = 0.
      tree->collapse_subtree( EXPORTING  i_node_key = node_key
                              EXCEPTIONS OTHERS     = 0 ).
    ELSE.
      tree->get_first_child( EXPORTING i_node_key       = node_key
                             IMPORTING e_child_node_key = l_child_node_key ).

      IF l_child_node_key IS INITIAL.
        expand_directory_node( EXPORTING  node_key = node_key ).
      ENDIF.

      tree->expand_node( EXPORTING  i_node_key = node_key
                         EXCEPTIONS OTHERS     = 0 ).
    ENDIF.
  ELSEIF file_double_click_fcode IS NOT INITIAL.
    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode = file_double_click_fcode
      EXCEPTIONS
        OTHERS       = 0.
  ENDIF.
ENDMETHOD.


METHOD handle_selection_changed.
  DATA l_node_data TYPE /gal/directory_content_entry.

  l_node_data = get_node_data( node_key ).

  IF l_node_data-is_directory = abap_false.
    IF file_click_fcode IS NOT INITIAL.
      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode = file_click_fcode
        EXCEPTIONS
          OTHERS       = 0.
    ENDIF.
  ELSE.
    IF directory_click_fcode IS NOT INITIAL.
      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode = directory_click_fcode
        EXCEPTIONS
          OTHERS       = 0.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD set_filter.
  DATA l_exp_nodes     TYPE lvc_t_nkey.
  DATA l_node_key      TYPE lvc_nkey.

  DATA l_node_data     TYPE /gal/directory_content_entry.
  DATA l_top_node_path TYPE string.

  DATA l_exp_paths     TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
  DATA l_path          TYPE string.

* Check if filter has changed
  CHECK me->filter <> filter.

* Update Filter
  me->filter = filter.

* Get scroll position
  tree->get_top_node( IMPORTING  e_node_key = l_node_key
                      EXCEPTIONS OTHERS     = 0 ).

  l_node_data = get_node_data( l_node_key ).
  l_top_node_path = l_node_data-path.

* Build list of expanded directories
  tree->get_expanded_nodes( CHANGING   ct_expanded_nodes = l_exp_nodes
                            EXCEPTIONS OTHERS            = 0 ).

  LOOP AT l_exp_nodes INTO l_node_key.
    l_node_data = get_node_data( l_node_key ).

    CHECK l_node_data-path IS NOT INITIAL.

    INSERT l_node_data-path INTO TABLE l_exp_paths.
  ENDLOOP.

* Rebuild tree with new filter
  expand_directory_node( ).

  LOOP AT l_exp_paths INTO l_path.
    l_node_key = get_node_key( l_path ).

    CHECK l_node_key IS NOT INITIAL.

    expand_directory_node( node_key = l_node_key ).

    tree->expand_node( EXPORTING  i_node_key = l_node_key
                       EXCEPTIONS OTHERS     = 0 ).
  ENDLOOP.

* Restore scroll position
  l_node_key = get_node_key( node_path = l_top_node_path ).

  IF l_node_key IS NOT INITIAL.
    tree->set_top_node( EXPORTING  i_node_key = l_node_key
                        EXCEPTIONS OTHERS     = 0 ).
  ENDIF.

* Update frontend control
  tree->frontend_update( ).
ENDMETHOD.


METHOD set_selected_file.
  DATA l_file_path        TYPE string.
  DATA l_file_path_length TYPE i.

  DATA l_exp_nodes        TYPE lvc_t_nkey.
  DATA l_sel_nodes        TYPE lvc_t_nkey.

  DATA l_node_data        LIKE LINE OF node_data.

  DATA l_root_path_length TYPE i.

  DATA l_path_components  TYPE STANDARD TABLE OF string.
  DATA l_path_component   TYPE string.
  DATA l_path             TYPE string.

  DATA l_node_key         TYPE lvc_nkey.

  DATA l_var1             TYPE string.

  DATA l_is_last          TYPE abap_bool.

  DATA l_result           TYPE abap_bool.

* Get selected file
  l_file_path = selected_file.

* Selected file must be in base path
  l_result = /gal/path=>starts_with( path           = l_file_path
                                     part           = root_path
                                     separator      = path_separator
                                     case_sensitive = case_sensistive_path ).

  IF l_result = abap_false.
    IF base_directory IS INITIAL. "No fixed base path
      root_path = /gal/path=>get_root_path( path      = l_file_path
                                            separator = path_separator ).

      root_path = /gal/path=>append_separator( path      = root_path
                                               separator = path_separator ).

      expand_directory_node( ).
    ELSE.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>file_not_in_path
          var1   = l_file_path
          var2   = root_path.
    ENDIF.
  ENDIF.

* Build list of expanded directories
  tree->get_expanded_nodes( CHANGING   ct_expanded_nodes = l_exp_nodes
                            EXCEPTIONS OTHERS            = 0 ).

* Select file in tree
  l_root_path_length = strlen( root_path ).
  l_file_path_length = strlen( l_file_path ).

  IF l_file_path_length >= l_root_path_length.
    SPLIT selected_file+l_root_path_length AT path_separator
     INTO TABLE l_path_components.
  ENDIF.

  l_path = root_path.

  LOOP AT l_path_components INTO l_path_component.
    AT LAST.
      l_is_last = abap_true.
    ENDAT.

    CONCATENATE l_path l_path_component INTO l_path.

    l_node_key = get_node_key( l_path ).

    IF l_node_key IS INITIAL.
      IF l_is_last = abap_false.
        RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING
            textid = /gal/cx_io_exception=>path_does_not_exist
            var1   = l_path.
      ELSE.
        is_existing_file      = abap_false.
        is_existing_directory = abap_false.

        IF filter IS NOT INITIAL AND ( path_separator <> `\` OR filter <> `*.*` ) AND l_path_component NP filter.
          RAISE EXCEPTION TYPE /gal/cx_io_exception
            EXPORTING
              textid = /gal/cx_io_exception=>file_does_not_match_filter
              var1   = l_path_component
              var2   = filter.
        ENDIF.
        EXIT.
      ENDIF.
    ENDIF.

    l_node_data = get_node_data( l_node_key ).

    IF l_node_data-is_directory = abap_true.
      READ TABLE l_exp_nodes
            WITH KEY table_line = l_node_key
                 TRANSPORTING NO FIELDS.                 "#EC CI_STDSEQ
      IF sy-subrc <> 0.
        expand_directory_node( node_key = l_node_key ).

        tree->expand_node( EXPORTING  i_node_key = l_node_key
                           EXCEPTIONS OTHERS     = 0 ).
      ENDIF.
    ELSEIF l_is_last = abap_false.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>path_does_not_exist
          var1   = l_path.
    ENDIF.

    IF l_is_last = abap_true.
      IF l_node_data-is_directory = abap_true.
        is_existing_file      = abap_false.
        is_existing_directory = abap_true.
      ELSE.
        is_existing_file      = abap_true.
        is_existing_directory = abap_false.
      ENDIF.
    ENDIF.

    CONCATENATE l_path path_separator INTO l_path.
  ENDLOOP.

  IF sy-subrc <> 0.
    is_existing_file      = abap_false.
    is_existing_directory = abap_true.
  ENDIF.

* Update tree selection
  IF is_existing_file = abap_true.
    INSERT l_node_key INTO TABLE l_sel_nodes.
  ENDIF.

  tree->set_selected_nodes( EXPORTING  it_selected_nodes = l_sel_nodes
                            EXCEPTIONS OTHERS            = 1 ).
  IF sy-subrc <> 0.
    l_var1 = l_node_key.

    RAISE EXCEPTION TYPE /gal/cx_control_exception
      EXPORTING
        textid = /gal/cx_control_exception=>cannot_select_tree_node
        var1   = l_var1.
  ENDIF.

* Move to selected directory
  IF is_existing_directory = abap_true AND l_node_key IS NOT INITIAL.
    tree->set_top_node( EXPORTING  i_node_key = l_node_key
                        EXCEPTIONS OTHERS     = 1 ).
    IF sy-subrc <> 0.
      l_var1 = l_node_key.

      RAISE EXCEPTION TYPE /gal/cx_control_exception
        EXPORTING
          textid = /gal/cx_control_exception=>cannot_set_tree_top_node
          var1   = l_var1.
    ENDIF.
  ENDIF.

* Update frontend control
  tree->frontend_update( ).
ENDMETHOD.
ENDCLASS.
