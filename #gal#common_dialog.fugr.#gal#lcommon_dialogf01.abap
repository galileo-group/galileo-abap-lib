*----------------------------------------------------------------------*
***INCLUDE /GAL/LCOMMON_DIALOGF01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  FORMAT_TEXT
*&---------------------------------------------------------------------*
*       Format text for display
*----------------------------------------------------------------------*
*      -->P_TEXT         Text to be formatted
*      -->P_FIELD_PREFIX Prefix for dynpro fields containing lines
*      -->P_LINE_SIZE    Maximum line size
*      <--P_LINES_USED   Number of lines used
*----------------------------------------------------------------------*
FORM format_text USING p_text         TYPE string
                       p_field_prefix TYPE string
                       p_line_size    TYPE i
              CHANGING p_lines_used   TYPE i.

  DATA l_text_lines TYPE /gal/stringtable.
  DATA l_index(2)   TYPE n.
  DATA l_field_name TYPE string.

  DATA l_wa_screen  TYPE screen.

  FIELD-SYMBOLS <l_table_text_line>  TYPE string.
  FIELD-SYMBOLS <l_dynpro_text_line> TYPE string.
  FIELD-SYMBOLS <l_dynpro_prev_line> TYPE string.

* Wrap text
  CALL METHOD /gal/string=>string_to_stringtable
    EXPORTING
      input              = p_text
      word_wrap_position = p_line_size
    RECEIVING
      output             = l_text_lines.

* Copy lines to dynpro fields
  p_lines_used = 0.

  LOOP AT l_text_lines ASSIGNING <l_table_text_line>.
    l_index = sy-tabix.

    CONCATENATE p_field_prefix l_index INTO l_field_name.
    ASSIGN (l_field_name) TO <l_dynpro_text_line>.

    IF sy-subrc = 0.
      <l_dynpro_text_line> = <l_table_text_line>.

      ASSIGN <l_dynpro_text_line> TO <l_dynpro_prev_line>.

      p_lines_used = p_lines_used + 1.
    ELSE.
      IF <l_dynpro_prev_line> IS ASSIGNED.
        CONCATENATE <l_dynpro_prev_line> `...` INTO <l_dynpro_prev_line>.
      ENDIF.

      EXIT.
    ENDIF.
  ENDLOOP.

* Hide unused dynpro fields
  DO.
    l_index = p_lines_used + sy-index.

    CONCATENATE p_field_prefix l_index INTO l_field_name.
    ASSIGN (l_field_name) TO <l_dynpro_text_line>.

    IF sy-subrc = 0.
      LOOP AT SCREEN INTO l_wa_screen.
        CHECK l_wa_screen-name = l_field_name.

        l_wa_screen-invisible = '1'.

        MODIFY SCREEN FROM l_wa_screen.
      ENDLOOP.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " FORMAT_TEXT

*&---------------------------------------------------------------------*
*&      Form  get_options
*&---------------------------------------------------------------------*
*       Get selected options
*----------------------------------------------------------------------*
*      -->P_OPTIONS          Options
*      -->P_OPTION_PREFIX    Prefix for dynpro fields containing options
*----------------------------------------------------------------------*
FORM get_options USING p_options       TYPE /gal/cdlg_options
                       p_option_prefix TYPE string.

  DATA l_index(2)    TYPE n.
  DATA l_option_name TYPE string.

  FIELD-SYMBOLS <l_option>        LIKE LINE OF p_options.
  FIELD-SYMBOLS <l_dynpro_option> TYPE abap_bool.

* Get selected options from dynpro
  LOOP AT p_options ASSIGNING <l_option>.
    l_index = sy-tabix.

    CONCATENATE p_option_prefix l_index INTO l_option_name.
    ASSIGN (l_option_name) TO <l_dynpro_option>.

    IF sy-subrc = 0.
      <l_option>-is_selected = <l_dynpro_option>.
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "get_options

*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_BUTTON
*&---------------------------------------------------------------------*
*       Initialize user-defined button
*----------------------------------------------------------------------*
*      -->P_ICON   Icon
*      -->P_TEXT   Text
*      <--P_BUTTON Dynpro field for button
*----------------------------------------------------------------------*
FORM initialize_button USING p_icon   TYPE icon_d
                             p_text   TYPE string
                    CHANGING p_button TYPE c.

  IF p_icon IS INITIAL.
    p_button = p_text.
  ELSE.
    CONCATENATE p_icon p_text INTO p_button SEPARATED BY space.
  ENDIF.
ENDFORM.                    " INITIALIZE_BUTTON

*&---------------------------------------------------------------------*
*&      Form  initialize_options
*&---------------------------------------------------------------------*
*       Initilize options
*----------------------------------------------------------------------*
*      -->P_OPTIONS          Options
*      -->P_SELECTED_OPTIONS Selected options
*      -->P_OPTION_PREFIX    Prefix for dynpro fields containing options
*      -->P_LABEL_PREFIX     Prefix for dynpro fields containing labels
*      <--P_LINES_USED       Number of lines used
*----------------------------------------------------------------------*
FORM initialize_options USING p_options          TYPE /gal/cdlg_options
                              p_option_prefix    TYPE string
                              p_label_prefix     TYPE string
                     CHANGING p_lines_used       TYPE i.

  DATA l_index(2)    TYPE n.
  DATA l_option_name TYPE string.
  DATA l_label_name  TYPE string.

  DATA l_wa_screen   TYPE screen.

  FIELD-SYMBOLS <l_option>        LIKE LINE OF p_options.
  FIELD-SYMBOLS <l_dynpro_option> TYPE abap_bool.
  FIELD-SYMBOLS <l_dynpro_label>  TYPE string.

* Copy options to dynpro fields
  p_lines_used = 0.

  LOOP AT p_options ASSIGNING <l_option>.
    l_index = sy-tabix.

    CONCATENATE p_option_prefix l_index INTO l_option_name.
    ASSIGN (l_option_name) TO <l_dynpro_option>.

    IF sy-subrc = 0.
      CONCATENATE p_label_prefix l_index INTO l_label_name.
      ASSIGN (l_label_name) TO <l_dynpro_label>.
    ENDIF.

    IF sy-subrc = 0.
      IF <l_option>-is_selected = abap_true.
        <l_dynpro_option> = abap_true.
      ELSE.
        <l_dynpro_option> = abap_false.
      ENDIF.

      IF <l_option>-is_disabled = abap_true.
        LOOP AT SCREEN INTO l_wa_screen.
          CHECK l_wa_screen-name = l_option_name
             OR l_wa_screen-name = l_label_name.

          l_wa_screen-input = '0'.

          MODIFY SCREEN FROM l_wa_screen.
        ENDLOOP.
      ENDIF.

      <l_dynpro_label> = <l_option>-text.

      p_lines_used = p_lines_used + 1.
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.

* Hide unused fields
  DO.
    l_index = p_lines_used + sy-index.

    CONCATENATE p_option_prefix l_index INTO l_option_name.
    CONCATENATE p_label_prefix l_index INTO l_label_name.

    ASSIGN (l_option_name) TO <l_dynpro_option>.

    IF sy-subrc = 0.
      ASSIGN (l_label_name) TO <l_dynpro_label>.
    ENDIF.

    IF sy-subrc = 0.
      LOOP AT SCREEN INTO l_wa_screen.
        CHECK l_wa_screen-name = l_option_name
           OR l_wa_screen-name = l_label_name.

        l_wa_screen-invisible = '1'.

        MODIFY SCREEN FROM l_wa_screen.
      ENDLOOP.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    "initialize_options

*&---------------------------------------------------------------------*
*&      Form  SET_DEFAULT_BUTTON
*&---------------------------------------------------------------------*
*       Set default button
*----------------------------------------------------------------------*
*      -->P_PREFIX         Prefix for dynpro field name of button
*      -->P_STYLE          Dialog style
*      -->P_DEFAULT_RESULT Default result
*----------------------------------------------------------------------*
FORM set_default_button USING p_prefix          TYPE string
                              p_allowed_results TYPE /gal/stringtable
                              p_default_result  TYPE string.

  DATA l_field_name(100) TYPE c.

  FIELD-SYMBOLS <l_default_result> TYPE string.

  READ TABLE p_allowed_results
        WITH KEY table_line = p_default_result
             ASSIGNING <l_default_result>.               "#EC CI_STDSEQ
  IF sy-subrc <> 0.
    READ TABLE p_allowed_results INDEX 1 ASSIGNING <l_default_result>.
  ENDIF.

  IF <l_default_result> IS ASSIGNED.
    CONCATENATE p_prefix <l_default_result> INTO l_field_name.

    SET CURSOR FIELD l_field_name.
  ENDIF.
ENDFORM.                    " SET_DEFAULT_BUTTON


*--------------------------------------------------------------------*
* CLASS lcl_folder_tree_dialog IMPLEMENTATION
*--------------------------------------------------------------------*
CLASS lcl_folder_tree_dialog IMPLEMENTATION.

  METHOD constructor.
    DATA l_events TYPE cntl_simple_events.
    DATA l_event  TYPE cntl_simple_event.

    IF store IS SUPPLIED AND store IS NOT INITIAL.
      me->store = store.
    ELSE.
      CREATE OBJECT me->store
        TYPE /gal/config_store_local.
    ENDIF.

    IF me->store IS NOT INITIAL.
      root_node = me->store->root.
    ENDIF.

    ui_program = sy-repid.

    CREATE OBJECT tree_container
      EXPORTING
        container_name = container_name.

    CREATE OBJECT tree
      EXPORTING
        node_selection_mode = cl_gui_simple_tree=>node_sel_mode_single.

    tree->create_tree_control( tree_container ).

    l_event-eventid    = cl_simple_tree_model=>eventid_node_double_click.
    l_event-appl_event = abap_false.
    INSERT l_event INTO TABLE l_events.
    tree->set_registered_events( l_events ).

    SET HANDLER handle_expand_no_children FOR tree.
    SET HANDLER handle_double_click FOR tree.

    populate_tree( ).

  ENDMETHOD.

  METHOD populate_tree.
    DATA l_node_key TYPE tm_nodekey.

    CHECK tree IS NOT INITIAL.

    tree->delete_all_nodes( ).
    l_node_key = root_node->id.

    tree->add_node( node_key    = l_node_key
                    isfolder    = abap_true
                    expander    = abap_true
                    text        = root_node->name
                    user_object = root_node ).

    handle_expand_no_children( l_node_key ).

    selected_node = root_node.
    handle_user_command( 'SELECT' ).

  ENDMETHOD.

  METHOD get_node.
    DATA l_node_properties TYPE treemsnodt.

    CHECK tree IS NOT INITIAL.

    tree->node_get_properties( EXPORTING node_key = node_key
                               IMPORTING properties = l_node_properties ).
    node ?= l_node_properties-userobject.
  ENDMETHOD.

  METHOD handle_expand_no_children.
    DATA l_exception    TYPE REF TO cx_root.
    DATA l_message      TYPE string.

    DATA l_node         TYPE REF TO /gal/config_node.
    DATA l_child_nodes  TYPE /gal/config_nodes.
    DATA l_node_key     TYPE tm_nodekey.
    DATA l_node_keys    TYPE treemnotab.
    DATA l_image        TYPE tv_image.

    CHECK tree IS NOT INITIAL.

    TRY.
        l_node = get_node( node_key ).

        tree->node_get_children( EXPORTING node_key       = node_key
                                 IMPORTING node_key_table = l_node_keys ).
        tree->delete_nodes( l_node_keys ).

        l_child_nodes = l_node->get_child_nodes( ).
        LOOP AT l_child_nodes INTO l_node.         "#EC CI_LOOP_INTO_WA
          CHECK l_node->is_folder = abap_true.

          TRY.
              store->authority_check( node   = l_node
                                      action = /gal/config_node_actions=>display_node ).

            CATCH /gal/cx_auth_check_exception.
              CONTINUE.

          ENDTRY.

          l_node_key = l_node->id.

          CASE l_node->type.

            WHEN /gal/config_node=>const_node_type_value_client.
              l_image = icon_oo_attribute.

            WHEN /gal/config_node=>const_node_type_value_system.
              l_image = icon_sym_log_server.

            WHEN /gal/config_node=>const_node_type_value_user.
              l_image = icon_position_hr.

            WHEN OTHERS.
              CLEAR l_image.

          ENDCASE.

          tree->add_node( node_key          = l_node_key
                          relative_node_key = node_key
                          relationship      = cl_simple_tree_model=>relat_last_child
                          isfolder          = l_node->is_folder
                          expander          = l_node->is_parent
                          image             = l_image
                          text              = l_node->name
                          user_object       = l_node ).
        ENDLOOP.

        IF sy-subrc IS INITIAL.
          tree->expand_node( node_key ).
        ELSE.
          tree->node_set_expander( node_key = node_key
                                   expander = abap_false ).
        ENDIF.

      CATCH /gal/cx_config_exception INTO l_exception.
        l_message = l_exception->get_text( ).
        MESSAGE l_message TYPE 'S'.
    ENDTRY.

  ENDMETHOD.

  METHOD handle_double_click.

    CHECK tree IS NOT INITIAL.

    selected_node = get_node( node_key ).
    cl_gui_cfw=>set_new_ok_code( 'SELECT' ).
  ENDMETHOD.

  METHOD handle_user_command.

    DATA l_message TYPE string.

    CASE user_command.
      WHEN /gal/common_dialog=>dlg_result_cancel OR /gal/common_dialog=>dlg_result_ok.

        IF user_command = /gal/common_dialog=>dlg_result_ok
          AND selected_node->path = '/'.

          l_message = TEXT-e02.
          MESSAGE l_message TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        set_ui_field_value( field_name  = 'G_DYNP_3000-EXIT_UCOMM'
                            field_value = user_command ).
        free( ).

        SET SCREEN 0.
        LEAVE SCREEN.

      WHEN 'SELECT'.
        IF selected_node IS NOT INITIAL.

          set_ui_field_value( field_name  = 'G_DYNP_3000-TARGET_ID'
                              field_value = selected_node->id ).

          set_ui_field_value( field_name  = 'G_DYNP_3000-TARGET_NAME'
                              field_value = selected_node->name ).

          set_ui_field_value( field_name  = 'G_DYNP_3000-TARGET_PATH'
                              field_value = selected_node->path ).
        ENDIF.

      WHEN OTHERS.
        cl_gui_cfw=>dispatch( ).
    ENDCASE.

  ENDMETHOD.

  METHOD set_ui_field_value.
    DATA l_field_name TYPE string.

    FIELD-SYMBOLS <l_field_value> TYPE any.

    CONCATENATE '(' ui_program ')' field_name INTO l_field_name.
    ASSIGN (l_field_name) TO <l_field_value>.

    IF sy-subrc = 0.
      <l_field_value> = field_value.
    ENDIF.
  ENDMETHOD.

  METHOD free.

    IF tree_container IS NOT INITIAL.
      tree_container->free( ).
    ENDIF.

    CLEAR tree.
    CLEAR tree_container.
  ENDMETHOD.

ENDCLASS.
