class /GAL/APP_CONFIG_EDITOR definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  data CURRENT_NODE type ref to /GAL/CONFIG_NODE read-only .
  data CURRENT_TAB_PROGRAM type PROGNAME read-only .
  data CURRENT_TAB_SCREEN type DYNNR read-only .
  data CURRENT_VALUE_PROGRAM type PROGNAME read-only .
  data CURRENT_VALUE_SCREEN type DYNNR read-only .
  data CURRENT_MODE type INT4 read-only .
  constants MODE_DISPLAY type INT4 value 0. "#EC NOTEXT
  constants MODE_CHANGE type INT4 value 1. "#EC NOTEXT

  methods CONSTRUCTOR .
  methods INITIALIZE_DYNPRO .
  methods PAI_0100_EXIT_COMMAND .
  methods PAI_0100_USER_COMMAND
    importing
      !USER_COMMAND type SY-UCOMM .
  methods PBO_0100_INITIALIZE .
  methods PBO_0110_INITIALIZE .
  methods PBO_0120_INITIALIZE .
  methods PBO_0130_INITIALIZE .
  methods PBO_0140_INITIALIZE .
  methods PBO_0150_INITIALIZE .
  methods RUN .
protected section.
private section.

  data CONFIG_STORE type ref to /GAL/CONFIG_STORE .
  data CURRENT_DOCU_LANGUAGE type LANGU .
  data CURRENT_VALUE_CLIENT type MANDT .
  data CURRENT_VALUE_SCOPE type C .
  data CURRENT_VALUE_TYPE type /GAL/CONFIG_VALUE_TYPE .
  data CURRENT_VALUE_USER type UNAME .
  data DOCU_CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER .
  data DOCU_EDITOR type ref to CL_GUI_TEXTEDIT .
  data ROOT_NODE type ref to /GAL/CONFIG_NODE .
  data SELECTED_NODE type ref to /GAL/CONFIG_NODE .
  data TREE type ref to CL_SIMPLE_TREE_MODEL .
  data TREE_CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER .
  data UI_PROGRAM type PROGNAME .
  data VALUE_CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER .
  data VALUE_EDITOR type ref to CL_GUI_TEXTEDIT .
  data VALUE_EDITOR_USED type ABAP_BOOL .
  data REFRESH_DROPDOWN_0130 type ABAP_BOOL .
  data REFRESH_DROPDOWN_0150 type ABAP_BOOL .
  data REFRESH_VALUE_EDITOR type ABAP_BOOL .
  data CURRENT_VALUE type STRING .
  data CURRENT_VALUE_EXISTS type ABAP_BOOL .

  methods SET_NODE_NAME
    importing
      !NODE type ref to /GAL/CONFIG_NODE
      !FORCE type ABAP_BOOL default ABAP_TRUE
    raising
      /GAL/CX_LOCK_EXCEPTION .
  methods ADD_NODE
    importing
      !PARENT_NODE type ref to /GAL/CONFIG_NODE
      !TYPE type /GAL/CONFIG_KEY_TYPE .
  methods AUTHORITY_CHECK
    raising
      /GAL/CX_AUTH_CHECK_EXCEPTION .
  methods CALL_SCREEN
    importing
      !SCREEN type SY-DYNNR .
  methods CHECK_NODE_DEFINITION_CHANGES .
  methods CHECK_NODE_DOCU_CHANGES .
  methods CHECK_NODE_VALUE_CHANGES .
  methods CLEANUP .
  methods COLLECT_CHILDREN
    importing
      !NODE type ref to /GAL/CONFIG_NODE
    changing
      !NODES type /GAL/CONFIG_NODES
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods CREATE_XML_TEMPLATE .
  methods DELETE_NODE_DOCUMENTATION .
  methods DELETE_NODE_VALUE .
  methods DELETE_SUBTREE
    importing
      !NODE type ref to /GAL/CONFIG_NODE
    raising
      /GAL/CX_LOCK_EXCEPTION .
  methods GET_NODE
    importing
      !NODE_KEY type TM_NODEKEY
    returning
      value(NODE) type ref to /GAL/CONFIG_NODE .
  methods GET_NODE_DEFINITION .
  methods GET_NODE_DOCUMENTATION .
  methods GET_NODE_VALUE .
  methods GET_UI_FIELD_VALUE
    importing
      !FIELD_NAME type CSEQUENCE
    exporting
      !FIELD_VALUE type ANY .
  methods GET_USER_NAME
    importing
      !USER type UNAME
    returning
      value(USER_NAME) type STRING .
  methods HANDLE_CONTEXT_MENU_REQ
    for event NODE_CONTEXT_MENU_REQUEST of CL_SIMPLE_TREE_MODEL
    importing
      !MENU
      !NODE_KEY .
  methods HANDLE_CONTEXT_MENU_SEL
    for event NODE_CONTEXT_MENU_SELECT of CL_SIMPLE_TREE_MODEL
    importing
      !FCODE .
  methods HANDLE_DOUBLE_CLICK
    for event NODE_DOUBLE_CLICK of CL_SIMPLE_TREE_MODEL
    importing
      !NODE_KEY .
  methods HANDLE_EXPAND_NO_CHILDREN
    for event EXPAND_NO_CHILDREN of CL_SIMPLE_TREE_MODEL
    importing
      !NODE_KEY .
  methods HANDLE_USER_COMMAND
    importing
      !USER_COMMAND type SY-UCOMM .
  methods POPULATE_TREE .
  methods RECORD_CLIENT_VALUES
    importing
      !NODES type /GAL/CONFIG_NODES
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods RECORD_DEFAULT_VALUES
    importing
      !NODES type /GAL/CONFIG_NODES
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods RECORD_DOCUMENTATION
    importing
      !NODES type /GAL/CONFIG_NODES
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods RECORD_STRUCTURE
    importing
      !NODES type /GAL/CONFIG_NODES
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods RECORD_SYSTEM_VALUES
    importing
      !NODES type /GAL/CONFIG_NODES
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods RECORD_TABLE_ENTRIES
    importing
      !TABLE type STRING
      !RECORDS type ANY TABLE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods RECORD_USER_VALUES
    importing
      !NODES type /GAL/CONFIG_NODES
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods SET_ACTIVE_TAB
    importing
      !USER_COMMAND type SY-UCOMM .
  methods SET_NODE_DEFINITION
    importing
      !SKIP_REFRESH type ABAP_BOOL default ABAP_FALSE .
  methods SET_NODE_DOCUMENTATION .
  methods SET_NODE_VALUE .
  methods SET_ROOT_NODE
    importing
      !NODE type ref to /GAL/CONFIG_NODE .
  methods SET_UI_FIELD_VALUE
    importing
      !FIELD_NAME type CSEQUENCE
      !FIELD_VALUE type ANY .
  methods TRANSPORT_SUBTREE
    importing
      !NODE type ref to /GAL/CONFIG_NODE
    exporting
      !MESSAGE type STRING
    raising
      /GAL/CX_LOCK_EXCEPTION .
  methods SET_ACTIVE_MODE
    importing
      !MODE type INT4 default MODE_DISPLAY .
  methods CHECK_VALUE_EDITOR_USAGE
    importing
      !VALUE_TYPE type /GAL/CONFIG_VALUE_TYPE optional
    exporting
      !VALUE_EDITOR_USED type ABAP_BOOL
      !IS_DATA_ELEMENT type ABAP_BOOL
      !IS_DOMAIN type ABAP_BOOL
      !IS_ABAP_BOOL type ABAP_BOOL
      !TYPE type /GAL/CONFIG_VALUE_TYPE .
  methods SET_ACTIVE_VALUE_SCREEN
    importing
      !DYNPRO type DYNNR .
  methods VALUE_CONVERSION_INPUT_OUTPUT
    importing
      !VALUE_INPUT type STRING
      !CONVERT_TO_INTERNAL type ABAP_BOOL default ABAP_TRUE
    returning
      value(VALUE_OUTPUT) type STRING .
  methods COPY_SUBTREE
    importing
      !NODE type ref to /GAL/CONFIG_NODE
    raising
      /GAL/CX_LOCK_EXCEPTION
      /GAL/CX_CONFIG_EXCEPTION .
ENDCLASS.



CLASS /GAL/APP_CONFIG_EDITOR IMPLEMENTATION.


METHOD add_node.
  CONSTANTS lc_max_length TYPE i VALUE 60.

  DATA l_name      TYPE /gal/config_key_name.
  DATA l_node      TYPE REF TO /gal/config_node.
  DATA l_node_key  TYPE tm_nodekey.

  DATA l_exception TYPE REF TO cx_root.
  DATA l_message   TYPE string.

  TRY.

* Prompt for node name
      IF type = /gal/config_node=>const_node_type_folder.
        /gal/common_dialog=>show_input_dialog( EXPORTING title            = text-t01
                                                         prompt           = text-p01
                                                         max_length       = lc_max_length
                                                         can_be_cancelled = abap_true
                                               CHANGING  value            = l_name ).
      ELSE.
        /gal/common_dialog=>show_input_dialog( EXPORTING title            = text-t02
                                                         prompt           = text-p02
                                                         max_length       = lc_max_length
                                                         can_be_cancelled = abap_true
                                               CHANGING  value            = l_name ).
      ENDIF.

* Create child node
      l_node = parent_node->create_child_node( name = l_name
                                               type = type ).

* Update children of parent node
      l_node_key = parent_node->id.
      handle_expand_no_children( l_node_key ).

* Select new node
      l_node_key = l_node->id.
      tree->set_selected_node( l_node_key ).

      selected_node = l_node.

      handle_user_command( user_command = 'SELECT' ).

    CATCH /gal/cx_config_exception
          /gal/cx_dialog_exception INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE l_message TYPE 'S'.

  ENDTRY.
ENDMETHOD.


METHOD authority_check.

* Very basic authority check only!
* All further checks are done by the object model when trying to actually
* modify data.
  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = '/GAL/CONFIG_EDITOR'
    EXCEPTIONS
      ok     = 0
      OTHERS = 1.
  IF sy-subrc <> 0.
    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        tcode  = 'SA38'
      EXCEPTIONS
        ok     = 0
        OTHERS = 1.
    IF sy-subrc = 0.
      AUTHORITY-CHECK OBJECT 'S_PROGRAM'
                          ID 'P_GROUP'  DUMMY
                          ID 'P_ACTION' FIELD 'SUBMIT'.
    ENDIF.
  ENDIF.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gal/cx_auth_check_exception
      EXPORTING
        textid = /gal/cx_auth_check_exception=>not_authorized.
  ENDIF.
ENDMETHOD.


METHOD call_screen.
  PERFORM call_screen IN PROGRAM (ui_program) USING screen.
ENDMETHOD.


METHOD check_node_definition_changes.
  DATA l_type       TYPE /gal/config_key_type.
  DATA l_fixed_type TYPE /gal/config_value_type.
  DATA l_auth_class TYPE /gal/config_auth_class_name.

  DATA l_result     TYPE string.

  DATA l_exception  TYPE REF TO cx_root.
  DATA l_message    TYPE string.

* Check if there is a current node
  IF current_node IS INITIAL.
    RETURN.
  ENDIF.

* Check if node definition has been modified
  get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0110-TYPE'
                      IMPORTING field_value = l_type ).

  get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0110-FIXED_TYPE'
                      IMPORTING field_value = l_fixed_type ).

  get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0110-AUTH_CLASS'
                      IMPORTING field_value = l_auth_class ).

  TRY.
      IF l_type       <> current_node->type OR
         l_fixed_type <> current_node->fixed_value_type OR
         l_auth_class <> current_node->authenticator_class.

* Prompt if changes should be saved
        l_result = /gal/common_dialog=>show_confirmation_dialog( title          = text-t13
                                                                 message        = text-q12
                                                                 style          = /gal/common_dialog=>dlg_style_yes_no
                                                                 default_result = /gal/common_dialog=>dlg_result_no ).

* Save changes if requested
        IF l_result = /gal/common_dialog=>dlg_result_yes.
          set_node_definition( skip_refresh = abap_true ).
        ELSE.
          get_node_definition( ).
        ENDIF.
      ENDIF.

    CATCH /gal/cx_config_exception INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE l_message TYPE 'S'.

  ENDTRY.
ENDMETHOD.


METHOD check_node_docu_changes.
  DATA l_is_modified   TYPE i.
  DATA l_result        TYPE string.

  DATA l_exception     TYPE REF TO cx_root.
  DATA l_message       TYPE string.

* Check if there is an editor, otherwise no documentation can be changed
  IF docu_editor IS INITIAL.
    RETURN.
  ENDIF.

* Check if documentation has been modified
  docu_editor->get_textstream( IMPORTING is_modified = l_is_modified ).
  cl_gui_cfw=>flush( ).

  TRY.
      IF l_is_modified = cl_gui_textedit=>true.

* Prompt if changes should be saved
        l_result = /gal/common_dialog=>show_confirmation_dialog( title          = text-t07
                                                                 message        = text-q05
                                                                 message_var1   = current_docu_language
                                                                 style          = /gal/common_dialog=>dlg_style_yes_no
                                                                 default_result = /gal/common_dialog=>dlg_result_no ).

* Save changes if requested
        IF l_result = /gal/common_dialog=>dlg_result_yes.
          set_node_documentation( ).
        ELSE.
          get_node_documentation( ).
        ENDIF.
      ENDIF.

    CATCH /gal/cx_config_exception INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE l_message TYPE 'S'.

  ENDTRY.
ENDMETHOD.


METHOD check_node_value_changes.
  DATA l_type        TYPE /gal/config_value_type.

  DATA l_is_modified TYPE i.
  DATA l_result      TYPE string.

  DATA l_exception   TYPE REF TO cx_root.
  DATA l_message     TYPE string.
  DATA l_value       TYPE string.

* Check if there is an editor, otherwise no value can be changed
  IF ( value_editor IS INITIAL ) AND ( value_editor_used IS NOT INITIAL ).
    RETURN.
  ENDIF.

* Get previously edited and currently selected value type
  get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0130-TYPE'
                      IMPORTING field_value = l_type ).

* Check if value type or value has been changed
  IF current_value_type <> l_type.
    l_is_modified = cl_gui_textedit=>true.
    refresh_dropdown_0150 = abap_true.
  ELSE.
* Check if value editor is used
    IF value_editor_used = abap_true.
      value_editor->get_textstream( IMPORTING is_modified = l_is_modified ).
    ELSE.
* When dropdown box is used, check if the value was changed
      get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0150-VALUE'
                          IMPORTING field_value = l_value ).
      IF current_value <> l_value.
        l_is_modified = cl_gui_textedit=>true.
      ENDIF.
    ENDIF.
  ENDIF.

  cl_gui_cfw=>flush( ).

  TRY.
      IF l_is_modified = cl_gui_textedit=>true.

* Prompt for confirmation to save changes
        l_result = /gal/common_dialog=>show_confirmation_dialog( title          = TEXT-t08
                                                                 message        = TEXT-q06
                                                                 message_var1   = current_value_scope
                                                                 style          = /gal/common_dialog=>dlg_style_yes_no
                                                                 default_result = /gal/common_dialog=>dlg_result_no ).

* Save changes if requested
        IF l_result = /gal/common_dialog=>dlg_result_yes.
          current_value_type = l_type.
          current_value = l_value.
          set_node_value( ).
        ELSE.
          get_node_value( ).
        ENDIF.
      ENDIF.

    CATCH /gal/cx_config_exception INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE l_message TYPE 'S'.

  ENDTRY.
ENDMETHOD.


  METHOD check_value_editor_usage.

    DATA l_type             TYPE /gal/config_value_type.
    DATA l_offset             TYPE i.
    DATA l_type_descr         TYPE REF TO cl_abap_typedescr.
    DATA l_element_descr      TYPE REF TO cl_abap_elemdescr.
    DATA l_domain             TYPE domname.
    DATA l_entitytab          TYPE entitytab.
    DATA l_is_dtel            TYPE abap_bool.
    DATA l_is_domain          TYPE abap_bool.
    DATA l_is_abap_bool       TYPE abap_bool.
    DATA l_config_store       TYPE REF TO /gal/config_store_local.
    DATA l_config_node        TYPE REF TO /gal/config_node.
    DATA l_drpdwn_deactivated TYPE /gal/config_drpdwn_excl_elem.

    value_editor_used = abap_true.
    is_data_element   = abap_false.
    is_domain         = abap_false.
    is_abap_bool      = abap_false.
    l_is_dtel         = abap_false.
    l_is_domain       = abap_false.
    l_is_abap_bool    = abap_false.
    CLEAR type.

    IF value_type IS NOT SUPPLIED.
      get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0130-TYPE'
                          IMPORTING field_value = l_type ).
    ELSE.
      l_type = value_type.
    ENDIF.

    IF l_type IS INITIAL.
      RETURN.
    ENDIF.

    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name      = l_type
                                         RECEIVING  p_descr_ref = l_type_descr
                                         EXCEPTIONS OTHERS      = 1 ).
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF l_type_descr->type_kind = cl_abap_typedescr=>typekind_string OR
       l_type_descr->type_kind = cl_abap_typedescr=>typekind_num    OR
       l_type_descr->type_kind = cl_abap_typedescr=>typekind_date   OR
       l_type_descr->type_kind = cl_abap_typedescr=>typekind_packed OR
       l_type_descr->type_kind = cl_abap_typedescr=>typekind_time   OR
       l_type_descr->type_kind = cl_abap_typedescr=>typekind_char   OR
       l_type_descr->type_kind = cl_abap_typedescr=>typekind_hex    OR
       l_type_descr->type_kind = cl_abap_typedescr=>typekind_float  OR
       l_type_descr->type_kind = cl_abap_typedescr=>typekind_int.

      IF l_type_descr->absolute_name CS '\TYPE='.
        l_offset = sy-fdpos + 6.
        l_type = l_type_descr->absolute_name+l_offset.
      ENDIF.

      IF l_type = 'ABAP_BOOL'.
        value_editor_used = abap_false.
        l_is_abap_bool = abap_true.
      ELSE.

        TRY.
            l_element_descr ?= l_type_descr.

* Only check if there are fixed values or an entity tab, if the output field length <= 10
* since the dropdown helper class can only handle keys which are less than 10 characters long
            IF l_element_descr->output_length <= 10.
              SELECT  SINGLE domname FROM dd04l
                INTO  l_domain
                WHERE as4local = 'A'
                  AND as4vers  = '0000'
                  AND rollname = l_type.
              IF sy-subrc IS INITIAL.
                l_is_dtel = abap_true.
              ELSE.
                l_domain = l_type.
              ENDIF.

              TRY.
                  CREATE OBJECT l_config_store.
                  l_config_node = l_config_store->get_node(
                    path = '/Galileo Group AG/Open Source Components/Config Editor/Disable Dropdown list for config element'
                  ).
                  l_config_node->get_value( IMPORTING value = l_drpdwn_deactivated ).

                CATCH /gal/cx_config_exception.
                  CLEAR l_drpdwn_deactivated.
              ENDTRY.

              IF l_drpdwn_deactivated IS NOT INITIAL.
                READ TABLE l_drpdwn_deactivated
                  WITH KEY table_line = current_node->path
                  TRANSPORTING NO FIELDS.
                IF sy-subrc IS INITIAL.
                  value_editor_used = abap_true.
                  type = l_type.
                  RETURN.
                ENDIF.
              ENDIF.

* Check if there is an entity tab for the domain/data element
              SELECT  SINGLE domname entitytab FROM dd01l
                INTO  (l_domain,l_entitytab)
                WHERE as4local  = 'A'
                  AND as4vers   = '0000'
                  AND domname   = l_domain.
              IF sy-subrc IS INITIAL.

                IF l_entitytab IS NOT INITIAL.
                  IF l_is_dtel = abap_false.
                    l_is_domain = abap_true.
                  ENDIF.
                  value_editor_used = abap_false.
                ELSE.
* Check if fixed values are maintained in the domain
                  SELECT  domname FROM dd07l UP TO 1 ROWS
                    INTO  l_domain
                    WHERE domname = l_domain
                      AND as4local = 'A'
                      AND as4vers  = '0000'.
                  ENDSELECT.
                  IF sy-subrc IS INITIAL.
                    IF l_is_dtel = abap_false.
                      l_is_domain = abap_true.
                    ENDIF.
                    value_editor_used = abap_false.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.

          CATCH cx_sy_move_cast_error.
            CLEAR l_element_descr.
        ENDTRY.
      ENDIF.
    ENDIF.

    IF is_data_element IS REQUESTED.
      is_data_element = l_is_dtel.
    ENDIF.

    IF is_domain IS REQUESTED.
      is_domain = l_is_domain.
    ENDIF.

    IF is_abap_bool IS REQUESTED.
      is_abap_bool = l_is_abap_bool.
    ENDIF.

    IF type IS REQUESTED.
      type = l_type.
    ENDIF.

  ENDMETHOD.


METHOD cleanup.
  IF docu_container IS NOT INITIAL.
    docu_container->free( ).

    IF docu_editor IS NOT INITIAL.
      docu_editor->free( ).
    ENDIF.

    CLEAR docu_editor.
    CLEAR docu_container.
  ENDIF.

  IF value_container IS NOT INITIAL.
    value_container->free( ).

    IF value_editor IS NOT INITIAL.
      value_editor->free( ).
    ENDIF.

    CLEAR value_editor.
    CLEAR value_container.
  ENDIF.

  tree_container->free( ).
ENDMETHOD.


METHOD collect_children.
  DATA l_children TYPE /gal/config_nodes.
  DATA l_node     TYPE REF TO /gal/config_node.

  IF node->is_folder = abap_true.
    l_children = node->get_child_nodes( ).

    LOOP AT l_children INTO l_node.
      INSERT l_node INTO TABLE nodes.

      IF l_node->is_folder = abap_true.
        collect_children( EXPORTING node  = l_node
                          CHANGING  nodes = nodes ).
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMETHOD.


METHOD constructor.

* Initialize current client and language
  current_docu_language = sy-langu.
  current_value_client  = sy-mandt.
  current_value_user    = sy-uname.

  refresh_dropdown_0130 = abap_true.
  refresh_dropdown_0150 = abap_true.
  refresh_value_editor  = abap_true.
ENDMETHOD.


  METHOD copy_subtree.

    DATA l_dialog_title TYPE string.
    DATA l_result       TYPE string.
    DATA l_node_key     TYPE tm_nodekey.
    data l_node         type ref to /gal/config_node.
    DATA l_target_id    TYPE /gal/config_key_id.
    DATA l_target_path  TYPE string.
    DATA l_exception    TYPE REF TO cx_root.
    DATA l_message      TYPE string.

    TRY.
        node->enqueue_node( EXPORTING enqueue_child_nodes = abap_true ).
        l_dialog_title = TEXT-t20.

        REPLACE '&' IN l_dialog_title WITH node->name.

        CALL FUNCTION '/GAL/CD_CSTORE_CHOOSE_FOLDER'
          EXPORTING
            title        = l_dialog_title
            config_store = config_store
          IMPORTING
            result       = l_result
            target_id    = l_target_id
            target_path  = l_target_path.

        IF l_result <> /gal/common_dialog=>dlg_result_ok.
          node->dequeue_node( ).
          RETURN.
        ENDIF.

        node->copy_node( EXPORTING copy_target_id   = l_target_id
                                   copy_target_path = l_target_path ).

        node->dequeue_node( ).


* Update children of parent node
        l_node_key = l_target_id.
        handle_expand_no_children( l_node_key ).

* Select new node
        l_node = get_node( l_node_key ).
        l_node = l_node->get_child_node( node->name ).
        l_node_key = l_node->id.
        tree->set_selected_node( l_node_key ).

        selected_node = l_node.

        handle_user_command( user_command = 'SELECT' ).

      CATCH /gal/cx_config_exception
            /gal/cx_dialog_exception INTO l_exception.

        l_message = l_exception->get_text( ).
        MESSAGE l_message TYPE 'S'.
    ENDTRY.


  ENDMETHOD.


METHOD create_xml_template.
  CONSTANTS lc_co_opt_normalizing        TYPE i VALUE 1.
  CONSTANTS lc_co_opt_no_empty           TYPE i VALUE 2.
  CONSTANTS lc_co_opt_ignore_conv_errros TYPE i VALUE 3.
  CONSTANTS lc_co_opt_linebreaks         TYPE i VALUE 4.
  CONSTANTS lc_co_opt_indent             TYPE i VALUE 5.

  DATA l_type              TYPE /gal/config_value_type.
  DATA l_type_descr        TYPE REF TO cl_abap_typedescr.
  DATA l_value             TYPE REF TO data.
  DATA l_result            TYPE string.

  DATA l_xml_writer        TYPE REF TO if_sxml_writer.
  DATA l_xml_string_writer TYPE REF TO cl_sxml_string_writer.
  DATA l_xml_string        TYPE string.
  DATA l_xml_xstring       TYPE xstring.

  DATA l_converter         TYPE REF TO cl_abap_conv_in_ce.

  DATA l_exception         TYPE REF TO cx_root.
  DATA l_message           TYPE string.

  DATA l_descr             TYPE REF TO cl_abap_typedescr.
  DATA l_classdescr        TYPE REF TO cl_abap_classdescr.

  FIELD-SYMBOLS <l_value>  TYPE any.
  FIELD-SYMBOLS <l_table>  TYPE ANY TABLE.

* Check if there is an editor, otherwise no value can be displayed
  IF value_editor IS INITIAL.
    RETURN.
  ENDIF.

  TRY.

* Get type description
      get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0130-TYPE'
                          IMPORTING field_value = l_type ).

      cl_abap_typedescr=>describe_by_name( EXPORTING  p_name      = l_type
                                           RECEIVING  p_descr_ref = l_type_descr
                                           EXCEPTIONS OTHERS      = 1 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE /gal/cx_config_exception
          EXPORTING
            textid = /gal/cx_config_exception=>unknown_type
            var1   = l_type.
      ENDIF.

      IF l_type_descr->type_kind = cl_abap_typedescr=>typekind_string OR
         l_type_descr->type_kind = cl_abap_typedescr=>typekind_num    OR
         l_type_descr->type_kind = cl_abap_typedescr=>typekind_date   OR
         l_type_descr->type_kind = cl_abap_typedescr=>typekind_packed OR
         l_type_descr->type_kind = cl_abap_typedescr=>typekind_time   OR
         l_type_descr->type_kind = cl_abap_typedescr=>typekind_char   OR
         l_type_descr->type_kind = cl_abap_typedescr=>typekind_hex    OR
         l_type_descr->type_kind = cl_abap_typedescr=>typekind_float  OR
         l_type_descr->type_kind = cl_abap_typedescr=>typekind_int.

        MESSAGE text-i01 TYPE 'I'.
        RETURN.
      ELSE.

* Prompt for confirmation before overwriting current value
        value_editor->get_textstream( IMPORTING text = l_xml_string ).

        cl_gui_cfw=>flush( ).

        IF l_xml_string IS NOT INITIAL.
          l_result = /gal/common_dialog=>show_confirmation_dialog( title          = text-t10
                                                                   message        = text-q07
                                                                   style          = /gal/common_dialog=>dlg_style_yes_no
                                                                   default_result = /gal/common_dialog=>dlg_result_no ).
          IF l_result <> /gal/common_dialog=>dlg_result_yes.
            RETURN.
          ENDIF.
        ENDIF.

* Create XML template
        TRY.
            CREATE DATA l_value TYPE (l_type).

          CATCH cx_static_check INTO l_exception.
            l_message = l_exception->get_text( ).
            MESSAGE l_message TYPE 'I'.
            RETURN.

        ENDTRY.

        ASSIGN l_value->* TO <l_value>.

        IF l_type_descr->type_kind = cl_abap_typedescr=>typekind_table.
          ASSIGN <l_value> TO <l_table>.
          INSERT INITIAL LINE INTO TABLE <l_table>.
        ENDIF.

        l_xml_string_writer = cl_sxml_string_writer=>create( ).

        l_xml_writer = l_xml_string_writer.

        CALL METHOD cl_abap_typedescr=>describe_by_object_ref
          EXPORTING
            p_object_ref         = l_xml_string_writer
          RECEIVING
            p_descr_ref          = l_descr
          EXCEPTIONS
            reference_is_initial = 1
            OTHERS               = 2.
        IF sy-subrc = 0.
          l_classdescr ?= l_descr.

          READ TABLE l_classdescr->methods
                WITH TABLE KEY name = 'IF_SXML_WRITER~SET_OPTION'
                     TRANSPORTING NO FIELDS.             "#EC CI_STDSEQ
          IF sy-subrc = 0.
            "Es gibt die Methode
            CALL METHOD l_xml_writer->('SET_OPTION')
              EXPORTING
                option = lc_co_opt_normalizing
                value  = abap_true.
            CALL METHOD l_xml_writer->('SET_OPTION')
              EXPORTING
                option = lc_co_opt_no_empty
                value  = abap_false.
            CALL METHOD l_xml_writer->('SET_OPTION')
              EXPORTING
                option = lc_co_opt_ignore_conv_errros
                value  = abap_false.
            CALL METHOD l_xml_writer->('SET_OPTION')
              EXPORTING
                option = lc_co_opt_linebreaks
                value  = abap_true.
            CALL METHOD l_xml_writer->('SET_OPTION')
              EXPORTING
                option = lc_co_opt_indent
                value  = abap_true.
          ENDIF.
        ENDIF.

        CALL TRANSFORMATION id
             OPTIONS    data_refs          = 'heap-or-create'
                        initial_components = 'include'
                        technical_types    = 'error'
                        value_handling     = 'default'
                        xml_header         = 'full'
             SOURCE     value              = <l_value>
             RESULT XML l_xml_writer.                       "#EC NOTEXT

        l_converter = cl_abap_conv_in_ce=>create( encoding = 'UTF-8'
                                                  endian   = ' ' ).

        l_xml_xstring = l_xml_string_writer->get_output( ).

        l_converter->convert( EXPORTING input = l_xml_xstring
                              IMPORTING data  = l_xml_string ).

        value_editor->set_textstream( l_xml_string ).

        cl_gui_cfw=>flush( ).
      ENDIF.

    CATCH /gal/cx_config_exception INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE l_message TYPE 'S'.

  ENDTRY.
ENDMETHOD.


METHOD delete_node_documentation.
  DATA l_documentation TYPE string.
  DATA l_result        TYPE string.

  DATA l_exception     TYPE REF TO cx_root.
  DATA l_message       TYPE string.

* Get current documentation
  docu_editor->get_textstream( IMPORTING text = l_documentation ).
  cl_gui_cfw=>flush( ).

* Check if there is anything to delete
  IF l_documentation IS NOT INITIAL.
    TRY.

* Prompt for confirmation
        l_result = /gal/common_dialog=>show_confirmation_dialog( title          = text-t05
                                                                 message        = text-q03
                                                                 message_var1   = current_docu_language
                                                                 style          = /gal/common_dialog=>dlg_style_yes_no
                                                                 default_result = /gal/common_dialog=>dlg_result_no ).

* Delete documentation if requested
        IF l_result = /gal/common_dialog=>dlg_result_yes.
          current_node->delete_description( language = current_docu_language ).
          get_node_documentation( ).
        ENDIF.

      CATCH /gal/cx_config_exception INTO l_exception.
        l_message = l_exception->get_text( ).

        MESSAGE l_message TYPE 'S'.

    ENDTRY.
  ENDIF.
ENDMETHOD.


METHOD delete_node_value.
  DATA l_result    TYPE string.

  DATA l_exception TYPE REF TO cx_root.
  DATA l_message   TYPE string.

* Check if there is anything that can be deleted
  IF current_value_type IS NOT INITIAL.
    TRY.

* Prompt for confirmation
        l_result = /gal/common_dialog=>show_confirmation_dialog( title          = text-t06
                                                                 message        = text-q04
                                                                 message_var1   = current_value_scope
                                                                 style          = /gal/common_dialog=>dlg_style_yes_no
                                                                 default_result = /gal/common_dialog=>dlg_result_no ).

* Delete value if requested
        IF l_result = /gal/common_dialog=>dlg_result_yes.
          IF current_value_scope = 'D'.
            current_node->delete_value( default = abap_true ).
          ELSE.
            current_node->delete_value( client    = current_value_client
                                        user_name = current_value_user ).
          ENDIF.
          get_node_value( ).
        ENDIF.

      CATCH /gal/cx_config_exception INTO l_exception.
        l_message = l_exception->get_text( ).

        MESSAGE l_message TYPE 'S'.

    ENDTRY.
  ENDIF.
ENDMETHOD.


METHOD delete_subtree.
  DATA l_result    TYPE string.
  DATA l_node      TYPE REF TO /gal/config_node.
  DATA l_node_key  TYPE tm_nodekey.

  DATA l_exception TYPE REF TO cx_root.
  DATA l_message   TYPE string.

  TRY.

* Prompt for confirmation
      IF node->is_folder = abap_true.
        l_result = /gal/common_dialog=>show_confirmation_dialog( title          = text-t03
                                                                 message        = text-q01
                                                                 style          = /gal/common_dialog=>dlg_style_yes_no
                                                                 default_result = /gal/common_dialog=>dlg_result_no ).
      ELSE.
        l_result = /gal/common_dialog=>show_confirmation_dialog( title          = text-t04
                                                                 message        = text-q02
                                                                 style          = /gal/common_dialog=>dlg_style_yes_no
                                                                 default_result = /gal/common_dialog=>dlg_result_no ).
      ENDIF.

* Exit if deletion was not confirmed
      IF l_result <> /gal/common_dialog=>dlg_result_yes.
        node->dequeue_node( ).
        RETURN.
      ENDIF.

* Record deleted node(s) in transport
      l_result = /gal/common_dialog=>show_confirmation_dialog( title          = text-t12
                                                               message        = text-q11
                                                               style          = /gal/common_dialog=>dlg_style_yes_no_cancel
                                                               default_result = /gal/common_dialog=>dlg_result_cancel ).
      IF l_result = /gal/common_dialog=>dlg_result_yes.
        transport_subtree( EXPORTING node    = node
                           IMPORTING message = l_message ).

        IF l_message IS NOT INITIAL.
          MESSAGE l_message TYPE 'I'.
          node->dequeue_node( ).
          RETURN.
        ENDIF.
      ELSEIF l_result = /gal/common_dialog=>dlg_result_cancel.
        node->dequeue_node( ).
        RETURN.
      ENDIF.

* Store node key of parent node
      l_node     = node->parent.
      l_node_key = l_node->id.

* Delete subtree
      node->delete( force = abap_true ).

* Select parent node and update children of parent node
      tree->set_selected_node( l_node_key ).

      handle_expand_no_children( l_node_key ).

      selected_node = l_node.

      handle_user_command( user_command = 'SELECT' ).

    CATCH /gal/cx_config_exception INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE l_message TYPE 'S'.

  ENDTRY.
ENDMETHOD.


METHOD get_node.
  DATA l_node_properties TYPE treemsnodt.

* Get node properties
  tree->node_get_properties( EXPORTING node_key   = node_key
                             IMPORTING properties = l_node_properties ).

  node ?= l_node_properties-userobject.
ENDMETHOD.


METHOD get_node_definition.
  set_ui_field_value( field_name  = 'G_DYNP_0110-TYPE'
                      field_value = current_node->type ).

  set_ui_field_value( field_name  = 'G_DYNP_0110-FIXED_TYPE'
                      field_value = current_node->fixed_value_type ).

  set_ui_field_value( field_name  = 'G_DYNP_0110-AUTH_CLASS'
                      field_value = current_node->authenticator_class ).
ENDMETHOD.


METHOD get_node_documentation.
  DATA l_documentation TYPE string.

  DATA l_exception     TYPE REF TO cx_root.
  DATA l_message       TYPE string.

* Check if there is an editor, otherwise no documentation can be displayed
  IF docu_editor IS INITIAL.
    RETURN.
  ENDIF.

* Get documentation for current language
  TRY.
      l_documentation = current_node->get_description( language = current_docu_language ).

    CATCH /gal/cx_config_exception INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE l_message TYPE 'S'.

  ENDTRY.

* Update text editor
  docu_editor->set_textstream( l_documentation ).
  cl_gui_cfw=>flush( ).
ENDMETHOD.


METHOD get_node_value.

  DATA l_exception TYPE REF TO cx_root.
  DATA l_message   TYPE string.
  DATA l_value     TYPE string.

* Check if there is an editor, otherwise no value can be displayed
  IF ( value_editor IS INITIAL ) AND ( value_editor_used = abap_true ).
    RETURN.
  ENDIF.

* Get value
  TRY.
      IF current_value_scope = 'D'.
        current_node->get_value_raw( EXPORTING default   = abap_true
                                     IMPORTING type      = current_value_type
                                               value_raw = current_value ).
      ELSE.
        current_node->get_value_raw( EXPORTING client    = current_value_client
                                               user_name = current_value_user
                                     IMPORTING type      = current_value_type
                                               value_raw = current_value ).
      ENDIF.

    CATCH /gal/cx_config_exception INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE l_message TYPE 'S'.

  ENDTRY.

* Update dropdown field
  set_ui_field_value( field_name = 'G_DYNP_0150-VALUE'
                      field_value = current_value ).

  IF current_value_type IS INITIAL AND l_value IS INITIAL.
    current_value_exists = abap_false.
  ELSE.
    current_value_exists = abap_true.
  ENDIF.

* Update editor control
  IF value_editor IS NOT INITIAL.

    IF current_value_exists = abap_false.
* Show special text to indicate that no value exists for current node
      l_value = TEXT-c01.
    ELSE.
* Convert value to external presentation if needed
      l_value = value_conversion_input_output( value_input         = current_value
                                               convert_to_internal = abap_false ).
    ENDIF.

    value_editor->set_textstream( l_value ).
    cl_gui_cfw=>flush( ).
  ENDIF.

* Use fixed value type for new values
  IF current_value_type IS INITIAL.
    current_value_type = current_node->fixed_value_type.
  ENDIF.

* Update dynpro fields
  set_ui_field_value( field_name  = 'G_DYNP_0130-TYPE'
                      field_value = current_value_type ).

  refresh_dropdown_0150 = abap_true.
ENDMETHOD.


METHOD get_ui_field_value.
  DATA l_field_name TYPE string.

  FIELD-SYMBOLS <l_field_value> TYPE any.

  CONCATENATE `(` ui_program `)` field_name INTO l_field_name.
  ASSIGN (l_field_name) TO <l_field_value>.

  IF sy-subrc = 0.
    field_value = <l_field_value>.
  ELSE.
    CLEAR field_value.
  ENDIF.
ENDMETHOD.


METHOD get_user_name.
  DATA l_address   TYPE bapiaddr3.
  DATA l_it_return TYPE TABLE OF bapiret2.

* Benutzerinformationen lesen *
  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username = user
    IMPORTING
      address  = l_address
    TABLES
      return   = l_it_return.

  IF l_address-fullname IS NOT INITIAL.
    user_name = l_address-fullname.
  ELSE.
    CONCATENATE l_address-firstname l_address-lastname
           INTO user_name SEPARATED BY space.
  ENDIF.

  SHIFT user_name LEFT DELETING LEADING space.

  IF user_name IS INITIAL.
    user_name = user.
  ENDIF.
ENDMETHOD.


METHOD handle_context_menu_req.

* Store selected node
  selected_node = get_node( node_key ).

* Build context menu
  TRY.
      config_store->authority_check( node   = selected_node
                                     action = /gal/config_node_actions=>modify_node ).

      IF selected_node->is_folder = abap_true.
        menu->add_function( fcode = 'ADD_FOLDER'
                            text  = text-m01 ).

        menu->add_function( fcode = 'ADD_VALUE_CLIENT'
                            text  = text-m02 ).

        menu->add_function( fcode = 'ADD_VALUE_SYSTEM'
                            text  = text-m03 ).

        menu->add_function( fcode = 'ADD_VALUE_USER'
                            text  = text-m08 ).

        menu->add_separator( ).

        menu->add_function( fcode = 'CHANGE_NAME'
                            text  = text-m09 ).

        menu->add_function( fcode = 'COPY_NODE'
                            text = text-m11 ).

        menu->add_separator( ).

        menu->add_function( fcode = 'TRANS_SUBTREE'
                            text  = text-m04 ).
      ELSE.
        menu->add_function( fcode = 'CHANGE_NAME'
                            text  = text-m10 ).

        menu->add_function( fcode = 'COPY_NODE'
                            text = text-m12 ).

        menu->add_separator( ).

        menu->add_function( fcode = 'TRANS_SUBTREE'
                            text  = text-m06 ).
      ENDIF.

      menu->add_separator( ).

    CATCH /gal/cx_auth_check_exception.                 "#EC NO_HANDLER
  ENDTRY.

  IF selected_node->parent IS NOT INITIAL.
    TRY.
        config_store->authority_check( node   = selected_node
                                       action = /gal/config_node_actions=>delete_node ).

        IF selected_node->is_folder = abap_true.
          menu->add_function( fcode = 'DEL_SUBTREE'
                              text  = text-m05 ).
        ELSE.
          menu->add_function( fcode = 'DEL_SUBTREE'
                              text  = text-m07 ).
        ENDIF.

      CATCH /gal/cx_auth_check_exception.               "#EC NO_HANDLER
    ENDTRY.
  ENDIF.
ENDMETHOD.


method HANDLE_CONTEXT_MENU_SEL.

    cl_gui_cfw=>set_new_ok_code( new_code = fcode ).

endmethod.


METHOD handle_double_click.

  DATA l_exception TYPE REF TO cx_root.

* Store selected node
  selected_node = get_node( node_key ).

* Unlock previous node if neccessary
  TRY.
      IF current_node IS NOT INITIAL.
        IF ( current_node <> selected_node ) AND ( current_node->is_enqueued( ) = abap_true ).
          current_node->dequeue_node( ).
        ENDIF.
      ENDIF.
    CATCH /gal/cx_lock_exception INTO l_exception.
      /gal/trace=>write_exception( EXPORTING exception = l_exception ).
  ENDTRY.

* Trigger PAI/PBO processing
  IF selected_node <> current_node.
    cl_gui_cfw=>set_new_ok_code( new_code = 'SELECT' ).
  ENDIF.
ENDMETHOD.


METHOD handle_expand_no_children.
  DATA l_node         TYPE REF TO /gal/config_node.
  DATA l_child_nodes  TYPE /gal/config_nodes.
  DATA l_node_key     TYPE tm_nodekey.
  DATA l_node_keys    TYPE treemnotab.

  DATA l_image        TYPE tv_image.

  DATA l_exception    TYPE REF TO cx_root.
  DATA l_message      TYPE string.

  TRY.

* Get node
      l_node = get_node( node_key ).

* Remove existing child nodes
      tree->node_get_children( EXPORTING node_key       = node_key
                               IMPORTING node_key_table = l_node_keys ).

      tree->delete_nodes( l_node_keys ).

* Add child nodes
      l_child_nodes = l_node->get_child_nodes( ).

      LOOP AT l_child_nodes INTO l_node.
        TRY.
            config_store->authority_check( node   = l_node
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

* Expand node
      IF sy-subrc = 0.
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


METHOD handle_user_command.
  DATA l_node_key  TYPE tm_nodekey.

  DATA l_exception TYPE REF TO cx_root.
  DATA l_message   TYPE string.

* Handle user command
  TRY.
      CASE user_command.

* Nodes
        WHEN 'ADD_FOLDER'. " Add folder
          check_node_definition_changes( ).
          check_node_docu_changes( ).
          check_node_value_changes( ).
          selected_node->enqueue_node( ).

          add_node( parent_node = selected_node
                    type        = /gal/config_node=>const_node_type_folder ).

        WHEN 'ADD_VALUE_CLIENT'. " Add client-specific value
          check_node_definition_changes( ).
          check_node_docu_changes( ).
          check_node_value_changes( ).
          selected_node->enqueue_node( ).

          add_node( parent_node = selected_node
                    type        = /gal/config_node=>const_node_type_value_client ).

        WHEN 'ADD_VALUE_SYSTEM'. " Add system-specific value
          check_node_definition_changes( ).
          check_node_docu_changes( ).
          check_node_value_changes( ).
          selected_node->enqueue_node( ).

          add_node( parent_node = selected_node
                    type        = /gal/config_node=>const_node_type_value_system ).

        WHEN 'ADD_VALUE_USER'. " Add user-specific value
          check_node_definition_changes( ).
          check_node_docu_changes( ).
          check_node_value_changes( ).
          selected_node->enqueue_node( ).

          add_node( parent_node = selected_node
                    type        = /gal/config_node=>const_node_type_value_user ).

        WHEN 'DEL_SUBTREE'. " Delete node including children
          check_node_definition_changes( ).
          check_node_docu_changes( ).
          check_node_value_changes( ).
          selected_node->enqueue_node( EXPORTING enqueue_child_nodes = abap_true ).

          delete_subtree( node = selected_node ).

        WHEN 'SELECT'. " Select configuration node
          check_node_definition_changes( ).
          check_node_docu_changes( ).
          check_node_value_changes( ).

          IF current_node IS NOT INITIAL.
            l_node_key = current_node->id.

            tree->node_set_style( EXPORTING  node_key = l_node_key
                                             style    = cl_tree_model=>style_default
                                  EXCEPTIONS OTHERS   = 0 ).
          ENDIF.

          TRY.
* Unlock previous node if neccessary
              IF ( current_node IS NOT INITIAL ) AND ( current_node <> selected_node ).
                current_node->dequeue_node( ).
              ENDIF.

              IF current_mode = mode_change.
* Switch to Display mode if current_node <> selected_node
                IF ( current_node IS NOT INITIAL ) AND ( current_node->id <> selected_node->id ).
                  set_active_mode( mode = mode_display ).
                ELSEIF ( current_node IS NOT INITIAL ) AND ( current_node->id = selected_node->id ).
* It is possible that current_node <> selected_node but the id's are the same
* In this case the display mode is not switched but the selected_node will be enqueued
                  selected_node->enqueue_node( ).
                ENDIF.
              ENDIF.
            CATCH /gal/cx_lock_exception INTO l_exception.
              /gal/trace=>write_exception( EXPORTING exception = l_exception ).
          ENDTRY.

          current_node = selected_node.

          l_node_key = current_node->id.

          tree->node_set_style( node_key = l_node_key
                                style    = cl_tree_model=>style_emphasized_positive ).

          get_node_definition( ).
          get_node_documentation( ).

          CLEAR current_value_scope. "Forces update of value during PBO

        WHEN 'TRANS_SUBTREE'. " Transport node including children
          check_node_definition_changes( ).
          check_node_docu_changes( ).
          check_node_value_changes( ).
          selected_node->enqueue_node( EXPORTING enqueue_child_nodes = abap_true ).

          transport_subtree( node = selected_node ).

* Node definition
        WHEN 'DEF_SAVE'. " Save documentation
          set_node_definition( ).

        WHEN 'DEF_UNDO'. " Undo documentation change
          get_node_definition( ).
          IF current_mode = mode_change.
            set_active_mode( mode_display ).
          ENDIF.

* Documentation
        WHEN 'DOC_LANGU'. " Change documentation language
          check_node_docu_changes( ).

          get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0120-LANGU'
                              IMPORTING field_value = current_docu_language ).

          get_node_documentation( ).

        WHEN 'DOC_DELETE'. " Delete documentation
          current_node->enqueue_node( ).
          delete_node_documentation( ).

          IF current_mode = mode_display.
            current_node->dequeue_node( ).
          ENDIF.

        WHEN 'DOC_SAVE'. " Save documentation
          set_node_documentation( ).

        WHEN 'DOC_UNDO'. " Undo documentation change
          get_node_documentation( ).
          IF current_mode = mode_change.
            set_active_mode( mode_display ).
          ENDIF.

* Values
        WHEN 'VAL_DELETE'. " Delete value
          current_node->enqueue_node( ).
          delete_node_value( ).

          IF current_mode = mode_display.
            current_node->dequeue_node( ).
          ENDIF.

        WHEN 'VAL_SAVE'. " Save value
          get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0130-TYPE'
                              IMPORTING field_value = current_value_type ).

          set_node_value( ).

        WHEN 'VAL_SCOPE'. " Change value scope, client or user
          check_node_value_changes( ).

        WHEN 'VAL_UNDO'. " Undo value change
          get_node_value( ).
          IF current_mode = mode_change.
            set_active_mode( mode_display ).
          ENDIF.

        WHEN 'XML_TEMPLATE'. " Create XML template for value
          create_xml_template( ).

        WHEN 'CHANGE_NAME'. "Change name of a folder or value
          check_node_definition_changes( ).
          check_node_docu_changes( ).
          check_node_value_changes( ).
          selected_node->enqueue_node( ).

          set_node_name( node = selected_node ).

        WHEN 'DOC_CHANGE_MODE'. " Switch Display <-> Edit Mode
          IF current_mode = mode_display.
            set_active_mode( mode = mode_change ).
          ELSE.
            check_node_definition_changes( ).
            check_node_docu_changes( ).
            IF current_node->type <> /gal/config_node=>const_node_type_folder.
              check_node_value_changes( ).
            ENDIF.
            set_active_mode( mode = mode_display ).
          ENDIF.
        WHEN 'DEF_CHANGE_MODE'. " Switch Display <-> Edit Mode
          IF current_mode = mode_display.
            set_active_mode( mode = mode_change ).
          ELSE.
            check_node_definition_changes( ).
            check_node_docu_changes( ).
            IF current_node->type <> /gal/config_node=>const_node_type_folder.
              check_node_value_changes( ).
            ENDIF.
            set_active_mode( mode = mode_display ).
          ENDIF.

        WHEN 'VAL_CHANGE_MODE'. " Switch Display <-> Edit Mode
          IF current_mode = mode_display.
            set_active_mode( mode = mode_change ).
          ELSE.
            check_node_definition_changes( ).
            check_node_docu_changes( ).
            check_node_value_changes( ).
            set_active_mode( mode = mode_display ).
          ENDIF.

        WHEN 'COPY_NODE'. "Show dialog to copy node to a destination
          check_node_definition_changes( ).
          check_node_docu_changes( ).
          check_node_value_changes( ).

          copy_subtree( node = selected_node ).


* Tab switching
        WHEN OTHERS.
          IF user_command CP 'TAB_++++'. " Tab switching
            set_active_tab( user_command ).
            RETURN.
          ENDIF.

      ENDCASE.

    CATCH /gal/cx_config_exception
          /gal/cx_lock_exception INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE l_message TYPE 'S'.

  ENDTRY.
ENDMETHOD.


METHOD initialize_dynpro.
  DATA BEGIN OF l_wa_scopes.
  DATA   key  TYPE mandt.
  DATA   text TYPE string.
  DATA END OF l_wa_scopes.

  DATA l_scopes    LIKE STANDARD TABLE OF l_wa_scopes.

  DATA l_exception TYPE REF TO cx_root.
  DATA l_message   TYPE string.

* Initialize dynpro
  TRY.

* Initialize value editor and scope dropdown
      IF current_node->is_folder = abap_false.
        IF value_editor IS NOT INITIAL.
          value_editor->set_visible( abap_true ).
        ENDIF.

        l_wa_scopes-key  = 'DEFAULT'.
        l_wa_scopes-text = TEXT-s02.
        INSERT l_wa_scopes INTO TABLE l_scopes.

        CASE current_node->type.
          WHEN /gal/config_node=>const_node_type_value_client.
            l_wa_scopes-key  = 'CLIENT'.
            l_wa_scopes-text = TEXT-s01.
            INSERT l_wa_scopes INTO TABLE l_scopes.

          WHEN /gal/config_node=>const_node_type_value_system.
            l_wa_scopes-key  = 'SYSTEM'.
            l_wa_scopes-text = TEXT-s03.
            INSERT l_wa_scopes INTO TABLE l_scopes.

          WHEN /gal/config_node=>const_node_type_value_user.
            l_wa_scopes-key  = 'USER'.
            l_wa_scopes-text = TEXT-s04.
            INSERT l_wa_scopes INTO TABLE l_scopes.

        ENDCASE.

        /gal/dropdown_helper=>init_by_value_table( field_name        = `G_DYNP_0100-SCOPE`
                                                   value_table       = l_scopes
                                                   set_default_value = abap_true ).
      ENDIF.

    CATCH /gal/cx_config_exception
          /gal/cx_dd_helper_exception INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE l_message TYPE 'S'.

  ENDTRY.
ENDMETHOD.


METHOD pai_0100_exit_command.
  LEAVE TO SCREEN 0.
ENDMETHOD.


METHOD pai_0100_user_command.
  handle_user_command( user_command ).
ENDMETHOD.


METHOD pbo_0100_initialize.
  DATA l_events    TYPE cntl_simple_events.
  DATA l_event     LIKE LINE OF l_events.

  DATA l_wa_screen TYPE screen.

* Set status and titlebar
  SET PF-STATUS 'DEFAULT' OF PROGRAM ui_program.
  SET TITLEBAR 'DEFAULT' OF PROGRAM ui_program.

* Create tree (this will only be executed once)
  IF tree_container IS INITIAL.

* Create container for tree control
    CREATE OBJECT tree_container
      EXPORTING
        container_name = 'TREE_CONTAINER'.

* Create tree model and tree control
    CREATE OBJECT tree
      EXPORTING
        node_selection_mode = cl_simple_tree_model=>node_sel_mode_single.

    tree->create_tree_control( tree_container ).

* Register events
    l_event-eventid    = cl_simple_tree_model=>eventid_node_context_menu_req.
    l_event-appl_event = abap_false.
    INSERT l_event INTO TABLE l_events.

    l_event-eventid    = cl_simple_tree_model=>eventid_node_double_click.
    l_event-appl_event = abap_false.
    INSERT l_event INTO TABLE l_events.

    tree->set_registered_events( l_events ).

* Register event handlers
    SET HANDLER handle_double_click FOR tree.
    SET HANDLER handle_expand_no_children FOR tree.
    SET HANDLER handle_context_menu_req FOR tree.
    SET HANDLER handle_context_menu_sel FOR tree.

* Populate tree
    populate_tree( ).

* Initialize tabstrip control
    set_active_tab( 'TAB_0110' ).
  ENDIF.

* No value tab for folders
  IF current_node->is_folder = abap_true AND current_tab_screen = '0130'.
    set_active_tab( 'TAB_0110' ).
  ENDIF.

* Initialize dynpro fields
  IF current_node->is_folder = abap_true.
    LOOP AT screen INTO l_wa_screen.
      CHECK l_wa_screen-group1 = 'VAL'.

      l_wa_screen-input     = '0'.
      l_wa_screen-invisible = '1'.
      MODIFY screen FROM l_wa_screen.
    ENDLOOP.
  ENDIF.

* Initialize user command
  CLEAR sy-ucomm.
ENDMETHOD.


METHOD pbo_0110_initialize.
  DATA l_node_type       TYPE string.
  DATA l_wa_screen       TYPE screen.
  DATA l_exception       TYPE REF TO cx_root.
  DATA l_message         TYPE string.
  DATA l_readonly_mode   TYPE i.
  DATA l_change_allowed  TYPE abap_bool.
  DATA l_icon_name       TYPE iconname.
  DATA l_btn_change      TYPE string.
  DATA l_no_authority    TYPE abap_bool.

* Set field status based on node type
  IF current_node->type = /gal/config_node=>const_node_type_folder.
    l_node_type = 'FLD'.
  ELSE.
    l_node_type = 'VAL'.
  ENDIF.

  LOOP AT SCREEN INTO l_wa_screen.
    CHECK l_wa_screen-group2 <> 'EDT'.

    IF l_wa_screen-group1 IS NOT INITIAL AND l_wa_screen-group1 <> l_node_type.
      l_wa_screen-input     = '0'.
      l_wa_screen-invisible = '1'.
    ELSEIF l_wa_screen-group2 IS NOT INITIAL AND l_wa_screen-group2 <> l_node_type.
      l_wa_screen-input = '0'.
    ENDIF.

    MODIFY screen FROM l_wa_screen.
  ENDLOOP.

  l_readonly_mode = cl_gui_textedit=>false.
  l_no_authority = abap_false.
  TRY.
      config_store->authority_check( node   = current_node
                                     action = /gal/config_node_actions=>modify_node ).

    CATCH /gal/cx_auth_check_exception.
      LOOP AT SCREEN INTO l_wa_screen.
        l_wa_screen-input = '0'.
        MODIFY screen FROM l_wa_screen.
      ENDLOOP.

      l_readonly_mode = cl_gui_textedit=>true.
      l_no_authority = abap_true.
  ENDTRY.

  IF current_mode = mode_change.

* Switch to read-only mode if the node is already locked
    IF l_readonly_mode = cl_gui_textedit=>false.
      TRY.
          current_node->enqueue_node( ).
        CATCH /gal/cx_config_exception INTO l_exception.
          l_message = l_exception->get_text( ).
          MESSAGE l_message TYPE 'S'.

          LOOP AT SCREEN INTO l_wa_screen.
            CHECK l_wa_screen-group2 <> 'EDT'.
            l_wa_screen-input = '0'.
            MODIFY screen FROM l_wa_screen.
          ENDLOOP.

          l_readonly_mode = cl_gui_textedit=>true.
      ENDTRY.
    ENDIF.

    IF l_readonly_mode = cl_gui_textedit=>false.
      l_change_allowed = abap_true.
    ELSE.
      l_change_allowed = abap_false.
      set_active_mode( mode = mode_display ).
    ENDIF.

  ELSE.
    l_change_allowed = abap_true.
    l_readonly_mode = cl_gui_textedit=>true.
  ENDIF.

  IF l_change_allowed = abap_true.

    IF l_readonly_mode = cl_gui_textedit=>true.
      l_icon_name = icon_change.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name   = l_icon_name
          text   = TEXT-b01
          info   = TEXT-b01
        IMPORTING
          result = l_btn_change
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc IS NOT INITIAL.
        CONCATENATE l_icon_name TEXT-b01 INTO l_btn_change SEPARATED BY space.
      ENDIF.
    ELSE.
      l_icon_name = icon_display.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name   = l_icon_name
          text   = TEXT-b02
          info   = TEXT-b02
        IMPORTING
          result = l_btn_change
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc IS NOT INITIAL.
        CONCATENATE l_icon_name TEXT-b02 INTO l_btn_change SEPARATED BY space.
      ENDIF.
    ENDIF.

    set_ui_field_value( field_name  = 'G_DYNP_0110-BTN_CHANGE'
                        field_value = l_btn_change ).

    LOOP AT SCREEN INTO l_wa_screen.
      CHECK ( l_wa_screen-group2 <> 'INP' )
        AND ( l_wa_screen-group2 <> 'DEL' )
        AND ( l_wa_screen-group2 <> 'EDT' ).

      CASE l_readonly_mode.
        WHEN cl_gui_textedit=>false.
          l_wa_screen-input = '1'.
        WHEN cl_gui_textedit=>true.
          l_wa_screen-input = '0'.
      ENDCASE.
      MODIFY screen FROM l_wa_screen.
    ENDLOOP.

  ELSE.

    l_icon_name = icon_change.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name   = l_icon_name
        text   = TEXT-b01
        info   = TEXT-b01
      IMPORTING
        result = l_btn_change
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc IS NOT INITIAL.
      CONCATENATE l_icon_name TEXT-b01 INTO l_btn_change SEPARATED BY space.
    ENDIF.

    set_ui_field_value( field_name  = 'G_DYNP_0110-BTN_CHANGE'
                        field_value = l_btn_change ).
  ENDIF.

* Deactivate Edit Button if user has not sufficient authorization
  IF l_no_authority IS NOT INITIAL.
    LOOP AT SCREEN INTO l_wa_screen.
      CHECK l_wa_screen-group2 = 'EDT'.

      l_wa_screen-input = 0.

      MODIFY screen FROM l_wa_screen.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


METHOD pbo_0120_initialize.
  DATA l_readonly_mode  TYPE i.
  DATA l_wa_screen      TYPE screen.

  DATA l_exception      TYPE REF TO cx_root.
  DATA l_message        TYPE string.
  DATA l_change_allowed TYPE abap_bool.

  DATA l_icon_name      TYPE iconname.
  DATA l_btn_change     TYPE string.
  DATA l_no_authority   TYPE abap_bool.

* Create documentation editor
  IF docu_container IS INITIAL.

* Create containers
    CREATE OBJECT docu_container
      EXPORTING
        container_name = 'DOCU_CONTAINER'.

* Create editor
    CREATE OBJECT docu_editor
      EXPORTING
        parent = docu_container.

    docu_editor->set_toolbar_mode( cl_gui_textedit=>false ).
    docu_editor->set_statusbar_mode( cl_gui_textedit=>false ).

* Initialize language dropdown and set default language
    TRY.
        /gal/dropdown_helper=>init_by_field( field_name    = `G_DYNP_0120-LANGU`
                                             sort_by_value = abap_true ).

      CATCH /gal/cx_dd_helper_exception INTO l_exception.
        l_message = l_exception->get_text( ).

        MESSAGE l_message TYPE 'S'.

    ENDTRY.

    set_ui_field_value( field_name  = 'G_DYNP_0120-LANGU'
                        field_value = current_docu_language ).

    get_node_documentation( ).
  ENDIF.

  l_readonly_mode = cl_gui_textedit=>false.
  l_no_authority = abap_false.

  TRY.
      config_store->authority_check( node   = current_node
                                     action = /gal/config_node_actions=>modify_node ).

    CATCH /gal/cx_auth_check_exception.
      LOOP AT SCREEN INTO l_wa_screen.
        CHECK ( l_wa_screen-group2 <> 'INP' )
          AND ( l_wa_screen-group2 <> 'EDT' ).

        l_wa_screen-input = '0'.
        MODIFY screen FROM l_wa_screen.
      ENDLOOP.

      l_readonly_mode = cl_gui_textedit=>true.
      l_no_authority = abap_true.

  ENDTRY.
  IF current_mode = mode_change.

* Switch to read-only mode if the node is already locked
    IF l_readonly_mode = cl_gui_textedit=>false.
      TRY.
          current_node->enqueue_node( ).
        CATCH /gal/cx_config_exception INTO l_exception.
          l_message = l_exception->get_text( ).
          MESSAGE l_message TYPE 'S'.

          LOOP AT SCREEN INTO l_wa_screen.
            CHECK ( l_wa_screen-group2 <> 'INP' )
              AND ( l_wa_screen-group2 <> 'EDT' )
              AND ( l_wa_screen-group2 <> 'DEL' ).

            l_wa_screen-input = '0'.
            MODIFY screen FROM l_wa_screen.
          ENDLOOP.

          l_readonly_mode = cl_gui_textedit=>true.

      ENDTRY.
    ENDIF.

    IF l_readonly_mode = cl_gui_textedit=>false.
      l_change_allowed = abap_true.
    ELSE.
      l_change_allowed = abap_false.
      set_active_mode( mode = mode_display ).
    ENDIF.

  ELSE.
    l_change_allowed = abap_true.
    l_readonly_mode = cl_gui_textedit=>true.

  ENDIF.

  IF l_change_allowed = abap_true.

    IF l_readonly_mode = cl_gui_textedit=>true.
      l_icon_name = icon_change.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name   = l_icon_name
          text   = TEXT-b01
          info   = TEXT-b01
        IMPORTING
          result = l_btn_change
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc IS NOT INITIAL.
        CONCATENATE l_icon_name TEXT-b01 INTO l_btn_change SEPARATED BY space.
      ENDIF.
    ELSE.
      l_icon_name = icon_display.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name   = l_icon_name
          text   = TEXT-b02
          info   = TEXT-b02
        IMPORTING
          result = l_btn_change
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc IS NOT INITIAL.
        CONCATENATE l_icon_name TEXT-b02 INTO l_btn_change SEPARATED BY space.
      ENDIF.
    ENDIF.

    set_ui_field_value( field_name  = 'G_DYNP_0120-BTN_CHANGE'
                        field_value = l_btn_change ).

    LOOP AT SCREEN INTO l_wa_screen.
      CHECK ( l_wa_screen-group2 <> 'INP' )
        AND ( l_wa_screen-group2 <> 'DEL' )
        AND ( l_wa_screen-group2 <> 'EDT' ).

      CASE l_readonly_mode.
        WHEN cl_gui_textedit=>false.
          l_wa_screen-input = '1'.
        WHEN cl_gui_textedit=>true.
          l_wa_screen-input = '0'.
      ENDCASE.
      MODIFY screen FROM l_wa_screen.
    ENDLOOP.

  ELSE.

    l_icon_name = icon_change.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name   = l_icon_name
        text   = TEXT-b01
        info   = TEXT-b01
      IMPORTING
        result = l_btn_change
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc IS NOT INITIAL.
      CONCATENATE l_icon_name TEXT-b01 INTO l_btn_change SEPARATED BY space.
    ENDIF.

    set_ui_field_value( field_name  = 'G_DYNP_0120-BTN_CHANGE'
                        field_value = l_btn_change ).

  ENDIF.

  docu_editor->set_readonly_mode( readonly_mode = l_readonly_mode ).

* Disable delete button if user is not authoprized to delete
  TRY.
      config_store->authority_check( node   = current_node
                                     action = /gal/config_node_actions=>delete_documentation ).

    CATCH /gal/cx_auth_check_exception.
      LOOP AT SCREEN INTO l_wa_screen.
        CHECK l_wa_screen-group2 = 'DEL'.

        l_wa_screen-input = '0'.
        MODIFY screen FROM l_wa_screen.
      ENDLOOP.

  ENDTRY.

* Deactivate Edit Button if user has not sufficient authorization
  IF l_no_authority IS NOT INITIAL.
    LOOP AT SCREEN INTO l_wa_screen.
      CHECK l_wa_screen-group2 = 'EDT'.

      l_wa_screen-input = 0.

      MODIFY screen FROM l_wa_screen.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


METHOD pbo_0130_initialize.
  DATA BEGIN OF l_wa_dropdown_values.
  DATA   key(12) TYPE c.
  DATA   text    TYPE string.
  DATA END OF l_wa_dropdown_values.

  DATA l_dropdown_values LIKE STANDARD TABLE OF l_wa_dropdown_values.

  DATA l_scope           LIKE current_value_scope.
  DATA l_client          LIKE current_value_client.
  DATA l_user            LIKE current_value_user.

  DATA l_exception       TYPE REF TO cx_root.
  DATA l_message         TYPE string.

  DATA l_readonly_mode   TYPE i.
  DATA l_change_allowed  TYPE abap_bool.
  DATA l_icon_name       TYPE iconname.
  DATA l_btn_change      TYPE string.

  DATA l_wa_screen       TYPE screen.
  DATA l_no_authority    TYPE abap_bool.


* Initialize client dropdown and set default client
  IF refresh_dropdown_0130 = abap_true.

    CLEAR l_dropdown_values.

    SELECT mandt
           mtext
      FROM t000
      INTO (l_client, l_wa_dropdown_values-text).

      TRY.
          config_store->authority_check( node   = current_node
                                         action = /gal/config_node_actions=>modify_value
                                         client = l_client ).

        CATCH /gal/cx_auth_check_exception.
          CONTINUE.

      ENDTRY.

      l_wa_dropdown_values-key = l_client.

      CONCATENATE l_wa_dropdown_values-key ` (` l_wa_dropdown_values-text `)`
             INTO l_wa_dropdown_values-text.

      INSERT l_wa_dropdown_values INTO TABLE l_dropdown_values.
    ENDSELECT.                                            "#EC CI_SUBRC

    TRY.
        /gal/dropdown_helper=>init_by_value_table( field_name        = `G_DYNP_0130-CLIENT`
                                                   value_table       = l_dropdown_values
                                                   set_default_value = abap_true ).

      CATCH /gal/cx_dd_helper_exception INTO l_exception.
        l_message = l_exception->get_text( ).

        MESSAGE l_message TYPE 'S'.

    ENDTRY.

    set_ui_field_value( field_name  = 'G_DYNP_0130-CLIENT'
                        field_value = current_value_client ).

    set_ui_field_value( field_name  = 'G_DYNP_0130-USER'
                        field_value = current_value_user ).

    refresh_dropdown_0130 = abap_false.
  ENDIF.

  get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0130-CLIENT'
                      IMPORTING field_value = l_client ).

* Initialize scope dropdown
  CLEAR l_dropdown_values.

  CASE current_node->type.
    WHEN /gal/config_node=>const_node_type_value_client.
      l_wa_dropdown_values-key  = 'C'.
      l_wa_dropdown_values-text = TEXT-s01.
      INSERT l_wa_dropdown_values INTO TABLE l_dropdown_values.

    WHEN /gal/config_node=>const_node_type_value_system.
      l_wa_dropdown_values-key  = 'S'.
      l_wa_dropdown_values-text = TEXT-s03.
      INSERT l_wa_dropdown_values INTO TABLE l_dropdown_values.

    WHEN /gal/config_node=>const_node_type_value_user.
      l_wa_dropdown_values-key  = 'U'.
      l_wa_dropdown_values-text = TEXT-s04.
      INSERT l_wa_dropdown_values INTO TABLE l_dropdown_values.

  ENDCASE.

  l_wa_dropdown_values-key  = 'D'.
  l_wa_dropdown_values-text = TEXT-s02.
  INSERT l_wa_dropdown_values INTO TABLE l_dropdown_values.

  TRY.
      /gal/dropdown_helper=>init_by_value_table( field_name        = `G_DYNP_0130-SCOPE`
                                                 value_table       = l_dropdown_values
                                                 set_default_value = abap_true ).

    CATCH /gal/cx_dd_helper_exception INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE l_message TYPE 'S'.

  ENDTRY.

  get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0130-SCOPE'
                      IMPORTING field_value = l_scope ).

* Initialize user dropdown
  IF current_node->type = /gal/config_node=>const_node_type_value_user.
    CLEAR l_dropdown_values.

    SELECT bname
      FROM usr02 CLIENT SPECIFIED
      INTO l_user                                        "#EC CI_CLIENT
     WHERE mandt = l_client.                            "#EC CI_GENBUFF
      TRY.
          config_store->authority_check( node      = current_node
                                         action    = /gal/config_node_actions=>modify_value
                                         client    = l_client
                                         user_name = l_user ).

        CATCH /gal/cx_auth_check_exception.
          CONTINUE.

      ENDTRY.

      l_wa_dropdown_values-key  = l_user.
      l_wa_dropdown_values-text = get_user_name( l_user ).

      IF l_wa_dropdown_values-text <> l_wa_dropdown_values-key.
        CONCATENATE l_wa_dropdown_values-key ` (` l_wa_dropdown_values-text `)`
               INTO l_wa_dropdown_values-text.
      ENDIF.

      INSERT l_wa_dropdown_values INTO TABLE l_dropdown_values.
    ENDSELECT.                                            "#EC CI_SUBRC

    TRY.
        /gal/dropdown_helper=>init_by_value_table( field_name        = `G_DYNP_0130-USER`
                                                   value_table       = l_dropdown_values
                                                   set_default_value = abap_true ).

      CATCH /gal/cx_dd_helper_exception INTO l_exception.
        l_message = l_exception->get_text( ).

        MESSAGE l_message TYPE 'S'.

    ENDTRY.
  ENDIF.

  get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0130-USER'
                      IMPORTING field_value = l_user ).

* Set field visibility
  LOOP AT SCREEN INTO l_wa_screen.
    CHECK l_wa_screen-group1 IS NOT INITIAL.

    IF current_node->type = /gal/config_node=>const_node_type_value_client AND l_wa_screen-group1 NA 'C' OR
       current_node->type = /gal/config_node=>const_node_type_value_user   AND l_wa_screen-group1 NA 'U' OR
       current_node->type = /gal/config_node=>const_node_type_value_system OR
       l_scope = 'D'.

      l_wa_screen-input     = '0'.
      l_wa_screen-invisible = '1'.
      MODIFY screen FROM l_wa_screen.
    ENDIF.

  ENDLOOP.

* Refresh value after change of scope
  IF current_value_scope  <> l_scope OR
     current_value_client <> l_client OR
     current_value_user   <> l_user.

    current_value_scope  = l_scope.
    current_value_client = l_client.
    current_value_user   = l_user.

    get_node_value( ).
  ENDIF.

* Check user authorization
  l_readonly_mode = cl_gui_textedit=>false.
  l_no_authority = abap_false.

  TRY.
      config_store->authority_check( node      = current_node
                                     action    = /gal/config_node_actions=>modify_value
                                     client    = current_value_client
                                     user_name = current_value_user ).
      IF current_value_scope = 'D'.
        config_store->authority_check( node      = current_node
                                       action    = /gal/config_node_actions=>modify_default_value
                                       client    = current_value_client
                                       user_name = current_value_user ).
      ENDIF.

    CATCH /gal/cx_auth_check_exception.
      LOOP AT SCREEN INTO l_wa_screen.
        CHECK ( l_wa_screen-group2 <> 'INP' )
          AND ( l_wa_screen-group2 <> 'EDT' ).

        l_wa_screen-input = '0'.
        MODIFY screen FROM l_wa_screen.
      ENDLOOP.

      l_readonly_mode = cl_gui_textedit=>true.
      l_no_authority = abap_true.

  ENDTRY.

  IF current_mode = mode_change.

* Switch to read-only mode if the node is already locked
    IF l_readonly_mode = cl_gui_textedit=>false.
      TRY.
          current_node->enqueue_node( ).
        CATCH /gal/cx_config_exception INTO l_exception.
          l_message = l_exception->get_text( ).
          MESSAGE l_message TYPE 'S'.

          LOOP AT SCREEN INTO l_wa_screen.
            CHECK ( l_wa_screen-group2 <> 'INP' )
              AND ( l_wa_screen-group2 <> 'EDT' )
              AND ( l_wa_screen-group2 <> 'DEL' ).

            l_wa_screen-input = '0'.
            MODIFY screen FROM l_wa_screen.
          ENDLOOP.

          l_readonly_mode = cl_gui_textedit=>true.
      ENDTRY.
    ENDIF.

    IF l_readonly_mode = cl_gui_textedit=>false.
      l_change_allowed = abap_true.
    ELSE.
      l_change_allowed = abap_false.
      set_active_mode( mode = mode_display ).
    ENDIF.


  ELSE.
    l_change_allowed = abap_true.
    l_readonly_mode = cl_gui_textedit=>true.
  ENDIF.

  IF l_change_allowed = abap_true.

    IF l_readonly_mode = cl_gui_textedit=>true.
      l_icon_name = icon_change.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name   = l_icon_name
          text   = TEXT-b01
          info   = TEXT-b01
        IMPORTING
          result = l_btn_change
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc IS NOT INITIAL.
        CONCATENATE l_icon_name TEXT-b01 INTO l_btn_change SEPARATED BY space.
      ENDIF.
    ELSE.
      l_icon_name = icon_display.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name   = l_icon_name
          text   = TEXT-b02
          info   = TEXT-b02
        IMPORTING
          result = l_btn_change
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc IS NOT INITIAL.
        CONCATENATE l_icon_name TEXT-b02 INTO l_btn_change SEPARATED BY space.
      ENDIF.
    ENDIF.

    set_ui_field_value( field_name  = 'G_DYNP_0130-BTN_CHANGE'
                        field_value = l_btn_change ).

    LOOP AT SCREEN INTO l_wa_screen.
      CHECK ( l_wa_screen-group2 <> 'INP' )
        AND ( l_wa_screen-group2 <> 'DEL' )
        AND ( l_wa_screen-group2 <> 'EDT' ).

      CASE l_readonly_mode.
        WHEN cl_gui_textedit=>false.
          IF l_wa_screen-invisible <> '1'.
            l_wa_screen-input = '1'.
          ENDIF.
        WHEN cl_gui_textedit=>true.
          l_wa_screen-input = '0'.
      ENDCASE.
      MODIFY screen FROM l_wa_screen.
    ENDLOOP.
  ENDIF.

  IF l_readonly_mode = cl_gui_textedit=>true.

    l_icon_name = icon_change.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name   = l_icon_name
        text   = TEXT-b01
        info   = TEXT-b01
      IMPORTING
        result = l_btn_change
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc IS NOT INITIAL.
      CONCATENATE l_icon_name TEXT-b01 INTO l_btn_change SEPARATED BY space.
    ENDIF.

    set_ui_field_value( field_name  = 'G_DYNP_0130-BTN_CHANGE'
                        field_value = l_btn_change ).
  ENDIF.


* Disable delete button if no value exists
  LOOP AT SCREEN INTO l_wa_screen.
    CHECK l_wa_screen-group2 = 'DEL'.
    IF current_value_exists = abap_false.
      l_wa_screen-input = '0'.
    ELSE.
      l_wa_screen-input = '1'.
    ENDIF.
    MODIFY screen FROM l_wa_screen.
  ENDLOOP.

* Disable delete button if user is not authoprized to delete
  TRY.
      config_store->authority_check( node      = current_node
                                     action    = /gal/config_node_actions=>delete_value
                                     client    = current_value_client
                                     user_name = current_value_user ).
      IF current_value_scope = 'D'.
        config_store->authority_check( node      = current_node
                                       action    = /gal/config_node_actions=>delete_default_value
                                       client    = current_value_client
                                       user_name = current_value_user ).
      ENDIF.

    CATCH /gal/cx_auth_check_exception.
      LOOP AT SCREEN INTO l_wa_screen.
        CHECK l_wa_screen-group2 = 'DEL'.

        l_wa_screen-input = '0'.
        MODIFY screen FROM l_wa_screen.
      ENDLOOP.

  ENDTRY.

* Fixed value type must not be changed
  LOOP AT SCREEN INTO l_wa_screen.
    CHECK l_wa_screen-name = 'G_DYNP_0130-TYPE'
      AND current_node->fixed_value_type = current_value_type
      AND current_node->fixed_value_type IS NOT INITIAL.

    l_wa_screen-input = '0'.
    MODIFY screen FROM l_wa_screen.
  ENDLOOP.

* Check if value editor should be used or a dropdown box to choose the possible values from
  IF current_mode = mode_change.
    check_value_editor_usage(
      IMPORTING value_editor_used = value_editor_used ).
  ELSE.
* In Display mode always use the editor
    value_editor_used = abap_true.
  ENDIF.

* Set the subscreen for the value editor or value dropdown box
  CASE value_editor_used.
    WHEN abap_true.
      set_active_value_screen( dynpro = '0140' ).
    WHEN abap_false.
      set_active_value_screen( dynpro = '0150' ).
  ENDCASE.

* Disable XML button if dropdown box is used to change the value
  IF l_readonly_mode = cl_gui_textedit=>false.
    LOOP AT SCREEN INTO l_wa_screen.
      CHECK l_wa_screen-group2 = 'XML'.
      IF value_editor_used = abap_true.
        l_wa_screen-input = '1'.
      ELSE.
        l_wa_screen-input = '0'.
      ENDIF.
      MODIFY screen FROM l_wa_screen.
    ENDLOOP.
  ENDIF.

* Deactivate Edit Button if user has not sufficient authorization
  IF l_no_authority IS NOT INITIAL.
    LOOP AT SCREEN INTO l_wa_screen.
      CHECK l_wa_screen-group2 = 'EDT'.

      l_wa_screen-input = 0.

      MODIFY screen FROM l_wa_screen.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


  METHOD pbo_0140_initialize.

    DATA l_value TYPE string.

* Create documentation editor
    IF value_container IS INITIAL.

* Create containers
      CREATE OBJECT value_container
        EXPORTING
          container_name = 'VALUE_CONTAINER'.

* Create editor
      CREATE OBJECT value_editor
        EXPORTING
          parent = value_container.

      value_editor->set_toolbar_mode( cl_gui_textedit=>false ).
      value_editor->set_statusbar_mode( cl_gui_textedit=>false ).

      get_node_value( ).
      refresh_value_editor = abap_false.
    ENDIF.

    IF refresh_value_editor = abap_true.
      get_node_value( ).
      refresh_value_editor = abap_false.
    ENDIF.

    IF current_mode = mode_change.
      value_editor->get_textstream( IMPORTING text = l_value ).
      cl_gui_cfw=>flush( ).
      IF l_value = TEXT-c01.
        CLEAR l_value.
        value_editor->set_textstream( l_value ).
        cl_gui_cfw=>flush( ).
      ENDIF.

      value_editor->set_readonly_mode( readonly_mode = cl_gui_textedit=>false ).

    ELSE.
      value_editor->set_readonly_mode( readonly_mode = cl_gui_textedit=>true ).
    ENDIF.


  ENDMETHOD.


  METHOD pbo_0150_initialize.

    DATA BEGIN OF l_wa_dropdown_values.
    DATA   key(12) TYPE c.
    DATA   text    TYPE string.
    DATA END OF l_wa_dropdown_values.

    DATA l_dropdown_values LIKE STANDARD TABLE OF l_wa_dropdown_values.

    DATA l_type            TYPE /gal/config_value_type.
    DATA l_domain          TYPE domname.
    DATA l_data_element    TYPE rollname.
    DATA l_is_dtel         TYPE abap_bool.
    DATA l_is_domain       TYPE abap_bool.
    DATA l_is_abap_bool    TYPE abap_bool.

    DATA l_message         TYPE string.
    DATA l_exception       TYPE REF TO cx_root.

    get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0130-TYPE'
                        IMPORTING field_value = l_type ).

    IF ( l_type <> current_value_type ) OR ( refresh_dropdown_0150 = abap_true ).

      check_value_editor_usage(
        IMPORTING is_data_element = l_is_dtel
                  is_domain       = l_is_domain
                  is_abap_bool    = l_is_abap_bool
                  type            = l_type ).

      TRY.

          CASE abap_true.

            WHEN l_is_abap_bool.

              l_wa_dropdown_values-key = abap_false.
              l_wa_dropdown_values-text = 'ABAP_FALSE'.
              INSERT l_wa_dropdown_values INTO TABLE l_dropdown_values.

              l_wa_dropdown_values-key = abap_true.
              l_wa_dropdown_values-text = 'ABAP_TRUE'.
              INSERT l_wa_dropdown_values INTO TABLE l_dropdown_values.

              /gal/dropdown_helper=>init_by_value_table( field_name        = 'G_DYNP_0150-VALUE'
                                                         value_table       = l_dropdown_values
                                                         set_default_value = abap_false ).


            WHEN l_is_domain.
              l_domain = l_type.
              /gal/dropdown_helper=>init_by_domain( field_name        = 'G_DYNP_0150-VALUE'
                                                    domain            = l_domain
                                                    set_default_value = abap_false ).

            WHEN l_is_dtel.
              l_data_element = l_type.
              /gal/dropdown_helper=>init_by_data_element( field_name        = 'G_DYNP_0150-VALUE'
                                                          data_element      = l_data_element
                                                          set_default_value  = abap_false ).

          ENDCASE.

        CATCH /gal/cx_dd_helper_exception INTO l_exception.
          l_message = l_exception->get_text( ).

          MESSAGE l_message TYPE 'S'.
      ENDTRY.

      refresh_dropdown_0150 = abap_false.
    ENDIF.


  ENDMETHOD.


METHOD populate_tree.
  DATA l_node_key TYPE tm_nodekey.

* Initialize tree
  tree->delete_all_nodes( ).

* Add and expand root node
  l_node_key = root_node->id.

  tree->add_node( node_key    = l_node_key
                  isfolder    = abap_true
                  expander    = abap_true
                  text        = root_node->name
                  user_object = root_node ).

  handle_expand_no_children( l_node_key ).

* Set root node as current node
  selected_node = root_node.
  handle_user_command( user_command = 'SELECT' ).
ENDMETHOD.


METHOD record_client_values.
  DATA BEGIN OF l_key.
  DATA   client TYPE mandt.
  DATA   id     TYPE /gal/config_key_id.
  DATA END OF l_key.

  DATA l_keys LIKE STANDARD TABLE OF l_key.

  DATA l_node TYPE REF TO /gal/config_node.

  LOOP AT nodes INTO l_node WHERE table_line->type = /gal/config_node=>const_node_type_value_client. "#EC CI_STDSEQ
    l_key-client = sy-mandt.
    l_key-id     = l_node->id.
    INSERT l_key INTO TABLE l_keys.
  ENDLOOP.

  record_table_entries( table   = `/GAL/CONFIG_CVAL`
                        records = l_keys ).
ENDMETHOD.


METHOD record_default_values.
  DATA BEGIN OF l_key.
  DATA   id TYPE /gal/config_key_id.
  DATA END OF l_key.

  DATA l_keys LIKE STANDARD TABLE OF l_key.

  DATA l_node TYPE REF TO /gal/config_node.

  LOOP AT nodes INTO l_node WHERE table_line->is_value = abap_true. "#EC CI_STDSEQ
    l_key-id = l_node->id.
    INSERT l_key INTO TABLE l_keys.
  ENDLOOP.

  record_table_entries( table   = `/GAL/CONFIG_VAL`
                        records = l_keys ).
ENDMETHOD.


METHOD record_documentation.
  DATA BEGIN OF l_key.
  DATA   id    TYPE /gal/config_key_id.
  DATA   langu TYPE langu.
  DATA END OF l_key.

  DATA l_keys LIKE STANDARD TABLE OF l_key.

  DATA l_node TYPE REF TO /gal/config_node.

  LOOP AT nodes INTO l_node.
    l_key-id    = l_node->id.
    l_key-langu = '*'.
    INSERT l_key INTO TABLE l_keys.
  ENDLOOP.

  record_table_entries( table   = `/GAL/CONFIG_TXT`
                        records = l_keys ).

ENDMETHOD.


method RECORD_STRUCTURE.

    DATA BEGIN OF l_key.
    DATA   id TYPE /gal/config_key_id.
    DATA END OF l_key.

    DATA l_keys LIKE STANDARD TABLE OF l_key.

    DATA l_node TYPE REF TO /gal/config_node.

    LOOP AT nodes INTO l_node.
      l_key-id = l_node->id.
      INSERT l_key INTO TABLE l_keys.
    ENDLOOP.

    record_table_entries( table   = `/GAL/CONFIG_KEY`
                          records = l_keys ).

endmethod.


METHOD record_system_values.
  DATA BEGIN OF l_key.
  DATA   id TYPE /gal/config_key_id.
  DATA END OF l_key.

  DATA l_keys LIKE STANDARD TABLE OF l_key.

  DATA l_node TYPE REF TO /gal/config_node.

  LOOP AT nodes INTO l_node WHERE table_line->is_value = abap_true. "#EC CI_STDSEQ
    l_key-id = l_node->id.
    INSERT l_key INTO TABLE l_keys.
  ENDLOOP.

  record_table_entries( table   = `/GAL/CONFIG_SVAL`
                        records = l_keys ).
ENDMETHOD.


METHOD record_table_entries.
  DATA l_ko200     TYPE STANDARD TABLE OF ko200.
  DATA l_wa_ko200  TYPE ko200.

  DATA l_e071k     TYPE STANDARD TABLE OF e071k.
  DATA l_wa_e071k  TYPE e071k.

  DATA l_field_len TYPE ddleng.
  DATA l_key_len   TYPE i.

  DATA l_message   TYPE string.

  FIELD-SYMBOLS <l_record> TYPE any.

  SELECT leng FROM dd03l INTO l_field_len
                        WHERE tabname  = table
                          AND as4local = 'A'
                          AND keyflag  = 'X'.

    l_key_len = l_key_len + l_field_len.
  ENDSELECT.                                              "#EC CI_SUBRC

  l_wa_ko200-pgmid    = 'R3TR'.
  l_wa_ko200-object   = 'TABU'.
  l_wa_ko200-obj_name =  table.
  l_wa_ko200-objfunc  = 'K'.
  INSERT l_wa_ko200 INTO TABLE l_ko200.

  LOOP AT records ASSIGNING <l_record>.
    l_wa_e071k-pgmid             = 'R3TR'.
    l_wa_e071k-object            = 'TABU'.
    l_wa_e071k-objname           =  table.
    l_wa_e071k-mastertype        = 'TABU'.
    l_wa_e071k-mastername        =  table.
    l_wa_e071k-tabkey(l_key_len) =  <l_record>.
    l_wa_e071k-sortflag          = '2'.
    INSERT l_wa_e071k INTO TABLE l_e071k.
  ENDLOOP.

  CALL FUNCTION 'TR_OBJECTS_CHECK'
    TABLES
      wt_ko200 = l_ko200
      wt_e071k = l_e071k
    EXCEPTIONS
      OTHERS   = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>custom_exception
        var1   = l_message.
  ENDIF.

  CALL FUNCTION 'TR_OBJECTS_INSERT'
    TABLES
      wt_ko200 = l_ko200
      wt_e071k = l_e071k
    EXCEPTIONS
      OTHERS   = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>custom_exception
        var1   = l_message.
  ENDIF.

ENDMETHOD.


METHOD record_user_values.
  DATA BEGIN OF l_key.
  DATA   client TYPE mandt.
  DATA   id     TYPE /gal/config_key_id.
  DATA END OF l_key.

  DATA l_keys LIKE STANDARD TABLE OF l_key.

  DATA l_node TYPE REF TO /gal/config_node.

  LOOP AT nodes INTO l_node WHERE table_line->type = /gal/config_node=>const_node_type_value_client. "#EC CI_STDSEQ
    l_key-client = sy-mandt.
    l_key-id     = l_node->id.
    INSERT l_key INTO TABLE l_keys.
  ENDLOOP.

  record_table_entries( table   = `/GAL/CONFIG_UVAL`
                        records = l_keys ).
ENDMETHOD.


METHOD run.
  DATA l_exception TYPE REF TO cx_root.
  DATA l_message   TYPE string.

* Determine UI program
  ui_program = sy-cprog.

* Launch main program if run method was called from anywhere else
  IF ui_program <> '/GAL/CONFIG_EDITOR'.
    SUBMIT /gal/config_editor AND RETURN.                "#EC CI_SUBMIT
    RETURN.
  ENDIF.

* Authority check
  TRY.
      authority_check( ).

    CATCH /gal/cx_auth_check_exception INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE l_message TYPE 'I'.
      LEAVE PROGRAM.
  ENDTRY.

* Run application
  CREATE OBJECT config_store TYPE /gal/config_store_local.

  set_root_node( config_store->root ).
  call_screen( '0100' ).
  cleanup( ).
ENDMETHOD.


  METHOD set_active_mode.

    DATA l_exception TYPE REF TO cx_root.

    IF current_mode <> mode.

      current_mode = mode.

      TRY.
          IF current_mode = mode_display.
            IF current_node IS NOT INITIAL.

* Get node value (to show "Not Defined" Value if necessary)
              IF current_node->is_folder = abap_false.
                get_node_value( ).
              ENDIF.

* Dequeue node if mode is switched from change to display
              current_node->dequeue_node( ).
            ENDIF.
          ENDIF.

        CATCH /gal/cx_lock_exception INTO l_exception.
          /gal/trace=>write_exception( EXPORTING exception = l_exception ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.


METHOD set_active_tab.
  DATA l_dummy        TYPE string.                          "#EC NEEDED
  DATA l_dynpro       TYPE dynnr.
  DATA l_user_command TYPE sy-ucomm.

* Process user command
  l_user_command = user_command.
  SPLIT l_user_command AT '_' INTO l_dummy l_dynpro.

* Prompt to save unsaved changes
  check_node_definition_changes( ).
  check_node_docu_changes( ).
  check_node_value_changes( ).

* Set active tab
  current_tab_program = ui_program.
  current_tab_screen  = l_dynpro.

  set_ui_field_value( field_name  = 'G_TABSTRIP-ACTIVETAB'
                      field_value = l_user_command ).

ENDMETHOD.


  METHOD set_active_value_screen.

* Set active screen
    current_value_program = ui_program.
    current_value_screen  = dynpro.

  ENDMETHOD.


METHOD set_node_definition.
  DATA l_type            TYPE /gal/config_key_type.
  DATA l_fixed_type      TYPE /gal/config_value_type.
  DATA l_auth_class      TYPE /gal/config_auth_class_name.

  DATA l_authenticator   TYPE REF TO /gal/config_node_authenticator.

  DATA l_result          TYPE string.

  DATA l_exception       TYPE REF TO cx_root.
  DATA l_message         TYPE string.

  DATA l_node_key        TYPE tm_nodekey.
  DATA l_parent_node_key TYPE tm_nodekey.

  get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0110-TYPE'
                      IMPORTING field_value = l_type ).

  get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0110-FIXED_TYPE'
                      IMPORTING field_value = l_fixed_type ).

  get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0110-AUTH_CLASS'
                      IMPORTING field_value = l_auth_class ).

* Warning when changing node type (values will be deleted)
  IF l_type <> current_node->type.
    IF l_type <> /gal/config_node=>const_node_type_folder.
      l_result = /gal/common_dialog=>show_confirmation_dialog( title          = TEXT-t11
                                                               message        = TEXT-q09
                                                               style          = /gal/common_dialog=>dlg_style_yes_no
                                                               default_result = /gal/common_dialog=>dlg_result_no ).
      IF l_result <> /gal/common_dialog=>dlg_result_yes.
        RETURN.
      ENDIF.
    ELSE.
      l_result = /gal/common_dialog=>show_confirmation_dialog( title          = TEXT-t11
                                                               message        = TEXT-q08
                                                               style          = /gal/common_dialog=>dlg_style_ok ).
      set_ui_field_value( field_name  = 'G_DYNP_0110-TYPE'
                          field_value = current_node->type ).
      RETURN.
    ENDIF.
  ENDIF.

* Warning when changing fixed type
  IF l_fixed_type IS NOT INITIAL AND l_fixed_type <> current_node->fixed_value_type.
    TRY.
        current_node->get_value( ).

        l_result = /gal/common_dialog=>show_confirmation_dialog( title          = TEXT-t11
                                                                 message        = TEXT-q10
                                                                 style          = /gal/common_dialog=>dlg_style_ok ).

      CATCH /gal/cx_config_exception.                   "#EC NO_HANDLER
        "Warning not required because there no value is defined

    ENDTRY.
  ENDIF.

* Check if authenticator class exists  and if user is allowed to use it
  IF l_auth_class IS NOT INITIAL.
    TRY.
        CREATE OBJECT l_authenticator TYPE (l_auth_class).

        l_authenticator->authority_check( node   = current_node
                                          action = /gal/config_node_actions=>modify_node ).

      CATCH /gal/cx_auth_check_exception INTO l_exception.
        l_message = l_exception->get_text( ).
        l_message = /gal/string=>replace_variables( input = TEXT-i02
                                                    var01 = l_message ).

        MESSAGE l_message TYPE 'I'.
        RETURN.

      CATCH cx_static_check INTO l_exception.
        l_message = l_exception->get_text( ).
        l_message = /gal/string=>replace_variables( input = TEXT-i03
                                                    var01 = l_auth_class
                                                    var02 = l_message ).

        MESSAGE l_message TYPE 'I'.
        RETURN.

    ENDTRY.
  ENDIF.

* Update node definition
  TRY.
      current_node->set_type( type      = l_type
                              no_commit = abap_true ).

      current_node->set_fixed_value_type( fixed_value_type = l_fixed_type
                                          no_commit = abap_true ).

      current_node->set_authenticator_class( authenticator_class = l_auth_class ).

      MESSAGE TEXT-t18 TYPE 'S'.

    CATCH /gal/cx_config_exception INTO l_exception.
      l_message = l_exception->get_text( ).

      ROLLBACK WORK.                                   "#EC CI_ROLLBACK

      MESSAGE l_message TYPE 'I'.
      RETURN.

  ENDTRY.

* Force node refresh
  IF skip_refresh = abap_false.
    l_node_key        = current_node->id.
    l_parent_node_key = current_node->parent->id.

    handle_expand_no_children( l_parent_node_key ).

    selected_node = get_node( l_node_key ).

    handle_user_command( user_command = 'SELECT' ).
  ENDIF.
ENDMETHOD.


METHOD set_node_documentation.
  DATA l_documentation TYPE string.
  DATA l_is_modified   TYPE i.

  DATA l_exception     TYPE REF TO cx_root.
  DATA l_message       TYPE string.

  docu_editor->get_textstream( IMPORTING text        = l_documentation
                                         is_modified = l_is_modified ).

  cl_gui_cfw=>flush( ).

  IF l_is_modified = cl_gui_textedit=>true.
    TRY.
        current_node->set_description( language    = current_docu_language
                                       description = l_documentation ).

        MESSAGE TEXT-t19 TYPE 'S'.

      CATCH /gal/cx_config_exception INTO l_exception.
        l_message = l_exception->get_text( ).

        MESSAGE l_message TYPE 'I'.

    ENDTRY.
  ENDIF.

  get_node_documentation( ).
ENDMETHOD.


METHOD set_node_name.

  CONSTANTS lc_max_length TYPE i VALUE 60.

  DATA l_name        TYPE /gal/config_key_name.
  DATA l_parent_node TYPE REF TO /gal/config_node.
  DATA l_node_key    TYPE tm_nodekey.
  DATA l_node        TYPE REF TO /gal/config_node.
  DATA l_exception   TYPE REF TO cx_root.
  DATA l_child_nodes TYPE /gal/config_nodes.
  DATA l_message     TYPE string.
  DATA l_result      TYPE string.

  IF node IS BOUND.
    TRY.

        IF force = abap_false AND node->is_folder = abap_true.
          l_child_nodes = node->get_child_nodes( ).
          IF l_child_nodes IS NOT INITIAL.
            RAISE EXCEPTION TYPE /gal/cx_config_exception
              EXPORTING
                textid = /gal/cx_config_exception=>node_cannot_renamed
                var1   = node->name.
          ENDIF.
        ENDIF.

* Warning when name is changed (active implementations has to be adjusted)
        l_result = /gal/common_dialog=>show_confirmation_dialog( title          = TEXT-t16
                                                                 message        = TEXT-q13
                                                                 style          = /gal/common_dialog=>dlg_style_yes_no
                                                                 default_result = /gal/common_dialog=>dlg_result_no ).
        IF l_result <> /gal/common_dialog=>dlg_result_yes.
          node->dequeue_node( ).
          RETURN.
        ENDIF.

* Record deleted node(s) in transport
        l_result = /gal/common_dialog=>show_confirmation_dialog( title          = TEXT-t12
                                                                 message        = TEXT-q11
                                                                 style          = /gal/common_dialog=>dlg_style_yes_no_cancel
                                                                 default_result = /gal/common_dialog=>dlg_result_cancel ).
        IF l_result = /gal/common_dialog=>dlg_result_yes.
          transport_subtree( EXPORTING node    = node
                             IMPORTING message = l_message ).

          IF l_message IS NOT INITIAL.
            MESSAGE l_message TYPE 'I'.
            node->dequeue_node( ).
            RETURN.
          ENDIF.
        ELSEIF l_result = /gal/common_dialog=>dlg_result_cancel.
          node->dequeue_node( ).
          RETURN.
        ENDIF.

* Prompt for node name
        IF node->type = /gal/config_node=>const_node_type_folder.
          /gal/common_dialog=>show_input_dialog( EXPORTING title            = TEXT-t14
                                                           prompt           = TEXT-p01
                                                           max_length       = lc_max_length
                                                           can_be_cancelled = abap_true
                                                 CHANGING  value            = l_name ).
        ELSE.
          /gal/common_dialog=>show_input_dialog( EXPORTING title            = TEXT-t15
                                                           prompt           = TEXT-p02
                                                           max_length       = lc_max_length
                                                           can_be_cancelled = abap_true
                                                 CHANGING  value            = l_name ).
        ENDIF.

        l_parent_node = node->parent.
        node->set_name( name = l_name ).
        node->dequeue_node( ).

* Update children of parent node
        l_node_key = l_parent_node->id.
        handle_expand_no_children( l_node_key ).

* Select new node
        l_node = l_parent_node->get_child_node( l_name ).
        l_node_key = l_node->id.
        tree->set_selected_node( l_node_key ).

        selected_node = l_node.

        handle_user_command( user_command = 'SELECT' ).

      CATCH /gal/cx_config_exception
            /gal/cx_dialog_exception INTO l_exception.
        l_message = l_exception->get_text( ).

        MESSAGE l_message TYPE 'S'.
    ENDTRY.
  ENDIF.

ENDMETHOD.


METHOD set_node_value.
  DATA l_value     TYPE string.

  DATA l_exception TYPE REF TO cx_root.
  DATA l_message   TYPE string.
  DATA l_error     TYPE abap_bool.

* Check if there is an editor, otherwise no value can be displayed
  IF ( value_editor IS INITIAL ) AND ( value_editor_used = abap_true ).
    RETURN.
  ENDIF.

* Get text from editor
  IF value_editor_used = abap_true.
    value_editor->get_textstream( IMPORTING text = l_value ).
    cl_gui_cfw=>flush( ).

* Convert value to external presentation if needed
    l_value = value_conversion_input_output( value_input         = l_value
                                             convert_to_internal = abap_true ).
  ELSE.
    get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0150-VALUE'
                        IMPORTING field_value = l_value ).
  ENDIF.

* Update configuration node
  TRY.
      CLEAR l_error.

      IF current_value_scope = 'D'.
        current_node->set_value_raw( default   = abap_true
                                     type      = current_value_type
                                     value_raw = l_value ).
      ELSE.
        current_node->set_value_raw( client    = current_value_client
                                     user_name = current_value_user
                                     type      = current_value_type
                                     value_raw = l_value ).
      ENDIF.

      MESSAGE TEXT-t17 TYPE 'S'.

    CATCH /gal/cx_config_exception INTO l_exception.
      l_message = l_exception->get_text( ).
      l_error = abap_true.

      MESSAGE l_message TYPE 'I'.

  ENDTRY.

  IF l_error IS INITIAL.
* Update editor (reflect formatting changes)
    get_node_value( ).

* Set flags to refresh dynpro components
    refresh_dropdown_0150 = abap_true.
    refresh_value_editor = abap_true.
  ENDIF.

ENDMETHOD.


method SET_ROOT_NODE.

    root_node = node.

endmethod.


METHOD set_ui_field_value.
  DATA l_field_name TYPE string.

  FIELD-SYMBOLS <l_field_value> TYPE any.

  CONCATENATE `(` ui_program `)` field_name INTO l_field_name.
  ASSIGN (l_field_name) TO <l_field_value>.

  IF sy-subrc = 0.
    <l_field_value> = field_value.
  ENDIF.
ENDMETHOD.


METHOD transport_subtree.
  DATA l_options        TYPE /gal/cdlg_options.
  DATA l_wa_options     LIKE LINE OF l_options.

  DATA l_node           TYPE REF TO /gal/config_node.
  DATA l_all            TYPE /gal/config_nodes.
  DATA l_parent         TYPE /gal/config_nodes.
  DATA l_children       TYPE /gal/config_nodes.

  DATA l_full_structure TYPE abap_bool.

  DATA l_exception      TYPE REF TO cx_root.
  DATA l_message        TYPE string.

  FIELD-SYMBOLS <l_options> LIKE LINE OF l_options.

* Clear message
  CLEAR message.

* Collect nodes
  TRY.
      l_node = node.

      WHILE l_node->parent IS NOT INITIAL.
        l_node = l_node->parent.
        INSERT l_node INTO TABLE l_parent.
      ENDWHILE.

      collect_children( EXPORTING node  = node
                        CHANGING  nodes = l_children ).

      INSERT node INTO TABLE l_children.

      INSERT LINES OF l_parent   INTO TABLE l_all.
      INSERT LINES OF l_children INTO TABLE l_all.

* Prompt for scope of transport
      SORT l_children BY table_line->type.

      l_wa_options-id          = `01`.
      l_wa_options-is_selected = abap_true.
      l_wa_options-text        = TEXT-o01.
      INSERT l_wa_options INTO TABLE l_options.

      l_wa_options-id          = `02`.
      l_wa_options-is_selected = abap_true.
      l_wa_options-text        = TEXT-o02.
      INSERT l_wa_options INTO TABLE l_options.

      LOOP AT l_children TRANSPORTING NO FIELDS
           WHERE table_line->type <> /gal/config_node=>const_node_type_folder. "#EC CI_STDSEQ
        EXIT.
      ENDLOOP.

      IF sy-subrc = 0.
        l_wa_options-id          = `03`.
        l_wa_options-is_selected = abap_true.
        l_wa_options-text        = TEXT-o03.
        INSERT l_wa_options INTO TABLE l_options.
      ENDIF.

      READ TABLE l_children
            WITH KEY table_line->type = /gal/config_node=>const_node_type_value_system
                 TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc = 0.
        l_wa_options-id          = `04`.
        l_wa_options-is_selected = abap_false.
        l_wa_options-text        = TEXT-o04.
        INSERT l_wa_options INTO TABLE l_options.
      ENDIF.

      READ TABLE l_children
            WITH KEY table_line->type = /gal/config_node=>const_node_type_value_client
                 TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc = 0.
        l_wa_options-id          = `05`.
        l_wa_options-is_selected = abap_false.
        l_wa_options-text        = TEXT-o05.
        INSERT l_wa_options INTO TABLE l_options.
      ENDIF.

      READ TABLE l_children
            WITH KEY table_line->type = /gal/config_node=>const_node_type_value_user
                 TRANSPORTING NO FIELDS BINARY SEARCH.   "#EC CI_STDSEQ
      IF sy-subrc = 0.
        l_wa_options-id          = `06`.
        l_wa_options-is_selected = abap_false.
        l_wa_options-text        = TEXT-o06.
        INSERT l_wa_options INTO TABLE l_options.
      ENDIF.

      /gal/common_dialog=>show_options_dialog( EXPORTING title            = TEXT-t09
                                                         message          = TEXT-p03
                                                         can_be_cancelled = abap_true
                                               CHANGING  options          = l_options ).

      READ TABLE l_options WITH KEY id = `01` ASSIGNING <l_options> BINARY SEARCH.
      IF sy-subrc = 0 AND <l_options>-is_selected = abap_true.
        l_full_structure = abap_true.

        record_structure( l_all ).
      ENDIF.

      READ TABLE l_options WITH KEY id = `02` ASSIGNING <l_options> BINARY SEARCH.
      IF sy-subrc = 0 AND <l_options>-is_selected = abap_true.
        IF l_full_structure = abap_true.
          record_documentation( l_all ).
        ELSE.
          record_documentation( l_children ).
        ENDIF.
      ENDIF.

      READ TABLE l_options WITH KEY id = `03` ASSIGNING <l_options> BINARY SEARCH.
      IF sy-subrc = 0 AND <l_options>-is_selected = abap_true.
        record_default_values( l_children ).
      ENDIF.

      READ TABLE l_options WITH KEY id = `04` ASSIGNING <l_options> BINARY SEARCH.
      IF sy-subrc = 0 AND <l_options>-is_selected = abap_true.
        record_system_values( l_children ).
      ENDIF.

      READ TABLE l_options WITH KEY id = `05` ASSIGNING <l_options> BINARY SEARCH.
      IF sy-subrc = 0 AND <l_options>-is_selected = abap_true.
        record_client_values( l_children ).
      ENDIF.

      READ TABLE l_options WITH KEY id = `06` ASSIGNING <l_options> BINARY SEARCH.
      IF sy-subrc = 0 AND <l_options>-is_selected = abap_true.
        record_user_values( l_children ).
      ENDIF.

    CATCH /gal/cx_config_exception
          /gal/cx_dialog_exception INTO l_exception.

      message = l_exception->get_text( ).

      IF message IS NOT REQUESTED.
        MESSAGE l_message TYPE 'S'.
      ENDIF.

  ENDTRY.

  node->dequeue_node( ).

ENDMETHOD.


  METHOD value_conversion_input_output.

    DATA l_type            TYPE /gal/config_value_type.
    DATA l_value           TYPE string.
    DATA l_is_domain       TYPE abap_bool.
    DATA l_is_data_element TYPE abap_bool.
    DATA l_domain          TYPE domname.
    DATA l_data_element    TYPE rollname.
    DATA l_vrm_values      TYPE vrm_values.
    DATA l_vrm_value       TYPE vrm_value.
    DATA l_exception       TYPE REF TO cx_root.
    DATA l_value_bool      TYPE abap_bool.

    l_value = value_input.
    value_output = value_input.

    IF convert_to_internal = abap_true.
      get_ui_field_value( EXPORTING field_name  = 'G_DYNP_0130-TYPE'
                          IMPORTING field_value = l_type ).
    ELSE.
      l_type = current_value_type.
    ENDIF.

* Value converson for type = abap_bool
    IF l_type = 'ABAP_BOOL' OR l_type CS '\TYPE=ABAP_BOOL'.

      l_value_bool = l_value.
      CASE convert_to_internal.
        WHEN abap_true.
          TRANSLATE: l_type TO UPPER CASE, l_value TO UPPER CASE.
          IF ( l_value_bool <> abap_true ) AND ( l_value_bool <> abap_false ).

            IF l_value CS 'TRUE'.
              value_output = abap_true.
            ELSEIF l_value CS 'FALSE'.
              value_output = abap_false.
            ELSE.
              value_output = abap_true.
            ENDIF.
          ENDIF.
        WHEN abap_false.
          IF l_value_bool = abap_true.
            value_output = 'ABAP_TRUE'.
          ELSE.
            value_output = 'ABAP_FALSE'.
          ENDIF.
      ENDCASE.

    ELSE.

      check_value_editor_usage(
        EXPORTING value_type      = l_type
        IMPORTING is_data_element = l_is_data_element
                  is_domain       = l_is_domain ).
      TRY.
          CASE abap_true.
            WHEN l_is_data_element.
              l_data_element = l_type.
              l_vrm_values = /gal/dropdown_helper=>init_by_data_element( field_name         = 'G_DYNP_0150-VALUE'
                                                                         data_element       = l_data_element
                                                                         set_default_value  = abap_false ).
            WHEN l_is_domain.
              l_domain = l_type.
              l_vrm_values = /gal/dropdown_helper=>init_by_domain( field_name        = 'G_DYNP_0150-VALUE'
                                                                   domain            = l_domain
                                                                   set_default_value = abap_false ).
          ENDCASE.

          IF l_vrm_values IS NOT INITIAL.
            CASE convert_to_internal.
              WHEN abap_true.
                READ TABLE l_vrm_values INTO l_vrm_value
                  WITH KEY text = value_input.              "#EC WARNOK
                IF sy-subrc IS INITIAL.
                  value_output = l_vrm_value-key.
                ENDIF.
              WHEN abap_false.
                READ TABLE l_vrm_values INTO l_vrm_value
                  WITH KEY key = value_input.               "#EC WARNOK
                IF sy-subrc IS INITIAL.
                  value_output = l_vrm_value-text.
                ENDIF.
            ENDCASE.
          ENDIF.
        CATCH /gal/cx_dd_helper_exception
           INTO l_exception.
          /gal/trace=>write_exception( EXPORTING exception = l_exception ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
