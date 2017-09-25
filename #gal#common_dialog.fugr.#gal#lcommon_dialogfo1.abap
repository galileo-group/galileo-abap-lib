*----------------------------------------------------------------------*
***INCLUDE /GAL/LCOMMON_DIALOGFO1.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_0200
*&---------------------------------------------------------------------*
*       Implementation of PBO Module INITIALIZE_0200
*----------------------------------------------------------------------*
FORM initialize_0200.
  DATA l_lines_used TYPE i.

  PERFORM format_text USING g_dynp_0200-message `G_DYNP_0200-MESSAGE` 95
                   CHANGING l_lines_used.
ENDFORM.                    " INITIALIZE_0200

*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_0400
*&---------------------------------------------------------------------*
*       Implementation of PBO Module INITIALIZE_0400
*----------------------------------------------------------------------*
FORM initialize_0400.
  DATA l_wa_screen TYPE screen.

  FIELD-SYMBOLS <l_value> TYPE any.

  LOOP AT SCREEN INTO l_wa_screen.
    IF l_wa_screen-name = 'G_DYNP_0400-INPUT' AND g_dynp_0400-is_input_required = abap_true.
      l_wa_screen-required = '1'.
    ENDIF.

    MODIFY screen FROM l_wa_screen.
  ENDLOOP.

  IF NOT ( g_dynp_0400-is_initialized = abap_false ).
    RETURN.
  ENDIF.

  ASSIGN g_dynp_0400-value->* TO <l_value>.

  g_dynp_0400-input          = <l_value>.
  g_dynp_0400-is_initialized = abap_true.

  SET CURSOR FIELD 'G_DYNP_0400-INPUT'.
ENDFORM.                    " INITIALIZE_0400

*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_0401
*&---------------------------------------------------------------------*
*       Implementation of PBO Module INITIALIZE_0401
*----------------------------------------------------------------------*
FORM initialize_0401.
  DATA l_wa_screen TYPE screen.

  FIELD-SYMBOLS <l_value> TYPE any.

  LOOP AT SCREEN INTO l_wa_screen.
    IF l_wa_screen-name = 'G_DYNP_0401-INPUT' AND g_dynp_0401-is_input_required = abap_true.
      l_wa_screen-required = '1'.
    ENDIF.

    MODIFY screen FROM l_wa_screen.
  ENDLOOP.

  IF NOT ( g_dynp_0401-is_initialized = abap_false ).
    RETURN.
  ENDIF.

  ASSIGN g_dynp_0401-value->* TO <l_value>.

  g_dynp_0401-input          = <l_value>.
  g_dynp_0401-is_initialized = abap_true.

  SET CURSOR FIELD 'G_DYNP_0401-INPUT'.
ENDFORM.                    " INITIALIZE_0401

*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_0500
*&---------------------------------------------------------------------*
*       Implementation of PBO Module INITIALIZE_0500
*----------------------------------------------------------------------*
FORM initialize_0500.
  DATA l_lines_used TYPE i.

  PERFORM initialize_options USING g_dynp_0500-options
                                   `G_DYNP_0500-OPTION`
                                   `G_DYNP_0500-LABEL`
                          CHANGING l_lines_used.
ENDFORM.                    " INITIALIZE_0500

*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_0501
*&---------------------------------------------------------------------*
*       Implementation of PBO Module INITIALIZE_0501
*----------------------------------------------------------------------*
FORM initialize_0501.
  DATA l_index      TYPE i.
  DATA l_lines_used TYPE i.

  FIELD-SYMBOLS <l_option> LIKE LINE OF g_dynp_0501-options.

* Make sure that exactly one radiobutton is selected
  READ TABLE g_dynp_0501-options
        WITH KEY is_selected = abap_true
             TRANSPORTING NO FIELDS.                     "#EC CI_STDSEQ
  IF sy-subrc = 0.
    l_index = sy-tabix + 1.

    LOOP AT g_dynp_0501-options FROM l_index ASSIGNING <l_option>.
      <l_option>-is_selected = abap_false.
    ENDLOOP.
  ELSE.
    READ TABLE g_dynp_0501-options INDEX 1 ASSIGNING <l_option>.

    <l_option>-is_selected = abap_true.
  ENDIF.

* Initialize options
  PERFORM initialize_options USING g_dynp_0501-options
                                   `G_DYNP_0501-OPTION`
                                   `G_DYNP_0501-LABEL`
                          CHANGING l_lines_used.
ENDFORM.                    " INITIALIZE_0501

*&---------------------------------------------------------------------*
*&      Form  PBO_0100_INIT
*&---------------------------------------------------------------------*
*       Initialize backend file browser
*----------------------------------------------------------------------*
FORM initialize_2000.
  DATA l_filter        TYPE STANDARD TABLE OF string.
  DATA l_filter_values TYPE vrm_values.
  DATA l_filter_value  LIKE LINE OF l_filter_values.
  DATA l_filter_string TYPE string.

  DATA l_exception     TYPE REF TO cx_root.

  SET TITLEBAR 'DEFAULT' WITH g_dynp_2000-title.

  IF g_dynp_2000-access = /gal/backend_file=>access_read.
    SET PF-STATUS '2000_OPEN'.
  ELSE.
    SET PF-STATUS '2000_SAVE'.
  ENDIF.

  IF g_dynp_2000-backend_file_browser IS INITIAL.
    SPLIT g_dynp_2000-filter AT '|' INTO TABLE l_filter.

    LOOP AT l_filter INTO l_filter_value-key.
      IF l_filter_value-text IS INITIAL.
        l_filter_value-text = l_filter_value-key.
        CONTINUE.
      ENDIF.

      INSERT l_filter_value INTO TABLE l_filter_values.
      CLEAR l_filter_value.
    ENDLOOP.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = 'G_DYNP_2000-FILE_TYPE'
        values = l_filter_values
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    READ TABLE l_filter_values
          WITH KEY key = g_dynp_2000-file_type
               INTO l_filter_value.                      "#EC CI_STDSEQ
    IF sy-subrc <> 0.
      READ TABLE l_filter_values INDEX 1 INTO l_filter_value.
    ENDIF.

    g_dynp_2000-file_type = l_filter_value-key.

    CREATE OBJECT g_dynp_2000-container
      EXPORTING
        container_name = 'CC_FILE_BROWSER'
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      CREATE OBJECT g_dynp_2000-exception TYPE /gal/cx_control_exception
        EXPORTING
          textid = /gal/cx_control_exception=>cannot_create_container.

      LEAVE TO SCREEN 0.
    ENDIF.

    TRY.
        l_filter_string = g_dynp_2000-file_type.

        CREATE OBJECT g_dynp_2000-backend_file_browser
          EXPORTING
            container             = g_dynp_2000-container
            rfc_route_info        = g_dynp_2000-rfc_route_info
            base_directory        = g_dynp_2000-base_directory
            initial_directory     = g_dynp_2000-initial_directory
            initial_file          = g_dynp_2000-initial_file
            initial_filter        = l_filter_string
            file_click_fcode      = 'SELECT'
            directory_click_fcode = 'SELECT_DIR'.

      CATCH cx_root INTO l_exception.
        g_dynp_2000-exception = l_exception.
        LEAVE TO SCREEN 0.

    ENDTRY.

    TRY.
        g_dynp_2000-selected_file = g_dynp_2000-backend_file_browser->get_selected_file( ).

      CATCH cx_root.
    ENDTRY.
  ENDIF.
ENDFORM.                    "initialize_2000

*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_3000
*&---------------------------------------------------------------------*
*       Initialize choose folder dialog
*----------------------------------------------------------------------*
FORM initialize_3000.

  SET TITLEBAR 'DEFAULT' WITH g_dynp_3000-title.
  SET PF-STATUS '3000_DIALOG'.

  IF g_dynp_3000-folder_tree IS INITIAL.
    CREATE OBJECT g_dynp_3000-folder_tree
      EXPORTING
        container_name = 'TREE_CONTROL'
        store          = g_dynp_3000-store.
  ENDIF.

ENDFORM.                    "initialize_3000
