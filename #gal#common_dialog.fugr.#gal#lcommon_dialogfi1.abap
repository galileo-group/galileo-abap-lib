*----------------------------------------------------------------------*
***INCLUDE /GAL/LCOMMON_DIALOGFI1 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  GET_OPTIONS_500
*&---------------------------------------------------------------------*
*       Get selected options
*----------------------------------------------------------------------*
FORM get_options_500.
  PERFORM get_options USING g_dynp_0500-options
                            `G_DYNP_0500-OPTION`.
ENDFORM.                    " GET_OPTIONS_500

*&---------------------------------------------------------------------*
*&      Form  GET_OPTIONS_501
*&---------------------------------------------------------------------*
*       Get selected options
*----------------------------------------------------------------------*
FORM get_options_501.
  PERFORM get_options USING g_dynp_0501-options
                            `G_DYNP_0501-OPTION`.
ENDFORM.                    " GET_OPTIONS_501

*&---------------------------------------------------------------------*
*&      Form  GET_VALUE_0400
*&---------------------------------------------------------------------*
*       Implementation of PAI Module GET_VALUE_0400
*----------------------------------------------------------------------*
FORM get_value_0400.
  DATA l_length TYPE i.

  FIELD-SYMBOLS <l_value> TYPE any.

  IF g_dynp_0400-max_length > 0.
    l_length = strlen( g_dynp_0400-input ).

    IF l_length > g_dynp_0400-max_length.
      MESSAGE TEXT-e01 TYPE 'E'.
    ENDIF.
  ENDIF.

  ASSIGN g_dynp_0400-value->* TO <l_value>.
  <l_value> = g_dynp_0400-input.
ENDFORM.                    " GET_VALUE_0400

*&---------------------------------------------------------------------*
*&      Form  GET_VALUE_0401
*&---------------------------------------------------------------------*
*       Implementation of PAI Module GET_VALUE_0401
*----------------------------------------------------------------------*
FORM get_value_0401.
  DATA l_length TYPE i.

  FIELD-SYMBOLS <l_value> TYPE any.

  IF g_dynp_0401-max_length > 0.
    l_length = strlen( g_dynp_0401-input ).

    IF l_length > g_dynp_0401-max_length.
      MESSAGE TEXT-e01 TYPE 'E'.
    ENDIF.
  ENDIF.

  ASSIGN g_dynp_0401-value->* TO <l_value>.
  <l_value> = g_dynp_0401-input.
ENDFORM.                    " GET_VALUE_0401

*&---------------------------------------------------------------------*
*&      Form  PROCESS_USER_COMMAND_01XX
*&---------------------------------------------------------------------*
*       Close dialog when a button has been pressed
*----------------------------------------------------------------------*
FORM process_user_command_01xx.
  FIELD-SYMBOLS <l_result> TYPE string.

  CHECK g_dynp_01xx-user_command IS NOT INITIAL.

  READ TABLE g_dynp_01xx-allowed_results
        WITH KEY table_line = g_dynp_01xx-user_command
             ASSIGNING <l_result>.                       "#EC CI_STDSEQ

  IF sy-subrc <> 0.
    READ TABLE g_dynp_01xx-allowed_results
          WITH KEY table_line = g_dynp_01xx-default_result
               ASSIGNING <l_result>.                     "#EC CI_STDSEQ
  ENDIF.

  IF sy-subrc <> 0.
    READ TABLE g_dynp_01xx-allowed_results INDEX 1 ASSIGNING <l_result>.
  ENDIF.

  IF <l_result> IS ASSIGNED.
    g_dynp_01xx-result = <l_result>.
  ELSE.
    g_dynp_01xx-result = g_dynp_01xx-user_command.
  ENDIF.

  LEAVE TO SCREEN 0.
ENDFORM.                    " PROCESS_USER_COMMAND_01XX

*&---------------------------------------------------------------------*
*&      Form  PROCESS_USER_COMMAND_2000
*&---------------------------------------------------------------------*
*       Handle user command of backend file browser
*----------------------------------------------------------------------*
FORM process_user_command_2000.
  DATA l_selected_file         TYPE string.
  DATA l_filter                TYPE string.

  DATA l_is_existing_file      TYPE abap_bool.
  DATA l_is_existing_directory TYPE abap_bool.

  DATA l_result                TYPE string.

  DATA l_exception             TYPE REF TO cx_root.
  DATA l_message               TYPE string.

  CASE sy-ucomm.

    WHEN 'OPEN' OR 'SAVE' OR 'ENTER'.
      IF g_dynp_2000-selected_file IS INITIAL.
        RETURN.
      ENDIF.

* Get current tree selection (if any)
      TRY.
          l_selected_file = g_dynp_2000-backend_file_browser->get_selected_file( ).

        CATCH cx_root.
      ENDTRY.

      TRY.

* Check file and update selection
          g_dynp_2000-backend_file_browser->set_selected_file( EXPORTING selected_file         = g_dynp_2000-selected_file
                                                               IMPORTING is_existing_file      = l_is_existing_file
                                                                         is_existing_directory = l_is_existing_directory ).

* Continue if only directory has been selected
          IF l_is_existing_directory = abap_true.
            RETURN.
          ENDIF.

* Check file selection
          IF g_dynp_2000-access = /gal/backend_file=>access_read.
            IF l_is_existing_file = abap_false.
              RAISE EXCEPTION TYPE /gal/cx_io_exception
                EXPORTING
                  textid = /gal/cx_io_exception=>file_does_not_exist
                  var1   = g_dynp_2000-selected_file.
            ENDIF.
          ELSE.
            IF l_is_existing_file = abap_true.
              /gal/common_dialog=>show_confirmation_dialog( EXPORTING title          = TEXT-t02
                                                                      message        = TEXT-m00
                                                                      message_var1   = g_dynp_2000-selected_file
                                                                      style          = /gal/common_dialog=>dlg_style_yes_no
                                                                      default_result = /gal/common_dialog=>dlg_result_no
                                                            RECEIVING result         = l_result ).
              IF l_result = /gal/common_dialog=>dlg_result_no.
                RETURN.
              ENDIF.
            ENDIF.
          ENDIF.

        CATCH cx_root INTO l_exception.
          l_message = l_exception->get_text( ).
          MESSAGE l_message TYPE 'E'.

      ENDTRY.

* Do not continue on enter after manual path entry
      IF sy-ucomm = 'ENTER' AND l_selected_file <> g_dynp_2000-selected_file.
        RETURN.
      ENDIF.

      LEAVE TO SCREEN 0.

    WHEN 'SELECT'.
      TRY.
          g_dynp_2000-selected_file = g_dynp_2000-backend_file_browser->get_selected_file( ).

        CATCH cx_root INTO l_exception.
          l_message = l_exception->get_text( ).
          MESSAGE l_message TYPE 'E'.

      ENDTRY.

    WHEN 'SELECT_DIR'.
      CLEAR g_dynp_2000-selected_file.

    WHEN 'TYPE_CHG'.
      l_filter = g_dynp_2000-file_type.

      g_dynp_2000-backend_file_browser->set_filter( l_filter ).

  ENDCASE.
ENDFORM.                    "process_user_command_2000

*&---------------------------------------------------------------------*
*&      Form  PROCESS_USER_COMMAND_3000
*&---------------------------------------------------------------------*
*       Handle user command of choose folder dialog
*----------------------------------------------------------------------*
FORM process_user_command_3000.
  IF g_dynp_3000-folder_tree IS NOT INITIAL.
    g_dynp_3000-folder_tree->handle_user_command( user_command = sy-ucomm ).
  ENDIF.
ENDFORM.                    "process_user_command_3000
