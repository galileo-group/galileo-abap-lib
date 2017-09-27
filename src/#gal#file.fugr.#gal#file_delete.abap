FUNCTION /gal/file_delete.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(FULL_NAME) TYPE  STRING
*"  EXCEPTIONS
*"      CANNOT_DELETE_FILE
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA l_auth_fname LIKE authb-filename.
  DATA l_message    TYPE string.
  DATA l_exception  TYPE REF TO cx_root.

  cfw_follow_rfc_route rfc_route_info.

  cfw_pass_exception cannot_delete_file.
  cfw_pass_exception rfc_exception.

  cfw_remote_coding.

* Check authorization
  l_auth_fname = full_name.

  CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
    EXPORTING
      activity = sabc_act_delete
      filename = l_auth_fname
    EXCEPTIONS
      OTHERS   = 1.
  IF NOT sy-subrc = 0.
    MESSAGE e001 WITH full_name RAISING cannot_delete_file.
  ENDIF.

* Delete file
  TRY.
      DELETE DATASET full_name.

      IF sy-subrc <> 0.
        MESSAGE e012 WITH full_name RAISING cannot_delete_file.
      ENDIF.

    CATCH cx_root INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE e013 WITH full_name l_message RAISING cannot_delete_file.

  ENDTRY.
ENDFUNCTION.
