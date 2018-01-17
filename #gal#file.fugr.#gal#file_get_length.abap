FUNCTION /gal/file_get_length.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(FULL_NAME) TYPE  STRING
*"  EXPORTING
*"     REFERENCE(LENGTH) TYPE  I
*"  EXCEPTIONS
*"      CANNOT_GET_LENGTH
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA l_auth_fname TYPE fileextern.
  DATA l_message    TYPE string.
  DATA l_exception  TYPE REF TO cx_root.

* Initialize result
  CLEAR length.

* Follow RFC route
  cfw_follow_rfc_route rfc_route_info.

  cfw_pass_exception cannot_get_length.
  cfw_pass_exception rfc_exception.

  cfw_remote_coding.

* Check authorization
  l_auth_fname = full_name.

  CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
    EXPORTING
      activity = sabc_act_read
      filename = l_auth_fname
    EXCEPTIONS
      OTHERS   = 1.
  IF sy-subrc <> 0.
    MESSAGE e001 WITH full_name RAISING cannot_get_length.
  ENDIF.

* Get file length
  TRY.
      OPEN DATASET full_name FOR INPUT IN BINARY MODE MESSAGE l_message.
      IF sy-subrc <> 0.
        MESSAGE e002 WITH full_name l_message RAISING cannot_get_length.
      ENDIF.

      SET DATASET full_name POSITION END OF FILE.

      GET DATASET full_name POSITION length.

      CLOSE DATASET full_name.

    CATCH cx_sy_file_access_error INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE e003 WITH full_name l_message RAISING cannot_get_length.

    CLEANUP.
      CLOSE DATASET full_name.

  ENDTRY.
ENDFUNCTION.
