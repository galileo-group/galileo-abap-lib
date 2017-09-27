FUNCTION /gal/file_get_dir_separator .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"  EXPORTING
*"     REFERENCE(SEPARATOR) TYPE  STRING
*"     REFERENCE(CASE_SENSISTIVE_PATH) TYPE  ABAP_BOOL
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.

  IF sy-opsys CS 'WIN'.
    separator            = '\'.
    case_sensistive_path = abap_false.
  ELSE.
    separator            = '/'.
    case_sensistive_path = abap_true.
  ENDIF.
ENDFUNCTION.
