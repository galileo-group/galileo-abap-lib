FUNCTION /gal/js_db_delete.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(TABLE_NAME) TYPE  TABNAME
*"     REFERENCE(ID) TYPE  GUID_32
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------


  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.

  IF table_name NP '/GAL/*'.
    RETURN.
  ENDIF.

  DELETE FROM (table_name) WHERE id = id.                 "#EC CI_SUBRC
                                                         "#EC CI_DYNTAB

  COMMIT WORK.
ENDFUNCTION.
