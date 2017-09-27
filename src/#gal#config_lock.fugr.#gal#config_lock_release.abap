FUNCTION /gal/config_lock_release.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(ID) TYPE  /GAL/CONFIG_KEY_ID
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.

  CALL FUNCTION 'DEQUEUE_/GAL/ECONF_LOCK'
    EXPORTING
      id = id.


ENDFUNCTION.
