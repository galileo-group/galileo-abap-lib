FUNCTION /gal/config_lock_acquire.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(ID) TYPE  /GAL/CONFIG_KEY_ID
*"  EXCEPTIONS
*"      CANNOT_ACQUIRE_LOCK
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception cannot_acquire_lock.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.

  CALL FUNCTION 'ENQUEUE_/GAL/ECONF_LOCK'
    EXPORTING
      id           = id
    EXCEPTIONS
      foreign_lock = 1
      OTHERS       = 2.
  IF sy-subrc IS NOT INITIAL.
    cfw_pass_message cannot_acquire_lock.
  ENDIF.

ENDFUNCTION.
