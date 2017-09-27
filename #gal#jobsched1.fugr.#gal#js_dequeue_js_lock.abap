FUNCTION /gal/js_dequeue_js_lock.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(LOCK_KEY) TYPE  CHAR10
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.


  CALL FUNCTION 'DEQUEUE_/GAL/E_JS_LOCK'
    EXPORTING
      lock_key = lock_key
      _scope   = '1'.



ENDFUNCTION.
