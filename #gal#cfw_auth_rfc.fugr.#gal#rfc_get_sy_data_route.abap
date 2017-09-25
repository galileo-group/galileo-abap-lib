FUNCTION /gal/rfc_get_sy_data_route .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     VALUE(READ_CALLSTACK) TYPE  FLAG DEFAULT SPACE
*"     VALUE(GET_AUTH_ACTIVE_STATE) TYPE  FLAG DEFAULT SPACE
*"  EXPORTING
*"     VALUE(SYSYSID) TYPE  SYSYSID
*"     VALUE(SYMANDT) TYPE  SYMANDT
*"     VALUE(SYUNAME) TYPE  SYUNAME
*"     VALUE(CALLSTACK) TYPE  ABAP_CALLSTACK
*"     VALUE(AUTH_ACTIVE) TYPE  FLAG
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"      FRAMEWORK_EXCEPTION
*"----------------------------------------------------------------------

  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.

  CALL FUNCTION '/GAL/RFC_GET_SY_DATA'
    EXPORTING
      read_callstack        = read_callstack
      get_auth_active_state = get_auth_active_state
    IMPORTING
      sysysid               = sysysid
      symandt               = symandt
      syuname               = syuname
      callstack             = callstack
      auth_active           = auth_active
    EXCEPTIONS
      framework_exception   = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          RAISING framework_exception.
  ENDIF.

ENDFUNCTION.
