FUNCTION /gal/rfc_get_sy_data .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(READ_CALLSTACK) TYPE  FLAG DEFAULT SPACE
*"     VALUE(GET_AUTH_ACTIVE_STATE) TYPE  FLAG DEFAULT SPACE
*"  EXPORTING
*"     VALUE(SYSYSID) TYPE  SYSYSID
*"     VALUE(SYMANDT) TYPE  SYMANDT
*"     VALUE(SYUNAME) TYPE  SYUNAME
*"     VALUE(CALLSTACK) TYPE  ABAP_CALLSTACK
*"     VALUE(AUTH_ACTIVE) TYPE  FLAG
*"  EXCEPTIONS
*"      FRAMEWORK_EXCEPTION
*"----------------------------------------------------------------------

  DATA l_ex_fw                  TYPE REF TO /gal/cx_cfw_auth_excep_fw.
  DATA l_ex_text                TYPE string.
  DATA l_auth_ex_text_c150(150) TYPE c.
  DATA l_syscli_id(7)           TYPE c.


  sysysid = sy-sysid.
  symandt = sy-mandt.
  syuname = sy-uname.

  IF NOT read_callstack IS INITIAL.
    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = callstack.
  ENDIF.

  IF NOT get_auth_active_state IS INITIAL.
    TRY.
        auth_active = /gal/cfw_auth_config=>is_active( ).
      CATCH /gal/cx_cfw_auth_excep_fw INTO l_ex_fw.
        l_ex_text = l_ex_fw->get_text( ).
        CONCATENATE sy-sysid sy-mandt INTO l_syscli_id SEPARATED BY '.'.
        l_auth_ex_text_c150 = l_ex_text.
        MESSAGE e018 WITH l_syscli_id l_auth_ex_text_c150(50) l_auth_ex_text_c150+50(50) l_auth_ex_text_c150+100(50) RAISING framework_exception.
    ENDTRY.
  ENDIF.

ENDFUNCTION.
