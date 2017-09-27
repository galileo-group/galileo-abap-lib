FUNCTION /gal/rfc_cfw_check_auth.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(AUTH_CONFIG) TYPE  /GAL/CFW_AUTH_DEFS
*"     REFERENCE(USER) TYPE  SYUNAME
*"  EXPORTING
*"     REFERENCE(FORBIDDEN) TYPE  ABAP_BOOL
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  FIELD-SYMBOLS <l_imp_sched_auth> TYPE /gal/cfw_auth_def.
  FIELD-SYMBOLS <l_auth_fieldval>  TYPE /gal/cfw_auth_field_val.


  forbidden = abap_false.

  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.

  IF auth_config IS INITIAL.
    forbidden = abap_true.
    RETURN.
  ENDIF.

  LOOP AT auth_config ASSIGNING <l_imp_sched_auth>.
    IF <l_imp_sched_auth>-object IS INITIAL.
      forbidden = abap_true.
      EXIT.
    ENDIF.
    LOOP AT <l_imp_sched_auth>-fields ASSIGNING <l_auth_fieldval>.
      AUTHORITY-CHECK OBJECT <l_imp_sched_auth>-object
        FOR USER user
        ID <l_auth_fieldval>-id FIELD <l_auth_fieldval>-field.
      CHECK sy-subrc <> 0.
      forbidden = abap_true.
      EXIT.
    ENDLOOP.
    IF sy-subrc <> 0.
      forbidden = abap_true.
    ENDIF.
    CHECK forbidden = abap_true.
    EXIT.
  ENDLOOP.



ENDFUNCTION.
