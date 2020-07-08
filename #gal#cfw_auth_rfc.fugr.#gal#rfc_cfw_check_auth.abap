FUNCTION /GAL/RFC_CFW_CHECK_AUTH.
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
  DATA: l_auth_fieldval1  TYPE /gal/cfw_auth_field_val,
        l_auth_fieldval2  TYPE /gal/cfw_auth_field_val,
        l_auth_fieldval3  TYPE /gal/cfw_auth_field_val,
        l_auth_fieldval4  TYPE /gal/cfw_auth_field_val,
        l_auth_fieldval5  TYPE /gal/cfw_auth_field_val,
        l_auth_fieldval6  TYPE /gal/cfw_auth_field_val,
        l_auth_fieldval7  TYPE /gal/cfw_auth_field_val,
        l_auth_fieldval8  TYPE /gal/cfw_auth_field_val,
        l_auth_fieldval9  TYPE /gal/cfw_auth_field_val,
        l_auth_fieldval10 TYPE /gal/cfw_auth_field_val,
        l_field_name(16)  TYPE c,
        l_count           TYPE i.


  FIELD-SYMBOLS: <l_imp_sched_auth>  TYPE /gal/cfw_auth_def,
                 <l_auth_fieldval>   TYPE /gal/cfw_auth_field_val,
                 <l_auth_fieldval_x> TYPE /gal/cfw_auth_field_val.


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
      RETURN.
    ENDIF.

    l_count = 0.
    LOOP AT <l_imp_sched_auth>-fields ASSIGNING <l_auth_fieldval>.
      l_count = l_count + 1.
      WRITE l_count TO l_field_name.
      CONDENSE l_field_name.
      CONCATENATE 'L_AUTH_FIELDVAL' l_field_name INTO l_field_name.
      ASSIGN (l_field_name) TO <l_auth_fieldval_x>.
      IF sy-subrc <> 0.
        forbidden = abap_true.
        RETURN.
      ELSE.
        <l_auth_fieldval_x> = <l_auth_fieldval>.
      ENDIF.
    ENDLOOP.

    CASE l_count.
      WHEN 1.
        AUTHORITY-CHECK OBJECT <l_imp_sched_auth>-object
          FOR USER user
          ID l_auth_fieldval1-id FIELD l_auth_fieldval1-field.
      WHEN 2.
        AUTHORITY-CHECK OBJECT <l_imp_sched_auth>-object
          FOR USER user
          ID l_auth_fieldval1-id FIELD l_auth_fieldval1-field
          ID l_auth_fieldval2-id FIELD l_auth_fieldval2-field.
      WHEN 3.
        AUTHORITY-CHECK OBJECT <l_imp_sched_auth>-object
          FOR USER user
          ID l_auth_fieldval1-id FIELD l_auth_fieldval1-field
          ID l_auth_fieldval2-id FIELD l_auth_fieldval2-field
          ID l_auth_fieldval3-id FIELD l_auth_fieldval3-field.
      WHEN 4.
        AUTHORITY-CHECK OBJECT <l_imp_sched_auth>-object
          FOR USER user
          ID l_auth_fieldval1-id FIELD l_auth_fieldval1-field
          ID l_auth_fieldval2-id FIELD l_auth_fieldval2-field
          ID l_auth_fieldval3-id FIELD l_auth_fieldval3-field
          ID l_auth_fieldval4-id FIELD l_auth_fieldval4-field.
      WHEN 5.
        AUTHORITY-CHECK OBJECT <l_imp_sched_auth>-object
          FOR USER user
          ID l_auth_fieldval1-id FIELD l_auth_fieldval1-field
          ID l_auth_fieldval2-id FIELD l_auth_fieldval2-field
          ID l_auth_fieldval3-id FIELD l_auth_fieldval3-field
          ID l_auth_fieldval4-id FIELD l_auth_fieldval4-field
          ID l_auth_fieldval5-id FIELD l_auth_fieldval5-field.
      WHEN 6.
        AUTHORITY-CHECK OBJECT <l_imp_sched_auth>-object
          FOR USER user
          ID l_auth_fieldval1-id FIELD l_auth_fieldval1-field
          ID l_auth_fieldval2-id FIELD l_auth_fieldval2-field
          ID l_auth_fieldval3-id FIELD l_auth_fieldval3-field
          ID l_auth_fieldval4-id FIELD l_auth_fieldval4-field
          ID l_auth_fieldval5-id FIELD l_auth_fieldval5-field
          ID l_auth_fieldval6-id FIELD l_auth_fieldval6-field.
      WHEN 7.
        AUTHORITY-CHECK OBJECT <l_imp_sched_auth>-object
          FOR USER user
          ID l_auth_fieldval1-id FIELD l_auth_fieldval1-field
          ID l_auth_fieldval2-id FIELD l_auth_fieldval2-field
          ID l_auth_fieldval3-id FIELD l_auth_fieldval3-field
          ID l_auth_fieldval4-id FIELD l_auth_fieldval4-field
          ID l_auth_fieldval5-id FIELD l_auth_fieldval5-field
          ID l_auth_fieldval6-id FIELD l_auth_fieldval6-field
          ID l_auth_fieldval7-id FIELD l_auth_fieldval7-field.
      WHEN 8.
        AUTHORITY-CHECK OBJECT <l_imp_sched_auth>-object
          FOR USER user
          ID l_auth_fieldval1-id FIELD l_auth_fieldval1-field
          ID l_auth_fieldval2-id FIELD l_auth_fieldval2-field
          ID l_auth_fieldval3-id FIELD l_auth_fieldval3-field
          ID l_auth_fieldval4-id FIELD l_auth_fieldval4-field
          ID l_auth_fieldval5-id FIELD l_auth_fieldval5-field
          ID l_auth_fieldval6-id FIELD l_auth_fieldval6-field
          ID l_auth_fieldval7-id FIELD l_auth_fieldval7-field
          ID l_auth_fieldval8-id FIELD l_auth_fieldval8-field.
      WHEN 9.
        AUTHORITY-CHECK OBJECT <l_imp_sched_auth>-object
          FOR USER user
          ID l_auth_fieldval1-id FIELD l_auth_fieldval1-field
          ID l_auth_fieldval2-id FIELD l_auth_fieldval2-field
          ID l_auth_fieldval3-id FIELD l_auth_fieldval3-field
          ID l_auth_fieldval4-id FIELD l_auth_fieldval4-field
          ID l_auth_fieldval5-id FIELD l_auth_fieldval5-field
          ID l_auth_fieldval6-id FIELD l_auth_fieldval6-field
          ID l_auth_fieldval7-id FIELD l_auth_fieldval7-field
          ID l_auth_fieldval8-id FIELD l_auth_fieldval8-field
          ID l_auth_fieldval9-id FIELD l_auth_fieldval9-field.
      WHEN 10.
        AUTHORITY-CHECK OBJECT <l_imp_sched_auth>-object
          FOR USER user
          ID l_auth_fieldval1-id FIELD l_auth_fieldval1-field
          ID l_auth_fieldval2-id FIELD l_auth_fieldval2-field
          ID l_auth_fieldval3-id FIELD l_auth_fieldval3-field
          ID l_auth_fieldval4-id FIELD l_auth_fieldval4-field
          ID l_auth_fieldval5-id FIELD l_auth_fieldval5-field
          ID l_auth_fieldval6-id FIELD l_auth_fieldval6-field
          ID l_auth_fieldval7-id FIELD l_auth_fieldval7-field
          ID l_auth_fieldval8-id FIELD l_auth_fieldval8-field
          ID l_auth_fieldval9-id FIELD l_auth_fieldval9-field
          ID l_auth_fieldval10-id FIELD l_auth_fieldval10-field.
      WHEN OTHERS.
        forbidden = abap_true.
        RETURN.
    ENDCASE.

    IF sy-subrc <> 0.
      forbidden = abap_true.
      RETURN.
    ENDIF.
  ENDLOOP.



ENDFUNCTION.
