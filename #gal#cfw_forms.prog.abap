*&---------------------------------------------------------------------*
*&  Subroutine pool   /GAL/CFW_FORMS
*&---------------------------------------------------------------------*
*& This subroutine pool contains forms required by the Galileo
*& Communication Framework.
*&---------------------------------------------------------------------*

PROGRAM /gal/cfw_forms.

TYPE-POOLS abap.

INCLUDE /gal/cfw_macros.                                   "#EC INCL_OK

*-----------------------------------------------------------------------
* Exception Handling: Raise classic exception (RFC_EXCEPTION only!)
*-----------------------------------------------------------------------
FORM cfw_raise_classic_exception USING p_exception_info TYPE /gal/exception_info
                                       p_function_name  TYPE string. "#EC CALLED

  DATA __l_exception_info  TYPE /gal/exception_info.
  DATA __l_function_name   TYPE string.

  DATA __l_exception_part1 TYPE symsgv.
  DATA __l_exception_part2 TYPE symsgv.
  DATA __l_exception_part3 TYPE symsgv.
  DATA __l_func_ex_info    TYPE symsgv.

  __l_exception_info = p_exception_info.
  __l_function_name  = p_function_name.

  cfw_raise_exception rfc_exception.                    "#EC RAIS_NO_FB
ENDFORM.                    "cfw_raise_classic_exception

*-----------------------------------------------------------------------
* Exception Handling: Raise class based exception
*-----------------------------------------------------------------------
FORM cfw_raise_new_exception USING p_exception_info TYPE /gal/exception_info
                                   p_function_name  TYPE string
                           RAISING cx_static_check.         "#EC CALLED

  DATA l_exception TYPE REF TO cx_root.

  IF p_exception_info-exception_type = 'C' AND p_exception_info-xml IS NOT INITIAL.
    CALL TRANSFORMATION id
         OPTIONS    value_handling = 'default'
         SOURCE XML p_exception_info-xml
         RESULT     exception      = l_exception.           "#EC NOTEXT

    RAISE EXCEPTION l_exception.
  ELSE.
    IF p_exception_info-message_text IS NOT INITIAL.
      RAISE EXCEPTION TYPE /gal/cx_cfw_exception
        EXPORTING
          textid = /gal/cx_cfw_exception=>custom_exception
          var1   = p_exception_info-message_text.
    ELSE.
      RAISE EXCEPTION TYPE /gal/cx_cfw_exception
        EXPORTING
          textid = /gal/cx_cfw_exception=>unknown_classic_exception
          var1   = p_exception_info-exception_name
          var2   = p_function_name.
    ENDIF.
  ENDIF.
ENDFORM.                    "cfw_raise_new_exception
