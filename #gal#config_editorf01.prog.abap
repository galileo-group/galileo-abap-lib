*&---------------------------------------------------------------------*
*&  Include           /GAL/CONFIG_EDITORF01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  call_screen
*&---------------------------------------------------------------------*
*       Start dynpro processing
*----------------------------------------------------------------------*
*      -->P_SCREEN   Screen number
*----------------------------------------------------------------------*
FORM call_screen USING  p_screen  TYPE sy-dynnr
                        p_col1    TYPE i
                        p_lin1    TYPE i.                   "#EC CALLED
  IF p_col1 EQ 0 AND p_lin1 EQ 0.
    CALL SCREEN p_screen.
  ELSE.
    CALL SCREEN p_screen STARTING AT  p_col1 p_lin1.
  ENDIF.
ENDFORM.                    "call_screen
