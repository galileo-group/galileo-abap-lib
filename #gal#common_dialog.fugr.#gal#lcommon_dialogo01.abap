*----------------------------------------------------------------------*
***INCLUDE /GAL/LCOMMON_DIALOGO01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  INITIALIZE_01XX  OUTPUT
*&---------------------------------------------------------------------*
*       Initialize subscreen container
*----------------------------------------------------------------------*
MODULE initialize_01xx OUTPUT.
  READ TABLE g_dynp_01xx-allowed_results
        WITH KEY table_line = /gal/common_dialog=>dlg_result_cancel
             TRANSPORTING NO FIELDS.                     "#EC CI_STDSEQ
  IF sy-subrc = 0.
    SET PF-STATUS 'DIALOG'.
  ELSE.
    SET PF-STATUS 'DIALOG' EXCLUDING /gal/common_dialog=>dlg_result_cancel.
  ENDIF.

  SET TITLEBAR 'DEFAULT' WITH g_dynp_01xx-title.
ENDMODULE.                 " INITIALIZE_01XX  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INITIALIZE_0200  OUTPUT
*&---------------------------------------------------------------------*
*       Initialize message text display with icon
*----------------------------------------------------------------------*
MODULE initialize_0200 OUTPUT.
  PERFORM initialize_0200.
ENDMODULE.                 " INITIALIZE_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INITIALIZE_0300  OUTPUT
*&---------------------------------------------------------------------*
*       Set default result
*----------------------------------------------------------------------*
MODULE initialize_0300 OUTPUT.
  CHECK g_dynp_0300-input_style = /gal/common_dialog=>dlg_input_style_none.

  SET CURSOR FIELD 'BUTTON_CLOSE'.
ENDMODULE.                 " INITIALIZE_0300  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INITIALIZE_0301  OUTPUT
*&---------------------------------------------------------------------*
*       Set default result
*----------------------------------------------------------------------*
MODULE initialize_0301 OUTPUT.
  CHECK g_dynp_0301-input_style = /gal/common_dialog=>dlg_input_style_none.

  SET CURSOR FIELD 'BUTTON_OK'.
ENDMODULE.                 " INITIALIZE_0301  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INITIALIZE_0302  OUTPUT
*&---------------------------------------------------------------------*
*       Set default result
*----------------------------------------------------------------------*
MODULE initialize_0302 OUTPUT.
  CHECK g_dynp_0302-input_style = /gal/common_dialog=>dlg_input_style_none.

  PERFORM set_default_button USING `BUTTON_`
                                   g_dynp_0302-allowed_results
                                   g_dynp_0302-default_result.
ENDMODULE.                 " INITIALIZE_0302  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INITIALIZE_0303  OUTPUT
*&---------------------------------------------------------------------*
*       Set default result
*----------------------------------------------------------------------*
MODULE initialize_0303 OUTPUT.
  CHECK g_dynp_0303-input_style = /gal/common_dialog=>dlg_input_style_none.

  PERFORM set_default_button USING `BUTTON_`
                                   g_dynp_0303-allowed_results
                                   g_dynp_0303-default_result.
ENDMODULE.                 " INITIALIZE_0303  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INITIALIZE_0304  OUTPUT
*&---------------------------------------------------------------------*
*       Set default result
*----------------------------------------------------------------------*
MODULE initialize_0304 OUTPUT.
  PERFORM set_default_button USING `BUTTON_`
                                   g_dynp_0304-allowed_results
                                   g_dynp_0304-default_result.
ENDMODULE.                 " INITIALIZE_0304  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INITIALIZE_0305  OUTPUT
*&---------------------------------------------------------------------*
*       Set default result
*----------------------------------------------------------------------*
MODULE initialize_0305 OUTPUT.
  PERFORM initialize_button USING g_dynp_0305-option1_icon
                                  g_dynp_0305-option1_text
                         CHANGING g_dynp_0305-button_1.

  PERFORM initialize_button USING g_dynp_0305-option2_icon
                                  g_dynp_0305-option2_text
                         CHANGING g_dynp_0305-button_2.

  IF NOT ( g_dynp_0304-input_style = /gal/common_dialog=>dlg_input_style_none ).
    RETURN.
  ENDIF.

  PERFORM set_default_button USING `G_DYNP_0305-BUTTON_`
                                   g_dynp_0305-allowed_results
                                   g_dynp_0305-default_result.
ENDMODULE.                 " INITIALIZE_0305  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INITIALIZE_0306  OUTPUT
*&---------------------------------------------------------------------*
*       Set default result
*----------------------------------------------------------------------*
MODULE initialize_0306 OUTPUT.
  PERFORM initialize_button USING g_dynp_0306-button1_icon
                                  g_dynp_0306-button1_text
                         CHANGING g_dynp_0306-button1.

  PERFORM initialize_button USING g_dynp_0306-button2_icon
                                  g_dynp_0306-button2_text
                         CHANGING g_dynp_0306-button2.

  PERFORM initialize_button USING g_dynp_0306-button3_icon
                                  g_dynp_0306-button3_text
                         CHANGING g_dynp_0306-button3.

  IF NOT ( g_dynp_0305-input_style = /gal/common_dialog=>dlg_input_style_none ).
    RETURN.
  ENDIF.

  PERFORM set_default_button USING `G_DYNP_0306-BUTTON_`
                                   g_dynp_0306-allowed_results
                                   g_dynp_0306-default_result.
ENDMODULE.                 " INITIALIZE_0306  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INITIALIZE_0400  OUTPUT
*&---------------------------------------------------------------------*
*       Initialize input field
*----------------------------------------------------------------------*
MODULE initialize_0400 OUTPUT.
  PERFORM initialize_0400.
ENDMODULE.                 " INITIALIZE_0400  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INITIALIZE_0401  OUTPUT
*&---------------------------------------------------------------------*
*       Initialize input field
*----------------------------------------------------------------------*
MODULE initialize_0401 OUTPUT.
  PERFORM initialize_0401.
ENDMODULE.                 " INITIALIZE_0401  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INITIALIZE_0500  OUTPUT
*&---------------------------------------------------------------------*
*       Initialize checkboxes
*----------------------------------------------------------------------*
MODULE initialize_0500 OUTPUT.
  PERFORM initialize_0500.
ENDMODULE.                 " INITIALIZE_0500  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INITIALIZE_0501  OUTPUT
*&---------------------------------------------------------------------*
*       Initialize checkboxes
*----------------------------------------------------------------------*
MODULE initialize_0501 OUTPUT.
  PERFORM initialize_0501.
ENDMODULE.                 " INITIALIZE_0501  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PBO_0100_INIT  OUTPUT
*&---------------------------------------------------------------------*
*      Initialize frontend file browser
*----------------------------------------------------------------------*
MODULE initialize_2000 OUTPUT.
  PERFORM initialize_2000.
ENDMODULE.                    "initialize_2000 OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INITIALIZE_3000  OUTPUT
*&---------------------------------------------------------------------*
*      Initialize folder choose dialoge for configuration store
*----------------------------------------------------------------------*
MODULE initialize_3000 OUTPUT.
  PERFORM initialize_3000.
ENDMODULE.
