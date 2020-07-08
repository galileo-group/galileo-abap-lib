FUNCTION /GAL/CD_SHOW_DIALOG.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(TITLE) TYPE  CSEQUENCE OPTIONAL
*"     REFERENCE(MESSAGE) TYPE  CSEQUENCE OPTIONAL
*"     REFERENCE(ICON) TYPE  ICON_D OPTIONAL
*"     REFERENCE(STYLE) TYPE  STRING OPTIONAL
*"     REFERENCE(DEFAULT_RESULT) TYPE  STRING OPTIONAL
*"     REFERENCE(BUTTON1_ICON) TYPE  ICON_D OPTIONAL
*"     REFERENCE(BUTTON1_TEXT) TYPE  CSEQUENCE OPTIONAL
*"     REFERENCE(BUTTON2_ICON) TYPE  ICON_D OPTIONAL
*"     REFERENCE(BUTTON2_TEXT) TYPE  CSEQUENCE OPTIONAL
*"     REFERENCE(BUTTON3_ICON) TYPE  ICON_D OPTIONAL
*"     REFERENCE(BUTTON3_TEXT) TYPE  CSEQUENCE OPTIONAL
*"     REFERENCE(INPUT_STYLE) TYPE  STRING OPTIONAL
*"     REFERENCE(PROMPT) TYPE  CSEQUENCE OPTIONAL
*"     REFERENCE(MAX_LENGTH) TYPE  I OPTIONAL
*"     REFERENCE(IS_INPUT_REQUIRED) TYPE  ABAP_BOOL DEFAULT ABAP_FALSE
*"     REFERENCE(OPTIONS_STYLE) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     REFERENCE(RESULT) TYPE  STRING
*"  CHANGING
*"     REFERENCE(VALUE) TYPE  ANY OPTIONAL
*"     REFERENCE(OPTIONS) TYPE  /GAL/CDLG_OPTIONS OPTIONAL
*"----------------------------------------------------------------------

  DATA l_subscreens         TYPE STANDARD TABLE OF gt_subscreen_info.
  DATA l_wa_subscreens      LIKE LINE OF l_subscreens.

  DATA l_subscreen_index(2) TYPE n.
  DATA l_fieldname          TYPE string.
  DATA l_value              TYPE REF TO data.               "#EC NEEDED

  FIELD-SYMBOLS <l_subscreen_info> TYPE gt_subscreen_info.
  FIELD-SYMBOLS <l_subscreen_data> TYPE any.
  FIELD-SYMBOLS <l_value>          TYPE any.
  FIELD-SYMBOLS <l_result>         TYPE string.             "#EC NEEDED

* Copy parameters to global structure
  CLEAR g_dynp_01xx.

  g_dynp_01xx-title             = title.
  g_dynp_01xx-icon              = icon.
  g_dynp_01xx-message           = message.
  g_dynp_01xx-style             = style.
  g_dynp_01xx-default_result    = default_result.

  g_dynp_01xx-button1_icon      = button1_icon.
  g_dynp_01xx-button1_text      = button1_text.
  g_dynp_01xx-button2_icon      = button2_icon.
  g_dynp_01xx-button2_text      = button2_text.
  g_dynp_01xx-button3_icon      = button3_icon.
  g_dynp_01xx-button3_text      = button3_text.

  g_dynp_01xx-input_style       = input_style.
  g_dynp_01xx-prompt            = prompt.
  g_dynp_01xx-max_length        = max_length.
  g_dynp_01xx-is_input_required = is_input_required.

  g_dynp_01xx-options_style     = options_style.
  g_dynp_01xx-options           = options.

* Build list of allowed results
  SPLIT g_dynp_01xx-style AT '|' INTO TABLE g_dynp_01xx-allowed_results.

* Build table with required subscreens
  IF message IS NOT INITIAL.
    l_wa_subscreens-program   = sy-repid.
    l_wa_subscreens-dynpro_no = '0200'.
    INSERT l_wa_subscreens INTO TABLE l_subscreens.
  ENDIF.

* Process input style
  IF value IS REQUESTED.
    CASE input_style.

      WHEN /gal/common_dialog=>dlg_input_style_string.
        l_wa_subscreens-program   = sy-repid.

        IF prompt IS INITIAL.
          l_wa_subscreens-dynpro_no = '0401'.
        ELSE.
          l_wa_subscreens-dynpro_no = '0400'.
        ENDIF.

        INSERT l_wa_subscreens INTO TABLE l_subscreens.

      WHEN OTHERS.
        g_dynp_01xx-input_style = /gal/common_dialog=>dlg_input_style_none.

    ENDCASE.

    IF g_dynp_01xx-input_style <> /gal/common_dialog=>dlg_input_style_none.
      CREATE DATA g_dynp_01xx-value LIKE value.
      ASSIGN g_dynp_01xx-value->* TO <l_value>.

      <l_value> = value.
    ENDIF.
  ELSE.
    g_dynp_01xx-input_style = /gal/common_dialog=>dlg_input_style_none.
  ENDIF.

* Process options style
  IF options IS REQUESTED AND options IS NOT INITIAL.
    CASE options_style.

      WHEN /gal/common_dialog=>dlg_options_style_multi.
        l_wa_subscreens-program   = sy-repid.
        l_wa_subscreens-dynpro_no = '0500'.
        INSERT l_wa_subscreens INTO TABLE l_subscreens.

      WHEN /gal/common_dialog=>dlg_options_style_single.
        l_wa_subscreens-program   = sy-repid.
        l_wa_subscreens-dynpro_no = '0501'.
        INSERT l_wa_subscreens INTO TABLE l_subscreens.

      WHEN OTHERS.
        g_dynp_01xx-options_style = /gal/common_dialog=>dlg_options_style_none.

    ENDCASE.
  ELSE.
    g_dynp_01xx-options_style = /gal/common_dialog=>dlg_options_style_none.
  ENDIF.

  l_wa_subscreens-program   = sy-repid.
  l_wa_subscreens-dynpro_no = '0600'.
  INSERT l_wa_subscreens INTO TABLE l_subscreens.

* Process dialog style (buttons)
  CASE style.

    WHEN /gal/common_dialog=>dlg_style_ok.
      l_wa_subscreens-program   = sy-repid.
      l_wa_subscreens-dynpro_no = '0301'.

    WHEN /gal/common_dialog=>dlg_style_ok_cancel.
      l_wa_subscreens-program   = sy-repid.
      l_wa_subscreens-dynpro_no = '0302'.

    WHEN /gal/common_dialog=>dlg_style_yes_no.
      l_wa_subscreens-program   = sy-repid.
      l_wa_subscreens-dynpro_no = '0303'.

    WHEN /gal/common_dialog=>dlg_style_yes_no_cancel.
      l_wa_subscreens-program   = sy-repid.
      l_wa_subscreens-dynpro_no = '0304'.

    WHEN /gal/common_dialog=>dlg_style_2_buttons.
      l_wa_subscreens-program   = sy-repid.
      l_wa_subscreens-dynpro_no = '0305'.

    WHEN /gal/common_dialog=>dlg_style_3_buttons.
      l_wa_subscreens-program   = sy-repid.
      l_wa_subscreens-dynpro_no = '0306'.

    WHEN OTHERS.
      g_dynp_01xx-style = /gal/common_dialog=>dlg_style_close.

      l_wa_subscreens-program   = sy-repid.
      l_wa_subscreens-dynpro_no = '0300'.

  ENDCASE.

  INSERT l_wa_subscreens INTO TABLE l_subscreens.

* Prepare subscreens
  DO.
    l_subscreen_index = sy-index.

    CONCATENATE `G_DYNP_01XX-SUBSCREEN` l_subscreen_index INTO l_fieldname.
    ASSIGN (l_fieldname) TO <l_subscreen_info>.
    IF sy-subrc <> 0.
      READ TABLE l_subscreens INDEX l_subscreen_index TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        IF l_subscreen_index > 1.
          EXIT.
        ELSE.
          MESSAGE TEXT-x02 TYPE 'X'.
        ENDIF.
      ELSE.
        MESSAGE TEXT-x01 TYPE 'X'.
      ENDIF.
    ENDIF.

    READ TABLE l_subscreens INDEX l_subscreen_index INTO l_wa_subscreens.
    IF sy-subrc = 0.
      <l_subscreen_info>-program   = l_wa_subscreens-program.
      <l_subscreen_info>-dynpro_no = l_wa_subscreens-dynpro_no.

      CONCATENATE `G_DYNP_` <l_subscreen_info>-dynpro_no INTO l_fieldname.
      ASSIGN (l_fieldname) TO <l_subscreen_data>.
      IF sy-subrc = 0.
        CLEAR <l_subscreen_data>.
        MOVE-CORRESPONDING g_dynp_01xx TO <l_subscreen_data>.
      ENDIF.
    ELSE.
      <l_subscreen_info>-program   = sy-repid.
      <l_subscreen_info>-dynpro_no = '0999'.
    ENDIF.
  ENDDO.

* Display dialog
  IF icon IS INITIAL.
    CALL SCREEN '0101' STARTING AT 10 3.
  ELSE.
    CALL SCREEN '0100' STARTING AT 10 3.
  ENDIF.

* Return input value and result
  IF g_dynp_01xx-input_style <> /gal/common_dialog=>dlg_input_style_none AND value IS REQUESTED.
    value = <l_value>.
  ENDIF.

* Return selected options
  IF g_dynp_01xx-options_style <> /gal/common_dialog=>dlg_options_style_none AND options IS REQUESTED.
    CASE options_style.

      WHEN /gal/common_dialog=>dlg_options_style_multi.
        g_dynp_01xx-options = g_dynp_0500-options.

      WHEN /gal/common_dialog=>dlg_options_style_single.
        g_dynp_01xx-options = g_dynp_0501-options.

    ENDCASE.

    options = g_dynp_01xx-options.
  ENDIF.

* Return dialog result
  result = g_dynp_01xx-result.
ENDFUNCTION.
