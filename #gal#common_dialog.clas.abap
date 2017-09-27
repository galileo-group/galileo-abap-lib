class /GAL/COMMON_DIALOG definition
  public
  create public .

*"* public components of class /GAL/COMMON_DIALOG
*"* do not include other source files here!!!
public section.
  type-pools ICON .

  constants DLG_INPUT_STYLE_NONE type STRING value ``. "#EC NOTEXT
  constants DLG_INPUT_STYLE_STRING type STRING value `STRING`. "#EC NOTEXT
  constants DLG_OPTIONS_STYLE_MULTI type STRING value `MULTI`. "#EC NOTEXT
  constants DLG_OPTIONS_STYLE_NONE type STRING value ``. "#EC NOTEXT
  constants DLG_OPTIONS_STYLE_SINGLE type STRING value `SINGLE`. "#EC NOTEXT
  constants DLG_RESULT_BUTTON1 type STRING value `1`. "#EC NOTEXT
  constants DLG_RESULT_BUTTON2 type STRING value `2`. "#EC NOTEXT
  constants DLG_RESULT_BUTTON3 type STRING value `3`. "#EC NOTEXT
  constants DLG_RESULT_CANCEL type STRING value `CANCEL`. "#EC NOTEXT
  constants DLG_RESULT_NO type STRING value `NO`. "#EC NOTEXT
  constants DLG_RESULT_OK type STRING value `OK`. "#EC NOTEXT
  constants DLG_RESULT_YES type STRING value `YES`. "#EC NOTEXT
  constants DLG_STYLE_2_BUTTONS type STRING value `1|2`. "#EC NOTEXT
  constants DLG_STYLE_3_BUTTONS type STRING value `1|2|3`. "#EC NOTEXT
  constants DLG_STYLE_CLOSE type STRING value `CLOSE`. "#EC NOTEXT
  constants DLG_STYLE_OK type STRING value `OK`. "#EC NOTEXT
  constants DLG_STYLE_OK_CANCEL type STRING value `OK|CANCEL`. "#EC NOTEXT
  constants DLG_STYLE_YES_NO type STRING value `YES|NO`. "#EC NOTEXT
  constants DLG_STYLE_YES_NO_CANCEL type STRING value `YES|NO|CANCEL`. "#EC NOTEXT

  type-pools ABAP .
  class-methods SHOW_COMPLEX_DIALOG
    importing
      !TITLE type CSEQUENCE optional
      !MESSAGE type CSEQUENCE default ``
      !MESSAGE_VAR1 type ANY optional
      !MESSAGE_VAR2 type ANY optional
      !MESSAGE_VAR3 type ANY optional
      !MESSAGE_VAR4 type ANY optional
      !MESSAGE_VAR5 type ANY optional
      !MESSAGE_VAR6 type ANY optional
      !MESSAGE_VAR7 type ANY optional
      !MESSAGE_VAR8 type ANY optional
      !MESSAGE_VAR9 type ANY optional
      !MESSAGE_VAR10 type ANY optional
      !ICON type ICON_D optional
      !STYLE type STRING default DLG_STYLE_OK
      !DEFAULT_RESULT type STRING optional
      !BUTTON1_ICON type ICON_D optional
      !BUTTON1_TEXT type CSEQUENCE optional
      !BUTTON2_ICON type ICON_D optional
      !BUTTON2_TEXT type CSEQUENCE optional
      !BUTTON3_ICON type ICON_D optional
      !BUTTON3_TEXT type CSEQUENCE optional
      !INPUT_STYLE type STRING default DLG_INPUT_STYLE_NONE
      !PROMPT type CSEQUENCE optional
      !MAX_LENGTH type I default 0
      !IS_INPUT_REQUIRED type ABAP_BOOL optional
      !OPTIONS_STYLE type STRING default DLG_OPTIONS_STYLE_NONE
      !LANGUAGE type LANGU default SY-LANGU
    exporting
      !RESULT type STRING
    changing
      !VALUE type ANY optional
      !OPTIONS type /GAL/CDLG_OPTIONS optional .
  class-methods SHOW_CONFIRMATION_DIALOG
    importing
      !TITLE type CSEQUENCE optional
      !MESSAGE type CSEQUENCE
      !MESSAGE_VAR1 type ANY optional
      !MESSAGE_VAR2 type ANY optional
      !MESSAGE_VAR3 type ANY optional
      !MESSAGE_VAR4 type ANY optional
      !MESSAGE_VAR5 type ANY optional
      !MESSAGE_VAR6 type ANY optional
      !MESSAGE_VAR7 type ANY optional
      !MESSAGE_VAR8 type ANY optional
      !MESSAGE_VAR9 type ANY optional
      !MESSAGE_VAR10 type ANY optional
      !ICON type ICON_D optional
      !STYLE type STRING default DLG_STYLE_OK
      !DEFAULT_RESULT type STRING optional
      !BUTTON1_ICON type ICON_D optional
      !BUTTON1_TEXT type CSEQUENCE optional
      !BUTTON2_ICON type ICON_D optional
      !BUTTON2_TEXT type CSEQUENCE optional
      !BUTTON3_ICON type ICON_D optional
      !BUTTON3_TEXT type CSEQUENCE optional
      !LANGUAGE type LANGU default SY-LANGU
    returning
      value(RESULT) type STRING .
  class-methods SHOW_INPUT_DIALOG
    importing
      !TITLE type CSEQUENCE optional
      !MESSAGE type CSEQUENCE default ``
      !MESSAGE_VAR1 type ANY optional
      !MESSAGE_VAR2 type ANY optional
      !MESSAGE_VAR3 type ANY optional
      !MESSAGE_VAR4 type ANY optional
      !MESSAGE_VAR5 type ANY optional
      !MESSAGE_VAR6 type ANY optional
      !MESSAGE_VAR7 type ANY optional
      !MESSAGE_VAR8 type ANY optional
      !MESSAGE_VAR9 type ANY optional
      !MESSAGE_VAR10 type ANY optional
      !ICON type ICON_D optional
      !INPUT_STYLE type STRING default DLG_INPUT_STYLE_STRING
      !PROMPT type CSEQUENCE optional
      !MAX_LENGTH type I default 0
      !IS_INPUT_REQUIRED type ABAP_BOOL default ABAP_TRUE
      !CAN_BE_CANCELLED type ABAP_BOOL default ABAP_TRUE
      !LANGUAGE type LANGU default SY-LANGU
    changing
      !VALUE type ANY
    raising
      /GAL/CX_DIALOG_EXCEPTION .
  class-methods SHOW_OPTIONS_DIALOG
    importing
      !TITLE type CSEQUENCE optional
      !MESSAGE type CSEQUENCE default ``
      !MESSAGE_VAR1 type ANY optional
      !MESSAGE_VAR2 type ANY optional
      !MESSAGE_VAR3 type ANY optional
      !MESSAGE_VAR4 type ANY optional
      !MESSAGE_VAR5 type ANY optional
      !MESSAGE_VAR6 type ANY optional
      !MESSAGE_VAR7 type ANY optional
      !MESSAGE_VAR8 type ANY optional
      !MESSAGE_VAR9 type ANY optional
      !MESSAGE_VAR10 type ANY optional
      !ICON type ICON_D optional
      !OPTIONS_STYLE type STRING default DLG_OPTIONS_STYLE_MULTI
      !CAN_BE_CANCELLED type ABAP_BOOL default ABAP_TRUE
      !LANGUAGE type LANGU default SY-LANGU
    changing
      !OPTIONS type /GAL/CDLG_OPTIONS
    raising
      /GAL/CX_DIALOG_EXCEPTION .
protected section.
*"* protected components of class /GAL/COMMON_DIALOG
*"* do not include other source files here!!!
private section.
*"* private components of class /GAL/COMMON_DIALOG
*"* do not include other source files here!!!

  class-methods GET_DEFAULT_ICON
    importing
      !STYLE type STRING default DLG_STYLE_OK
      !INPUT_STYLE type STRING default DLG_INPUT_STYLE_NONE
      !OPTIONS_STYLE type STRING default DLG_OPTIONS_STYLE_NONE
    returning
      value(ICON) type ICON_D .
  class-methods GET_DEFAULT_RESULT
    importing
      !STYLE type STRING default DLG_STYLE_OK
    returning
      value(RESULT) type ICON_D .
ENDCLASS.



CLASS /GAL/COMMON_DIALOG IMPLEMENTATION.


METHOD get_default_icon.
  IF input_style   = dlg_input_style_none AND
     options_style = dlg_options_style_none.
    IF style = dlg_style_ok.
      icon = icon_message_information.
    ELSEIF style = dlg_style_yes_no.
      icon = icon_message_question.
    ELSE.
      icon = icon_message_warning.
    ENDIF.
  ELSEIF options_style <> dlg_options_style_none.
    icon = icon_message_question.
  ENDIF.
ENDMETHOD.


METHOD get_default_result.
  IF style CP dlg_result_ok.
    result = dlg_result_ok.
  ELSEIF style CP dlg_result_yes.
    result = dlg_result_yes.
  ELSE.
    result = ``.
  ENDIF.
ENDMETHOD.


METHOD show_complex_dialog.
  DATA l_message        TYPE string.
  DATA l_icon           TYPE icon_d.
  DATA l_default_result TYPE string.

* Build message string
  l_message = /gal/string=>replace_variables( input    = message
                                              var01    = message_var1
                                              var02    = message_var2
                                              var03    = message_var3
                                              var04    = message_var4
                                              var05    = message_var5
                                              var06    = message_var6
                                              var07    = message_var7
                                              var08    = message_var8
                                              var09    = message_var9
                                              var10    = message_var10
                                              language = language ).

* Determine icon to be used
  IF icon IS SUPPLIED.
    l_icon = icon.
  ELSE.
    l_icon = get_default_icon( style       = style
                               input_style = input_style ).
  ENDIF.

* Determine default result
  IF default_result IS SUPPLIED.
    l_default_result = default_result.
  ELSE.
    l_default_result = get_default_result( style ).
  ENDIF.

* Show dialog
  CALL FUNCTION '/GAL/CD_SHOW_DIALOG'
    EXPORTING
      title             = title
      MESSAGE           = l_message
      icon              = l_icon
      style             = style
      default_result    = l_default_result
      button1_icon      = button1_icon
      button1_text      = button1_text
      button2_icon      = button2_icon
      button2_text      = button2_text
      button3_icon      = button3_icon
      button3_text      = button3_text
      input_style       = input_style
      prompt            = prompt
      max_length        = max_length
      is_input_required = is_input_required
      options_style     = options_style
    IMPORTING
      RESULT            = RESULT
    changing
      value             = value
      OPTIONS           = options.
ENDMETHOD.


METHOD show_confirmation_dialog.
  DATA l_icon TYPE icon_d.

* Determine icon to be used
  IF icon IS SUPPLIED.
    l_icon = icon.
  ELSE.
    l_icon = get_default_icon( style = style ).
  ENDIF.

* Show dialog
  show_complex_dialog( EXPORTING title             = title
                                 message           = message
                                 message_var1      = message_var1
                                 message_var2      = message_var2
                                 message_var3      = message_var3
                                 message_var4      = message_var4
                                 message_var5      = message_var5
                                 message_var6      = message_var6
                                 message_var7      = message_var7
                                 message_var8      = message_var8
                                 message_var9      = message_var9
                                 message_var10     = message_var10
                                 icon              = l_icon
                                 style             = style
                                 default_result    = default_result
                                 button1_icon      = button1_icon
                                 button1_text      = button1_text
                                 button2_icon      = button2_icon
                                 button2_text      = button2_text
                                 button3_icon      = button3_icon
                                 button3_text      = button3_text
                                 language          = language
                       IMPORTING result            = result ).
ENDMETHOD.


METHOD show_input_dialog.
  DATA l_icon TYPE icon_d.
  DATA l_style  TYPE string.
  DATA l_result TYPE string.

* Determine dialog style to be used
  IF can_be_cancelled = abap_true.
    l_style = dlg_style_ok_cancel.
  ELSE.
    l_style = dlg_style_ok.
  ENDIF.

* Determine icon to be used
  IF icon IS SUPPLIED.
    l_icon = icon.
  ELSE.
    l_icon = get_default_icon( style       = l_style
                               input_style = input_style ).
  ENDIF.

* Show dialog
  show_complex_dialog( EXPORTING title             = title
                                 message           = message
                                 message_var1      = message_var1
                                 message_var2      = message_var2
                                 message_var3      = message_var3
                                 message_var4      = message_var4
                                 message_var5      = message_var5
                                 message_var6      = message_var6
                                 message_var7      = message_var7
                                 message_var8      = message_var8
                                 message_var9      = message_var9
                                 message_var10     = message_var10
                                 icon              = l_icon
                                 style             = l_style
                                 input_style       = input_style
                                 prompt            = prompt
                                 max_length        = max_length
                                 is_input_required = is_input_required
                                 language          = language
                       IMPORTING result            = l_result
                       CHANGING  value             = value ).

* Check result
  IF l_result <> `OK`.
    RAISE EXCEPTION TYPE /gal/cx_dialog_exception
          EXPORTING textid = /gal/cx_dialog_exception=>cancelled_by_user.
  ENDIF.
ENDMETHOD.


METHOD SHOW_OPTIONS_DIALOG.
  DATA l_icon TYPE icon_d.
  DATA l_style  TYPE string.
  DATA l_result TYPE string.

* Determine dialog style to be used
  IF can_be_cancelled = abap_true.
    l_style = dlg_style_ok_cancel.
  ELSE.
    l_style = dlg_style_ok.
  ENDIF.

* Determine icon to be used
  IF icon IS SUPPLIED.
    l_icon = icon.
  ELSE.
    l_icon = get_default_icon( style         = l_style
                               options_style = options_style ).
  ENDIF.

* Show dialog
  show_complex_dialog( EXPORTING title             = title
                                 message           = message
                                 message_var1      = message_var1
                                 message_var2      = message_var2
                                 message_var3      = message_var3
                                 message_var4      = message_var4
                                 message_var5      = message_var5
                                 message_var6      = message_var6
                                 message_var7      = message_var7
                                 message_var8      = message_var8
                                 message_var9      = message_var9
                                 message_var10     = message_var10
                                 icon              = l_icon
                                 style             = l_style
                                 options_style     = options_style
                                 language          = language
                       IMPORTING result            = l_result
                       CHANGING  options           = options ).

* Check result
  IF l_result <> `OK`.
    RAISE EXCEPTION TYPE /gal/cx_dialog_exception
          EXPORTING textid = /gal/cx_dialog_exception=>cancelled_by_user.
  ENDIF.
ENDMETHOD.
ENDCLASS.
