class /GAL/CX_GUI_SHORTCUT_EXCEPTION definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.
  type-pools ABAP .

  constants /GAL/CX_GUI_SHORTCUT_EXCEPTION type SOTR_CONC value '00155D0122041ED785A932990489367C'. "#EC NOTEXT
  constants INVALID_PARAM_COMBINATION type SOTR_CONC value '00155D0122041ED785A932990489567C'. "#EC NOTEXT
  constants SAPGUI_COMMUNICATION_ERROR type SOTR_CONC value '00155D0122041ED785AAB80770E6367C'. "#EC NOTEXT
  data VAR1 type STRING .
  data VAR2 type STRING .
  data VAR3 type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !VAR1 type STRING optional
      !VAR2 type STRING optional
      !VAR3 type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS /GAL/CX_GUI_SHORTCUT_EXCEPTION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = /GAL/CX_GUI_SHORTCUT_EXCEPTION .
 ENDIF.
me->VAR1 = VAR1 .
me->VAR2 = VAR2 .
me->VAR3 = VAR3 .
  endmethod.
ENDCLASS.
