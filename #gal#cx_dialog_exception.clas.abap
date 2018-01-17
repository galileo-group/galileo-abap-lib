class /GAL/CX_DIALOG_EXCEPTION definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

*"* public components of class /GAL/CX_DIALOG_EXCEPTION
*"* do not include other source files here!!!
public section.

  constants /GAL/CX_DIALOG_EXCEPTION type SOTR_CONC value 'E393C9BE0ADBFAF1BF5100155D012203'. "#EC NOTEXT
  constants CANCELLED_BY_USER type SOTR_CONC value 'E393C9BE0ADBFCF1BF5100155D012203'. "#EC NOTEXT
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
*"* protected components of class /GAL/CX_DIALOG_EXCEPTION
*"* do not include other source files here!!!
private section.
*"* private components of class /GAL/CX_DIALOG_EXCEPTION
*"* do not include other source files here!!!
ENDCLASS.



CLASS /GAL/CX_DIALOG_EXCEPTION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = /GAL/CX_DIALOG_EXCEPTION .
 ENDIF.
me->VAR1 = VAR1 .
me->VAR2 = VAR2 .
me->VAR3 = VAR3 .
  endmethod.
ENDCLASS.
