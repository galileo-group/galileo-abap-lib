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

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
  methods VAR1 .
  methods VAR2 .
  methods VAR3 .
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
endmethod.


method VAR1.
endmethod.


method VAR2.
endmethod.


method VAR3.
endmethod.
ENDCLASS.
