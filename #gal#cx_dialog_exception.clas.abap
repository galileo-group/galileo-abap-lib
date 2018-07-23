class /GAL/CX_DIALOG_EXCEPTION definition
  public
  inheriting from /GAL/CX_EXCEPTION
  final
  create public .

*"* public components of class /GAL/CX_DIALOG_EXCEPTION
*"* do not include other source files here!!!
public section.
  type-pools ABAP .

  constants /GAL/CX_DIALOG_EXCEPTION type SOTR_CONC value 'E393C9BE0ADBFAF1BF5100155D012203'. "#EC NOTEXT
  constants CANCELLED_BY_USER type SOTR_CONC value 'E393C9BE0ADBFCF1BF5100155D012203'. "#EC NOTEXT

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !VAR1 type STRING optional
      !VAR2 type STRING optional
      !VAR3 type STRING optional
      !VAR4 type STRING optional
      !VAR5 type STRING optional
      !VAR6 type STRING optional
      !VAR7 type STRING optional
      !VAR8 type STRING optional
      !VAR9 type STRING optional .
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
VAR1 = VAR1
VAR2 = VAR2
VAR3 = VAR3
VAR4 = VAR4
VAR5 = VAR5
VAR6 = VAR6
VAR7 = VAR7
VAR8 = VAR8
VAR9 = VAR9
.
 IF textid IS INITIAL.
   me->textid = /GAL/CX_DIALOG_EXCEPTION .
 ENDIF.
  endmethod.
ENDCLASS.
