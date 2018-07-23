class /GAL/CX_JS_MISSING_RESOURCE definition
  public
  inheriting from /GAL/CX_JS_EXCEPTION
  final
  create public .

*"* public components of class /GAL/CX_JS_MISSING_RESOURCE
*"* do not include other source files here!!!
public section.
  type-pools ABAP .

  constants /GAL/CX_JS_MISSING_RESOURCE type SOTR_CONC value 'E38A7036B453A4F1BF5100155D012203'. "#EC NOTEXT
  constants MISSING_RESOURCE type SOTR_CONC value 'E38A7036B453A5F1BF5100155D012203'. "#EC NOTEXT

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
*"* protected components of class /GAL/CX_JS_MISSING_RESOURCE
*"* do not include other source files here!!!
private section.
*"* private components of class /GAL/CX_JS_MISSING_RESOURCE
*"* do not include other source files here!!!
ENDCLASS.



CLASS /GAL/CX_JS_MISSING_RESOURCE IMPLEMENTATION.


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
   me->textid = /GAL/CX_JS_MISSING_RESOURCE .
 ENDIF.
  endmethod.
ENDCLASS.
