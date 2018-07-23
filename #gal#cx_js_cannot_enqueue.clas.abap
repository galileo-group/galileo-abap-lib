class /GAL/CX_JS_CANNOT_ENQUEUE definition
  public
  inheriting from /GAL/CX_JS_EXCEPTION
  final
  create public .

public section.
  type-pools ABAP .

  constants /GAL/CX_JS_CANNOT_ENQUEUE type SOTR_CONC value '00155D0122031EE4AADD5DC4DA681341'. "#EC NOTEXT
  constants CANNOT_ENQUEUE type SOTR_CONC value '00155D0122031EE4AADCD05A74EE1341'. "#EC NOTEXT

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
private section.
ENDCLASS.



CLASS /GAL/CX_JS_CANNOT_ENQUEUE IMPLEMENTATION.


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
   me->textid = /GAL/CX_JS_CANNOT_ENQUEUE .
 ENDIF.
  endmethod.
ENDCLASS.
