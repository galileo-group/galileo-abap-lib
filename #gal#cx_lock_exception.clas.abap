class /GAL/CX_LOCK_EXCEPTION definition
  public
  inheriting from /GAL/CX_EXCEPTION
  final
  create public .

public section.
  type-pools ABAP .

  constants /GAL/CX_LOCK_EXCEPTION type SOTR_CONC value '00155D0122031EE3BAE00BB6BDAB42CA'. "#EC NOTEXT
  constants CANNOT_ACQUIRE_LOCK type SOTR_CONC value '00155D0122031EE3BAE00BB6BDAB62CA'. "#EC NOTEXT
  constants RFC_EXCEPTION type SOTR_CONC value '00155D0122031EE3BAE00BB6BDAB82CA'. "#EC NOTEXT
  constants NOT_LOCKED_SET_SYNC_TIMESTAMP type SOTR_CONC value '00155D0122031EE3BAE21990619302CA'. "#EC NOTEXT

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



CLASS /GAL/CX_LOCK_EXCEPTION IMPLEMENTATION.


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
   me->textid = /GAL/CX_LOCK_EXCEPTION .
 ENDIF.
  endmethod.
ENDCLASS.
