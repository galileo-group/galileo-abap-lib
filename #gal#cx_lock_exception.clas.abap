class /GAL/CX_LOCK_EXCEPTION definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.
  type-pools ABAP .

  constants /GAL/CX_LOCK_EXCEPTION type SOTR_CONC value '00155D0122031EE3BAE00BB6BDAB42CA'. "#EC NOTEXT
  constants CANNOT_ACQUIRE_LOCK type SOTR_CONC value '00155D0122031EE3BAE00BB6BDAB62CA'. "#EC NOTEXT
  constants RFC_EXCEPTION type SOTR_CONC value '00155D0122031EE3BAE00BB6BDAB82CA'. "#EC NOTEXT
  constants NOT_LOCKED_SET_SYNC_TIMESTAMP type SOTR_CONC value '00155D0122031EE3BAE21990619302CA'. "#EC NOTEXT
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



CLASS /GAL/CX_LOCK_EXCEPTION IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = /GAL/CX_LOCK_EXCEPTION .
 ENDIF.
me->VAR1 = VAR1 .
me->VAR2 = VAR2 .
me->VAR3 = VAR3 .
endmethod.
ENDCLASS.
