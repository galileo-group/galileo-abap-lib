class /GAL/CX_AUTH_CHECK_EXCEPTION definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  constants /GAL/CX_AUTH_CHECK_EXCEPTION type SOTR_CONC value '00155D0122031EE4AADA89FF32B67341'. "#EC NOTEXT
  constants NOT_AUTHORIZED type SOTR_CONC value '00155D0122031EE4AADE4C0D729C3341'. "#EC NOTEXT
  constants CUSTOM_EXCEPTION type SOTR_CONC value '00155D0122031EE4ABA65969AC8652EF'. "#EC NOTEXT
  constants NOT_AUTHORIZED_FOR_CLIENT type SOTR_CONC value '00155D0122031EE4ABA65969AC8672EF'. "#EC NOTEXT
  data VAR1 type STRING read-only .
  data VAR2 type STRING read-only .
  data VAR3 type STRING read-only .

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



CLASS /GAL/CX_AUTH_CHECK_EXCEPTION IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = /GAL/CX_AUTH_CHECK_EXCEPTION .
 ENDIF.
me->VAR1 = VAR1 .
me->VAR2 = VAR2 .
me->VAR3 = VAR3 .
endmethod.
ENDCLASS.
