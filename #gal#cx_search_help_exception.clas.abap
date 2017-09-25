class /GAL/CX_SEARCH_HELP_EXCEPTION definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.
  type-pools ABAP .

  constants /GAL/CX_SEARCH_HELP_EXCEPTION type SOTR_CONC value '00155D0122031EE5BBEA0EEB9B96A6E2'. "#EC NOTEXT
  constants CUSTOM_EXCEPTION type SOTR_CONC value '00155D0122031EE5BBEA2630352FE6E2'. "#EC NOTEXT
  constants ABORTED_BY_USER type SOTR_CONC value '00155D0122031EE5BBEA14B5154A66E2'. "#EC NOTEXT
  constants NO_SEARCH_HELP_FOR_DATA_ELEM type SOTR_CONC value '00155D0122031EE5BBEAB22797B806E2'. "#EC NOTEXT
  constants SEARCH_HELP_NOT_FOUND type SOTR_CONC value '00155D0122031EE5BBEAB22797B826E2'. "#EC NOTEXT
  data ABORTED type ABAP_BOOL read-only value ABAP_FALSE. "#EC NOTEXT
  data VAR1 type STRING read-only .
  data VAR2 type STRING read-only .
  data VAR3 type STRING read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !ABORTED type ABAP_BOOL default ABAP_FALSE
      !VAR1 type STRING optional
      !VAR2 type STRING optional
      !VAR3 type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS /GAL/CX_SEARCH_HELP_EXCEPTION IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = /GAL/CX_SEARCH_HELP_EXCEPTION .
 ENDIF.
me->ABORTED = ABORTED .
me->VAR1 = VAR1 .
me->VAR2 = VAR2 .
me->VAR3 = VAR3 .
endmethod.
ENDCLASS.
