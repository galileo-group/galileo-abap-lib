class /GAL/CX_SEARCH_HELP_EXCEPTION definition
  public
  inheriting from /GAL/CX_EXCEPTION
  final
  create public .

public section.
  type-pools ABAP .

  constants /GAL/CX_SEARCH_HELP_EXCEPTION type SOTR_CONC value '00155D0122031EE5BBEA0EEB9B96A6E2'. "#EC NOTEXT
  constants ABORTED_BY_USER type SOTR_CONC value '00155D0122031EE5BBEA14B5154A66E2'. "#EC NOTEXT
  constants NO_SEARCH_HELP_FOR_DATA_ELEM type SOTR_CONC value '00155D0122031EE5BBEAB22797B806E2'. "#EC NOTEXT
  constants SEARCH_HELP_NOT_FOUND type SOTR_CONC value '00155D0122031EE5BBEAB22797B826E2'. "#EC NOTEXT
  data ABORTED type ABAP_BOOL read-only value ABAP_FALSE. "#EC NOTEXT

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
      !VAR9 type STRING optional
      !ABORTED type ABAP_BOOL default ABAP_FALSE .
protected section.
private section.
ENDCLASS.



CLASS /GAL/CX_SEARCH_HELP_EXCEPTION IMPLEMENTATION.


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
   me->textid = /GAL/CX_SEARCH_HELP_EXCEPTION .
 ENDIF.
me->ABORTED = ABORTED .
  endmethod.
ENDCLASS.
