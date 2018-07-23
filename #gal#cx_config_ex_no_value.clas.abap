class /GAL/CX_CONFIG_EX_NO_VALUE definition
  public
  inheriting from /GAL/CX_CONFIG_EXCEPTION
  final
  create public .

public section.
  type-pools ABAP .

  constants /GAL/CX_CONFIG_EX_NO_VALUE type SOTR_CONC value '00155DF935091ED798CA1305899516A9'. "#EC NOTEXT
  constants NO_VALUE_DEFINED type SOTR_CONC value '00155DF935091ED798CA102E25FDF6A9'. "#EC NOTEXT

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



CLASS /GAL/CX_CONFIG_EX_NO_VALUE IMPLEMENTATION.


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
   me->textid = /GAL/CX_CONFIG_EX_NO_VALUE .
 ENDIF.
  endmethod.
ENDCLASS.
