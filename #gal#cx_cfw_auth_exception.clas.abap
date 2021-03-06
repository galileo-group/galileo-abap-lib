class /GAL/CX_CFW_AUTH_EXCEPTION definition
  public
  inheriting from /GAL/CX_CFW_EXCEPTION
  create public .

public section.
  type-pools ABAP .

  constants /GAL/CX_CFW_AUTH_EXCEPTION type SOTR_CONC value '00155DF935091ED7A682AA411456F6AD'. "#EC NOTEXT
  constants RFC_BACK_CALL_ERROR1 type SOTR_CONC value '00155DF935091ED7A6981703D8C916AD'. "#EC NOTEXT
  constants RFC_BACK_CALL_STACK_INCONS4 type SOTR_CONC value '00155DF935091ED7A6981703D8C996AD'. "#EC NOTEXT
  constants RFC_BACK_CALL_STACK_INCONS3 type SOTR_CONC value '00155DF935091ED7A6981703D8C976AD'. "#EC NOTEXT
  constants RFC_BACK_CALL_STACK_INCONS2 type SOTR_CONC value '00155DF935091ED7A6981703D8C956AD'. "#EC NOTEXT
  constants RFC_BACK_CALL_STACK_INCONS1 type SOTR_CONC value '00155DF935091ED7A6981703D8C936AD'. "#EC NOTEXT
  constants ERROR_READING_CONFIG type SOTR_CONC value '00155DF935091ED7A6BB8E1F8C55D6AD'. "#EC NOTEXT
  constants FORBIDDEN_RFC_CALL_OF_LOC_FUNC type SOTR_CONC value '00155DF935091ED7A6BB8E1F8C55F6AD'. "#EC NOTEXT
  constants FORBIDDEN_RFC_CALLER type SOTR_CONC value '00155DF935091ED7A6FA8ACD1D9DB6AD'. "#EC NOTEXT
  constants FORBIDDEN_PROGRAM type SOTR_CONC value '00155DF935091ED7A8D6052D511B16AE'. "#EC NOTEXT
  constants FORBIDDEN_PROGRAM_USER type SOTR_CONC value '00155DF935091ED7A8D6052D511B36AE'. "#EC NOTEXT

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



CLASS /GAL/CX_CFW_AUTH_EXCEPTION IMPLEMENTATION.


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
   me->textid = /GAL/CX_CFW_AUTH_EXCEPTION .
 ENDIF.
  endmethod.
ENDCLASS.
