class /GAL/CX_AGGREGATE_EXCEPTION definition
  public
  inheriting from /GAL/CX_EXCEPTION
  create public .

public section.
  type-pools ABAP .

  constants /GAL/CX_AGGREGATE_EXCEPTION type SOTR_CONC value '00155D0122031EE6B8FAF2312E7E5417'. "#EC NOTEXT
  data AGGREGATED_EXCEPTIONS type /GAL/EXCEPTIONS read-only .

  methods ADD_EXCEPTION
    importing
      !EXCEPTION type ref to CX_ROOT .
  methods ADD_EXCEPTIONS
    importing
      !AGGREGATED_EXCEPTIONS type /GAL/EXCEPTIONS .
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
      !AGGREGATED_EXCEPTIONS type /GAL/EXCEPTIONS optional .
protected section.
private section.
ENDCLASS.



CLASS /GAL/CX_AGGREGATE_EXCEPTION IMPLEMENTATION.


METHOD add_exception.
  INSERT exception INTO TABLE aggregated_exceptions.
ENDMETHOD.


METHOD add_exceptions.
  INSERT LINES OF aggregated_exceptions INTO TABLE me->aggregated_exceptions.
ENDMETHOD.


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
   me->textid = /GAL/CX_AGGREGATE_EXCEPTION .
 ENDIF.
me->AGGREGATED_EXCEPTIONS = AGGREGATED_EXCEPTIONS .
  endmethod.
ENDCLASS.
