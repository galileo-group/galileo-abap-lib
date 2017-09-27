class /GAL/CX_AGGREGATE_EXCEPTION definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  constants /GAL/CX_AGGREGATE_EXCEPTION type SOTR_CONC value '00155D0122031EE6B8FAF2312E7E5417'. "#EC NOTEXT
  data EXCEPTIONS type /GAL/EXCEPTIONS read-only .

  methods ADD_EXCEPTION
    importing
      !EXCEPTION type ref to CX_ROOT .
  methods ADD_EXCEPTIONS
    importing
      !EXCEPTIONS type /GAL/EXCEPTIONS .
  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !EXCEPTIONS type /GAL/EXCEPTIONS optional .
protected section.
private section.
ENDCLASS.



CLASS /GAL/CX_AGGREGATE_EXCEPTION IMPLEMENTATION.


METHOD add_exception.
  INSERT exception INTO TABLE exceptions.
ENDMETHOD.


METHOD add_exceptions.
  INSERT LINES OF exceptions INTO TABLE me->exceptions.
ENDMETHOD.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = /GAL/CX_AGGREGATE_EXCEPTION .
 ENDIF.
me->EXCEPTIONS = EXCEPTIONS .
endmethod.
ENDCLASS.
