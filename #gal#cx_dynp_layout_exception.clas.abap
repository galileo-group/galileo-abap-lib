class /GAL/CX_DYNP_LAYOUT_EXCEPTION definition
  public
  inheriting from /GAL/CX_EXCEPTION
  final
  create public .

public section.
  type-pools ABAP .

  constants /GAL/CX_DYNP_LAYOUT_EXCEPTION type SOTR_CONC value '00155D0122031EE497E82A32DE0ED4B2'. "#EC NOTEXT
  constants UNKNOWN_CONTAINER_NAME type SOTR_CONC value '00155D0122031EE497E83119934194B2'. "#EC NOTEXT
  constants CONTAINER_NAME_ALREADY_EXISTS type SOTR_CONC value '00155D0122031EE497E86185257334B2'. "#EC NOTEXT
  constants INVALID_NO_OF_ROWS_OR_COLUMNS type SOTR_CONC value '00155D0122031EE497EDD1FEF21994B2'. "#EC NOTEXT
  constants NOT_IMPLEMENTED type SOTR_CONC value '00155D0122031EE49AB4086262C32BB5'. "#EC NOTEXT
  constants INVALID_NO_OF_CHILD_NAMES type SOTR_CONC value '00155D0122031EE497EDD44A60A274B2'. "#EC NOTEXT
  constants CONTAINER_CREATION_FAILED type SOTR_CONC value '00155D0122031EE497EDDFA33C7E94B2'. "#EC NOTEXT
  constants MIXED_COLUMN_METRIC type SOTR_CONC value '00155D0122031ED4989F40F0F89EE5A4'. "#EC NOTEXT
  constants MIXED_ROW_METRIC type SOTR_CONC value '00155D0122031ED4989F40F0F89EC5A4'. "#EC NOTEXT
  constants UNKNOWN_UNIT_FOR_SIZE type SOTR_CONC value '00155D0122031ED4989D371295B9E5A4'. "#EC NOTEXT
  constants ABSOLUTE_SIZE_REQUIRED type SOTR_CONC value '00155D0122031ED4989D6EAA9A6D05A4'. "#EC NOTEXT
  constants SIZE_REQUIRED type SOTR_CONC value '00155D0122031ED4989D7C13E1E6C5A4'. "#EC NOTEXT
  constants LAST_COLUMN_SIZE_FIXED type SOTR_CONC value '00155D0122031ED4989F355EDFE445A4'. "#EC NOTEXT
  constants LAST_ROW_SIZE_FIXED type SOTR_CONC value '00155D0122031ED4989F355EDFE465A4'. "#EC NOTEXT
  constants METRIC_NOT_SUPPORTED type SOTR_CONC value '00155D0122031ED4989F48C1EDBAA5A4'. "#EC NOTEXT

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



CLASS /GAL/CX_DYNP_LAYOUT_EXCEPTION IMPLEMENTATION.


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
   me->textid = /GAL/CX_DYNP_LAYOUT_EXCEPTION .
 ENDIF.
  endmethod.
ENDCLASS.
