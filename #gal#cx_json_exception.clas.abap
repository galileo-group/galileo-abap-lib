class /GAL/CX_JSON_EXCEPTION definition
  public
  inheriting from /GAL/CX_EXCEPTION
  final
  create public .

public section.
  type-pools ABAP .

  constants /GAL/CX_JSON_EXCEPTION type SOTR_CONC value '00155D0122031ED681ABF0929C6E931E'. "#EC NOTEXT
  constants MISSING_CLOSING_QUOTE type SOTR_CONC value '00155D0122031ED681ABF0929C6EB31E'. "#EC NOTEXT
  constants INVALID_DOCUMENT_STRUCTURE type SOTR_CONC value '00155D0122031ED681AC3F04992D531E'. "#EC NOTEXT
  constants VALUE_NOT_SUPPORTED type SOTR_CONC value '00155D0122031ED681AC54FF8C79931E'. "#EC NOTEXT
  constants DUPLICATE_ATTRIBUTE_NAME type SOTR_CONC value '00155D0122031ED681ACC27971A8131E'. "#EC NOTEXT
  constants UNEXPECTED_TOKEN type SOTR_CONC value '00155D0122031ED681ACD75D28C3D31E'. "#EC NOTEXT
  constants INVALID_ATTRIBUTE_NAME type SOTR_CONC value '00155D0122031ED681AD06EC354CF31E'. "#EC NOTEXT
  constants NO_MATCHING_CHILD_ELEMENT type SOTR_CONC value '00155D0122031EE681C1C536567E19D4'. "#EC NOTEXT
  constants ELEMENT_TYPE_NOT_SUPPORTED type SOTR_CONC value '00155D0122031EE681C57B9D865719D4'. "#EC NOTEXT
  constants CANNOT_DETERMINE_JSON_TYPE type SOTR_CONC value '00155DF935091ED995EE8F979D3C56CC'. "#EC NOTEXT
  constants CANNOT_DETERMINE_TYPE type SOTR_CONC value '00155DF935091ED995F5AC98210856CC'. "#EC NOTEXT
  constants NO_HANDLER_FOR_DATA_TYPE type SOTR_CONC value '00155DF935091ED996BC35806203F6CC'. "#EC NOTEXT
  constants CONV_FORMAT_ERROR_TIME type SOTR_CONC value '00155DF935091ED996D8BE88E7F096CC'. "#EC NOTEXT
  constants CONV_FORMAT_ERROR_DATE type SOTR_CONC value '00155DF935091ED996D92FFDDF33B6CC'. "#EC NOTEXT
  constants CONV_FORMAT_ERROR_TIMESTAMP type SOTR_CONC value '00155DF935091ED996DADCABB13EB6CC'. "#EC NOTEXT
  constants UNKNOWN_STRUCTURE_FIELD type SOTR_CONC value '00155DF935091ED996F09723323596CC'. "#EC NOTEXT

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



CLASS /GAL/CX_JSON_EXCEPTION IMPLEMENTATION.


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
   me->textid = /GAL/CX_JSON_EXCEPTION .
 ENDIF.
  endmethod.
ENDCLASS.
