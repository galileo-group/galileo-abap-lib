class /GAL/FONT_INFO definition
  public
  create public .

public section.
  type-pools ABAP .

  data NAME type STRING read-only .
  data SIZE type ref to /GAL/LENGTH read-only .

  methods CONSTRUCTOR
    importing
      !NAME type CSEQUENCE
      !SIZE type ref to /GAL/LENGTH optional
      !SIZE_VALUE type NUMERIC optional
      !SIZE_UNIT type STRING optional .
  methods GET_FONT_NAME_FOR_CSS
    importing
      !ALTERNATIVE_FONT_NAMES type STRING optional
    returning
      value(FONT_NAME) type STRING .
  methods GET_FONT_SIZE_FOR_CSS
    returning
      value(FONT_SIZE) type STRING
    raising
      /GAL/CX_CONVERSION_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS /GAL/FONT_INFO IMPLEMENTATION.


METHOD CONSTRUCTOR.
  me->name = name.

  IF size IS NOT INITIAL.
    me->size = size.
  ELSE.
    CREATE OBJECT me->size
      EXPORTING
        value = size_value
        unit  = size_unit.
  ENDIF.
ENDMETHOD.


METHOD get_font_name_for_css.
  CONCATENATE `"` name `"` INTO font_name.

  IF alternative_font_names IS NOT INITIAL.
    CONCATENATE name alternative_font_names
           INTO font_name SEPARATED BY `, `.
  ENDIF.
ENDMETHOD.


METHOD get_font_size_for_css.
  font_size = size->get_length_for_css( ).
ENDMETHOD.
ENDCLASS.
