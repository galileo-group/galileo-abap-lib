class /GAL/LENGTH definition
  public
  create public .

public section.
  type-pools ABAP .

  constants UNIT_CM type STRING value `cm`. "#EC NOTEXT
  constants UNIT_EM type STRING value `em`. "#EC NOTEXT
  constants UNIT_IN type STRING value `in`. "#EC NOTEXT
  constants UNIT_MM type STRING value `mm`. "#EC NOTEXT
  constants UNIT_PC type STRING value `pc`. "#EC NOTEXT
  constants UNIT_PERCENT type STRING value `%`. "#EC NOTEXT
  constants UNIT_PT type STRING value `pt`. "#EC NOTEXT
  constants UNIT_PX type STRING value `px`. "#EC NOTEXT
  constants UNIT_UNDEFINED type STRING value `Undefined`. "#EC NOTEXT
  data UNIT type STRING read-only .
  data VALUE type F read-only .

  methods CONSTRUCTOR
    importing
      !VALUE type NUMERIC
      !UNIT type STRING default UNIT_UNDEFINED .
  methods CONVERT
    importing
      !UNIT type STRING optional
      !RESOLUTION type NUMERIC optional
      !TOTAL_LENGTH type ref to /GAL/LENGTH optional
    returning
      value(LENGTH) type ref to /GAL/LENGTH
    raising
      /GAL/CX_CONVERSION_EXCEPTION .
  methods GET_LENGTH_FOR_CSS
    returning
      value(LENGTH) type STRING
    raising
      /GAL/CX_CONVERSION_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS /GAL/LENGTH IMPLEMENTATION.


METHOD CONSTRUCTOR.
  me->unit  = unit.
  me->value = value.
ENDMETHOD.


METHOD convert.
  DATA l_length_mm       LIKE value.
  DATA l_length_conv     LIKE value.
  DATA l_total_length_mm TYPE REF TO /gal/length.

* No conversion necessary
  IF unit = me->unit.
    length = me.
    EXIT.
  ENDIF.

* Convert to millimeters
  CASE me->unit.

    WHEN unit_cm.
      l_length_mm = value * 10.

    WHEN unit_in.
      l_length_mm = value * '25.4'.

    WHEN unit_mm.
      l_length_mm = value.

    WHEN unit_pc.
      l_length_mm = value * '4.2175176'.

    WHEN unit_percent.
      IF total_length IS INITIAL.
        RAISE EXCEPTION TYPE /gal/cx_conversion_exception
          EXPORTING
            textid = /gal/cx_conversion_exception=>total_length_required
            var1   = me->unit
            var2   = unit.
      ELSE.
        l_total_length_mm = total_length->convert( unit       = unit_mm
                                                   resolution = resolution ).

        l_length_mm = l_total_length_mm->value * value / 100.
      ENDIF.

    WHEN unit_pt.
      l_length_mm = value * '0.3514598035'.

    WHEN unit_px.
      IF resolution IS INITIAL.
        RAISE EXCEPTION TYPE /gal/cx_conversion_exception
          EXPORTING
            textid = /gal/cx_conversion_exception=>resolution_required
            var1   = me->unit
            var2   = unit.
      ELSE.
        l_length_mm = value * '25.4' / resolution.
      ENDIF.

    WHEN OTHERS.
      RAISE EXCEPTION TYPE /gal/cx_conversion_exception
        EXPORTING
          textid = /gal/cx_conversion_exception=>conversion_not_supported
          var1   = me->unit
          var2   = unit.

  ENDCASE.

* Convert to target unit of measure
  CASE unit.

    WHEN unit_cm.
      l_length_conv = l_length_mm / 10.

    WHEN unit_in.
      l_length_conv = l_length_mm / '25.4'.

    WHEN unit_mm.
      l_length_conv = l_length_mm.

    WHEN unit_pc.
      l_length_conv = l_length_mm * '4.2175176'.

    WHEN unit_percent.
      IF total_length IS INITIAL.
        RAISE EXCEPTION TYPE /gal/cx_conversion_exception
          EXPORTING
            textid = /gal/cx_conversion_exception=>total_length_required
            var1   = me->unit
            var2   = unit.
      ELSE.
        l_total_length_mm = total_length->convert( unit       = unit_mm
                                                   resolution = resolution ).

        l_length_conv = l_length_mm * 100 / l_total_length_mm->value.
      ENDIF.

    WHEN unit_pt.
      l_length_conv = l_length_mm * '0.3514598035'.

    WHEN unit_px.
      IF resolution IS INITIAL.
        RAISE EXCEPTION TYPE /gal/cx_conversion_exception
          EXPORTING
            textid = /gal/cx_conversion_exception=>resolution_required
            var1   = me->unit
            var2   = unit.
      ELSE.
        l_length_conv = l_length_mm / '25.4' * resolution.
      ENDIF.

    WHEN OTHERS.
      RAISE EXCEPTION TYPE /gal/cx_conversion_exception
        EXPORTING
          textid = /gal/cx_conversion_exception=>conversion_not_supported
          var1   = me->unit
          var2   = unit.

  ENDCASE.

  CREATE OBJECT length
    EXPORTING
      value = l_length_conv
      unit  = unit.
ENDMETHOD.


METHOD get_length_for_css.
  DATA l_value TYPE p DECIMALS 3.

  IF unit = unit_cm      OR
     unit = unit_em      OR
     unit = unit_in      OR
     unit = unit_mm      OR
     unit = unit_pc      OR
     unit = unit_percent OR
     unit = unit_pt      OR
     unit = unit_px.

    l_value = value.

    length = l_value.

    length = /gal/string=>trim_left( length ).

    length = /gal/string=>trim_right( input            = length
                                      whitespace_chars = ` 0` ).

    length = /gal/string=>trim_right( input            = length
                                      whitespace_chars = `.` ).

    CONCATENATE length unit INTO length.
  ELSE.
    RAISE EXCEPTION TYPE /gal/cx_conversion_exception
      EXPORTING
        textid = /gal/cx_conversion_exception=>unit_not_supported_by_css
        var1   = unit.
  ENDIF.
ENDMETHOD.
ENDCLASS.
