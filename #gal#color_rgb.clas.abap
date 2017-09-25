class /GAL/COLOR_RGB definition
  public
  inheriting from /GAL/COLOR
  create public .

public section.
  type-pools ABAP .

  data R type F read-only .
  data G type F read-only .
  data B type F read-only .

  class-methods FROM_RGB24_HEX
    importing
      !RGB24_HEX type XSEQUENCE
      !LITTLE_ENDIAN type ABAP_BOOL default ABAP_FALSE
    returning
      value(COLOR_RGB) type ref to /GAL/COLOR_RGB .
  class-methods FROM_RGB24_INT
    importing
      !RGB24_INT type I
      !LITTLE_ENDIAN type ABAP_BOOL default ABAP_TRUE
    returning
      value(COLOR_RGB) type ref to /GAL/COLOR_RGB .
  methods CONSTRUCTOR
    importing
      !R type NUMERIC
      !G type NUMERIC
      !B type NUMERIC .
  methods GET_COLOR_FOR_CSS
    returning
      value(COLOR) type STRING .
  methods MIX
    importing
      !COLOR_RGB type ref to /GAL/COLOR_RGB
      !RATIO type NUMERIC default '0.5'
    returning
      value(RESULT) type ref to /GAL/COLOR_RGB .
  methods TO_RGB24_HEX
    importing
      !LITTLE_ENDIAN type ABAP_BOOL default ABAP_FALSE
    returning
      value(RGB24_HEX) type /GAL/COLOR_RGB24_HEX .
  methods TO_RGB24_INT
    importing
      !LITTLE_ENDIAN type ABAP_BOOL default ABAP_TRUE
    returning
      value(RGB24_INT) type I .
protected section.
private section.
ENDCLASS.



CLASS /GAL/COLOR_RGB IMPLEMENTATION.


METHOD constructor.
  DATA l_type TYPE c.

  super->constructor( ).

  DESCRIBE FIELD r TYPE l_type.

  IF l_type CA 'BSI'.
    me->r = r / 255.
  ELSE.
    me->r = r.
  ENDIF.

  IF me->r < 0.
    me->r = 0.
  ELSEIF me->r > 1.
    me->r = 1.
  ENDIF.

  DESCRIBE FIELD g TYPE l_type.

  IF l_type CA 'BSI'.
    me->g = g / 255.
  ELSE.
    me->g = g.
  ENDIF.

  IF me->g < 0.
    me->g = 0.
  ELSEIF me->g > 1.
    me->g = 1.
  ENDIF.

  DESCRIBE FIELD b TYPE l_type.

  IF l_type CA 'BSI'.
    me->b = b / 255.
  ELSE.
    me->b = b.
  ENDIF.

  IF me->b < 0.
    me->b = 0.
  ELSEIF me->b > 1.
    me->b = 1.
  ENDIF.
ENDMETHOD.


METHOD from_rgb24_hex.
  DATA l_hex(3) TYPE x.
  DATA l_offset TYPE i.

  DATA r TYPE i.
  DATA g TYPE i.
  DATA b TYPE i.

  l_offset = xstrlen( rgb24_hex ) - 3.

  IF l_offset > 0.
    l_hex = rgb24_hex+l_offset.
  ELSE.
    l_hex = rgb24_hex.
  ENDIF.

  IF little_endian = abap_true.
    r = l_hex+2(1).
    g = l_hex+1(1).
    b = l_hex(1).
  ELSE.
    r = l_hex(1).
    g = l_hex+1(1).
    b = l_hex+2(1).
  ENDIF.

  CREATE OBJECT color_rgb
    EXPORTING
      r = r
      g = g
      b = b.
ENDMETHOD.


METHOD from_rgb24_int.
  DATA l_hex(4) TYPE x.

  l_hex = rgb24_int.

  color_rgb = from_rgb24_hex( rgb24_hex     = l_hex
                              little_endian = little_endian ).
ENDMETHOD.


METHOD get_color_for_css.
  DATA l_rgb24_hex TYPE /gal/color_rgb24_hex.

  l_rgb24_hex = to_rgb24_hex( ).
  color       = l_rgb24_hex.

  CONCATENATE `#` color INTO color.

  TRANSLATE color TO LOWER CASE.
ENDMETHOD.


METHOD mix.
  DATA l_f1 TYPE f.
  DATA l_f2 TYPE f.

  DATA l_r TYPE f.
  DATA l_g TYPE f.
  DATA l_b TYPE f.

  IF ratio < 0.
    l_f1 = 0.
  ELSEIF ratio > 1.
    l_f1 = 1.
  ELSE.
    l_f1 = ratio.
  ENDIF.

  l_f2 = 1 - l_f1.

  l_r = r * l_f1 + color_rgb->r * l_f2.
  l_g = g * l_f1 + color_rgb->g * l_f2.
  l_b = b * l_f1 + color_rgb->b * l_f2.

  CREATE OBJECT result
    EXPORTING
      r = l_r
      g = l_g
      b = l_b.
ENDMETHOD.


METHOD to_rgb24_hex.
  DATA rf TYPE f.
  DATA gf TYPE f.
  DATA bf TYPE f.

  DATA rx TYPE x.
  DATA gx TYPE x.
  DATA bx TYPE x.

  IF r < 0.
    rf = 0.
  ELSEIF r > 1.
    rf = 1.
  ELSE.
    rf = r.
  ENDIF.

  IF g < 0.
    gf = 0.
  ELSEIF g > 1.
    gf = 1.
  ELSE.
    gf = g.
  ENDIF.

  IF b < 0.
    bf = 0.
  ELSEIF b > 1.
    bf = 1.
  ELSE.
    bf = b.
  ENDIF.

  rx = rf * 255.
  gx = gf * 255.
  bx = bf * 255.

  IF little_endian = abap_true.
    rgb24_hex+2(1) = rx.
    rgb24_hex+1(1) = gx.
    rgb24_hex(1)   = bx.
  ELSE.
    rgb24_hex(1)   = rx.
    rgb24_hex+1(1) = gx.
    rgb24_hex+2(1) = bx.
  ENDIF.
ENDMETHOD.


METHOD to_rgb24_int.
  DATA l_rgb24_hex TYPE /gal/color_rgb24_hex.

  l_RGB24_hex = to_rgb24_hex( little_endian = little_endian ).

  rgb24_int = l_rgb24_hex.
ENDMETHOD.
ENDCLASS.
