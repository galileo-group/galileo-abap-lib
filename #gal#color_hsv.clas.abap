class /GAL/COLOR_HSV definition
  public
  inheriting from /GAL/COLOR
  create public .

public section.
  type-pools ABAP .

  data H type F read-only .
  data S type F read-only .
  data V type F read-only .

  class-methods FROM_COLOR_RGB
    importing
      !COLOR_RGB type ref to /GAL/COLOR_RGB
    returning
      value(COLOR_HSV) type ref to /GAL/COLOR_HSV .
  class-methods FROM_RGB24_HEX
    importing
      !RGB24_HEX type XSEQUENCE
      !LITTLE_ENDIAN type ABAP_BOOL default ABAP_FALSE
    returning
      value(COLOR_HSV) type ref to /GAL/COLOR_HSV .
  class-methods FROM_RGB24_INT
    importing
      !RGB24_INT type I
      !LITTLE_ENDIAN type ABAP_BOOL default ABAP_TRUE
    returning
      value(COLOR_HSV) type ref to /GAL/COLOR_HSV .
  methods CONSTRUCTOR
    importing
      !H type NUMERIC
      !S type NUMERIC
      !V type NUMERIC .
  methods GET_COLOR_FOR_CSS
    returning
      value(COLOR) type STRING .
  methods TO_COLOR_RGB
    returning
      value(COLOR_RGB) type ref to /GAL/COLOR_RGB .
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



CLASS /GAL/COLOR_HSV IMPLEMENTATION.


METHOD constructor.
  DATA l_type TYPE c.

  super->constructor( ).

  me->h = h - ( 360 * trunc( h / 360 ) ).                "#EC NUMBER_OK

  IF me->h < 0.
    me->h = me->h + 360.                                 "#EC NUMBER_OK
  ENDIF.

  DESCRIBE FIELD s TYPE l_type.

  IF l_type CA 'BSI'.
    me->s = s / 255.                                     "#EC NUMBER_OK
  ELSE.
    me->s = s.
  ENDIF.

  IF me->s < 0.
    me->s = 0.
  ELSEIF me->s > 1.
    me->s = 1.
  ENDIF.

  DESCRIBE FIELD v TYPE l_type.

  IF l_type CA 'BSI'.
    me->v = v / 255.                                     "#EC NUMBER_OK
  ELSE.
    me->v = v.
  ENDIF.

  IF me->v < 0.
    me->v = 0.
  ELSEIF me->v > 1.
    me->v = 1.
  ENDIF.
ENDMETHOD.


METHOD from_color_rgb.
  DATA l_min TYPE f.
  DATA l_max TYPE f.

  DATA l_h   TYPE f.
  DATA l_s   TYPE f.
  DATA l_v   TYPE f.

  l_min = color_rgb->r.
  l_max = color_rgb->r.

  IF color_rgb->g < l_min.
    l_min = color_rgb->g.
  ELSEIF color_rgb->g > l_max.
    l_max = color_rgb->g.
  ENDIF.

  IF color_rgb->b < l_min.
    l_min = color_rgb->b.
  ELSEIF color_rgb->b > l_max.
    l_max = color_rgb->b.
  ENDIF.

  IF l_max = l_min.
    l_h = 0.
  ELSEIF l_max = color_rgb->r.
    l_h = 60 * ( 0 + ( ( color_rgb->g - color_rgb->b ) / ( l_max - l_min ) ) ). "#EC NUMBER_OK
  ELSEIF l_max = color_rgb->g.
    l_h = 60 * ( 2 + ( ( color_rgb->b - color_rgb->r ) / ( l_max - l_min ) ) ). "#EC NUMBER_OK
  ELSE.
    l_h = 60 * ( 4 + ( ( color_rgb->r - color_rgb->g ) / ( l_max - l_min ) ) ). "#EC NUMBER_OK
  ENDIF.

  IF l_max = 0.
    l_s = 0.
  ELSE.
    l_s = ( l_max - l_min ) / l_max.
  ENDIF.

  l_v = l_max.

  CREATE OBJECT color_hsv
    EXPORTING
      h = l_h
      s = l_s
      v = l_v.
ENDMETHOD.


METHOD from_rgb24_hex.
  DATA l_rgb TYPE REF TO /gal/color_rgb.

  l_rgb = /gal/color_rgb=>from_rgb24_hex( rgb24_hex     = rgb24_hex
                                          little_endian = little_endian ).

  color_hsv = from_color_rgb( l_rgb ).
ENDMETHOD.


METHOD from_rgb24_int.
  DATA l_rgb TYPE REF TO /gal/color_rgb.

  l_rgb = /gal/color_rgb=>from_rgb24_int( rgb24_int     = rgb24_int
                                          little_endian = little_endian ).

  color_hsv = from_color_rgb( l_rgb ).
ENDMETHOD.


METHOD GET_COLOR_FOR_CSS.
  DATA l_rgb24_hex TYPE /gal/color_rgb24_hex.

  l_rgb24_hex = to_rgb24_hex( ).
  color       = l_rgb24_hex.

  CONCATENATE `#` color INTO color.

  TRANSLATE color TO LOWER CASE.
ENDMETHOD.


METHOD to_color_rgb.
  DATA l_hi TYPE i.

  DATA l_f  TYPE f.
  DATA l_p  TYPE f.
  DATA l_q  TYPE f.
  DATA l_t  TYPE f.

  DATA l_r  TYPE f.
  DATA l_g  TYPE f.
  DATA l_b  TYPE f.

  l_hi = floor( h / 60 ).                                "#EC NUMBER_OK
  l_f  = h / 60 - l_hi.                                  "#EC NUMBER_OK
  l_p  = v * ( 1 - s ).
  l_q  = v * ( 1 - s * l_f ).
  l_t  = v * ( 1 - s * ( 1 - l_f ) ).

  CASE l_hi.
    WHEN 0. l_r = v.   l_g = l_t. l_b = l_p.
    WHEN 1. l_r = l_q. l_g = v.   l_b = l_p.
    WHEN 2. l_r = l_p. l_g = v.   l_b = l_t.
    WHEN 3. l_r = l_p. l_g = l_q. l_b = v.
    WHEN 4. l_r = l_t. l_g = l_p. l_b = v.
    WHEN 5. l_r = v.   l_g = l_p. l_b = l_q.
  ENDCASE.

  CREATE OBJECT color_rgb
    EXPORTING
      r = l_r
      g = l_g
      b = l_b.
ENDMETHOD.


METHOD to_rgb24_hex.
  DATA l_rgb TYPE REF TO /gal/color_rgb.

  l_rgb = to_color_rgb( ).

  rgb24_hex = l_rgb->to_rgb24_hex( little_endian = little_endian ).
ENDMETHOD.


METHOD to_rgb24_int.
  DATA l_rgb TYPE REF TO /gal/color_rgb.

  l_rgb = to_color_rgb( ).

  rgb24_int = l_rgb->to_rgb24_int( little_endian = little_endian ).
ENDMETHOD.
ENDCLASS.
