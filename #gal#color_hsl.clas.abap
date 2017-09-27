class /GAL/COLOR_HSL definition
  public
  inheriting from /GAL/COLOR
  create public .

public section.
  type-pools ABAP .

  data H type F read-only .
  data S type F read-only .
  data L type F read-only .

  class-methods FROM_COLOR_RGB
    importing
      !COLOR_RGB type ref to /GAL/COLOR_RGB
    returning
      value(COLOR_HSL) type ref to /GAL/COLOR_HSL .
  class-methods FROM_RGB24_HEX
    importing
      !RGB24_HEX type XSEQUENCE
      !LITTLE_ENDIAN type ABAP_BOOL default ABAP_FALSE
    returning
      value(COLOR_HSL) type ref to /GAL/COLOR_HSL .
  class-methods FROM_RGB24_INT
    importing
      !RGB24_INT type I
      !LITTLE_ENDIAN type ABAP_BOOL default ABAP_TRUE
    returning
      value(COLOR_HSL) type ref to /GAL/COLOR_HSL .
  methods CONSTRUCTOR
    importing
      !H type NUMERIC
      !S type NUMERIC
      !L type NUMERIC .
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



CLASS /GAL/COLOR_HSL IMPLEMENTATION.


METHOD constructor.
  DATA l_type TYPE c.

  super->constructor( ).

  me->h = h - ( 360 * trunc( h / 360 ) ).

  IF me->h < 0.
    me->h = me->h + 360.
  ENDIF.

  DESCRIBE FIELD s TYPE l_type.

  IF l_type CA 'BSI'.
    me->s = s / 255.
  ELSE.
    me->s = s.
  ENDIF.

  IF me->s < 0.
    me->s = 0.
  ELSEIF me->s > 1.
    me->s = 1.
  ENDIF.

  DESCRIBE FIELD l TYPE l_type.

  IF l_type CA 'BSI'.
    me->l = l / 255.
  ELSE.
    me->l = l.
  ENDIF.

  IF me->l < 0.
    me->l = 0.
  ELSEIF me->l > 1.
    me->l = 1.
  ENDIF.
ENDMETHOD.


METHOD from_color_rgb.
  DATA l_min TYPE f.
  DATA l_max TYPE f.

  DATA l_h   TYPE f.
  DATA l_s   TYPE f.
  DATA l_l   TYPE f.

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
    l_h = 60 * ( 0 + ( ( color_rgb->g - color_rgb->b ) / ( l_max - l_min ) ) ).
  ELSEIF l_max = color_rgb->g.
    l_h = 60 * ( 2 + ( ( color_rgb->b - color_rgb->r ) / ( l_max - l_min ) ) ).
  ELSE.
    l_h = 60 * ( 4 + ( ( color_rgb->r - color_rgb->g ) / ( l_max - l_min ) ) ).
  ENDIF.

  IF l_min = 1 or l_max = 0.
    l_s = 0.
  ELSE.
    l_s = ( l_max - l_min ) / ( 1 - abs( l_max + l_min - 1 ) ).
  ENDIF.

  l_l = ( l_max + l_min ) / 2.

  CREATE OBJECT color_hsl
    EXPORTING
      h = l_h
      s = l_s
      l = l_l.
ENDMETHOD.


METHOD from_rgb24_hex.
  DATA l_rgb TYPE REF TO /gal/color_rgb.

  l_rgb = /gal/color_rgb=>from_rgb24_hex( rgb24_hex     = rgb24_hex
                                          little_endian = little_endian ).

  color_hsl = from_color_rgb( l_rgb ).
ENDMETHOD.


METHOD from_rgb24_int.
  DATA l_rgb TYPE REF TO /gal/color_rgb.

  l_rgb = /gal/color_rgb=>from_rgb24_int( rgb24_int     = rgb24_int
                                          little_endian = little_endian ).

  color_hsl = from_color_rgb( l_rgb ).
ENDMETHOD.


METHOD GET_COLOR_FOR_CSS.
  DATA l_rgb24_hex TYPE /gal/color_rgb24_hex.

  l_rgb24_hex = to_rgb24_hex( ).
  color       = l_rgb24_hex.

  CONCATENATE `#` color INTO color.

  TRANSLATE color TO LOWER CASE.
ENDMETHOD.


METHOD to_color_rgb.
*  DEFINE hue_2_rgb.
*    if &3 < 0.
*      &3 = &3 + 1.
*    elseif &3 > 1.
*      &3 = &3 - 1.
*    endif.
*
*    if &3 < 1 / '0.1666666667'.
*      &4 = &1 + ( &2 - &1 ) * 6 * &3.
*    elseif &3 < '0.5'.
*      &4 = &2.
*    elseif &3 < '0.6666666667'.
*      &4 = &1 + ( &2 - &1 ) * ( ( 2 / 3 ) - &3 ) * 6.
*    else.
*      &4 = &1.
*    endif.
*  END-OF-DEFINITION.
*
*  DATA l_v1 TYPE f.
*  DATA l_v2 TYPE f.
*  DATA l_v3 TYPE f.
*
*  DATA l_r  TYPE f.
*  DATA l_g  TYPE f.
*  DATA l_b  TYPE f.
*
*  IF s = 0.
*    l_r = l.
*    l_g = l.
*    l_b = l.
*  ELSE.
*    IF l < '0.5'.
*      l_v2 = l * ( 1 + s ).
*    ELSE.
*      l_v2 = ( l + s ) - ( s * l ).
*    ENDIF.
*
*    l_v1 = 2 * l - l_v2.
*
*    l_v3 = h + ( 1 / 3 ).
*
*    hue_2_rgb l_v1 l_v2 l_v3 l_r.
*
*    l_v3 = h.
*    hue_2_rgb l_v1 l_v2 l_v3 l_g.
*
*    l_v3 = h + 1 / 3.
*    hue_2_rgb l_v1 l_v2 l_v3 l_b.
*  ENDIF.

  DATA l_hd TYPE f.
  DATA l_hi TYPE i.

  DATA l_chroma TYPE f.
  DATA l_min    TYPE f.
  DATA l_temp   TYPE f.

  DATA l_r      TYPE f.
  DATA l_g      TYPE f.
  DATA l_b      TYPE f.

  l_hd = h / 60.
  l_hi = floor( l_hd ).

  WHILE l_hd >= 2.
    l_hd = l_hd - 2.
  ENDWHILE.

  l_chroma  = s * ( 1 - abs( 2 * l - 1 ) ).
  l_min     = l - l_chroma / 2.
  l_temp    = l_chroma * ( 1 - abs( l_hd - 1 ) ).

  l_chroma = l_chroma + l_min.
  l_temp   = l_temp   + l_min.

  CASE l_hi.
    WHEN 0. l_r = l_chroma. l_g = l_temp.   l_b = l_min.
    WHEN 1. l_r = l_temp.   l_g = l_chroma. l_b = l_min.
    WHEN 2. l_r = l_min.    l_g = l_chroma. l_b = l_temp.
    WHEN 3. l_r = l_min.    l_g = l_temp.   l_b = l_chroma.
    WHEN 4. l_r = l_temp.   l_g = l_min.    l_b = l_chroma.
    WHEN 5. l_r = l_chroma. l_g = l_min.    l_b = l_temp.
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
