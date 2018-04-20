class /GAL/JAVASCRIPT_CONVERTER definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  data STRING_DELIMITER type C value ''''. "#EC NOTEXT

  methods BOOLEAN_TO_JAVASCRIPT
    importing
      !INPUT type ABAP_BOOL
    returning
      value(OUTPUT) type STRING .
  methods FLOAT_TO_JAVASCRIPT
    importing
      !INPUT type F
    returning
      value(OUTPUT) type STRING .
  methods INTEGER_TO_JAVASCRIPT
    importing
      !INPUT type I
    returning
      value(OUTPUT) type STRING .
  methods JAVASCRIPT_TO_BOOLEAN
    importing
      !INPUT type STRING
    returning
      value(OUTPUT) type ABAP_BOOL .
  methods JAVASCRIPT_TO_FLOAT
    importing
      !INPUT type STRING
    returning
      value(OUTPUT) type F .
  methods JAVASCRIPT_TO_INTEGER
    importing
      !INPUT type STRING
    returning
      value(OUTPUT) type I
    raising
      CX_SY_CONVERSION_OVERFLOW .
  methods JAVASCRIPT_TO_STRING
    importing
      !INPUT type STRING
    returning
      value(OUTPUT) type STRING .
  methods STRING_TO_JAVASCRIPT
    importing
      !INPUT type STRING
    returning
      value(OUTPUT) type STRING .
protected section.
private section.
ENDCLASS.



CLASS /GAL/JAVASCRIPT_CONVERTER IMPLEMENTATION.


METHOD boolean_to_javascript.
  IF input CA 'XYJ1'.
    output = /gal/javascript_constants=>true.
  ELSE.
    output = /gal/javascript_constants=>false.
  ENDIF.
ENDMETHOD.


METHOD float_to_javascript.
  DATA l_sign_int  TYPE i.
  DATA l_trunc_int TYPE i.
  DATA l_frac_int  TYPE i.

  DATA l_sign_str  TYPE string.
  DATA l_trunc_str TYPE string.
  DATA l_frac_str  TYPE string.

* Split number into sign, integer and fraction parts
  l_sign_int  = sign( input ).
  l_trunc_int = trunc( input * sign( input ) ).
  l_frac_int  = frac( input * sign( input ) ) * 100000000. "#EC NUMBER_OK

* Convert parts to string
  IF l_sign_int < 0.
    l_sign_str = `-`.
  ENDIF.

  l_trunc_str = l_trunc_int.
  l_frac_str  = l_frac_int.

  SHIFT l_trunc_str RIGHT DELETING TRAILING space.
  SHIFT l_trunc_str LEFT DELETING LEADING space.

  SHIFT l_frac_str RIGHT DELETING TRAILING space.
  SHIFT l_frac_str RIGHT DELETING TRAILING '0'.
  SHIFT l_frac_str LEFT DELETING LEADING space.

* Build number in javascript format
  IF l_frac_str IS INITIAL.
    CONCATENATE l_sign_str l_trunc_str INTO output.
  ELSE.
    CONCATENATE l_sign_str l_trunc_str '.' l_frac_str INTO output.
  ENDIF.
ENDMETHOD.


METHOD integer_to_javascript.
  DATA l_value  TYPE i.
  DATA l_length TYPE i.

  IF input >= 0.
    output = input.
  ELSE.
    l_value = - input.

    output = l_value.

    CONCATENATE `-` output INTO output.
  ENDIF.

  l_length = numofchar( output ).

  output = output(l_length).
ENDMETHOD.


METHOD javascript_to_boolean.
  IF input = /gal/javascript_constants=>true.
    output = abap_true.
  ELSE.
    output = abap_false.
  ENDIF.
ENDMETHOD.


METHOD javascript_to_float.
  output = input.
ENDMETHOD.


METHOD javascript_to_integer.
  output = input.
ENDMETHOD.


METHOD javascript_to_string.
  CONSTANTS lc_hex_chars TYPE string VALUE `0123456789abcdefABCDEF`. "#EC NOTEXT
  CONSTANTS lc_oct_chars TYPE string VALUE `01234567`.      "#EC NOTEXT

  DATA l_offset           TYPE i.
  DATA l_length           TYPE i.

  DATA l_temp_offset      TYPE i.

  DATA l_unicode_i        TYPE i.
  DATA l_unicode_hex_c(4) TYPE c.
  DATA l_unicode_hex_x(2) TYPE x.
  DATA l_unicode_char     TYPE c.

  DATA l_unicode_oct_c(3) TYPE c.

* Check if there is something to convert
  IF input IS INITIAL.
    RETURN.
  ENDIF.

* Remove quotes
  l_offset = strlen( input ) - 1.

  IF l_offset > 1 AND input(1) CA `'"` AND input+l_offset(1) = input(1).
    l_offset = 1.
    l_length = strlen( input ) - 2.
  ELSE.
    l_offset = 0.
    l_length = strlen( input ).
  ENDIF.

* Replace special characters
  WHILE l_length > 1 AND input+l_offset CA '\'.
    CONCATENATE output input+l_offset(sy-fdpos) INTO output RESPECTING BLANKS.

    l_offset = l_offset + sy-fdpos.
    l_length = l_length - sy-fdpos.

    CASE input+l_offset(2).

      WHEN '\\' OR '\"' OR '\'''.
        l_offset = l_offset + 1.
        CONCATENATE output input+l_offset(1) INTO output RESPECTING BLANKS.
        l_offset = l_offset + 1.
        l_length = l_length - 2.

      WHEN '\v'.
        CONCATENATE output cl_abap_char_utilities=>vertical_tab INTO output RESPECTING BLANKS.
        l_offset = l_offset + 2.
        l_length = l_length - 2.

      WHEN '\r'.
        CONCATENATE output cl_abap_char_utilities=>cr_lf(1) INTO output RESPECTING BLANKS.
        l_offset = l_offset + 2.
        l_length = l_length - 2.

      WHEN '\t'.
        CONCATENATE output cl_abap_char_utilities=>horizontal_tab INTO output RESPECTING BLANKS.
        l_offset = l_offset + 2.
        l_length = l_length - 2.

      WHEN '\n'.
        CONCATENATE output cl_abap_char_utilities=>newline INTO output RESPECTING BLANKS.
        l_offset = l_offset + 2.
        l_length = l_length - 2.

      WHEN '\f'.
        CONCATENATE output cl_abap_char_utilities=>form_feed INTO output RESPECTING BLANKS.
        l_offset = l_offset + 2.
        l_length = l_length - 2.

      WHEN '\b'.
        CONCATENATE output cl_abap_char_utilities=>backspace INTO output RESPECTING BLANKS.
        l_offset = l_offset + 2.
        l_length = l_length - 2.

      WHEN '\u'.
        l_temp_offset = l_offset + 2.

        IF l_length >= 6 AND input+l_temp_offset(4) CO lc_hex_chars.
          l_unicode_hex_c = input+l_temp_offset(4).

          TRANSLATE l_unicode_hex_c TO UPPER CASE.

          l_unicode_hex_x = l_unicode_hex_c.
          l_unicode_char  = cl_abap_conv_in_ce=>uccp( l_unicode_hex_x ).

          CONCATENATE output l_unicode_char INTO output RESPECTING BLANKS.
          l_offset = l_offset + 6.
          l_length = l_length - 6.
        ELSE.
          CONCATENATE output input+l_offset(2) INTO output RESPECTING BLANKS.
          l_offset = l_offset + 2.
          l_length = l_length - 2.
        ENDIF.

      WHEN '\x'.
        l_temp_offset = l_offset + 2.

        IF l_length >= 4 AND input+l_temp_offset(2) CO lc_hex_chars.
          CONCATENATE '00' input+l_temp_offset(2) INTO l_unicode_hex_c.

          TRANSLATE l_unicode_hex_c TO UPPER CASE.

          l_unicode_hex_x = l_unicode_hex_c.
          l_unicode_char  = cl_abap_conv_in_ce=>uccp( l_unicode_hex_x ).

          CONCATENATE output l_unicode_char INTO output RESPECTING BLANKS.
          l_offset = l_offset + 4.
          l_length = l_length - 4.
        ELSE.
          CONCATENATE output input+l_offset(2) INTO output RESPECTING BLANKS.
          l_offset = l_offset + 2.
          l_length = l_length - 2.
        ENDIF.

      WHEN '\0' OR '\1' OR '\2' OR '\3' OR '\4' OR '\5' OR '\6' OR '\7'.
        l_temp_offset = l_offset + 1.

        IF l_length >= 4 AND input+l_temp_offset(3) CO lc_oct_chars AND input+l_temp_offset(3) <= '377'.
          l_unicode_oct_c = input+l_temp_offset(3).
          l_unicode_i     = l_unicode_oct_c(1) * 64 + l_unicode_oct_c+1(1) * 8 + l_unicode_oct_c+2(1).
          l_unicode_hex_x = l_unicode_i.
          l_unicode_char  = cl_abap_conv_in_ce=>uccp( l_unicode_hex_x ).

          CONCATENATE output l_unicode_char INTO output RESPECTING BLANKS.
          l_offset = l_offset + 4.
          l_length = l_length - 4.
        ELSEIF l_length >= 3 AND input+l_temp_offset(2) CO lc_oct_chars.
          l_unicode_oct_c = input+l_temp_offset(2).
          l_unicode_i     = l_unicode_oct_c(1) * 8 + l_unicode_oct_c+1(1).
          l_unicode_hex_x = l_unicode_i.
          l_unicode_char  = cl_abap_conv_in_ce=>uccp( l_unicode_hex_x ).

          CONCATENATE output l_unicode_char INTO output RESPECTING BLANKS.
          l_offset = l_offset + 3.
          l_length = l_length - 3.
        ELSEIF l_length >= 2 AND input+l_temp_offset(1) CO lc_oct_chars.
          l_unicode_oct_c = input+l_temp_offset(1).
          l_unicode_i     = l_unicode_oct_c(1).
          l_unicode_hex_x = l_unicode_i.
          l_unicode_char  = cl_abap_conv_in_ce=>uccp( l_unicode_hex_x ).

          CONCATENATE output l_unicode_char INTO output RESPECTING BLANKS.
          l_offset = l_offset + 2.
          l_length = l_length - 2.
        ELSE.
          CONCATENATE output input+l_offset(2) INTO output RESPECTING BLANKS.
          l_offset = l_offset + 2.
          l_length = l_length - 2.
        ENDIF.

      WHEN OTHERS.
        CONCATENATE output input+l_offset(2) INTO output RESPECTING BLANKS.
        l_offset = l_offset + 2.
        l_length = l_length - 2.

    ENDCASE.
  ENDWHILE.

* Add Remaining part of string to output
  IF l_length > 0.
    CONCATENATE output input+l_offset(l_length) INTO output RESPECTING BLANKS.
  ENDIF.
ENDMETHOD.


METHOD string_to_javascript.
  DATA l_escaped_delimiter(2) TYPE c.

  l_escaped_delimiter+0(1) = '\'.
  l_escaped_delimiter+1(1) = string_delimiter.

  output = input.

  REPLACE ALL OCCURRENCES OF '\'                                    IN output WITH '\\'.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>backspace      IN output WITH '\b'.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed      IN output WITH '\f'.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline        IN output WITH '\n'.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf(1)       IN output WITH '\r'.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN output WITH '\t'.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>vertical_tab   IN output WITH '\v'.
  REPLACE ALL OCCURRENCES OF string_delimiter                       IN output WITH l_escaped_delimiter.

  CONCATENATE string_delimiter output string_delimiter INTO output RESPECTING BLANKS.
ENDMETHOD.
ENDCLASS.
