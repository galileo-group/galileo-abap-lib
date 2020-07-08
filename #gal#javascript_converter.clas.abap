"! <p class="shorttext synchronized" lang="en">Converter for JavaScript Literals</p>
CLASS /gal/javascript_converter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPE-POOLS:
      abap.

    DATA:
    "! <p class="shorttext synchronized" lang="en">String delimiter</p>
      string_delimiter TYPE c VALUE '"'.                    "#EC NOTEXT

    METHODS:

      "! <p class="shorttext synchronized" lang="en">Conversion: Boolean -&gt; JavaScript</p>
      "!
      "! @parameter input  | <p class="shorttext synchronized" lang="en">Value to convert</p>
      "! @parameter output | <p class="shorttext synchronized" lang="en">Converted value</p>
      boolean_to_javascript
        IMPORTING
          !input        TYPE abap_bool
        RETURNING
          VALUE(output) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Conversion: Date -&gt; JavaScript</p>
      "!
      "! @parameter input  | <p class="shorttext synchronized" lang="en">Value to convert</p>
      "! @parameter output | <p class="shorttext synchronized" lang="en">Converted value</p>
      date_to_javascript
        IMPORTING
          !input        TYPE d
        RETURNING
          VALUE(output) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Conversion: JavaScript -&gt; Boolean</p>
      "!
      "! @parameter input  | <p class="shorttext synchronized" lang="en">Value to convert</p>
      "! @parameter output | <p class="shorttext synchronized" lang="en">Converted value</p>
      javascript_to_boolean
        IMPORTING
          !input        TYPE string
        RETURNING
          VALUE(output) TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">Conversion: JavaScript -&gt; Date</p>
      "!
      "! @parameter input                | <p class="shorttext synchronized" lang="en">Value to convert</p>
      "! @parameter output               | <p class="shorttext synchronized" lang="en">Converted value</p>
      "! @raising /gal/cx_json_exception | <p class="shorttext synchronized" lang="en">Conversion error</p>
      javascript_to_date
        IMPORTING
          !input        TYPE string
        RETURNING
          VALUE(output) TYPE d
        RAISING
          /gal/cx_json_exception,

      "! <p class="shorttext synchronized" lang="en">Conversion: JavaScript -&gt; Number</p>
      "!
      "! @parameter input  | <p class="shorttext synchronized" lang="en">Value to convert</p>
      "! @parameter output | <p class="shorttext synchronized" lang="en">Converted value</p>
      javascript_to_number
        IMPORTING
          !input        TYPE string
        RETURNING
          VALUE(output) TYPE /gal/javascript_number,

      "! <p class="shorttext synchronized" lang="en">Conversion: JavaScript -&gt; String</p>
      "!
      "! @parameter input  | <p class="shorttext synchronized" lang="en">Value to convert</p>
      "! @parameter output | <p class="shorttext synchronized" lang="en">Converted value</p>
      javascript_to_string
        IMPORTING
          !input        TYPE string
        RETURNING
          VALUE(output) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Conversion: JavaScript -&gt; Time</p>
      "!
      "! @parameter input                | <p class="shorttext synchronized" lang="en">Value to convert</p>
      "! @parameter output               | <p class="shorttext synchronized" lang="en">Converted value</p>
      "! @raising /gal/cx_json_exception | <p class="shorttext synchronized" lang="en">Conversion error</p>
      javascript_to_time
        IMPORTING
          !input        TYPE string
        RETURNING
          VALUE(output) TYPE t
        RAISING
          /gal/cx_json_exception,

      "! <p class="shorttext synchronized" lang="en">Conversion: JavaScript -&gt; Timestamp</p>
      "!
      "! @parameter input                | <p class="shorttext synchronized" lang="en">Value to convert</p>
      "! @parameter output               | <p class="shorttext synchronized" lang="en">Converted value</p>
      "! @raising /gal/cx_json_exception | <p class="shorttext synchronized" lang="en">Conversion error</p>
      javascript_to_timestamp
        IMPORTING
          !input        TYPE string
        RETURNING
          VALUE(output) TYPE timestampl
        RAISING
          /gal/cx_json_exception,

      "! <p class="shorttext synchronized" lang="en">Conversion: Integer -&gt; JavaScript</p>
      "!
      "! @parameter input  | <p class="shorttext synchronized" lang="en">Value to convert</p>
      "! @parameter output | <p class="shorttext synchronized" lang="en">Converted value</p>
      number_to_javascript
        IMPORTING
          !input        TYPE numeric
        RETURNING
          VALUE(output) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Conversion: String -&gt; JavaScript</p>
      "!
      "! @parameter input  | <p class="shorttext synchronized" lang="en">Value to convert</p>
      "! @parameter output | <p class="shorttext synchronized" lang="en">Converted value</p>
      string_to_javascript
        IMPORTING
          !input        TYPE csequence
        RETURNING
          VALUE(output) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Conversion: Time -&gt; JavaScript</p>
      "!
      "! @parameter input  | <p class="shorttext synchronized" lang="en">Value to convert</p>
      "! @parameter output | <p class="shorttext synchronized" lang="en">Converted value</p>
      time_to_javascript
        IMPORTING
          !input        TYPE t
        RETURNING
          VALUE(output) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Conversion: Timestamp -&gt; JavaScript</p>
      "!
      "! @parameter input  | <p class="shorttext synchronized" lang="en">Value to convert</p>
      "! @parameter output | <p class="shorttext synchronized" lang="en">Converted value</p>
      timestamp_to_javascript
        IMPORTING
          !input        TYPE numeric
        RETURNING
          VALUE(output) TYPE string.


  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS /GAL/JAVASCRIPT_CONVERTER IMPLEMENTATION.


  METHOD boolean_to_javascript.
    IF input CA 'XYJ1'.
      output = /gal/javascript_constants=>true.
    ELSE.
      output = /gal/javascript_constants=>false.
    ENDIF.
  ENDMETHOD.


  METHOD date_to_javascript.
    CONCATENATE string_delimiter input(4) '-' input+4(2) '-' input+6(2) string_delimiter
           INTO output.
  ENDMETHOD.


  METHOD javascript_to_boolean.
    IF input = /gal/javascript_constants=>true.
      output = abap_true.
    ELSE.
      output = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD javascript_to_date.
    DATA:
      l_length TYPE i,
      l_input  TYPE string.

    l_length = strlen( input ).

    IF l_length > 2 AND input(1) CA '''"'.
      l_input  = javascript_to_string( input ).
      l_length = strlen( l_input ).
    ELSE.
      l_input = input.
    ENDIF.

    IF l_length     = 10  AND
       l_input+4(1) = '-' AND
       l_input+7(1) = '-' AND
       l_input(4)   CO '0123456789' AND
       l_input+5(2) CO '0123456789' AND
       l_input+8(2) CO '0123456789'.

      output(4)   = l_input(4).
      output+4(2) = l_input+5(2).
      output+6(2) = l_input+8(2).

      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date   = output
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc = 0.
        RETURN.
      ENDIF.
    ENDIF.

    RAISE EXCEPTION TYPE /gal/cx_json_exception
      EXPORTING
        textid = /gal/cx_json_exception=>conv_format_error_date
        var1   = input.
  ENDMETHOD.


  METHOD javascript_to_number.
    output = input.
  ENDMETHOD.


  METHOD javascript_to_string.
    CONSTANTS lc_hex_chars TYPE string VALUE `0123456789abcdefABCDEF`. "#EC NOTEXT
    CONSTANTS lc_oct_chars TYPE string VALUE `01234567`.    "#EC NOTEXT

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

    IF l_offset > 0 AND input(1) CA `'"` AND input+l_offset(1) = input(1).
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


  METHOD javascript_to_time.
    DATA:
      l_length     TYPE i,
      l_input      TYPE string,

      l_hours(2)   TYPE n,
      l_minutes(2) TYPE n,
      l_seconds(2) TYPE n.

    l_length = strlen( input ).

    IF l_length > 2 AND input(1) CA '''"'.
      l_input  = javascript_to_string( input ).
      l_length = strlen( l_input ).
    ELSE.
      l_input = input.
    ENDIF.

    IF l_length     = 8   AND
       l_input+2(1) = ':' AND
       l_input+5(1) = ':' AND
       l_input(2)   CO '0123456789' AND
       l_input+3(2) CO '0123456789' AND
       l_input+6(2) CO '0123456789'.

      l_hours   = l_input(2).
      l_minutes = l_input+3(2).
      l_seconds = l_input+6(2).

      IF l_hours < '24' AND l_minutes < '60' AND l_seconds < '60'.
        output(2)   = l_hours.
        output+2(2) = l_minutes.
        output+4(2) = l_seconds.
        RETURN.
      ENDIF.
    ENDIF.

    RAISE EXCEPTION TYPE /gal/cx_json_exception
      EXPORTING
        textid = /gal/cx_json_exception=>conv_format_error_time
        var1   = input.
  ENDMETHOD.


  METHOD javascript_to_timestamp.
    DATA:
      l_length         TYPE i,
      l_offset         TYPE i,
      l_input          TYPE string,

      l_date_str       TYPE string,
      l_date           TYPE d,

      l_time_str       TYPE string,
      l_time           TYPE t,

      l_fract_str      TYPE string,
      l_fract          TYPE timestampl,

      l_tzone_str      TYPE string,

      l_offset_hours   TYPE i,
      l_offset_minutes TYPE i,
      l_offset_seconds TYPE i,

      l_timestamp      TYPE timestampl,
      l_timestamp_obj  TYPE REF TO /gal/timestamp_long,

      l_js_time        TYPE string,
      l_dummy          TYPE string.                         "#EC NEEDED

* Get string length and content
    l_length = strlen( input ).

    IF l_length > 2 AND input(1) CA '''"'.
      l_input  = javascript_to_string( input ).
      l_length = strlen( l_input ).
    ELSE.
      l_input = input.
    ENDIF.

    IF l_length     >= 19  AND                           "#EC NUMBER_OK
       l_input+4(1)  = '-' AND
       l_input+7(1)  = '-' AND
       l_input+10(1) = 'T' AND
       l_input+13(1) = ':' AND
       l_input+16(1) = ':' AND
       l_input(4)    CO '0123456789' AND
       l_input+5(2)  CO '0123456789' AND
       l_input+8(2)  CO '0123456789' AND
       l_input+11(2) CO '0123456789' AND
       l_input+14(2) CO '0123456789' AND
       l_input+17(2) CO '0123456789'.

      l_date_str = l_input(10).
      l_date     = javascript_to_date( l_date_str ).

      l_time_str = l_input+11(8).
      l_time     = javascript_to_time( l_time_str ).

      IF l_length > 20 AND l_input+19(1) CA '.,' AND l_input+20(1) CO '0123456789'. "#EC NUMBER_OK
        IF l_input+20 CN '0123456789'.
          l_offset    = 20 + sy-fdpos.                   "#EC NUMBER_OK
          l_fract_str = l_input+20(sy-fdpos).
          l_tzone_str = l_input+l_offset.
        ELSE.
          l_fract_str = l_input+20.
        ENDIF.

        CONCATENATE '0.' l_fract_str INTO l_fract_str.

        l_fract = l_fract_str.
      ENDIF.
    ELSEIF l_input CP 'DATE(+*)'
        OR l_input CP '/DATE(+*)/'
        OR l_input CP '\/DATE(+*)\/'
        OR l_input CP 'NEW DATE(+*)'.

      SPLIT l_input   AT '(' INTO l_dummy   l_js_time.
      SPLIT l_js_time AT ')' INTO l_js_time l_dummy.

      IF l_js_time CN '0123456789' AND sy-fdpos > 0.
        l_tzone_str = l_js_time+sy-fdpos.
        l_js_time   = l_js_time(sy-fdpos).
      ENDIF.

      IF l_js_time CO '0123456789'.
        l_date           = '19700101'.
        l_time           = '000000'.
        l_fract          = frac( l_js_time / 1000 ).
        l_offset_seconds = trunc( l_js_time / 1000 ).
      ELSE.
        RAISE EXCEPTION TYPE /gal/cx_json_exception
          EXPORTING
            textid = /gal/cx_json_exception=>conv_format_error_timestamp
            var1   = input.
      ENDIF.
    ENDIF.

* Convert date, time and time zone to timestamp
    IF l_date IS NOT INITIAL.
      CONVERT DATE l_date TIME l_time INTO TIME STAMP l_timestamp TIME ZONE /gal/timestamp_base=>time_zone_utc.
      l_timestamp = l_timestamp + l_fract.

* Parse time zone offset
      IF l_tzone_str IS NOT INITIAL AND l_tzone_str <> `Z`.
        IF l_tzone_str(1) CA '+-' AND l_tzone_str CP '+++' AND l_tzone_str+1(2) CO '0123456789'.
          l_offset_hours = l_tzone_str+1(2).
        ELSEIF l_tzone_str(1) CA '+-' AND l_tzone_str CP '+++:++' AND l_tzone_str+1(2) CO '0123456789' AND l_tzone_str+4(2) CO '0123456789'.
          l_offset_hours   = l_tzone_str+1(2).
          l_offset_minutes = l_tzone_str+4(2).
        ELSE.
          RAISE EXCEPTION TYPE /gal/cx_json_exception
            EXPORTING
              textid = /gal/cx_json_exception=>conv_format_error_timestamp
              var1   = input.
        ENDIF.

        IF l_tzone_str(1) = '+'.
          l_offset_hours   = - l_offset_hours.
          l_offset_minutes = - l_offset_minutes.
        ENDIF.
      ENDIF.

* Apply offset to timestamp
      IF l_offset_hours <> 0 OR l_offset_minutes <> 0 OR l_offset_seconds <> 0.
        CREATE OBJECT l_timestamp_obj
          EXPORTING
            value = l_timestamp.

        l_timestamp_obj->add_interval( hours   = l_offset_hours
                                       minutes = l_offset_minutes
                                       seconds = l_offset_seconds ).

        output = l_timestamp_obj->value.
      ELSE.
        output = l_timestamp.
      ENDIF.

      RETURN.
    ENDIF.

* Format is not supported
    RAISE EXCEPTION TYPE /gal/cx_json_exception
      EXPORTING
        textid = /gal/cx_json_exception=>conv_format_error_timestamp
        var1   = input.
  ENDMETHOD.


  METHOD number_to_javascript.
    DATA:
      l_sign       TYPE string,
      l_abs_value  TYPE /gal/javascript_number,
      l_digits(31) TYPE c.

* Get sign and absolute value
    IF input < 0.
      l_sign      = `-`.
      l_abs_value = - input.
    ELSE.
      l_abs_value = input.
    ENDIF.

* Convert to decimal number
    UNPACK l_abs_value TO l_digits.
    CONCATENATE l_digits(17) l_digits+17 INTO output SEPARATED BY '.'.

* Remove leading and trailing zeros
    SHIFT output LEFT DELETING LEADING '0'.

    SHIFT output RIGHT DELETING TRAILING '0'.
    SHIFT output LEFT DELETING LEADING space.

* Format number for JSON
    SHIFT output RIGHT DELETING TRAILING '.'.
    SHIFT output LEFT DELETING LEADING space.

    IF output IS INITIAL OR output(1) = '.'.
      CONCATENATE l_sign '0' output INTO output.
    ELSE.
      CONCATENATE l_sign output INTO output.
    ENDIF.
  ENDMETHOD.


  METHOD string_to_javascript.
    DATA:
      l_escaped_delimiter(2) TYPE c.

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


  METHOD timestamp_to_javascript.
    DATA:
      l_timestamp  TYPE timestampl,
      l_digits(21) TYPE c.

    l_timestamp = input.

    UNPACK l_timestamp TO l_digits.

    CONCATENATE string_delimiter
                l_digits+0(4) '-' l_digits+4(2) '-' l_digits+6(2) 'T'
                l_digits+8(2) ':' l_digits+10(2) ':' l_digits+12(2) '.'
                l_digits+14(3) 'Z'
                string_delimiter
           INTO output.
  ENDMETHOD.


  METHOD time_to_javascript.
    CONCATENATE string_delimiter input(2) ':' input+2(2) ':' input+4(2) string_delimiter INTO output.
  ENDMETHOD.
ENDCLASS.
