class /GAL/STRING definition
  public
  create public .

public section.
  type-pools ABAP .

  constants ADJUST_CASE_MODE_DEFAULT type STRING value `Default`. "#EC NOTEXT
  constants ADJUST_CASE_MODE_LOWER type STRING value `Lower`. "#EC NOTEXT
  constants ADJUST_CASE_MODE_UPPER type STRING value `Upper`. "#EC NOTEXT
  constants FORMAT_DEFAULT type STRING value `Default`. "#EC NOTEXT
  constants FORMAT_DOMAIN_VALUE_TEXT type STRING value `DomainValueText`. "#EC NOTEXT
  constants FORMAT_SHORT type STRING value `Short`. "#EC NOTEXT
  class-data LINE_BREAK_UNIX type STRING read-only .
  class-data LINE_BREAK_WINDOWS type STRING read-only .
  class-data MAX_CHAR type C read-only .
  class-data MIN_CHAR type C read-only .
  class-data NON_BREAKING_SPACE type STRING read-only .
  class-data TAB type STRING read-only .
  class-data WHITESPACE_CHARS type STRING read-only .

  class-methods ADJUST_CASE
    importing
      !INPUT type STRING
      !MODE type STRING default ADJUST_CASE_MODE_DEFAULT
    returning
      value(OUTPUT) type STRING .
  class-methods ANY_TO_STRING
    importing
      !INPUT type ANY
      !FORMAT type STRING optional
      !LANGUAGE type SY-LANGU default SY-LANGU
    returning
      value(OUTPUT) type STRING .
  class-methods CLASS_CONSTRUCTOR .
  class-methods DATE_TO_STRING
    importing
      !INPUT type D default SY-DATLO
      !FORMAT type STRING default FORMAT_DEFAULT
      !LANGUAGE type SY-LANGU default SY-LANGU
    returning
      value(OUTPUT) type STRING .
  class-methods DOMVALUE_TO_STRING
    importing
      !INPUT type ANY
      !LANGUAGE type SY-LANGU default SY-LANGU
    returning
      value(OUTPUT) type STRING .
  class-methods ENDS_WITH
    importing
      !INPUT type CSEQUENCE
      !PART type CSEQUENCE
    returning
      value(RESULT) type ABAP_BOOL .
  class-methods GET_LENGTH
    importing
      !INPUT type CSEQUENCE
      !WHITESPACE_CHARS type CSEQUENCE default WHITESPACE_CHARS
    returning
      value(LENGTH) type I .
  class-methods GET_SPECIAL_CHAR
    importing
      !ID type CSEQUENCE
    returning
      value(RESULT) type STRING .
  class-methods LIMIT_LENGTH
    importing
      !INPUT type STRING
      !LENGTH type I
      !WHITESPACE_CHARS type STRING default WHITESPACE_CHARS
      !SUFFIX type STRING default `...`
    returning
      value(OUTPUT) type STRING .
  class-methods LONG_TIMESTAMP_TO_STRING
    importing
      !TIMESTAMP type TIMESTAMPL
      !TIMEZONE type SY-ZONLO default SY-ZONLO
      !FORMAT type STRING default FORMAT_DEFAULT
      !LANGUAGE type SY-LANGU default SY-LANGU
    returning
      value(OUTPUT) type STRING .
  class-methods OBJECT_TO_STRING
    importing
      !INPUT type ref to OBJECT
    returning
      value(OUTPUT) type STRING .
  class-methods REPLACE_VARIABLES
    importing
      !INPUT type CSEQUENCE
      !VAR01 type ANY optional
      !VAR02 type ANY optional
      !VAR03 type ANY optional
      !VAR04 type ANY optional
      !VAR05 type ANY optional
      !VAR06 type ANY optional
      !VAR07 type ANY optional
      !VAR08 type ANY optional
      !VAR09 type ANY optional
      !VAR10 type ANY optional
      !LANGUAGE type SY-LANGU default SY-LANGU
    returning
      value(OUTPUT) type STRING .
  class-methods STARTS_WITH
    importing
      !INPUT type CSEQUENCE
      !PART type CSEQUENCE
    returning
      value(RESULT) type ABAP_BOOL .
  class-methods STRINGTABLE_TO_TEXTTABLE
    importing
      !INPUT type /GAL/STRINGTABLE
    exporting
      !OUTPUT type ANY TABLE .
  class-methods STRING_TO_DOMVALUE
    importing
      !INPUT type CSEQUENCE
      !LANGUAGE type SY-LANGU default SY-LANGU
    exporting
      !OUTPUT type ANY .
  class-methods STRING_TO_MESSAGE
    importing
      !INPUT type CSEQUENCE
      !MSGTY type SYMSGTY default 'E'
      !MSGID type SYMSGID
      !MSGNO type SYMSGNO .
  class-methods STRING_TO_MESSAGE_VARS
    importing
      !INPUT type CSEQUENCE
    exporting
      !MSGV1 type SYMSGV
      !MSGV2 type SYMSGV
      !MSGV3 type SYMSGV
      !MSGV4 type SYMSGV .
  class-methods STRING_TO_STRINGTABLE
    importing
      !INPUT type CSEQUENCE
      !WORD_WRAP_POSITION type I default 0
      !WORD_WRAP_CHARACTERS type STRING default ` -.,?!;:`
      !AUTO_TRIM type ABAP_BOOL default ABAP_TRUE
    returning
      value(OUTPUT) type /GAL/STRINGTABLE .
  class-methods STRING_TO_XSTRING
    importing
      !INPUT type STRING
      !ENCODING type ABAP_ENCODING default 'DEFAULT'
      !ENDIAN type ABAP_ENDIAN optional
      !REPLACEMENT type ABAP_REPL default '#'
    returning
      value(OUTPUT) type XSTRING .
  class-methods TEXTTABLE_TO_STRINGTABLE
    importing
      !INPUT type ANY TABLE
    returning
      value(OUTPUT) type /GAL/STRINGTABLE .
  class-methods TIMESTAMP_TO_STRING
    importing
      !TIMESTAMP type TIMESTAMP optional
      !TIMEZONE type SY-ZONLO default SY-ZONLO
      !FORMAT type STRING default FORMAT_DEFAULT
      !LANGUAGE type SY-LANGU default SY-LANGU
    returning
      value(OUTPUT) type STRING .
  class-methods TIME_TO_STRING
    importing
      !INPUT type T default SY-TIMLO
      !FORMAT type STRING default FORMAT_DEFAULT
      !LANGUAGE type SY-LANGU default SY-LANGU
    returning
      value(OUTPUT) type STRING .
  class-methods TRIM
    importing
      !INPUT type CSEQUENCE
      !WHITESPACE_CHARS type CSEQUENCE default WHITESPACE_CHARS
    returning
      value(OUTPUT) type STRING .
  class-methods TRIM_LEFT
    importing
      !INPUT type CSEQUENCE
      !WHITESPACE_CHARS type CSEQUENCE default WHITESPACE_CHARS
    returning
      value(OUTPUT) type STRING .
  class-methods TRIM_RIGHT
    importing
      !INPUT type CSEQUENCE
      !WHITESPACE_CHARS type CSEQUENCE default WHITESPACE_CHARS
    returning
      value(OUTPUT) type STRING .
  class-methods XSTRING_TO_STRING
    importing
      !INPUT type XSTRING
      !ENCODING type ABAP_ENCODING default 'DEFAULT'
      !ENDIAN type ABAP_ENDIAN optional
      !REPLACEMENT type ABAP_REPL default '#'
    returning
      value(OUTPUT) type STRING .
protected section.
private section.
ENDCLASS.



CLASS /GAL/STRING IMPLEMENTATION.


METHOD adjust_case.
  DATA l_initial TYPE c.

  CASE mode.

* Convert first letter to upper case and everything else to lower case
    WHEN adjust_case_mode_default.
      IF input IS NOT INITIAL.
        l_initial = input(1).
        output    = input+1.

        TRANSLATE l_initial TO UPPER CASE.
        TRANSLATE output    TO LOWER CASE.

        CONCATENATE l_initial output INTO output.
      ENDIF.

* Convert to upper case
    WHEN adjust_case_mode_upper.
      output = input.
      TRANSLATE output TO UPPER CASE.

* Convert to lower case
    WHEN adjust_case_mode_lower.
      output = input.
      TRANSLATE output TO LOWER CASE.

    WHEN OTHERS.
      output = input.

  ENDCASE.
ENDMETHOD.                    "adjust_case


METHOD any_to_string.
  DATA l_type_descr        TYPE REF TO cl_abap_typedescr.
  DATA l_elem_descr        TYPE REF TO cl_abap_elemdescr.

  DATA l_ddic_field_info   TYPE dfies.
  DATA l_ddic_fixed_values TYPE ddfixvalues.

  DATA l_format            TYPE string.

  DATA l_edit_mask         TYPE string.

  DATA l_output_char(255)  TYPE c.

  DATA l_temp              TYPE string.

  FIELD-SYMBOLS <l_ddic_fixed_values> LIKE LINE OF l_ddic_fixed_values.
  FIELD-SYMBOLS <l_dref>              TYPE REF TO data.
  FIELD-SYMBOLS <l_field>             TYPE any.
  FIELD-SYMBOLS <l_table>             TYPE ANY TABLE.

* Get type description
  l_type_descr = cl_abap_typedescr=>describe_by_data( input ).

  CATCH SYSTEM-EXCEPTIONS OTHERS = 0.
    l_elem_descr ?= l_type_descr.

    l_elem_descr->get_ddic_field( EXPORTING  p_langu    = language
                                  RECEIVING  p_flddescr = l_ddic_field_info
                                  EXCEPTIONS OTHERS     = 0 ).

    IF l_ddic_field_info-convexit IS NOT INITIAL.
      CONCATENATE `==` l_ddic_field_info-convexit INTO l_edit_mask.
    ENDIF.
  ENDCATCH.                                               "#EC CI_SUBRC

* Determine format
  IF format IS INITIAL.
    l_format = format_short.
  ELSE.
    l_format = format.
  ENDIF.

* Extended formatting for data, time and timestamps
  IF format NP `ConvExit=+*` AND
     format NP `EditMask=+*` AND
     format <> `NoConvExit`.

* Format date
    IF l_type_descr->type_kind = cl_abap_typedescr=>typekind_date.
      output = date_to_string( input    = input
                               format   = l_format
                               language = language ).
      RETURN.

* Format time
    ELSEIF l_type_descr->type_kind = cl_abap_typedescr=>typekind_time.
      output = time_to_string( input    = input
                               format   = l_format
                               language = language ).
      RETURN.

* Format short timestamps
    ELSEIF l_ddic_field_info-rollname = 'TIMESTAMP' OR l_ddic_field_info-domname = 'TZNTSTMPS'.
      output = timestamp_to_string( timestamp = input
                                    format    = l_format
                                    language  = language ).
      RETURN.

* Format long timestamps
    ELSEIF l_ddic_field_info-rollname = 'TIMESTAMPL' OR l_ddic_field_info-domname = 'TZNTSTMPL'.
      output = long_timestamp_to_string( timestamp = input
                                         format    = l_format
                                         language  = language ).
      RETURN.
    ENDIF.
  ENDIF.

* Use domain values if requested
  IF l_ddic_field_info-domname IS NOT INITIAL AND format = `DomainValueText`.
    l_elem_descr->get_ddic_fixed_values( EXPORTING  p_langu        = language
                                         RECEIVING  p_fixed_values = l_ddic_fixed_values
                                         EXCEPTIONS OTHERS         = 1 ).
    IF sy-subrc = 0.
      READ TABLE l_ddic_fixed_values
            WITH KEY low    = input
                     high   = ''
                     option = 'EQ' ASSIGNING <l_ddic_fixed_values>. "#EC CI_STDSEQ
      IF sy-subrc = 0.
        output = <l_ddic_fixed_values>-ddtext.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

* Type kind dependent conversion
  CASE l_type_descr->type_kind.

    WHEN cl_abap_typedescr=>typekind_char
      OR cl_abap_typedescr=>typekind_date
      OR cl_abap_typedescr=>typekind_num
      OR cl_abap_typedescr=>typekind_hex
      OR cl_abap_typedescr=>typekind_string
      OR cl_abap_typedescr=>typekind_xstring
      OR cl_abap_typedescr=>typekind_time.

      IF format CP `ConvExit=+*`.
        CONCATENATE `==` format+9 INTO l_edit_mask.
      ELSEIF format CP `EditMask=+*`.
        l_edit_mask = format+9.
      ENDIF.

      IF l_edit_mask IS INITIAL OR format = `NoConvExit`.
        output = input.
      ELSE.
        WRITE input TO l_output_char USING EDIT MASK l_edit_mask.
        output = l_output_char.
      ENDIF.

      RETURN.

    WHEN cl_abap_typedescr=>typekind_packed
      OR cl_abap_typedescr=>typekind_float
      OR cl_abap_typedescr=>typekind_int
      OR cl_abap_typedescr=>typekind_int1
      OR cl_abap_typedescr=>typekind_int2.
      output = input.
      output = trim( output ).
      RETURN.

    WHEN cl_abap_typedescr=>typekind_oref
      OR cl_abap_typedescr=>typekind_iref.
      output = object_to_string( input ).
      RETURN.

    WHEN cl_abap_typedescr=>typekind_struct1
      OR cl_abap_typedescr=>typekind_struct2.
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE input TO <l_field>.
        IF sy-subrc = 0.
          l_temp = any_to_string( <l_field> ).

          IF output IS INITIAL.
            output = l_temp.
          ELSE.
            CONCATENATE output l_temp INTO output SEPARATED BY '|'.
          ENDIF.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
      RETURN.

    WHEN cl_abap_typedescr=>typekind_table.
      ASSIGN input TO <l_table>.                          "#EC CI_SUBRC

      DESCRIBE TABLE <l_table> LINES sy-tfill.
      l_temp = sy-tfill.

      CONCATENATE `[Table (` l_temp `lines)]` INTO output.  "#EC NOTEXT
      RETURN.

    WHEN cl_abap_typedescr=>typekind_dref.
      ASSIGN input TO <l_dref>.                           "#EC CI_SUBRC
      ASSIGN <l_dref>->* TO <l_field>.
      IF sy-subrc = 0.
        output = any_to_string( <l_field> ).
      ELSE.
        output = `[Initial]`.                               "#EC NOTEXT
      ENDIF.

      CONCATENATE `->` output INTO output SEPARATED BY space. "#EC NOTEXT
      RETURN.

  ENDCASE.

  output = input.
ENDMETHOD.                    "any_to_string


METHOD class_constructor.
  CONSTANTS lc_non_breaking_space_uc(2) TYPE x VALUE '00A0'.
  CONSTANTS lc_non_breaking_space       TYPE x VALUE 'A0'.

  DATA l_converter TYPE REF TO cl_abap_conv_in_ce.

* Get line break representations
  min_char = cl_abap_char_utilities=>minchar.
  max_char = cl_abap_char_utilities=>maxchar.

  line_break_unix    = cl_abap_char_utilities=>newline.
  line_break_windows = cl_abap_char_utilities=>cr_lf.
  tab                = cl_abap_char_utilities=>horizontal_tab.

* Get non-breaking space
  IF cl_abap_char_utilities=>charsize > 1.
    non_breaking_space = cl_abap_conv_in_ce=>uccp( lc_non_breaking_space_uc ).
  ELSE.
    l_converter = cl_abap_conv_in_ce=>create( input = lc_non_breaking_space ).

    l_converter->read( IMPORTING data = non_breaking_space ).
  ENDIF.

* Get whitespace characters
  whitespace_chars = cl_abap_char_utilities=>get_simple_spaces_for_cur_cp( ).
ENDMETHOD.                    "class_constructor


METHOD date_to_string.
  DATA l_language_backup TYPE langu.

  DATA l_date            TYPE d.
  DATA l_weekday         TYPE c.

  DATA l_weekday_short_c TYPE t246-kurzt.
  DATA l_weekday_long_c  TYPE t246-langt.
  DATA l_weekday_short1  TYPE string.
  DATA l_weekday_short2  TYPE string.
  DATA l_weekday_long1   TYPE string.
  DATA l_weekday_long2   TYPE string.
  DATA l_weekday_long3   TYPE string.
  DATA l_weekday_long4   TYPE string.

  DATA l_day_n_short     TYPE string.
  DATA l_day_n2_short    TYPE string.
  DATA l_month_n_short   TYPE string.

  DATA l_month_short_c   TYPE t247-ktx.
  DATA l_month_long_c    TYPE t247-ltx.
  DATA l_month_short     TYPE string.
  DATA l_month_long      TYPE string.

  DATA l_prefix          TYPE string.


  IF input IS INITIAL.
    CLEAR output.
    RETURN.
  ENDIF.

* Backup current language and set desired language
  l_language_backup = sy-langu.

  IF l_language_backup <> language.
    SET LANGUAGE language.
  ENDIF.

* Determine weekday
  CALL FUNCTION 'DATE_COMPUTE_DAY'
    EXPORTING
      date = input
    IMPORTING
      day  = l_weekday.

  SELECT SINGLE kurzt langt FROM t246
                            INTO (l_weekday_short_c,l_weekday_long_c)
                           WHERE sprsl = language
                             AND wotnr = l_weekday.       "#EC CI_SUBRC

  l_weekday_short1 = l_weekday_short_c.
  l_weekday_long1  = l_weekday_long_c.

  l_weekday_short1 = adjust_case( l_weekday_short1 ).
  l_weekday_short2 = l_weekday_short1.

  l_weekday_long1  = adjust_case( l_weekday_long1 ).
  l_weekday_long2  = l_weekday_long1.
  l_weekday_long3  = l_weekday_long1.
  l_weekday_long4  = l_weekday_long1.

  CASE language.

    WHEN 'D'.
      REPLACE `Sonnabend` IN l_weekday_long1 WITH `Samstag`. "#EC NOTEXT
      REPLACE `Sonnabend` IN l_weekday_long3 WITH `Samstag`. "#EC NOTEXT

    WHEN 'E'.
      CASE l_weekday.
        WHEN 1. l_weekday_short2 = `Mon`.
        WHEN 2. l_weekday_short2 = `Tue`.
        WHEN 3. l_weekday_short2 = `Wed`.
        WHEN 4. l_weekday_short2 = `Thu`.
        WHEN 5. l_weekday_short2 = `Fri`.
        WHEN 6. l_weekday_short2 = `Sat`.
        WHEN 7. l_weekday_short2 = `Sun`.
      ENDCASE.

  ENDCASE.

* Determine day
  l_day_n_short = input+6(2).
  SHIFT l_day_n_short LEFT DELETING LEADING '0'.

  CASE language.

    WHEN 'D'.
      CONCATENATE l_day_n_short '.' INTO l_day_n2_short.

    WHEN 'E'.
      CASE input+6(2).
        WHEN '01'.
          CONCATENATE l_day_n_short `st` INTO l_day_n2_short.

        WHEN '02'.
          CONCATENATE l_day_n_short `nd` INTO l_day_n2_short.

        WHEN '03'.
          CONCATENATE l_day_n_short `rd` INTO l_day_n2_short.

        WHEN OTHERS.
          CONCATENATE l_day_n_short `th` INTO l_day_n2_short.

      ENDCASE.

  ENDCASE.

* Get prefix
  IF input = sy-datum.
    l_prefix = TEXT-000.
  ELSE.
    l_date = input + 1.

    IF l_date = sy-datum.
      l_prefix = TEXT-001.
    ENDIF.
  ENDIF.

  IF NOT l_prefix IS INITIAL.
    l_weekday_long3 = l_prefix.
    l_weekday_long4 = l_prefix.
    CONCATENATE l_prefix `, ` INTO l_prefix RESPECTING BLANKS.
  ENDIF.

* Determine month
  l_month_n_short = input+4(2).
  SHIFT l_month_n_short LEFT DELETING LEADING '0'.

  SELECT SINGLE ktx ltx FROM t247
                        INTO (l_month_short_c,l_month_long_c)
                       WHERE spras = language
                         AND mnr   = input+4(2).          "#EC CI_SUBRC
  l_month_short = l_month_short_c.
  l_month_long  = l_month_long_c.

  l_month_short = adjust_case( l_month_short ).
  l_month_long  = adjust_case( l_month_long ).

* Determine format
  CASE format.

    WHEN format_default.
      output = TEXT-t00.

    WHEN format_short.
      output = TEXT-t03.

    WHEN OTHERS.
      output = format.

  ENDCASE.

* Build date
  REPLACE ALL OCCURRENCES OF `[PREFIX]` IN output WITH l_prefix.
  REPLACE ALL OCCURRENCES OF `[WWWW+]`  IN output WITH l_weekday_long3.
  REPLACE ALL OCCURRENCES OF `[WWWW+-]` IN output WITH l_weekday_long4.
  REPLACE ALL OCCURRENCES OF `[WWWW]`   IN output WITH l_weekday_long1.
  REPLACE ALL OCCURRENCES OF `[WWWW-]`  IN output WITH l_weekday_long2.
  REPLACE ALL OCCURRENCES OF `[WWW]`    IN output WITH l_weekday_short2.
  REPLACE ALL OCCURRENCES OF `[WW]`     IN output WITH l_weekday_short1.
  REPLACE ALL OCCURRENCES OF `[DDD]`    IN output WITH l_day_n2_short.
  REPLACE ALL OCCURRENCES OF `[DD]`     IN output WITH input+6(2).
  REPLACE ALL OCCURRENCES OF `[D]`      IN output WITH l_day_n_short.
  REPLACE ALL OCCURRENCES OF `[MMMM]`   IN output WITH l_month_long.
  REPLACE ALL OCCURRENCES OF `[MMM]`    IN output WITH l_month_short.
  REPLACE ALL OCCURRENCES OF `[MM]`     IN output WITH input+4(2).
  REPLACE ALL OCCURRENCES OF `[M]`      IN output WITH l_month_n_short.
  REPLACE ALL OCCURRENCES OF `[YYYY]`   IN output WITH input(4).
  REPLACE ALL OCCURRENCES OF `[YY]`     IN output WITH input+2(2).

* Restore language
  IF l_language_backup <> language.
    SET LANGUAGE l_language_backup.
  ENDIF.
ENDMETHOD.                    "date_to_string


METHOD domvalue_to_string.
  output = any_to_string( input  = input
                          format = format_domain_value_text ).
ENDMETHOD.                    "domvalue_to_string


METHOD ends_with.
  DATA l_input        TYPE string.
  DATA l_part         TYPE string.

  DATA l_input_length TYPE i.
  DATA l_part_length  TYPE i.

  DATA l_offset       TYPE i.

* Convert parameters
  l_input = input.
  l_part  = part.

* Determine length
  l_input_length = strlen( input ).
  l_part_length  = strlen( part ).

* Always true for empty part
  IF l_part_length = 0.
    result = abap_true.
    RETURN.
  ENDIF.

* Check if part fits into string
  IF l_input_length < l_part_length.
    result = abap_false.
    RETURN.
  ENDIF.

* Actual comparison
  l_offset = l_input_length - l_part_length.

  IF l_input+l_offset = l_part.
    result = abap_true.
  ELSE.
    result = abap_false.
  ENDIF.
ENDMETHOD.


METHOD get_length.
  DATA l_position TYPE i.

  CHECK NOT input CO whitespace_chars.

  length = strlen( input ).

  IF length > 0.
    l_position = length - 1.

    WHILE input+l_position(1) CA whitespace_chars.
      l_position = l_position - 1.
    ENDWHILE.

    length = l_position + 1.
  ENDIF.
ENDMETHOD.                    "get_length


METHOD get_special_char.
  CONSTANTS lc_hex_copyright(2) TYPE x VALUE '00A9'.
  CONSTANTS lc_hex_trademark(2) TYPE x VALUE '2122'.

  CASE id.

    WHEN '(C)'.
      IF cl_abap_char_utilities=>charsize > 1.
        result = cl_abap_conv_in_ce=>uccp( lc_hex_copyright ).
      ELSE.
        result = '©'.
      ENDIF.

    WHEN '(TM)'.
      IF cl_abap_char_utilities=>charsize > 1.
        result = cl_abap_conv_in_ce=>uccp( lc_hex_trademark ).
      ENDIF.

    WHEN '(MIN)'.
      result = cl_abap_char_utilities=>minchar.

    WHEN '(MAX)'.
      result = cl_abap_char_utilities=>maxchar.

  ENDCASE.
ENDMETHOD.


  METHOD LIMIT_LENGTH.
    DATA l_length        TYPE i.
    DATA l_suffix_length TYPE i.

    CALL METHOD get_length
      EXPORTING
        input            = input
        whitespace_chars = whitespace_chars
      RECEIVING
        length           = l_length.

    IF l_length <= length.
      output = input.
    ELSE.
      CALL METHOD get_length
        EXPORTING
          input            = suffix
          whitespace_chars = whitespace_chars
        RECEIVING
          length           = l_suffix_length.

      IF length > l_suffix_length.
        l_length = length - l_suffix_length.

        output = input(l_length).

        CALL METHOD trim_right
          EXPORTING
            input            = output
            whitespace_chars = whitespace_chars
          RECEIVING
            output           = output.

        CONCATENATE output suffix(l_suffix_length) INTO output.
      ELSE.
        output = input(length).
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "limit_length


  METHOD LONG_TIMESTAMP_TO_STRING.
    DATA l_timestamp TYPE timestampl.
    DATA l_date      TYPE d.
    DATA l_time      TYPE t.

    DATA l_date_str  TYPE string.
    DATA l_time_str  TYPE string.
    DATA l_tz_str    TYPE string.

* Ggf. Timestamp vorbelegen *
    IF timestamp IS INITIAL.
      GET TIME STAMP FIELD l_timestamp.
    ELSE.
      l_timestamp = timestamp.
    ENDIF.

* Datum und Uhrzeit ermitteln *
    CONVERT TIME STAMP l_timestamp TIME ZONE timezone
       INTO DATE l_date TIME l_time.

* Zeitstempel aufbauen *
    IF format = 'Default'.
      output = text-t02.
    ELSE.
      output = format.
    ENDIF.

    IF output CS '[tz]'.
      l_tz_str = timezone.
      REPLACE '[tz]' WITH l_tz_str INTO output.
    ENDIF.

    IF output CS '[Date]'.
      CALL METHOD date_to_string
        EXPORTING
          input    = l_date
          language = language
        RECEIVING
          output   = l_date_str.
      REPLACE '[Date]' WITH l_date_str INTO output.
    ENDIF.

    IF output CS '[Time]'.
      CALL METHOD time_to_string
        EXPORTING
          input    = l_time
          language = language
        RECEIVING
          output   = l_time_str.
      REPLACE '[Time]' WITH l_time_str INTO output.
    ENDIF.

    IF output CA '['.
      CALL METHOD date_to_string
        EXPORTING
          input    = l_date
          format   = output
          language = language
        RECEIVING
          output   = output.

      CALL METHOD time_to_string
        EXPORTING
          input    = l_time
          format   = output
          language = language
        RECEIVING
          output   = output.
    ENDIF.
  ENDMETHOD.                    "long_timestamp_to_string


METHOD object_to_string.
  DATA l_class_descr     TYPE REF TO cl_abap_classdescr.
  DATA l_interface_descr TYPE REF TO cl_abap_intfdescr.

  FIELD-SYMBOLS <l_description> TYPE any.

* Make sure that an object has been passed
  IF input IS INITIAL.
    RETURN.
  ENDIF.

* Try to call method TO_STRING
  CATCH SYSTEM-EXCEPTIONS OTHERS = 1.
    CALL METHOD input->('TO_STRING')
      RECEIVING
        text = output.

    RETURN.
  ENDCATCH.                                               "#EC CI_SUBRC

* Try to call method GET_TEXT
  CATCH SYSTEM-EXCEPTIONS OTHERS = 1.
    CALL METHOD input->('GET_TEXT')
      RECEIVING
        text = output.

    RETURN.
  ENDCATCH.                                               "#EC CI_SUBRC

* Try to access attribute TEXT
  CATCH SYSTEM-EXCEPTIONS OTHERS = 1.
    ASSIGN input->('TEXT') TO <l_description>.

    IF sy-subrc = 0.
      output = <l_description>.
      RETURN.
    ENDIF.
  ENDCATCH.                                               "#EC CI_SUBRC

* Try to access attribute DESCRIPTION
  CATCH SYSTEM-EXCEPTIONS OTHERS = 1.
    ASSIGN input->('DESCRIPTION') TO <l_description>.

    IF sy-subrc = 0.
      output = <l_description>.
      RETURN.
    ENDIF.
  ENDCATCH.                                               "#EC CI_SUBRC

* Return class or interface name
  CATCH SYSTEM-EXCEPTIONS OTHERS = 1.
    l_class_descr ?= cl_abap_typedescr=>describe_by_object_ref( input ).
  ENDCATCH.

  IF sy-subrc = 0.
    CONCATENATE `{` l_class_descr->absolute_name `}` INTO output.
    RETURN.
  ENDIF.

  CATCH SYSTEM-EXCEPTIONS OTHERS = 1.
    l_interface_descr ?= cl_abap_typedescr=>describe_by_object_ref( input ).
  ENDCATCH.

  IF sy-subrc = 0.
    CONCATENATE `{` l_interface_descr->absolute_name `}` INTO output.
    RETURN.
  ENDIF.

  output = `[Object]`.                                      "#EC NOTEXT
ENDMETHOD.                    "object_to_string


  METHOD replace_variables.
    DATA l_offset       TYPE i.

    DATA l_var_id(2)    TYPE n.
    DATA l_var_name     TYPE string.

    DATA l_format       TYPE string.
    DATA l_value        TYPE string.

    DATA l_input_length TYPE i.

    FIELD-SYMBOLS <l_variable> TYPE any.

* Determine length of input string
    l_input_length = strlen( input ).

* Process all variable tags
    WHILE input+l_offset CA '{'.
      IF sy-fdpos > 0.
        CONCATENATE output input+l_offset(sy-fdpos) INTO output RESPECTING BLANKS.
      ENDIF.

      l_offset = l_offset + sy-fdpos + 1.

* Get variable
      IF input+l_offset CA ':}'.
        CLEAR l_var_id.

        UNASSIGN <l_variable>.

        CATCH SYSTEM-EXCEPTIONS OTHERS = 1.
          IF input+l_offset(1) = '\'. "Support escaping
            l_offset = l_offset + 1.
          ELSE.
            l_var_id = input+l_offset(sy-fdpos).

            CONCATENATE `VAR` l_var_id INTO l_var_name.

            ASSIGN (l_var_name) TO <l_variable>.

            IF sy-subrc = 0.
              l_offset = l_offset + sy-fdpos.
            ENDIF.
          ENDIF.
        ENDCATCH.

        IF sy-subrc <> 0 OR <l_variable> IS NOT ASSIGNED.
          CONCATENATE output `{` INTO output RESPECTING BLANKS.
          CONTINUE.
        ENDIF.
      ELSE.
        CONCATENATE output `{` INTO output RESPECTING BLANKS.
        EXIT.
      ENDIF.

* Get format
      CLEAR l_format.

      IF input+l_offset(1) = ':'.
        l_offset = l_offset + 1.

        IF input+l_offset CA '}'.
          l_format = input+l_offset(sy-fdpos).
          l_offset = l_offset + sy-fdpos + 1.
        ELSE.
          EXIT.
        ENDIF.
      ELSE.
        l_offset = l_offset + 1.
      ENDIF.

* Write variable to output
      CALL METHOD any_to_string
        EXPORTING
          input    = <l_variable>
          format   = l_format
          language = language
        RECEIVING
          output   = l_value.

      CONCATENATE output l_value INTO output RESPECTING BLANKS.

      CHECK l_offset >= l_input_length.
      EXIT.
    ENDWHILE.

    IF l_offset < l_input_length.
      CONCATENATE output input+l_offset INTO output RESPECTING BLANKS.
    ENDIF.
  ENDMETHOD.                    "replace_variables


METHOD starts_with.
  DATA l_input        TYPE string.
  DATA l_part         TYPE string.

  DATA l_input_length TYPE i.
  DATA l_part_length  TYPE i.

* Convert parameters
  l_input = input.
  l_part  = part.

* Determine length
  l_input_length = strlen( input ).
  l_part_length  = strlen( part ).

* Always true for empty part
  IF l_part_length = 0.
    result = abap_true.
    RETURN.
  ENDIF.

* Check if part fits into string
  IF l_input_length < l_part_length.
    result = abap_false.
    RETURN.
  ENDIF.

* Actual comparison
  IF l_input(l_part_length) = l_part.
    result = abap_true.
  ELSE.
    result = abap_false.
  ENDIF.
ENDMETHOD.


  METHOD stringtable_to_texttable.
    DATA l_line_text TYPE REF TO data.

    FIELD-SYMBOLS <l_line_string> LIKE LINE OF input.
    FIELD-SYMBOLS <l_line_text>   TYPE any.

    CLEAR output.

    CREATE DATA l_line_text LIKE LINE OF output.
    ASSIGN l_line_text->* TO <l_line_text>.

    LOOP AT input ASSIGNING <l_line_string>.
      <l_line_text> = <l_line_string>.
      INSERT <l_line_text> INTO TABLE output.
    ENDLOOP.
  ENDMETHOD.


METHOD string_to_domvalue.
  DATA l_type_descr        TYPE REF TO cl_abap_typedescr.
  DATA l_elem_descr        TYPE REF TO cl_abap_elemdescr.

  DATA l_ddic_fixed_values TYPE ddfixvalues.
  DATA l_hit_counter       TYPE i.

  FIELD-SYMBOLS <l_ddic_fixed_values> LIKE LINE OF l_ddic_fixed_values.

* Initialize result
  CLEAR output.

* Get type description of output field
  l_type_descr = cl_abap_typedescr=>describe_by_data( output ).

  TRY.
      l_elem_descr ?= l_type_descr.

    CATCH cx_sy_move_cast_error.
      RETURN.

  ENDTRY.

* Get domain fixed values
  l_elem_descr->get_ddic_fixed_values( EXPORTING  p_langu        = language
                                       RECEIVING  p_fixed_values = l_ddic_fixed_values
                                       EXCEPTIONS OTHERS         = 1 ).
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* Try to find matching text
  LOOP AT l_ddic_fixed_values ASSIGNING <l_ddic_fixed_values>
       WHERE ddtext = input
         AND option = 'EQ'
         AND high IS INITIAL.
    IF l_hit_counter = 0.
      l_hit_counter = l_hit_counter + 1.
      output        = <l_ddic_fixed_values>-low.
    ELSE.
      CLEAR output. "Text is not unique
      EXIT.
    ENDIF.
  ENDLOOP.
ENDMETHOD.                    "domvalue_to_string


METHOD string_to_message.
  DATA l_msgv1 TYPE symsgv.
  DATA l_msgv2 TYPE symsgv.
  DATA l_msgv3 TYPE symsgv.
  DATA l_msgv4 TYPE symsgv.

  string_to_message_vars( EXPORTING input = input
                          IMPORTING msgv1 = l_msgv1
                                    msgv2 = l_msgv2
                                    msgv3 = l_msgv3
                                    msgv4 = l_msgv4 ).

  MESSAGE ID msgid TYPE msgty NUMBER msgno
        WITH l_msgv1 l_msgv2 l_msgv3 l_msgv4.
ENDMETHOD.                    "string_to_message


  METHOD STRING_TO_MESSAGE_VARS.
    DATA l_temp(200) TYPE c.

    CLEAR msgv2.
    CLEAR msgv3.
    CLEAR msgv4.

    l_temp = input.
    SHIFT l_temp LEFT DELETING LEADING space.

    msgv1 = l_temp(50).

    IF msgv2 IS REQUESTED AND l_temp+50 IS NOT INITIAL.
      WHILE l_temp+49(1) = space.
        SHIFT l_temp RIGHT.
      ENDWHILE.

      msgv2 = l_temp+50(50).

      IF msgv3 IS REQUESTED AND l_temp+100 IS NOT INITIAL.
        WHILE l_temp+99(1) = space.
          SHIFT l_temp+50 RIGHT.
        ENDWHILE.

        msgv3 = l_temp+100(50).

        IF msgv4 IS REQUESTED AND l_temp+150 IS NOT INITIAL.
          WHILE l_temp+149(1) = space.
            SHIFT l_temp+100 RIGHT.
          ENDWHILE.

          msgv4 = l_temp+150(50).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "string_to_message_vars


METHOD string_to_stringtable.
  DATA l_word_wrap_pos TYPE i.

  DATA l_position      TYPE i.
  DATA l_offset        TYPE i.
  DATA l_length        TYPE i.
  DATA l_index         TYPE i.

  DATA l_temp          TYPE string.

  FIELD-SYMBOLS <l_output> LIKE LINE OF output.

* Caculate word wrap position
  IF word_wrap_position < 0.
    l_word_wrap_pos = 0.
  ELSE.
    l_word_wrap_pos = word_wrap_position.
  ENDIF.

* Add lines to string table (this coding assumes the avery 0x0D if followed by 0x0A!)
  DO.
    IF input+l_position CA line_break_windows.
      l_offset = l_position + sy-fdpos.

      IF input+l_offset(1) = line_break_windows(1).
        APPEND input+l_position(sy-fdpos) TO output.
        l_position = l_position + sy-fdpos + 2.
      ELSE.
        APPEND input+l_position(sy-fdpos) TO output.
        l_position = l_position + sy-fdpos + 1.
      ENDIF.
    ELSE.
      APPEND input+l_position TO output.
      EXIT.
    ENDIF.
  ENDDO.

* Format output table
  LOOP AT output ASSIGNING <l_output>.
    l_index = sy-tabix + 1.

    IF NOT auto_trim IS INITIAL.
      CALL METHOD trim_right
        EXPORTING
          input  = <l_output>
        RECEIVING
          output = <l_output>.
    ENDIF.

    CHECK l_word_wrap_pos > 0.

* Check if word wrapping is required
    l_length = strlen( <l_output> ).
    CHECK l_length > l_word_wrap_pos.

    l_position = l_word_wrap_pos - 1.

* Calculate word wrap position
    WHILE l_position > 0.
      IF <l_output>+l_position(1) CA word_wrap_characters.
        l_position = l_position + 1.
        EXIT.
      ELSE.
        l_position = l_position - 1.
      ENDIF.
    ENDWHILE.

    IF l_position = 0.
      l_position = l_word_wrap_pos.
    ENDIF.

* Insert line break
    IF auto_trim = abap_true.
      l_temp = <l_output>+l_position.

      CALL METHOD trim_left
        EXPORTING
          input  = l_temp
        RECEIVING
          output = l_temp.

      INSERT l_temp INTO output INDEX l_index.

      l_temp = <l_output>(l_position).

      CALL METHOD trim_right
        EXPORTING
          input  = l_temp
        RECEIVING
          output = <l_output>.
    ELSE.
      INSERT <l_output>+l_position INTO output INDEX l_index.

      <l_output> = <l_output>(l_position).
    ENDIF.
  ENDLOOP.
ENDMETHOD.                    "string_to_stringtable


METHOD string_to_xstring.
  DATA l_conv TYPE REF TO cl_abap_conv_out_ce.

  cl_abap_conv_out_ce=>create( EXPORTING  encoding    = encoding
                                          endian      = endian
                                          replacement = replacement
                                          ignore_cerr = abap_true
                               RECEIVING  conv        = l_conv
                               EXCEPTIONS OTHERS      = 1 ).
  IF sy-subrc = 0.
    l_conv->write( EXPORTING  data   = input
                   EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc = 0.
      output = l_conv->get_buffer( ).
    ENDIF.
  ENDIF.
ENDMETHOD.


  METHOD texttable_to_stringtable.
    DATA l_line TYPE string.

    FIELD-SYMBOLS <l_line> TYPE any.

    LOOP AT input ASSIGNING <l_line>.
      l_line = <l_line>.
      INSERT l_line INTO TABLE output.
    ENDLOOP.
  ENDMETHOD.


METHOD timestamp_to_string.
  DATA l_timestamp TYPE timestamp.
  DATA l_date      TYPE d.
  DATA l_time      TYPE t.

  DATA l_date_str  TYPE string.
  DATA l_time_str  TYPE string.
  DATA l_tz_str    TYPE string.

* Use current time if no timestamp is supplied
  IF timestamp IS INITIAL.
    GET TIME STAMP FIELD l_timestamp.
  ELSE.
    l_timestamp = timestamp.
  ENDIF.

* Get date and time
  CONVERT TIME STAMP l_timestamp TIME ZONE timezone
     INTO DATE l_date TIME l_time.

* Build string
  IF format = format_default.
    output = TEXT-t02.

    IF timezone <> sy-zonlo.
      CONCATENATE output `[tz]` INTO output SEPARATED BY space.
    ENDIF.
  ELSEIF format = format_short.
    CONCATENATE TEXT-t03 TEXT-t04 INTO output SEPARATED BY space.

    IF timezone <> sy-zonlo.
      CONCATENATE output `[tz]` INTO output SEPARATED BY space.
    ENDIF.
  ELSE.
    output = format.
  ENDIF.

  IF output CS '[tz]'.
    l_tz_str = timezone.
    REPLACE '[tz]' WITH l_tz_str INTO output.
  ENDIF.

  IF output CS '[Date]'.
    l_date_str = date_to_string( input    = l_date
                                 language = language ).

    REPLACE '[Date]' WITH l_date_str INTO output.
  ENDIF.

  IF output CS '[Time]'.
    l_time_str = time_to_string( input    = l_time
                                 language = language ).

    REPLACE '[Time]' WITH l_time_str INTO output.
  ENDIF.

  IF output CA '['.
    output = date_to_string( input    = l_date
                             format   = output
                             language = language ).

    output = time_to_string( input    = l_time
                             format   = output
                             language = language ).
  ENDIF.
ENDMETHOD.                    "timestamp_to_string


METHOD time_to_string.
  DATA l_language_backup TYPE langu.

  DATA l_hour_temp(2)    TYPE n.

  DATA l_hour_24_short   TYPE string.
  DATA l_hour_12_short   TYPE string.
  DATA l_hour_12_long    TYPE string.

  DATA l_am_pm_upper     TYPE string.
  DATA l_am_pm_lower     TYPE string.

* Backup current language and set desired language
  l_language_backup = sy-langu.

  IF l_language_backup <> language.
    SET LANGUAGE language.
  ENDIF.

* Determine hour
  l_hour_temp = input(2).

  l_hour_24_short = l_hour_temp.
  SHIFT l_hour_24_short LEFT DELETING LEADING '0'.

  IF l_hour_temp < 12.                                   "#EC NUMBER_OK
    l_am_pm_upper = `AM`.
    l_am_pm_lower = `am`.
  ELSE.
    l_am_pm_upper = `PM`.
    l_am_pm_lower = `pm`.
  ENDIF.

  l_hour_temp = l_hour_temp MOD 12.                      "#EC NUMBER_OK
  IF l_hour_temp = 0.
    l_hour_temp = 12.                                    "#EC NUMBER_OK
  ENDIF.

  l_hour_12_long  = l_hour_temp.
  l_hour_12_short = l_hour_temp.
  SHIFT l_hour_12_short LEFT DELETING LEADING '0'.

* Determine format
  CASE format.

    WHEN format_default.
      output = TEXT-t01.

    WHEN format_short.
      output = TEXT-t04.

    WHEN OTHERS.
      output = format.

  ENDCASE.

* Build time
  REPLACE ALL OCCURRENCES OF `[hh24]` IN output WITH input(2).
  REPLACE ALL OCCURRENCES OF `[hh12]` IN output WITH l_hour_12_long.
  REPLACE ALL OCCURRENCES OF `[h24]`  IN output WITH l_hour_24_short.
  REPLACE ALL OCCURRENCES OF `[h12]`  IN output WITH l_hour_12_short.
  REPLACE ALL OCCURRENCES OF `[mm]`   IN output WITH input+2(2).
  REPLACE ALL OCCURRENCES OF `[ss]`   IN output WITH input+4(2).
  REPLACE ALL OCCURRENCES OF `[AMPM]` IN output WITH l_am_pm_upper.
  REPLACE ALL OCCURRENCES OF `[ampm]` IN output WITH l_am_pm_lower.

* Restore language
  IF l_language_backup <> language.
    SET LANGUAGE l_language_backup.
  ENDIF.
ENDMETHOD.                    "time_to_string


METHOD trim.
  DATA l_start  TYPE i.
  DATA l_end    TYPE i.
  DATA l_length TYPE i.

  IF input CO whitespace_chars.
    CLEAR output.
  ELSE.
    l_start  = sy-fdpos.

    CALL METHOD get_length
      EXPORTING
        input            = input
        whitespace_chars = whitespace_chars
      RECEIVING
        length           = l_end.

    l_length = l_end - l_start.
    output   = input+l_start(l_length).
  ENDIF.
ENDMETHOD.                    "trim


  METHOD TRIM_LEFT.
    IF input CO whitespace_chars.
      CLEAR output.
    ELSE.
      output = input+sy-fdpos.
    ENDIF.
  ENDMETHOD.                    "trim_left


  METHOD TRIM_RIGHT.
    DATA l_length TYPE i.

    CALL METHOD get_length
      EXPORTING
        input            = input
        whitespace_chars = whitespace_chars
      RECEIVING
        length           = l_length.

    IF l_length > 0.
      output = input(l_length).
    ELSE.
      CLEAR output.
    ENDIF.
  ENDMETHOD.                    "trim_right


METHOD xstring_to_string.
  DATA l_conv TYPE REF TO cl_abap_conv_in_ce.

  cl_abap_conv_in_ce=>create( EXPORTING  encoding    = encoding
                                         endian      = endian
                                         replacement = replacement
                                         ignore_cerr = abap_true
                                         input       = input
                              RECEIVING  conv        = l_conv
                              EXCEPTIONS OTHERS      = 1 ).
  IF sy-subrc = 0.
    l_conv->read( IMPORTING  data   = output
                  EXCEPTIONS OTHERS = 0 ).
  ENDIF.
ENDMETHOD.
ENDCLASS.
