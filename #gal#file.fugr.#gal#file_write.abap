FUNCTION /gal/file_write.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(FULL_NAME) TYPE  STRING
*"     REFERENCE(MODE) TYPE  /GAL/FILE_MODE
*"     REFERENCE(OPTIONS) TYPE  /GAL/FILE_OPTIONS
*"     REFERENCE(LINE_BREAK) TYPE  /GAL/FILE_LINE_BREAK OPTIONAL
*"     REFERENCE(DATA_BIN) TYPE  XSTRING OPTIONAL
*"     REFERENCE(DATA_TXT) TYPE  /GAL/STRINGTABLE OPTIONAL
*"  EXCEPTIONS
*"      CANNOT_WRITE_FILE
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA l_auth_fname       LIKE authb-filename.

  DATA l_options_create   TYPE /gal/file_options.
  DATA l_options_truncate TYPE /gal/file_options.
  DATA l_options_append   TYPE /gal/file_options.
  DATA l_options_recreate TYPE /gal/file_options.

  DATA l_exists           TYPE abap_bool.

  DATA l_message          TYPE string.
  DATA l_exception        TYPE REF TO cx_root.

  FIELD-SYMBOLS <l_line>  TYPE string.

  cfw_follow_rfc_route rfc_route_info.

  cfw_pass_exception cannot_write_file.
  cfw_pass_exception rfc_exception.

  cfw_remote_coding.

* Check authorization
  l_auth_fname = full_name.

  CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
    EXPORTING
      activity = sabc_act_write
      filename = l_auth_fname
    EXCEPTIONS
      OTHERS   = 1.
  IF NOT sy-subrc = 0.
    MESSAGE e001 WITH full_name RAISING cannot_write_file.
  ENDIF.

* Make sure that options are defined
  IF options = 0.
    MESSAGE e007 WITH full_name RAISING cannot_write_file.
  ENDIF.

* Analyze options
  l_options_create   = options BIT-AND /gal/file=>options_create_only.
  l_options_truncate = options BIT-AND /gal/file=>options_truncate.
  l_options_append   = options BIT-AND /gal/file=>options_append_only.
  l_options_recreate = options BIT-AND /gal/file=>options_recreate_only.

* Truncate, re-create and append are exclusive
  IF l_options_truncate > 0 AND ( l_options_append   > 0 OR l_options_recreate > 0 ) OR
     l_options_append   > 0 AND ( l_options_truncate > 0 OR l_options_recreate > 0 ) OR
     l_options_recreate > 0 AND ( l_options_append   > 0 OR l_options_truncate > 0 ).

    MESSAGE e008 WITH full_name RAISING cannot_write_file.
  ENDIF.

* Check if file exists
  OPEN DATASET full_name
       FOR INPUT
       IN BINARY MODE
       MESSAGE l_message.
  IF sy-subrc = 0.
    CLOSE DATASET full_name.
    l_exists = abap_true.
  ELSE.
    l_exists = abap_false.
  ENDIF.

* Error if file exists and mode is create only
  IF l_options_create   > 0 AND
     l_options_truncate = 0 AND
     l_options_append   = 0 AND
     l_options_recreate = 0 AND
     l_exists = abap_true.

    MESSAGE e004 WITH full_name RAISING cannot_write_file.
  ENDIF.

* Error if file does not exist and mode does not allow file creation
  IF l_options_create = 0 AND l_exists = abap_false.
    MESSAGE e006 WITH full_name RAISING cannot_write_file.
  ENDIF.

  TRY.

* Delete file if re-create is requested
      IF l_options_recreate > 0.
        DELETE DATASET full_name.
        IF sy-subrc <> 0.
          MESSAGE e012 WITH full_name RAISING cannot_write_file.
        ENDIF.

        l_exists = abap_false.
      ENDIF.

* Write file
      CASE mode.

        WHEN /gal/file=>mode_binary.
          IF l_exists = abap_false.
            OPEN DATASET full_name
                 FOR OUTPUT IN BINARY MODE
                 MESSAGE l_message.
            IF sy-subrc <> 0.
              MESSAGE e005 WITH full_name l_message RAISING cannot_write_file.
            ENDIF.
          ELSEIF l_options_truncate > 0.
            OPEN DATASET full_name
                 FOR UPDATE IN BINARY MODE
                 MESSAGE l_message.
            IF sy-subrc <> 0.
              MESSAGE e009 WITH full_name l_message RAISING cannot_write_file.
            ENDIF.

            TRUNCATE DATASET full_name
                     AT POSITION 0.
          ELSEIF l_options_append > 0.
            OPEN DATASET full_name
                 FOR APPENDING
                 IN BINARY MODE
                 MESSAGE l_message.
            IF sy-subrc <> 0.
              MESSAGE e010 WITH full_name l_message RAISING cannot_write_file.
            ENDIF.
          ENDIF.

          TRANSFER data_bin TO full_name.

          CLOSE DATASET full_name.

        WHEN /gal/file=>mode_text.
          IF l_exists = abap_false.
            CASE line_break.

              WHEN /gal/file=>line_break_unix.
                OPEN DATASET full_name
                     FOR OUTPUT
                     IN TEXT MODE
                     WITH UNIX LINEFEED
                     ENCODING DEFAULT
                     MESSAGE l_message.

              WHEN /gal/file=>line_break_windows.
                OPEN DATASET full_name
                     FOR OUTPUT
                     IN TEXT MODE
                     WITH WINDOWS LINEFEED
                     ENCODING DEFAULT
                     MESSAGE l_message.

              WHEN OTHERS.
                OPEN DATASET full_name
                     FOR OUTPUT
                     IN TEXT MODE
                     WITH SMART LINEFEED
                     ENCODING DEFAULT
                     MESSAGE l_message.

            ENDCASE.

            IF sy-subrc <> 0.
              MESSAGE e005 WITH full_name l_message RAISING cannot_write_file.
            ENDIF.
          ELSEIF l_options_truncate > 0.
            CASE line_break.

              WHEN /gal/file=>line_break_unix.
                OPEN DATASET full_name
                     FOR UPDATE
                     IN TEXT MODE
                     WITH UNIX LINEFEED
                     ENCODING DEFAULT
                     MESSAGE l_message.

              WHEN /gal/file=>line_break_windows.
                OPEN DATASET full_name
                     FOR UPDATE
                     IN TEXT MODE
                     WITH WINDOWS LINEFEED
                     ENCODING DEFAULT
                     MESSAGE l_message.

              WHEN OTHERS.
                OPEN DATASET full_name
                     FOR UPDATE
                     IN TEXT MODE
                     WITH SMART LINEFEED
                     ENCODING DEFAULT
                     MESSAGE l_message.

            ENDCASE.

            IF sy-subrc <> 0.
              MESSAGE e009 WITH full_name l_message RAISING cannot_write_file.
            ENDIF.

            TRUNCATE DATASET full_name AT POSITION 0.
          ELSEIF l_options_append > 0.
            CASE line_break.

              WHEN /gal/file=>line_break_unix.
                OPEN DATASET full_name
                     FOR APPENDING
                     IN TEXT MODE
                     WITH UNIX LINEFEED
                     ENCODING DEFAULT
                     MESSAGE l_message.

              WHEN /gal/file=>line_break_windows.
                OPEN DATASET full_name
                     FOR APPENDING
                     IN TEXT MODE
                     WITH WINDOWS LINEFEED
                     ENCODING DEFAULT
                     MESSAGE l_message.

              WHEN OTHERS.
                OPEN DATASET full_name
                     FOR APPENDING
                     IN TEXT MODE
                     WITH SMART LINEFEED
                     ENCODING DEFAULT
                     MESSAGE l_message.

            ENDCASE.
            IF sy-subrc <> 0.
              MESSAGE e010 WITH full_name l_message RAISING cannot_write_file.
            ENDIF.
          ENDIF.

          LOOP AT data_txt ASSIGNING <l_line>.
            AT LAST.
              TRANSFER <l_line> TO full_name NO END OF LINE.
              CONTINUE.
            ENDAT.

            TRANSFER <l_line> TO full_name.
          ENDLOOP.

          CLOSE DATASET full_name.

      ENDCASE.

    CATCH cx_root INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE e011 WITH full_name l_message RAISING cannot_write_file.

    CLEANUP.
      CLOSE DATASET full_name.

  ENDTRY.
ENDFUNCTION.
