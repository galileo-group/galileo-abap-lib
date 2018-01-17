FUNCTION /gal/file_read.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(FULL_NAME) TYPE  STRING
*"     REFERENCE(MODE) TYPE  /GAL/FILE_MODE
*"     REFERENCE(LINE_BREAK) TYPE  /GAL/FILE_LINE_BREAK OPTIONAL
*"     REFERENCE(POSITION) TYPE  I DEFAULT -1
*"     REFERENCE(LENGTH) TYPE  I DEFAULT -1
*"  EXPORTING
*"     REFERENCE(DATA_BIN) TYPE  XSTRING
*"     REFERENCE(DATA_TXT) TYPE  /GAL/STRINGTABLE
*"  EXCEPTIONS
*"      CANNOT_READ_FILE
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  CONSTANTS lc_line_break TYPE x VALUE '0A'.

  DATA l_auth_fname TYPE fileextern.
  DATA l_message    TYPE string.
  DATA l_exception  TYPE REF TO cx_root.

  DATA l_line       TYPE string.
  DATA l_count      TYPE i.

* Initialize result
  CLEAR data_bin.
  CLEAR data_txt.

* Follow RFC route
  cfw_follow_rfc_route rfc_route_info.

  cfw_pass_exception cannot_read_file.
  cfw_pass_exception rfc_exception.

  cfw_remote_coding.

* Check authorization
  l_auth_fname = full_name.

  CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
    EXPORTING
      activity = sabc_act_read
      filename = l_auth_fname
    EXCEPTIONS
      OTHERS   = 1.
  IF NOT sy-subrc = 0.
    MESSAGE e001 WITH full_name RAISING cannot_read_file.
  ENDIF.

* Read file
  TRY.

      CASE mode.

        WHEN /gal/file=>mode_binary.
          IF position < 0.
            OPEN DATASET full_name
                 FOR INPUT IN BINARY MODE
                 MESSAGE l_message.
          ELSE.
            OPEN DATASET full_name
                 FOR INPUT IN BINARY MODE
                 AT POSITION position
                 MESSAGE l_message.
          ENDIF.

          IF sy-subrc <> 0.
            MESSAGE e002 WITH full_name l_message RAISING cannot_read_file.
          ENDIF.

          IF length < 0.
            READ DATASET full_name INTO data_bin.
          ELSE.
            READ DATASET full_name INTO data_bin MAXIMUM LENGTH length.
          ENDIF.

          CLOSE DATASET full_name.

        WHEN /gal/file=>mode_text.
          CASE line_break.

            WHEN /gal/file=>line_break_unix.
              OPEN DATASET full_name
                   FOR INPUT IN TEXT MODE
                   ENCODING DEFAULT
                   WITH UNIX LINEFEED
                   MESSAGE l_message.

            WHEN /gal/file=>line_break_windows.
              OPEN DATASET full_name
                   FOR INPUT IN TEXT MODE
                   ENCODING DEFAULT
                   WITH WINDOWS LINEFEED
                   MESSAGE l_message.

            WHEN OTHERS.
              OPEN DATASET full_name
                   FOR INPUT IN TEXT MODE
                   ENCODING DEFAULT
                   WITH SMART LINEFEED
                   MESSAGE l_message.

          ENDCASE.

          IF sy-subrc <> 0.
            MESSAGE e002 WITH full_name l_message RAISING cannot_read_file.
          ENDIF.

          DO position TIMES.
            READ DATASET full_name INTO l_line.
          ENDDO.

          l_count = 0.

          WHILE length < 0 OR l_count < length.
            READ DATASET full_name INTO l_line.
            IF sy-subrc = 0.
              INSERT l_line INTO TABLE data_txt.
              l_count = l_count + 1.
            ELSE.
              EXIT.
            ENDIF.
          ENDWHILE.

          CLOSE DATASET full_name.

* Workaround: READ DATASET omits trailing empty line
          DATA l_last_byte_pos TYPE i.
          DATA l_last_byte     TYPE x.

          OPEN DATASET full_name FOR INPUT IN BINARY MODE AT POSITION l_last_byte_pos MESSAGE l_message.
          IF sy-subrc <> 0.
            MESSAGE e002 WITH full_name l_message RAISING cannot_read_file.
          ENDIF.

          SET DATASET full_name POSITION END OF FILE.
          GET DATASET full_name POSITION l_last_byte_pos.

          l_last_byte_pos = l_last_byte_pos - 1.

          SET DATASET full_name POSITION l_last_byte_pos.

          READ DATASET full_name INTO l_last_byte.
          CLOSE DATASET full_name.

          IF l_last_byte = lc_line_break.
            INSERT INITIAL LINE INTO TABLE data_txt.
          ENDIF.

      ENDCASE.

    CATCH cx_sy_file_access_error INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE e003 WITH full_name l_message RAISING cannot_read_file.

    CLEANUP.
      CLOSE DATASET full_name.

  ENDTRY.
ENDFUNCTION.
