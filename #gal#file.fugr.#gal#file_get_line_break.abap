FUNCTION /gal/file_get_line_break.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(FULL_NAME) TYPE  STRING
*"  EXPORTING
*"     REFERENCE(LINE_BREAK) TYPE  /GAL/FILE_LINE_BREAK
*"  EXCEPTIONS
*"      CANNOT_GET_LINE_BREAK_STYLE
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  CONSTANTS lc_block_length TYPE i VALUE 1025.
  CONSTANTS lc_block_offset TYPE i VALUE 1024.

  CONSTANTS lc_ascii_lf TYPE x VALUE '0A'.
  CONSTANTS lc_ascii_cr TYPE x VALUE '0D'.

  DATA l_auth_fname TYPE fileextern.
  DATA l_message    TYPE string.
  DATA l_exception  TYPE REF TO cx_root.

  DATA l_buffer     TYPE xstring.
  DATA l_position   TYPE i.
  DATA l_offset     TYPE i.

* Initialize result
  CLEAR line_break.

* Follow RFC route
  cfw_follow_rfc_route rfc_route_info.

  cfw_pass_exception cannot_get_line_break_style.
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
    MESSAGE e001 WITH full_name RAISING cannot_get_line_break_style.
  ENDIF.

* Get line break style of first line break
  line_break = /gal/file=>line_break_undefined.

  TRY.
      OPEN DATASET full_name
           FOR INPUT IN BINARY MODE
           MESSAGE l_message.
      IF sy-subrc <> 0.
        RETURN. " Assuming that file does not exist
      ENDIF.

      l_position = 0.

      DO.
        READ DATASET full_name INTO l_buffer MAXIMUM LENGTH lc_block_length.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        IF l_buffer BYTE-CA lc_ascii_lf.
          line_break = /gal/file=>line_break_unix.

          IF sy-fdpos > 0.
            l_offset = sy-fdpos - 1.

            IF l_buffer+l_offset(1) = lc_ascii_cr.
              line_break = /gal/file=>line_break_windows.
            ENDIF.
          ENDIF.

          EXIT.
        ENDIF.

        l_position = l_position + lc_block_offset.

        SET DATASET full_name POSITION l_position.
      ENDDO.

      CLOSE DATASET full_name.

    CATCH cx_sy_file_access_error INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE e003 WITH full_name l_message RAISING cannot_get_line_break_style.

    CLEANUP.
      CLOSE DATASET full_name.

  ENDTRY.
ENDFUNCTION.
