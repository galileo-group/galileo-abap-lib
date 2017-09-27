FUNCTION /gal/file_get_dir_content.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO
*"     REFERENCE(PATH) TYPE  STRING
*"     REFERENCE(GENERIC_NAME) TYPE  FILENAME_AL11 OPTIONAL
*"     REFERENCE(FILTER) TYPE  STRING OPTIONAL
*"     REFERENCE(MAX_ENTRIES) TYPE  I DEFAULT 0
*"  EXPORTING
*"     REFERENCE(CONTENT) TYPE  /GAL/DIRECTORY_CONTENT
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"      IO_EXCEPTION
*"----------------------------------------------------------------------

  DATA l_separator        TYPE string.
  DATA l_result           TYPE abap_bool.

  DATA l_path             TYPE filename_al11.
  DATA l_object           TYPE /gal/directory_content_entry.
  DATA l_counter          TYPE i.
  DATA l_opcode           TYPE x.
  DATA l_timestamp_i      TYPE i.
  DATA l_timestamp_c(14)  TYPE c.

  DATA l_name(255)        TYPE c.
  DATA l_type(255)        TYPE c.
  DATA l_owner(255)       TYPE c.

  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.

* Get directory separator
  CALL FUNCTION '/GAL/FILE_GET_DIR_SEPARATOR'
    IMPORTING
      separator = l_separator
    EXCEPTIONS
      OTHERS    = 0.

* Clear separator if it is already part of the base path
  l_result = /gal/string=>ends_with( input = path
                                     part  = l_separator ).
  IF l_result = abap_true.
    CLEAR l_separator.
  ENDIF.

* Cleanup (just in case)
  CALL 'C_DIR_READ_FINISH' ID 'ERRNO'  FIELD l_object-error_no
                           ID 'ERRMSG' FIELD l_object-error_message. "#EC CI_CCALL

* Start reading directory
  CLEAR content.

* Special case: Root path on windows systems requires an empty path
  IF path = `\`.
    CLEAR l_path.
  ELSEIF path <> l_separator.
    l_path = /gal/path=>remove_separator( path      = path
                                          separator = l_separator ).
  ELSE.
    l_path = path.
  ENDIF.

  CALL 'C_DIR_READ_START' ID 'DIR'    FIELD l_path
                          ID 'FILE'   FIELD generic_name
                          ID 'ERRNO'  FIELD l_object-error_no
                          ID 'ERRMSG' FIELD l_object-error_message. "#EC CI_CCALL
  IF sy-subrc <> 0.
    IF l_object-error_message IS INITIAL.
      MESSAGE e014(/gal/file) WITH l_path
                           RAISING io_exception.
    ELSE.
      MESSAGE e000(/gal/file) WITH l_object-error_no
                                   l_object-error_message
                           RAISING io_exception.
    ENDIF.
  ENDIF.

  DO.
    CLEAR l_object.

* Get next file or directory
    CALL 'C_DIR_READ_NEXT' ID 'TYPE'   FIELD l_type
                           ID 'NAME'   FIELD l_name
                           ID 'LEN'    FIELD l_object-length
                           ID 'OWNER'  FIELD l_owner
                           ID 'MTIME'  FIELD l_object-mod_system_time
                           ID 'MODE'   FIELD l_object-mode
                           ID 'ERRNO'  FIELD l_object-error_no
                           ID 'ERRMSG' FIELD l_object-error_message. "#EC CI_CCALL
    IF sy-subrc = 1.
      EXIT.
    ELSEIF sy-subrc = 3.
      MESSAGE e000(/gal/file) WITH l_object-error_no
                                   l_object-error_message
                           RAISING io_exception.
    ELSEIF sy-subrc <> 0 AND sy-subrc <> 5.
      CONTINUE.
    ENDIF.

    l_object-name        = l_name.
    l_object-type        = l_type.
    l_object-owner       = l_owner.
    l_object-return_code = sy-subrc.

    l_object-path = /gal/path=>combine( path1     = path
                                        path2     = l_object-name
                                        separator = l_separator ).

    IF l_object-type CA 'Dd' AND l_object-length = 0.
      l_object-is_directory = abap_true.
    ELSE.
      l_object-is_directory = abap_false.

      CHECK filter IS INITIAL OR l_separator = `\` AND filter = `*.*` OR l_object-name CP filter.
    ENDIF.

* Convert time stamps
    l_opcode      = 3.
    l_timestamp_i = l_object-mod_system_time.

    CALL 'RstrDateConv'
      ID 'OPCODE'    FIELD l_opcode
      ID 'TIMESTAMP' FIELD l_timestamp_i
      ID 'ABAPSTAMP' FIELD l_timestamp_c.                 "#EC CI_CCALL

    l_object-mod_timestamp = l_timestamp_c.
    l_object-mod_timezone  = sy-zonlo.

    CONVERT TIME STAMP l_object-mod_timestamp TIME ZONE l_object-mod_timezone
       INTO DATE l_object-mod_date TIME l_object-mod_time.

    INSERT l_object INTO TABLE content.

    l_counter = l_counter + 1.

    IF max_entries > 0 AND l_counter >= max_entries.
      EXIT.
    ENDIF.
  ENDDO.

* Cleanup
  CALL 'C_DIR_READ_FINISH' ID 'ERRNO'  FIELD l_object-error_no
                           ID 'ERRMSG' FIELD l_object-error_message. "#EC CI_CCALL

  SORT content BY is_directory DESCENDING name ASCENDING.
ENDFUNCTION.
