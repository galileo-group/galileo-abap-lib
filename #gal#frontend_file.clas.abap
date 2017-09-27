class /GAL/FRONTEND_FILE definition
  public
  inheriting from /GAL/FILE
  final
  create public .

*"* public components of class /GAL/FRONTEND_FILE
*"* do not include other source files here!!!
public section.

  class-methods SELECT
    importing
      !ACCESS type /GAL/FILE_ACCESS default ACCESS_READ
      !DEFAULT_DIRECTORY type CSEQUENCE optional
      !DEFAULT_NAME type CSEQUENCE optional
      !DEFAULT_EXTENSION type CSEQUENCE optional
      !TITLE type CSEQUENCE optional
      !FILTER type CSEQUENCE optional
    returning
      value(FILE) type ref to /GAL/FRONTEND_FILE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !NAME type CSEQUENCE .

  methods DELETE
    redefinition .
  methods EXISTS
    redefinition .
  methods GET_LENGTH
    redefinition .
  methods OPEN
    redefinition .
protected section.
*"* protected components of class /GAL/FRONTEND_FILE
*"* do not include other source files here!!!

  methods READ
    redefinition .
  methods WRITE
    redefinition .
private section.
*"* private components of class /GAL/FRONTEND_FILE
*"* do not include other source files here!!!
ENDCLASS.



CLASS /GAL/FRONTEND_FILE IMPLEMENTATION.


METHOD constructor.

* Call parent implementation
  super->constructor( name = name ).
ENDMETHOD.


METHOD delete.
  DATA l_rc TYPE i.

* Delete file
  CALL METHOD cl_gui_frontend_services=>file_delete
    EXPORTING
      filename       = full_name
    CHANGING
      rc             = l_rc
    EXCEPTIONS
      file_not_found = 1
      access_denied  = 2
      OTHERS         = 3.

  CASE sy-subrc.

    WHEN 1.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
            EXPORTING textid = /gal/cx_io_exception=>file_does_not_exist
                      var1   = full_name.

    WHEN 2.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
            EXPORTING textid = /gal/cx_io_exception=>file_access_denied
                      var1   = full_name.

    WHEN 3.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
            EXPORTING textid = /gal/cx_io_exception=>cannot_delete_file
                      var1   = full_name.

  ENDCASE.
ENDMETHOD.


METHOD exists.
  cl_gui_frontend_services=>file_exist( EXPORTING  file                 = full_name
                                        RECEIVING  result               = file_exists
                                        EXCEPTIONS error_no_gui         = 1
                                                   not_supported_by_gui = 2
                                                   OTHERS               = 3 ).
  IF sy-subrc <> 0.
    CASE sy-subrc.

      WHEN 1.
        RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING
            textid = /gal/cx_io_exception=>cannot_access_local_file09
            var1   = full_name.

      WHEN 2.
        RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING
            textid = /gal/cx_io_exception=>cannot_access_local_file08
            var1   = full_name.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING
            textid = /gal/cx_io_exception=>cannot_access_local_file10
            var1   = full_name.

    ENDCASE.
  ENDIF.
ENDMETHOD.


METHOD get_length.
  CALL METHOD cl_gui_frontend_services=>file_get_size
    EXPORTING
      file_name            = full_name
    IMPORTING
      file_size            = length
    EXCEPTIONS
      file_get_size_failed = 1
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 1.
  IF sy-subrc <> 0.
    CASE sy-subrc.

      WHEN 1.
        RAISE EXCEPTION TYPE /gal/cx_io_exception
              EXPORTING textid = /gal/cx_io_exception=>cannot_get_file_length
                        var1   = full_name.

      WHEN 2.
        RAISE EXCEPTION TYPE /gal/cx_io_exception
              EXPORTING textid = /gal/cx_io_exception=>cannot_access_local_file09
                        var1   = full_name.

      WHEN 3.
        RAISE EXCEPTION TYPE /gal/cx_io_exception
              EXPORTING textid = /gal/cx_io_exception=>cannot_access_local_file08
                        var1   = full_name.

    ENDCASE.
  ENDIF.
ENDMETHOD.


METHOD open.
  DATA l_line_break LIKE line_break.
  DATA l_platform   TYPE i.

  DATA l_read   TYPE /gal/file_access.
  DATA l_append TYPE /gal/file_options.

  super->open( ).

* Automatic determination of line break style
  IF mode = mode_text.
    l_read   = access BIT-AND access_read.
    l_append = options BIT-AND options_append_only.

* If no line break style is supplied get line break style from existing file
    IF ( l_read > 0 OR l_append > 0 ) AND ( line_break = line_break_auto OR line_break = line_break_undefined ).
      l_line_break = get_line_break( ).
    ELSE.
      l_line_break = line_break.
    ENDIF.

* Use plattform to get line break style when line_break is set to auto
    IF l_line_break = line_break_auto.
      cl_gui_frontend_services=>get_platform( RECEIVING  platform = l_platform
                                              EXCEPTIONS OTHERS   = 1 ).
      IF sy-subrc = 0 AND ( l_platform = cl_gui_frontend_services=>platform_windows95 OR
                            l_platform = cl_gui_frontend_services=>platform_windows98 OR
                            l_platform = cl_gui_frontend_services=>platform_nt351     OR
                            l_platform = cl_gui_frontend_services=>platform_nt40      OR
                            l_platform = cl_gui_frontend_services=>platform_nt50      OR
                            l_platform = cl_gui_frontend_services=>platform_windowsxp ).
        l_line_break = line_break_windows.
      ELSE.
        l_line_break = line_break_unix.
      ENDIF.
    ENDIF.
  ELSE.
    l_line_break = line_break_undefined.
  ENDIF.

* Create stream
  CREATE OBJECT stream TYPE /gal/mem_buffered_file_stream
    EXPORTING
      file       = me
      access     = access
      mode       = mode
      options    = options
      line_break = l_line_break.
ENDMETHOD.


METHOD read.
  CONSTANTS lc_line_size_bin TYPE i VALUE 4096.

  TYPES lt_data_bin_type(lc_line_size_bin) TYPE x.

  DATA lt_data_bin TYPE STANDARD TABLE OF lt_data_bin_type.
  DATA lt_data_txt TYPE STANDARD TABLE OF string.

  DATA l_file_size TYPE i.

  CLEAR data_bin.
  CLEAR data_txt.

  IF mode = mode_text.
    cl_gui_frontend_services=>gui_upload( EXPORTING  filename                = full_name
                                                     filetype                = 'ASC'
                                          IMPORTING  filelength              = l_file_size
                                          CHANGING   data_tab                = lt_data_txt
                                          EXCEPTIONS file_open_error         = 1
                                                     file_read_error         = 2
                                                     no_batch                = 3
                                                     gui_refuse_filetransfer = 4
                                                     invalid_type            = 5
                                                     no_authority            = 6
                                                     unknown_error           = 1
                                                     bad_data_format         = 5
                                                     header_not_allowed      = 5
                                                     separator_not_allowed   = 5
                                                     header_too_long         = 5
                                                     unknown_dp_error        = 1
                                                     access_denied           = 7
                                                     dp_out_of_memory        = 8
                                                     disk_full               = 9
                                                     dp_timeout              = 10
                                                     not_supported_by_gui    = 11
                                                     error_no_gui            = 12
                                                     OTHERS                  = 1 ).
    IF sy-subrc = 0.
      IF line_break = line_break_unix.
        CONCATENATE LINES OF lt_data_txt
               INTO data_txt SEPARATED BY /gal/string=>line_break_unix.
      ELSE.
        CONCATENATE LINES OF lt_data_txt
               INTO data_txt SEPARATED BY /gal/string=>line_break_windows.
      ENDIF.

      RETURN.
    ENDIF.
  ELSE.
    cl_gui_frontend_services=>gui_upload( EXPORTING  filename                = full_name
                                                     filetype                = 'BIN'
                                          IMPORTING  filelength              = l_file_size
                                          CHANGING   data_tab                = lt_data_bin
                                          EXCEPTIONS file_open_error         = 1
                                                     file_read_error         = 2
                                                     no_batch                = 3
                                                     gui_refuse_filetransfer = 4
                                                     invalid_type            = 5
                                                     no_authority            = 6
                                                     unknown_error           = 1
                                                     bad_data_format         = 5
                                                     header_not_allowed      = 5
                                                     separator_not_allowed   = 5
                                                     header_too_long         = 5
                                                     unknown_dp_error        = 1
                                                     access_denied           = 7
                                                     dp_out_of_memory        = 8
                                                     disk_full               = 9
                                                     dp_timeout              = 10
                                                     not_supported_by_gui    = 11
                                                     error_no_gui            = 12
                                                     OTHERS                  = 1 ).
    IF sy-subrc = 0.
      CONCATENATE LINES OF lt_data_bin INTO data_bin IN BYTE MODE.
      data_bin = data_bin(l_file_size).
      RETURN.
    ENDIF.
  ENDIF.

  CASE sy-subrc.

    WHEN 1.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_open_file
          var1   = full_name.

    WHEN 2.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_read_file
          var1   = full_name.

    WHEN 3.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file01
          var1   = full_name.

    WHEN 4.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file02
          var1   = full_name.

    WHEN 5.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>incorrect_file_type
          var1   = full_name.

    WHEN 6.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file03
          var1   = full_name.

    WHEN 7.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file04
          var1   = full_name.

    WHEN 8.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file05
          var1   = full_name.

    WHEN 9.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file06
          var1   = full_name.

    WHEN 10.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file07
          var1   = full_name.

    WHEN 11.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file08
          var1   = full_name.

    WHEN 12.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file09
          var1   = full_name.

  ENDCASE.
ENDMETHOD.


METHOD select.
  DATA l_default_directory TYPE string.
  DATA l_default_name      TYPE string.
  DATA l_default_extension TYPE string.
  DATA l_title             TYPE string.
  DATA l_filter            TYPE string.

  DATA l_file_table        TYPE filetable.
  DATA l_dialog_rc         TYPE i.
  DATA l_action            TYPE i.

  DATA l_filename          TYPE string.

  DATA l_dummy1            TYPE string.
  DATA l_dummy2            TYPE string.

  FIELD-SYMBOLS <l_file_table> LIKE LINE OF l_file_table.

* Convert parameters to string
  l_default_directory = default_directory.
  l_default_name      = default_name.
  l_default_extension = default_extension.

* Set title and filter
  IF title IS SUPPLIED.
    l_title = title.
  ELSE.
    l_title = text-c00.
  ENDIF.

  IF filter IS SUPPLIED.
    l_filter = filter.
  ELSE.
    l_filter = text-c01.
  ENDIF.

* Display file selection dialog
  IF access = access_read.
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        initial_directory = l_default_directory
        default_filename  = l_default_name
        default_extension = l_default_extension
        window_title      = l_title
        file_filter       = l_filter
      CHANGING
        file_table        = l_file_table
        rc                = l_dialog_rc
        user_action       = l_action
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>file_selection_dialog_failed.
    ELSEIF l_action = cl_gui_frontend_services=>action_cancel.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>aborted_by_user.
    ELSEIF l_dialog_rc < 1.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>file_selection_dialog_failed.
    ENDIF.

    READ TABLE l_file_table INDEX 1 ASSIGNING <l_file_table>.
    l_filename = <l_file_table>-filename.
  ELSE.
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title      = l_title
        initial_directory = l_default_directory
        default_file_name = l_default_name
        default_extension = l_default_extension
        file_filter       = l_filter
      CHANGING
        filename          = l_dummy1
        path              = l_dummy2
        fullpath          = l_filename
        user_action       = l_action
      EXCEPTIONS
        OTHERS            = 1.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>file_selection_dialog_failed.
    ELSEIF l_action = cl_gui_frontend_services=>action_cancel.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>aborted_by_user.
    ELSEIF l_action <> cl_gui_frontend_services=>action_ok.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>file_selection_dialog_failed.
    ENDIF.
  ENDIF.

* Create file object
  CREATE OBJECT file TYPE /gal/frontend_file
    EXPORTING
      name = l_filename.
ENDMETHOD.


METHOD write.
  CONSTANTS lc_line_size_bin TYPE i VALUE 4096.

  TYPES lt_data_bin_type(lc_line_size_bin) TYPE x.

  DATA lt_data_bin TYPE STANDARD TABLE OF lt_data_bin_type.
  DATA lt_data_txt TYPE STANDARD TABLE OF string.

  DATA l_data_bin  LIKE LINE OF lt_data_bin.

  DATA l_options   TYPE /gal/file_options.
  DATA l_append    TYPE abap_bool.

  DATA l_file_size TYPE i.
  DATA l_offset    TYPE i.

  l_options = options BIT-AND options_append_only.
  IF l_options = 0.
    l_append = abap_false.
  ELSE.
    l_append = abap_true.
  ENDIF.

  IF mode = mode_text.
    INSERT data_txt INTO TABLE lt_data_txt.

    cl_gui_frontend_services=>gui_download( EXPORTING  filename                 = full_name
                                                       filetype                 = 'ASC'
                                                       append                   = l_append
                                                       write_lf                 = abap_false
                                                       write_lf_after_last_line = abap_false
                                            CHANGING   data_tab                 = lt_data_txt
                                            EXCEPTIONS file_write_error         = 1
                                                       no_batch                 = 2
                                                       gui_refuse_filetransfer  = 3
                                                       invalid_type             = 4
                                                       no_authority             = 5
                                                       unknown_error            = 1
                                                       header_not_allowed       = 4
                                                       separator_not_allowed    = 4
                                                       filesize_not_allowed     = 4
                                                       header_too_long          = 4
                                                       dp_error_create          = 1
                                                       dp_error_send            = 1
                                                       dp_error_write           = 1
                                                       unknown_dp_error         = 1
                                                       access_denied            = 6
                                                       dp_out_of_memory         = 7
                                                       disk_full                = 8
                                                       dp_timeout               = 9
                                                       file_not_found           = 10
                                                       dataprovider_exception   = 1
                                                       control_flush_error      = 1
                                                       not_supported_by_gui     = 11
                                                       error_no_gui             = 12
                                                       OTHERS                   = 1 ).
    IF sy-subrc = 0.
      RETURN.
    ENDIF.
  ELSE.
    l_file_size = xstrlen( data_bin ).

    WHILE l_offset < l_file_size.
      l_data_bin = data_bin+l_offset.
      l_offset   = l_offset + lc_line_size_bin.

      INSERT l_data_bin INTO TABLE lt_data_bin.
    ENDWHILE.

    cl_gui_frontend_services=>gui_download( EXPORTING  filename                = full_name
                                                       filetype                = 'BIN'
                                                       bin_filesize            = l_file_size
                                                       append                  = l_append
                                            CHANGING   data_tab                = lt_data_bin
                                            EXCEPTIONS file_write_error        = 1
                                                       no_batch                = 2
                                                       gui_refuse_filetransfer = 3
                                                       invalid_type            = 4
                                                       no_authority            = 5
                                                       unknown_error           = 1
                                                       header_not_allowed      = 4
                                                       separator_not_allowed   = 4
                                                       filesize_not_allowed    = 4
                                                       header_too_long         = 4
                                                       dp_error_create         = 1
                                                       dp_error_send           = 1
                                                       dp_error_write          = 1
                                                       unknown_dp_error        = 1
                                                       access_denied           = 6
                                                       dp_out_of_memory        = 7
                                                       disk_full               = 8
                                                       dp_timeout              = 9
                                                       file_not_found          = 10
                                                       dataprovider_exception  = 1
                                                       control_flush_error     = 1
                                                       not_supported_by_gui    = 11
                                                       error_no_gui            = 12
                                                       OTHERS                  = 1 ).
    IF sy-subrc = 0.
      RETURN.
    ENDIF.
  ENDIF.

  CASE sy-subrc.

    WHEN 1.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_write_file
          var1   = full_name.

    WHEN 2.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file01
          var1   = full_name.

    WHEN 3.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file02
          var1   = full_name.

    WHEN 4.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>incorrect_file_type
          var1   = full_name.

    WHEN 5.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file03
          var1   = full_name.

    WHEN 6.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file04
          var1   = full_name.

    WHEN 7.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file05
          var1   = full_name.

    WHEN 8.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file06
          var1   = full_name.

    WHEN 9.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file07
          var1   = full_name.

    WHEN 10.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>file_does_not_exist
          var1   = full_name.

    WHEN 11.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file08
          var1   = full_name.

    WHEN 12.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>cannot_access_local_file09
          var1   = full_name.

  ENDCASE.
ENDMETHOD.
ENDCLASS.
