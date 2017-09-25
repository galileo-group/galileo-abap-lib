*----------------------------------------------------------------------*
*       CLASS /GAL/BACKEND_FILE DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class /GAL/BACKEND_FILE definition
  public
  inheriting from /GAL/FILE
  final
  create public .

*"* public components of class /GAL/BACKEND_FILE
*"* do not include other source files here!!!
*"* protected components of class /GAL/BACKEND_FILE
*"* do not include other source files here!!!
public section.

  data RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO read-only .

  class-methods SELECT
    importing
      !RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO
      !ACCESS type /GAL/FILE_ACCESS default ACCESS_READ
      !BASE_DIRECTORY type CSEQUENCE optional
      !DEFAULT_DIRECTORY type CSEQUENCE optional
      !DEFAULT_NAME type CSEQUENCE optional
      !DEFAULT_EXTENSION type CSEQUENCE optional
      !TITLE type CSEQUENCE optional
      !FILTER type CSEQUENCE optional
    returning
      value(FILE) type ref to /GAL/BACKEND_FILE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !NAME type CSEQUENCE
      !RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO optional .

  methods DELETE
    redefinition .
  methods EXISTS
    redefinition .
  methods GET_LENGTH
    redefinition .
  methods GET_LINE_BREAK
    redefinition .
  methods OPEN
    redefinition .
protected section.

  methods READ
    redefinition .
  methods WRITE
    redefinition .
*"* private components of class /GAL/BACKEND_FILE
*"* do not include other source files here!!!
  PRIVATE SECTION.
ENDCLASS.



CLASS /GAL/BACKEND_FILE IMPLEMENTATION.


METHOD constructor.

* Call parent implementation
  super->constructor( name = name ).

* Set RFC route info
  me->rfc_route_info = rfc_route_info.
ENDMETHOD.                    "constructor


  METHOD delete.
    DATA l_message TYPE string.

    CALL FUNCTION '/GAL/FILE_DELETE'
      EXPORTING
        rfc_route_info = rfc_route_info
        full_name      = full_name
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO l_message.

      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>rfc_message
          var1   = l_message.
    ENDIF.
  ENDMETHOD.                    "delete


METHOD exists.
  DATA l_message TYPE string.

  CALL FUNCTION '/GAL/FILE_READ'
    EXPORTING
      rfc_route_info   = rfc_route_info
      full_name        = full_name
      mode             = mode_binary
      length           = 0
    EXCEPTIONS
      cannot_read_file = 1
      OTHERS           = 2.
  IF sy-subrc = 0.
    file_exists = abap_true.
  ELSEIF sy-subrc = 1.
    file_exists = abap_false.
  ELSE.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>rfc_message
        var1   = l_message.
  ENDIF.
ENDMETHOD.


METHOD get_length.
  DATA l_message TYPE string.

  CALL FUNCTION '/GAL/FILE_GET_LENGTH'
    EXPORTING
      rfc_route_info = rfc_route_info
      full_name      = full_name
    IMPORTING
      length         = length
    EXCEPTIONS
      OTHERS         = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>rfc_message
        var1   = l_message.
  ENDIF.
ENDMETHOD.                    "get_length


METHOD get_line_break.
  DATA l_message TYPE string.

  CALL FUNCTION '/GAL/FILE_GET_LINE_BREAK'
    EXPORTING
      rfc_route_info = rfc_route_info
      full_name      = full_name
    IMPORTING
      line_break     = line_break
    EXCEPTIONS
      OTHERS         = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>rfc_message
        var1   = l_message.
  ENDIF.
ENDMETHOD.                    "get_line_break


METHOD open.
  DATA l_line_break LIKE line_break.

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
      IF sy-opsys CS 'WIN'.
        l_line_break = line_break_windows.
      ELSE.
        l_line_break = line_break_unix.
      ENDIF.
    ENDIF.
  ELSE.
    l_line_break = line_break_undefined.
  ENDIF.

  CREATE OBJECT stream TYPE /gal/mem_buffered_file_stream
    EXPORTING
      file       = me
      access     = access
      mode       = mode
      options    = options
      line_break = l_line_break.
ENDMETHOD.                    "open


  METHOD read.
    DATA lt_data_txt TYPE /gal/stringtable.

    DATA l_message   TYPE string.

* Read file
    CALL FUNCTION '/GAL/FILE_READ'
      EXPORTING
        rfc_route_info = rfc_route_info
        full_name      = full_name
        mode           = mode
        line_break     = line_break
      IMPORTING
        data_bin       = data_bin
        data_txt       = lt_data_txt
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO l_message.

      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>rfc_message
          var1   = l_message.
    ENDIF.

    IF line_break = line_break_windows.
      CONCATENATE LINES OF lt_data_txt
             INTO data_txt SEPARATED BY /gal/string=>line_break_windows.
    ELSE.
      CONCATENATE LINES OF lt_data_txt
             INTO data_txt SEPARATED BY /gal/string=>line_break_unix.
    ENDIF.
  ENDMETHOD.                    "read


METHOD select.
  DATA l_base_directory    TYPE string.
  DATA l_default_directory TYPE string.
  DATA l_default_name      TYPE string.
  DATA l_title             TYPE string.
  DATA l_filter            TYPE string.

  DATA l_initial_filter    TYPE string.

  DATA l_filename          TYPE string.
  DATA l_aborted           TYPE abap_bool.
  DATA l_exception         TYPE REF TO cx_root.
  DATA l_io_exception      TYPE REF TO /gal/cx_io_exception.
  DATA l_message           TYPE string.

* Convert parameters to string
  l_base_directory    = base_directory.
  l_default_directory = default_directory.
  l_default_name      = default_name.

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

  IF default_extension IS INITIAL.
    l_initial_filter = '*'.
  ELSE.
    CONCATENATE '*.' default_extension INTO l_initial_filter.
  ENDIF.

  CALL FUNCTION '/GAL/CD_SELECT_BACKEND_FILES'
    EXPORTING
      rfc_route_info    = rfc_route_info
      access            = access
      base_directory    = l_base_directory
      initial_directory = l_default_directory
      initial_file      = l_default_name
      initial_filter    = l_initial_filter
      filter            = l_filter
      title             = l_title
    IMPORTING
      selected_file     = l_filename
      aborted           = l_aborted
      exception         = l_exception.

  IF l_aborted <> abap_false.
    IF l_exception IS INITIAL.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>aborted_by_user.
    ELSE.
      TRY.
          l_io_exception ?= l_exception.

          RAISE EXCEPTION l_io_exception.

        CATCH cx_sy_move_cast_error.
          l_message = l_exception->get_text( ).

          RAISE EXCEPTION TYPE /gal/cx_io_exception
            EXPORTING
              textid = /gal/cx_io_exception=>custom_exception
              var1   = l_message.

      ENDTRY.
    ENDIF.
  ENDIF.

* Create file object
  CREATE OBJECT file TYPE /gal/backend_file
    EXPORTING
      name           = l_filename
      rfc_route_info = rfc_route_info.
ENDMETHOD.                    "SELECT


METHOD write.
  DATA lt_data_txt TYPE /gal/stringtable.

  DATA l_length    TYPE i.
  DATA l_offset    TYPE i.

  DATA l_message   TYPE string.

  FIELD-SYMBOLS <l_line> TYPE string.

* The WHILE loops are necessarey because SPLIT ... INTO TABLE ... loses trainling lines!
  IF mode = mode_text.
    CASE line_break.

      WHEN line_break_unix.
        WHILE data_txt+l_offset CS /gal/string=>line_break_unix.
          IF sy-fdpos = 0.
            INSERT INITIAL LINE INTO TABLE lt_data_txt.
          ELSE.
            INSERT data_txt+l_offset(sy-fdpos) INTO TABLE lt_data_txt.
          ENDIF.

          l_offset = l_offset + sy-fdpos + strlen( /gal/string=>line_break_unix ).
        ENDWHILE.

        INSERT data_txt+l_offset INTO TABLE lt_data_txt.

      WHEN line_break_windows.
        WHILE data_txt+l_offset CS /gal/string=>line_break_windows.
          IF sy-fdpos = 0.
            INSERT INITIAL LINE INTO TABLE lt_data_txt.
          ELSE.
            INSERT data_txt+l_offset(sy-fdpos) INTO TABLE lt_data_txt.
          ENDIF.

          l_offset = l_offset + sy-fdpos + strlen( /gal/string=>line_break_windows ).
        ENDWHILE.

        INSERT data_txt+l_offset INTO TABLE lt_data_txt.

      WHEN OTHERS.
        WHILE data_txt+l_offset CS /gal/string=>line_break_windows.
          IF sy-fdpos = 0.
            INSERT INITIAL LINE INTO TABLE lt_data_txt.
          ELSE.
            INSERT data_txt+l_offset(sy-fdpos) INTO TABLE lt_data_txt.
          ENDIF.

          l_offset = l_offset + sy-fdpos + strlen( /gal/string=>line_break_windows ).
        ENDWHILE.

        INSERT data_txt+l_offset INTO TABLE lt_data_txt.

        LOOP AT lt_data_txt ASSIGNING <l_line>.
          l_length = strlen( <l_line> ).

          IF l_length > 0.
            l_offset = l_length - 1.

            IF <l_line>+l_offset = /gal/string=>line_break_windows(1).
              <l_line> = <l_line>(l_offset).
            ENDIF.
          ENDIF.
        ENDLOOP.

    ENDCASE.
  ENDIF.

  CALL FUNCTION '/GAL/FILE_WRITE'
    EXPORTING
      rfc_route_info = rfc_route_info
      full_name      = full_name
      mode           = mode
      options        = options
      line_break     = line_break
      data_bin       = data_bin
      data_txt       = lt_data_txt
    EXCEPTIONS
      OTHERS         = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>rfc_message
        var1   = l_message.
  ENDIF.
ENDMETHOD.                    "write
ENDCLASS.
