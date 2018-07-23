class /GAL/STREAM definition
  public
  abstract
  create public .

*"* public components of class /GAL/STREAM
*"* do not include other source files here!!!
public section.

  constants ACCESS_RANDOM type /GAL/FILE_ACCESS value /GAL/FILE=>ACCESS_RANDOM. "#EC NOTEXT
  constants ACCESS_READ type /GAL/FILE_ACCESS value /GAL/FILE=>ACCESS_READ. "#EC NOTEXT
  constants ACCESS_WRITE type /GAL/FILE_ACCESS value /GAL/FILE=>ACCESS_WRITE. "#EC NOTEXT
  constants LINE_BREAK_AUTO type /GAL/FILE_LINE_BREAK value /GAL/FILE=>LINE_BREAK_AUTO. "#EC NOTEXT
  constants LINE_BREAK_UNDEFINED type /GAL/FILE_LINE_BREAK value /GAL/FILE=>LINE_BREAK_UNDEFINED. "#EC NOTEXT
  constants LINE_BREAK_UNIX type /GAL/FILE_LINE_BREAK value /GAL/FILE=>LINE_BREAK_UNIX. "#EC NOTEXT
  constants LINE_BREAK_WINDOWS type /GAL/FILE_LINE_BREAK value /GAL/FILE=>LINE_BREAK_WINDOWS. "#EC NOTEXT
  constants MODE_BINARY type /GAL/FILE_MODE value /GAL/FILE=>MODE_BINARY. "#EC NOTEXT
  constants MODE_TEXT type /GAL/FILE_MODE value /GAL/FILE=>MODE_TEXT. "#EC NOTEXT
  constants OPTIONS_APPEND type /GAL/FILE_OPTIONS value /GAL/FILE=>OPTIONS_APPEND. "#EC NOTEXT
  constants OPTIONS_APPEND_ONLY type /GAL/FILE_OPTIONS value /GAL/FILE=>OPTIONS_APPEND_ONLY. "#EC NOTEXT
  constants OPTIONS_CREATE type /GAL/FILE_OPTIONS value /GAL/FILE=>OPTIONS_CREATE. "#EC NOTEXT
  constants OPTIONS_CREATE_ONLY type /GAL/FILE_OPTIONS value /GAL/FILE=>OPTIONS_CREATE_ONLY. "#EC NOTEXT
  constants OPTIONS_RECREATE type /GAL/FILE_OPTIONS value /GAL/FILE=>OPTIONS_RECREATE. "#EC NOTEXT
  constants OPTIONS_RECREATE_ONLY type /GAL/FILE_OPTIONS value /GAL/FILE=>OPTIONS_RECREATE_ONLY. "#EC NOTEXT
  constants OPTIONS_TRUNCATE type /GAL/FILE_OPTIONS value /GAL/FILE=>OPTIONS_TRUNCATE. "#EC NOTEXT
  data ACCESS type /GAL/FILE_ACCESS read-only .
  data CAN_READ type ABAP_BOOL read-only .
  data CAN_SEEK type ABAP_BOOL read-only .
  data CAN_WRITE type ABAP_BOOL read-only .
  data FILE type ref to /GAL/FILE read-only .
  data IS_CLOSED type ABAP_BOOL read-only .
  data IS_EOF type ABAP_BOOL read-only .
  data LENGTH type I read-only .
  data LINE_BREAK type /GAL/FILE_LINE_BREAK read-only .
  data MODE type /GAL/FILE_MODE read-only .
  data OPTIONS type /GAL/FILE_OPTIONS read-only .
  data POSITION type I read-only .

  methods CLOSE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !FILE type ref to /GAL/FILE
      !ACCESS type /GAL/FILE_ACCESS default ACCESS_READ
      !MODE type /GAL/FILE_MODE default MODE_TEXT
      !OPTIONS type /GAL/FILE_OPTIONS default OPTIONS_CREATE
      !LINE_BREAK type /GAL/FILE_LINE_BREAK default LINE_BREAK_AUTO .
  methods COPY_TO
    importing
      !TARGET_STREAM type ref to /GAL/STREAM
    raising
      /GAL/CX_IO_EXCEPTION .
  methods FLUSH
    raising
      /GAL/CX_IO_EXCEPTION .
  methods READ_ALL_BYTES
    exporting
      !BYTES type XSEQUENCE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods READ_ALL_BYTES_TO_XSTRING
    returning
      value(BYTES) type XSTRING
    raising
      /GAL/CX_IO_EXCEPTION .
  methods READ_ALL_CHARS
    exporting
      !CHARS type CSEQUENCE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods READ_ALL_CHARS_TO_STRING
    returning
      value(CHARS) type STRING
    raising
      /GAL/CX_IO_EXCEPTION .
  methods READ_ALL_LINES
    exporting
      !LINES type /GAL/STRINGTABLE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods READ_BYTE
    exporting
      !BYTE type XSEQUENCE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods READ_BYTES
    importing
      !COUNT type I
    exporting
      !BYTES type XSEQUENCE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods READ_BYTES_TO_END
    exporting
      !BYTES type XSEQUENCE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods READ_CHAR
    exporting
      !CHAR type CSEQUENCE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods READ_CHARS
    importing
      !COUNT type I
    exporting
      !CHARS type CSEQUENCE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods READ_CHARS_TO_END
    exporting
      !CHARS type CSEQUENCE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods READ_LINE
    exporting
      !LINE type CLIKE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods READ_LINES
    changing
      !LINES type ANY TABLE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods SEEK
    importing
      !POSITION type I
    raising
      /GAL/CX_IO_EXCEPTION .
  methods SET_LENGTH
    importing
      !LENGTH type I
    raising
      /GAL/CX_IO_EXCEPTION .
  methods WRITE_BYTE
    importing
      !BYTE type XSEQUENCE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods WRITE_BYTES
    importing
      !BYTES type XSEQUENCE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods WRITE_CHAR
    importing
      !CHAR type CSEQUENCE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods WRITE_CHARS
    importing
      !CHARS type STRING
    raising
      /GAL/CX_IO_EXCEPTION .
  methods WRITE_LINE
    importing
      !LINE type CLIKE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods WRITE_LINES
    importing
      !LINES type ANY TABLE
    raising
      /GAL/CX_IO_EXCEPTION .
protected section.
*"* protected components of class /GAL/STREAM
*"* do not include other source files here!!!

  methods UPDATE .
private section.
*"* private components of class /GAL/STREAM
*"* do not include other source files here!!!
ENDCLASS.



CLASS /GAL/STREAM IMPLEMENTATION.


METHOD close.
  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>stream_already_closed.
  ENDIF.

  flush( ).

  is_closed = abap_true.

  file->unregister_stream( me ).
ENDMETHOD.


METHOD constructor.
  me->file       = file.
  me->access     = access.
  me->mode       = mode.
  me->options    = options.
  me->line_break = line_break.

* Set attributes
  IF access O access_read.
    can_read = abap_true.
  ELSE.
    can_read = abap_false.
  ENDIF.

  IF access O access_write.
    can_write = abap_true.
  ELSE.
    can_write = abap_false.
  ENDIF.

  is_closed = abap_false.

  file->register_stream( me ).
ENDMETHOD.


METHOD copy_to.
  DATA l_data_bin TYPE xstring.
  DATA l_data_txt TYPE string.

  IF mode <> target_stream->mode.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>stream_types_do_not_match.
  ENDIF.

  CASE mode.

    WHEN mode_binary.
      read_all_bytes( IMPORTING bytes = l_data_bin ).
      target_stream->write_bytes( l_data_bin ).

    WHEN mode_text.
      read_all_chars( IMPORTING chars = l_data_txt ).
      target_stream->write_chars( l_data_txt ).

  ENDCASE.
ENDMETHOD.


METHOD flush.
  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>stream_already_closed.
  ENDIF.
ENDMETHOD.


METHOD read_all_bytes.
  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_read <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING textid = /gal/cx_io_exception=>method_not_supported_by_stream
                var1   = `READ_ALL_BYTES`.
  ELSEIF mode <> mode_binary.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>requires_binary_mode_stream
                    var1   = `READ_ALL_BYTES`.
  ENDIF.

  seek( 0 ).

  CALL METHOD read_bytes
    EXPORTING
      count = length
    IMPORTING
      bytes = bytes.
ENDMETHOD.


METHOD read_all_bytes_to_xstring.
  read_all_bytes(
    IMPORTING
      bytes = bytes
  ).
ENDMETHOD.


METHOD read_all_chars.
  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_read <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING textid = /gal/cx_io_exception=>method_not_supported_by_stream
                var1   = `READ_ALL_CHARS`.
  ELSEIF mode <> mode_text.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>requires_text_mode_stream
                    var1   = `READ_ALL_CHARS`.
  ENDIF.

  seek( 0 ).

  read_chars( EXPORTING count = length
              IMPORTING chars = chars ).
ENDMETHOD.


METHOD read_all_chars_to_string.
  read_all_chars(
    IMPORTING
      chars = chars
  ).
ENDMETHOD.


METHOD read_all_lines.
  DATA l_line TYPE string.

  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_read <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING textid = /gal/cx_io_exception=>method_not_supported_by_stream
                var1   = `READ_ALL_LINES`.
  ELSEIF mode <> mode_text.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>requires_text_mode_stream
                    var1   = `READ_ALL_LINES`.
  ENDIF.

  seek( 0 ).

  CLEAR lines.

  WHILE NOT is_eof = abap_true.
    read_line( IMPORTING line = l_line ).
    INSERT l_line INTO TABLE lines.
  ENDWHILE.
ENDMETHOD.


METHOD read_byte.
  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_read <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING textid = /gal/cx_io_exception=>method_not_supported_by_stream
                var1   = `READ_BYTE`.
  ELSEIF mode <> mode_binary.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>requires_binary_mode_stream
                    var1   = `READ_BYTE`.
  ENDIF.

  CALL METHOD read_bytes
    EXPORTING
      count = 1
    IMPORTING
      bytes = byte.
ENDMETHOD.


METHOD read_bytes.
  CLEAR bytes.

  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_read <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>method_not_supported_by_stream
        var1   = `READ_BYTES`.
  ELSEIF mode <> mode_binary.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>requires_binary_mode_stream
        var1   = `READ_BYTES`.
  ENDIF.
ENDMETHOD.


METHOD read_bytes_to_end.
  DATA l_count TYPE i.

  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_read <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING textid = /gal/cx_io_exception=>method_not_supported_by_stream
                var1   = `READ_BYTES_TO_END`.
  ELSEIF mode <> mode_binary.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>requires_binary_mode_stream
                    var1   = `READ_BYTES_TO_END`.
  ENDIF.

  l_count = length - position.

  CALL METHOD read_bytes
    EXPORTING
      count = l_count
    IMPORTING
      bytes = bytes.
ENDMETHOD.


METHOD read_char.
  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_read <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING textid = /gal/cx_io_exception=>method_not_supported_by_stream
                var1   = `READ_CHAR`.
  ELSEIF mode <> mode_text.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>requires_text_mode_stream
                    var1   = `READ_CHAR`.
  ENDIF.

  CALL METHOD read_chars
    EXPORTING
      count = 1
    IMPORTING
      chars = char.
ENDMETHOD.


METHOD read_chars.
  CLEAR chars.

  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_read <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>method_not_supported_by_stream
        var1   = `READ_CHARS`.
  ELSEIF mode <> mode_text.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>requires_text_mode_stream
        var1   = `READ_CHARS`.
  ENDIF.
ENDMETHOD.


METHOD read_chars_to_end.
  DATA l_count TYPE i.

  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_read <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING textid = /gal/cx_io_exception=>method_not_supported_by_stream
                var1   = `READ_CHARS_TO_END`.
  ELSEIF mode <> mode_text.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>requires_text_mode_stream
                    var1   = `READ_CHARS_TO_END`.
  ENDIF.

  l_count = length - position.

  CALL METHOD read_chars
    EXPORTING
      count = l_count
    IMPORTING
      chars = chars.
ENDMETHOD.


METHOD read_line.
  CLEAR line.

  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_read <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>method_not_supported_by_stream
        var1   = `READ_LINE`.
  ELSEIF mode <> mode_text.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>requires_text_mode_stream
        var1   = `READ_LINE`.
  ENDIF.
ENDMETHOD.


METHOD read_lines.
  DATA l_line TYPE REF TO data.

  FIELD-SYMBOLS <l_line> TYPE any.

* Initialize result
  CLEAR lines.

* Check stream status and mode
  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_read <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>method_not_supported_by_stream
        var1   = `READ_LINES`.
  ELSEIF mode <> mode_text.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>requires_text_mode_stream
        var1   = `READ_LINES`.
  ENDIF.

* Read lines until end of file
  WHILE NOT is_eof = abap_true.
    CREATE DATA l_line LIKE LINE OF lines.
    ASSIGN l_line->* TO <l_line>.

    read_line( IMPORTING line = <l_line> ).

    INSERT <l_line> INTO TABLE lines.
  ENDWHILE.
ENDMETHOD.


METHOD seek.
  DATA l_position_string TYPE string.

  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_seek <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING textid = /gal/cx_io_exception=>method_not_supported_by_stream
                var1   = `SEEK`.
  ENDIF.

  IF position < 0 OR position > length.
    l_position_string = position.

    RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING textid = /gal/cx_io_exception=>invalid_seek_position
                  var1   = l_position_string.
  ENDIF.

  me->position = position.

  IF position = length.
    is_eof = abap_true.
  ELSE.
    is_eof = abap_false.
  ENDIF.
ENDMETHOD.


METHOD set_length.
  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_write <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>method_not_supported_by_stream
                    var1   = `SET_LENGTH`.
  ENDIF.

  IF length < 0.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>invalid_stream_length.
  ENDIF.

  me->length = length.

  update( ).
ENDMETHOD.


METHOD update.

* Update position
  IF position < 0.
    position = 0.
  ELSEIF position > length.
    position = length.
  ENDIF.

* Update EOF flag
  IF position = length.
    is_eof = abap_true.
  ELSE.
    is_eof = abap_false.
  ENDIF.
ENDMETHOD.


METHOD write_byte.
  DATA l_length TYPE i.

  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_write <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING textid = /gal/cx_io_exception=>method_not_supported_by_stream
                var1   = `WRITE_BYTE`.
  ELSEIF mode <> mode_binary.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>requires_binary_mode_stream
                    var1   = `WRITE_BYTE`.
  ENDIF.

  DESCRIBE FIELD byte LENGTH l_length IN BYTE MODE.

  IF l_length <> 1.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>write_byte_argument_mismatch.
  ENDIF.

  write_bytes( bytes = byte ).
ENDMETHOD.


METHOD write_bytes.
  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_write <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING textid = /gal/cx_io_exception=>method_not_supported_by_stream
                var1   = `WRITE_BYTES`.
  ELSEIF mode <> mode_binary.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>requires_binary_mode_stream
                    var1   = `WRITE_BYTES`.
  ENDIF.
ENDMETHOD.


METHOD write_char.
  DATA l_length TYPE i.
  DATA l_type   TYPE c.

  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_write <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING textid = /gal/cx_io_exception=>method_not_supported_by_stream
                var1   = `WRITE_CHAR`.
  ELSEIF mode <> mode_text.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>requires_text_mode_stream
                    var1   = `WRITE_CHAR`.
  ENDIF.

  DESCRIBE FIELD char TYPE l_type.

  IF l_type = 'g'.
    l_length = STRLEN( char ).
  ELSE.
    DESCRIBE FIELD char LENGTH l_length IN CHARACTER MODE.
  ENDIF.

  IF l_length <> 1.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>write_char_argument_mismatch.
  ENDIF.

  IF l_type = 'C' AND char = space.
    write_chars( chars = ` ` ).
  ELSE.
    write_chars( chars = char ).
  ENDIF.
ENDMETHOD.


METHOD write_chars.
  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_write <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING textid = /gal/cx_io_exception=>method_not_supported_by_stream
                var1   = `WRITE_CHARS`.
  ELSEIF mode <> mode_text.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>requires_text_mode_stream
                    var1   = `WRITE_CHARS`.
  ENDIF.
ENDMETHOD.


METHOD write_line.
  DATA l_line TYPE string.

  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_write <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING textid = /gal/cx_io_exception=>method_not_supported_by_stream
                var1   = `WRITE_LINE`.
  ELSEIF mode <> mode_text.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>requires_text_mode_stream
                    var1   = `WRITE_LINE`.
  ENDIF.

  CASE line_break.

    WHEN line_break_unix.
      CONCATENATE line /gal/string=>line_break_unix
             INTO l_line RESPECTING BLANKS.

    WHEN line_break_windows.
      CONCATENATE line /gal/string=>line_break_windows
             INTO l_line RESPECTING BLANKS.

    WHEN OTHERS.
      CONCATENATE line /gal/string=>line_break_unix
             INTO l_line RESPECTING BLANKS.

  ENDCASE.

  write_chars( chars = l_line ).
ENDMETHOD.


METHOD write_lines.
  FIELD-SYMBOLS <l_line> TYPE any.

* Check stream status and mode
  IF is_closed = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>stream_already_closed.
  ELSEIF can_write <> abap_true.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>method_not_supported_by_stream
        var1   = `WRITE_LINES`.
  ELSEIF mode <> mode_text.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
      EXPORTING
        textid = /gal/cx_io_exception=>requires_text_mode_stream
        var1   = `WRITE_LINES`.
  ENDIF.

* Write all lines to stream
  LOOP AT lines ASSIGNING <l_line>.
    write_line( line = <l_line> ).
  ENDLOOP.
ENDMETHOD.
ENDCLASS.
