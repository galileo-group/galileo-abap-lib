class /GAL/MEM_BUFFERED_FILE_STREAM definition
  public
  inheriting from /GAL/STREAM
  final
  create private

  global friends /GAL/FILE .

*"* public components of class /GAL/MEM_BUFFERED_FILE_STREAM
*"* do not include other source files here!!!
public section.

  methods CLOSE
    redefinition .
  methods FLUSH
    redefinition .
  methods READ_BYTES
    redefinition .
  methods READ_CHARS
    redefinition .
  methods READ_LINE
    redefinition .
  methods SET_LENGTH
    redefinition .
  methods WRITE_BYTES
    redefinition .
  methods WRITE_CHARS
    redefinition .
protected section.
*"* protected components of class /GAL/MEM_BUFFERED_FILE_STREAM
*"* do not include other source files here!!!
private section.
*"* private components of class /GAL/MEM_BUFFERED_FILE_STREAM
*"* do not include other source files here!!!

  data DATA_BIN type XSTRING .
  data DATA_TXT type STRING .
  data UNBUFFERED_LENGTH type I .

  methods CONSTRUCTOR
    importing
      !FILE type ref to /GAL/FILE
      !ACCESS type /GAL/FILE_ACCESS default ACCESS_READ
      !MODE type /GAL/FILE_MODE default MODE_TEXT
      !OPTIONS type /GAL/FILE_OPTIONS default OPTIONS_CREATE
      !LINE_BREAK type /GAL/FILE_LINE_BREAK default LINE_BREAK_AUTO
      !POSITION type I default -1
    raising
      /GAL/CX_IO_EXCEPTION .
ENDCLASS.



CLASS /GAL/MEM_BUFFERED_FILE_STREAM IMPLEMENTATION.


METHOD close.
  super->close( ).

  CLEAR file.
ENDMETHOD.


METHOD constructor.
  DATA l_options_append TYPE /gal/file_options.

  DATA l_exception      TYPE REF TO /gal/cx_io_exception.

* Call parent implementation
  super->constructor( EXPORTING file       = file
                                access     = access
                                mode       = mode
                                options    = options
                                line_break = line_break ).

  TRY.

* Read existing file into buffer (if required)
      IF can_read = abap_true.
        file->read( EXPORTING mode       = mode
                              line_break = me->line_break
                    IMPORTING data_bin   = data_bin
                              data_txt   = data_txt ).
      ENDIF.

* Analyze file options
      l_options_append = options BIT-AND options_append_only.

* Get length that is not present in buffer
      IF can_read = abap_false AND l_options_append > 0.
        unbuffered_length = file->get_length( ).
      ELSE.
        unbuffered_length = 0.
      ENDIF.

* Determine actual file length
      IF mode = mode_binary.
        length = xstrlen( data_bin ) + unbuffered_length.
      ELSEIF mode = mode_text.
        length = strlen( data_txt ) + unbuffered_length.
      ENDIF.

* Seeking is only supported when reading or using random access
      can_seek = can_read.

* Set initial position
      IF position < 0.
        IF l_options_append > 0.
          me->position = length.
        ELSE.
          me->position = 0.
        ENDIF.
      ELSE.
        me->position = position.
      ENDIF.

      update( ).

    CATCH /gal/cx_io_exception INTO l_exception.
      file->unregister_stream( me ).
      RAISE EXCEPTION l_exception.

  ENDTRY.
ENDMETHOD.


METHOD flush.
  DATA l_options TYPE /gal/file_options.

* Flush does not make any sense for read only streams
  IF can_write <> abap_true.
    RETURN.
  ENDIF.

* Call parent implementation
  super->flush( ).

* If seeking is disabled there is no way to go back
* to a previous position. In this case data can be
* appended to file and removed from buffer. In all
* other cases the file needs to be re-written.
  IF can_seek = abap_false.
    IF unbuffered_length = 0.
      l_options = options BIT-AND BIT-NOT options_append_only.
    ELSE.
      l_options = options BIT-OR options_append_only.
    ENDIF.

    file->write( mode       = mode
                 options    = l_options
                 line_break = line_break
                 data_bin   = data_bin
                 data_txt   = data_txt ).

    unbuffered_length = length.

    CLEAR data_bin.
    CLEAR data_txt.
  ELSE.
    l_options = options BIT-AND BIT-NOT options_append_only.

    file->write( mode       = mode
                 options    = l_options
                 line_break = line_break
                 data_bin   = data_bin
                 data_txt   = data_txt ).
  ENDIF.
ENDMETHOD.


METHOD read_bytes.
  DATA l_length TYPE i.
  DATA l_offset TYPE i.

  CALL METHOD super->read_bytes
    EXPORTING
      count = count
    IMPORTING
      bytes = bytes.

  l_length = length - position.

  IF count < l_length.
    l_length = count.
  ENDIF.

  l_offset = position - unbuffered_length.
  bytes    = data_bin+l_offset(l_length).
  position = position + l_length.

  update( ).
ENDMETHOD.


METHOD read_chars.
  DATA l_length TYPE i.
  data l_offset TYPE i.

  CALL METHOD super->read_chars
    EXPORTING
      count = count
    IMPORTING
      chars = chars.

  l_length = length - position.

  IF count < l_length.
    l_length = count.
  ENDIF.

  l_offset = position - unbuffered_length.
  chars    = data_txt+l_offset(l_length).
  position = position + l_length.

  update( ).
ENDMETHOD.


METHOD read_line.
  DATA l_length           TYPE i.
  DATA l_offset           TYPE i.
  DATA l_linebreak_length TYPE i.

  CALL METHOD super->read_line
    IMPORTING
      line = line.

  l_offset = position - unbuffered_length.

  CASE line_break.

    WHEN line_break_unix.
      IF data_txt+l_offset CA /gal/string=>line_break_unix.
        l_length           = sy-fdpos.
        l_linebreak_length = 1.
      ELSE.
        l_length = length - position.
        l_linebreak_length = 0.
      ENDIF.

    WHEN line_break_windows.
      IF data_txt+l_offset CA /gal/string=>line_break_windows.
        l_length           = sy-fdpos.
        l_linebreak_length = 1.
      ELSE.
        l_length = length - position.
        l_linebreak_length = 0.
      ENDIF.

    WHEN OTHERS.
      IF data_txt+l_offset CA /gal/string=>line_break_unix.
        IF sy-fdpos > 0.
          l_offset = l_offset + sy-fdpos - 1.

          IF data_txt+l_offset(2) = /gal/string=>line_break_windows.
            l_length           = sy-fdpos - 1.
            l_linebreak_length = 2.
          ELSE.
            l_length           = sy-fdpos.
            l_linebreak_length = 1.
          ENDIF.
        ELSE.
          l_length           = 0.
          l_linebreak_length = 1.
        ENDIF.
      ELSE.
        l_length = length - position.
        l_linebreak_length = 0.
      ENDIF.

  ENDCASE.

  l_offset = position - unbuffered_length.
  line     = data_txt+l_offset(l_length).

  position = position + l_length + l_linebreak_length.

  update( ).
ENDMETHOD.


METHOD set_length.
  DATA l_old_length TYPE i.
  DATA l_offset     TYPE i.

* Length must be within buffered area
  IF length < unbuffered_length.
    RAISE EXCEPTION TYPE /gal/cx_io_exception
          EXPORTING textid = /gal/cx_io_exception=>invalid_stream_length2.
  ENDIF.

* Remember old length
  l_old_length = me->length.

* Call parent implementation
  super->set_length( length ).

* Adjust length an create missing buffer space (if necessary)
  IF length < l_old_length.
    CASE mode.

      WHEN mode_binary.
        data_bin = data_bin(length).

      WHEN mode_text.
        data_txt = data_txt(length).

    ENDCASE.
  ELSEIF length > l_old_length.
    l_offset = length - l_old_length.

    CASE mode.

      WHEN mode_binary.
        SHIFT data_bin RIGHT BY l_offset PLACES IN BYTE MODE.
        SHIFT data_bin LEFT BY l_offset PLACES IN BYTE MODE CIRCULAR.

      WHEN mode_text.
        SHIFT data_txt RIGHT BY l_offset PLACES.
        SHIFT data_txt LEFT BY l_offset PLACES CIRCULAR.

    ENDCASE.
  ENDIF.
ENDMETHOD.


METHOD write_bytes.
  DATA l_data_length TYPE i.
  DATA l_old_offset  TYPE i.
  DATA l_new_offset  TYPE i.
  DATA l_max_offset  TYPE i.

  CALL METHOD super->write_bytes
    EXPORTING
      bytes = bytes.

  l_data_length = XSTRLEN( bytes ).
  l_old_offset  = position - unbuffered_length.
  l_new_offset  = l_old_offset + l_data_length.
  l_max_offset  = length - unbuffered_length.

  IF access <> access_random OR l_new_offset > l_max_offset.
    CONCATENATE data_bin(l_old_offset) bytes
           INTO data_bin IN BYTE MODE.

    length = l_new_offset + unbuffered_length.
  ELSE.
    CONCATENATE data_bin(l_old_offset) bytes data_bin+l_new_offset
           INTO data_bin IN BYTE MODE.
  ENDIF.

  position = l_new_offset + unbuffered_length.

  update( ).
ENDMETHOD.


METHOD write_chars.
  DATA l_data_length TYPE i.
  DATA l_old_offset  TYPE i.
  DATA l_new_offset  TYPE i.
  DATA l_max_offset  TYPE i.

  super->write_chars( EXPORTING chars = chars ).

  l_data_length = STRLEN( chars ).
  l_old_offset  = position - unbuffered_length.
  l_new_offset  = l_old_offset + l_data_length.
  l_max_offset  = length - unbuffered_length.

  IF access <> access_random OR l_new_offset > l_max_offset.
    CONCATENATE data_txt(l_old_offset) chars
           INTO data_txt IN CHARACTER MODE RESPECTING BLANKS.

    length = l_new_offset + unbuffered_length.
  ELSE.
    CONCATENATE data_txt(l_old_offset) chars data_txt+l_new_offset
           INTO data_txt IN CHARACTER MODE RESPECTING BLANKS.
  ENDIF.

  position = l_new_offset + unbuffered_length.

  update( ).
ENDMETHOD.
ENDCLASS.
