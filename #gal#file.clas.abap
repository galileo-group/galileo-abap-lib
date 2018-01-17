class /GAL/FILE definition
  public
  abstract
  create public

  global friends /GAL/MEM_BUFFERED_FILE_STREAM
                 /GAL/STREAM .

*"* public components of class /GAL/FILE
*"* do not include other source files here!!!
public section.

  constants ACCESS_RANDOM type /GAL/FILE_ACCESS value '03'. "#EC NOTEXT
  constants ACCESS_READ type /GAL/FILE_ACCESS value '01'. "#EC NOTEXT
  constants ACCESS_WRITE type /GAL/FILE_ACCESS value '02'. "#EC NOTEXT
  constants LINE_BREAK_AUTO type /GAL/FILE_LINE_BREAK value '01'. "#EC NOTEXT
  constants LINE_BREAK_UNDEFINED type /GAL/FILE_LINE_BREAK value '00'. "#EC NOTEXT
  constants LINE_BREAK_UNIX type /GAL/FILE_LINE_BREAK value '02'. "#EC NOTEXT
  constants LINE_BREAK_WINDOWS type /GAL/FILE_LINE_BREAK value '03'. "#EC NOTEXT
  constants MODE_BINARY type /GAL/FILE_MODE value '01'. "#EC NOTEXT
  constants MODE_TEXT type /GAL/FILE_MODE value '02'. "#EC NOTEXT
  constants OPTIONS_APPEND type /GAL/FILE_OPTIONS value '05'. "#EC NOTEXT
  constants OPTIONS_APPEND_ONLY type /GAL/FILE_OPTIONS value '04'. "#EC NOTEXT
  constants OPTIONS_CREATE type /GAL/FILE_OPTIONS value '03'. "#EC NOTEXT
  constants OPTIONS_CREATE_ONLY type /GAL/FILE_OPTIONS value '01'. "#EC NOTEXT
  constants OPTIONS_RECREATE type /GAL/FILE_OPTIONS value '09'. "#EC NOTEXT
  constants OPTIONS_RECREATE_ONLY type /GAL/FILE_OPTIONS value '08'. "#EC NOTEXT
  constants OPTIONS_TRUNCATE type /GAL/FILE_OPTIONS value '02'. "#EC NOTEXT
  data FULL_NAME type STRING read-only .

  methods CONSTRUCTOR
    importing
      !NAME type CSEQUENCE .
  methods COPY
    importing
      !MODE type /GAL/FILE_MODE default MODE_BINARY
      !OPTIONS type /GAL/FILE_OPTIONS default OPTIONS_CREATE
      !SOURCE_LINE_BREAK type /GAL/FILE_LINE_BREAK default LINE_BREAK_AUTO
      !TARGET_LINE_BREAK type /GAL/FILE_LINE_BREAK default LINE_BREAK_AUTO
      !TARGET_FILE type ref to /GAL/FILE
    raising
      /GAL/CX_IO_EXCEPTION .
  methods DELETE
  abstract
    raising
      /GAL/CX_IO_EXCEPTION .
  type-pools ABAP .
  methods EXISTS
  abstract
    returning
      value(FILE_EXISTS) type ABAP_BOOL
    raising
      /GAL/CX_IO_EXCEPTION .
  methods GET_LENGTH
  abstract
    returning
      value(LENGTH) type I
    raising
      /GAL/CX_IO_EXCEPTION .
  methods GET_LINE_BREAK
    returning
      value(LINE_BREAK) type /GAL/FILE_LINE_BREAK
    raising
      /GAL/CX_IO_EXCEPTION .
  methods OPEN
    importing
      !ACCESS type /GAL/FILE_ACCESS default ACCESS_READ
      !MODE type /GAL/FILE_MODE default MODE_TEXT
      !OPTIONS type /GAL/FILE_OPTIONS default OPTIONS_CREATE
      !LINE_BREAK type /GAL/FILE_LINE_BREAK default LINE_BREAK_AUTO
    returning
      value(STREAM) type ref to /GAL/STREAM
    raising
      /GAL/CX_IO_EXCEPTION .
protected section.
*"* protected components of class /GAL/FILE
*"* do not include other source files here!!!

  data OPEN_STREAMS type /GAL/STREAMS .

  methods READ
  abstract
    importing
      !MODE type /GAL/FILE_MODE
      !LINE_BREAK type /GAL/FILE_LINE_BREAK
    exporting
      !DATA_BIN type XSTRING
      !DATA_TXT type STRING
    raising
      /GAL/CX_IO_EXCEPTION .
  methods REGISTER_STREAM
    importing
      !STREAM type ref to /GAL/STREAM .
  methods UNREGISTER_STREAM
    importing
      !STREAM type ref to /GAL/STREAM .
  methods WRITE
  abstract
    importing
      !MODE type /GAL/FILE_MODE
      !OPTIONS type /GAL/FILE_OPTIONS
      !LINE_BREAK type /GAL/FILE_LINE_BREAK
      !DATA_BIN type XSTRING
      !DATA_TXT type STRING
    raising
      /GAL/CX_IO_EXCEPTION .
private section.
*"* private components of class /GAL/FILE
*"* do not include other source files here!!!
ENDCLASS.



CLASS /GAL/FILE IMPLEMENTATION.


METHOD constructor.
  full_name = name.
ENDMETHOD.


METHOD copy.
  DATA l_source_stream     TYPE REF TO /gal/stream.
  DATA l_target_stream     TYPE REF TO /gal/stream.

  DATA l_target_line_break TYPE /gal/file_line_break.

* Open source file
  l_source_stream = open( access     = access_read
                          mode       = mode
                          line_break = source_line_break ).

* Determine line break format to be used for target file
  IF target_line_break <> line_break_unix AND target_line_break <> line_break_windows.
    l_target_line_break = l_source_stream->line_break.
  ELSE.
    l_target_line_break = target_line_break.
  ENDIF.

* Open target file
  l_target_stream = target_file->open( access     = access_write
                                       mode       = mode
                                       options    = options
                                       line_break = l_target_line_break ).

* Copy source file to target stream
  l_source_stream->copy_to( l_target_stream ).

* Close streams
  l_source_stream->close( ).
  l_target_stream->close( ).
ENDMETHOD.


METHOD get_line_break.
  line_break = line_break_undefined.
ENDMETHOD.


METHOD open.
  IF access <> access_read.
    LOOP AT open_streams TRANSPORTING NO FIELDS WHERE table_line->access <> access_read. "#EC CI_STDSEQ
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>file_already_open_for_write
          var1   = full_name.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD register_stream.
  INSERT stream INTO TABLE open_streams.
ENDMETHOD.


METHOD unregister_stream.
  DELETE TABLE open_streams FROM stream.                 "#EC CI_STDSEQ
ENDMETHOD.
ENDCLASS.
