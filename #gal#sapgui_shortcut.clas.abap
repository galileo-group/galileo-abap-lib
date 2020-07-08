"! <p class="shorttext synchronized" lang="en">SAPgui Shortcut</p>
class /GAL/SAPGUI_SHORTCUT definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  "! <p class="shorttext synchronized" lang="en">SAPgui shortcut type: Report</p>
  constants SHORTCUT_TYPE_REPORT type I value 2. "#EC NOTEXT
  "! <p class="shorttext synchronized" lang="en">SAPgui shortcut type: System Command</p>
  constants SHORTCUT_TYPE_SYSTEM_COMMAND type I value 3. "#EC NOTEXT
  "! <p class="shorttext synchronized" lang="en">SAPgui shortcut type: Transaction</p>
  constants SHORTCUT_TYPE_TRANSACTION type I value 1. "#EC NOTEXT
  "! <p class="shorttext synchronized" lang="en">SAPgui shortcut type: Undefined</p>
  constants SHORTCUT_TYPE_UNDEFINED type I value 0. "#EC NOTEXT
  "! <p class="shorttext synchronized" lang="en">Client</p>
  data CLIENT type MANDT read-only .
  "! <p class="shorttext synchronized" lang="en">Do not re-use existing session</p>
  data FORCE_LOGIN type ABAP_BOOL read-only .
  "! <p class="shorttext synchronized" lang="en">Language Key</p>
  data LANGUAGE type LANGU read-only .
  "! <p class="shorttext synchronized" lang="en">Parameters</p>
  data PARAMETERS type STRING read-only .
  "! <p class="shorttext synchronized" lang="en">Program Name (type Report only)</p>
  data PROGRAM_NAME type PROGNAME read-only .
  "! <p class="shorttext synchronized" lang="en">System description used in SAPlogon</p>
  data SAPLOGON_SYSTEM_DESCRIPTION type STRING read-only .
  "! <p class="shorttext synchronized" lang="en">System Command (type System Command)</p>
  data SYSTEM_COMMAND type STRING read-only .
  "! <p class="shorttext synchronized" lang="en">System ID</p>
  data SYSTEM_ID type SYSYSID read-only .
  "! <p class="shorttext synchronized" lang="en">Title</p>
  data TITLE type STRING read-only .
  "! <p class="shorttext synchronized" lang="en">Transaction Code (type Transaction only)</p>
  data TRANSACTION_CODE type TCODE read-only .
  "! <p class="shorttext synchronized" lang="en">SAPgui shortcut type</p>
  data TYPE type I read-only value SHORTCUT_TYPE_UNDEFINED. "#EC NOTEXT
  "! <p class="shorttext synchronized" lang="en">User Name</p>
  data USER type SYUNAME read-only .
  "! <p class="shorttext synchronized" lang="en">Skip first screen for transaction</p>
  data SKIP_FIRST_SCREEN type ABAP_BOOL read-only .
"! <p class="shorttext synchronized" lang="en">Include SNC information</p>
  data WITH_SNC_INFO type ABAP_BOOL read-only.
"! <p class="shorttext synchronized" lang="en">SNC name of target system</p>
  data SYS_SNC_NAME type STRING read-only.
  "! <p class="shorttext synchronized" lang="en">SNC quality of protection</p>
  data SYS_SNC_QOP type CHAR1 read-only.

  "! <p class="shorttext synchronized" lang="en">Constructor</p>
  "!
  "! @parameter transaction_code               | <p class="shorttext synchronized" lang="en">Transaction code</p>
  "! @parameter program_name                   | <p class="shorttext synchronized" lang="en">Program name</p>
  "! @parameter system_command                 | <p class="shorttext synchronized" lang="en">System Command</p>
  "! @parameter system_id                      | <p class="shorttext synchronized" lang="en">System ID</p>
  "! @parameter saplogon_system_description    | <p class="shorttext synchronized" lang="en">System description used in SAPlogon</p>
  "! @parameter client                         | <p class="shorttext synchronized" lang="en">Client</p>
  "! @parameter user                           | <p class="shorttext synchronized" lang="en">User</p>
  "! @parameter language                       | <p class="shorttext synchronized" lang="en">Language</p>
  "! @parameter title                          | <p class="shorttext synchronized" lang="en">Title</p>
  "! @parameter force_login                    | <p class="shorttext synchronized" lang="en">Do not re-use existing session</p>
  "! @parameter sys_snc_name                   | <p class="shorttext synchronized" lang="en">SNC name of target system</p>
  "! @parameter sys_snc_qop                    | <p class="shorttext synchronized" lang="en">SNC quality of protection</p>
  "! @raising   /gal/cx_gui_shortcut_exception | <p class="shorttext synchronized" lang="en">SAPgui Shortcut related exception</p>
  methods CONSTRUCTOR
    importing
      !TRANSACTION_CODE type CSEQUENCE optional
      !PROGRAM_NAME type CSEQUENCE optional
      !SYSTEM_COMMAND type CSEQUENCE optional
      !SYSTEM_ID type SYSYSID default SY-SYSID
      !SAPLOGON_SYSTEM_DESCRIPTION type CSEQUENCE optional
      !CLIENT type MANDT default SY-MANDT
      !USER type CSEQUENCE default SY-UNAME
      !LANGUAGE type LANGU default SY-LANGU
      !TITLE type CSEQUENCE optional
      !FORCE_LOGIN type ABAP_BOOL default ABAP_FALSE
      !SYS_SNC_NAME type CSEQUENCE optional
      !SYS_SNC_QOP TYPE CSEQUENCE optional
    raising
      /GAL/CX_GUI_SHORTCUT_EXCEPTION .
  "! <p class="shorttext synchronized" lang="en">Write to temporary frontend file and execute</p>
  "!
  "! @raising   /gal/cx_gui_shortcut_exception | <p class="shorttext synchronized" lang="en">SAPgui Shortcut related exception</p>
  "! @raising   /gal/cx_io_exception           | <p class="shorttext synchronized" lang="en">IO Exception</p>
  methods EXECUTE
    raising
      /GAL/CX_GUI_SHORTCUT_EXCEPTION
      /GAL/CX_IO_EXCEPTION .
  "! <p class="shorttext synchronized" lang="en">Set dynpro field value (type Transaction only)</p>
  "!
  "! @parameter field_name                     | <p class="shorttext synchronized" lang="en">Field name</p>
  "! @parameter field_value                    | <p class="shorttext synchronized" lang="en">Field value</p>
  "! @raising   /gal/cx_gui_shortcut_exception | <p class="shorttext synchronized" lang="en">SAPgui Shortcut related exception</p>
  methods SET_DYNPRO_FIELD_VALUE
    importing
      !FIELD_NAME type CSEQUENCE
      !FIELD_VALUE type ANY
    raising
      /GAL/CX_GUI_SHORTCUT_EXCEPTION .
  "! <p class="shorttext synchronized" lang="en">Write to file</p>
  "!
  "! @parameter file                 | <p class="shorttext synchronized" lang="en">File</p>
  "! @raising   /gal/cx_io_exception | <p class="shorttext synchronized" lang="en">IO Exception</p>
  methods WRITE_TO_FILE
    importing
      !FILE type ref to /GAL/FILE
    raising
      /GAL/CX_IO_EXCEPTION .
  "! <p class="shorttext synchronized" lang="en">Write to frontend file</p>
  "!
  "! @parameter name                 | <p class="shorttext synchronized" lang="en">File name (full path)</p>
  "! @raising   /gal/cx_io_exception | <p class="shorttext synchronized" lang="en">IO Exception</p>
  methods WRITE_TO_FRONTEND_FILE
    importing
      !NAME type CSEQUENCE
    raising
      /GAL/CX_IO_EXCEPTION .
  "! <p class="shorttext synchronized" lang="en">Write to stream</p>
  "!
  "! @parameter stream               | <p class="shorttext synchronized" lang="en">Stream</p>
  "! @raising   /gal/cx_io_exception | <p class="shorttext synchronized" lang="en">IO Exception</p>
  methods WRITE_TO_STREAM
    importing
      !STREAM type ref to /GAL/STREAM
    raising
      /GAL/CX_IO_EXCEPTION .
  "! <p class="shorttext synchronized" lang="en">Write to string</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en">Result</p>
  methods WRITE_TO_STRING
    returning
      value(RESULT) type STRING .
  "! <p class="shorttext synchronized" lang="en">Set indicator to skip first screen (type Transaction only)</p>
  "!
  "! @parameter skip_first_screen              | <p class="shorttext synchronized" lang="en">Skip first screen for transaction</p>
  "! @raising   /gal/cx_gui_shortcut_exception | <p class="shorttext synchronized" lang="en">SAPgui Shortcut related exception</p>
  methods SET_SKIP_FIRST_SCREEN
    importing
      !SKIP_FIRST_SCREEN type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_GUI_SHORTCUT_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS /GAL/SAPGUI_SHORTCUT IMPLEMENTATION.


METHOD constructor.
  DATA l_language_iso(2) TYPE c.

* Determine shortcut type
  IF transaction_code IS NOT INITIAL AND program_name IS INITIAL AND system_command IS INITIAL.
    me->title            = transaction_code.
    me->transaction_code = transaction_code.
    me->type             = shortcut_type_transaction.
  ELSEIF transaction_code IS INITIAL AND program_name IS NOT INITIAL AND system_command IS INITIAL.
    me->title            = program_name.
    me->program_name     = program_name.
    me->type             = shortcut_type_report.
  ELSEIF transaction_code IS INITIAL AND program_name IS INITIAL AND system_command IS NOT INITIAL.
    me->title            = system_command.
    me->system_command   = system_command.
    me->type             = shortcut_type_system_command.
  ELSE.
    RAISE EXCEPTION TYPE /gal/cx_gui_shortcut_exception
      EXPORTING
        textid = /gal/cx_gui_shortcut_exception=>invalid_param_combination.
  ENDIF.

* Copy parameters to attributes
  me->system_id                   = system_id.
  me->saplogon_system_description = saplogon_system_description.
  me->client                      = client.
  me->user                        = user.
  me->language                    = language.
  me->force_login                 = force_login.

  me->sys_snc_name               = sys_snc_name.
  me->sys_snc_qop                = sys_snc_qop.

  IF me->sys_snc_name IS NOT INITIAL AND me->sys_snc_qop IS NOT INITIAL.
    me->with_snc_info = abap_true.
  ELSE.
    me->with_snc_info = abap_false.
  ENDIF.

  IF title IS NOT INITIAL.
    me->title = title.
  ELSE.
    CONCATENATE system_id `.` client ` - ` user ` - ` me->title INTO me->title.

    IF language IS NOT INITIAL.
      WRITE language TO l_language_iso.
      CONCATENATE me->title ` (` l_language_iso `)` INTO me->title.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD execute.
  DATA l_temp_path TYPE string.
  DATA l_temp_name TYPE string.
  DATA l_full_name TYPE string.

  DATA l_file      TYPE REF TO /gal/frontend_file.

  DATA l_message   TYPE string.

* Get folder for temporary files
  cl_gui_frontend_services=>get_temp_directory( CHANGING   temp_dir = l_temp_path
                                                EXCEPTIONS OTHERS  = 1 ).
  IF sy-subrc <> 0.
    l_message = TEXT-e01.

    RAISE EXCEPTION TYPE /gal/cx_gui_shortcut_exception
      EXPORTING
        textid = /gal/cx_gui_shortcut_exception=>sapgui_communication_error
        var1   = l_message.
  ELSE.
    cl_gui_cfw=>flush( ).
  ENDIF.

* Create unique file name for temporary file
  l_temp_name = /gal/uuid=>create_char( ).

  CONCATENATE l_temp_name `.sap` INTO l_temp_name.          "#EC NOTEXT

* Create full file name
  l_full_name = /gal/path=>combine( path1     = l_temp_path
                                    path2     = l_temp_name
                                    separator = /gal/path=>client_path_separator ).

* Write shortcut to temporary file
  CREATE OBJECT l_file
    EXPORTING
      name = l_full_name.

  write_to_file( l_file ).

* Execute shortcut
  cl_gui_frontend_services=>execute( EXPORTING  document          = l_full_name
                                                default_directory = l_temp_path
                                                synchronous       = `X`
                                     EXCEPTIONS OTHERS            = 1 ).
  IF sy-subrc <> 0.
    l_message = TEXT-e02.

    RAISE EXCEPTION TYPE /gal/cx_gui_shortcut_exception
      EXPORTING
        textid = /gal/cx_gui_shortcut_exception=>sapgui_communication_error
        var1   = l_message.
  ENDIF.

* Delete temporary file
  TRY.
      l_file->delete( ).

    CATCH /gal/cx_io_exception.                         "#EC NO_HANDLER
      "File is delete by SAPgui after successful execution.
      "Therefore file may have been deleted already.

  ENDTRY.
ENDMETHOD.


METHOD set_dynpro_field_value.
  DATA l_value TYPE string.

* Pre-defined field values are only supported for transactions
  IF type <> shortcut_type_transaction.
    RAISE EXCEPTION TYPE /gal/cx_gui_shortcut_exception
      EXPORTING
        textid = /gal/cx_gui_shortcut_exception=>invalid_param_combination.
  ENDIF.

* Add field value to parameters
  l_value = field_value.

  CONCATENATE parameters field_name `=` l_value `;` INTO parameters.
ENDMETHOD.


  METHOD set_skip_first_screen.

* Indicator for skipping first screen is only possible for transactions
    IF type <> shortcut_type_transaction.
      RAISE EXCEPTION TYPE /gal/cx_gui_shortcut_exception
        EXPORTING
          textid = /gal/cx_gui_shortcut_exception=>invalid_param_combination.
    ENDIF.

    me->skip_first_screen = skip_first_screen.

  ENDMETHOD.


METHOD write_to_file.
  DATA l_stream TYPE REF TO /gal/stream.

  l_stream = file->open( access     = /gal/file=>access_write
                         mode       = /gal/file=>mode_text
                         options    = /gal/file=>options_create
                         line_break = /gal/file=>line_break_windows ).

  write_to_stream( l_stream ).

  l_stream->close( ).
ENDMETHOD.


METHOD write_to_frontend_file.
  DATA l_file TYPE REF TO /gal/frontend_file.

  CREATE OBJECT l_file
    EXPORTING
      name = name.

  write_to_file( l_file ).
ENDMETHOD.


METHOD write_to_stream.
  DATA l_data TYPE string.

  l_data = write_to_string( ).

  stream->write_chars( l_data ).
ENDMETHOD.


METHOD write_to_string.

* Function module SWN_CREATE_SHORTCUT is not used here because of string length limitations!

  DATA l_language_iso(2) TYPE c.
  DATA l_command         TYPE string.
  DATA l_type            TYPE string.
  DATA l_reuse           TYPE string.
  DATA l_window_size     TYPE string.

* Determine command and type
  CASE type.

    WHEN shortcut_type_transaction.
      IF skip_first_screen = abap_false.
        CONCATENATE transaction_code parameters INTO l_command SEPARATED BY space.
      ELSE.
        CONCATENATE '*' transaction_code INTO l_command.    "#EC NOTEXT
        CONCATENATE l_command parameters INTO l_command SEPARATED BY space.
      ENDIF.
      l_type    = `Transaction`.                            "#EC NOTEXT

    WHEN shortcut_type_report.
      l_command = program_name.
      l_type    = `Report`.                                 "#EC NOTEXT

    WHEN shortcut_type_system_command.
      l_command = system_command.
      l_type    = `SystemCommand`.                          "#EC NOTEXT

  ENDCASE.

* Convert language key to ISO code
  WRITE language TO l_language_iso.

* Determine re-use option
  IF force_login = abap_true.
    l_reuse = `0`.
  ELSE.
    l_reuse = `1`.
  ENDIF.

* Set window size
  l_window_size = `Normal window`.                          "#EC NOTEXT

* Create shortcut
  IF with_snc_info = abap_false.
    CONCATENATE `[System]`                                 /gal/string=>line_break_windows "#EC NOTEXT
                `Name=`        system_id                   /gal/string=>line_break_windows "#EC NOTEXT
                `Description=` saplogon_system_description /gal/string=>line_break_windows "#EC NOTEXT
                `Client=`      client                      /gal/string=>line_break_windows "#EC NOTEXT
                `[User]`                                   /gal/string=>line_break_windows "#EC NOTEXT
                `Name=`        user                        /gal/string=>line_break_windows "#EC NOTEXT
                `Language=`    l_language_iso              /gal/string=>line_break_windows "#EC NOTEXT
                `[Function]`                               /gal/string=>line_break_windows "#EC NOTEXT
                `Title=`       title                       /gal/string=>line_break_windows "#EC NOTEXT
                `Command=`     l_command                   /gal/string=>line_break_windows "#EC NOTEXT
                `Type=`        l_type                      /gal/string=>line_break_windows "#EC NOTEXT
                `[Configuration]`                          /gal/string=>line_break_windows "#EC NOTEXT
                `GuiSize=`     l_window_size               /gal/string=>line_break_windows "#EC NOTEXT
                `[Options]`                                /gal/string=>line_break_windows "#EC NOTEXT
                `Reuse=`       l_reuse                     /gal/string=>line_break_windows "#EC NOTEXT
           INTO result.
  ELSE.
    CONCATENATE `[System]`                                 /gal/string=>line_break_windows "#EC NOTEXT
                `Name=`        system_id                   /gal/string=>line_break_windows "#EC NOTEXT
                `Description=` saplogon_system_description /gal/string=>line_break_windows "#EC NOTEXT
                `Client=`      client                      /gal/string=>line_break_windows "#EC NOTEXT
                `snc_name=`    sys_snc_name                /gal/string=>line_break_windows "#EC NOTEXT
                `snc_qop=`     sys_snc_qop                 /gal/string=>line_break_windows "#EC NOTEXT
                `[User]`                                   /gal/string=>line_break_windows "#EC NOTEXT
                `Name=`        user                        /gal/string=>line_break_windows "#EC NOTEXT
                `Language=`    l_language_iso              /gal/string=>line_break_windows "#EC NOTEXT
                `[Function]`                               /gal/string=>line_break_windows "#EC NOTEXT
                `Title=`       title                       /gal/string=>line_break_windows "#EC NOTEXT
                `Command=`     l_command                   /gal/string=>line_break_windows "#EC NOTEXT
                `Type=`        l_type                      /gal/string=>line_break_windows "#EC NOTEXT
                `[Configuration]`                          /gal/string=>line_break_windows "#EC NOTEXT
                `GuiSize=`     l_window_size               /gal/string=>line_break_windows "#EC NOTEXT
                `[Options]`                                /gal/string=>line_break_windows "#EC NOTEXT
                `Reuse=`       l_reuse                     /gal/string=>line_break_windows "#EC NOTEXT
           INTO result.
  ENDIF.
ENDMETHOD.
ENDCLASS.
