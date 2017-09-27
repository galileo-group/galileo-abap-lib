class /GAL/JOB_SAP definition
  public
  inheriting from /GAL/JOB
  final
  create private .

*"* public components of class /GAL/JOB_SAP
*"* do not include other source files here!!!
public section.
  type-pools ABAP .

  data PROGRAM_NAME type SYREPID read-only .
  data SELECTION_TABLE type RSPARAMS_TT read-only .
  data EXECUTION_SYSTEM type /GAL/SYSTEM_ID .

  class-methods CREATE_JOB
    importing
      !JOB_NAME type BTCJOB default 'BACKGROUND_JOB'
      !DESTINATION type STRING optional
      !EXECUTION_SYSTEM type /GAL/SYSTEM_ID optional
      !PROGRAM_NAME type SYREPID
      !SELECTION_TABLE type RSPARAMS_TT optional
    returning
      value(JOB) type ref to /GAL/JOB_SAP
    raising
      /GAL/CX_JS_EXCEPTION .
  class-methods READ_JOB_FROM_DB_SAP
    importing
      !ID type /GAL/JOB_ID
      !UNDELETE_BEFORE_INIT type ABAP_BOOL default ABAP_FALSE
    returning
      value(JOB) type ref to /GAL/JOB_SAP
    raising
      /GAL/CX_JS_EXCEPTION .

  methods DELETE_FROM_DB
    redefinition .
  methods EXECUTE_ASYNC
    redefinition .
  methods GET_PROGRAM_NAME
    redefinition .
  methods STORE_TO_DB
    redefinition .
protected section.
*"* protected components of class /GAL/JOB_SAP
*"* do not include other source files here!!!

  methods INIT_ATTRS_CREATE_SAP
    importing
      !EXECUTION_SYSTEM type /GAL/SYSTEM_ID optional
      !PROGRAM_NAME type SYREPID
      !SELECTION_TABLE type RSPARAMS_TT optional .

  methods INIT_ATTRS_FROM_DB
    redefinition .
private section.
*"* private components of class /GAL/JOB_SAP
*"* do not include other source files here!!!
ENDCLASS.



CLASS /GAL/JOB_SAP IMPLEMENTATION.


METHOD create_job.

  DATA l_ex TYPE REF TO /gal/cx_js_exception.

  IF NOT destination IS INITIAL AND execution_system IS INITIAL.
    RAISE EXCEPTION TYPE /gal/cx_js_exception
      EXPORTING
        textid = /gal/cx_js_exception=>sap_job_dest_wo_exec_sys.
  ENDIF.

* Create job
  CREATE OBJECT job.

* Initialize base class
  job->init_attrs_create( job_name    = job_name
                          destination = destination ).

* Initialize SAP job
  job->init_attrs_create_sap( execution_system = execution_system
                              program_name     = program_name
                              selection_table  = selection_table ).

* Store job in database
  job->enqueue( refresh = abap_false ).

  TRY.
      job->store_to_db( ).

    CATCH /gal/cx_js_exception INTO l_ex.
      job->dequeue( ).
      RAISE EXCEPTION l_ex.

  ENDTRY.

  job->dequeue( ).
ENDMETHOD.


METHOD delete_from_db.

  DATA:
    l_message   TYPE string,
    l_var1      TYPE string,
    l_ex        TYPE REF TO /gal/cx_js_exception,
    l_key_value TYPE string.

  enqueue( ).

  l_key_value = id.
  CALL FUNCTION '/GAL/JS_DB_COPY_ENTRY_TO_HIST'
    EXPORTING
      rfc_route_info           = store_rfc_route_info
      hist_table_name          = '/GAL/JD01S_HIST'
      table_name               = '/GAL/JOBDATA01S'
      key_field                = 'ID'
      key_value                = l_key_value
    EXCEPTIONS
      rfc_exception            = 1
      cannot_create_hist_entry = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0.
    /gal/trace=>write_error( ).
  ENDIF.

  CALL FUNCTION '/GAL/JS_DB_DELETE'
    EXPORTING
      rfc_route_info = store_rfc_route_info
      table_name     = '/GAL/JOBDATA01S'
      id             = id
    EXCEPTIONS
      rfc_exception  = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.
    dequeue( ).
    l_var1 = id.
    RAISE EXCEPTION TYPE /gal/cx_js_exception
      EXPORTING
        textid = /gal/cx_js_exception=>cannot_delete_job_from_db
        var1   = l_var1
        var2   = '/GAL/JOBDATA01S'
        var3   = l_message.
  ENDIF.

  TRY.
      CALL METHOD super->delete_from_db
        EXPORTING
          force = force.
    CATCH /gal/cx_js_exception INTO l_ex.
      dequeue( ).
      RAISE EXCEPTION l_ex.
  ENDTRY.

  dequeue( ).

ENDMETHOD.


METHOD execute_async.
  DATA l_ex          TYPE REF TO /gal/cx_js_exception.
  DATA l_var1        TYPE string.

  DATA l_list_object TYPE STANDARD TABLE OF abaplist.
  DATA l_progname    TYPE progname.

  enqueue( ).

  TRY.
      super->execute_async( ).

      SELECT SINGLE name INTO l_progname FROM trdir WHERE name = program_name. "#EC CI_SUBRC
      IF l_progname IS INITIAL.
        l_var1 = program_name.

        RAISE EXCEPTION TYPE /gal/cx_js_exception
          EXPORTING
            textid = /gal/cx_js_exception=>programm_does_not_exist
            var1   = l_var1.
      ENDIF.

* Execute report and save list
      SUBMIT (program_name)
        WITH SELECTION-TABLE selection_table
             EXPORTING LIST TO MEMORY
         AND RETURN.                                     "#EC CI_SUBMIT

* Write list to spool (if any)
      CALL FUNCTION 'LIST_FROM_MEMORY'
        TABLES
          listobject = l_list_object
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc = 0.
        CALL FUNCTION 'WRITE_LIST'
          TABLES
            listobject = l_list_object
          EXCEPTIONS
            OTHERS     = 0.
      ENDIF.

* Perform post processing
      post_process( ).

      /gal/job=>run_job_scheduler( ).

    CATCH /gal/cx_js_exception INTO l_ex.
      dequeue( ).
      RAISE EXCEPTION l_ex.
  ENDTRY.

  dequeue( ).
ENDMETHOD.


METHOD get_program_name.
  program_name = me->program_name.
ENDMETHOD.


METHOD init_attrs_create_sap.

  me->execution_system = execution_system.
  me->program_name     = program_name.
  me->selection_table  = selection_table.

  type = 'S'.

ENDMETHOD.


METHOD init_attrs_from_db.

  DATA:
    l_table_line      TYPE /gal/db_datas,
    l_table_line_elem TYPE /gal/db_data,
    l_message         TYPE string,
    l_var1            TYPE string,
    l_xml_ex          TYPE REF TO cx_transformation_error,
    l_key_value       TYPE string.

  super->init_attrs_from_db(
    undelete_before_init = undelete_before_init
    id                   = id
  ).

  type = 'S'.

  IF undelete_before_init = abap_true.
    l_key_value = id.
    CALL FUNCTION '/GAL/JS_DB_MOVE_HIST_E_TO_DB'
      EXPORTING
        rfc_route_info      = store_rfc_route_info
        hist_table_name     = '/GAL/JD01S_HIST'
        table_name          = '/GAL/JOBDATA01S'
        key_field           = 'ID'
        key_value           = l_key_value
      EXCEPTIONS
        rfc_exception       = 1
        cannot_create_entry = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO l_message.
      l_var1 = id.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_undelete_job
          var1   = l_var1
          var2   = l_message.
    ENDIF.
  ENDIF.

  IF read_from_hist = abap_false.
    CALL FUNCTION '/GAL/JS_DB_SELECT_SINGLE'
      EXPORTING
        rfc_route_info = store_rfc_route_info
        table_name     = '/GAL/JOBDATA01S'
        id             = id
      IMPORTING
        table_line     = l_table_line
      EXCEPTIONS
        no_data_found  = 1
        OTHERS         = 2.
  ELSE.
    CALL FUNCTION '/GAL/JS_DB_SELECT_SINGLE'
      EXPORTING
        rfc_route_info = store_rfc_route_info
        table_name     = '/GAL/JD01S_HIST'
        id             = id
      IMPORTING
        table_line     = l_table_line
      EXCEPTIONS
        no_data_found  = 1
        OTHERS         = 2.
  ENDIF.
  IF sy-subrc = 1.
    RAISE EXCEPTION TYPE /gal/cx_js_no_job_data_found
      EXPORTING
        textid = /gal/cx_js_no_job_data_found=>/gal/cx_js_no_job_data_found
        var1   = l_var1
        var2   = '/GAL/JOBDATA01S'.
  ELSEIF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.
    l_var1 = id.
    RAISE EXCEPTION TYPE /gal/cx_js_exception
      EXPORTING
        textid = /gal/cx_js_exception=>cannot_read_job_from_db
        var1   = l_var1
        var2   = '/GAL/JOBDATA01S'
        var3   = l_message.
  ENDIF.

  IF l_table_line IS INITIAL.
    l_var1 = id.
    RAISE EXCEPTION TYPE /gal/cx_js_no_job_data_found
      EXPORTING
        textid = /gal/cx_js_no_job_data_found=>/gal/cx_js_no_job_data_found
        var1   = l_var1.
  ENDIF.

  CLEAR l_table_line_elem.

  READ TABLE l_table_line WITH KEY attribute = 'PROGRAM_NAME' INTO l_table_line_elem.
  IF sy-subrc = 0.
    program_name = l_table_line_elem-value.
  ENDIF.

  READ TABLE l_table_line WITH KEY attribute = 'EXECUTION_SYSTEM' INTO l_table_line_elem.
  IF sy-subrc = 0.
    execution_system = l_table_line_elem-value.
  ENDIF.

  TRY.
      CLEAR l_table_line_elem.

      READ TABLE l_table_line WITH KEY attribute = 'SEL_TABLE_SER' INTO l_table_line_elem.
      IF sy-subrc = 0.
        CALL TRANSFORMATION id
             OPTIONS    value_handling = 'default'
             SOURCE XML l_table_line_elem-value
             RESULT     selection_table = selection_table.  "#EC NOTEXT
      ENDIF.

    CATCH cx_transformation_error INTO l_xml_ex.
      CALL METHOD l_xml_ex->if_message~get_text
        RECEIVING
          result = l_var1.

      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>error_deserializing_xml
          var1   = l_var1.

  ENDTRY.

ENDMETHOD.


METHOD read_job_from_db_sap.

  CREATE OBJECT job.

  job->init_attrs_from_db(
    undelete_before_init = undelete_before_init
    id                   = id
  ).

ENDMETHOD.


METHOD store_to_db.
  DATA l_var1            TYPE string.
  DATA lt_table_line     TYPE /gal/db_datas.
  DATA l_table_line_elem TYPE /gal/db_data.
  DATA l_message         TYPE string.
  DATA l_xml_ex          TYPE REF TO cx_transformation_error.

  l_table_line_elem-attribute = 'ID'.
  l_table_line_elem-value     = id.
  INSERT l_table_line_elem INTO TABLE lt_table_line.

  l_table_line_elem-attribute = 'EXECUTION_SYSTEM'.
  l_table_line_elem-value     = execution_system.
  INSERT l_table_line_elem INTO TABLE lt_table_line.

  l_table_line_elem-attribute = 'PROGRAM_NAME'.
  l_table_line_elem-value     = program_name.
  INSERT l_table_line_elem INTO TABLE lt_table_line.

  TRY.
      l_table_line_elem-attribute = 'SEL_TABLE_SER'.

      CALL TRANSFORMATION id
         OPTIONS    data_refs          = 'heap-or-create'
                    initial_components = 'include'
                    technical_types    = 'error'
                    value_handling     = 'default'
                    xml_header         = 'full'
         SOURCE     selection_table    = selection_table
         RESULT XML l_table_line_elem-value.                "#EC NOTEXT

      INSERT l_table_line_elem INTO TABLE lt_table_line.

    CATCH cx_transformation_error INTO l_xml_ex.
      l_var1 = l_xml_ex->if_message~get_text( ).

      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>error_creating_xml
          var1   = l_var1.
  ENDTRY.

  CALL FUNCTION '/GAL/JS_DB_WRITE'
    EXPORTING
      rfc_route_info     = store_rfc_route_info
      table_name         = '/GAL/JOBDATA01S'
      table_line         = lt_table_line
    EXCEPTIONS
      rfc_exception      = 1
      wrong_content_data = 2
      cannot_write_to_db = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    l_var1 = id.

    RAISE EXCEPTION TYPE /gal/cx_js_exception
      EXPORTING
        textid = /gal/cx_js_exception=>cannot_create_job_in_db
        var1   = l_var1
        var2   = '/GAL/JOBDATA01S'
        var3   = l_message.
  ENDIF.

  super->store_to_db( ).

ENDMETHOD.
ENDCLASS.
