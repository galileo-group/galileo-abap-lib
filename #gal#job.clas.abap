*----------------------------------------------------------------------*
*       CLASS /GAL/JOB DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class /GAL/JOB definition
  public
  abstract
  create public .

*"* public components of class /GAL/JOB
*"* do not include other source files here!!!
*"* protected components of class /GAL/JOB
*"* do not include other source files here!!!
public section.
  type-pools ABAP .

  class-data STORE_RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO read-only .
  data AUTO_CONTINUE type FLAG read-only .
  data AUTO_EVENT type FLAG .
  data DESTINATION type STRING read-only .
  data ERROR_LOG type /GAL/TT_MESSAGE_STRUCT_TS read-only .
  data EXEC_USER_ID type /GAL/USER_ID read-only .
  data ID type /GAL/JOB_ID read-only .
  data JOB_COUNT type BTCJOBCNT read-only .
  data JOB_NAME type BTCJOB read-only .
  data MOD_TIMESTAMP type TIMESTAMP read-only .
  data STATUS type /GAL/JOB_STATUS read-only .
  data TRANSITION_LOG type /GAL/TRANSITION_LOG read-only .
  data TYPE type /GAL/JOB_TYPE read-only .
  data WAIT_FOR_RES type FLAG read-only .

  class-methods CLASS_CONSTRUCTOR .
  class-methods CLASS_GET_INIT_EXCEPTION
    returning
      value(INIT_EXCEPTION) type ref to CX_ROOT .
  class-methods DETERMINE_STORE_DESTINATION
    returning
      value(STORE_DESTINATION) type /GAL/RFC_DESTINATION
    raising
      /GAL/CX_JS_EXCEPTION .
  class-methods RAISE_USER_EVENT
    importing
      !EVENT_ID type /GAL/PRECONDITION_ID
      !DO_NOT_RUN_SCHEDULER type FLAG optional
    raising
      /GAL/CX_JS_EXCEPTION .
  class-methods READ_JOB_FROM_DB
    importing
      !ID type /GAL/JOB_ID
      !ENQUEUE type FLAG optional
      !UNDELETE_BEFORE_INIT type ABAP_BOOL default ABAP_FALSE
    returning
      value(JOB) type ref to /GAL/JOB
    raising
      /GAL/CX_JS_EXCEPTION .
  class-methods RUN_JOB_SCHEDULER
    importing
      !RETRY_TIMES type INT4 default 3
      !RETRY_WAIT_INTERVAL type INT4 default 5
      !IN_BACKGROUND type FLAG optional
    exporting
      !MESSAGES type /GAL/TT_MESSAGE_STRUCT
    raising
      /GAL/CX_JS_EXCEPTION .
  class-methods UPDATE_PRECONDITIONS
    raising
      /GAL/CX_JS_EXCEPTION .
  methods ADD_NEEDED_RESOURCE
    importing
      !RESOURCE_STRING type /GAL/RESOURCE_STRING
    exporting
      !STATUS type /GAL/PRECONDITION_STATUS
    raising
      /GAL/CX_JS_EXCEPTION .
  methods ADD_PREDECESSOR_JOB
    importing
      !JOB type ref to /GAL/JOB
    raising
      /GAL/CX_JS_EXCEPTION .
  methods ADD_START_TIMESTAMP
    importing
      !TIMESTAMP type TIMESTAMP
    raising
      /GAL/CX_JS_EXCEPTION .
  methods ADD_USER_EVENT
    returning
      value(EVENT_ID) type /GAL/PRECONDITION_ID
    raising
      /GAL/CX_JS_EXCEPTION .
  methods CANCEL
    importing
      !MUTEX_NAME type STRING optional
    raising
      /GAL/CX_JS_EXCEPTION
      /GAL/CX_LOCK_EXCEPTION .
  methods DELETE_FROM_DB
    importing
      !FORCE type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_JS_EXCEPTION .
  methods DEQUEUE
    raising
      /GAL/CX_JS_EXCEPTION .
  methods ENQUEUE
    importing
      !REFRESH type FLAG default 'X'
    raising
      /GAL/CX_JS_CANNOT_ENQUEUE
      /GAL/CX_JS_EXCEPTION .
  methods EXECUTE
    raising
      /GAL/CX_JS_EXCEPTION .
  methods EXECUTE_ASYNC
    raising
      /GAL/CX_JS_EXCEPTION .
  methods GET_PREDECESSOR_JOBS
    returning
      value(PREDECESSOR_JOBS) type /GAL/JOBS
    raising
      /GAL/CX_JS_EXCEPTION .
  methods GET_PROGRAM_NAME
    returning
      value(PROGRAM_NAME) type SYREPID .
  methods IS_WAITING_FOR_EVENT
    importing
      !IGNORE_WHEN_MISSING_PREDECS type FLAG optional
    returning
      value(EVENT_IDS) type /GAL/PRECONDITION_IDS
    raising
      /GAL/CX_JS_EXCEPTION .
  methods POST_PROCESS
    raising
      /GAL/CX_JS_EXCEPTION .
  methods RELEASE
    importing
      !NO_COMMIT type FLAG optional
    raising
      /GAL/CX_JS_EXCEPTION .
  methods RESTART
    raising
      /GAL/CX_JS_EXCEPTION .
  methods RESUME
    importing
      !SKIP_JOB_SCHEDULER type FLAG optional
      !AUTO_CONTINUED type FLAG optional
    raising
      /GAL/CX_JS_EXCEPTION .
  methods SET_JOBDATA
    importing
      !JOB_NAME type BTCJOB
      !JOB_COUNT type BTCJOBCNT
    raising
      /GAL/CX_JS_EXCEPTION .
  methods SET_STATUS_TO_ERROR
    importing
      value(SYMSGID) type SYMSGID default SY-MSGID
      value(SYMSGTY) type SYMSGTY default SY-MSGTY
      value(SYMSGNO) type SYMSGNO default SY-MSGNO
      value(SYMSGV1) type SYMSGV default SY-MSGV1
      value(SYMSGV2) type SYMSGV default SY-MSGV2
      value(SYMSGV3) type SYMSGV default SY-MSGV3
      value(SYMSGV4) type SYMSGV default SY-MSGV4
    preferred parameter SYMSGID .
  methods SET_STATUS_TO_OBSOLETE
    raising
      /GAL/CX_JS_EXCEPTION .
  methods STORE_TO_DB
    raising
      /GAL/CX_JS_EXCEPTION .
  methods IS_AUTO_CONTINUED
    returning
      value(AUTO_CONTINUED) type FLAG .
protected section.

  class-data CONFIG type ref to /GAL/JS_CONFIG_PROVIDER .
  class-data TRACE type ABAP_BOOL .
  data ENQUEUE_COUNTER type I .
  data READ_FROM_HIST type ABAP_BOOL .

  methods CHANGE_STATUS
    importing
      !NEW_STATUS type /GAL/JOB_STATUS
      !AUTO_TRANSITION type FLAG optional .
  methods INIT_ATTRS_CREATE
    importing
      !JOB_NAME type BTCJOB default 'BACKGROUND_JOB'
      !DESTINATION type STRING optional
      !AUTO_CONTINUE type ABAP_BOOL default ABAP_FALSE
      !AUTO_EVENT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_JS_EXCEPTION .
  methods INIT_ATTRS_FROM_DB
    importing
      !ID type /GAL/JOB_ID
      !UNDELETE_BEFORE_INIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_JS_EXCEPTION .
*"* private components of class /GAL/JOB
*"* do not include other source files here!!!
private section.

  class-data CLASS_INIT_EXCEPTION type ref to CX_ROOT .
ENDCLASS.



CLASS /GAL/JOB IMPLEMENTATION.


  METHOD add_needed_resource.

    DATA:
      l_var1            TYPE string,
      l_var2            TYPE string,
      l_message         TYPE string.

    CALL FUNCTION '/GAL/JS_ADD_RESOURCE'
      EXPORTING
        rfc_route_info  = store_rfc_route_info
        job_id          = id
        resource_string = resource_string
      IMPORTING
        status          = status
      EXCEPTIONS
        rfc_exception   = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      l_var1 = id.
      l_var2 = resource_string.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO l_message.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_add_resource
          var1   = l_var1
          var2   = l_var2
          var3   = l_message.
    ENDIF.


  ENDMETHOD.                    "add_needed_resource


  METHOD add_predecessor_job.

    DATA:
      l_var1           TYPE string,
      l_var2           TYPE string,
      l_message        TYPE string.


    CALL FUNCTION '/GAL/JS_ADD_PREDECESSOR_JOB'
      EXPORTING
        rfc_route_info     = store_rfc_route_info
        job_id             = id
        predecessor_job_id = job->id
      EXCEPTIONS
        rfc_exception      = 1
        execution_failed   = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      l_var1 = job->id.
      l_var2 = id.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO l_message.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_add_predecessor
          var1   = l_var1
          var2   = l_var2
          var3   = l_message.
    ENDIF.


  ENDMETHOD.                    "add_predecessor_job


  METHOD add_start_timestamp.

    DATA:
      l_var1           TYPE string,
      l_var2           TYPE string,
      l_message        TYPE string.


* Add precondition to database
    CALL FUNCTION '/GAL/JS_ADD_START_TS'
      EXPORTING
        rfc_route_info = store_rfc_route_info
        job_id         = id
        timestamp      = timestamp
      EXCEPTIONS
        rfc_exception  = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      l_var1 = timestamp.
      l_var2 = id.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO l_message.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_add_start_timestamp
          var1   = l_var1
          var2   = l_var2
          var3   = l_message.
    ENDIF.


* Schedule job scheduler for plannend start timestamp so that job is really executed
    CALL FUNCTION '/GAL/JS_RUN_SCHEDULER'
      EXPORTING
        rfc_route_info       = store_rfc_route_info
        start_timestamp      = timestamp
      EXCEPTIONS
        rfc_exception        = 1
        cannot_run_scheduler = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO l_message.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_schedule_scheduler
          var1   = l_message.
    ENDIF.


  ENDMETHOD.                    "add_start_timestamp


  METHOD add_user_event.

    DATA:
      l_var1           TYPE string,
      l_message        TYPE string.


    CALL FUNCTION '/GAL/JS_ADD_USER_EVENT'
      EXPORTING
        rfc_route_info = store_rfc_route_info
        job_id         = id
      IMPORTING
        event_id       = event_id
      EXCEPTIONS
        rfc_exception  = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      l_var1 = id.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO l_message.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_add_user_event
          var1   = l_var1
          var2   = l_message.
    ENDIF.

    IF auto_event IS NOT INITIAL.
      CALL METHOD /gal/job=>raise_user_event
        EXPORTING
          event_id             = event_id
          do_not_run_scheduler = 'X'.
    ENDIF.

  ENDMETHOD.                    "add_user_event


METHOD cancel.

  DATA:
    l_lock                 TYPE REF TO /gal/mutex,
    l_ex_lock              TYPE REF TO /gal/cx_lock_exception,
    l_error                TYPE string,
    l_id                   TYPE string,
    l_rfc_route_info_step2 TYPE /gal/rfc_route_info,
    l_syuname              TYPE symsgv,
    l_config_store         TYPE REF TO /gal/config_store_local,
    l_config_folder        TYPE REF TO /gal/config_node,
    l_lock_timeout         TYPE i,
    l_message_struct       TYPE /gal/st_message_struct_ts,
    l_ex                   TYPE REF TO /gal/cx_js_exception.


  IF NOT mutex_name IS INITIAL.
    TRY.
        CREATE OBJECT l_config_store.
        l_config_folder = l_config_store->get_node(
          path = '/Galileo Group AG/Open Source Components/Job Scheduler/Exclusive scheduling mutex timeout'
        ).                                                  "#EC NOTEXT
        l_config_folder->get_value( IMPORTING value = l_lock_timeout ).

      CATCH /gal/cx_config_exception.
        l_lock_timeout = 30.                             "#EC NUMBER_OK
    ENDTRY.
    TRY.
        CREATE OBJECT l_lock
          EXPORTING
            rfc_route_info = store_rfc_route_info
            name           = mutex_name.
        l_lock->acquire(
          EXPORTING
            lock_timeout = l_lock_timeout
            wait_timeout = l_lock_timeout
        ).
      CATCH /gal/cx_lock_exception INTO l_ex_lock.
        RAISE EXCEPTION l_ex_lock.
    ENDTRY.
  ENDIF.


  IF NOT status CA 'IWRS'.
    TRY.
        l_lock->release( ).
      CATCH /gal/cx_lock_exception INTO l_ex_lock.
        /gal/trace=>write_exception(
          EXPORTING
            exception               = l_ex_lock
        ).
    ENDTRY.
    l_error = TEXT-001.
    l_id = id.
    RAISE EXCEPTION TYPE /gal/cx_js_exception
      EXPORTING
        textid = /gal/cx_js_exception=>cannot_cancel_job
        var1   = l_id
        var2   = l_error.
  ENDIF.


  IF status = 'R'.
    IF job_name IS INITIAL OR job_count IS INITIAL.
      TRY.
          l_lock->release( ).
        CATCH /gal/cx_lock_exception INTO l_ex_lock.
          /gal/trace=>write_exception(
            EXPORTING
              exception               = l_ex_lock
          ).
      ENDTRY.
      l_error = TEXT-000.
      l_id = id.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_cancel_job
          var1   = l_id
          var2   = l_error.
    ENDIF.

    l_rfc_route_info_step2 = /gal/cfw_helper=>rfc_route_info_from_string( destination ).
    CALL FUNCTION '/GAL/JS_CANCEL_JOB'
      EXPORTING
        rfc_route_info       = store_rfc_route_info
        rfc_route_info_step2 = l_rfc_route_info_step2
        job_name             = job_name
        job_count            = job_count
        wait                 = abap_true
      EXCEPTIONS
        cannot_be_cancelled  = 1
        rfc_exception        = 2
        execution_failed     = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 INTO l_error.
      IF NOT mutex_name IS INITIAL.
        TRY.
            l_lock->release( ).
          CATCH /gal/cx_lock_exception INTO l_ex_lock.
            /gal/trace=>write_exception(
              EXPORTING
                exception               = l_ex_lock
            ).
        ENDTRY.
      ENDIF.
      l_id = id.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_cancel_job
          var1   = l_id
          var2   = l_error.
    ENDIF.
  ENDIF.

  IF NOT mutex_name IS INITIAL.
    TRY.
        l_lock->release( ).
      CATCH /gal/cx_lock_exception INTO l_ex_lock.
        /gal/trace=>write_exception(
          EXPORTING
            exception               = l_ex_lock
        ).
    ENDTRY.
  ENDIF.

  /gal/job=>run_job_scheduler( ).

  enqueue( ).

  DO 0 TIMES. MESSAGE e026 WITH ''. ENDDO.
  IF NOT status = 'E'.
    l_syuname = sy-uname.
    set_status_to_error(
      EXPORTING
        symsgid = '/GAL/JS'
        symsgty = 'E'
        symsgno = '026'
        symsgv1 = l_syuname
    ).
  ELSE.
    CLEAR l_message_struct.
    GET TIME STAMP FIELD l_message_struct-timestamp.
    l_message_struct-message_id = '/GAL/JS'.
    l_message_struct-message_type = 'E'.
    l_message_struct-message_number = '026'.
    l_message_struct-message_var1 = sy-uname.
    APPEND l_message_struct TO error_log.
    TRY.
        store_to_db( ).
      CATCH /gal/cx_js_exception INTO l_ex.
        dequeue( ).
        RAISE EXCEPTION l_ex.
    ENDTRY.
  ENDIF.

  dequeue( ).

ENDMETHOD.


METHOD change_status.

  DATA:
    l_trans_log_entry   TYPE /gal/transition_log_entry.


  l_trans_log_entry-source_state = status.
  GET TIME STAMP FIELD l_trans_log_entry-timestamp.
  l_trans_log_entry-syuname = sy-uname.

  status = new_status.
  l_trans_log_entry-target_state = status.

  l_trans_log_entry-auto_trans = auto_transition.

  APPEND l_trans_log_entry TO transition_log.

ENDMETHOD.


  METHOD class_constructor.

    DATA:
      l_store_destination TYPE /gal/rfc_destination,
      l_ex                TYPE REF TO /gal/cx_js_exception,
      l_config_store      TYPE REF TO /gal/config_store_remote,
      l_config_node       TYPE REF TO /gal/config_node,
      l_ex_config         TYPE REF TO /gal/cx_config_exception,
      l_message           TYPE string.


    TRY.
        " Trying to instanciate the CCM specific config provider.
        CREATE OBJECT config TYPE ('/GAL/CCM_JS_CONFIG_PROVIDER').
      CATCH cx_sy_create_object_error.
        " CCM specific config provider failed to instanciate.
        " creating common config provider
        /gal/trace=>write_text(
          EXPORTING
            text = 'INFO: No CCM specific config provider found. Falling back to OS config provider'
        ). "#EC NOTEXT
        CREATE OBJECT config.
    ENDTRY.

    TRY.
        l_store_destination = /gal/job=>determine_store_destination( ).

      CATCH /gal/cx_js_exception INTO l_ex.
        class_init_exception = l_ex.
        /gal/trace=>write_exception( l_ex ).
        RETURN.
    ENDTRY.

    CALL METHOD /gal/cfw_helper=>rfc_route_info_from_string
      EXPORTING
        string         = l_store_destination
      RECEIVING
        rfc_route_info = store_rfc_route_info.


    TRY.
        CREATE OBJECT l_config_store
          EXPORTING
            rfc_route_info = store_rfc_route_info.
        l_config_node = l_config_store->get_node( path = '/Galileo Group AG/Open Source Components/Job Scheduler/Detailled Tracing' ).
        IF NOT l_config_node IS INITIAL.
          l_config_node->get_value(
            IMPORTING
              value    = trace
          ).
        ENDIF.
      CATCH /gal/cx_config_exception INTO l_ex_config.
        class_init_exception = l_ex_config.
        l_message = l_ex_config->get_text( ).
        CALL METHOD /gal/trace=>write_text
          EXPORTING
            text = l_message.
        RETURN.
    ENDTRY.




  ENDMETHOD.                    "class_constructor


  METHOD class_get_init_exception.

    init_exception = class_init_exception.

  ENDMETHOD.


  METHOD delete_from_db.

    DATA:
      l_message         TYPE string,
      l_var1            TYPE string,
      l_var2            TYPE string,
      l_key_value       TYPE string.


    IF force = abap_false AND NOT status CA 'OF'.
      l_var1 = id.
      l_var2 = text-002.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_delete_job
          var1   = l_var1
          var2   = l_var2.
    ENDIF.

    enqueue( ).

    l_key_value = id.
    CALL FUNCTION '/GAL/JS_DB_COPY_ENTRY_TO_HIST'
      EXPORTING
        rfc_route_info           = store_rfc_route_info
        hist_table_name          = '/GAL/JD01_HIST'
        table_name               = '/GAL/JOBDATA01'
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
        table_name     = '/GAL/JOBDATA01'
        id             = id
      EXCEPTIONS
        rfc_exception  = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO l_message.
      l_var1 = id.
      dequeue( ).
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_delete_job_from_db
          var1   = l_var1
          var2   = '/GAL/JOBDATA01'
          var3   = l_message.
    ENDIF.

    dequeue( ).

  ENDMETHOD.                    "delete_from_db


  METHOD dequeue.

    DATA:
      l_var1         TYPE string,
      l_var2         TYPE string,
      l_callstack    TYPE abap_callstack,
      l_level        TYPE i.

    FIELD-SYMBOLS:
      <l_callstack>  LIKE LINE OF l_callstack.


    IF id IS INITIAL.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>no_id_for_locking.
    ENDIF.


    IF trace = abap_true.

      /gal/trace=>write_text(
        EXPORTING
          text      = `===============================`
          no_flush  = 'X'
      ).                                                    "#EC NOTEXT

      CALL FUNCTION 'SYSTEM_CALLSTACK'
        EXPORTING
          max_level = 5
        IMPORTING
          callstack = l_callstack.

      l_level = enqueue_counter - 1.

      LOOP AT l_callstack ASSIGNING <l_callstack>.
        /gal/trace=>write_text(
          EXPORTING
            text      = `{1} {2} - Counter: {3}, Caller: {4} - {5}`
            var01     = id
            var02     = 'DEQUEUE'
            var03     = l_level
            var04     = <l_callstack>-blockname
            var05     = <l_callstack>-line
            no_flush  = 'X'
        ).                                                  "#EC NOTEXT
      ENDLOOP.
      /gal/trace=>flush( ).
    ENDIF.

    IF enqueue_counter > 1.
      enqueue_counter = enqueue_counter - 1.
      RETURN.
    ENDIF.


    CALL FUNCTION '/GAL/JS_DEQUEUE_JOB'
      EXPORTING
        rfc_route_info = store_rfc_route_info
        job_id         = id
        trace          = trace
      EXCEPTIONS
        rfc_exception  = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
*    /gal/ccm_trace_handler=>write_error( ).

      l_var1 = id.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO l_var2.

      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_dequeue
          var1   = l_var1
          var2   = l_var2.
    ENDIF.

    enqueue_counter = 0.


  ENDMETHOD.                    "dequeue


  METHOD determine_store_destination.

    store_destination = config->get_store_destination( ).

  ENDMETHOD.                    "determine_store_destination


  METHOD enqueue.

    DATA:
      l_var1         TYPE string,
      l_var2         TYPE string,
      l_callstack    TYPE abap_callstack.

    FIELD-SYMBOLS:
      <l_callstack>  LIKE LINE OF l_callstack.


    IF id IS INITIAL.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>no_id_for_locking.
    ENDIF.

    IF read_from_hist = abap_true.
      l_var1 = id.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>job_read_only
          var1   = l_var1.
    ENDIF.


    IF trace = abap_true.

      /gal/trace=>write_text(
        EXPORTING
          text      = `===============================`
          no_flush  = 'X'
      ).                                                    "#EC NOTEXT

      CALL FUNCTION 'SYSTEM_CALLSTACK'
        EXPORTING
          max_level = 5
        IMPORTING
          callstack = l_callstack.

      LOOP AT l_callstack ASSIGNING <l_callstack>.
        /gal/trace=>write_text(
          EXPORTING
            text      = `{1} {2} - Counter: {3}, Caller: {4} - {5}`
            var01     = id
            var02     = 'ENQUEUE'
            var03     = enqueue_counter
            var04     = <l_callstack>-blockname
            var05     = <l_callstack>-line
            no_flush  = 'X'
        ).                                                  "#EC NOTEXT
      ENDLOOP.
      /gal/trace=>flush( ).
    ENDIF.

* Prüfen, ob Sperrung nötig ist *
    IF enqueue_counter > 0.
      enqueue_counter = enqueue_counter + 1.
      RETURN.
    ENDIF.

    CALL FUNCTION '/GAL/JS_ENQUEUE_JOB'
      EXPORTING
        rfc_route_info   = store_rfc_route_info
        job_id           = id
        trace            = trace
      EXCEPTIONS
        rfc_exception    = 1
        foreign_lock     = 2
        execution_failed = 3
        OTHERS           = 4.
    IF sy-subrc = 2.
      l_var1 = id.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO l_var2.
      RAISE EXCEPTION TYPE /gal/cx_js_cannot_enqueue
        EXPORTING
          textid = /gal/cx_js_cannot_enqueue=>cannot_enqueue
          var1   = l_var1
          var2   = l_var2.
    ELSEIF sy-subrc <> 0.
      l_var1 = id.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO l_var2.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>enqueue_error
          var1   = l_var1
          var2   = l_var2.
    ENDIF.

    enqueue_counter = 1.

    IF NOT refresh IS INITIAL.
      init_attrs_from_db( id = id ).
    ENDIF.

  ENDMETHOD.                    "enqueue


METHOD execute.

  DATA:
    l_var1                 TYPE string,
    l_message              TYPE string,
    l_fulfilled            TYPE flag,
    l_locked_resource_id   TYPE /gal/resource_string,
    l_rfc_route_info_step2 TYPE /gal/rfc_route_info,
    l_ex                   TYPE REF TO /gal/cx_js_exception,
    l_ex_res               TYPE REF TO /gal/cx_js_missing_resource,
    l_msgv1                TYPE sy-msgv1,
    l_msgv2                TYPE sy-msgv2,
    l_msgv3                TYPE sy-msgv3,
    l_msgv4                TYPE sy-msgv4.

* Background break point support
  cfw_break_point_support.
  cfw_break_point `/GAL/JOB=>EXECUTE`.

* Enqueue job
  enqueue( ).

  TRY.

* Make sure that job ist in waiting status
      IF status <> 'W'.
*   Job is not in status 'waiting' => error
        l_var1 = id.

        RAISE EXCEPTION TYPE /gal/cx_js_exception
          EXPORTING
            textid = /gal/cx_js_exception=>job_not_waiting
            var1   = l_var1.
      ENDIF.

* Begin of critical section
* (this coding may not be executed by multiple processes at the sime time!)
      CALL FUNCTION '/GAL/JS_ENQUEUE_JS_LOCK'
        EXPORTING
          rfc_route_info = store_rfc_route_info
          lock_key       = 'RES_CHECKS'
          wait           = abap_true
        EXCEPTIONS
          OTHERS         = 1.
      IF sy-subrc <> 0.
        l_var1 = id.

        RAISE EXCEPTION TYPE /gal/cx_js_exception
          EXPORTING
            textid = /gal/cx_js_exception=>cannot_allocate_semphore
            var1   = l_var1.
      ENDIF.

      CALL FUNCTION '/GAL/JS_CHECK_RESOURCE'
        EXPORTING
          rfc_route_info     = store_rfc_route_info
          job_id             = id
        IMPORTING
          locked_resource_id = l_locked_resource_id
        EXCEPTIONS
          rfc_exception      = 1
          OTHERS             = 2.
      IF sy-subrc <> 0.
        l_var1 = id.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO l_message.

        CALL FUNCTION '/GAL/JS_DEQUEUE_JS_LOCK'
          EXPORTING
            rfc_route_info = store_rfc_route_info
            lock_key       = 'RES_CHECKS'
          EXCEPTIONS
            OTHERS         = 0.

        RAISE EXCEPTION TYPE /gal/cx_js_exception
          EXPORTING
            textid = /gal/cx_js_exception=>cannot_check_resources
            var1   = l_var1
            var2   = l_message.
      ENDIF.

      IF l_locked_resource_id IS NOT INITIAL.
        CALL FUNCTION '/GAL/JS_DEQUEUE_JS_LOCK'
          EXPORTING
            rfc_route_info = store_rfc_route_info
            lock_key       = 'RES_CHECKS'
          EXCEPTIONS
            OTHERS         = 0.

        l_var1 = l_locked_resource_id.

        RAISE EXCEPTION TYPE /gal/cx_js_missing_resource
          EXPORTING
            textid = /gal/cx_js_missing_resource=>missing_resource
            var1   = l_var1.
      ENDIF.

      CALL FUNCTION '/GAL/JS_UPDATE_PRECONDITIONS'
        EXPORTING
          rfc_route_info = store_rfc_route_info
          job_id         = id
        EXCEPTIONS
          OTHERS         = 1.
      IF sy-subrc <> 0.
        l_var1 = id.

        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO l_message.

        CALL FUNCTION '/GAL/JS_DEQUEUE_JS_LOCK'
          EXPORTING
            rfc_route_info = store_rfc_route_info
            lock_key       = 'RES_CHECKS'
          EXCEPTIONS
            OTHERS         = 0.

        RAISE EXCEPTION TYPE /gal/cx_js_exception
          EXPORTING
            textid = /gal/cx_js_exception=>cannot_update_precondition
            var1   = l_var1
            var2   = l_message.
      ENDIF.

      CALL FUNCTION '/GAL/JS_CHECK_PRECONDITIONS'
        EXPORTING
          rfc_route_info = store_rfc_route_info
          job_id         = id
        IMPORTING
          fulfilled      = l_fulfilled
        EXCEPTIONS
          rfc_exception  = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        l_var1 = id.

        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO l_message.

        CALL FUNCTION '/GAL/JS_DEQUEUE_JS_LOCK'
          EXPORTING
            rfc_route_info = store_rfc_route_info
            lock_key       = 'RES_CHECKS'
          EXCEPTIONS
            OTHERS         = 0.

        RAISE EXCEPTION TYPE /gal/cx_js_exception
          EXPORTING
            textid = /gal/cx_js_exception=>cannot_check_precondition
            var1   = l_var1
            var2   = l_message.
      ENDIF.


      IF l_fulfilled IS INITIAL.
        CALL FUNCTION '/GAL/JS_DEQUEUE_JS_LOCK'
          EXPORTING
            rfc_route_info = store_rfc_route_info
            lock_key       = 'RES_CHECKS'
          EXCEPTIONS
            OTHERS         = 0.
        l_var1 = id.
        RAISE EXCEPTION TYPE /gal/cx_js_exception
          EXPORTING
            textid = /gal/cx_js_exception=>preconditions_not_met
            var1   = l_var1.
      ENDIF.


      CALL FUNCTION '/GAL/JS_LOCK_RESOURCES'
        EXPORTING
          rfc_route_info = store_rfc_route_info
          job_id         = id
        EXCEPTIONS
          rfc_exception  = 1.
      IF sy-subrc <> 0.
        l_var1 = id.

        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO l_message.

        CALL FUNCTION '/GAL/JS_DEQUEUE_JS_LOCK'
          EXPORTING
            rfc_route_info = store_rfc_route_info
            lock_key       = 'RES_CHECKS'
          EXCEPTIONS
            OTHERS         = 0.

        RAISE EXCEPTION TYPE /gal/cx_js_exception
          EXPORTING
            textid = /gal/cx_js_exception=>cannot_lock_resources
            var1   = l_var1
            var2   = l_message.
      ENDIF.

* Set status tu 'running'and update job
      change_status( new_status =  'R' ).

      TRY.
          store_to_db( ).

        CATCH /gal/cx_js_exception INTO l_ex.
          CALL FUNCTION '/GAL/JS_DEQUEUE_JS_LOCK'
            EXPORTING
              rfc_route_info = store_rfc_route_info
              lock_key       = 'RES_CHECKS'
            EXCEPTIONS
              OTHERS         = 0.

          dequeue( ).

          RAISE EXCEPTION l_ex.

      ENDTRY.

* End of critical section
* (this coding may not be executed by multiple processes at the sime time!)
      CALL FUNCTION '/GAL/JS_DEQUEUE_JS_LOCK'
        EXPORTING
          rfc_route_info = store_rfc_route_info
          lock_key       = 'RES_CHECKS'
        EXCEPTIONS
          OTHERS         = 0.


    CATCH /gal/cx_js_missing_resource INTO l_ex_res.
      TRY.
          store_to_db( ).

        CATCH /gal/cx_js_exception INTO l_ex.
          /gal/trace=>write_exception( exception = l_ex ).

      ENDTRY.

      dequeue( ).

      RAISE EXCEPTION l_ex_res.

    CATCH /gal/cx_js_exception INTO l_ex.
      l_message = l_ex->get_text( ).

      /gal/string=>string_to_message_vars( EXPORTING input = l_message
                                           IMPORTING msgv1 = l_msgv1
                                                     msgv2 = l_msgv2
                                                     msgv3 = l_msgv3
                                                     msgv4 = l_msgv4 ).

      DO 0 TIMES. MESSAGE e012(/gal/js) WITH l_msgv1 l_msgv2 l_msgv3 l_msgv4. ENDDO.

      set_status_to_error( symsgid = '/GAL/JS'
                           symsgty = 'E'
                           symsgno = '012'
                           symsgv1 = l_msgv1
                           symsgv2 = l_msgv2
                           symsgv3 = l_msgv3
                           symsgv4 = l_msgv4 ).
      dequeue( ).

      RETURN.

  ENDTRY.

  dequeue( ).

* Schedule job
  l_rfc_route_info_step2 = /gal/cfw_helper=>rfc_route_info_from_string( destination ).

  CALL FUNCTION '/GAL/JS_RUN_JOBS_ASYNC_PART'
    EXPORTING
      rfc_route_info       = store_rfc_route_info
      rfc_route_info_step2 = l_rfc_route_info_step2
      js_job_id            = id
      job_name             = job_name
    IMPORTING
      job_count            = job_count
    EXCEPTIONS
      OTHERS               = 1.
  IF sy-subrc <> 0.
    set_status_to_error( symsgid = sy-msgid
                         symsgty = sy-msgty
                         symsgno = sy-msgno
                         symsgv1 = sy-msgv1
                         symsgv2 = sy-msgv2
                         symsgv3 = sy-msgv3
                         symsgv4 = sy-msgv4 ).
    RETURN.
  ENDIF.
ENDMETHOD.                    "execute


METHOD execute_async.
  cfw_break_point_support.
  cfw_break_point `/GAL/JOB=>EXECUTE_ASYNC`.
ENDMETHOD.


  METHOD get_predecessor_jobs.

    DATA:
      l_var1             TYPE string,
      l_message          TYPE string,
      lt_predec_job_ids  TYPE /gal/tt_job_ids,
      l_job              TYPE REF TO /gal/job.

    FIELD-SYMBOLS:
      <l_predec_job_id>  TYPE /gal/job_id.


    CALL FUNCTION '/GAL/JS_GET_PREDEC_JOBS'
      EXPORTING
        rfc_route_info      = store_rfc_route_info
        job_id              = id
      IMPORTING
        predecessor_job_ids = lt_predec_job_ids
      EXCEPTIONS
        rfc_exception       = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO l_message.
      l_var1 = id.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_get_predecessor_jobs
          var1   = l_var1
          var2   = l_message.
    ENDIF.


    LOOP AT lt_predec_job_ids ASSIGNING <l_predec_job_id>.
      CALL METHOD /gal/job=>read_job_from_db
        EXPORTING
          id  = <l_predec_job_id>
        RECEIVING
          job = l_job.
      APPEND l_job TO predecessor_jobs.
    ENDLOOP.

  ENDMETHOD.                    "get_predecessor_jobs


METHOD get_program_name.
  program_name = '/GAL/JS_RUN_JOB_ASYNC_PART'.
ENDMETHOD.


METHOD init_attrs_create.
  id = /gal/uuid=>create_char( ).

  me->job_name      = job_name.
  me->destination   = destination.
  me->auto_continue = auto_continue.
  me->auto_event    = auto_event.

* Jobs werden immer im Status 'W' angelegt
  status = 'I'.

* Aktueller Zeitstempel als Modifikationszeitpunkt
  GET TIME STAMP FIELD mod_timestamp.
ENDMETHOD.                    "init_attrs_create


  METHOD init_attrs_from_db.

    DATA:
      l_var1              TYPE string,
      l_table_line        TYPE /gal/db_datas,
      l_table_line_elem   TYPE /gal/db_data,
      l_message           TYPE string,
      l_xml_ex            TYPE REF TO cx_transformation_error,
      l_key_value         TYPE string,
      l_must_be_obsolete  TYPE abap_bool.


    me->id = id.


    IF undelete_before_init = abap_true.
      l_key_value = id.
      CALL FUNCTION '/GAL/JS_DB_MOVE_HIST_E_TO_DB'
        EXPORTING
          rfc_route_info      = store_rfc_route_info
          hist_table_name     = '/GAL/JD01_HIST'
          table_name          = '/GAL/JOBDATA01'
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


    CALL FUNCTION '/GAL/JS_DB_SELECT_SINGLE'
      EXPORTING
        rfc_route_info = store_rfc_route_info
        table_name     = '/GAL/JOBDATA01'
        id             = id
      IMPORTING
        table_line     = l_table_line
      EXCEPTIONS
        no_data_found  = 1
        unknown_table  = 2
        rfc_exception  = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO l_message.
      l_var1 = id.
      IF undelete_before_init = abap_false.
        l_must_be_obsolete = abap_true.
        read_from_hist = abap_true.
        CALL FUNCTION '/GAL/JS_DB_SELECT_SINGLE'
          EXPORTING
            rfc_route_info = store_rfc_route_info
            table_name     = '/GAL/JD01_HIST'
            id             = id
          IMPORTING
            table_line     = l_table_line
          EXCEPTIONS
            no_data_found  = 1
            unknown_table  = 2
            rfc_exception  = 3
            OTHERS         = 4.
      ENDIF.
      IF undelete_before_init = abap_true OR sy-subrc <> 0.
        RAISE EXCEPTION TYPE /gal/cx_js_exception
          EXPORTING
            textid = /gal/cx_js_exception=>cannot_read_job_from_db
            var1   = l_var1
            var2   = '/GAL/JOBDATA01'
            var3   = l_message.
      ENDIF.
    ENDIF.


    IF l_must_be_obsolete = abap_true.
      status = 'O'.
    ELSE.
      READ TABLE l_table_line WITH KEY attribute = 'STATUS' INTO l_table_line_elem.
      IF sy-subrc = 0.
        status = l_table_line_elem-value.
      ENDIF.
    ENDIF.

    CLEAR l_table_line_elem.
    READ TABLE l_table_line WITH KEY attribute = 'DESTINATION' INTO l_table_line_elem.
    IF sy-subrc = 0.
      destination = l_table_line_elem-value.
    ENDIF.

    CLEAR l_table_line_elem.
    READ TABLE l_table_line WITH KEY attribute = 'WAIT_FOR_RES' INTO l_table_line_elem.
    IF sy-subrc = 0.
      wait_for_res = l_table_line_elem-value.
    ENDIF.

    CLEAR l_table_line_elem.
    READ TABLE l_table_line WITH KEY attribute = 'AUTO_CONTINUE' INTO l_table_line_elem.
    IF sy-subrc = 0.
      auto_continue = l_table_line_elem-value.
    ENDIF.

    CLEAR l_table_line_elem.
    READ TABLE l_table_line WITH KEY attribute = 'MOD_TIMESTAMP' INTO l_table_line_elem.
    IF sy-subrc = 0.
      mod_timestamp = l_table_line_elem-value.
    ENDIF.

    CLEAR l_table_line_elem.
    READ TABLE l_table_line WITH KEY attribute = 'EXEC_USER_ID' INTO l_table_line_elem.
    IF sy-subrc = 0.
      exec_user_id = l_table_line_elem-value.
    ENDIF.

    CLEAR l_table_line_elem.
    READ TABLE l_table_line WITH KEY attribute = 'JOB_NAME' INTO l_table_line_elem.
    IF sy-subrc = 0.
      job_name = l_table_line_elem-value.
    ENDIF.

    CLEAR l_table_line_elem.
    READ TABLE l_table_line WITH KEY attribute = 'JOB_COUNT' INTO l_table_line_elem.
    IF sy-subrc = 0.
      job_count = l_table_line_elem-value.
    ENDIF.

    CLEAR l_table_line_elem.
    READ TABLE l_table_line WITH KEY attribute = 'AUTO_EVENT' INTO l_table_line_elem.
    IF sy-subrc = 0.
      auto_event = l_table_line_elem-value.
    ENDIF.

    TRY.

        CLEAR l_table_line_elem.
        READ TABLE l_table_line WITH KEY attribute = 'TRANSLOG_SER' INTO l_table_line_elem.
        IF sy-subrc = 0 AND NOT l_table_line_elem-value IS INITIAL.
          CALL TRANSFORMATION id
               OPTIONS    value_handling = 'default'
               SOURCE XML l_table_line_elem-value
               RESULT     selection_table = transition_log. "#EC NOTEXT
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


    TRY.

        CLEAR l_table_line_elem.
        READ TABLE l_table_line WITH KEY attribute = 'ERRLOG_SER' INTO l_table_line_elem.
        IF sy-subrc = 0.
          CALL TRANSFORMATION id
               OPTIONS    value_handling = 'default'
               SOURCE XML l_table_line_elem-value
               RESULT     selection_table = error_log.      "#EC NOTEXT
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


  ENDMETHOD.                    "init_attrs_from_db


  METHOD is_auto_continued.

    FIELD-SYMBOLS <l_transition_log> LIKE LINE OF transition_log.

    CLEAR auto_continued.
    LOOP AT transition_log ASSIGNING <l_transition_log> WHERE source_state = 'S' AND target_state = 'W'.
      IF <l_transition_log>-auto_trans IS NOT INITIAL.
        auto_continued = 'X'.
      ELSE.
        CLEAR auto_continued.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_waiting_for_event.

    DATA:
      l_message    TYPE string,
      l_var1       TYPE string,
      l_trace_text TYPE string.


    CALL FUNCTION '/GAL/JS_IS_WAITING_FOR_EVENT'
      EXPORTING
        rfc_route_info              = store_rfc_route_info
        ignore_when_missing_predecs = ignore_when_missing_predecs
        job_id                      = id
      IMPORTING
        event_ids                   = event_ids
      EXCEPTIONS
        rfc_exception               = 1
        execution_failed            = 2
        OTHERS                      = 3.
    IF sy-subrc <> 0.
      l_var1 = id.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO l_message.
      /gal/trace=>write_error(
        EXPORTING
          no_flush = 'X'
      ).
      /gal/cfw_helper=>rfc_route_info_to_string(
        EXPORTING
          rfc_route_info = store_rfc_route_info
        RECEIVING
          string         = l_trace_text
      ).
      CONCATENATE 'Error occured on destination:' l_trace_text INTO l_trace_text SEPARATED BY space. "#EC NOTEXT
      /gal/trace=>write_text(
        EXPORTING
          text = l_trace_text
      ).
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_determine_events
          var1   = l_var1
          var2   = l_message.
    ENDIF.


  ENDMETHOD.                    "is_waiting_for_event


  METHOD post_process.

    DATA:
      l_message TYPE string,
      l_ex      TYPE REF TO /gal/cx_js_exception.

    enqueue( ).

* Set status to 'finished'
    IF NOT status = 'F'.
      change_status( new_status =  'F' ).
    ELSE.
      /gal/trace=>write_text(
        EXPORTING
          text = 'Not changing Job status from "F" to "F" (Bug?)' "#EC NOTEXT
      ).
    ENDIF.

* Write changes to database
    TRY.
        store_to_db( ).
      CATCH /gal/cx_js_exception INTO l_ex.
        dequeue( ).
        RAISE EXCEPTION l_ex.
    ENDTRY.

    CALL FUNCTION '/GAL/JS_UPDATE_RESOURCES'
      EXPORTING
        rfc_route_info = store_rfc_route_info
      EXCEPTIONS
        rfc_exception  = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO l_message.
      /gal/trace=>write_error( ).
      dequeue( ).
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_update_resources
          var1   = l_message.
    ENDIF.

    dequeue( ).


  ENDMETHOD.                    "post_process


  METHOD raise_user_event.

    DATA:
      l_var1            TYPE string,
      lt_table_line     TYPE /gal/db_datas,
      l_table_line_elem TYPE /gal/db_data,
      l_message         TYPE string.


    l_table_line_elem-attribute = 'ID'.
    l_table_line_elem-value = event_id.
    INSERT l_table_line_elem INTO TABLE lt_table_line.
    l_table_line_elem-attribute = 'STATUS'.
    l_table_line_elem-value = 'R'.
    INSERT l_table_line_elem INTO TABLE lt_table_line.


    CALL FUNCTION '/GAL/JS_DB_WRITE'
      EXPORTING
        rfc_route_info     = store_rfc_route_info
        table_name         = '/GAL/JOBDATA02U'
        table_line         = lt_table_line
        modify_only        = abap_true
      EXCEPTIONS
        rfc_exception      = 1
        wrong_content_data = 2
        cannot_write_to_db = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO l_message.
      l_var1 = event_id.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_raise_user_event
          var1   = l_var1
          var2   = l_message.
    ENDIF.

    update_preconditions( ).

    IF do_not_run_scheduler IS INITIAL.
      run_job_scheduler( ).
    ENDIF.

  ENDMETHOD.                    "raise_user_event


  METHOD read_job_from_db.

    DATA:
      l_table_line      TYPE /gal/db_datas,
      l_table_line_elem TYPE /gal/db_data,
      l_message         TYPE string,
      l_var1            TYPE string,
      l_type            TYPE /gal/job_type.


    IF id IS INITIAL.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_read_job_with_empty_id.
    ENDIF.


* At first we need to determine the type of the job to be created
    IF undelete_before_init = abap_false.
      CALL FUNCTION '/GAL/JS_DB_SELECT_SINGLE'
        EXPORTING
          rfc_route_info = store_rfc_route_info
          table_name     = '/GAL/JOBDATA01'
          id             = id
        IMPORTING
          table_line     = l_table_line
        EXCEPTIONS
          no_data_found  = 1
          unknown_table  = 2
          rfc_exception  = 3
          OTHERS         = 4.
      IF sy-subrc =  1.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO l_message.
        l_var1 = id.
        CALL FUNCTION '/GAL/JS_DB_SELECT_SINGLE'
          EXPORTING
            rfc_route_info = store_rfc_route_info
            table_name     = '/GAL/JD01_HIST'
            id             = id
          IMPORTING
            table_line     = l_table_line
          EXCEPTIONS
            no_data_found  = 1
            unknown_table  = 2
            rfc_exception  = 3
            OTHERS         = 4.
        IF sy-subrc = 1.
          RAISE EXCEPTION TYPE /gal/cx_js_no_job_data_found
            EXPORTING
              textid = /gal/cx_js_no_job_data_found=>/gal/cx_js_no_job_data_found
              var1   = l_var1
              var2   = '/GAL/JOBDATA01'.
        ELSEIF sy-subrc <> 0.
          RAISE EXCEPTION TYPE /gal/cx_js_exception
            EXPORTING
              textid = /gal/cx_js_exception=>cannot_read_job_from_db
              var1   = l_var1
              var2   = '/GAL/JOBDATA01'
              var3   = l_message.
        ENDIF.
      ELSEIF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO l_message.
        l_var1 = id.
        RAISE EXCEPTION TYPE /gal/cx_js_exception
          EXPORTING
            textid = /gal/cx_js_exception=>cannot_read_job_from_db
            var1   = l_var1
            var2   = '/GAL/JOBDATA01'
            var3   = l_message.
      ENDIF.
    ELSE.
      CALL FUNCTION '/GAL/JS_DB_SELECT_SINGLE'
        EXPORTING
          rfc_route_info = store_rfc_route_info
          table_name     = '/GAL/JD01_HIST'
          id             = id
        IMPORTING
          table_line     = l_table_line
        EXCEPTIONS
          no_data_found  = 1
          unknown_table  = 2
          rfc_exception  = 3
          OTHERS         = 4.
      IF sy-subrc = 1.
        RAISE EXCEPTION TYPE /gal/cx_js_no_job_data_found
          EXPORTING
            textid = /gal/cx_js_no_job_data_found=>/gal/cx_js_no_job_data_found
            var1   = l_var1
            var2   = '/GAL/JD01_HIST'.
      ELSEIF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO l_message.
        l_var1 = id.
        RAISE EXCEPTION TYPE /gal/cx_js_exception
          EXPORTING
            textid = /gal/cx_js_exception=>cannot_read_job_from_db
            var1   = l_var1
            var2   = '/GAL/JD01_HIST'
            var3   = l_message.
      ENDIF.
    ENDIF.

    IF l_table_line IS INITIAL.
      l_var1 = id.
      RAISE EXCEPTION TYPE /gal/cx_js_no_job_data_found
        EXPORTING
          textid = /gal/cx_js_no_job_data_found=>/gal/cx_js_no_job_data_found
          var1   = l_var1.
    ENDIF.

    READ TABLE l_table_line WITH KEY attribute = 'TYPE' INTO l_table_line_elem.
    l_type = l_table_line_elem-value.

    CASE l_type.
*   Job is a Sap job (report)
      WHEN 'S'.
        /gal/job_sap=>read_job_from_db_sap(
          EXPORTING
            undelete_before_init = undelete_before_init
            id                   = id
          RECEIVING
            job                  = job
        ).
*   Job is a (CCM) import job
      WHEN 'I'.
        CALL METHOD ('/GAL/CCM_JOB_IMPORT')=>read_job_from_db_ic
          EXPORTING
            undelete_before_init = undelete_before_init
            id                   = id
          RECEIVING
            job                  = job.
    ENDCASE.

    IF NOT enqueue IS INITIAL.
      job->enqueue( ).
    ENDIF.

  ENDMETHOD.                    "read_job_from_db


  METHOD release.

    DATA l_var1      TYPE string.


    IF NOT status = 'I'.
* Only job with status 'initial' can be released
      l_var1 = id.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>job_not_initial
          var1   = l_var1.
    ENDIF.

    IF no_commit IS INITIAL.
      enqueue( ).
      IF NOT status = 'I'.
        dequeue( ).
* Only job with status 'initial' can be released
        l_var1 = id.
        RAISE EXCEPTION TYPE /gal/cx_js_exception
          EXPORTING
            textid = /gal/cx_js_exception=>job_not_initial
            var1   = l_var1.
      ENDIF.
    ENDIF.

* set status to 'waiting'
    change_status( new_status =  'W' ).

    CONCATENATE sy-sysid '.' sy-mandt '.' sy-uname INTO exec_user_id.

* store changes to database
    IF no_commit IS INITIAL.
      store_to_db( ).

      dequeue( ).
    ENDIF.

  ENDMETHOD.                    "release


  METHOD restart.

    DATA:
      l_var1            TYPE string,
      l_ex              TYPE REF TO /gal/cx_js_exception,
      l_message_struct  TYPE /gal/st_message_struct_ts.


    IF NOT status = 'E'.
*   Job is not in status 'error' => error
      l_var1 = id.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>job_not_error
          var1   = l_var1.
    ENDIF.


    enqueue( ).

    IF NOT status = 'E'.
*   Job is not in status 'error' => error
      dequeue( ).
      l_var1 = id.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>job_not_error
          var1   = l_var1.
    ENDIF.


    DO 0 TIMES. MESSAGE i010. ENDDO.
    GET TIME STAMP FIELD l_message_struct-timestamp.
    l_message_struct-message_id = '/GAL/JS'.
    l_message_struct-message_type = 'I'.
    l_message_struct-message_number = '010'.
    APPEND l_message_struct TO error_log.

    change_status( new_status =  'W' ).

    CONCATENATE sy-sysid '.' sy-mandt '.' sy-uname INTO exec_user_id.

    TRY.
        store_to_db( ).
      CATCH /gal/cx_js_exception INTO l_ex.
        dequeue( ).
        RAISE EXCEPTION l_ex.
    ENDTRY.

    dequeue( ).

    run_job_scheduler( ).

  ENDMETHOD.                    "restart


  METHOD resume.

    DATA:
      l_var1            TYPE string,
      l_ex              TYPE REF TO /gal/cx_js_exception,
      l_message_struct  TYPE /gal/st_message_struct_ts.


    IF NOT status = 'S'.
*   Job is not in status 'stopped' => error
      l_var1 = id.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>job_not_stopped
          var1   = l_var1.
    ENDIF.

    enqueue( ).

    IF NOT status = 'S'.
*   Job is not in status 'stopped' => error
      dequeue( ).
      l_var1 = id.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>job_not_stopped
          var1   = l_var1.
    ENDIF.

    DO 0 TIMES. MESSAGE i023. ENDDO.
    GET TIME STAMP FIELD l_message_struct-timestamp.
    l_message_struct-message_id = '/GAL/JS'.
    l_message_struct-message_type = 'I'.
    l_message_struct-message_number = '023'.
    APPEND l_message_struct TO error_log.

    change_status(
      EXPORTING
        new_status      =  'W'
        auto_transition = auto_continued
    ).


    " AUTO_CONTINUE Flag gilt nur für ein RESUME
    CLEAR auto_continue.

    CONCATENATE sy-sysid '.' sy-mandt '.' sy-uname INTO exec_user_id.

    wait_for_res = 'X'.

    TRY.
        store_to_db( ).
      CATCH /gal/cx_js_exception INTO l_ex.
        dequeue( ).
        RAISE EXCEPTION l_ex.
    ENDTRY.

    dequeue( ).

    IF NOT skip_job_scheduler IS INITIAL.
      RETURN.
    ENDIF.

    run_job_scheduler( ).

  ENDMETHOD.                    "resume


  METHOD run_job_scheduler.

    DATA:
      l_message              TYPE string.


* run job scheduler on central system
    CALL FUNCTION '/GAL/JS_RUN_SCHEDULER'
      EXPORTING
        rfc_route_info       = store_rfc_route_info
        retry_times          = retry_times
        retry_wait_interval  = retry_wait_interval
        in_background        = in_background
      IMPORTING
        messages             = messages
      EXCEPTIONS
        rfc_exception        = 1
        cannot_run_scheduler = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO l_message.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_run_scheduler
          var1   = l_message.
    ENDIF.



  ENDMETHOD.                    "run_job_scheduler


METHOD set_jobdata.

  DATA:
    l_ex       TYPE REF TO /gal/cx_js_exception.


  enqueue( ).

  me->job_name = job_name.
  me->job_count = job_count.

  TRY.
      store_to_db( ).

    CATCH /gal/cx_js_exception INTO l_ex.    " Exception from Job Scheduler
      dequeue( ).
      RAISE EXCEPTION l_ex.
  ENDTRY.

  dequeue( ).

ENDMETHOD.


  METHOD set_status_to_error.

    DATA:
      l_message_struct  TYPE /gal/st_message_struct_ts,
      l_ex              TYPE REF TO /gal/cx_js_exception.

    TRY.
        enqueue( ).
      CATCH /gal/cx_js_exception INTO l_ex.
        /gal/trace=>write_exception(
          EXPORTING
            exception               = l_ex
        ).
        " We are already within error handling. Throwing more exceptions would make things nasty.
        RETURN.
    ENDTRY.

    TRY.
        GET TIME STAMP FIELD l_message_struct-timestamp.
        l_message_struct-message_id = symsgid.
        l_message_struct-message_type = symsgty.
        l_message_struct-message_number = symsgno.
        l_message_struct-message_var1 = symsgv1.
        l_message_struct-message_var2 = symsgv2.
        l_message_struct-message_var3 = symsgv3.
        l_message_struct-message_var4 = symsgv4.
        APPEND l_message_struct TO error_log.

* Set status to 'error'
        change_status( new_status =  'E' ).

* Write changes to database

        store_to_db( ).


        CALL FUNCTION '/GAL/JS_UPDATE_RESOURCES'
          EXPORTING
            rfc_route_info = store_rfc_route_info
          EXCEPTIONS
            OTHERS         = 0.
        " We are already within error handling. Throwing more exceptions would make things nasty.

      CATCH /gal/cx_js_exception INTO l_ex.
        /gal/trace=>write_exception(
          EXPORTING
            exception               = l_ex
        ).

    ENDTRY.
    TRY.
        dequeue( ).
      CATCH /gal/cx_js_exception INTO l_ex.
        /gal/trace=>write_exception(
          EXPORTING
            exception               = l_ex
        ).
    ENDTRY.

  ENDMETHOD.                    "set_status_to_error


METHOD set_status_to_obsolete.

  DATA:
    l_ex TYPE REF TO /gal/cx_js_exception.


  enqueue( ).

  change_status(
    EXPORTING
      new_status = 'O'
  ).

  TRY.
      store_to_db( ).
    CATCH /gal/cx_js_exception INTO l_ex.
      dequeue( ).
      RAISE EXCEPTION l_ex.
  ENDTRY.

  dequeue( ).

ENDMETHOD.


  METHOD store_to_db.

    DATA:
      l_var1            TYPE string,
      lt_table_line     TYPE /gal/db_datas,
      l_table_line_elem TYPE /gal/db_data,
      l_message         TYPE string,
      l_timestamp       TYPE timestamp,
      l_xml_ex          TYPE REF TO cx_transformation_error.


    IF read_from_hist = abap_true.
      l_var1 = id.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>job_read_only
          var1   = l_var1.
    ENDIF.

    IF enqueue_counter = 0.
      l_var1 = id.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_write_unlocked_object
          var1   = l_var1.
    ENDIF.


* Build table with all attributes of class in order to save them to DB
    l_table_line_elem-attribute = 'ID'.
    l_table_line_elem-value = id.
    INSERT l_table_line_elem INTO TABLE lt_table_line.

    l_table_line_elem-attribute = 'TYPE'.
    l_table_line_elem-value = type.
    INSERT l_table_line_elem INTO TABLE lt_table_line.

    l_table_line_elem-attribute = 'DESTINATION'.
    l_table_line_elem-value = destination.
    INSERT l_table_line_elem INTO TABLE lt_table_line.

    l_table_line_elem-attribute = 'STATUS'.
    l_table_line_elem-value = status.
    INSERT l_table_line_elem INTO TABLE lt_table_line.

    l_table_line_elem-attribute = 'WAIT_FOR_RES'.
    l_table_line_elem-value = wait_for_res.
    INSERT l_table_line_elem INTO TABLE lt_table_line.

    l_table_line_elem-attribute = 'AUTO_CONTINUE'.
    l_table_line_elem-value = auto_continue.
    INSERT l_table_line_elem INTO TABLE lt_table_line.

    l_table_line_elem-attribute = 'MOD_TIMESTAMP'.
    GET TIME STAMP FIELD  l_timestamp.
    l_table_line_elem-value = l_timestamp.
    INSERT l_table_line_elem INTO TABLE lt_table_line.

    l_table_line_elem-attribute = 'EXEC_USER_ID'.
    l_table_line_elem-value = exec_user_id.
    INSERT l_table_line_elem INTO TABLE lt_table_line.

    l_table_line_elem-attribute = 'JOB_NAME'.
    l_table_line_elem-value = job_name.
    INSERT l_table_line_elem INTO TABLE lt_table_line.

    l_table_line_elem-attribute = 'JOB_COUNT'.
    l_table_line_elem-value = job_count.
    INSERT l_table_line_elem INTO TABLE lt_table_line.

    l_table_line_elem-attribute = 'AUTO_EVENT'.
    l_table_line_elem-value = auto_event.
    INSERT l_table_line_elem INTO TABLE lt_table_line.

    TRY.
        l_table_line_elem-attribute = 'TRANSLOG_SER'.
        CALL TRANSFORMATION id
           OPTIONS    data_refs          = 'heap-or-create'
                      initial_components = 'include'
                      technical_types    = 'error'
                      value_handling     = 'default'
                      xml_header         = 'full'
           SOURCE     selection_table    = transition_log
           RESULT XML l_table_line_elem-value.              "#EC NOTEXT
        INSERT l_table_line_elem INTO TABLE lt_table_line.

      CATCH cx_transformation_error INTO l_xml_ex.
        CALL METHOD l_xml_ex->if_message~get_text
          RECEIVING
            result = l_var1.
        RAISE EXCEPTION TYPE /gal/cx_js_exception
          EXPORTING
            textid = /gal/cx_js_exception=>error_creating_xml
            var1   = l_var1.
    ENDTRY.


    TRY.
        l_table_line_elem-attribute = 'ERRLOG_SER'.
        CALL TRANSFORMATION id
           OPTIONS    data_refs          = 'heap-or-create'
                      initial_components = 'include'
                      technical_types    = 'error'
                      value_handling     = 'default'
                      xml_header         = 'full'
           SOURCE     selection_table    = error_log
           RESULT XML l_table_line_elem-value.              "#EC NOTEXT
        INSERT l_table_line_elem INTO TABLE lt_table_line.

      CATCH cx_transformation_error INTO l_xml_ex.
        CALL METHOD l_xml_ex->if_message~get_text
          RECEIVING
            result = l_var1.
        RAISE EXCEPTION TYPE /gal/cx_js_exception
          EXPORTING
            textid = /gal/cx_js_exception=>error_creating_xml
            var1   = l_var1.
    ENDTRY.

    CALL FUNCTION '/GAL/JS_DB_WRITE'
      EXPORTING
        rfc_route_info     = store_rfc_route_info
        table_name         = '/GAL/JOBDATA01'
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
          var2   = '/GAL/JOBDATA01'
          var3   = l_message.
    ENDIF.


  ENDMETHOD.                    "store_to_db


  METHOD update_preconditions.

    DATA:
      l_message TYPE string.


    CALL FUNCTION '/GAL/JS_UPDATE_PRECONDITIONS'
      EXPORTING
        rfc_route_info   = store_rfc_route_info
      EXCEPTIONS
        execution_failed = 1
        rfc_exception    = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO l_message.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_update_precondition
          var1   = '*'
          var2   = l_message.
    ENDIF.

  ENDMETHOD.                    "update_preconditions
ENDCLASS.
