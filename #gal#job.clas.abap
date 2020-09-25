*----------------------------------------------------------------------*
*       CLASS /GAL/JOB DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
"! <p class="shorttext synchronized" lang="en">Common job (to be redefined)</p>
CLASS /gal/job DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

*"* public components of class /GAL/JOB
*"* do not include other source files here!!!
*"* protected components of class /GAL/JOB
*"* do not include other source files here!!!
  PUBLIC SECTION.
    TYPE-POOLS abap .

    "! <p class="shorttext synchronized" lang="en">Version of storage type</p>
    CLASS-DATA db_layer_version     TYPE /gal/js_db_layer_version .
    "! <p class="shorttext synchronized" lang="en">RFC Route information</p>
    CLASS-DATA store_rfc_route_info TYPE /gal/rfc_route_info READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Automatically continue job after stop</p>
    DATA auto_continue  TYPE flag READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Automatically raise user events</p>
    DATA auto_event     TYPE flag .
    "! <p class="shorttext synchronized" lang="en">Job Class Name</p>
    DATA classname      TYPE classname READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">RFC Route from central system to execution target</p>
    DATA destination    TYPE string READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Table with messages and timestamp</p>
    DATA error_log      TYPE /gal/tt_message_struct_ts READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">User ID in Format sy-sysid.sy-mandt.sy-uname</p>
    DATA exec_user_id   TYPE /gal/user_id READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Job ID</p>
    DATA id             TYPE /gal/job_id READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Job ID</p>
    DATA job_count      TYPE btcjobcnt READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Background job name</p>
    DATA job_name       TYPE btcjob READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">UTC Time Stamp in Short Form (YYYYMMDDhhmmss)</p>
    DATA mod_timestamp  TYPE timestamp READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">DO NOT CHANGE DIRECTLY! -&gt; Method CHANGE_STATUS (Job status)</p>
    DATA status         TYPE /gal/job_status READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Job transition log</p>
    DATA transition_log TYPE /gal/transition_log READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Job is waiting to be resumed</p>
    DATA wait_for_res   TYPE flag READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Enabled UC4 mode status</p>
    DATA uc4_mode       TYPE /gal/uc4_mode READ-ONLY.

    "! <p class="shorttext synchronized" lang="en">Klassenkonstruktor</p>
    CLASS-METHODS class_constructor .
    "! <p class="shorttext synchronized" lang="en">CLASS_INIT_EXCEPTION holen</p>
    "!
    "! @parameter init_exception | <p class="shorttext synchronized" lang="en">Abstract Superclass for All Global Exceptions</p>
    CLASS-METHODS class_get_init_exception
      RETURNING
        VALUE(init_exception) TYPE REF TO cx_root .
    "! <p class="shorttext synchronized" lang="en">Determine RFC Destination to storage location</p>
    "!
    "! @parameter store_destination    | <p class="shorttext synchronized" lang="en">RFC Destination</p>
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    CLASS-METHODS determine_store_destination
      RETURNING
        VALUE(store_destination) TYPE /gal/rfc_destination
      RAISING
        /gal/cx_js_exception .
    CLASS-METHODS get_jobtype_description
      IMPORTING
        !classname   TYPE classname
      EXPORTING
        !description TYPE string .
    "! <p class="shorttext synchronized" lang="en">Raise an user event</p>
    "!
    "! @parameter event_id             | <p class="shorttext synchronized" lang="en">Job precondition: ID</p>
    "! @parameter do_not_run_scheduler | <p class="shorttext synchronized" lang="en">Flag: do not run job scheduler</p>
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    CLASS-METHODS raise_user_event
      IMPORTING
        !event_id             TYPE /gal/precondition_id
        !do_not_run_scheduler TYPE flag OPTIONAL
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Read a job (any kind) from DB</p>
    "!
    "! @parameter id                   | <p class="shorttext synchronized" lang="en">Job ID</p>
    "! @parameter enqueue              | <p class="shorttext synchronized" lang="en">Enqueue the job</p>
    "! @parameter undelete_before_init | <p class="shorttext synchronized" lang="en">Perform an Undelete before the init</p>
    "! @parameter job                  | <p class="shorttext synchronized" lang="en">Common job (to be redefined)</p>
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    CLASS-METHODS read_job_from_db
      IMPORTING
        !id                   TYPE /gal/job_id
        !enqueue              TYPE flag OPTIONAL
        !undelete_before_init TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(job)            TYPE REF TO /gal/job
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Run the job scheduler on central system</p>
    "!
    "! @parameter retry_times          | <p class="shorttext synchronized" lang="en">Times to retry</p>
    "! @parameter retry_wait_interval  | <p class="shorttext synchronized" lang="en">Wait between retries in second</p>
    "! @parameter in_background        | <p class="shorttext synchronized" lang="en">Run job scheduler in background</p>
    "! @parameter messages             | <p class="shorttext synchronized" lang="en">Table with messages</p>
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    CLASS-METHODS run_job_scheduler
      IMPORTING
        !retry_times         TYPE int4 DEFAULT 3
        !retry_wait_interval TYPE int4 DEFAULT 5
        !in_background       TYPE flag OPTIONAL
      EXPORTING
        !messages            TYPE /gal/tt_message_struct
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Update the precondition status</p>
    "!
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    CLASS-METHODS update_preconditions
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Add a resource string specifying a needed resource</p>
    "!
    "! @parameter resource_string      | <p class="shorttext synchronized" lang="en">String specifying a resource</p>
    "! @parameter status               | <p class="shorttext synchronized" lang="en">Status: Fulfilled or Not fulfilled (Resource free =&gt; F)</p>
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS add_needed_resource
      IMPORTING
        !resource_string TYPE /gal/resource_string
      EXPORTING
        !status          TYPE /gal/precondition_status
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Add a predecessor job as precondition</p>
    "!
    "! @parameter job                  | <p class="shorttext synchronized" lang="en">Common job (to be redefined)</p>
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS add_predecessor_job
      IMPORTING
        !job TYPE REF TO /gal/job
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Add a timestamp to be reached as precondition</p>
    "!
    "! @parameter timestamp            | <p class="shorttext synchronized" lang="en">UTC Time Stamp in Short Form (YYYYMMDDhhmmss)</p>
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS add_start_timestamp
      IMPORTING
        !timestamp TYPE timestamp
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Add a user event as precondition</p>
    "!
    "! @parameter event_id             | <p class="shorttext synchronized" lang="en">Job precondition: ID</p>
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS add_user_event
      RETURNING
        VALUE(event_id) TYPE /gal/precondition_id
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Cancel the job's background processing</p>
    "!
    "! @parameter mutex_name             | <p class="shorttext synchronized" lang="en">Name of mutex to be aquired</p>
    "! @raising   /gal/cx_js_exception   | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    "! @raising   /gal/cx_lock_exception | <p class="shorttext synchronized" lang="en">Lock Management Exception</p>
    METHODS cancel
      IMPORTING
        !mutex_name TYPE string OPTIONAL
      RAISING
        /gal/cx_js_exception
        /gal/cx_lock_exception .
    "! <p class="shorttext synchronized" lang="en">Delete Job from database</p>
    "!
    "! @parameter force                | <p class="shorttext synchronized" lang="en">Force deletion despite wrong state</p>
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS delete_from_db
      IMPORTING
        !force TYPE abap_bool DEFAULT abap_false
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Dequeue the job object (DB lock)</p>
    "!
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS dequeue
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Enqueue the job object (DB lock)</p>
    "!
    "! @parameter refresh                   | <p class="shorttext synchronized" lang="en">Refresh the object from DB</p>
    "! @raising   /gal/cx_js_cannot_enqueue | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    "! @raising   /gal/cx_js_exception      | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS enqueue
      IMPORTING
        !refresh TYPE flag DEFAULT 'X'
      RAISING
        /gal/cx_js_cannot_enqueue
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Execute the job</p>
    "!
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS execute
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">INTERNAL USE ONLY: Execute the asynchronous part</p>
    "!
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS execute_async
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Get the predecessor jobs</p>
    "!
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS get_predecessor_jobs
      RETURNING
        VALUE(predecessor_jobs) TYPE /gal/jobs
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Get name of program to be executed as a job</p>
    "!
    "! @parameter program_name | <p class="shorttext synchronized" lang="en">Program name</p>
    METHODS get_program_name
      RETURNING
        VALUE(program_name) TYPE syrepid .
    "! <p class="shorttext synchronized" lang="en">Check whether the job was auto continued</p>
    "!
    "! @parameter auto_continued | <p class="shorttext synchronized" lang="en">Flag whether the job was auto continued</p>
    METHODS is_auto_continued
      RETURNING
        VALUE(auto_continued) TYPE flag .
    "! <p class="shorttext synchronized" lang="en">Determine if job can be resumed</p>
    "!
    "! @parameter restartable | <p class="shorttext synchronized" lang="en">Job can be restarted</p>
    METHODS is_restartable
      RETURNING
        VALUE(restartable) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Determine if job can be resumed</p>
    "!
    "! @parameter resumable | <p class="shorttext synchronized" lang="en">Job can be resumed</p>
    METHODS is_resumeable
      RETURNING
        VALUE(resumable) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Check whether job is waiting for event[s]</p>
    "!
    "! @parameter ignore_when_missing_predecs | <p class="shorttext synchronized" lang="en">Ignore events, if there are missing predecessor jobs</p>
    "! @parameter event_ids                   | <p class="shorttext synchronized" lang="en">Job precondition: Table with IDs</p>
    "! @raising   /gal/cx_js_exception        | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS is_waiting_for_event
      IMPORTING
        !ignore_when_missing_predecs TYPE flag OPTIONAL
      RETURNING
        VALUE(event_ids)             TYPE /gal/precondition_ids
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Post processing after job has been run</p>
    "!
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS post_process
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Release the job to status 'Waiting'</p>
    "!
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS release
      IMPORTING
        !no_commit TYPE flag OPTIONAL
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Resume stopped Job</p>
    "!
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS restart
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Resume stopped Job</p>
    "!
    "! @parameter skip_job_scheduler   | <p class="shorttext synchronized" lang="en">Job Scheduler nicht ausführen</p>
    "! @parameter auto_continued       | <p class="shorttext synchronized" lang="en">Es handelt sich um eine automatische Fortsetzung</p>
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS resume
      IMPORTING
        !skip_job_scheduler TYPE flag OPTIONAL
        !auto_continued     TYPE flag OPTIONAL
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">INTERNAL USE ONLY: Set JOBNAME and JOBCOUNT</p>
    "!
    "! @parameter job_name             | <p class="shorttext synchronized" lang="en">Background job name</p>
    "! @parameter job_count            | <p class="shorttext synchronized" lang="en">Job ID</p>
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS set_jobdata
      IMPORTING
        !job_name  TYPE btcjob
        !job_count TYPE btcjobcnt
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Post processing after job has been run</p>
    "!
    "! @parameter symsgid | <p class="shorttext synchronized" lang="en">Message Class</p>
    "! @parameter symsgty | <p class="shorttext synchronized" lang="en">Message Type</p>
    "! @parameter symsgno | <p class="shorttext synchronized" lang="en">Message Number</p>
    "! @parameter symsgv1 | <p class="shorttext synchronized" lang="en">Message Variable</p>
    "! @parameter symsgv2 | <p class="shorttext synchronized" lang="en">Message Variable</p>
    "! @parameter symsgv3 | <p class="shorttext synchronized" lang="en">Message Variable</p>
    "! @parameter symsgv4 | <p class="shorttext synchronized" lang="en">Message Variable</p>
    METHODS set_status_to_error
      IMPORTING
        VALUE(symsgid) TYPE symsgid DEFAULT sy-msgid
        VALUE(symsgty) TYPE symsgty DEFAULT sy-msgty
        VALUE(symsgno) TYPE symsgno DEFAULT sy-msgno
        VALUE(symsgv1) TYPE symsgv DEFAULT sy-msgv1
        VALUE(symsgv2) TYPE symsgv DEFAULT sy-msgv2
        VALUE(symsgv3) TYPE symsgv DEFAULT sy-msgv3
        VALUE(symsgv4) TYPE symsgv DEFAULT sy-msgv4
          PREFERRED PARAMETER symsgid .
    "! <p class="shorttext synchronized" lang="en">Cancel the job</p>
    "!
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS set_status_to_obsolete
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Store Job to database</p>
    "!
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS store_to_db
      RAISING
        /gal/cx_js_exception .
  PROTECTED SECTION.

    "! <p class="shorttext synchronized" lang="en">Configuration provider for job scheduler</p>
    CLASS-DATA config TYPE REF TO /gal/js_config_provider .
    "! <p class="shorttext synchronized" lang="en">Perform tracing</p>
    CLASS-DATA trace TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">The enqueue counter</p>
    DATA enqueue_counter TYPE i .
    "! <p class="shorttext synchronized" lang="en">Read only mode (read from history)</p>
    DATA read_from_hist TYPE abap_bool .

    "! <p class="shorttext synchronized" lang="en">Change the job status</p>
    "!
    "! @parameter new_status      | <p class="shorttext synchronized" lang="en">Job status</p>
    "! @parameter stop_options    | <p class="shorttext synchronized" lang="en">Options for allowed behavior after stopping a job</p>
    "! @parameter auto_transition | <p class="shorttext synchronized" lang="en">Es handelt sich um eine automatische Transition</p>
    METHODS change_status
      IMPORTING
        !new_status      TYPE /gal/job_status
        !stop_options    TYPE /gal/job_stop_options OPTIONAL
        !auto_transition TYPE flag OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Init Attributes from parameters</p>
    "!
    "! @parameter job_name             | <p class="shorttext synchronized" lang="en">Job name</p>
    "! @parameter destination          | <p class="shorttext synchronized" lang="en">RFC Route from central system to execution target</p>
    "! @parameter auto_continue        | <p class="shorttext synchronized" lang="en">Automatically continue after Stop</p>
    "! @parameter auto_event           | <p class="shorttext synchronized" lang="en">Automatically raise user events</p>
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS init_attrs_create
      IMPORTING
        !job_name      TYPE btcjob DEFAULT 'BACKGROUND_JOB'
        !destination   TYPE string OPTIONAL
        !auto_continue TYPE abap_bool DEFAULT abap_false
        !auto_event    TYPE abap_bool DEFAULT abap_false
        !uc4_mode      TYPE /gal/uc4_mode DEFAULT space
      RAISING
        /gal/cx_js_exception .
    "! <p class="shorttext synchronized" lang="en">Init Attributes from database</p>
    "!
    "! @parameter id                   | <p class="shorttext synchronized" lang="en">Job ID</p>
    "! @parameter undelete_before_init | <p class="shorttext synchronized" lang="en">Perform an Undelete before the init</p>
    "! @raising   /gal/cx_js_exception | <p class="shorttext synchronized" lang="en">Exception from Job Scheduler</p>
    METHODS init_attrs_from_db
      IMPORTING
        !id                   TYPE /gal/job_id
        !undelete_before_init TYPE abap_bool DEFAULT abap_false
      RAISING
        /gal/cx_js_exception .
*"* private components of class /GAL/JOB
*"* do not include other source files here!!!
  PRIVATE SECTION.

    "! <p class="shorttext synchronized" lang="en">Exception bei Initialisierung im Klassenkonstruktor</p>
    CLASS-DATA class_init_exception TYPE REF TO cx_root .
ENDCLASS.



CLASS /gal/job IMPLEMENTATION.


  METHOD add_needed_resource.

    DATA:
      l_var1    TYPE string,
      l_var2    TYPE string,
      l_message TYPE string.

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
      l_var1    TYPE string,
      l_var2    TYPE string,
      l_message TYPE string.


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
      l_var1    TYPE string,
      l_var2    TYPE string,
      l_message TYPE string.


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
      l_var1    TYPE string,
      l_message TYPE string.


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
          ).                                                "#EC NOTEXT
          l_config_folder->get_value( IMPORTING value = l_lock_timeout ).

        CATCH /gal/cx_config_exception.
          l_lock_timeout = 30.                           "#EC NUMBER_OK
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
      l_trans_log_entry TYPE /gal/transition_log_entry,
      l_stop_options    TYPE /gal/job_stop_options,
      l_option          TYPE /gal/attrib_value.


    IF new_status = 'S'.
      IF stop_options IS NOT SUPPLIED.
        l_stop_options-allow_continue = abap_true.
        l_stop_options-allow_restart  = abap_false.
      ELSE.
        l_stop_options = stop_options.
      ENDIF.
      l_option-attribute = 'ALLOW_CONTINUE'.
      l_option-value     = l_stop_options-allow_continue.
      INSERT l_option INTO TABLE l_trans_log_entry-options.
      l_option-attribute = 'ALLOW_RESTART'.
      l_option-value     = l_stop_options-allow_restart.
      INSERT l_option INTO TABLE l_trans_log_entry-options.
    ENDIF.

    l_trans_log_entry-source_state = status.
    GET TIME STAMP FIELD l_trans_log_entry-timestamp.
    l_trans_log_entry-syuname = sy-uname.

    status = new_status.
    l_trans_log_entry-target_state = status.

    l_trans_log_entry-auto_trans = auto_transition.

    INSERT l_trans_log_entry INTO TABLE transition_log.

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
        ).                                                  "#EC NOTEXT
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
      l_message   TYPE string,
      l_var1      TYPE string,
      l_var2      TYPE string,
      l_key_value TYPE string.


    IF force = abap_false AND NOT status CA 'OF'.
      l_var1 = id.
      l_var2 = TEXT-002.
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
      l_var1      TYPE string,
      l_var2      TYPE string,
      l_callstack TYPE abap_callstack,
      l_level     TYPE i.

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
      l_var1      TYPE string,
      l_var2      TYPE string,
      l_callstack TYPE abap_callstack.

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
        IF status <> 'W' OR uc4_mode = 'W'.
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

        IF uc4_mode = space.
          change_status( new_status = 'R' ).
        ELSE.
          uc4_mode = 'W'.
        ENDIF.

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
        release_sap_job_only = uc4_mode
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

* Set status to 'running'and update job

    IF uc4_mode = 'X' OR uc4_mode = 'W'.
      change_status( new_status =  'R' ).
      me->uc4_mode = 'X'.
      store_to_db( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_jobtype_description.

    CLEAR description.

    TRY.
        CALL METHOD (classname)=>get_jobtype_descr_jobspec
          EXPORTING
            classname   = classname
          IMPORTING
            description = description.
      CATCH cx_sy_dyn_call_error.
        description = classname.
    ENDTRY.

  ENDMETHOD.


  METHOD get_predecessor_jobs.

    DATA:
      l_var1            TYPE string,
      l_message         TYPE string,
      lt_predec_job_ids TYPE /gal/tt_job_ids,
      l_job             TYPE REF TO /gal/job.

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
    me->uc4_mode      = uc4_mode.

* Jobs werden immer im Status 'I' angelegt
    status = 'I'.

* Aktueller Zeitstempel als Modifikationszeitpunkt
    GET TIME STAMP FIELD mod_timestamp.
  ENDMETHOD.                    "init_attrs_create


  METHOD init_attrs_from_db.

    DATA:
      l_var1             TYPE string,
      l_table_line       TYPE /gal/db_datas,
      l_table_line_elem  TYPE /gal/db_data,
      l_message          TYPE string,
      l_xml_ex           TYPE REF TO cx_transformation_error,
      l_key_value        TYPE string,
      l_must_be_obsolete TYPE abap_bool.

    cfw_break_point_support.
    cfw_break_point `/GAL/JOB=>INIT_ATTRS_FROM_DB`.

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

    CLEAR l_table_line_elem.
    READ TABLE l_table_line WITH KEY attribute = 'UC4_MODE' INTO l_table_line_elem.
    IF sy-subrc = 0.
      uc4_mode = l_table_line_elem-value.
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


  METHOD is_restartable.

    FIELD-SYMBOLS:
      <l_translog> TYPE /gal/transition_log_entry.


    IF status EQ 'E'.
      restartable = abap_true.
      RETURN.
    ENDIF.

    IF status NE 'S'.
      restartable = abap_false.
      RETURN.
    ENDIF.

    restartable = abap_false.
    DESCRIBE TABLE transition_log LINES sy-tfill.
    IF sy-tfill > 0.
      READ TABLE transition_log INDEX sy-tfill ASSIGNING <l_translog>.
      IF NOT <l_translog>-target_state = 'S'.
        RETURN.
      ENDIF.
      READ TABLE <l_translog>-options WITH KEY attribute = 'ALLOW_RESTART' value = abap_true TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        restartable = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD is_resumeable.

    FIELD-SYMBOLS:
      <l_translog> TYPE /gal/transition_log_entry.


    IF NOT status = 'S'.
      resumable = abap_false.
      RETURN.
    ENDIF.

    resumable = abap_true.
    DESCRIBE TABLE transition_log LINES sy-tfill.
    IF sy-tfill > 0.
      READ TABLE transition_log INDEX sy-tfill ASSIGNING <l_translog>.
      IF NOT <l_translog>-target_state = 'S'.
        RETURN.
      ENDIF.
      READ TABLE <l_translog>-options WITH KEY attribute = 'ALLOW_CONTINUE' value = abap_false TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        resumable = abap_false.
      ENDIF.
    ENDIF.

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
      l_table_line       TYPE /gal/db_datas,
      l_table_line_elem  TYPE /gal/db_data,
      l_message          TYPE string,
      l_var1             TYPE string,
      l_classname        TYPE classname,
      l_after_correction TYPE abap_bool.


    IF id IS INITIAL.
      RAISE EXCEPTION TYPE /gal/cx_js_exception
        EXPORTING
          textid = /gal/cx_js_exception=>cannot_read_job_with_empty_id.
    ENDIF.


    DO 2 TIMES.

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

      READ TABLE l_table_line WITH KEY attribute = 'CLASSNAME' INTO l_table_line_elem.
      l_classname = l_table_line_elem-value.
      IF l_classname IS INITIAL.

        IF l_after_correction = abap_false AND db_layer_version < '001'.
          CALL FUNCTION '/GAL/JS_FILL_JD_CLASSNAME'
            EXPORTING
              rfc_route_info             = store_rfc_route_info
              fill_current               = abap_true
              fill_hist                  = abap_true
            EXCEPTIONS
              rfc_exception              = 1
              unsupported_jobdat_version = 2
              OTHERS                     = 3.
          IF sy-subrc <> 0.
            /gal/trace=>write_error( ).
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                       INTO l_message.
            RAISE EXCEPTION TYPE /gal/cx_js_exception
              EXPORTING
                textid = /gal/cx_js_exception=>cannot_migrate_jobdata
                var1   = l_message.
          ENDIF.
          l_after_correction = abap_true.
          CONTINUE.
        ENDIF.

        IF l_classname IS INITIAL.
          l_var1 = id.
          RAISE EXCEPTION TYPE /gal/cx_js_exception
            EXPORTING
              textid = /gal/cx_js_exception=>cannot_determine_legacy_name
              var1   = l_var1.
        ENDIF.
      ENDIF.

      EXIT.

    ENDDO.

    CALL METHOD (l_classname)=>read_job_from_db_jobspec
      EXPORTING
        undelete_before_init = undelete_before_init
        id                   = id
      RECEIVING
        job                  = job.

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
      l_var1           TYPE string,
      l_ex             TYPE REF TO /gal/cx_js_exception,
      l_message_struct TYPE /gal/st_message_struct_ts.


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
      l_var1           TYPE string,
      l_ex             TYPE REF TO /gal/cx_js_exception,
      l_message_struct TYPE /gal/st_message_struct_ts.


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
      l_message_struct TYPE /gal/st_message_struct_ts,
      l_ex             TYPE REF TO /gal/cx_js_exception.

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

    l_table_line_elem-attribute = 'CLASSNAME'.
    l_table_line_elem-value = classname.
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

    l_table_line_elem-attribute = 'UC4_MODE'.
    l_table_line_elem-value = uc4_mode.
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
