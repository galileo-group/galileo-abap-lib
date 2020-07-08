class /GAL/JOB_SCHEDULER definition
  public
  final
  create public .

public section.

  class-methods RUN
    importing
      !RETRY_TIMES type INT4 default 3
      !RETRY_WAIT_INTERVAL type INT4 default 5
    exporting
      !MESSAGES type /GAL/TT_MESSAGE_STRUCT .
protected section.
private section.
ENDCLASS.



CLASS /GAL/JOB_SCHEDULER IMPLEMENTATION.


METHOD run.

**********************************************************************
* CAUTION: This method is only to be run on central system           *
**********************************************************************

  INCLUDE /gal/cfw_macros.                                 "#EC INCL_OK

  DATA:
    l_jobdata01    TYPE /gal/jobdata01,
    lt_jobdata01   TYPE TABLE OF /gal/jobdata01,
    l_job          TYPE REF TO /gal/job,
    l_ex           TYPE REF TO cx_root,
    l_skip_ids     TYPE STANDARD TABLE OF /gal/job_id,
    l_message      TYPE /gal/st_message_struct,
    l_message_text TYPE string,
    l_retry_count  TYPE i.


* Breakpoint support
  cfw_break_point_support.
  cfw_break_point `/GAL/JS_JOB_SCHEDULER`.


  DO 0 TIMES. MESSAGE i016. ENDDO.
  CLEAR l_message.
  l_message-message_id = '/GAL/JS'.
  l_message-message_type = 'I'.
  l_message-message_number = '016'.
  APPEND l_message TO messages.


  DO.
    " First a table lock is retrieved in order to make sure the job scheduler is running exclusively.
    " This is ensured since the scheduler may only run on the single central system.
    CALL FUNCTION 'ENQUEUE_/GAL/E_JS_LOCK'
      EXPORTING
        lock_key       = 'SCHEDULER'
        _scope         = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 1.
      /gal/trace=>write_error( ).
      IF l_retry_count <= retry_times.
        " Retry in order to avoid "lost" jobs due to bad timing
        l_retry_count = l_retry_count + 1.
        WAIT UP TO retry_wait_interval SECONDS.
        CONTINUE.
      ENDIF.
      DO 0 TIMES. MESSAGE i015. ENDDO.
      CLEAR l_message.
      l_message-message_id = '/GAL/JS'.
      l_message-message_type = 'I'.
      l_message-message_number = '015'.
      APPEND l_message TO messages.
      RETURN.
    ELSEIF sy-subrc <> 0.
      /gal/trace=>write_error( ).
      CLEAR l_message.
      l_message-message_id = sy-msgid.
      l_message-message_type = 'E'.
      l_message-message_number = sy-msgno.
      l_message-message_var1 = sy-msgv1.
      l_message-message_var2 = sy-msgv2.
      l_message-message_var3 = sy-msgv3.
      l_message-message_var4 = sy-msgv4.
      APPEND l_message TO messages.
      RETURN.
    ENDIF.
    EXIT.
  ENDDO.

  CALL FUNCTION '/GAL/JS_CHECK_RUNNING_JOBS'
    EXPORTING
      do_not_run_scheduler = abap_true
    EXCEPTIONS
      rfc_exception        = 1
      check_failed         = 2
      OTHERS               = 3.
  IF sy-subrc <> 0.
    CLEAR l_message.
    l_message-message_id = sy-msgid.
    l_message-message_type = 'W'.
    l_message-message_number = sy-msgno.
    l_message-message_var1 = sy-msgv1.
    l_message-message_var2 = sy-msgv2.
    l_message-message_var3 = sy-msgv3.
    l_message-message_var4 = sy-msgv4.
    APPEND l_message TO messages.
* Continueing anyhow, since this is no fatal error.
  ENDIF.

* Cleanup orphaned jobs, i.e. set them to "error" if the background job has been lost.
  CALL FUNCTION '/GAL/JS_HANDLE_ORPHANED_JOBS'
    EXCEPTIONS
      rfc_exception    = 1
      execution_failed = 2
      OTHERS           = 3.
  IF sy-subrc <> 0.
    CLEAR l_message.
    l_message-message_id = sy-msgid.
    l_message-message_type = 'W'.
    l_message-message_number = sy-msgno.
    l_message-message_var1 = sy-msgv1.
    l_message-message_var2 = sy-msgv2.
    l_message-message_var3 = sy-msgv3.
    l_message-message_var4 = sy-msgv4.
    APPEND l_message TO messages.
* Continueing anyhow, since this is no fatal error.
  ENDIF.


* Cleanup unneeded preconditions
  CALL FUNCTION '/GAL/JS_CLEAN_PRECONDITIONS'.


* Now iterate through the jobs, until no new jobs to process are found
  DO.

    DO 0 TIMES. MESSAGE i018. ENDDO.
    CLEAR l_message.
    l_message-message_id = '/GAL/JS'.
    l_message-message_type = 'I'.
    l_message-message_number = '018'.
    APPEND l_message TO messages.

    " Update the preconditions, so that eventual new candidates are found
    CALL FUNCTION '/GAL/JS_UPDATE_PRECONDITIONS'
      EXCEPTIONS
        execution_failed = 1
        rfc_exception    = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      CLEAR l_message.
      l_message-message_id = sy-msgid.
      l_message-message_type = 'W'.
      l_message-message_number = sy-msgno.
      l_message-message_var1 = sy-msgv1.
      l_message-message_var2 = sy-msgv2.
      l_message-message_var3 = sy-msgv3.
      l_message-message_var4 = sy-msgv4.
      APPEND l_message TO messages.
    ENDIF.

    REFRESH lt_jobdata01.

* Checking for waiting jobs which have all preconditions fulfilled.
    DO 0 TIMES. MESSAGE i019. ENDDO.
    CLEAR l_message.
    l_message-message_id = '/GAL/JS'.
    l_message-message_type = 'I'.
    l_message-message_number = '019'.
    APPEND l_message TO messages.

    SELECT * FROM /gal/jobdata01 AS j01
           INTO TABLE lt_jobdata01
           WHERE status = 'W'
             AND uc4_mode <> 'W'
                 AND NOT EXISTS (
                   SELECT * FROM /gal/jobdata02
                           WHERE job_id = j01~id
                             AND status = 'N' )
           ORDER BY mod_timestamp ASCENDING.              "#EC CI_SUBRC

* Skip jobs that caused an error during the prevous loop
    LOOP AT lt_jobdata01 INTO l_jobdata01.
      READ TABLE l_skip_ids
            WITH KEY table_line = l_jobdata01-id
                 TRANSPORTING NO FIELDS.
      CHECK sy-subrc = 0.

      DELETE lt_jobdata01.
    ENDLOOP.

* Check if there are any jobs left to process
    IF lt_jobdata01 IS INITIAL.
      DO 0 TIMES. MESSAGE i020. ENDDO.
      CLEAR l_message.
      l_message-message_id = '/GAL/JS'.
      l_message-message_type = 'I'.
      l_message-message_number = '020'.
      APPEND l_message TO messages.
      EXIT.
    ENDIF.

    DESCRIBE TABLE lt_jobdata01 LINES sy-tfill.
    DO 0 TIMES. MESSAGE i021 WITH ''. ENDDO.
    CLEAR l_message.
    l_message-message_id = '/GAL/JS'.
    l_message-message_type = 'I'.
    l_message-message_number = '021'.
    l_message-message_var1 = sy-tfill.
    APPEND l_message TO messages.

    READ TABLE lt_jobdata01 INDEX 1 INTO l_jobdata01.
    DO 0 TIMES. MESSAGE i022 WITH ''. ENDDO.
    CLEAR l_message.
    l_message-message_id = '/GAL/JS'.
    l_message-message_type = 'I'.
    l_message-message_number = '022'.
    l_message-message_var1 = l_jobdata01-id.
    APPEND l_message TO messages.

    TRY.
        CALL METHOD /gal/job=>read_job_from_db
          EXPORTING
            id  = l_jobdata01-id
          RECEIVING
            job = l_job.

        l_job->execute( ).

      CATCH  /gal/cx_js_missing_resource /gal/cx_js_cannot_enqueue /gal/cx_js_exception cx_root INTO l_ex. "#EC CATCH_ALL
        " In case of any error give out the error message
        /gal/trace=>write_exception( l_ex ).
        l_message_text = l_ex->get_text( ).

        CLEAR l_message.
        l_message-message_id     = '/GAL/JS'.
        l_message-message_type   = 'E'.
        l_message-message_number = '012'.

        /gal/string=>string_to_message_vars( EXPORTING input = l_message_text
                                             IMPORTING msgv1 = l_message-message_var1
                                                       msgv2 = l_message-message_var2
                                                       msgv3 = l_message-message_var3
                                                       msgv4 = l_message-message_var4 ).

        DO 0 TIMES. "Dummy for where-used list
          MESSAGE e012(/gal/js) WITH l_message-message_var1
                                     l_message-message_var2
                                     l_message-message_var3
                                     l_message-message_var4.
        ENDDO.

        INSERT l_message      INTO TABLE messages.
        INSERT l_jobdata01-id INTO TABLE l_skip_ids. " Erroneous jobs are skipped during this run, in order to avoid an endless loop

    ENDTRY.
  ENDDO.

  COMMIT WORK.


  " Finally release the lock
  CALL FUNCTION 'DEQUEUE_/GAL/E_JS_LOCK'
    EXPORTING
      lock_key = 'SCHEDULER'
      _scope   = '1'.

  DO 0 TIMES. MESSAGE i017. ENDDO.
  CLEAR l_message.
  l_message-message_id = '/GAL/JS'.
  l_message-message_type = 'I'.
  l_message-message_number = '017'.
  APPEND l_message TO messages.

ENDMETHOD.
ENDCLASS.
