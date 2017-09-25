FUNCTION /gal/js_handle_orphaned_jobs.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(RUN_SCHEDULER) TYPE  FLAG OPTIONAL
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"      EXECUTION_FAILED
*"----------------------------------------------------------------------

  DATA:
    l_jobdata01       TYPE /gal/jobdata01,
    lt_jobdata01      TYPE TABLE OF /gal/jobdata01,
    l_status          TYPE /gal/btcjob_status,
    l_rfc_route_info  TYPE /gal/rfc_route_info,
    l_job             TYPE REF TO /gal/job,
    l_run_scheduler   TYPE flag,
    l_ex              TYPE REF TO /gal/cx_js_exception,
    l_ex_enqueue      TYPE REF TO /gal/cx_js_exception,
    l_text            TYPE string,
    l_trace_text      TYPE string,
    l_symsgv1         TYPE symsgv,
    l_symsgv2         TYPE symsgv,
    l_curr_ts         TYPE timestamp,
    l_ts_obj          TYPE REF TO /gal/timestamp_short,
    l_timeout         TYPE flag,
    l_notex           TYPE abap_bool.


  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception execution_failed.
  cfw_remote_coding.


  SELECT * FROM /gal/jobdata01 INTO TABLE lt_jobdata01 WHERE status = 'R'. "#EC CI_SUBRC
* Erst SELECT INTO TABLE, dann ein LOOP, da es sonst zu CX_SY_OPEN_SQL_DB kommt,
* weil der RFC ein COMMIT WORK durchführt
  LOOP AT lt_jobdata01 INTO l_jobdata01.

    CLEAR l_status.
    CLEAR l_timeout.

    IF NOT l_jobdata01-job_name IS INITIAL AND NOT l_jobdata01-job_count IS INITIAL.

      CALL METHOD /gal/cfw_helper=>rfc_route_info_from_string
        EXPORTING
          string         = l_jobdata01-destination
        RECEIVING
          rfc_route_info = l_rfc_route_info.

      CALL FUNCTION '/GAL/JS_CHECK_BTCJOB_STATUS'
        EXPORTING
          rfc_route_info     = l_rfc_route_info
          job_name           = l_jobdata01-job_name
          job_count          = l_jobdata01-job_count
        IMPORTING
          status             = l_status
        EXCEPTIONS
          rfc_exception      = 1
          job_does_not_exist = 2
          execution_failed   = 3
          OTHERS             = 4.
      IF sy-subrc = 2.
        l_notex = abap_true.
      ELSEIF sy-subrc <> 0.
        /gal/trace=>write_error(
          EXPORTING
            no_flush = 'X'
        ).
        /gal/cfw_helper=>rfc_route_info_to_string(
          EXPORTING
            rfc_route_info = l_rfc_route_info
          RECEIVING
            string         = l_trace_text
        ).
        CONCATENATE 'Error occured on destination:' l_trace_text INTO l_trace_text SEPARATED BY space. "#EC NOTEXT
        /gal/trace=>write_text(
          EXPORTING
            text = l_trace_text
        ).

        CONTINUE.
      ENDIF.

      CHECK l_status CA 'FA' OR l_notex = abap_true.

    ELSE.

      GET TIME STAMP FIELD l_curr_ts.
      CREATE OBJECT l_ts_obj
        EXPORTING
          value = l_curr_ts.
      l_ts_obj->subtract_interval(
        EXPORTING
          minutes = 10
      ).

      CHECK l_ts_obj->value > l_jobdata01-mod_timestamp.
      l_timeout = 'X'.

    ENDIF.

    l_run_scheduler = 'X'.

    TRY.
        CALL METHOD /gal/job=>read_job_from_db
          EXPORTING
            id  = l_jobdata01-id
          RECEIVING
            job = l_job.

        TRY.
            l_job->enqueue( ).
          CATCH /gal/cx_js_cannot_enqueue /gal/cx_js_exception INTO l_ex_enqueue.
            l_text = l_ex_enqueue->get_text( ).
            /gal/trace=>write_text(
              EXPORTING
                text                    = l_text
            ).
            CONTINUE.
        ENDTRY.

        IF NOT l_job->status = 'R'.
          l_job->dequeue( ).
          CONTINUE.
        ENDIF.

        IF l_status = 'A'.
          DO 0 TIMES. MESSAGE e014 WITH l_jobdata01-job_name l_jobdata01-job_count. ENDDO.
          l_symsgv1 = l_jobdata01-job_name.
          l_symsgv2 = l_jobdata01-job_count.
          l_job->set_status_to_error(
            EXPORTING
              symsgid = '/GAL/JS'
              symsgty = 'E'
              symsgno = '014'
              symsgv1 = l_symsgv1
              symsgv2 = l_symsgv2
          ).
        ELSEIF l_notex = abap_true.
          DO 0 TIMES. MESSAGE e030 WITH l_jobdata01-job_name l_jobdata01-job_count. ENDDO.
          l_symsgv1 = l_jobdata01-job_name.
          l_symsgv2 = l_jobdata01-job_count.
          l_job->set_status_to_error(
            EXPORTING
              symsgid = '/GAL/JS'
              symsgty = 'E'
              symsgno = '030'
              symsgv1 = l_symsgv1
              symsgv2 = l_symsgv2
          ).
        ELSEIF NOT l_timeout IS INITIAL.
          DO 0 TIMES. MESSAGE e024. ENDDO.
          l_job->set_status_to_error( symsgid = '/GAL/JS'
                                      symsgty = 'E'
                                      symsgno = '024' ).
        ELSE.
          l_job->post_process( ).
        ENDIF.

        l_job->dequeue( ).

      CATCH /gal/cx_js_exception INTO l_ex.
        l_text = l_ex->get_text( ).
        MESSAGE e007 WITH l_jobdata01-id l_text RAISING execution_failed.

    ENDTRY.

  ENDLOOP.

  IF run_scheduler IS INITIAL OR l_run_scheduler IS INITIAL.
    RETURN.
  ENDIF.

  TRY.
      /gal/job=>run_job_scheduler( ).
    CATCH /gal/cx_js_exception INTO l_ex.
      l_text = l_ex->get_text( ).
      MESSAGE e008 WITH l_text RAISING execution_failed.
  ENDTRY.

ENDFUNCTION.
