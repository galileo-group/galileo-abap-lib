FUNCTION /gal/js_check_running_jobs.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(JOB_ID) TYPE  /GAL/JOB_ID OPTIONAL
*"     REFERENCE(DO_NOT_RUN_SCHEDULER) TYPE  ABAP_BOOL DEFAULT
*"       ABAP_FALSE
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"      CHECK_FAILED
*"----------------------------------------------------------------------

  DATA:
    l_jobdata01      TYPE /gal/jobdata01,
    lt_jobdata01     TYPE TABLE OF /gal/jobdata01,
    l_error          TYPE abap_bool,
    l_status_db      TYPE btcstatus,
    l_actual_status  TYPE btcstatus,
    l_run_scheduler  TYPE abap_bool,
    l_rfc_route_info TYPE /gal/rfc_route_info.

  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception check_failed.
  cfw_remote_coding.


  IF do_not_run_scheduler = abap_false.
    /gal/job_scheduler=>run( ).
  ENDIF.


  SELECT * FROM /gal/jobdata01 AS j01
         INTO TABLE lt_jobdata01
         WHERE status = 'R'
         ORDER BY mod_timestamp ASCENDING.                "#EC CI_SUBRC

  IF NOT job_id IS INITIAL.
    DELETE lt_jobdata01 WHERE id NE job_id.
  ENDIF.

  IF lt_jobdata01 IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT lt_jobdata01 INTO l_jobdata01.
    CLEAR l_rfc_route_info.
    /gal/cfw_helper=>rfc_route_info_from_string(
      EXPORTING
        string         = l_jobdata01-destination
      RECEIVING
        rfc_route_info = l_rfc_route_info
    ).
    CALL FUNCTION '/GAL/JS_BP_JOB_CHECKSTATE'
      EXPORTING
        rfc_route_info               = l_rfc_route_info
        dialog                       = 'N'
        jobcount                     = l_jobdata01-job_count
        jobname                      = l_jobdata01-job_name
        start_asap                   = space
        read_only_status             = space
      IMPORTING
        status_according_to_db       = l_status_db
        actual_status                = l_actual_status
      EXCEPTIONS
        rfc_exception                = 1
        checking_of_job_has_failed   = 2
        correcting_job_status_failed = 3
        invalid_dialog_type          = 4
        job_does_not_exist           = 5
        no_check_privilege_given     = 6
        ready_switch_too_dangerous   = 7
        OTHERS                       = 8.
    IF sy-subrc <> 0.
      /gal/trace=>write_error( ).
      IF NOT job_id IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                   RAISING check_failed.
      ENDIF.
      l_error = abap_true.
    ELSE.
      IF l_status_db <> l_actual_status.
        l_run_scheduler = abap_true.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF do_not_run_scheduler = abap_false AND l_run_scheduler = abap_true.
    /gal/job_scheduler=>run( ).
  ENDIF.

  IF l_error = abap_true.
    MESSAGE e033 RAISING check_failed.
  ENDIF.

ENDFUNCTION.
