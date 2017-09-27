FUNCTION /gal/js_cancel_job.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(RFC_ROUTE_INFO_STEP2) TYPE  /GAL/RFC_ROUTE_INFO
*"       OPTIONAL
*"     REFERENCE(JOB_NAME) TYPE  BTCJOB
*"     REFERENCE(JOB_COUNT) TYPE  BTCJOBCNT
*"     REFERENCE(WAIT) TYPE  ABAP_BOOL DEFAULT ABAP_FALSE
*"     REFERENCE(IGNORE_NOT_RUNNING_JOB) TYPE  ABAP_BOOL DEFAULT
*"       ABAP_TRUE
*"  EXCEPTIONS
*"      CANNOT_BE_CANCELLED
*"      RFC_EXCEPTION
*"      EXECUTION_FAILED
*"----------------------------------------------------------------------

  CONSTANTS:
    lc_timeout   TYPE i VALUE 120.

  DATA:
    l_job_status TYPE /gal/btcjob_status.


* Follow RFC route
  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception cannot_be_cancelled.
  cfw_pass_exception execution_failed.
  cfw_remote_coding.

* Schedule job on target system if different from central system
  IF rfc_route_info_step2 IS NOT INITIAL.
    CALL FUNCTION '/GAL/JS_CANCEL_JOB'
      EXPORTING
        rfc_route_info         = rfc_route_info_step2
        job_name               = job_name
        job_count              = job_count
        wait                   = wait
        ignore_not_running_job = ignore_not_running_job
      EXCEPTIONS
        rfc_exception          = 1
        cannot_be_cancelled    = 2
        OTHERS                 = 3.
    IF sy-subrc = 1.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING rfc_exception.
    ELSEIF sy-subrc = 2.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING cannot_be_cancelled.
    ELSEIF sy-subrc = 3.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING execution_failed.
    ENDIF.

    RETURN.
  ENDIF.

  CALL FUNCTION 'BP_JOB_ABORT'
    EXPORTING
      jobcount                   = job_count
      jobname                    = job_name
    EXCEPTIONS
      checking_of_job_has_failed = 1
      job_abort_has_failed       = 2
      job_does_not_exist         = 3
      job_is_not_active          = 4
      no_abort_privilege_given   = 5
      OTHERS                     = 6.
  IF ignore_not_running_job = abap_true AND sy-subrc = 4.
    RETURN.
  ELSEIF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               RAISING cannot_be_cancelled.
  ENDIF.

  IF wait = abap_true.
    DO.
      IF sy-index = lc_timeout.
        MESSAGE e000 WITH job_name job_count RAISING execution_failed.
      ENDIF.
      CALL FUNCTION '/GAL/JS_CHECK_BTCJOB_STATUS'
        EXPORTING
          job_name         = job_name
          job_count        = job_count
        IMPORTING
          status           = l_job_status
        EXCEPTIONS
          rfc_exception    = 1
          execution_failed = 2
          OTHERS           = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                   RAISING execution_failed.
      ENDIF.
      IF l_job_status NA 'R'.
        EXIT.
      ENDIF.
      WAIT UP TO 1 SECONDS.
    ENDDO.
  ENDIF.


ENDFUNCTION.
