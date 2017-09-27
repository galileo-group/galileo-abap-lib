FUNCTION /gal/js_check_btcjob_status .
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(JOB_NAME) TYPE  BTCJOB
*"     REFERENCE(JOB_COUNT) TYPE  BTCJOBCNT
*"  EXPORTING
*"     REFERENCE(STATUS) TYPE  /GAL/BTCJOB_STATUS
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"      JOB_DOES_NOT_EXIST
*"      EXECUTION_FAILED
*"----------------------------------------------------------------------

  DATA:
    l_aborted     TYPE flag,
    l_finished    TYPE flag,
    l_preliminary TYPE flag,
    l_ready       TYPE flag,
    l_running     TYPE flag,
    l_scheduled   TYPE flag,
    l_suspended   TYPE flag,
    l_other       TYPE flag.                                "#EC NEEDED

* Initialize result
  CLEAR status.

* Follow RFC route
  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception job_does_not_exist.
  cfw_pass_exception execution_failed.
  cfw_remote_coding.


  CALL FUNCTION 'SHOW_JOBSTATE'
    EXPORTING
      jobcount         = job_count
      jobname          = job_name
    IMPORTING
      aborted          = l_aborted
      finished         = l_finished
      preliminary      = l_preliminary
      ready            = l_ready
      running          = l_running
      scheduled        = l_scheduled
      suspended        = l_suspended
      other            = l_other
    EXCEPTIONS
      jobcount_missing = 1
      jobname_missing  = 2
      job_notex        = 3
      OTHERS           = 4.
  IF sy-subrc = 3.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING job_does_not_exist.
  ELSEIF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING execution_failed.
  ENDIF.


  IF NOT l_aborted IS INITIAL.
    status = 'A'.
  ELSEIF NOT l_finished IS INITIAL.
    status = 'F'.
  ELSEIF NOT l_preliminary IS INITIAL.
    status = 'P'.
  ELSEIF NOT l_ready IS INITIAL.
    status = 'Y'.
  ELSEIF NOT l_running IS INITIAL.
    status = 'R'.
  ELSEIF NOT l_scheduled IS INITIAL.
    status = 'S'.
  ELSEIF NOT l_suspended IS INITIAL.
    status = 'Z'.
  ELSE.
    status = 'O'.
  ENDIF.




ENDFUNCTION.
