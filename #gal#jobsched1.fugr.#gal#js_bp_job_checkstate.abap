FUNCTION /gal/js_bp_job_checkstate.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     VALUE(DIALOG) TYPE  BTCH0000-CHAR1
*"     VALUE(JOBCOUNT) TYPE  TBTCJOB-JOBCOUNT
*"     VALUE(JOBNAME) TYPE  TBTCJOB-JOBNAME
*"     VALUE(START_ASAP) TYPE  BTCH0000-CHAR1 OPTIONAL
*"     VALUE(TIME_LIMIT) TYPE  I OPTIONAL
*"     VALUE(READ_ONLY_STATUS) TYPE  BTCH0000-CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(STATUS_ACCORDING_TO_DB) TYPE  TBTCJOB-STATUS
*"     VALUE(ACTUAL_STATUS) TYPE  TBTCJOB-STATUS
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"      CHECKING_OF_JOB_HAS_FAILED
*"      CORRECTING_JOB_STATUS_FAILED
*"      INVALID_DIALOG_TYPE
*"      JOB_DOES_NOT_EXIST
*"      NO_CHECK_PRIVILEGE_GIVEN
*"      READY_SWITCH_TOO_DANGEROUS
*"      EXECUTION_FAILED
*"----------------------------------------------------------------------

  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception checking_of_job_has_failed.
  cfw_pass_exception correcting_job_status_failed.
  cfw_pass_exception invalid_dialog_type.
  cfw_pass_exception job_does_not_exist.
  cfw_pass_exception no_check_privilege_given.
  cfw_pass_exception ready_switch_too_dangerous.
  cfw_pass_exception execution_failed.
  cfw_remote_coding.

  CALL FUNCTION 'BP_JOB_CHECKSTATE'
    EXPORTING
      dialog                       = dialog
      jobcount                     = jobcount
      jobname                      = jobname
      start_asap                   = start_asap
      time_limit                   = time_limit
      read_only_status             = read_only_status
    IMPORTING
      status_according_to_db       = status_according_to_db
      actual_status                = actual_status
    EXCEPTIONS
      checking_of_job_has_failed   = 1
      correcting_job_status_failed = 2
      invalid_dialog_type          = 3
      job_does_not_exist           = 4
      no_check_privilege_given     = 5
      ready_switch_too_dangerous   = 6
      OTHERS                       = 7.
  IF sy-subrc = 1.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               RAISING checking_of_job_has_failed.
  ELSEIF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               RAISING correcting_job_status_failed.
  ELSEIF sy-subrc = 3.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               RAISING invalid_dialog_type.
  ELSEIF sy-subrc = 4.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               RAISING job_does_not_exist.
  ELSEIF sy-subrc = 5.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               RAISING no_check_privilege_given .
  ELSEIF sy-subrc = 6.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               RAISING ready_switch_too_dangerous.
  ELSEIF sy-subrc = 7.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               RAISING execution_failed.
  ENDIF.


ENDFUNCTION.
