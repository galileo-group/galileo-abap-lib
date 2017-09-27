FUNCTION /gal/js_is_waiting_for_event.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(JOB_ID) TYPE  /GAL/JOB_ID
*"     REFERENCE(IGNORE_WHEN_MISSING_PREDECS) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(EVENT_IDS) TYPE  /GAL/PRECONDITION_IDS
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"      EXECUTION_FAILED
*"----------------------------------------------------------------------

* Initialize result
  CLEAR event_ids.

* Follow RFC route
  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception execution_failed.
  cfw_remote_coding.

  CALL FUNCTION '/GAL/JS_UPDATE_PRECONDITIONS'
    EXPORTING
      job_id           = job_id
    EXCEPTIONS
      rfc_exception    = 1
      execution_failed = 2
      OTHERS           = 3.
  IF sy-subrc = 1.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING rfc_exception.
  ELSEIF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING execution_failed.
  ENDIF.

  IF ignore_when_missing_predecs IS INITIAL.
    SELECT id INTO TABLE event_ids
           FROM /gal/jobdata02
           WHERE job_id = job_id AND type = 'U' AND status = 'N'. "#EC CI_SUBRC
  ELSE.
    SELECT id INTO TABLE event_ids
           FROM /gal/jobdata02
           WHERE job_id = job_id AND type = 'U' AND status = 'N'
           AND NOT EXISTS (
             SELECT * FROM /gal/jobdata02 WHERE job_id = job_id AND type = 'J' AND status = 'N'
           ).                                             "#EC CI_SUBRC
  ENDIF.
ENDFUNCTION.
