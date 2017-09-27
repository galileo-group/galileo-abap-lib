FUNCTION /gal/js_get_jobs_states .
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(JOB_IDS) TYPE  /GAL/TT_JOB_IDS
*"     REFERENCE(TOLERANT_MODE) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(STATUS) TYPE  /GAL/TT_JOB_STATUS
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"      EXECUTION_FAILED
*"----------------------------------------------------------------------

  DATA:
    l_status     TYPE /gal/job_status,
    l_id         TYPE /gal/job_id,
    l_job_status TYPE /gal/st_job_status.

* Initialize result
  CLEAR status.

* Follow RFC route
  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception execution_failed.
  cfw_remote_coding.

  LOOP AT job_ids INTO l_id.
    SELECT SINGLE status FROM /gal/jobdata01 INTO (l_status) WHERE id = l_id. "#EC CI_SUBRC
    IF sy-dbcnt = 0.
      IF tolerant_mode IS INITIAL.
        MESSAGE e013 WITH l_id RAISING execution_failed.
      ELSE.
        /gal/trace=>write_text(
          EXPORTING
            text                    = text-000
            var01                   = l_id
        ).
        CONTINUE.
      ENDIF.
    ENDIF.
    l_job_status-id = l_id.
    l_job_status-status = l_status.
    INSERT l_job_status INTO TABLE status.                "#EC CI_SUBRC
  ENDLOOP.
ENDFUNCTION.
