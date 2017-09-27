FUNCTION /gal/js_add_predecessor_job.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(JOB_ID) TYPE  /GAL/JOB_ID
*"     REFERENCE(PREDECESSOR_JOB_ID) TYPE  /GAL/JOB_ID
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"      EXECUTION_FAILED
*"----------------------------------------------------------------------


  DATA:
    l_jobdata02  TYPE /gal/jobdata02,
    l_jobdata02j TYPE /gal/jobdata02j,
    l_job        TYPE REF TO /gal/job,
    l_text       TYPE string,
    l_ex         TYPE REF TO /gal/cx_js_exception.

  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception execution_failed.
  cfw_remote_coding.


  TRY.
      CALL METHOD /gal/job=>read_job_from_db
        EXPORTING
          id  = predecessor_job_id
        RECEIVING
          job = l_job.
    CATCH /gal/cx_js_exception INTO l_ex.
      l_text = l_ex->get_text( ).
      MESSAGE e006(/gal/js) WITH predecessor_job_id l_text RAISING execution_failed.
  ENDTRY.

  IF l_job->status = 'E' OR l_job->status = 'F'.
    l_jobdata02-status = 'F'.
  ELSE.
    l_jobdata02-status = 'N'.
  ENDIF.

  l_jobdata02-id = /gal/uuid=>create_char( ).
  l_jobdata02-job_id = job_id.
  l_jobdata02-type = 'J'.

  INSERT /gal/jobdata02 FROM l_jobdata02.                 "#EC CI_SUBRC

  l_jobdata02j-id = l_jobdata02-id.
  l_jobdata02j-predec_job_id = predecessor_job_id.

  INSERT /gal/jobdata02j FROM l_jobdata02j.               "#EC CI_SUBRC

  COMMIT WORK.
ENDFUNCTION.
