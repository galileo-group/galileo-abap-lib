FUNCTION /gal/js_get_predec_jobs.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(JOB_ID) TYPE  /GAL/JOB_ID
*"  EXPORTING
*"     REFERENCE(PREDECESSOR_JOB_IDS) TYPE  /GAL/TT_JOB_IDS
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

* Initialize result
  CLEAR predecessor_job_ids.

* Follow RFC route
  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.

* Select predecessor job IDs
  SELECT j02j~predec_job_id
         FROM /gal/jobdata02 AS j02 INNER JOIN /gal/jobdata02j AS j02j
         ON j02~id = j02j~id
         INTO TABLE predecessor_job_ids
         WHERE j02~job_id = job_id.                       "#EC CI_SUBRC
ENDFUNCTION.
