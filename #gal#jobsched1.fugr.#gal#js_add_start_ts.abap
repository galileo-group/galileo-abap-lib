FUNCTION /gal/js_add_start_ts.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(JOB_ID) TYPE  /GAL/JOB_ID
*"     REFERENCE(TIMESTAMP) TYPE  TIMESTAMP
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------


  DATA:
    l_jobdata02  TYPE /gal/jobdata02,
    l_jobdata02t TYPE /gal/jobdata02t,
    l_curr_ts    TYPE timestamp.

  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.


  GET TIME STAMP FIELD l_curr_ts.

  IF l_curr_ts >= timestamp.
    l_jobdata02-status = 'F'.
  ELSE.
    l_jobdata02-status = 'N'.
  ENDIF.

  l_jobdata02-id = /gal/uuid=>create_char( ).
  l_jobdata02-job_id = job_id.
  l_jobdata02-type = 'T'.

  INSERT /gal/jobdata02 FROM l_jobdata02.                 "#EC CI_SUBRC

  l_jobdata02t-id = l_jobdata02-id.
  l_jobdata02t-timestamp = timestamp.

  INSERT /gal/jobdata02t FROM l_jobdata02t.               "#EC CI_SUBRC

  COMMIT WORK.
ENDFUNCTION.
