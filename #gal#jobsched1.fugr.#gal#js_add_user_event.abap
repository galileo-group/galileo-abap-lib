FUNCTION /gal/js_add_user_event.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(JOB_ID) TYPE  /GAL/JOB_ID
*"  EXPORTING
*"     REFERENCE(EVENT_ID) TYPE  /GAL/PRECONDITION_ID
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA:
    l_jobdata02  TYPE /gal/jobdata02,
    l_jobdata02u TYPE /gal/jobdata02u.

* Initialize result
  CLEAR event_id.

* Follow RFC route
  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.


  l_jobdata02-status = 'N'.

  l_jobdata02-id = /gal/uuid=>create_char( ).
  l_jobdata02-job_id = job_id.
  l_jobdata02-type = 'U'.

  INSERT /gal/jobdata02 FROM l_jobdata02.                 "#EC CI_SUBRC

  l_jobdata02u-id = l_jobdata02-id.
  l_jobdata02u-status = 'N'.

  INSERT /gal/jobdata02u FROM l_jobdata02u.               "#EC CI_SUBRC

  event_id = l_jobdata02-id.

  COMMIT WORK.
ENDFUNCTION.
