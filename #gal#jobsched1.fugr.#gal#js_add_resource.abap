FUNCTION /gal/js_add_resource.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(JOB_ID) TYPE  /GAL/JOB_ID
*"     REFERENCE(RESOURCE_STRING) TYPE  /GAL/RESOURCE_STRING
*"  EXPORTING
*"     REFERENCE(STATUS) TYPE  /GAL/PRECONDITION_STATUS
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA:
    l_jobdata02        TYPE /gal/jobdata02,
    l_jobdata02r       TYPE /gal/jobdata02r,
    l_locked           TYPE flag.

* Initialize result
  CLEAR status.

* Follow RFC route
  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.


* Check if there are running jobs with the same resource string
* The select statement is a bit complicated an working with a loop.
* This is due to the fact, a string cannot be used in the where clause
  SELECT COUNT( * ) UP TO 1 ROWS
    FROM ( /gal/jobdata02r AS j02r INNER JOIN /gal/jobdata02 AS j02 ON j02r~id = j02~id )
         INNER JOIN /gal/jobdata01 AS j01 ON j02~job_id = j01~id
    WHERE j01~status = 'R' AND j02~type = 'R' AND j02r~resource_id = resource_string. "#EC CI_SUBRC
  IF sy-dbcnt > 0.
    l_locked = 'X'.
  ENDIF.

  IF l_locked IS INITIAL.
    l_jobdata02-status = 'F'.
  ELSE.
    l_jobdata02-status = 'N'.
  ENDIF.

  l_jobdata02-id = /gal/uuid=>create_char( ).
  l_jobdata02-job_id = job_id.
  l_jobdata02-type = 'R'.

  INSERT /gal/jobdata02 FROM l_jobdata02.                 "#EC CI_SUBRC

  l_jobdata02r-id = l_jobdata02-id.
  l_jobdata02r-resource_id = resource_string.

  INSERT /gal/jobdata02r FROM l_jobdata02r.               "#EC CI_SUBRC

  status = l_jobdata02-status.

  COMMIT WORK.
ENDFUNCTION.
