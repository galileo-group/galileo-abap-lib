FUNCTION /gal/js_check_preconditions.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(JOB_ID) TYPE  /GAL/JOB_ID
*"  EXPORTING
*"     REFERENCE(FULFILLED) TYPE  FLAG
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

* Initialize result
  CLEAR fulfilled.

* Follow RFC route
  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.

* Check if preconditions for job are met
  SELECT COUNT( * ) UP TO 1 ROWS
         FROM /gal/jobdata01 AS j1 INNER JOIN /gal/jobdata02 AS j2
              ON j1~id = j2~job_id
              WHERE j2~status = 'N' AND j1~id = job_id.   "#EC CI_SUBRC
  IF sy-dbcnt = 0.
    fulfilled = abap_true.
  ELSE.
    fulfilled = abap_false.
  ENDIF.
ENDFUNCTION.
