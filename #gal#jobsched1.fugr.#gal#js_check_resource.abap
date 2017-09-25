FUNCTION /gal/js_check_resource.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(JOB_ID) TYPE  /GAL/JOB_ID
*"  EXPORTING
*"     REFERENCE(LOCKED_RESOURCE_ID) TYPE  /GAL/RESOURCE_STRING
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA l_resource_id_outer TYPE /gal/resource_string.

* Initialize result
  CLEAR locked_resource_id.

  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.

  " SELECT all resources needed by job
  SELECT j2r~resource_id INTO (l_resource_id_outer)
         FROM /gal/jobdata02 AS j2 INNER JOIN /gal/jobdata02r AS j2r ON j2~id = j2r~id
         WHERE j2~job_id = job_id.

    SELECT SINGLE j2r~resource_id INTO (locked_resource_id)
           FROM /gal/jobdata01 AS j1
                INNER JOIN /gal/jobdata02 AS j2 ON j1~id = j2~job_id
                INNER JOIN /gal/jobdata02r AS j2r ON j2~id = j2r~id "#EC CI_SUBRC
           WHERE j1~status = 'R' AND j2r~resource_id = l_resource_id_outer AND j2r~freed = space. "#EC WARNOK

    CHECK NOT locked_resource_id IS INITIAL.
    EXIT.                                           "#EC CI_EXIT_SELECT
  ENDSELECT.                                              "#EC CI_SUBRC
ENDFUNCTION.
