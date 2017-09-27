FUNCTION /gal/jm_find_job_ids.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(LATEST_MOD_TIMESTAMP) TYPE  TIMESTAMP OPTIONAL
*"     REFERENCE(ONLY_OF) TYPE  ABAP_BOOL DEFAULT ABAP_TRUE
*"  EXPORTING
*"     REFERENCE(JOB_IDS) TYPE  /GAL/TT_JOB_IDS
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA:
    l_latest_mod_ts TYPE timestamp.


  CLEAR job_ids.

* Follow RFC route
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.


  IF latest_mod_timestamp IS INITIAL.
    GET TIME STAMP FIELD l_latest_mod_ts.
  ELSE.
    l_latest_mod_ts = latest_mod_timestamp.
  ENDIF.

  IF only_of = abap_true.
    SELECT id FROM /gal/jobdata01 INTO TABLE job_ids
           WHERE ( status = 'F' OR status = 'O' ) AND mod_timestamp < l_latest_mod_ts. "#EC CI_SUBRC
  ELSE.
    SELECT id FROM /gal/jobdata01 INTO TABLE job_ids
       WHERE mod_timestamp < l_latest_mod_ts.           "#EC CI_NOFIELD
                                                          "#EC CI_SUBRC
  ENDIF.

ENDFUNCTION.
