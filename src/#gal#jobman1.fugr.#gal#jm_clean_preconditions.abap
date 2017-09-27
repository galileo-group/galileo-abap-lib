FUNCTION /gal/jm_clean_preconditions.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(LATEST_MOD_TIMESTAMP) TYPE  TIMESTAMP OPTIONAL
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA:
    l_precond_id    TYPE /gal/precondition_id,
    l_precond_type  TYPE /gal/precondition_type,
    l_latest_mod_ts TYPE timestamp.


* Follow RFC route
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.


  IF latest_mod_timestamp IS INITIAL.
    GET TIME STAMP FIELD l_latest_mod_ts.
  ELSE.
    l_latest_mod_ts = latest_mod_timestamp.
  ENDIF.

* First delete preconditions of jobs that are finished or obsolete
  SELECT j02~id j02~type FROM /gal/jobdata01 AS j01 INNER JOIN /gal/jobdata02 AS j02 ON j02~job_id = j01~id
         INTO (l_precond_id, l_precond_type) WHERE ( j01~status = 'O' OR j01~status = 'F' ) AND j01~mod_timestamp < l_latest_mod_ts.
    CASE l_precond_type.
      WHEN 'T'.
        DELETE FROM /gal/jobdata02t WHERE id = l_precond_id. "#EC CI_SUBRC
      WHEN 'J'.
        DELETE FROM /gal/jobdata02j WHERE id = l_precond_id. "#EC CI_SUBRC
      WHEN 'R'.
        DELETE FROM /gal/jobdata02r WHERE id = l_precond_id. "#EC CI_SUBRC
      WHEN 'U'.
        DELETE FROM /gal/jobdata02u WHERE id = l_precond_id. "#EC CI_SUBRC
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.
    DELETE FROM /gal/jobdata02 WHERE id = l_precond_id.   "#EC CI_SUBRC
  ENDSELECT.                                              "#EC CI_SUBRC
* Now delete preconditions that do not have a job anymore
  SELECT j02~id j02~type FROM /gal/jobdata02 AS j02 INTO (l_precond_id, l_precond_type)
    WHERE NOT EXISTS ( SELECT * FROM /gal/jobdata01 WHERE id = j02~job_id ). "#EC CI_NOFIELD
    CASE l_precond_type.
      WHEN 'T'.
        DELETE FROM /gal/jobdata02t WHERE id = l_precond_id. "#EC CI_SUBRC
      WHEN 'J'.
        DELETE FROM /gal/jobdata02j WHERE id = l_precond_id. "#EC CI_SUBRC
      WHEN 'R'.
        DELETE FROM /gal/jobdata02r WHERE id = l_precond_id. "#EC CI_SUBRC
      WHEN 'U'.
        DELETE FROM /gal/jobdata02u WHERE id = l_precond_id. "#EC CI_SUBRC
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.
    DELETE FROM /gal/jobdata02 WHERE id = l_precond_id.   "#EC CI_SUBRC
  ENDSELECT.                                              "#EC CI_SUBRC

  COMMIT WORK.

ENDFUNCTION.
