FUNCTION /gal/js_clean_preconditions.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(MIN_AGE_IN_DAYS) TYPE  INT4 DEFAULT 14
*"----------------------------------------------------------------------

  DATA:
    l_timestamp_obj TYPE REF TO /gal/timestamp_short,
    l_j02_id        TYPE /gal/precondition_id,
    l_j02_type      TYPE /gal/precondition_type.


  CREATE OBJECT l_timestamp_obj.

  l_timestamp_obj->subtract_interval(
    EXPORTING
      days    = min_age_in_days
  ).

  SELECT j02~id j02~type INTO (l_j02_id, l_j02_type)
         FROM /gal/jobdata01 AS j01 INNER JOIN /gal/jobdata02 AS j02 ON j01~id = j02~job_id
         WHERE j01~status = 'F' AND j01~mod_timestamp < l_timestamp_obj->value AND j02~status = 'F'.

    CASE l_j02_type.
      WHEN 'J'.
        DELETE FROM /gal/jobdata02j WHERE id = l_j02_id.  "#EC CI_SUBRC
      WHEN 'T'.
        DELETE FROM /gal/jobdata02t WHERE id = l_j02_id.  "#EC CI_SUBRC
      WHEN 'U'.
        DELETE FROM /gal/jobdata02u WHERE id = l_j02_id.  "#EC CI_SUBRC
      WHEN 'R'.
        DELETE FROM /gal/jobdata02r WHERE id = l_j02_id.  "#EC CI_SUBRC
    ENDCASE.

    DELETE FROM /gal/jobdata02 WHERE id = l_j02_id.       "#EC CI_SUBRC

  ENDSELECT.                                              "#EC CI_SUBRC
ENDFUNCTION.
