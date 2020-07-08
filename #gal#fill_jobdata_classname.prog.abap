*&---------------------------------------------------------------------*
*& Report /GAL/FILL_JOBDATA_CLASSNAME
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gal/fill_jobdata_classname.

START-OF-SELECTION.

  AUTHORITY-CHECK OBJECT 'S_BTCH_JOB'
    ID 'JOBACTION' FIELD 'RELE'
    ID 'JOBGROUP'  FIELD '*'.
  IF sy-subrc <> 0.
    MESSAGE e676(00).
  ENDIF.

  PERFORM main.


FORM main.

  DATA:
    l_rfc_dest           TYPE /gal/rfc_destination,
    l_rfc_route_info     TYPE /gal/rfc_route_info,
    lt_inconsistent      TYPE /gal/tt_job_ids,
    lt_inconsistent_hist TYPE /gal/tt_job_ids,
    l_error              TYPE string,
    l_ex                 TYPE REF TO /gal/cx_js_exception.

  FIELD-SYMBOLS:
    <l_job_id>           TYPE /gal/job_id.

  TRY.
      l_rfc_dest = /gal/job=>determine_store_destination( ).
      l_rfc_route_info = /gal/cfw_helper=>rfc_route_info_from_string(
        string = l_rfc_dest
      ).
      CALL FUNCTION '/GAL/JS_FILL_JD_CLASSNAME'
        EXPORTING
          rfc_route_info             = l_rfc_route_info
          fill_current               = abap_true
          fill_hist                  = abap_true
        IMPORTING
          inconsistent_jobs_hist     = lt_inconsistent_hist
          inconsistent_jobs_current  = lt_inconsistent
        EXCEPTIONS
          rfc_exception              = 1
          unsupported_jobdat_version = 2
          OTHERS                     = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                   INTO l_error.
        IF sy-subrc = 2.
          WRITE / l_error COLOR COL_TOTAL.
        ELSE.
          WRITE / l_error COLOR COL_NEGATIVE.
        ENDIF.
        RETURN.
      ENDIF.
    CATCH /gal/cx_js_exception INTO l_ex.
      l_error = l_ex->get_text( ).
      WRITE / l_error COLOR COL_NEGATIVE.
      RETURN.
  ENDTRY.

  IF lt_inconsistent IS NOT INITIAL.
    WRITE / TEXT-000 COLOR COL_TOTAL.
    LOOP AT lt_inconsistent ASSIGNING <l_job_id>.
      WRITE / <l_job_id>.
    ENDLOOP.
  ENDIF.
  IF lt_inconsistent_hist IS NOT INITIAL.
    WRITE / TEXT-001 COLOR COL_TOTAL.
    LOOP AT lt_inconsistent_hist ASSIGNING <l_job_id>.
      WRITE / <l_job_id>.
    ENDLOOP.
  ENDIF.

  SKIP.
  WRITE / TEXT-002.

ENDFORM.
