*&---------------------------------------------------------------------*
*& Report  /GAL/JS_EXECUTE_BG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT /gal/js_execute_bg.

PARAMETERS p_id TYPE /gal/job_id.

START-OF-SELECTION.
  PERFORM main.

FORM main.
  DATA:
    l_job  TYPE REF TO /gal/job,
    l_ex   TYPE REF TO /gal/cx_js_exception,
    l_text TYPE string.

  TRY.
      CALL METHOD /gal/job=>read_job_from_db
        EXPORTING
          id  = p_id
        RECEIVING
          job = l_job.
      CALL METHOD l_job->execute.
    CATCH /gal/cx_js_exception INTO l_ex.
      l_text = l_ex->get_text( ).
      WRITE: / 'ERROR: ', l_text.
  ENDTRY.
ENDFORM.
