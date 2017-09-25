*&---------------------------------------------------------------------*
*& Report  /GAL/JS_RUN_JOB_SCHEDULER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  /gal/js_run_job_scheduler.

START-OF-SELECTION.

  DATA:
    l_text       TYPE string,
    l_ex         TYPE REF TO /gal/cx_js_exception.

  TRY.
      CALL METHOD /gal/job=>run_job_scheduler.
    CATCH /gal/cx_js_exception INTO l_ex.
      l_text = l_ex->get_text( ).
      WRITE: / 'ERROR: ', l_text.
  ENDTRY.
