*&---------------------------------------------------------------------*
*& Report  /GAL/JS_RUN_JOBS_ASYNC_PART
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT /gal/js_run_jobs_async_part.

TYPE-POOLS abap.

PARAMETERS p_job_id TYPE /gal/job_id NO-DISPLAY.

START-OF-SELECTION.
  PERFORM main.

*&---------------------------------------------------------------------*
*&      Form  main
*&---------------------------------------------------------------------*
*       Main logic
*----------------------------------------------------------------------*
FORM main.
  DATA l_job       TYPE REF TO /gal/job.
  DATA l_ex        TYPE REF TO /gal/cx_js_exception.

  DATA l_message   TYPE string.

  DATA l_init_ex   TYPE REF TO cx_root.
  DATA l_msgv1     TYPE symsgv.
  DATA l_msgv2     TYPE symsgv.
  DATA l_msgv3     TYPE symsgv.
  DATA l_msgv4     TYPE symsgv.


* Support for asynchronous break points
  INCLUDE /gal/cfw_macros.                                 "#EC INCL_OK


  cfw_break_point_support.
  cfw_break_point '/GAL/JS_RUN_JOBS_ASYNC_PART'.


* Exit if there is no job id
  IF p_job_id IS INITIAL.
    MESSAGE i032(/gal/js) INTO l_message.
    WRITE l_message COLOR COL_NEGATIVE.
    MESSAGE e032(/gal/js). "Quit Job with error
  ENDIF.


  l_init_ex = /gal/job=>class_get_init_exception( ).
  IF NOT l_init_ex IS INITIAL.
    l_message = l_init_ex->get_text( ).
    /gal/string=>string_to_message_vars(
      EXPORTING
        input = l_message
      IMPORTING
        msgv1 = l_msgv1
        msgv2 = l_msgv2
        msgv3 = l_msgv3
        msgv4 = l_msgv4
    ).
    WRITE l_message COLOR COL_NEGATIVE.
    MESSAGE e031(/gal/js) WITH l_msgv1 l_msgv2 l_msgv3 l_msgv4. "Quit Job with error
  ENDIF.

* Read job and perform asynchronous part of job
  TRY.
      l_job = /gal/job=>read_job_from_db( id  = p_job_id ).
    CATCH /gal/cx_js_exception INTO l_ex.
      l_message = l_ex->get_text( ).
      /gal/string=>string_to_message_vars(
        EXPORTING
          input = l_message
        IMPORTING
          msgv1 = l_msgv1
          msgv2 = l_msgv2
          msgv3 = l_msgv3
          msgv4 = l_msgv4
      ).
      WRITE l_message COLOR COL_NEGATIVE.
      MESSAGE e031(/gal/js) WITH l_msgv1 l_msgv2 l_msgv3 l_msgv4. "Quit Job with error
  ENDTRY.


  TRY.
      l_job->execute_async( ).
      IF l_job->status = 'S' AND NOT l_job->auto_continue IS INITIAL.
        l_job->resume( auto_continued = abap_true ).
      ENDIF.
    CATCH /gal/cx_js_exception INTO l_ex.
      TRY.
          /gal/job=>run_job_scheduler( ).

        CATCH /gal/cx_js_exception.                     "#EC NO_HANDLER
      ENDTRY.

      l_message = l_ex->get_text( ).
      WRITE l_message COLOR COL_TOTAL.
      RETURN.

  ENDTRY.

ENDFORM.                    "main
