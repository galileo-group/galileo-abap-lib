*&---------------------------------------------------------------------*
*& Report  /GAL/JS_JOB_SCHEDULER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  /gal/js_job_scheduler LINE-SIZE 255.

**********************************************************************
* CAUTION: This report is only to be run on central system           *
**********************************************************************

PARAMETERS:
  p_rt_tim TYPE i DEFAULT 3,
  p_rt_wi  TYPE i DEFAULT 5.

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
    lt_messages TYPE /gal/tt_message_struct,
    l_message   TYPE string.

  FIELD-SYMBOLS:
    <l_message> TYPE /gal/st_message_struct.


  /gal/job_scheduler=>run(
    EXPORTING
      retry_times         = p_rt_tim
      retry_wait_interval = p_rt_wi
    IMPORTING
      messages            = lt_messages
  ).

  LOOP AT lt_messages ASSIGNING <l_message>.
    CHECK NOT <l_message>-message_type IS INITIAL.
    MESSAGE ID <l_message>-message_id TYPE <l_message>-message_type NUMBER <l_message>-message_number
            WITH <l_message>-message_var1 <l_message>-message_var2 <l_message>-message_var3 <l_message>-message_var4
            INTO l_message.
    CASE <l_message>-message_type.
      WHEN 'A' OR 'E' OR 'X'.
        WRITE / l_message COLOR COL_NEGATIVE.
      WHEN 'W'.
        WRITE / l_message COLOR COL_TOTAL.
      WHEN OTHERS.
        WRITE / l_message.
    ENDCASE.
  ENDLOOP.

ENDFORM.
