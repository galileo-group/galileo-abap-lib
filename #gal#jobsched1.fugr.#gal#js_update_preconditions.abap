FUNCTION /gal/js_update_preconditions.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(JOB_ID) TYPE  /GAL/JOB_ID OPTIONAL
*"  EXCEPTIONS
*"      EXECUTION_FAILED
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------


  DATA:
    l_jobdata02  TYPE /gal/jobdata02,
    l_jobdata02j TYPE /gal/jobdata02j,
    l_jobdata02r TYPE /gal/jobdata02r,
    BEGIN OF l_jobdata,
      job_id       TYPE /gal/job_id,
      precond_id   TYPE /gal/precondition_id,
      precond_type TYPE /gal/precondition_type,
    END OF l_jobdata,
    lt_jobdata      LIKE TABLE OF l_jobdata,
    lt_fulfilled    TYPE TABLE OF /gal/precondition_id,
    l_fulfilled     TYPE /gal/precondition_id,
    l_curr_ts       TYPE timestamp,
    l_job_status    TYPE /gal/job_status,
    lt_job_id_range TYPE RANGE OF /gal/job_id,
    l_job_id_range  LIKE LINE OF lt_job_id_range,
    l_message_stack TYPE REF TO /gal/message_stack,
    l_dummy         TYPE /gal/precondition_id.              "#EC NEEDED


  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception execution_failed.
  cfw_remote_coding.


  CREATE OBJECT l_message_stack.


  IF NOT job_id IS INITIAL.
    l_job_id_range-sign = 'I'.
    l_job_id_range-option = 'EQ'.
    l_job_id_range-low = job_id.
    INSERT l_job_id_range INTO TABLE lt_job_id_range.     "#EC CI_SUBRC
  ENDIF.

  SELECT j1~id j2~id j2~type
         INTO (l_jobdata-job_id, l_jobdata-precond_id, l_jobdata-precond_type)
         FROM /gal/jobdata01 AS j1 INNER JOIN /gal/jobdata02 AS j2
              ON j1~id = j2~job_id
         WHERE j1~status = 'W' AND j1~id IN lt_job_id_range.
    APPEND l_jobdata TO lt_jobdata.
  ENDSELECT.                                              "#EC CI_SUBRC


  LOOP AT lt_jobdata INTO l_jobdata.
    CASE l_jobdata-precond_type.

      WHEN 'J'.
        SELECT SINGLE * FROM /gal/jobdata02j INTO l_jobdata02j WHERE id = l_jobdata-precond_id. "#EC CI_SUBRC
        SELECT SINGLE status FROM /gal/jobdata01 INTO l_job_status WHERE id = l_jobdata02j-predec_job_id. "#EC CI_SUBRC
        IF l_job_status = 'F'.
*         Jobs mit Status 'E' brauchen eine andere Möglichkeit auf 'F' gesetzt zu werden
          APPEND l_jobdata-precond_id TO lt_fulfilled.
        ENDIF.

      WHEN 'T'.
        GET TIME STAMP FIELD l_curr_ts.
        SELECT SINGLE id FROM /gal/jobdata02t
                         INTO l_dummy
                        WHERE id = l_jobdata-precond_id AND timestamp <= l_curr_ts. "#EC CI_SUBRC
        IF sy-dbcnt > 0.
          INSERT l_jobdata-precond_id INTO TABLE lt_fulfilled.
        ENDIF.

      WHEN 'U'.
        SELECT SINGLE id FROM /gal/jobdata02u
                         INTO l_dummy
                        WHERE id = l_jobdata-precond_id AND status = 'N'. "#EC CI_SUBRC
        IF sy-dbcnt = 0.
          INSERT l_jobdata-precond_id INTO TABLE lt_fulfilled.
        ENDIF.

      WHEN 'R'.
        SELECT SINGLE * FROM /gal/jobdata02r INTO l_jobdata02r WHERE id = l_jobdata-precond_id. "#EC CI_SUBRC
        CALL FUNCTION '/GAL/JS_UPDATE_RESOURCES'
          EXPORTING
            resource_string = l_jobdata02r-resource_id
          EXCEPTIONS
            rfc_exception   = 1
            OTHERS          = 2.
        IF sy-subrc = 1.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                  RAISING rfc_exception.
        ELSEIF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                  RAISING execution_failed.
        ENDIF.

    ENDCASE.
  ENDLOOP.

  CALL FUNCTION 'ENQUEUE_/GAL/E_JOBDATA02'
    EXPORTING
      _wait          = 'X'
      _scope         = '1'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    l_message_stack->push( ).
    /gal/trace=>write_error( ).
    l_message_stack->pop( ).
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING execution_failed.
  ENDIF.

  LOOP AT lt_fulfilled INTO l_fulfilled.
    SELECT SINGLE * FROM /gal/jobdata02 INTO l_jobdata02 WHERE id = l_fulfilled AND status NE 'F'.
    "Exclude fulfilled in select cause status might have changed sincve checking
    CHECK sy-subrc = 0.
    l_jobdata02-status = 'F'.
    UPDATE /gal/jobdata02 FROM l_jobdata02.               "#EC CI_SUBRC
  ENDLOOP.

  COMMIT WORK.

  CALL FUNCTION 'DEQUEUE_/GAL/E_JOBDATA02'
    EXPORTING
      _scope = '1'.
ENDFUNCTION.
