FUNCTION /gal/js_update_resources.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(RESOURCE_STRING) TYPE  /GAL/RESOURCE_STRING OPTIONAL
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"      EXECUTION_FAILED
*"----------------------------------------------------------------------


  DATA:
    l_resource_string TYPE /gal/resource_string,
    l_precondition_id TYPE /gal/precondition_id,
    lt_resource_range TYPE RANGE OF /gal/resource_string,
    l_resource_range  LIKE LINE OF lt_resource_range,
    l_message_stack   TYPE REF TO /gal/message_stack,
    l_config_store    TYPE REF TO /gal/config_store_local,
    l_config_folder   TYPE REF TO /gal/config_node,
    l_lock_timeout    TYPE i,
    l_timer           TYPE REF TO /gal/stopwatch,
    l_elapsed_seconds TYPE i.


  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception execution_failed.
  cfw_remote_coding.


  CREATE OBJECT l_message_stack.

  TRY.
      CREATE OBJECT l_config_store.
      l_config_folder = l_config_store->get_node(
        path = '/Galileo Group AG/Open Source Components/Job Scheduler/Update resources timeout'
      ).                                                    "#EC NOTEXT
      l_config_folder->get_value( IMPORTING value = l_lock_timeout ).

    CATCH /gal/cx_config_exception.
      l_lock_timeout = 30.                               "#EC NUMBER_OK
  ENDTRY.

  CREATE OBJECT l_timer.
  l_timer->start( ).

  DO.
    CALL FUNCTION 'ENQUEUE_/GAL/E_JOBDATA02'
      EXPORTING
        _scope = '1'
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      l_timer->get_elapsed_time( IMPORTING elapsed_seconds = l_elapsed_seconds ).

      IF l_elapsed_seconds < l_lock_timeout.
        WAIT UP TO 1 SECONDS.
        CONTINUE.
      ELSE.
        l_message_stack->push( ).
        /gal/trace=>write_error( ).
        l_message_stack->pop( ).
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                RAISING execution_failed.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  IF NOT resource_string IS INITIAL.
    l_resource_range-sign = 'I'.
    l_resource_range-option = 'EQ'.
    l_resource_range-low = resource_string.
    INSERT l_resource_range INTO TABLE lt_resource_range. "#EC CI_SUBRC
  ENDIF.

  SELECT j02~id j02r~resource_id INTO (l_precondition_id, l_resource_string)
    FROM /gal/jobdata02 AS j02 INNER JOIN /gal/jobdata02r AS j02r ON j02~id = j02r~id
         WHERE j02~status = 'N' AND j02~type = 'R' AND j02r~resource_id IN lt_resource_range.
* First select precondition_id and resource_id for all preconditions of type
* 'R' (='Resource') with status 'N' (='Not fulfilled')
* in order to re-check those resources
* If range is filled, limit the selection to a certain resource_id

    SELECT COUNT(*) UP TO 1 ROWS
      FROM ( /gal/jobdata02r AS j02r INNER JOIN /gal/jobdata02 AS j02 ON j02r~id = j02~id )
           INNER JOIN /gal/jobdata01 AS j01 ON j02~job_id = j01~id
      WHERE j01~status = 'R' AND j02~type = 'R' AND j02r~resource_id = l_resource_string AND j02r~freed = space. "#EC CI_SUBRC
    CHECK sy-dbcnt = 0.

    UPDATE /gal/jobdata02 SET status = 'F' WHERE id = l_precondition_id. "#EC CI_SUBRC
  ENDSELECT.                                              "#EC CI_SUBRC

  COMMIT WORK.

  CALL FUNCTION 'DEQUEUE_/GAL/E_JOBDATA02'
    EXPORTING
      _scope = '1'.
ENDFUNCTION.
