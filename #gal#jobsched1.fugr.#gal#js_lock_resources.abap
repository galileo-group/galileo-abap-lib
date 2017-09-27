FUNCTION /gal/js_lock_resources.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(JOB_ID) TYPE  /GAL/JOB_ID
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"      EXECUTION_FAILED
*"----------------------------------------------------------------------


  DATA:
    l_resource_string  TYPE /gal/resource_string,
    l_id               TYPE /gal/precondition_id,
    l_message_stack    TYPE REF TO /gal/message_stack.


  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception execution_failed.
  cfw_remote_coding.


  CREATE OBJECT l_message_stack.

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

  SELECT j02r~resource_id INTO (l_resource_string)
    FROM /gal/jobdata02  AS j02 INNER JOIN
         /gal/jobdata02r AS j02r ON j02~id = j02r~id
    WHERE j02~job_id = job_id.

    SELECT j02~id INTO (l_id)
      FROM /gal/jobdata01  AS j01  INNER JOIN
           /gal/jobdata02  AS j02  ON j01~id = j02~job_id INNER JOIN
           /gal/jobdata02r AS j02r ON j02~id = j02r~id
      WHERE j02~job_id <> job_id AND j02~status = 'F' AND j01~status = 'W' AND j02r~resource_id = l_resource_string.
      UPDATE /gal/jobdata02 SET status = 'N' WHERE id = l_id. "#EC CI_SUBRC
    ENDSELECT.                                            "#EC CI_SUBRC
  ENDSELECT.                                              "#EC CI_SUBRC

  COMMIT WORK.

  CALL FUNCTION 'DEQUEUE_/GAL/E_JOBDATA02'
    EXPORTING
      _scope = '1'.
ENDFUNCTION.
