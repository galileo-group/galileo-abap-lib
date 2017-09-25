FUNCTION /gal/js_enqueue_job.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(JOB_ID) TYPE  /GAL/JOB_ID
*"     REFERENCE(TRACE) TYPE  ABAP_BOOL DEFAULT ABAP_FALSE
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"      FOREIGN_LOCK
*"      EXECUTION_FAILED
*"----------------------------------------------------------------------


  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception execution_failed.
  cfw_pass_exception foreign_lock.
  cfw_remote_coding.


  DATA l_message_stack TYPE REF TO /gal/message_stack.
  CREATE OBJECT l_message_stack.


  IF trace = abap_true.
    CALL METHOD /gal/trace=>write_text
      EXPORTING
        text     = 'Locking job {1}'
        var01    = job_id
        no_flush = 'X'.                                     "#EC NOTEXT

    CALL METHOD /gal/trace=>write_table
      EXPORTING
        table = rfc_route_info-call_stack.
  ENDIF.

  CALL FUNCTION 'ENQUEUE_/GAL/E_JOBDATA01'
    EXPORTING
      id             = job_id
      _scope         = '1'
      _wait          = 'X'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc = 1.
    l_message_stack->push( ).
    /gal/trace=>write_error( ).
    l_message_stack->pop( ).
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING foreign_lock.
  ELSEIF sy-subrc <> 0.
    l_message_stack->push( ).
    /gal/trace=>write_error( ).
    l_message_stack->pop( ).
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING execution_failed.
  ENDIF.



ENDFUNCTION.
