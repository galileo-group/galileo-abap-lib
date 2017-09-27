FUNCTION /gal/js_dequeue_job.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(JOB_ID) TYPE  /GAL/JOB_ID
*"     REFERENCE(TRACE) TYPE  ABAP_BOOL DEFAULT ABAP_FALSE
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.


  IF trace = abap_true.
    CALL METHOD /gal/trace=>write_text
      EXPORTING
        text     = 'Unlocking job {1}'
        var01    = job_id
        no_flush = 'X'.                                     "#EC NOTEXT

    CALL METHOD /gal/trace=>write_table
      EXPORTING
        table = rfc_route_info-call_stack.
  ENDIF.


  CALL FUNCTION 'DEQUEUE_/GAL/E_JOBDATA01'
    EXPORTING
      id     = job_id
      _scope = '1'.


ENDFUNCTION.
