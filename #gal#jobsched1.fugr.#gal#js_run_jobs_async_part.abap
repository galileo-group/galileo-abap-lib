FUNCTION /gal/js_run_jobs_async_part.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(RFC_ROUTE_INFO_STEP2) TYPE  /GAL/RFC_ROUTE_INFO
*"       OPTIONAL
*"     REFERENCE(JS_JOB_ID) TYPE  /GAL/JOB_ID
*"     REFERENCE(JOB_NAME) TYPE  BTCJOB DEFAULT 'BACKGROUND_JOB'
*"  EXPORTING
*"     REFERENCE(JOB_COUNT) TYPE  BTCJOBCNT
*"  EXCEPTIONS
*"      CANNOT_SUBMIT_JOB
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA:
    l_dummy         TYPE trdir,                             "#EC NEEDED
    l_job           TYPE REF TO /gal/job,
    l_ex            TYPE REF TO /gal/cx_js_exception,
    l_enqueued      TYPE flag,
    l_rfc_ex_info   TYPE /gal/exception_info,
    l_config_store  TYPE REF TO /gal/config_store_local,
    l_config_folder TYPE REF TO /gal/config_node,
    l_jobclass      TYPE btcjobclas,
    l_message       TYPE string.

  DATA l_program            TYPE syrepid.

  DATA l_print_parameters   TYPE pri_params.
  DATA l_print_params_valid TYPE abap_bool.

* Initialize result
  CLEAR job_count.

* Follow RFC route
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception cannot_submit_job.
  cfw_remote_coding.

* Schedule job on target system if different from central system
  IF rfc_route_info_step2 IS NOT INITIAL.
    CALL FUNCTION '/GAL/JS_RUN_JOBS_ASYNC_PART'
      EXPORTING
        rfc_route_info = rfc_route_info_step2
        js_job_id      = js_job_id
        job_name       = job_name
      IMPORTING
        job_count      = job_count
      EXCEPTIONS
        rfc_exception  = 1
        OTHERS         = 2.
    IF sy-subrc = 1.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING rfc_exception.
    ELSEIF sy-subrc = 2.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING cannot_submit_job.
    ENDIF.

    RETURN.
  ENDIF.

* Check connection to central system
  CALL FUNCTION '/GAL/RFC_ROUTE_PING'
    EXPORTING
      rfc_route_info = /gal/job=>store_rfc_route_info
    IMPORTING
      exception_info = l_rfc_ex_info.
  IF l_rfc_ex_info IS NOT INITIAL.
    IF l_rfc_ex_info-message_text IS NOT INITIAL.
      /gal/trace=>write_text( text = l_rfc_ex_info-message_text ).
    ENDIF.

    /gal/string=>string_to_message_vars( EXPORTING input = text-001
                                         IMPORTING msgv1 = sy-msgv1
                                                   msgv2 = sy-msgv2
                                                   msgv3 = sy-msgv3
                                                   msgv4 = sy-msgv4 ).

    MESSAGE e012(/gal/js) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                       RAISING rfc_exception.
  ENDIF.

* Jobklasse aus dem Konfigurationseditor lesen
  TRY.
*     Create instance of configuration store
      CREATE OBJECT l_config_store.

*     Get instance of node
      l_config_folder = l_config_store->get_node( path = '/Galileo Group AG/Open Source Components/Job Scheduler/Job class for satellite system' ). "#EC NOTEXT
      l_config_folder->get_value( IMPORTING value = l_jobclass ).

    CATCH /gal/cx_config_exception.
      CLEAR l_jobclass.

  ENDTRY.

* Open new background job
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname  = job_name
      jobclass = l_jobclass
    IMPORTING
      jobcount = job_count
    EXCEPTIONS
      OTHERS   = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING cannot_submit_job.
  ENDIF.

* Update job data
  TRY.
      l_job = /gal/job=>read_job_from_db( id      = js_job_id
                                          enqueue = abap_true ).

      l_enqueued = abap_true.

      l_job->set_jobdata( job_name  = job_name
                          job_count = job_count ).

      l_job->dequeue( ).

    CATCH /gal/cx_js_exception INTO l_ex.
      IF l_enqueued = abap_true.
        TRY.
            l_job->dequeue( ).
          CATCH /gal/cx_js_exception.                   "#EC NO_HANDLER
        ENDTRY.
      ENDIF.

      l_message = l_ex->get_text( ).

      /gal/string=>string_to_message_vars( EXPORTING input = l_message
                                           IMPORTING msgv1 = sy-msgv1
                                                     msgv2 = sy-msgv2
                                                     msgv3 = sy-msgv3
                                                     msgv4 = sy-msgv4 ).

      MESSAGE e012(/gal/js) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                         RAISING cannot_submit_job.

  ENDTRY.

* Get output parameters
  CLEAR l_print_parameters.

  l_program = l_job->get_program_name( ).

  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
      in_parameters            = l_print_parameters
      no_dialog                = abap_true
      report                   = l_program
    IMPORTING
      out_parameters           = l_print_parameters
      valid_for_spool_creation = l_print_params_valid
    EXCEPTIONS
      OTHERS                   = 1.

* Schedule job execution report
  IF sy-subrc = 0 AND l_print_params_valid = abap_true.
    SUBMIT /gal/js_run_jobs_async_part
           WITH p_job_id = js_job_id
           TO SAP-SPOOL
           SPOOL PARAMETERS l_print_parameters
           WITHOUT SPOOL DYNPRO
           VIA JOB job_name NUMBER job_count
           AND RETURN.                                   "#EC CI_SUBMIT
  ELSE.
    SUBMIT /gal/js_run_jobs_async_part
           WITH p_job_id = js_job_id
           VIA JOB job_name NUMBER job_count
           AND RETURN.                                   "#EC CI_SUBMIT
  ENDIF.

* Close job
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount  = job_count
      jobname   = job_name
      strtimmed = abap_true
    EXCEPTIONS
      OTHERS    = 1.
  IF sy-subrc <> 0.
* TODO: Delete Job

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING cannot_submit_job.
  ENDIF.
ENDFUNCTION.
