FUNCTION /gal/js_run_scheduler.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(START_TIMESTAMP) TYPE  TIMESTAMP OPTIONAL
*"     REFERENCE(IN_BACKGROUND) TYPE  FLAG OPTIONAL
*"     REFERENCE(RETRY_TIMES) TYPE  INT4 DEFAULT 3
*"     REFERENCE(RETRY_WAIT_INTERVAL) TYPE  INT4 DEFAULT 5
*"  EXPORTING
*"     REFERENCE(MESSAGES) TYPE  /GAL/TT_MESSAGE_STRUCT
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"      CANNOT_RUN_SCHEDULER
*"----------------------------------------------------------------------

  DATA:
    l_job_name        TYPE btcjob,
    l_job_count       TYPE btcjobcnt,
    l_date            TYPE dats,
    l_time            TYPE tims,
    l_jobclass        TYPE btcjobclas,
    l_config_store    TYPE REF TO /gal/config_store_local,
    l_config_node     TYPE REF TO /gal/config_node,
    l_start_timestamp TYPE REF TO /gal/timestamp_short,
    lt_params         TYPE rsparams_tt,
    l_wa_param        TYPE rsparams.

* Initialize result
  CLEAR messages.

* Follow RFC route
  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception cannot_run_scheduler.
  cfw_remote_coding.

  IF in_background IS INITIAL AND start_timestamp IS INITIAL.

    /gal/job_scheduler=>run(
      EXPORTING
        retry_times         = retry_times
        retry_wait_interval = retry_wait_interval
      IMPORTING
        messages            = messages
    ).

  ELSE.

    l_job_name = 'JOB_SCHEDULER'.

*   Jobklasse aus dem Konfigurationseditor lesen
    TRY.
*       Create instance of configuration store
        CREATE OBJECT l_config_store.

*       Get instance of node
        l_config_node = l_config_store->get_node(
          path = '/Galileo Group AG/Open Source Components/Job Scheduler/Job class for central system' ). "#EC NOTEXT

        CALL METHOD l_config_node->get_value
          IMPORTING
            value = l_jobclass.

      CATCH /gal/cx_config_exception.
        CLEAR l_jobclass.
    ENDTRY.

* Creat new job
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = l_job_name
        jobclass         = l_jobclass
      IMPORTING
        jobcount         = l_job_count
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING cannot_run_scheduler.
    ENDIF.

    l_wa_param-kind = 'P'.
    l_wa_param-selname = 'P_RT_TIM'.
    l_wa_param-low = retry_times.
    INSERT l_wa_param INTO TABLE lt_params.
    l_wa_param-kind = 'P'.
    l_wa_param-selname = 'P_RT_WI'.
    l_wa_param-low = retry_wait_interval.
    INSERT l_wa_param INTO TABLE lt_params.

* Create step for job scheduler
    SUBMIT /gal/js_job_scheduler
      WITH SELECTION-TABLE lt_params
      VIA JOB l_job_name NUMBER l_job_count AND RETURN.  "#EC CI_SUBMIT

    IF start_timestamp IS INITIAL.

* Schedule job for immediate start
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = l_job_count
          jobname              = l_job_name
          strtimmed            = abap_true
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          invalid_target       = 8
          OTHERS               = 9.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                RAISING cannot_run_scheduler.
      ENDIF.
    ELSE.

* Convert time stamp to system date and time
      CREATE OBJECT l_start_timestamp
        EXPORTING
          value = start_timestamp.

      l_start_timestamp->to_date_time( EXPORTING time_zone = /gal/timestamp_base=>time_zone_system
                                       IMPORTING date      = l_date
                                                 time      = l_time ).

* Schedule job
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = l_job_count
          jobname              = l_job_name
          sdlstrtdt            = l_date
          sdlstrttm            = l_time
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          invalid_target       = 8
          OTHERS               = 9.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                RAISING cannot_run_scheduler.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFUNCTION.
