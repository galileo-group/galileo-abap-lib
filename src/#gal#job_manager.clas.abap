class /GAL/JOB_MANAGER definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !STORE_RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO .
  methods CLEANUP_PRECONDITIONS
    importing
      !MIN_JOB_AGE_IN_DAYS type INT4 default 30
    raising
      /GAL/CX_JS_EXCEPTION .
  methods CLEANUP_OLD_JOBS
    importing
      !MIN_JOB_AGE_IN_DAYS type INT4 default 30
    raising
      /GAL/CX_JS_EXCEPTION .
protected section.

  data STORE_RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO .
private section.
ENDCLASS.



CLASS /GAL/JOB_MANAGER IMPLEMENTATION.


METHOD cleanup_old_jobs.

  DATA:
    l_timestamp  TYPE REF TO /gal/timestamp_short,
    l_message    TYPE string,
    lt_job_ids   TYPE /gal/tt_job_ids,
    l_job_id     TYPE /gal/job_id,
    l_job        TYPE REF TO /gal/job,
    l_ex_job     TYPE REF TO /gal/cx_js_exception.


  CREATE OBJECT l_timestamp.
  l_timestamp->subtract_interval(
      days    = min_job_age_in_days
  ).

  CALL FUNCTION '/GAL/JM_FIND_JOB_IDS'
    EXPORTING
      rfc_route_info       = store_rfc_route_info
      latest_mod_timestamp = l_timestamp->value
    IMPORTING
      job_ids              = lt_job_ids
    EXCEPTIONS
      rfc_exception        = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               INTO l_message.
    RAISE EXCEPTION TYPE /gal/cx_js_exception
      EXPORTING
        textid = /gal/cx_js_exception=>cannot_clean_preconditions
        var1   = l_message.
  ENDIF.


  LOOP AT lt_job_ids INTO l_job_id.
    TRY.
        /gal/job=>read_job_from_db(
          EXPORTING
            id                   = l_job_id
          RECEIVING
            job                  = l_job
        ).
        IF l_job->status NE 'R'.
          l_job->delete_from_db( EXPORTING force = abap_true ).
        ENDIF.
      CATCH /gal/cx_js_exception INTO l_ex_job.
        /gal/trace=>write_exception(
          EXPORTING
            exception               = l_ex_job
            no_flush                = abap_true
        ).
        CONTINUE.
    ENDTRY.
  ENDLOOP.

  /gal/trace=>flush( ).

ENDMETHOD.


METHOD cleanup_preconditions.

  DATA:
    l_timestamp TYPE REF TO /gal/timestamp_short,
    l_message   TYPE string.


  CREATE OBJECT l_timestamp.
  l_timestamp->subtract_interval(
      days    = min_job_age_in_days
  ).

  CALL FUNCTION '/GAL/JM_CLEAN_PRECONDITIONS'
    EXPORTING
      rfc_route_info       = store_rfc_route_info
      latest_mod_timestamp = l_timestamp->value
    EXCEPTIONS
      rfc_exception        = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               INTO l_message.
    RAISE EXCEPTION TYPE /gal/cx_js_exception
      EXPORTING
        textid = /gal/cx_js_exception=>cannot_clean_preconditions
        var1   = l_message.
  ENDIF.

ENDMETHOD.


METHOD constructor.

  me->store_rfc_route_info = store_rfc_route_info.

ENDMETHOD.
ENDCLASS.
