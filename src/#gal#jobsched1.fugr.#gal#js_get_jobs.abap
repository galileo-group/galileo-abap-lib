FUNCTION /gal/js_get_jobs.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO
*"     REFERENCE(NAME) TYPE  BTCJOB
*"     REFERENCE(DESTINATION) TYPE  /GAL/RFC_DESTINATION
*"     REFERENCE(STATUS_I) TYPE  ABAP_BOOL
*"     REFERENCE(STATUS_W) TYPE  ABAP_BOOL
*"     REFERENCE(STATUS_R) TYPE  ABAP_BOOL
*"     REFERENCE(STATUS_S) TYPE  ABAP_BOOL
*"     REFERENCE(STATUS_F) TYPE  ABAP_BOOL
*"     REFERENCE(STATUS_E) TYPE  ABAP_BOOL
*"     REFERENCE(TYPE_I) TYPE  ABAP_BOOL
*"     REFERENCE(TYPE_S) TYPE  ABAP_BOOL
*"     REFERENCE(FROM) TYPE  TIMESTAMP
*"     REFERENCE(TILL) TYPE  TIMESTAMP
*"     REFERENCE(JOB_ID) TYPE  /GAL/JOB_ID
*"  EXPORTING
*"     REFERENCE(JOB_DATAS) TYPE  /GAL/JOB_DATAS
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA:
     l_job TYPE /gal/jobdata01,
     l_condition TYPE string,
     l_status TYPE string,
     l_type TYPE string,
     l_name TYPE string,
     l_job_id TYPE string,
     l_time TYPE string.

* Initialize result
  CLEAR job_datas.

* Follow RFC route
  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.

  IF status_i EQ abap_false AND status_w EQ abap_false AND status_r EQ abap_false AND status_s EQ abap_false AND status_f EQ abap_false AND status_e EQ abap_false.
    RETURN.
  ENDIF.

  IF type_i EQ abap_false AND type_s EQ abap_false.
    RETURN.
  ENDIF.

  IF status_i EQ abap_true AND status_w EQ abap_true AND status_r EQ abap_true AND status_s EQ abap_true AND status_f EQ abap_true AND status_e EQ abap_true.
    l_status = ``.
  ELSE.
    IF status_i EQ abap_true.
      l_status = `1234status = 'I'`.                        "#EC NOTEXT
    ENDIF.
    IF status_w EQ abap_true.
      CONCATENATE l_status ` OR status = 'W'` INTO l_status. "#EC NOTEXT
    ENDIF.
    IF status_r EQ abap_true.
      CONCATENATE l_status ` OR status = 'R'` INTO l_status. "#EC NOTEXT
    ENDIF.
    IF status_s EQ abap_true.
      CONCATENATE l_status ` OR status = 'S'` INTO l_status. "#EC NOTEXT
    ENDIF.
    IF status_f EQ abap_true.
      CONCATENATE l_status ` OR status = 'F'` INTO l_status. "#EC NOTEXT
    ENDIF.
    IF status_e EQ abap_true.
      CONCATENATE l_status ` OR status = 'E'` INTO l_status. "#EC NOTEXT
    ENDIF.
    SHIFT l_status BY 4 PLACES LEFT.
  ENDIF.

  IF type_i EQ abap_true AND type_s EQ abap_true.
    l_type = ``.
  ELSE.
    IF type_i EQ abap_true.
      l_type =  `1234type = 'I'`.                           "#EC NOTEXT
    ENDIF.
    IF type_s EQ abap_true.
      CONCATENATE l_type ` OR type = 'S'` INTO l_type.      "#EC NOTEXT
    ENDIF.
    SHIFT l_type BY 4 PLACES LEFT.
  ENDIF.

  IF from IS NOT INITIAL.
    l_time = `12345mod_timestamp >= from`.                  "#EC NOTEXT
  ENDIF.
  IF till IS NOT INITIAL.
    CONCATENATE l_time ` AND mod_timestamp <= till` INTO l_time. "#EC NOTEXT
  ENDIF.
  SHIFT l_time BY 5 PLACES LEFT.


  IF name NE ''.
    l_name = name.
    TRANSLATE l_name USING '*%'.
    CONCATENATE l_condition `12345job_name LIKE '` l_name `'` INTO l_condition. "#EC NOTEXT
  ENDIF.
  IF job_id NE ''.
    l_job_id = job_id.
    TRANSLATE l_job_id USING '*%'.
    CONCATENATE l_condition ` AND id LIKE '` l_job_id `'` INTO l_condition. "#EC NOTEXT
  ENDIF.
  IF l_status NE ``.
    CONCATENATE l_condition ` AND ( ` l_status ` )` INTO  l_condition. "#EC NOTEXT
  ENDIF.
  IF l_type NE ``.
    CONCATENATE l_condition ` AND ( ` l_type ` )` INTO l_condition. "#EC NOTEXT
  ENDIF.
  IF l_time NE ``.
    CONCATENATE l_condition ` AND ( ` l_time ` )` INTO l_condition. "#EC NOTEXT
  ENDIF.
  SHIFT l_condition BY 5 PLACES LEFT.

  SELECT *
    FROM /gal/jobdata01
    INTO l_job
    WHERE (l_condition)
    ORDER BY mod_timestamp DESCENDING.                 "#EC CI_DYNWHERE
    CHECK l_job-destination CP destination OR destination IS INITIAL. "#EC CI_CHECK
    APPEND l_job TO job_datas.
  ENDSELECT.                                              "#EC CI_SUBRC
ENDFUNCTION.
