FUNCTION /gal/js_get_job_resource_datas.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(FROM) TYPE  TIMESTAMP
*"     REFERENCE(RESOURCE_ID) TYPE  /GAL/RESOURCE_STRING
*"     REFERENCE(ASSIGNED) TYPE  ABAP_BOOL
*"     REFERENCE(REQUESTED) TYPE  ABAP_BOOL
*"     REFERENCE(FREE) TYPE  ABAP_BOOL
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO
*"  EXPORTING
*"     REFERENCE(JOB_RESOURCE_DATAS) TYPE  /GAL/JOB_RESOURCE_DATAS
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA:
     l_condition TYPE string,
     l_add_and TYPE abap_bool,
     l_job_state TYPE /gal/job_status,
     l_resource_state TYPE /gal/precondition_status,
     l_job_resource_data TYPE /gal/job_resource_data.

* Initialize result
  CLEAR job_resource_datas.

  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.

  IF assigned = abap_false AND free = abap_false AND requested = abap_false.
    RETURN.
  ENDIF.

  IF requested = abap_true.
    l_job_state = 'R'.
  ELSE.
    l_job_state = ''.
  ENDIF.

  IF assigned = abap_true AND free = abap_true.
    l_resource_state = ''.
    l_job_state = ''. "everything get return, so flag is no more important.
  ELSEIF assigned = abap_true.
    l_resource_state = 'N'.
  ELSEIF free = abap_true.
    l_resource_state = 'F'.
  ELSE.
    l_resource_state = ''.
  ENDIF.


  IF l_resource_state EQ ''.
    l_job_state = ''.
  ENDIF.
  "TRANSLATE l_resource_id USING '*%'.

  IF from = 0 OR l_resource_state EQ '' AND l_job_state EQ '' .
    l_add_and = abap_false.
  ELSE.
    l_add_and = abap_true.
  ENDIF.
  l_condition = ``.
  IF from <> 0.
    CONCATENATE l_condition `j01~mod_timestamp >= from` INTO l_condition. "#EC NOTEXT
  ENDIF.
  IF l_add_and = abap_true.
    CONCATENATE l_condition ` AND ( ` INTO l_condition.     "#EC NOTEXT
  ENDIF.
  IF l_resource_state NE ''.
    CONCATENATE l_condition `j02~status = l_resource_state` INTO l_condition. "#EC NOTEXT
  ENDIF.
  IF l_job_state NE ''.
    IF l_resource_state NE ''.
      CONCATENATE l_condition ` OR ` INTO l_condition.      "#EC NOTEXT
    ENDIF.
    CONCATENATE l_condition `j01~status = l_job_state` INTO l_condition. "#EC NOTEXT
  ENDIF.

  IF l_add_and = abap_true.
    CONCATENATE l_condition ` ) ` INTO l_condition.
  ENDIF.

  SELECT j02r~resource_id j02~status j01~id j01~mod_timestamp j01~status j02r~id
    FROM /gal/jobdata02r AS j02r
    INNER JOIN /gal/jobdata02 AS j02
    ON j02r~id = j02~id
    INNER JOIN /gal/jobdata01 AS j01
    ON j02~job_id = j01~id
    INTO l_job_resource_data
    WHERE (l_condition)
    ORDER BY j01~mod_timestamp DESCENDING.             "#EC CI_DYNWHERE
    CHECK l_job_resource_data-resource_id CP resource_id OR resource_id IS INITIAL. "#EC CI_CHECK
    APPEND l_job_resource_data TO job_resource_datas.
  ENDSELECT.                                              "#EC CI_SUBRC
ENDFUNCTION.
