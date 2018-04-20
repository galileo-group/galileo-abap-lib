FUNCTION /gal/js_fill_jd_classname.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(FILL_CURRENT) TYPE  ABAP_BOOL DEFAULT ABAP_TRUE
*"     REFERENCE(FILL_HIST) TYPE  ABAP_BOOL DEFAULT ABAP_TRUE
*"  EXPORTING
*"     REFERENCE(INCONSISTENT_JOBS_HIST) TYPE  /GAL/TT_JOB_IDS
*"     REFERENCE(INCONSISTENT_JOBS_CURRENT) TYPE  /GAL/TT_JOB_IDS
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"      UNSUPPORTED_JOBDAT_VERSION
*"----------------------------------------------------------------------

  DATA:
    l_job_id       TYPE /gal/job_id,
    l_job_id_dummy TYPE /gal/job_id,                        "#EC NEEDED
    l_classname    TYPE classname,
    l_value(255)   TYPE c,
    l_version      TYPE /gal/js_db_layer_version,
    l_js_config    TYPE /gal/js_config.


  CLEAR inconsistent_jobs_current.
  CLEAR inconsistent_jobs_hist.


* Follow RFC route
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception unsupported_jobdat_version.
  cfw_remote_coding.


  SELECT SINGLE value INTO l_value FROM /gal/js_config WHERE attribute = 'DB_LAYER_VERSION'.
  IF sy-subrc <> 0.
    l_version = '000'.
  ELSE.
    l_version = l_value.
  ENDIF.

  IF l_version > '001'.
    MESSAGE e035 WITH l_version RAISING unsupported_jobdat_version.
    RETURN.
  ENDIF.

  IF fill_current = abap_true.
    SELECT id FROM /gal/jobdata01 INTO l_job_id WHERE classname = space. "#EC CI_NOFIELD
      CLEAR l_classname.
      SELECT id FROM /gal/jobdata01s INTO l_job_id_dummy WHERE id = l_job_id.
        l_classname = '/GAL/JOB_SAP'.
        EXIT.                                       "#EC CI_EXIT_SELECT
      ENDSELECT.                                          "#EC CI_SUBRC
      IF l_classname IS INITIAL.
        SELECT id FROM ('/GAL/JOBDATA01I') INTO l_job_id_dummy WHERE id = l_job_id. "#EC CI_DYNTAB
          l_classname = '/GAL/CCM_JOB_IMPORT'.
          EXIT.                                     "#EC CI_EXIT_SELECT
        ENDSELECT.                                        "#EC CI_SUBRC
      ENDIF.
      IF l_classname IS INITIAL.
        INSERT l_job_id INTO TABLE inconsistent_jobs_current. "#EC CI_SUBRC
        CONTINUE.
      ENDIF.
      UPDATE /gal/jobdata01 SET classname = l_classname WHERE id = l_job_id. "#EC CI_SUBRC
    ENDSELECT.                                            "#EC CI_SUBRC
  ENDIF.

  IF fill_hist = abap_true.
    SELECT id FROM /gal/jd01_hist INTO l_job_id WHERE classname = space. "#EC CI_NOFIELD
      CLEAR l_classname.
      SELECT id FROM /gal/jd01s_hist INTO l_job_id_dummy WHERE id = l_job_id.
        l_classname = '/GAL/JOB_SAP'.
        EXIT.                                       "#EC CI_EXIT_SELECT
      ENDSELECT.                                          "#EC CI_SUBRC
      IF l_classname IS INITIAL.
        SELECT id FROM ('/GAL/JD01I_HIST') INTO l_job_id_dummy WHERE id = l_job_id. "#EC CI_DYNTAB
          l_classname = '/GAL/CCM_JOB_IMPORT'.
          EXIT.                                     "#EC CI_EXIT_SELECT
        ENDSELECT.                                        "#EC CI_SUBRC
      ENDIF.
      IF l_classname IS INITIAL.
        INSERT l_job_id INTO TABLE inconsistent_jobs_hist. "#EC CI_SUBRC
        CONTINUE.
      ENDIF.
      UPDATE /gal/jd01_hist SET classname = l_classname WHERE id = l_job_id. "#EC CI_SUBRC
    ENDSELECT.                                            "#EC CI_SUBRC
  ENDIF.

  l_js_config-attribute = 'DB_LAYER_VERSION'.
  l_js_config-value     = '001'.
  MODIFY /gal/js_config FROM l_js_config.                 "#EC CI_SUBRC

  COMMIT WORK.

ENDFUNCTION.
