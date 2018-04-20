*----------------------------------------------------------------------*
***INCLUDE /GAL/_MONITOR_F01 .
*----------------------------------------------------------------------*

FORM pbo_0100_status.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'JOB_MONITOR_TITLE'.

  IF g_dynp_0100-subscr_dynpro IS INITIAL.
    g_dynp_0100-subscr_dynpro = '0102'.
  ENDIF.

  IF tab-activetab IS INITIAL.
    tab-activetab = 'TAB_J'.
  ENDIF.

ENDFORM.                    "pbo_0100_status

*&---------------------------------------------------------------------*
*&      Form  pbo_0100_init_RFC_ROUTE_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pbo_0100_init_rfc_route_info.
  DATA:
    l_store_destination TYPE /gal/rfc_destination,
    l_cx_js_exception   TYPE REF TO /gal/cx_js_exception,
    l_error_text        TYPE string.
  TRY.
      CALL METHOD /gal/job=>determine_store_destination
        RECEIVING
          store_destination = l_store_destination.

      CALL METHOD /gal/cfw_helper=>rfc_route_info_from_string
        EXPORTING
          string         = l_store_destination
        RECEIVING
          rfc_route_info = g_dynp_0100-rfc_route_info.
    CATCH /gal/cx_js_exception INTO l_cx_js_exception.
      l_error_text = l_cx_js_exception->get_text( ).
      MESSAGE l_error_text TYPE 'E'.
  ENDTRY.
ENDFORM.                    "pbo_0100_status


*&---------------------------------------------------------------------*
*&      Form  PBO_0101_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM pbo_0101_init.

  IF g_dynp_0101-custom_container IS INITIAL.
    CREATE OBJECT g_dynp_0101-custom_container
      EXPORTING
        container_name = 'CC_RESOURCE_TABLE'.

    CREATE OBJECT g_dynp_0101-grid
      EXPORTING
        i_parent = g_dynp_0101-custom_container.

    CREATE OBJECT g_dynp_0101-jm_event_receiver.
    SET HANDLER g_dynp_0101-jm_event_receiver->handle_user_command FOR g_dynp_0101-grid.
    SET HANDLER g_dynp_0101-jm_event_receiver->handle_cm_request FOR g_dynp_0101-grid.

    g_dynp_0101-cb_assigned = abap_true.
    g_dynp_0101-cb_requested = abap_true.

    PERFORM pbo_0101_init_table.
    PERFORM pbo_0101_load_data_into_grid.
  ENDIF.
ENDFORM.                    " PBO_0101_STATUS

*----------------------------------------------------------------------*
***INCLUDE /GAL/_MONITOR_F04 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  /gal/pbo_0101_init_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pbo_0101_init_table .
  DATA:
    l_layout           TYPE disvariant,
    l_table_layout     TYPE lvc_s_layo,
    l_it_column_layout TYPE lvc_t_fcat,
    l_el_column_layout TYPE lvc_s_fcat,
    l_it_fun           TYPE ui_functions.

  "Load data INTO TABLE  the grid and display them
  l_table_layout-sel_mode = 'C'.

  l_layout-report = sy-repid.

  l_table_layout-info_fname = 'LINE_COLOR'.

  l_el_column_layout-fieldname = 'RESOURCE_ID'.
  l_el_column_layout-inttype = 'C'.
  l_el_column_layout-outputlen = 35.                     "#EC NUMBER_OK
  l_el_column_layout-coltext = TEXT-005.
  l_el_column_layout-seltext = TEXT-005.

  INSERT l_el_column_layout INTO TABLE l_it_column_layout.
  CLEAR l_el_column_layout.

  l_el_column_layout-fieldname = 'RESOURCE_STATE'.
  l_el_column_layout-inttype = 'C'.
  l_el_column_layout-outputlen = 15.                     "#EC NUMBER_OK
  l_el_column_layout-coltext = TEXT-006.
  l_el_column_layout-seltext = TEXT-006.

  INSERT l_el_column_layout INTO TABLE l_it_column_layout.
  CLEAR l_el_column_layout.

  l_el_column_layout-fieldname = 'JOB_ID'.
  l_el_column_layout-inttype = 'C'.
  l_el_column_layout-outputlen = 35.                     "#EC NUMBER_OK
  l_el_column_layout-coltext = TEXT-007.
  l_el_column_layout-seltext = TEXT-007.

  INSERT l_el_column_layout INTO TABLE l_it_column_layout.
  CLEAR l_el_column_layout.

  l_el_column_layout-fieldname = 'JOB_STATE'.
  l_el_column_layout-inttype = 'C'.
  l_el_column_layout-outputlen = 10.                     "#EC NUMBER_OK
  l_el_column_layout-coltext = TEXT-008.
  l_el_column_layout-seltext = TEXT-008.

  INSERT l_el_column_layout INTO TABLE l_it_column_layout.
  CLEAR l_el_column_layout.

  l_el_column_layout-fieldname = 'TIME'.
  l_el_column_layout-inttype = 'T'.
  l_el_column_layout-outputlen = 10.                     "#EC NUMBER_OK
  l_el_column_layout-coltext = TEXT-009.
  l_el_column_layout-seltext = TEXT-009.

  INSERT l_el_column_layout INTO TABLE l_it_column_layout.
  CLEAR l_el_column_layout.

  l_el_column_layout-fieldname = 'DATE'.
  l_el_column_layout-inttype = 'D'.
  l_el_column_layout-outputlen = 10.                     "#EC NUMBER_OK
  l_el_column_layout-coltext = TEXT-010.
  l_el_column_layout-seltext = TEXT-010.

  INSERT l_el_column_layout INTO TABLE l_it_column_layout.
  CLEAR l_el_column_layout.

  PERFORM pbo_010x_init_toolbar CHANGING l_it_fun.

  CALL METHOD g_dynp_0101-grid->set_table_for_first_display
    EXPORTING      "i_structure_name = 'L_JOB_LAY_RES_DATA' "'/GAL/JOB_RESOURCE_DATA'
      is_variant           = l_layout
      i_save               = ''
      is_layout            = l_table_layout
      it_toolbar_excluding = l_it_fun
    CHANGING
      it_fieldcatalog      = l_it_column_layout
      it_outtab            = g_dynp_0101-job_lay_res_datas.

ENDFORM.                    " /gal/pbo_0101_init_table

*----------------------------------------------------------------------*
***INCLUDE /GAL/_MONITOR_F02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  pbo_0101_load_data_into_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  pbo_0101_load_data_into_grid .
  DATA:
    l_job_resource_data TYPE /gal/job_resource_data,
    l_from              TYPE timestamp,
    l_job_lay_res_data  LIKE g_dynp_0101-job_lay_res_data.


  CONVERT DATE g_dynp_0101-tv_from_date TIME g_dynp_0101-tv_from_time
  INTO TIME STAMP l_from TIME ZONE sy-zonlo.

  "read data from tables
  CALL FUNCTION '/GAL/JS_GET_JOB_RESOURCE_DATAS'
    EXPORTING
      rfc_route_info     = g_dynp_0100-rfc_route_info
      from               = l_from
      resource_id        = g_dynp_0101-tv_resource_id
      assigned           = g_dynp_0101-cb_assigned
      requested          = g_dynp_0101-cb_requested
      free               = g_dynp_0101-cb_free
    IMPORTING
      job_resource_datas = g_dynp_0101-job_resource_datas
    EXCEPTIONS
      rfc_exception      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

  CLEAR g_dynp_0101-job_lay_res_datas.
  LOOP AT g_dynp_0101-job_resource_datas INTO l_job_resource_data.
    IF l_job_resource_data-job_state EQ 'R'.
      l_job_lay_res_data-line_color = 'C311'."intensive yellow
    ELSE.
      l_job_lay_res_data-line_color = ''.
    ENDIF.
    DATA l_dd07v_tab TYPE TABLE OF dd07v.
    DATA l_dd07v LIKE LINE OF l_dd07v_tab.
    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = '/GAL/PRECONDITION_STATUS'
        state         = 'A'
        langu         = sy-langu
      TABLES
        dd07v_tab     = l_dd07v_tab
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    READ TABLE l_dd07v_tab WITH KEY domvalue_l = l_job_resource_data-resource_state INTO l_dd07v.
    l_job_lay_res_data-resource_state = l_dd07v-ddtext.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = '/GAL/JOB_STATUS'
        state         = 'A'
        langu         = sy-langu
      TABLES
        dd07v_tab     = l_dd07v_tab
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    READ TABLE l_dd07v_tab WITH KEY domvalue_l = l_job_resource_data-job_state INTO l_dd07v.
    l_job_lay_res_data-job_state = l_dd07v-ddtext.

    l_job_lay_res_data-resource_id = l_job_resource_data-resource_id.
    l_job_lay_res_data-job_id = l_job_resource_data-job_id.

    CONVERT TIME STAMP l_job_resource_data-mod_timestamp TIME ZONE sy-zonlo
    INTO DATE l_job_lay_res_data-date TIME l_job_lay_res_data-time.

    INSERT l_job_lay_res_data INTO TABLE g_dynp_0101-job_lay_res_datas.
  ENDLOOP.

  CALL METHOD g_dynp_0101-grid->refresh_table_display.
ENDFORM.                    " pbo_0101_load_data_into_grid

*&---------------------------------------------------------------------*
*&      Form  pbo_0102_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pbo_0102_init.
  IF g_dynp_0102-custom_container IS INITIAL.
    CREATE OBJECT g_dynp_0102-custom_container
      EXPORTING
        container_name = 'CC_JOB_TABLE'.

    CREATE OBJECT g_dynp_0102-grid
      EXPORTING
        i_parent = g_dynp_0102-custom_container.

    CREATE OBJECT g_dynp_0102-jm_event_receiver.
    SET HANDLER g_dynp_0102-jm_event_receiver->handle_user_command FOR g_dynp_0102-grid.
    SET HANDLER g_dynp_0102-jm_event_receiver->handle_cm_request FOR g_dynp_0102-grid.

    g_dynp_0102-cb_type_i = abap_true.
    g_dynp_0102-cb_type_s = abap_true.

    g_dynp_0102-cb_status_i = abap_true.
    g_dynp_0102-cb_status_w = abap_true.
    g_dynp_0102-cb_status_r = abap_true.
    g_dynp_0102-cb_status_s = abap_true.
    g_dynp_0102-cb_status_e = abap_true.

    PERFORM pbo_0102_init_table.
    PERFORM pbo_0102_load_data_into_grid.
  ENDIF.
ENDFORM.                    "pbo_0102_init

*&---------------------------------------------------------------------*
*&      Form  pbo_0102_init_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pbo_0102_init_table .
  DATA:
    l_layout           TYPE disvariant,
    l_table_layout     TYPE lvc_s_layo,
    l_it_column_layout TYPE lvc_t_fcat,
    l_el_column_layout TYPE lvc_s_fcat,
    l_it_fun           TYPE ui_functions.

  "Load data INTO TABLE  the grid and display them
  l_table_layout-sel_mode = 'C'.

  l_layout-report = sy-repid.

  l_table_layout-info_fname = 'LINE_COLOR'.

  l_el_column_layout-fieldname = 'ID'.
  l_el_column_layout-inttype = 'C'.
  l_el_column_layout-outputlen = 35.                     "#EC NUMBER_OK
  l_el_column_layout-coltext = TEXT-011.
  l_el_column_layout-seltext = TEXT-011.

  INSERT l_el_column_layout INTO TABLE l_it_column_layout.
  CLEAR l_el_column_layout.

  l_el_column_layout-fieldname = 'NAME'.
  l_el_column_layout-inttype = 'C'.
  l_el_column_layout-outputlen = 20.                     "#EC NUMBER_OK
  l_el_column_layout-coltext = TEXT-012.
  l_el_column_layout-seltext = TEXT-012.

  INSERT l_el_column_layout INTO TABLE l_it_column_layout.
  CLEAR l_el_column_layout.

  l_el_column_layout-fieldname = 'COUNT'.
  l_el_column_layout-inttype = 'C'.
  l_el_column_layout-outputlen = 12.                     "#EC NUMBER_OK
  l_el_column_layout-coltext = TEXT-016.
  l_el_column_layout-seltext = TEXT-016.

  INSERT l_el_column_layout INTO TABLE l_it_column_layout.
  CLEAR l_el_column_layout.

  l_el_column_layout-fieldname = 'TYPE'.
  l_el_column_layout-inttype = 'C'.
  l_el_column_layout-outputlen = 10.                     "#EC NUMBER_OK
  l_el_column_layout-coltext = TEXT-013.
  l_el_column_layout-seltext = TEXT-013.

  INSERT l_el_column_layout INTO TABLE l_it_column_layout.
  CLEAR l_el_column_layout.

  l_el_column_layout-fieldname = 'DESTINATION'.
  l_el_column_layout-inttype = 'C'.
  l_el_column_layout-outputlen = 20.                     "#EC NUMBER_OK
  l_el_column_layout-coltext = TEXT-014.
  l_el_column_layout-seltext = TEXT-014.

  INSERT l_el_column_layout INTO TABLE l_it_column_layout.
  CLEAR l_el_column_layout.

  l_el_column_layout-fieldname = 'STATUS'.
  l_el_column_layout-inttype = 'C'.
  l_el_column_layout-outputlen = 10.                     "#EC NUMBER_OK
  l_el_column_layout-coltext = TEXT-015.
  l_el_column_layout-seltext = TEXT-015.

  INSERT l_el_column_layout INTO TABLE l_it_column_layout.
  CLEAR l_el_column_layout.

  l_el_column_layout-fieldname = 'TIME'.
  l_el_column_layout-inttype = 'T'.
  l_el_column_layout-outputlen = 10.                     "#EC NUMBER_OK
  l_el_column_layout-coltext = TEXT-009.
  l_el_column_layout-seltext = TEXT-009.

  INSERT l_el_column_layout INTO TABLE l_it_column_layout.
  CLEAR l_el_column_layout.

  l_el_column_layout-fieldname = 'DATE'.
  l_el_column_layout-inttype = 'D'.
  l_el_column_layout-outputlen = 10.                     "#EC NUMBER_OK
  l_el_column_layout-coltext = TEXT-010.
  l_el_column_layout-seltext = TEXT-010.

  INSERT l_el_column_layout INTO TABLE l_it_column_layout.
  CLEAR l_el_column_layout.

  PERFORM pbo_010x_init_toolbar CHANGING l_it_fun.

  CALL METHOD g_dynp_0102-grid->set_table_for_first_display
    EXPORTING
      is_variant           = l_layout
      i_save               = ''
      is_layout            = l_table_layout
      it_toolbar_excluding = l_it_fun
    CHANGING
      it_fieldcatalog      = l_it_column_layout
      it_outtab            = g_dynp_0102-job_lay_datas.

ENDFORM.                    " /gal/pbo_0101_init_table

*&---------------------------------------------------------------------*
*&      Form  pbo_010x_init_toolbar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--IT_FUNCTIONS  text
*----------------------------------------------------------------------*
FORM pbo_010x_init_toolbar
  CHANGING p_t_functions TYPE ui_functions.

  APPEND cl_gui_alv_grid=>mc_fc_maximum TO p_t_functions.
  APPEND cl_gui_alv_grid=>mc_fc_minimum TO p_t_functions.
  APPEND cl_gui_alv_grid=>mc_fc_subtot  TO p_t_functions.
  APPEND cl_gui_alv_grid=>mc_fc_sum     TO p_t_functions.
  APPEND cl_gui_alv_grid=>mc_fc_print   TO p_t_functions.
  APPEND cl_gui_alv_grid=>mc_fc_info    TO p_t_functions.
  APPEND cl_gui_alv_grid=>mc_fc_graph   TO p_t_functions.
  APPEND cl_gui_alv_grid=>mc_fc_average TO p_t_functions.
  APPEND cl_gui_alv_grid=>mc_mb_variant TO p_t_functions.
  "APPEND cl_gui_alv_grid=>mc_fg_sort TO it_functions.
  "APPEND cl_gui_alv_grid=>mc_fc_filter TO it_functions.
  "APPEND cl_gui_alv_grid=>mc_fc_detail TO it_functions.

ENDFORM.                    "pbo_010x_init_toolbar

*&---------------------------------------------------------------------*
*&      Form  pbo_0102_load_data_into_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM  pbo_0102_load_data_into_grid .
  DATA:
    l_job_data     TYPE /gal/jobdata01,
    l_from         TYPE timestamp,
    l_till         TYPE timestamp,
    l_job_lay_data LIKE g_dynp_0102-job_lay_data,
    l_description  TYPE string,
    l_dd07v_tab    TYPE TABLE OF dd07v,
    l_dd07v        LIKE LINE OF l_dd07v_tab.


  CONVERT DATE g_dynp_0102-tv_from_date TIME g_dynp_0102-tv_from_time
  INTO TIME STAMP l_from TIME ZONE sy-zonlo.

  CONVERT DATE g_dynp_0102-tv_to_date TIME g_dynp_0102-tv_to_time
  INTO TIME STAMP l_till TIME ZONE sy-zonlo.

  "read data from tables
  CALL FUNCTION '/GAL/JS_GET_JOBS'
    EXPORTING
      rfc_route_info = g_dynp_0100-rfc_route_info
      name           = g_dynp_0102-tv_job_name
      destination    = g_dynp_0102-tv_destination
      status_i       = g_dynp_0102-cb_status_i
      status_w       = g_dynp_0102-cb_status_w
      status_r       = g_dynp_0102-cb_status_r
      status_s       = g_dynp_0102-cb_status_s
      status_f       = g_dynp_0102-cb_status_f
      status_e       = g_dynp_0102-cb_status_e
      type_i         = g_dynp_0102-cb_type_i
      type_s         = g_dynp_0102-cb_type_s
      from           = l_from
      till           = l_till
      job_id         = g_dynp_0102-tv_job_id
    IMPORTING
      job_datas      = g_dynp_0102-job_datas
    EXCEPTIONS
      rfc_exception  = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR g_dynp_0102-job_lay_datas.
  LOOP AT g_dynp_0102-job_datas INTO l_job_data.

    /gal/job=>get_jobtype_description(
      EXPORTING
        classname   = l_job_data-classname
      IMPORTING
        description = l_description
    ).
    l_job_lay_data-type = l_description.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = '/GAL/JOB_STATUS'
        state         = 'A'
        langu         = sy-langu
      TABLES
        dd07v_tab     = l_dd07v_tab
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    READ TABLE l_dd07v_tab WITH KEY domvalue_l = l_job_data-status INTO l_dd07v.
    l_job_lay_data-status = l_dd07v-ddtext.

    CONVERT TIME STAMP l_job_data-mod_timestamp TIME ZONE sy-zonlo
    INTO DATE l_job_lay_data-date TIME l_job_lay_data-time.

    l_job_lay_data-id = l_job_data-id.
    l_job_lay_data-name = l_job_data-job_name.
    l_job_lay_data-count = l_job_data-job_count.
    l_job_lay_data-destination = l_job_data-destination.

    INSERT l_job_lay_data INTO TABLE g_dynp_0102-job_lay_datas.
  ENDLOOP.

  CALL METHOD g_dynp_0102-grid->refresh_table_display.
ENDFORM.                    " pbo_0101_load_data_into_grid
