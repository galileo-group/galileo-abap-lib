*&---------------------------------------------------------------------*
*& Report  /GAL/JOB_MONITOR
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  /gal/job_monitor.

TYPE-POOLS icon.
TYPE-POOLS cxtab.
TYPE-POOLS abap.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_jm_event_receiver DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
    handle_cm_request FOR EVENT context_menu_request OF cl_gui_alv_grid
    IMPORTING
      e_object,

    handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
    IMPORTING
      e_ucomm.
  PRIVATE SECTION.
    DATA rows TYPE lvc_t_row.
    DATA row TYPE LINE OF lvc_t_row.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_0102_jm_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_0102_jm_event_receiver DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
    handle_cm_request FOR EVENT context_menu_request OF cl_gui_alv_grid
    IMPORTING
      e_object,

    handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
    IMPORTING
      e_ucomm.
  PRIVATE SECTION.
    DATA rows TYPE lvc_t_row.
    DATA row TYPE LINE OF lvc_t_row.
ENDCLASS.                    "lcl_event_receiver DEFINITION

DATA BEGIN OF g_dynp_0100.
DATA  rfc_route_info TYPE /gal/rfc_route_info.
DATA  subscr_dynpro  TYPE sy-dynnr.
DATA END OF g_dynp_0100.

DATA BEGIN OF g_dynp_0101.
DATA:
      cb_assigned TYPE abap_bool,
      cb_requested TYPE abap_bool,
      cb_free TYPE abap_bool,
      tv_resource_id TYPE /gal/resource_string,
      tv_from_date TYPE dats,
      tv_from_time TYPE tims,

      job_resource_data TYPE /gal/job_resource_data,
      job_resource_datas TYPE /gal/job_resource_datas,

      grid TYPE REF TO cl_gui_alv_grid,
      custom_container TYPE REF TO cl_gui_custom_container,
      jm_event_receiver TYPE REF TO lcl_jm_event_receiver.


DATA BEGIN OF job_lay_res_data.
DATA:
      resource_id TYPE /gal/resource_string,
      resource_state(20) TYPE c,
      job_id TYPE /gal/job_id,
      job_state(20) TYPE c,
      time TYPE t,
      date TYPE d,
      line_color(4) TYPE c.
DATA END OF job_lay_res_data.
DATA job_lay_res_datas LIKE STANDARD TABLE OF g_dynp_0101-job_lay_res_data.

DATA END OF g_dynp_0101.
CONTROLS tab TYPE TABSTRIP.

DATA BEGIN OF g_dynp_0102.
DATA:
      cb_type_i TYPE abap_bool,
      cb_type_s TYPE abap_bool,

      cb_status_i TYPE abap_bool,
      cb_status_w TYPE abap_bool,
      cb_status_r TYPE abap_bool,
      cb_status_s TYPE abap_bool,
      cb_status_f TYPE abap_bool,
      cb_status_e TYPE abap_bool,

      tv_job_id TYPE /gal/job_id,
      tv_destination TYPE /gal/rfc_destination,
      tv_job_name TYPE btcjob,
      tv_job_count TYPE btcjobcnt,
      tv_from_date TYPE dats,
      tv_from_time TYPE tims,
      tv_to_date TYPE dats,
      tv_to_time TYPE tims,


      job_data TYPE /gal/jobdata01,
      job_datas TYPE /gal/job_datas,

      subscr_dynpro   TYPE sy-dynnr,

      grid TYPE REF TO cl_gui_alv_grid,
      custom_container TYPE REF TO cl_gui_custom_container,
      jm_event_receiver TYPE REF TO lcl_0102_jm_event_receiver.


DATA BEGIN OF job_lay_data.
DATA:
      id TYPE /gal/job_id,
      type(20) TYPE c, "/gal/job_type,
      destination TYPE /gal/rfc_destination,"search
      status(20) TYPE c, "/gal/job_status, "search
      time TYPE t, "search
      date TYPE d, "search
      name TYPE btcjob, "search
      count TYPE btcjobcnt,
      line_color(4) TYPE c.
DATA END OF job_lay_data.
DATA job_lay_datas LIKE STANDARD TABLE OF g_dynp_0102-job_lay_data.

DATA END OF g_dynp_0102.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_jm_event_receiver IMPLEMENTATION.
  METHOD handle_cm_request.
    DATA l_disable_approval TYPE abap_bool.
    DATA lt_fcodes TYPE ui_functions.

    CALL METHOD e_object->clear.

    CALL METHOD g_dynp_0101-grid->get_selected_rows
      IMPORTING
        et_index_rows = rows.
    LOOP AT rows INTO row.
      READ TABLE g_dynp_0101-job_resource_datas INDEX row-index
        INTO g_dynp_0101-job_resource_data.
      IF g_dynp_0101-job_resource_data-job_state NE 'R'.
        l_disable_approval = abap_true.
      ENDIF.
    ENDLOOP.
    IF sy-subrc = 0.
      CALL METHOD e_object->add_function "e_object TYPE REF OF CL_CTMENU
        EXPORTING
          fcode = 'APPROVAL'
          text  = text-000.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'STATUS' " js_update_resources(resource_id)
          text  = text-001.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'DELETE'
          text  = text-002.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'JUMP_TO_JOB'
          text  = text-021.
      CLEAR lt_fcodes.
      IF l_disable_approval = abap_true.
        APPEND 'APPROVAL' TO lt_fcodes.
      ENDIF.
      CALL METHOD e_object->disable_functions
        EXPORTING
          fcodes = lt_fcodes.
    ENDIF.
  ENDMETHOD.                    "handle_cm_request

  METHOD handle_user_command.
    DATA l_cx_js_exception TYPE REF TO /gal/cx_js_exception.
    CASE e_ucomm.
      WHEN 'APPROVAL'.
        DATA:
         l_job TYPE REF TO /gal/job,
         l_error_text TYPE string.
        TRY.
            CALL METHOD /gal/job=>read_job_from_db
              EXPORTING
                id  = g_dynp_0101-job_resource_data-job_id
              RECEIVING
                job = l_job.
            CALL METHOD l_job->post_process.
            CALL METHOD /gal/job=>run_job_scheduler.
            PERFORM pbo_0101_load_data_into_grid.
          CATCH /gal/cx_js_exception INTO l_cx_js_exception.
            l_error_text = l_cx_js_exception->get_text( ).
            MESSAGE l_error_text TYPE 'W'.
        ENDTRY.

      WHEN 'STATUS'.
        CALL FUNCTION '/GAL/JS_UPDATE_RESOURCES'
          EXPORTING
            rfc_route_info  = g_dynp_0100-rfc_route_info
            resource_string = g_dynp_0101-job_resource_data-resource_id
          EXCEPTIONS
            rfc_exception   = 1
            OTHERS          = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        PERFORM pbo_0101_load_data_into_grid.

      WHEN 'DELETE'.
        DATA l_result TYPE c.

        CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
          EXPORTING
            titel          = text-003
            textline1      = text-004
            textline2      = text-020
            defaultoption  = 'N'
            cancel_display = abap_false
          IMPORTING
            answer         = l_result.
        IF l_result <> 'N'.
          CALL FUNCTION '/GAL/JS_DB_DELETE'
            EXPORTING
              rfc_route_info = g_dynp_0100-rfc_route_info
              table_name     = '/GAL/JOBDATA02'
              id             = g_dynp_0101-job_resource_data-precondition_id
            EXCEPTIONS
              rfc_exception  = 1
              OTHERS         = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            RETURN.
          ENDIF.

          CALL FUNCTION '/GAL/JS_DB_DELETE'
            EXPORTING
              rfc_route_info = g_dynp_0100-rfc_route_info
              table_name     = '/GAL/JOBDATA02R'
              id             = g_dynp_0101-job_resource_data-precondition_id
            EXCEPTIONS
              rfc_exception  = 1
              OTHERS         = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            RETURN.
          ENDIF.
          TRY.
              CALL METHOD /gal/job=>run_job_scheduler.
            CATCH /gal/cx_js_exception INTO l_cx_js_exception.
              l_error_text = l_cx_js_exception->get_text( ).
              MESSAGE l_error_text TYPE 'W'.
          ENDTRY.
          PERFORM pbo_0101_load_data_into_grid.
        ENDIF.
      WHEN 'JUMP_TO_JOB'.
        g_dynp_0102-cb_status_i = abap_true.
        g_dynp_0102-cb_status_w = abap_true.
        g_dynp_0102-cb_status_r = abap_true.
        g_dynp_0102-cb_status_s = abap_true.
        g_dynp_0102-cb_status_f = abap_true.
        g_dynp_0102-cb_status_e = abap_true.
        g_dynp_0102-cb_type_i = abap_true.
        g_dynp_0102-cb_type_s = abap_true.
        g_dynp_0102-tv_job_id = g_dynp_0101-job_resource_data-job_id.

        g_dynp_0100-subscr_dynpro = '0102'.
        tab-activetab = 'TAB_J'.
        CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'.

        PERFORM  pbo_0102_load_data_into_grid.
    ENDCASE.

  ENDMETHOD.                    "handle_user_command
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_jm_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_0102_jm_event_receiver IMPLEMENTATION.
  METHOD handle_cm_request.
    DATA l_disable_post_processing TYPE abap_bool.
    DATA lt_fcodes TYPE ui_functions.

    CALL METHOD e_object->clear.

    CALL METHOD g_dynp_0102-grid->get_selected_rows
      IMPORTING
        et_index_rows = rows.
    LOOP AT rows INTO row.
      READ TABLE g_dynp_0102-job_datas INDEX row-index
        INTO g_dynp_0102-job_data.
      IF g_dynp_0102-job_data-status NE 'R'.
        l_disable_post_processing = abap_true.
      ENDIF.
    ENDLOOP.
    IF sy-subrc = 0.
      CALL METHOD e_object->add_function "e_object TYPE REF OF CL_CTMENU
      EXPORTING
        fcode = 'REBOOKING'
        text  = text-017.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'DELETE'
          text  = text-002.

      CLEAR lt_fcodes.
      IF l_disable_post_processing = abap_true.
        APPEND 'REBOOKING' TO lt_fcodes.
      ENDIF.
      CALL METHOD e_object->disable_functions
        EXPORTING
          fcodes = lt_fcodes.
    ENDIF.
  ENDMETHOD.                    "handle_cm_request

  METHOD handle_user_command.
    DATA: l_cx_js_exception TYPE REF TO /gal/cx_js_exception,
          l_job TYPE REF TO /gal/job,
          l_error_text TYPE string.
    CASE e_ucomm.
      WHEN 'REBOOKING'.
        DATA l_result TYPE c.

        CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
          EXPORTING
            titel          = text-018
            textline1      = text-019
            textline2      = text-020
            defaultoption  = 'N'
            cancel_display = abap_false
          IMPORTING
            answer         = l_result.
        IF l_result = 'J'.
          TRY.
              CALL METHOD /gal/job=>read_job_from_db
                EXPORTING
                  id  = g_dynp_0102-job_data-id
                RECEIVING
                  job = l_job.
              CALL METHOD l_job->post_process.
              CALL METHOD /gal/job=>run_job_scheduler.
              PERFORM pbo_0102_load_data_into_grid.
              IF g_dynp_0101-grid IS NOT INITIAL.
                PERFORM pbo_0101_load_data_into_grid.
              ENDIF.
            CATCH /gal/cx_js_exception INTO l_cx_js_exception.
              l_error_text = l_cx_js_exception->get_text( ).
              MESSAGE l_error_text TYPE 'I'.
          ENDTRY.
        ENDIF.
      WHEN 'DELETE'.
        CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
          EXPORTING
            titel          = text-022
            textline1      = text-023
            defaultoption  = 'N'
            cancel_display = abap_false
          IMPORTING
            answer         = l_result.
        IF l_result = 'J'.
          TRY.
              CALL METHOD /gal/job=>read_job_from_db
                EXPORTING
                  id  = g_dynp_0102-job_data-id
                RECEIVING
                  job = l_job.
              CALL METHOD l_job->delete_from_db
                EXPORTING
                  force = abap_true.
              CALL METHOD /gal/job=>run_job_scheduler.
              PERFORM pbo_0102_load_data_into_grid.
              IF g_dynp_0101-grid IS NOT INITIAL.
                PERFORM pbo_0101_load_data_into_grid.
              ENDIF.
            CATCH /gal/cx_js_exception INTO l_cx_js_exception.
              l_error_text = l_cx_js_exception->get_text( ).
              MESSAGE l_error_text TYPE 'I'.
          ENDTRY.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "handle_user_command
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

START-OF-SELECTION.


*  g_dynp_0100-subscr_dynpro = 0101.
*  tab-activetab = 'TAB_R'.
  CALL SCREEN 0100.

  INCLUDE /gal/job_monitor_m_pai.

  INCLUDE /gal/job_monitor_m_pbo.

  INCLUDE /gal/job_monitor_s_pai.

  INCLUDE /gal/job_monitor_s_pbo.
