*&---------------------------------------------------------------------*
*&  Include           /GAL/CFW_MACROS
*&---------------------------------------------------------------------*
*& This include provides a set of macros to make the use of the Galileo
*& Communication Framework as simple as possible.
*&---------------------------------------------------------------------*

TYPE-POOLS abap.



DEFINE cfw_custom_auth.
  DATA __l_custom_auth_behave TYPE /gal/cfw_custom_auth_behave. "#EC NEEDED
  __l_custom_auth_behave = &1.
END-OF-DEFINITION.




*-----------------------------------------------------------------------
* Follow RFC route
*-----------------------------------------------------------------------
DEFINE cfw_follow_rfc_route.
  DATA __l_interface_info  TYPE /gal/func_interface_info.

  DATA __lt_param_bindings TYPE abap_func_parmbind_tab.
  DATA __l_param_binding   LIKE LINE OF __lt_param_bindings.

  DATA __l_exception_name  TYPE string.                     "#EC NEEDED

  DATA __l_rfc_route_info  TYPE /gal/rfc_route_info.
  DATA __l_function_name   TYPE string.
  DATA __lt_call_stack     TYPE abap_callstack.

  DATA __l_task            TYPE REF TO /gal/task.

  DATA __l_exception       TYPE REF TO cx_root.
  DATA __l_exception_info  TYPE /gal/exception_info.
  DATA __l_exception_part1 TYPE symsgv.                     "#EC NEEDED
  DATA __l_exception_part2 TYPE symsgv.                     "#EC NEEDED
  DATA __l_exception_part3 TYPE symsgv.                     "#EC NEEDED
  DATA __l_func_ex_info    TYPE symsgv.                     "#EC NEEDED
  DATA __l_auth_active     TYPE abap_bool.
  DATA __l_local_check_req TYPE abap_bool.
  DATA __l_auth_excep      TYPE REF TO /gal/cx_cfw_auth_exception.
  DATA __l_auth_excep_fw   TYPE REF TO /gal/cx_cfw_auth_excep_fw.
  DATA __l_ctx_data_set    TYPE abap_bool.

  FIELD-SYMBOLS <__l_param_info>          TYPE /gal/func_interface_param_info.
  FIELD-SYMBOLS <__l_param_value_local>   TYPE any.
  FIELD-SYMBOLS <__l_rfc_route_step_info> TYPE /gal/rfc_route_step_info.
  FIELD-SYMBOLS <__l_custom_auth_behave>  TYPE /gal/cfw_custom_auth_behave.

* Define variables for break-point support
  cfw_break_point_support.

* Initialize working copy of RFC route info before executing first step
  __l_rfc_route_info = &1.

* Initialize control variables
  CLEAR __lt_param_bindings.
  CLEAR __lt_call_stack.
  CLEAR __l_exception_info.
  CLEAR __l_exception_part1.
  CLEAR __l_exception_part2.
  CLEAR __l_exception_part3.
  __l_local_check_req = abap_false.
  __l_ctx_data_set = abap_false.

* A custom authority check behavior can be defined by using the macro cfw_Custom_auth in the function module declaration.
* The behavior is stored in field symbol <__l_custom_auth_behave> if existing otherwise default is set.
  ASSIGN ('__L_CUSTOM_AUTH_BEHAVE') TO <__l_custom_auth_behave>.
  IF sy-subrc <> 0.
  DATA __l_custom_auth_behave_default TYPE /gal/cfw_custom_auth_behave VALUE /gal/cfw_auth=>const_cab_final_rfc_step.
  ASSIGN __l_custom_auth_behave_default TO <__l_custom_auth_behave>.
  ENDIF.

  TRY.

  IF &1-current_step = 0 AND
  &1-step_infos IS INITIAL AND
  <__l_custom_auth_behave> NE /gal/cfw_auth=>const_cab_no_check AND
  <__l_custom_auth_behave> NE /gal/cfw_auth=>const_cab_final_rfc_step.
* If we are at the initial step and no RFC steps are to be done, checks are only done if required by auth behavior definition.
  __l_local_check_req = abap_true.
  ENDIF.

  IF &1-current_step = 0 AND  &1-step_infos IS NOT INITIAL OR __l_local_check_req = abap_true.
* Initialization is needed if:
* - we are at the initial step and RFC step follows
* or
* - a local check is needed (only possible ast initial step)

* Get the function name of current scope (which is to be checked)
  /gal/cfw_helper=>get_function_name_ext(
  IMPORTING
  function_name = __l_function_name
  call_stack    = __lt_call_stack
  ).

* Determine function interface
  __l_interface_info = /gal/cfw_helper=>get_function_interface(
  function_name       = __l_function_name
  check_compatibility = abap_true
  ).

* Create parameter binding table for IMPORTING and CHANGING parameters
  LOOP AT __l_interface_info-param_info ASSIGNING <__l_param_info>
  WHERE parameter_kind CA 'IC'
  AND ( parameter_name <> `RFC_ROUTE_INFO` OR parameter_type <> `/GAL/RFC_ROUTE_INFO` ). "#EC CI_STDSEQ

  __l_param_binding-name = <__l_param_info>-parameter_name.
  __l_param_binding-kind = <__l_param_info>-parameter_kind_int.

  ASSIGN (<__l_param_info>-parameter_name) TO <__l_param_value_local>. "#EC CI_SUBRC
  GET REFERENCE OF <__l_param_value_local> INTO __l_param_binding-value.

  INSERT __l_param_binding INTO TABLE __lt_param_bindings.
  ENDLOOP.

* Add EXPORTING parameters with generic type to parameter binding table
  LOOP AT __l_interface_info-param_info ASSIGNING <__l_param_info>
  WHERE parameter_kind CA 'E'
  AND (
  parameter_type = `ANY`            OR
  parameter_type = `ANY TABLE`      OR
  parameter_type = `C`              OR
  parameter_type = `CLIKE`          OR
  parameter_type = `CSEQUENCE`      OR
  parameter_type = `DATA`           OR
  parameter_type = `HASHED TABLE`   OR
  parameter_type = `INDEX TABLE`    OR
  parameter_type = `N`              OR
  parameter_type = `NUMERIC`        OR
  parameter_type = `OBJECT`         OR
  parameter_type = `P`              OR
  parameter_type = `SIMPLE`         OR
  parameter_type = `SORTED TABLE`   OR
  parameter_type = `STANDARD TABLE` OR
  parameter_type = `TABLE`          OR
  parameter_type = `X`              OR
  parameter_type = `XSEQUENCE`
  ).                                                     "#EC CI_STDSEQ

  __l_param_binding-name = <__l_param_info>-parameter_name.
  __l_param_binding-kind = <__l_param_info>-parameter_kind_int.

  ASSIGN (<__l_param_info>-parameter_name) TO <__l_param_value_local>. "#EC CI_SUBRC
  CREATE DATA __l_param_binding-value LIKE <__l_param_value_local>.

  INSERT __l_param_binding INTO TABLE __lt_param_bindings.
  ENDLOOP.

  ENDIF.

* Check if the authentification framework is active
  __l_auth_active = /gal/cfw_auth_config=>is_active( ).
  IF __l_auth_active = abap_true AND __l_local_check_req = abap_true.
* If authentification framework is active and we need to perform a local check, the required data is initialized now.
  /gal/cfw_auth=>init( ).
  /gal/cfw_auth=>set_context_data(
  EXPORTING
  function_name                  = __l_function_name
  param_bindings                 = __lt_param_bindings
  initial_caller_sysysid         = sy-sysid
  initial_caller_symandt         = sy-mandt
  initial_caller_syuname         = sy-uname
  last_caller_sysysid            = sy-sysid
  last_caller_symandt            = sy-mandt
  last_caller_syuname            = sy-uname
  local_execution                = abap_true
  first_step                     = abap_true
  final_step                     = abap_true
  rfc_route_info                 = &1
  direct_call                    = abap_true
  ).
  __l_ctx_data_set = abap_true.
  ENDIF.

  IF ( NOT &1-step_infos IS INITIAL AND &1-current_step > 0 ) OR __l_local_check_req = abap_true.
* Check authentification if required (valid for local and remote scopes).
* Note: in RFC scope context hast been initialized in function module /GAL/RFC_PROXY_FUNCTION.
  /gal/cfw_auth=>check_auth( <__l_custom_auth_behave> ).
  IF __l_local_check_req = abap_true.
* Handle authentification excdeptions if we are in a local call.
* In RFC context this handling is done in function module /GAL/RFC_PROXY_FUNCTION.
  /gal/cfw_auth=>raise_auth_exception_if_exists( ).
  IF __l_auth_active = abap_true.
* Reset context after handling is done.
  /gal/cfw_auth=>reset_context_data( ).
  ENDIF.
  RETURN.
  ENDIF.
  ENDIF.

* Skip logic if not at initial step
  IF &1-current_step = 0.
  /gal/cfw_helper=>initialize_rfc_route_info( CHANGING rfc_route_info = __l_rfc_route_info ).

  INSERT INITIAL LINE INTO __l_rfc_route_info-step_infos INDEX 1.

  __l_rfc_route_info-current_step = 1.

* Get name of current function
  IF __l_function_name IS INITIAL.
  /gal/cfw_helper=>get_function_name_ext(
  IMPORTING
  function_name = __l_function_name
  call_stack    = __l_rfc_route_info-call_stack
  ).
  ELSE.
  __l_rfc_route_info-call_stack = __lt_call_stack.
  ENDIF.

  ENDIF.

* Skip logic if execution on local system is requested
  IF &1-current_step = 0 AND &1-step_infos IS NOT INITIAL.

* Create task
  CREATE OBJECT __l_task
  EXPORTING
  function       = __l_function_name
  rfc_route_info = __l_rfc_route_info.

* Perform synchronous remote function call
  __l_task->set_parameters( __lt_param_bindings ).

  __l_task->start_sync( ).

  __lt_param_bindings = __l_task->parameters_out.
  __l_rfc_route_info  = __l_task->rfc_route_info.
  __l_exception_info  = __l_task->exception_info.

* Copy results to EXPORTING and CHANGING parameters
  IF __l_exception_info-exception_type IS INITIAL.
  LOOP AT __l_interface_info-param_info ASSIGNING <__l_param_info> WHERE parameter_kind CA 'EC'. "#EC CI_STDSEQ

  ASSIGN (<__l_param_info>-parameter_name) TO <__l_param_value_local>.
  CHECK sy-subrc = 0.

  __l_task->get_parameter(
  EXPORTING
  kind  = <__l_param_info>-parameter_kind_int
  name  = <__l_param_info>-parameter_name
  IMPORTING
  value = <__l_param_value_local>
  ).

  ENDLOOP.
  ENDIF.

* Free reference to task object
  CLEAR __l_task.

  __l_rfc_route_info-current_step = 0.

  ELSE.

  READ TABLE __l_rfc_route_info-step_infos INDEX 1 ASSIGNING <__l_rfc_route_step_info>.
  /gal/cfw_helper=>update_rfc_route_step_info( CHANGING rfc_route_step_info = <__l_rfc_route_step_info> ).

  ENDIF.


* Now authentification exceptions are handled
  CATCH /gal/cx_cfw_auth_exception INTO __l_auth_excep.
  IF __l_ctx_data_set = abap_true.
  TRY.
  /gal/cfw_auth=>reset_context_data( force_auth_exception_discard = abap_true ).
  CATCH /gal/cx_cfw_auth_excep_fw.                      "#EC NO_HANDLER
  ENDTRY.
  ENDIF.

  __l_exception_info = /gal/cfw_auth=>fill_cfw_auth_err_ex_info( __l_auth_excep ).

* Finally all other exceptions are handled
  CATCH cx_static_check INTO __l_exception.
  IF __l_ctx_data_set = abap_true.
  TRY.
  /gal/cfw_auth=>reset_context_data( ).
  __l_exception_info = /gal/cfw_helper=>get_exception_info( __l_exception ).
  CATCH /gal/cx_cfw_auth_excep_fw INTO __l_auth_excep_fw.
  __l_exception_info = /gal/cfw_auth=>fill_cfw_auth_err_ex_info( __l_auth_excep_fw ).
  ENDTRY.
  ELSE.
  __l_exception_info = /gal/cfw_helper=>get_exception_info( __l_exception ).
  ENDIF.

  ENDTRY.

END-OF-DEFINITION.

*-----------------------------------------------------------------------
* Exception Handling Header
*-----------------------------------------------------------------------
DEFINE cfw_handle.
  __l_exception_name = `&1`.
  TRANSLATE __l_exception_name TO UPPER CASE.

  IF __l_rfc_route_info-current_step = 0 AND
     (
       __l_exception_name = __l_exception_info-exception_name
       OR
       __l_exception_name = `RFC_EXCEPTION` AND
       __l_exception_info-exception_name = 'SYSTEM_FAILURE'
       OR
       __l_exception_name = `RFC_EXCEPTION` AND
       __l_exception_info-exception_name = 'COMMUNICATION_FAILURE'
       OR
       __l_exception_name = `ANY_EXCEPTION` AND
       __l_exception_info-exception_type IS NOT INITIAL
       OR
       __l_exception_name = `SUCCESS` AND
       __l_exception_info-exception_type IS INITIAL
       OR
       __l_exception_name = `ALL`
     ).
END-OF-DEFINITION.

*-----------------------------------------------------------------------
* Exception Handling Footer
*-----------------------------------------------------------------------
DEFINE cfw_endhandle.
  RETURN.
ENDIF.
END-OF-DEFINITION.

*-----------------------------------------------------------------------
* Exception Handling: Pass message of classic exception
*-----------------------------------------------------------------------
DEFINE cfw_pass_message.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
     RAISING &1.
END-OF-DEFINITION.

*-----------------------------------------------------------------------
* Exception Handling: Pass classic exception
*-----------------------------------------------------------------------
DEFINE cfw_pass_exception.
  cfw_handle &1.
  cfw_raise_exception &1.
  cfw_endhandle.
END-OF-DEFINITION.

*-----------------------------------------------------------------------
* Exception Handling: Raise classic exception
*-----------------------------------------------------------------------
DEFINE cfw_raise_exception.
  IF __l_exception_info-exception_type = 'E'.
    IF __l_exception_info-message_id     IS NOT INITIAL AND
       __l_exception_info-message_type   IS NOT INITIAL AND
       __l_exception_info-message_number IS NOT INITIAL.
      MESSAGE ID __l_exception_info-message_id
            TYPE __l_exception_info-message_type
          NUMBER __l_exception_info-message_number
            WITH __l_exception_info-message_var1
                 __l_exception_info-message_var2
                 __l_exception_info-message_var3
                 __l_exception_info-message_var4
         RAISING &1.
    ELSEIF __l_exception_info-message_text IS NOT INITIAL.
      /gal/string=>string_to_message_vars( EXPORTING input = __l_exception_info-message_text
                                                     IMPORTING msgv1 = __l_exception_part1
                                                               msgv2 = __l_exception_part2
                                                               msgv3 = __l_exception_part3 ).
      CONCATENATE __l_function_name __l_exception_info-exception_name INTO __l_func_ex_info SEPARATED BY `: `.
      MESSAGE e001(/gal/cfw)
         WITH __l_func_ex_info
              __l_exception_part1
              __l_exception_part2
              __l_exception_part3
              RAISING &1.
    ELSE.
      CONCATENATE __l_function_name __l_exception_info-exception_name INTO __l_func_ex_info SEPARATED BY `: `.
      MESSAGE e000(/gal/cfw)
         WITH __l_func_ex_info
              RAISING &1.
    ENDIF.
  ELSE.
    IF __l_exception_info-message_text IS NOT INITIAL.
      /gal/string=>string_to_message_vars( EXPORTING input = __l_exception_info-message_text
                                                     IMPORTING msgv1 = __l_exception_part1
                                                               msgv2 = __l_exception_part2
                                                               msgv3 = __l_exception_part3 ).
      CONCATENATE __l_function_name __l_exception_info-exception_name INTO __l_func_ex_info SEPARATED BY `: `.
      DO 0 TIMES. MESSAGE e001(/gal/cfw) WITH '' '' '' ''. ENDDO.
      MESSAGE ID '/GAL/CFW' TYPE __l_exception_info-message_type NUMBER '001'
            WITH __l_func_ex_info
                 __l_exception_part1
                 __l_exception_part2
                 __l_exception_part3
         RAISING &1.
    ELSE.
      CONCATENATE __l_function_name __l_exception_info-exception_name INTO __l_func_ex_info SEPARATED BY `: `.
      DO 0 TIMES. MESSAGE e000(/gal/cfw) WITH ''. ENDDO.
      MESSAGE ID '/GAL/CFW' TYPE __l_exception_info-message_type NUMBER '000'
            WITH __l_func_ex_info
         RAISING &1.
    ENDIF.
  ENDIF.
END-OF-DEFINITION.

*-----------------------------------------------------------------------
* Header for remote coding
*-----------------------------------------------------------------------
DEFINE cfw_remote_coding.
  IF __l_exception_info-exception_type IS NOT INITIAL.
    IF __l_interface_info-uses_exception_classes IS NOT INITIAL.
      PERFORM cfw_raise_new_exception IN PROGRAM /gal/cfw_forms
        USING __l_exception_info __l_function_name.
    ELSE.
      PERFORM cfw_raise_classic_exception IN PROGRAM /gal/cfw_forms
        USING __l_exception_info __l_function_name.
    ENDIF.
  ENDIF.

  IF __l_rfc_route_info-current_step = 0.
    RETURN. "Finished
  ENDIF.

  DESCRIBE TABLE __l_rfc_route_info-step_infos LINES sy-tfill.
  IF __l_rfc_route_info-current_step < sy-tfill.
    RETURN. "On the way back
  ENDIF.
END-OF-DEFINITION.

*-----------------------------------------------------------------------
* Get exception mode
*-----------------------------------------------------------------------
DEFINE cfw_get_exception_info.
  &1 = __l_exception_info.
END-OF-DEFINITION.

*-----------------------------------------------------------------------
* Get RFC route info
*-----------------------------------------------------------------------
DEFINE cfw_get_rfc_route_info.
  &1 = __l_rfc_route_info.
END-OF-DEFINITION.

*-----------------------------------------------------------------------
* Debugging Server support
*-----------------------------------------------------------------------
DEFINE cfw_break_point_support.
  CONSTANTS __lc_timeout   TYPE i VALUE 3000000.            "#EC NEEDED

  DATA __l_server          TYPE /gal/cfw_debug01.           "#EC NEEDED
  DATA __l_server_found    TYPE abap_bool.                  "#EC NEEDED
  DATA __l_commit_required TYPE abap_bool.                  "#EC NEEDED

  DATA __l_debug_request   TYPE /gal/cfw_debug02.           "#EC NEEDED

  DATA __l_wait_start      TYPE i.                          "#EC NEEDED
  DATA __l_wait_current    TYPE i.                          "#EC NEEDED
  DATA __l_wait_end        TYPE i.                          "#EC NEEDED
  DATA __l_skip            TYPE abap_bool.                  "#EC NEEDED

  DATA __l_dref            TYPE REF TO data.                "#EC NEEDED
  DATA __l_oref            TYPE REF TO object.              "#EC NEEDED

  FIELD-SYMBOLS <__l_fs>        TYPE STANDARD TABLE.        "#EC NEEDED
  FIELD-SYMBOLS <__l_fs2>       TYPE any.                   "#EC NEEDED
  FIELD-SYMBOLS <__l_fs3>       TYPE any.                   "#EC NEEDED
  FIELD-SYMBOLS <__l_step_info> TYPE /gal/rfc_route_step_info. "#EC NEEDED
END-OF-DEFINITION.

*-----------------------------------------------------------------------
* RFC break point
*-----------------------------------------------------------------------
DEFINE cfw_rfc_break_point.

* Get information about source step
  READ TABLE &1-step_infos INDEX 1
       ASSIGNING <__l_step_info>.

* Check for active Debugging servers
  SELECT * FROM /gal/cfw_debug01
           INTO __l_server
           ORDER BY end_date DESCENDING
                    end_time DESCENDING.                "#EC CI_NOWHERE

    IF __l_server-end_date < sy-datum OR
       __l_server-end_date = sy-datum AND __l_server-end_time < sy-uzeit.
      DELETE FROM /gal/cfw_debug01 WHERE server_id = __l_server-server_id. "#EC CI_SUBRC
      DELETE FROM /gal/cfw_debug02 WHERE server_id = __l_server-server_id. "#EC CI_SUBRC

      __l_commit_required = abap_true.
    ELSEIF __l_server_found = abap_false
       AND ( &2 = __l_server-event
             OR
             &2 CP __l_server-event AND &2 <> `/GAL/RFC_PROXY_FUNCTION` ) "#EC BOOL_OK
       AND <__l_step_info>-system_id CP __l_server-source_system_id
       AND <__l_step_info>-client_id CP __l_server-source_client_id
       AND <__l_step_info>-user_id   CP __l_server-source_user_id
       AND sy-sysid                  CP __l_server-target_system_id
       AND sy-mandt                  CP __l_server-target_client_id
       AND sy-uname                  CP __l_server-target_user_id. "#EC USER_OK

      __l_debug_request-server_id = __l_server-server_id.

      CALL FUNCTION 'TH_GET_OWN_WP_NO'
        IMPORTING
          wp_no    = __l_debug_request-process_no
          wp_pid   = __l_debug_request-process_id
          wp_index = __l_debug_request-process_index.

      IF sy-saprl >= '740'.
        CREATE DATA __l_dref TYPE ('SSI_WORKER_LIST').
        ASSIGN __l_dref->* TO <__l_fs>.

        TRY.
            CREATE OBJECT __l_oref TYPE ('CL_SERVER_INFO').

            CALL METHOD __l_oref->('GET_WORKER_LIST')
              EXPORTING
                worker_index = __l_debug_request-process_index
              RECEIVING
                worker_list  = <__l_fs>.

          CATCH cx_root.                                 "#EC CATCH_ALL
            __l_skip = abap_true.

        ENDTRY.

        IF __l_skip = abap_false.
          READ TABLE <__l_fs> INDEX 1 ASSIGNING <__l_fs2>.
          ASSIGN COMPONENT 'LOGON_ID' OF STRUCTURE <__l_fs2> TO <__l_fs3>.

          __l_debug_request-process_logon_id = <__l_fs3>.
        ENDIF.
      ENDIF.

      IF __l_skip = abap_false.
        INSERT /gal/cfw_debug02 FROM __l_debug_request.

        IF sy-subrc = 0.
          __l_server_found    = abap_true.
          __l_commit_required = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDSELECT.                                              "#EC CI_SUBRC

* Commit database changes (if any)
  IF __l_commit_required = abap_true.
    CALL FUNCTION 'DB_COMMIT'.
  ENDIF.

* Wait for Debugging Server if found
  IF __l_server_found = abap_true.
    CLEAR sy-datum.                                       "#EC WRITE_OK

    GET RUN TIME FIELD __l_wait_start.

    __l_wait_end = __l_wait_start + __lc_timeout.

    WHILE sy-datum IS INITIAL.
      GET RUN TIME FIELD __l_wait_current.
      CHECK __l_wait_current >= __l_wait_end.
      GET TIME.
    ENDWHILE.
  ENDIF.
END-OF-DEFINITION.

*-----------------------------------------------------------------------
* Break point
*-----------------------------------------------------------------------
DEFINE cfw_break_point.

* Check for active Debugging servers
  SELECT * FROM /gal/cfw_debug01
           INTO __l_server
           ORDER BY end_date DESCENDING
                    end_time DESCENDING.                "#EC CI_NOWHERE

    IF __l_server-end_date < sy-datum OR
       __l_server-end_date = sy-datum AND __l_server-end_time < sy-uzeit.
      DELETE FROM /gal/cfw_debug01 WHERE server_id = __l_server-server_id. "#EC CI_SUBRC
      DELETE FROM /gal/cfw_debug02 WHERE server_id = __l_server-server_id. "#EC CI_SUBRC

      __l_commit_required = abap_true.
    ELSEIF __l_server_found = abap_false
       AND &1       CP __l_server-event
       AND sy-sysid CP __l_server-source_system_id
       AND sy-mandt CP __l_server-source_client_id
       AND sy-uname CP __l_server-source_user_id
       AND sy-sysid CP __l_server-target_system_id
       AND sy-mandt CP __l_server-target_client_id
       AND sy-uname CP __l_server-target_user_id.          "#EC USER_OK

      __l_debug_request-server_id = __l_server-server_id.

      CALL FUNCTION 'TH_GET_OWN_WP_NO'
        IMPORTING
          wp_no    = __l_debug_request-process_no
          wp_pid   = __l_debug_request-process_id
          wp_index = __l_debug_request-process_index.

      IF sy-saprl >= '740'.
        CREATE DATA __l_dref TYPE ('SSI_WORKER_LIST').
        ASSIGN __l_dref->* TO <__l_fs>.

        TRY.
            CREATE OBJECT __l_oref TYPE ('CL_SERVER_INFO').

            CALL METHOD __l_oref->('GET_WORKER_LIST')
              EXPORTING
                worker_index = __l_debug_request-process_index
              RECEIVING
                worker_list  = <__l_fs>.

          CATCH cx_root.                                 "#EC CATCH_ALL
            __l_skip = abap_true.

        ENDTRY.

        IF __l_skip = abap_false.
          READ TABLE <__l_fs> INDEX 1 ASSIGNING <__l_fs2>.
          ASSIGN COMPONENT 'LOGON_ID' OF STRUCTURE <__l_fs2> TO <__l_fs3>.

          __l_debug_request-process_logon_id = <__l_fs3>.
        ENDIF.
      ENDIF.

      IF __l_skip = abap_false.
        INSERT /gal/cfw_debug02 FROM __l_debug_request.

        IF sy-subrc = 0.
          __l_server_found    = abap_true.
          __l_commit_required = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDSELECT.                                              "#EC CI_SUBRC

* Commit database changes (if any)
  IF __l_commit_required = abap_true.
    CALL FUNCTION 'DB_COMMIT'.
  ENDIF.

* Wait for Debugging Server if found (Timeout: 3 seconds)
  IF __l_server_found = abap_true.
    CLEAR sy-datum.                                       "#EC WRITE_OK

    GET RUN TIME FIELD __l_wait_start.

    __l_wait_end = __l_wait_start + 3000000.             "#EC NUMBER_OK

    WHILE sy-datum IS INITIAL.
      GET RUN TIME FIELD __l_wait_current.
      CHECK __l_wait_current >= __l_wait_end.
      GET TIME.
    ENDWHILE.
  ENDIF.
END-OF-DEFINITION.
