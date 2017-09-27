FUNCTION /gal/rfc_proxy_function.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FUNCTION_NAME) TYPE  STRING
*"     VALUE(PARAMETERS_IN) TYPE  STRING
*"     VALUE(INITIAL_CALLER_SYSYSID) TYPE  SYSYSID OPTIONAL
*"     VALUE(INITIAL_CALLER_SYMANDT) TYPE  SYMANDT OPTIONAL
*"     VALUE(INITIAL_CALLER_SYUNAME) TYPE  SYUNAME OPTIONAL
*"  EXPORTING
*"     VALUE(PARAMETERS_OUT) TYPE  STRING
*"     VALUE(EXCEPTION_INFO) TYPE  /GAL/EXCEPTION_INFO
*"  CHANGING
*"     VALUE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"  EXCEPTIONS
*"      NO_AUTHORIZATION
*"      AUTHORITY_INTEGRITY_FAILED
*"----------------------------------------------------------------------

  STATICS ls_config_flag         TYPE abap_bool VALUE abap_false.
  STATICS ls_trace_flag          TYPE abap_bool VALUE abap_false.

  DATA l_config_store            TYPE REF TO /gal/config_store_local.
  DATA l_config_folder           TYPE REF TO /gal/config_node.
  DATA l_config_node             TYPE REF TO /gal/config_node.

  DATA l_exception               TYPE REF TO cx_root.

  DATA l_interface_info          TYPE /gal/func_interface_info.

  DATA lt_param_bindings         TYPE abap_func_parmbind_tab.
  DATA l_param_binding           LIKE LINE OF lt_param_bindings.

  DATA lt_exception_bindings     TYPE abap_func_excpbind_tab.
  DATA l_exception_binding       TYPE abap_func_excpbind.

  DATA l_error_mess_char(200)    TYPE c.
  DATA l_error_mess_char_sh(100) TYPE c.
  DATA l_error_message           TYPE string.

  DATA l_next_step               TYPE i.

  DATA l_local_execution         TYPE abap_bool.

  DATA l_init_caller_sysysid     TYPE sysysid.
  DATA l_init_caller_symandt     TYPE symandt.
  DATA l_init_caller_syuname     TYPE syuname.
  DATA l_last_caller_sysysid     TYPE sysysid.
  DATA l_last_caller_symandt     TYPE symandt.
  DATA l_last_caller_syuname     TYPE syuname.
  DATA l_rfc_error(100)          TYPE c.
  DATA l_final_step              TYPE abap_bool.
  DATA l_first_step              TYPE abap_bool.
  DATA l_auth_exception          TYPE REF TO /gal/cx_cfw_auth_exception.
  DATA l_auth_exception_fw       TYPE REF TO /gal/cx_cfw_auth_excep_fw.
  DATA l_auth_exception_text     TYPE string.
  DATA l_auth_ex_text_c150(150)  TYPE c.
  DATA l_auth_active             TYPE abap_bool.
  DATA l_no_of_steps             TYPE i.
  DATA l_last_auth_active        TYPE abap_bool.
  DATA lt_back_callstack         TYPE abap_callstack.
  DATA l_read_callstack          TYPE abap_bool.
  DATA l_sysubrc                 TYPE sysubrc.
  DATA l_param_bindings_des      TYPE abap_bool.
  DATA l_context_data_set        TYPE abap_bool.
  DATA l_syscli_id(7)            TYPE c.

  FIELD-SYMBOLS <l_rfc_route_step_info> TYPE /gal/rfc_route_step_info.

  FIELD-SYMBOLS <l_param_info>          LIKE LINE OF l_interface_info-param_info.
  FIELD-SYMBOLS <l_param_binding>       LIKE LINE OF lt_param_bindings.

  FIELD-SYMBOLS <l_exception_binding>   LIKE LINE OF lt_exception_bindings.


* Enable support for background breakpoints
  cfw_break_point_support.


* Read configuration
  IF ls_config_flag = abap_false.
    TRY.
        CREATE OBJECT l_config_store.

        l_config_folder = l_config_store->get_node( path = `/Galileo Group AG/Open Source Components/Communication Framework` ). "#EC NOTEXT
        l_config_node = l_config_folder->get_child_node( `Detailed Tracing` ). "#EC NOTEXT
        l_config_node->get_value( EXPORTING default_value = abap_false
                                  IMPORTING value         = ls_trace_flag ).

      CATCH /gal/cx_config_exception.                   "#EC NO_HANDLER
        "Nothing needs to be done here. Default values are used.
    ENDTRY.

    ls_config_flag = abap_true.
  ENDIF.


* Follow RFC route
  TRY.

      TRY.

          l_auth_active = /gal/cfw_auth_config=>is_active( ).

* Collect information regarding current step
          READ TABLE rfc_route_info-step_infos INDEX rfc_route_info-current_step
               ASSIGNING <l_rfc_route_step_info>.
          IF sy-subrc = 0.
            /gal/cfw_helper=>update_rfc_route_step_info( CHANGING rfc_route_step_info = <l_rfc_route_step_info> ).
          ENDIF.

* Perform authorization check for RFC (no check in source system)
          IF rfc_route_info-current_step > 1.
            /gal/cfw_helper=>check_rfc_authorization( function_name = function_name ).

*        IF l_is_critical = abap_true.
            IF l_auth_active = abap_true.

              DESCRIBE TABLE rfc_route_info-step_infos LINES l_no_of_steps.
              IF l_no_of_steps < rfc_route_info-current_step.
                MESSAGE e003 RAISING authority_integrity_failed.
              ENDIF.
              IF l_no_of_steps = rfc_route_info-current_step.
                l_final_step = abap_true.
              ELSE.
                l_final_step = abap_false.
              ENDIF.

              IF rfc_route_info-current_step = 2. "First hop -> get initial caller data

                IF NOT initial_caller_sysysid IS INITIAL OR
                   NOT initial_caller_symandt IS INITIAL OR
                   NOT initial_caller_syuname IS INITIAL.
                  MESSAGE e004 RAISING authority_integrity_failed.
                ENDIF.

                CALL FUNCTION '/GAL/RFC_GET_SY_DATA'
                  DESTINATION 'BACK'
                  EXPORTING
                    get_auth_active_state = abap_true
                    read_callstack        = abap_true
                  IMPORTING
                    sysysid               = l_init_caller_sysysid
                    symandt               = l_init_caller_symandt
                    syuname               = l_init_caller_syuname
                    auth_active           = l_last_auth_active
                    callstack             = lt_back_callstack
                  EXCEPTIONS
                    system_failure        = 1 MESSAGE l_rfc_error
                    communication_failure = 2 MESSAGE l_rfc_error
                    framework_exception   = 3.
                IF sy-subrc <> 0.
                  IF sy-subrc = 3.
                    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                          INTO l_error_message.
                    l_error_mess_char_sh = l_error_message.
                  ELSE.
                    l_error_mess_char_sh = l_rfc_error.
                  ENDIF.
                  MESSAGE e002 WITH l_error_mess_char_sh(50) l_error_mess_char_sh+50(50) RAISING authority_integrity_failed.
                ENDIF.
                IF l_last_auth_active = abap_false.
                  MESSAGE e014 WITH l_init_caller_sysysid RAISING authority_integrity_failed.
                ENDIF.

                /gal/cfw_auth=>check_rfc_context_external(
                  function_name           = function_name
                  back_callstack_external = lt_back_callstack
                  rfc_route_info_external = rfc_route_info
                ).
                TRY.
                    /gal/cfw_auth=>raise_auth_exception_if_exists( ).
                  CATCH /gal/cx_cfw_auth_exception INTO l_auth_exception.
                    l_auth_exception_text = l_auth_exception->get_text( ).
                    CONCATENATE sy-sysid sy-mandt INTO l_syscli_id SEPARATED BY '.'.
                    l_auth_ex_text_c150 = l_auth_exception_text.
                    MESSAGE e016 WITH l_syscli_id l_auth_ex_text_c150(50) l_auth_ex_text_c150+50(50) l_auth_ex_text_c150+100(50) RAISING no_authorization.
                ENDTRY.

                IF l_init_caller_sysysid IS INITIAL OR
                   l_init_caller_symandt IS INITIAL OR
                   l_init_caller_syuname IS INITIAL.
                  MESSAGE e015 RAISING authority_integrity_failed.
                ENDIF.

                IF l_final_step = abap_true.
                  l_last_caller_sysysid = l_init_caller_sysysid.
                  l_last_caller_symandt = l_init_caller_symandt.
                  l_last_caller_syuname = l_init_caller_syuname.
                ENDIF.

              ELSE. " current_step > 2

                IF initial_caller_sysysid IS INITIAL OR
                   initial_caller_symandt IS INITIAL OR
                   initial_caller_syuname IS INITIAL.
                  MESSAGE e005 RAISING authority_integrity_failed.
                ENDIF.

                l_init_caller_sysysid = initial_caller_sysysid.
                l_init_caller_symandt = initial_caller_symandt.
                l_init_caller_syuname = initial_caller_syuname.

                IF l_final_step = abap_false.
                  l_read_callstack = abap_true.
                ENDIF.
                CALL FUNCTION '/GAL/RFC_GET_SY_DATA'
                  DESTINATION 'BACK'
                  EXPORTING
                    get_auth_active_state = abap_true
                    read_callstack        = l_read_callstack
                  IMPORTING
                    sysysid               = l_last_caller_sysysid
                    symandt               = l_last_caller_symandt
                    syuname               = l_last_caller_syuname
                    auth_active           = l_last_auth_active
                    callstack             = lt_back_callstack
                  EXCEPTIONS
                    system_failure        = 1 MESSAGE l_rfc_error
                    communication_failure = 2 MESSAGE l_rfc_error
                    framework_exception   = 3.
                IF sy-subrc <> 0.
                  IF sy-subrc = 3.
                    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                          INTO l_error_message.
                    l_error_mess_char_sh = l_error_message.
                  ELSE.
                    l_error_mess_char_sh = l_rfc_error.
                  ENDIF.
                  MESSAGE e009 WITH l_error_mess_char_sh(50) l_error_mess_char_sh+50(50) RAISING authority_integrity_failed.
                ENDIF.
                IF l_last_auth_active = abap_false.
                  MESSAGE e014 WITH l_last_caller_sysysid RAISING authority_integrity_failed.
                ENDIF.
                IF l_final_step = abap_false.
                  CLEAR l_last_caller_sysysid.
                  CLEAR l_last_caller_symandt.
                  CLEAR l_last_caller_syuname.
                ENDIF.

              ENDIF.

            ENDIF.

          ELSEIF rfc_route_info-current_step = 1.

            DESCRIBE TABLE rfc_route_info-step_infos LINES sy-tfill.
            IF sy-tfill = 1. "Local execution (no further RFC destinations
              l_local_execution = abap_true.
              l_final_step      = abap_true.
              l_init_caller_sysysid  = sy-sysid.
              l_init_caller_symandt  = sy-mandt.
              l_init_caller_syuname  = sy-uname.
            ENDIF.

            l_first_step = abap_true.

          ELSE.

            IF NOT rfc_route_info-step_infos IS INITIAL.
              READ TABLE rfc_route_info-step_infos INDEX 1 ASSIGNING <l_rfc_route_step_info>.
              IF NOT <l_rfc_route_step_info>-rfc_destination IS INITIAL.
                UNASSIGN <l_rfc_route_step_info>.
                /gal/cfw_helper=>initialize_rfc_route_info( CHANGING rfc_route_info = rfc_route_info ).
                INSERT INITIAL LINE INTO rfc_route_info-step_infos INDEX 1.
                rfc_route_info-current_step = 1.
              ELSE.
                MESSAGE e006 RAISING authority_integrity_failed.
              ENDIF.
            ENDIF.

            l_local_execution = abap_true.
            l_final_step      = abap_true.
            l_first_step      = abap_true.

            IF l_auth_active = abap_true.
              CALL FUNCTION 'RFC_PING'
                DESTINATION 'BACK'
                EXCEPTIONS
                  system_failure        = 1 MESSAGE l_rfc_error
                  communication_failure = 2 MESSAGE l_rfc_error.
              IF sy-subrc = 0.
                MESSAGE e008 RAISING authority_integrity_failed.
              ENDIF.
            ENDIF.

          ENDIF.

          IF l_auth_active = abap_true AND rfc_route_info-current_step > 1.
            /gal/cfw_helper=>deserialize_function_params( EXPORTING xml                = parameters_in
                                                          IMPORTING parameter_bindings = lt_param_bindings ).
            l_param_bindings_des = abap_true.
            /gal/cfw_auth=>init( ).
            /gal/cfw_auth=>set_context_data(
              EXPORTING
                function_name                  = function_name
                param_bindings                 = lt_param_bindings
                initial_caller_sysysid         = l_init_caller_sysysid
                initial_caller_symandt         = l_init_caller_symandt
                initial_caller_syuname         = l_init_caller_syuname
                last_caller_sysysid            = l_last_caller_sysysid
                last_caller_symandt            = l_last_caller_symandt
                last_caller_syuname            = l_last_caller_syuname
                local_execution                = l_local_execution
                first_step                     = l_first_step
                final_step                     = l_final_step
                rfc_route_info                 = rfc_route_info
                direct_call                    = abap_false
            ).
            l_context_data_set = abap_true.

            IF l_final_step = abap_true.
              /gal/cfw_auth=>check_local_func_limits( ).
              /gal/cfw_auth=>raise_auth_exception_if_exists( ).
            ENDIF.
            IF l_final_step = abap_false AND rfc_route_info-current_step > 2.
              /gal/cfw_auth=>check_rfc_context_external(
                function_name           = function_name
                back_callstack_external = lt_back_callstack
                rfc_route_info_external = rfc_route_info
              ).
              TRY.
                  /gal/cfw_auth=>raise_auth_exception_if_exists( ).
                CATCH /gal/cx_cfw_auth_exception INTO l_auth_exception.
                  l_auth_exception_text = l_auth_exception->get_text( ).
                  CONCATENATE sy-sysid sy-mandt INTO l_syscli_id SEPARATED BY '.'.
                  l_auth_ex_text_c150 = l_auth_exception_text.
                  MESSAGE e017 WITH l_syscli_id l_auth_ex_text_c150(50) l_auth_ex_text_c150+50(50) l_auth_ex_text_c150+100(50) RAISING no_authorization.
              ENDTRY.
            ENDIF.

          ENDIF.
        CATCH /gal/cx_cfw_auth_excep_fw INTO l_auth_exception_fw.
          IF l_context_data_set = abap_true.
            /gal/cfw_auth=>reset_context_data( ).
          ENDIF.
          l_auth_exception_text = l_auth_exception_fw->get_text( ).
          CONCATENATE sy-sysid sy-mandt INTO l_syscli_id SEPARATED BY '.'.
          l_auth_ex_text_c150 = l_auth_exception_text.
          MESSAGE e018 WITH l_syscli_id l_auth_ex_text_c150(50) l_auth_ex_text_c150+50(50) l_auth_ex_text_c150+100(50) RAISING authority_integrity_failed.
      ENDTRY.

* Prepare RFC route info for execution of next step
      l_next_step = rfc_route_info-current_step + 1.

* Check if there is another step to follow
      READ TABLE rfc_route_info-step_infos INDEX l_next_step
           ASSIGNING <l_rfc_route_step_info>.
      IF sy-subrc = 0.
        IF l_context_data_set = abap_true.
          /gal/cfw_auth=>reset_context_data( ).
        ENDIF.

* Trace: Follow RFC Route
        IF ls_trace_flag = abap_true.
          IF rfc_route_info-current_step > 1.
            /gal/cfw_helper=>write_to_trace( context        = `RFC: Follow RFC Route` "#EC NOTEXT
                                             function_name  = function_name
                                             rfc_route_info = rfc_route_info ).
          ELSE.
            /gal/cfw_helper=>write_to_trace( context            = `RFC: Invoke` "#EC NOTEXT
                                             function_name      = function_name
                                             rfc_route_info     = rfc_route_info
                                             include_call_stack = abap_true ).
          ENDIF.
        ENDIF.

* Prepare RFC route info for execution of next step
        rfc_route_info-current_step = l_next_step.

* Follow the RFC Route until reaching the target system
        CALL FUNCTION '/GAL/RFC_PROXY_FUNCTION'
          DESTINATION <l_rfc_route_step_info>-rfc_destination
          EXPORTING
            function_name              = function_name
            parameters_in              = parameters_in
            initial_caller_sysysid     = l_init_caller_sysysid
            initial_caller_symandt     = l_init_caller_symandt
            initial_caller_syuname     = l_init_caller_syuname
          IMPORTING
            parameters_out             = parameters_out
            exception_info             = exception_info
          CHANGING
            rfc_route_info             = rfc_route_info
          EXCEPTIONS
            system_failure             = 1 MESSAGE l_error_mess_char
            communication_failure      = 2 MESSAGE l_error_mess_char
            authority_integrity_failed = 3
            no_authorization           = 4.

* Handle RFC Exceptions
        IF sy-subrc <> 0.
          CLEAR exception_info.

          IF sy-subrc = 1 OR sy-subrc = 2.
            exception_info-message_text = l_error_mess_char.
          ELSE.
            exception_info-message_id      = sy-msgid.
            exception_info-message_type    = sy-msgty.
            exception_info-message_number  = sy-msgno.
            exception_info-message_var1    = sy-msgv1.
            exception_info-message_var2    = sy-msgv2.
            exception_info-message_var3    = sy-msgv3.
            exception_info-message_var4    = sy-msgv4.
          ENDIF.
          IF sy-subrc = 1.
            exception_info-exception_name  = 'SYSTEM_FAILURE'.
          ELSEIF sy-subrc = 2.
            exception_info-exception_name  = 'COMMUNICATION_FAILURE'.
          ELSEIF sy-subrc = 3.
            exception_info-exception_name  = 'AUTHORITY_INTEGRITY_FAILED'.
          ELSE.
            exception_info-exception_name  = 'NO_AUTHORIZATION'.
          ENDIF.

          exception_info-system_id       = sy-sysid.
          exception_info-client_id       = sy-mandt.
          exception_info-rfc_destination = <l_rfc_route_step_info>-rfc_destination.
          exception_info-exception_type  = 'E'.

        ENDIF.

        rfc_route_info-current_step = rfc_route_info-current_step - 1.

* Trace: RFC completed
        IF ls_trace_flag = abap_true.
          IF rfc_route_info-current_step > 1.
            /gal/cfw_helper=>write_to_trace( context        = `RFC: Follow RFC route (returning to caller)` "#EC NOTEXT
                                             function_name  = function_name
                                             rfc_route_info = rfc_route_info ).
          ELSE.
            /gal/cfw_helper=>write_to_trace( context        = `RFC: Completed` "#EC NOTEXT
                                             function_name  = function_name
                                             rfc_route_info = rfc_route_info ).
          ENDIF.
        ENDIF.

        RETURN.
      ENDIF.


*==========================================================
* The following code is executed on the target system only
*==========================================================

* Trace: Executing function
      IF ls_trace_flag = abap_true.
        /gal/cfw_helper=>write_to_trace( context        = `RFC: Reached destination system (executing function)` "#EC NOTEXT
                                         function_name  = function_name
                                         rfc_route_info = rfc_route_info ).
      ENDIF.

* Allow low level debugging of the proxy function
      cfw_rfc_break_point rfc_route_info `/GAL/RFC_PROXY_FUNCTION`. "#EC BOOL_OK

* Try to restore caller's language settings (in case of remote execution)
      IF rfc_route_info-current_step > 1.
        READ TABLE rfc_route_info-step_infos INDEX 1
             ASSIGNING <l_rfc_route_step_info>.

        /gal/cfw_helper=>restore_user_environment( language        = <l_rfc_route_step_info>-user_language
                                                   country         = <l_rfc_route_step_info>-user_country
                                                   locale_modifier = <l_rfc_route_step_info>-user_locale_modifier ).
      ELSE.
        rfc_route_info-current_step = rfc_route_info-current_step + 1.
      ENDIF.

* Determine function interface
      l_interface_info = /gal/cfw_helper=>get_function_interface( function_name ).

* Deserialize parameter binding table
      IF l_param_bindings_des = abap_false.
        /gal/cfw_helper=>deserialize_function_params( EXPORTING xml                = parameters_in
                                                      IMPORTING parameter_bindings = lt_param_bindings ).
      ENDIF.
* Adjust parameter binding table using local interface
      LOOP AT lt_param_bindings ASSIGNING <l_param_binding>.
        READ TABLE l_interface_info-param_info
              WITH KEY parameter_name = <l_param_binding>-name
                   ASSIGNING <l_param_info>.             "#EC CI_STDSEQ
        IF sy-subrc <> 0.
          DELETE lt_param_bindings.
        ELSEIF <l_param_binding>-kind = 0.
          l_param_binding      = <l_param_binding>.
          l_param_binding-kind = <l_param_info>-parameter_kind_int.
          INSERT l_param_binding INTO TABLE lt_param_bindings.

          DELETE lt_param_bindings.
        ENDIF.
      ENDLOOP.

      LOOP AT l_interface_info-param_info ASSIGNING <l_param_info>.
        IF <l_param_info>-parameter_kind CA 'ICT'.
          IF <l_param_info>-parameter_name = `RFC_ROUTE_INFO` AND
             <l_param_info>-parameter_type = `/GAL/RFC_ROUTE_INFO`.

            READ TABLE lt_param_bindings
                  WITH KEY name = <l_param_info>-parameter_name "#EC WARNOK
                       TRANSPORTING NO FIELDS.          "#EC CI_SORTSEQ
            IF sy-subrc <> 0.
              CLEAR l_param_binding.
              l_param_binding-name = <l_param_info>-parameter_name.
              l_param_binding-kind = <l_param_info>-parameter_kind_int.
              GET REFERENCE OF rfc_route_info INTO l_param_binding-value.

              INSERT l_param_binding INTO TABLE lt_param_bindings.
            ENDIF.
          ENDIF.
        ELSEIF <l_param_info>-parameter_kind = 'E'.
          READ TABLE lt_param_bindings
                WITH KEY name = <l_param_info>-parameter_name "#EC WARNOK
                     TRANSPORTING NO FIELDS.            "#EC CI_SORTSEQ
          IF sy-subrc <> 0.
            CLEAR l_param_binding.
            l_param_binding-name  = <l_param_info>-parameter_name.
            l_param_binding-kind  = <l_param_info>-parameter_kind_int.
            l_param_binding-value = /gal/cfw_helper=>create_data( <l_param_info>-parameter_type ).
            INSERT l_param_binding INTO TABLE lt_param_bindings.
          ENDIF.
        ENDIF.
      ENDLOOP.

* Build exception binding table
      LOOP AT l_interface_info-param_info ASSIGNING <l_param_info>
           WHERE parameter_kind = 'X'.                   "#EC CI_STDSEQ

        l_exception_binding-name  = <l_param_info>-parameter_name.
        l_exception_binding-value = sy-tabix.

        INSERT l_exception_binding INTO TABLE lt_exception_bindings.
      ENDLOOP.

* Attach session to break point server (if requested)
      cfw_rfc_break_point rfc_route_info function_name.

* Call function module
      CALL FUNCTION function_name
        PARAMETER-TABLE
        lt_param_bindings
        EXCEPTION-TABLE
        lt_exception_bindings.
      l_sysubrc = sy-subrc.
      IF l_sysubrc = 0.
        TRY.
            /gal/cfw_auth=>raise_auth_exception_if_exists( ).
          CATCH /gal/cx_cfw_auth_exception INTO l_auth_exception.
            IF l_context_data_set = abap_true.
              /gal/cfw_auth=>reset_context_data( ).
            ENDIF.
            l_auth_exception_text = l_auth_exception->get_text( ).
            CONCATENATE sy-sysid sy-mandt INTO l_syscli_id SEPARATED BY '.'.
            l_auth_ex_text_c150 = l_auth_exception_text.
            MESSAGE e007 WITH l_syscli_id l_auth_ex_text_c150(50) l_auth_ex_text_c150+50(50) l_auth_ex_text_c150+100(50) RAISING no_authorization.
        ENDTRY.
        IF l_context_data_set = abap_true.
          /gal/cfw_auth=>reset_context_data( ).
        ENDIF.
      ELSE.
* Process exception (if any)
        IF l_context_data_set = abap_true.
          /gal/cfw_auth=>reset_context_data( ).
        ENDIF.

        CLEAR exception_info.

        IF sy-msgid = '/GAL/CFW' AND sy-msgno = '019'.
          DO 0 TIMES. MESSAGE e019 WITH '' '' '' ''. ENDDO.
          MESSAGE e020 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_auth_exception_text.
          CONCATENATE sy-sysid sy-mandt INTO l_syscli_id SEPARATED BY '.'.
          l_auth_ex_text_c150 = l_auth_exception_text.
          MESSAGE e018 WITH l_syscli_id l_auth_ex_text_c150(50) l_auth_ex_text_c150+50(50) l_auth_ex_text_c150+100(50) RAISING authority_integrity_failed.
        ENDIF.
        READ TABLE lt_exception_bindings
              WITH KEY value = l_sysubrc
                   ASSIGNING <l_exception_binding>.     "#EC CI_HASHSEQ

        exception_info-system_id      = sy-sysid.
        exception_info-client_id      = sy-mandt.
        exception_info-exception_type = 'E'.
        exception_info-exception_name = <l_exception_binding>-name.
        exception_info-message_id     = sy-msgid.
        exception_info-message_type   = sy-msgty.
        exception_info-message_number = sy-msgno.
        exception_info-message_var1   = sy-msgv1.
        exception_info-message_var2   = sy-msgv2.
        exception_info-message_var3   = sy-msgv3.
        exception_info-message_var4   = sy-msgv4.

        IF sy-msgid IS NOT INITIAL AND
           sy-msgty IS NOT INITIAL AND
           sy-msgno IS NOT INITIAL.

          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO exception_info-message_text.
        ENDIF.

        RETURN. " No values should be returned in case of an exception!
      ENDIF.

* Remove IMPORTING parameters from parameter binding table
      DELETE lt_param_bindings WHERE kind = abap_func_exporting.

* Serialize remaining part of parameter binding table
      /gal/cfw_helper=>serialize_function_params( EXPORTING parameter_bindings = lt_param_bindings
                                                  IMPORTING xml                = parameters_out ).

    CATCH cx_static_check INTO l_exception.
      exception_info = /gal/cfw_helper=>get_exception_info( l_exception ).

  ENDTRY.

* Adjust current step in case of local execution
  IF l_local_execution = abap_true.
    rfc_route_info-current_step = rfc_route_info-current_step - 1.
  ENDIF.

* Trace: Function execution completed
  IF ls_trace_flag = abap_true.
    /gal/cfw_helper=>write_to_trace( context        = `RFC: Function execution completed` "#EC NOTEXT
                                     function_name  = function_name
                                     rfc_route_info = rfc_route_info ).
  ENDIF.
ENDFUNCTION.
