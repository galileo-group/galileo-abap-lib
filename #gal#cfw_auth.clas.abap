class /GAL/CFW_AUTH definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  constants CONST_CAB_FINAL_RFC_STEP type /GAL/CFW_CUSTOM_AUTH_BEHAVE value 'R'. "#EC NOTEXT
  constants CONST_CAB_FINAL_STEP type /GAL/CFW_CUSTOM_AUTH_BEHAVE value 'D'. "#EC NOTEXT
  constants CONST_CAB_NO_CHECK type /GAL/CFW_CUSTOM_AUTH_BEHAVE value 'N'. "#EC NOTEXT

  class-methods CHECK_AUTH
    importing
      !CUSTOM_AUTH_BEHAVE type /GAL/CFW_CUSTOM_AUTH_BEHAVE
    raising
      /GAL/CX_CFW_AUTH_EXCEP_FW .
  class-methods CHECK_LOCAL_FUNC_LIMITS
    importing
      !FUNCTION_NAME_EXTERNAL type STRING optional
    raising
      /GAL/CX_CFW_AUTH_EXCEP_FW .
  class-methods CHECK_RFC_CONTEXT_EXTERNAL
    importing
      !FUNCTION_NAME type STRING
      !BACK_CALLSTACK_EXTERNAL type ABAP_CALLSTACK
      !RFC_ROUTE_INFO_EXTERNAL type /GAL/RFC_ROUTE_INFO
    raising
      /GAL/CX_CFW_AUTH_EXCEP_FW .
  class-methods FILL_CFW_AUTH_ERR_EX_INFO
    importing
      !CFW_AUTH_EXCEPTION type ref to /GAL/CX_CFW_AUTH_EXCEPTION
    returning
      value(EXCEPTION_INFO) type /GAL/EXCEPTION_INFO .
  class-methods INIT
    raising
      /GAL/CX_CFW_AUTH_EXCEP_FW .
  class-methods RAISE_AUTH_EXCEPTION_IF_EXISTS
    raising
      /GAL/CX_CFW_AUTH_EXCEPTION .
  class-methods RESET_CONTEXT_DATA
    importing
      !FORCE_AUTH_EXCEPTION_DISCARD type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CFW_AUTH_EXCEP_FW .
  class-methods SET_CONTEXT_DATA
    importing
      !FUNCTION_NAME type STRING
      !PARAM_BINDINGS type ABAP_FUNC_PARMBIND_TAB
      !INITIAL_CALLER_SYSYSID type SYSYSID
      !INITIAL_CALLER_SYMANDT type SYMANDT
      !INITIAL_CALLER_SYUNAME type SYUNAME
      !LAST_CALLER_SYSYSID type SYSYSID
      !LAST_CALLER_SYMANDT type SYMANDT
      !LAST_CALLER_SYUNAME type SYUNAME
      !LOCAL_EXECUTION type ABAP_BOOL
      !FIRST_STEP type ABAP_BOOL
      !FINAL_STEP type ABAP_BOOL
      !DIRECT_CALL type ABAP_BOOL
      !RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO
    raising
      /GAL/CX_CFW_AUTH_EXCEP_FW .
protected section.
private section.

  class-data AUTH_EXCEPTION type ref to /GAL/CX_CFW_AUTH_EXCEPTION .
  class-data CONFIG type ref to /GAL/CFW_AUTH_CONFIG .
  class-data CTX_DIRECT_CALL type ABAP_BOOL .
  class-data CTX_FINAL_STEP type ABAP_BOOL .
  class-data CTX_FIRST_STEP type ABAP_BOOL .
  class-data CTX_FUNCTION_NAME type STRING .
  class-data CTX_INITIAL_CALLER_SYMANDT type SYMANDT .
  class-data CTX_INITIAL_CALLER_SYSYSID type SYSYSID .
  class-data CTX_INITIAL_CALLER_SYUNAME type SYUNAME .
  class-data CTX_LAST_CALLER_SYMANDT type SYMANDT .
  class-data CTX_LAST_CALLER_SYSYSID type SYSYSID .
  class-data CTX_LAST_CALLER_SYUNAME type SYUNAME .
  class-data CTX_LOCAL_EXECUTION type ABAP_BOOL .
  class-data CTX_PARAM_BINDINGS type ABAP_FUNC_PARMBIND_TAB .
  class-data CTX_RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO .
  class-data INT_RFC_CONTEXT_CHECKED type ABAP_BOOL .

  class-methods CHECK_RFC_CONTEXT
    importing
      !FUNCTION_NAME_EXTERNAL type STRING optional
      !BACK_CALLSTACK_EXTERNAL type ABAP_CALLSTACK optional
      !RFC_ROUTE_INFO_EXTERNAL type /GAL/RFC_ROUTE_INFO optional
    raising
      /GAL/CX_CFW_AUTH_EXCEP_FW .
  class-methods REGULAR_AUTH_EXCEPTION_FOUND
    importing
      !TEXTID type SOTR_CONC optional
      !VAR1 type ANY optional
      !VAR2 type ANY optional
      !VAR3 type ANY optional
      !AUTH_EXCEPTION_OBJECT type ref to /GAL/CX_CFW_AUTH_EXCEPTION optional .
ENDCLASS.



CLASS /GAL/CFW_AUTH IMPLEMENTATION.


  METHOD check_auth.
    DATA l_function_name           TYPE char30.
    DATA l_is_active               TYPE abap_bool.
    DATA l_badi_handle             TYPE REF TO /gal/cfw_auth_checker.
    DATA l_badi_filter_func        TYPE string.


    CLEAR auth_exception.

    "Check if according to hardcoded function check behavior, a check is required
    IF   custom_auth_behave = /gal/cfw_auth=>const_cab_no_check
       OR
         custom_auth_behave = /gal/cfw_auth=>const_cab_final_step AND ctx_final_step = abap_false
      OR
         custom_auth_behave = /gal/cfw_auth=>const_cab_final_rfc_step AND
         ( ctx_final_step = abap_false OR ctx_direct_call = abap_true OR ctx_local_execution = abap_true ).
      RETURN.
    ENDIF.

    "If a check is required according to coding, it also is checked if the auth framework is activated in config
    l_is_active = /gal/cfw_auth_config=>is_active( ).
    IF l_is_active = abap_false.
      RETURN.
    ENDIF.

    "when this method is called and checks are active and required, the config has to be initialized.
    "if this is not the case, an internal software error exists
    IF config IS INITIAL.
      RAISE EXCEPTION TYPE /gal/cx_cfw_auth_excep_fw
        EXPORTING
          textid = /gal/cx_cfw_auth_excep_fw=>check_without_init.
    ENDIF.

    "functions can be set to be ignored by customizing them in config store
    l_function_name = ctx_function_name.
    READ TABLE /gal/cfw_auth_config=>ignored_functions WITH KEY table_line = l_function_name TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    IF ctx_direct_call = abap_false AND int_rfc_context_checked = abap_false.
      " if we are in RFC context and this context has not been checked yet, the rfc context callback abap stack is checked
      check_rfc_context( ).
      IF NOT auth_exception IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    IF ctx_local_execution = abap_false AND ctx_initial_caller_sysysid NE sy-sysid OR ctx_initial_caller_symandt NE sy-mandt.
      check_local_func_limits( ).
      IF NOT auth_exception IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    IF ctx_function_name CP '/GAL/*'.
      "use all check BAdIs, also the ones that are used only for Galileo funtions
      l_badi_filter_func = '/GAL/*'.
    ELSE.
      l_badi_filter_func = '*'.
      "use only the BAdIs declared for common checks
    ENDIF.

    TRY.
        "now call all activated auth check BAdIs
        GET BADI l_badi_handle FILTERS /gal/cfw_ac_function = l_badi_filter_func.
        CALL BADI l_badi_handle->check_auth
          EXPORTING
            direct_call            = ctx_direct_call
            final_step             = ctx_final_step
            first_step             = ctx_first_step
            function_name          = ctx_function_name
            initial_caller_symandt = ctx_initial_caller_symandt
            initial_caller_sysysid = ctx_initial_caller_sysysid
            initial_caller_syuname = ctx_initial_caller_syuname
            last_caller_symandt    = ctx_last_caller_symandt
            last_caller_sysysid    = ctx_last_caller_sysysid
            last_caller_syuname    = ctx_last_caller_syuname
            local_execution        = ctx_local_execution
            param_bindings         = ctx_param_bindings
            rfc_route_info         = ctx_rfc_route_info.
      CATCH /gal/cx_cfw_auth_exception INTO auth_exception.
        regular_auth_exception_found(
          auth_exception_object = auth_exception
        ).
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD check_local_func_limits.
* Check if the current function is forbidden for remote execution and raise an exception if this is the case.

    DATA l_function_name           TYPE char30.
    DATA l_conf_ex                 TYPE REF TO /gal/cx_config_exception.
    DATA l_error_message           TYPE string.


    IF function_name_external IS SUPPLIED.
      l_function_name = function_name_external.
    ELSE.
      l_function_name = ctx_function_name.
    ENDIF.

    IF l_function_name IS INITIAL.
      RAISE EXCEPTION TYPE /gal/cx_cfw_auth_excep_fw
        EXPORTING
          textid = /gal/cx_cfw_auth_excep_fw=>check_localfunc_without_funcn.
    ENDIF.

    TRY.
        /gal/cfw_auth_config=>init_config_params( ).
      CATCH /gal/cx_config_exception INTO l_conf_ex.
        l_error_message = l_conf_ex->get_text( ).
        RAISE EXCEPTION TYPE /gal/cx_cfw_auth_excep_fw
          EXPORTING
            textid = /gal/cx_cfw_auth_excep_fw=>missing_config_locfunc
            var1   = l_error_message.
    ENDTRY.


    READ TABLE /gal/cfw_auth_config=>local_only_functions WITH KEY table_line = l_function_name TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      regular_auth_exception_found(
        EXPORTING
          textid = /gal/cx_cfw_auth_exception=>forbidden_rfc_call_of_loc_func
          var1   = ctx_function_name
      ).
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD check_rfc_context.

    " This method checks if the RFC context is legal.
    " NOTE: If this implementation throws an exception although a valid use case is given,
    "       the caller can be permitted using config parameter
    "       '/Galileo Group AG/Open Source Components/Communication Framework/Security/Custom Allowed Callers' or
    "       '/Galileo Group AG/Open Source Components/Communication Framework/Security/Ignored Functions'
    "       until the coding is fixed in order to reflect the missing case.

    DATA l_rfc_error(100)             TYPE c.
    DATA l_callstack_index            TYPE i.
    DATA l_callstack_lines            TYPE i.
    DATA l_index                      TYPE i.
    DATA lt_back_callstack            TYPE abap_callstack.
    DATA l_galileo_context            TYPE abap_bool.
    DATA l_custom_allow               TYPE abap_bool.
    DATA l_rfc_scope_extended_context TYPE abap_bool.
    DATA l_trigger_function           TYPE char30.
    DATA l_direct_call                TYPE abap_bool.
    DATA lt_back_callstack_copy       TYPE abap_callstack.
    DATA l_non_gal_prog_found         TYPE abap_bool.
    DATA l_non_gal_prog               TYPE syrepid.
    DATA l_gal_ctx_left               TYPE abap_bool.
    DATA l_idx_rev                    TYPE i.
    DATA l_idx_do                     TYPE i.
    DATA l_callstack_offset           TYPE i.
    DATA l_error_message              TYPE string.

    FIELD-SYMBOLS <l_callstack_line>        TYPE abap_callstack_line.
    FIELD-SYMBOLS <l_callstack_copy_line>   TYPE abap_callstack_line.
    FIELD-SYMBOLS <l_custom_allowed_caller> TYPE /gal/cfw_custom_allow_caller.
    FIELD-SYMBOLS <lt_back_callstack>       TYPE abap_callstack.
    FIELD-SYMBOLS <l_first_step_info>       TYPE /gal/rfc_route_step_info.
    FIELD-SYMBOLS <l_rfc_route_info>        TYPE /gal/rfc_route_info.


    IF ctx_local_execution = abap_true AND function_name_external IS INITIAL.
      RETURN.
    ENDIF.


    IF function_name_external IS INITIAL.
      CALL FUNCTION '/GAL/RFC_GET_SY_DATA'
        DESTINATION 'BACK'
        EXPORTING
          read_callstack        = abap_true
        IMPORTING
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
          RAISE EXCEPTION TYPE /gal/cx_cfw_auth_excep_fw
            EXPORTING
              textid = /gal/cx_cfw_auth_excep_fw=>fw_exception_remote
              var1   = l_error_message.
        ELSE.
          regular_auth_exception_found(
            EXPORTING
              textid = /gal/cx_cfw_auth_exception=>rfc_back_call_error1
              var1   = l_rfc_error
          ).
          RETURN.
        ENDIF.
      ENDIF.

      l_trigger_function = ctx_function_name.
      ASSIGN lt_back_callstack  TO <lt_back_callstack>.
      ASSIGN ctx_rfc_route_info TO <l_rfc_route_info>.
    ELSE.
      /gal/cfw_auth_config=>init_config_params( ).
      l_trigger_function = function_name_external.
      READ TABLE /gal/cfw_auth_config=>ignored_functions WITH KEY table_line = l_trigger_function TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        RETURN.
      ENDIF.
      ASSIGN back_callstack_external TO <lt_back_callstack>.
      ASSIGN rfc_route_info_external TO <l_rfc_route_info>.
    ENDIF.

    DESCRIBE TABLE <lt_back_callstack> LINES l_callstack_lines.
    IF l_callstack_lines < 7.
      IF l_callstack_lines = 6.
        l_direct_call = abap_true.
      ELSE.
        regular_auth_exception_found(
          EXPORTING
            textid = /gal/cx_cfw_auth_exception=>rfc_back_call_stack_incons1
        ).
        IF /gal/cfw_auth_config=>trace_violation_details = abap_true.
          /gal/trace=>write_callstack( custom_callstack = <lt_back_callstack> ).
        ENDIF.
        RETURN.
      ENDIF.
    ENDIF.
    l_callstack_index = l_callstack_lines.

    l_rfc_scope_extended_context = abap_true.
    DO 2 TIMES.
      READ TABLE <lt_back_callstack> INDEX l_callstack_index ASSIGNING <l_callstack_line>.
      l_index = sy-index.
      CASE l_index.
        WHEN 1.
          IF NOT ( <l_callstack_line>-mainprogram = 'SAPMSSY1' AND <l_callstack_line>-blockname = '%_RFC_START' ).
            l_rfc_scope_extended_context = abap_false.
            EXIT.
          ENDIF.
        WHEN 2.
          IF NOT ( <l_callstack_line>-mainprogram = 'SAPMSSY1' AND <l_callstack_line>-blocktype = 'FORM' AND <l_callstack_line>-blockname = 'REMOTE_FUNCTION_CALL' ).
            l_rfc_scope_extended_context = abap_false.
            EXIT.
          ENDIF.
      ENDCASE.
      l_callstack_index = l_callstack_index - 1.
    ENDDO.

    DO 8 TIMES.
      l_index = sy-index.
      READ TABLE <lt_back_callstack> INDEX l_index ASSIGNING <l_callstack_line>.
      IF sy-subrc <> 0.
        /gal/trace=>write_text(
          EXPORTING
            text = 'Internal CFW auth RFC check error. Assigning non-existing callstack line {1}'
            var01 = l_index
        ).                                                  "#EC NOTEXT
        IF /gal/cfw_auth_config=>trace_violation_details = abap_true.
          /gal/trace=>write_callstack( custom_callstack = <lt_back_callstack> ).
        ENDIF.
      ELSE.
        CASE l_index.

          WHEN 1.
            CHECK <l_callstack_line>-blockname NE '/GAL/RFC_GET_SY_DATA' OR <l_callstack_line>-blocktype NE 'FUNCTION'.

          WHEN 2.
            CHECK <l_callstack_line>-blockname NE '/GAL/RFC_GET_SY_DATA' OR <l_callstack_line>-blocktype NE 'FORM'.

          WHEN 3.
            IF <l_callstack_line>-blockname EQ '/GAL/RFC_PROXY_FUNCTION' AND <l_callstack_line>-blocktype EQ 'RFC'.
              CONTINUE.
            ELSEIF <l_rfc_route_info>-current_step = 2.
              lt_back_callstack_copy = <lt_back_callstack>.
              l_non_gal_prog_found   = abap_false.
              l_gal_ctx_left         = abap_false.
              LOOP AT lt_back_callstack_copy ASSIGNING <l_callstack_copy_line>.
                IF   <l_callstack_copy_line>-mainprogram = 'SAPMSSY1' AND
                     ( <l_callstack_copy_line>-blockname = '%_RFC_START'
                       OR
                       <l_callstack_copy_line>-blocktype = 'FORM' AND <l_callstack_copy_line>-blockname = 'REMOTE_FUNCTION_CALL'
                      )
                  OR
                      <l_callstack_copy_line>-mainprogram = 'OS_APPLICATION' AND
                      <l_callstack_copy_line>-blockname   = 'RUN_OO_APPLICATION'.
                  l_gal_ctx_left = abap_true.
                  CONTINUE.
                ENDIF.
                IF l_gal_ctx_left = abap_false AND <l_callstack_copy_line>-mainprogram CP '/GAL/*'.
                  CONTINUE.
                ENDIF.

                l_non_gal_prog_found = abap_true.
                l_non_gal_prog = <l_callstack_line>-mainprogram.
                EXIT.
              ENDLOOP.
              IF l_non_gal_prog_found = abap_false.
                EXIT.
              ELSE.
                DESCRIBE TABLE lt_back_callstack_copy LINES l_idx_rev.
                DO 3 TIMES.
                  l_idx_do = sy-index.
                  READ TABLE lt_back_callstack_copy INDEX l_idx_rev ASSIGNING <l_callstack_copy_line>.
                  IF l_idx_do < 3.
                    IF <l_callstack_copy_line>-mainprogram NE 'OS_APPLICATION' OR
                       <l_callstack_copy_line>-blockname   NE 'RUN_OO_APPLICATION'.
                      EXIT.
                    ENDIF.
                  ELSE.
                    IF <l_callstack_copy_line>-mainprogram CP '/GAL/CCM_APP_REP_BROWSER*' AND
                       <l_callstack_copy_line>-blockname   EQ 'RUN'.
                      l_non_gal_prog_found = abap_false.
                      CLEAR l_non_gal_prog.
                    ENDIF.
                  ENDIF.
                  l_idx_rev = l_idx_rev - 1.
                ENDDO.
                IF l_non_gal_prog_found = abap_false.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDIF.

          WHEN 4.
            CHECK <l_callstack_line>-blockname NE '/GAL/RFC_PROXY_FUNCTION' OR <l_callstack_line>-blocktype NE 'FUNCTION'.

          WHEN 5.
            IF l_rfc_scope_extended_context = abap_false.
              CHECK <l_callstack_line>-mainprogram NP '/GAL/TASK*' OR <l_callstack_line>-blockname NE 'START_SYNC' OR <l_callstack_line>-blocktype NE 'METHOD'.
            ELSE.
              IF <l_callstack_line>-blockname = '/GAL/RFC_PROXY_FUNCTION' AND <l_callstack_line>-blocktype = 'FORM'.
                CONTINUE.
              ENDIF.
              IF <l_callstack_line>-mainprogram CP '/GAL/TASK*' AND <l_callstack_line>-blockname = 'START_SYNC' AND <l_callstack_line>-blocktype = 'METHOD'.
                l_rfc_scope_extended_context = abap_false.
                CONTINUE.
              ENDIF.
            ENDIF.

          WHEN 6.
            IF l_rfc_scope_extended_context = abap_false.
              IF <l_callstack_line>-mainprogram CP '/GAL/*'.
                l_galileo_context = abap_true.
                IF l_direct_call = abap_true.
                  EXIT.
                ENDIF.
                CONTINUE.
              ELSE.
                l_custom_allow = abap_false.

                /gal/cfw_auth_config=>init_config_params( ).
                LOOP AT /gal/cfw_auth_config=>custom_allowed_callers ASSIGNING <l_custom_allowed_caller>.
                  CHECK <l_callstack_line>-mainprogram CP <l_custom_allowed_caller>-mainprogram AND
                        <l_callstack_line>-blocktype   CP <l_custom_allowed_caller>-blocktype   AND
                        <l_callstack_line>-blockname   CP <l_custom_allowed_caller>-blockname   AND
                        l_trigger_function             CP <l_custom_allowed_caller>-called_function.
                  l_custom_allow = abap_true.
                  EXIT.
                ENDLOOP.
                IF l_custom_allow = abap_true.
                  EXIT.
                ELSE.
                  l_non_gal_prog = <l_callstack_line>-mainprogram.
                ENDIF.
              ENDIF.
            ELSE.
              IF <l_callstack_line>-mainprogram = 'SAPMSSY1' AND <l_callstack_line>-blocktype = 'FORM' AND <l_callstack_line>-blockname = 'REMOTE_FUNCTION_CALL'.
                EXIT.
              ENDIF.
            ENDIF.
            CHECK l_direct_call = abap_true.

          WHEN 7.

            IF l_galileo_context = abap_true AND <l_callstack_line>-mainprogram NP '/GAL/*'.
              l_custom_allow = abap_false.
              /gal/cfw_auth_config=>init_config_params( ).
              LOOP AT /gal/cfw_auth_config=>custom_allowed_callers ASSIGNING <l_custom_allowed_caller>.
                CHECK <l_callstack_line>-mainprogram CP <l_custom_allowed_caller>-mainprogram AND
                      <l_callstack_line>-blocktype   CP <l_custom_allowed_caller>-blocktype   AND
                      <l_callstack_line>-blockname   CP <l_custom_allowed_caller>-blockname   AND
                      l_trigger_function             CP <l_custom_allowed_caller>-called_function.
                l_custom_allow = abap_true.
                EXIT.
              ENDLOOP.
              IF l_custom_allow = abap_true.
                EXIT.
              ENDIF.
              l_non_gal_prog = <l_callstack_line>-mainprogram.
            ELSE.
              IF <l_callstack_line>-mainprogram CP '/GAL/*'.
                CONTINUE.
              ELSE.
                l_non_gal_prog = <l_callstack_line>-mainprogram.
              ENDIF.
            ENDIF.

          WHEN 8.
            READ TABLE <lt_back_callstack> WITH KEY mainprogram = 'SAPLSEUJ' TRANSPORTING NO FIELDS.
            CHECK sy-subrc = 0.

        ENDCASE.
      ENDIF.

      READ TABLE <l_rfc_route_info>-step_infos INDEX 1 ASSIGNING <l_first_step_info>.
      IF sy-subrc = 0.
        /gal/trace=>write_text(
          EXPORTING
            text                    = 'RFC call auth error calling  function {1} by user {2} on system {3} client {4}'
            var01                   = l_trigger_function
            var02                   = <l_first_step_info>-user_id
            var03                   = <l_first_step_info>-system_id
            var04                   = <l_first_step_info>-client_id
        ).                                                  "#EC NOTEXT
      ELSE.
        /gal/trace=>write_text(
          EXPORTING
            text                    = 'Internal Error. Initial Step Infos in RFC check error for function {1}'
            var01                   = l_trigger_function
        ).                                                  "#EC NOTEXT
      ENDIF.
      IF l_non_gal_prog IS INITIAL.
        regular_auth_exception_found(
          EXPORTING
            textid = /gal/cx_cfw_auth_exception=>rfc_back_call_stack_incons2
        ).
      ELSE.
        regular_auth_exception_found(
          EXPORTING
            textid = /gal/cx_cfw_auth_exception=>forbidden_rfc_caller
            var1   = l_non_gal_prog
        ).
      ENDIF.
      IF /gal/cfw_auth_config=>trace_violation_details = abap_true.
        /gal/trace=>write_text(
          EXPORTING
            text                    = 'Stack parser metadata: {1} {2}'
            var01                   = <l_rfc_route_info>-current_step
            var02                   = l_index
            no_flush                = abap_true
        ).                                                  "#EC NOTEXT
        /gal/trace=>write_callstack(
          custom_callstack = <lt_back_callstack>
        ).
      ELSE.
        l_callstack_offset = l_callstack_index - 1.
        /gal/trace=>write_callstack(
          callstack_offset = l_callstack_offset
          number_of_lines  = 2
          custom_callstack = <lt_back_callstack>
        ).
      ENDIF.
      RETURN.
    ENDDO.

  ENDMETHOD.


  METHOD check_rfc_context_external.
    "Wrapper for checking the RFC context for forbidden callstacks from outside this class

    check_rfc_context(
       function_name_external  = function_name
       back_callstack_external = back_callstack_external
       rfc_route_info_external = rfc_route_info_external
    ).
    int_rfc_context_checked = abap_true.
  ENDMETHOD.


  METHOD fill_cfw_auth_err_ex_info.

    "Fill the exception info for RFC processing from an existing exception object

    DATA l_symsgv1               TYPE symsgv.
    DATA l_symsgv2               TYPE symsgv.
    DATA l_symsgv3               TYPE symsgv.
    DATA l_symsgv4               TYPE symsgv.
    DATA l_cfw_auth_exception_fw TYPE REF TO /gal/cx_cfw_auth_excep_fw. "#EC NEEDED

    exception_info = /gal/cfw_helper=>get_exception_info( exception = cfw_auth_exception ).
    exception_info-message_id     = '/GAL/CFW'.
    exception_info-message_type   = 'E'.
    TRY.
        l_cfw_auth_exception_fw ?= cfw_auth_exception.
        DO 0 TIMES. MESSAGE e019(/gal/cfw) WITH '' '' '' ''. ENDDO.
        exception_info-message_number = '019'.
      CATCH cx_sy_move_cast_error.
        DO 0 TIMES. MESSAGE e021(/gal/cfw) WITH '' '' '' ''. ENDDO.
        exception_info-message_number = '021'.
    ENDTRY.

    /gal/string=>string_to_message_vars(
      EXPORTING
        input = exception_info-message_text
      IMPORTING
        msgv1 = l_symsgv1
        msgv2 = l_symsgv2
        msgv3 = l_symsgv3
        msgv4 = l_symsgv4
    ).
    exception_info-message_var1 = l_symsgv1.
    exception_info-message_var2 = l_symsgv2.
    exception_info-message_var3 = l_symsgv3.
    exception_info-message_var4 = l_symsgv4.

  ENDMETHOD.


  METHOD init.

    IF config IS INITIAL.
      "Initialize config object
      CREATE OBJECT config.
    ENDIF.

  ENDMETHOD.


  METHOD raise_auth_exception_if_exists.
    DATA l_auth_exception TYPE REF TO /gal/cx_cfw_auth_exception.

    "Raise a previously found auth exception if existing
    IF NOT auth_exception IS INITIAL.
      l_auth_exception = auth_exception.
      CLEAR auth_exception.
      RAISE EXCEPTION l_auth_exception.
    ENDIF.
  ENDMETHOD.


  METHOD regular_auth_exception_found.

    " This method stores a regular auth exception that occured for further processing.
    " Either the exception obejct or text data can be specified.
    " If activated the caller callstack is written to trace.

    DATA l_var1 TYPE string.
    DATA l_var2 TYPE string.
    DATA l_var3 TYPE string.

    l_var1 = var1.
    l_var2 = var2.
    l_var3 = var3.

    TRY.
        IF auth_exception_object IS INITIAL.
          RAISE EXCEPTION TYPE /gal/cx_cfw_auth_exception
            EXPORTING
              textid = textid
              var1   = l_var1
              var2   = l_var2
              var3   = l_var3.
        ELSE.
          RAISE EXCEPTION auth_exception_object.
        ENDIF.
      CATCH /gal/cx_cfw_auth_exception INTO auth_exception.
        /gal/trace=>write_exception(
          EXPORTING
            exception = auth_exception
         ).
        IF /gal/cfw_auth_config=>trace_violation_details = abap_true.
          /gal/trace=>write_callstack( ).
        ENDIF.
    ENDTRY.
  ENDMETHOD.


  METHOD reset_context_data.

    " All context data is cleared.
    " If no force is specified, the clear is denied in case an exception has been found but not handled yet.

    DATA l_unhandled_text TYPE string.

    CLEAR ctx_function_name.
    CLEAR ctx_param_bindings.
    CLEAR ctx_initial_caller_sysysid.
    CLEAR ctx_initial_caller_symandt.
    CLEAR ctx_initial_caller_syuname.
    CLEAR ctx_last_caller_sysysid.
    CLEAR ctx_last_caller_symandt.
    CLEAR ctx_last_caller_syuname.
    CLEAR ctx_local_execution.
    CLEAR ctx_first_step.
    CLEAR ctx_final_step.
    CLEAR ctx_direct_call.
    CLEAR ctx_rfc_route_info.
    CLEAR int_rfc_context_checked.

    IF NOT auth_exception IS INITIAL.
      IF force_auth_exception_discard = abap_true.
        CLEAR auth_exception.
        RETURN.
      ENDIF.
      l_unhandled_text = auth_exception->get_text( ).
      /gal/trace=>write_callstack( ).
      RAISE EXCEPTION TYPE /gal/cx_cfw_auth_excep_fw
        EXPORTING
          textid = /gal/cx_cfw_auth_excep_fw=>ctx_reset_in_err_state
          var1   = l_unhandled_text.
    ENDIF.
  ENDMETHOD.


  METHOD set_context_data.

    " Set all needed context data.

    DATA l_var1 TYPE string.
    DATA l_var2 TYPE string.


    IF NOT /gal/cfw_auth=>ctx_function_name IS INITIAL AND /gal/cfw_auth=>ctx_function_name <> function_name.
      " Overwriting already set context data is not allowed.
      " Existing data has to be processed and cleared before.
      /gal/trace=>write_text(
        text     = 'INTERNAL ERROR: Overwriting context in CFW auth framwork. This should not happen. Old: {1} New: {2}'
        var01    = /gal/cfw_auth=>ctx_function_name
        var02    = function_name
        no_flush = 'X'
      ).                                                    "#EC NOTEXT
      /gal/trace=>write_callstack( ).
      l_var1 = /gal/cfw_auth=>ctx_function_name.
      l_var2 = function_name.
      RAISE EXCEPTION TYPE /gal/cx_cfw_auth_excep_fw
        EXPORTING
          textid = /gal/cx_cfw_auth_excep_fw=>ctx_in_use_overwrite
          var1   = l_var1
          var2   = l_var2.
    ENDIF.

    /gal/cfw_auth=>ctx_function_name                  = function_name.
    /gal/cfw_auth=>ctx_param_bindings                 = param_bindings.
    /gal/cfw_auth=>ctx_initial_caller_sysysid         = initial_caller_sysysid.
    /gal/cfw_auth=>ctx_initial_caller_symandt         = initial_caller_symandt.
    /gal/cfw_auth=>ctx_initial_caller_syuname         = initial_caller_syuname.
    /gal/cfw_auth=>ctx_last_caller_sysysid            = last_caller_sysysid.
    /gal/cfw_auth=>ctx_last_caller_symandt            = last_caller_symandt.
    /gal/cfw_auth=>ctx_last_caller_syuname            = last_caller_syuname.
    /gal/cfw_auth=>ctx_local_execution                = local_execution.
    /gal/cfw_auth=>ctx_first_step                     = first_step.
    /gal/cfw_auth=>ctx_final_step                     = final_step.
    /gal/cfw_auth=>ctx_direct_call                    = direct_call.
    /gal/cfw_auth=>ctx_rfc_route_info                 = rfc_route_info.

  ENDMETHOD.
ENDCLASS.
