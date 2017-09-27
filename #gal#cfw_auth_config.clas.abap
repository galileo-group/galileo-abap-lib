class /GAL/CFW_AUTH_CONFIG definition
  public
  final
  create public

  global friends /GAL/CFW_AUTH .

public section.
  type-pools ABAP .

  class-methods IS_ACTIVE
    returning
      value(ACTIVE) type ABAP_BOOL
    raising
      /GAL/CX_CFW_AUTH_EXCEP_FW .
  methods CONSTRUCTOR
    raising
      /GAL/CX_CFW_AUTH_EXCEP_FW .
protected section.
private section.

  class-data CFW_SEC_CONFIG_FOLDER type ref to /GAL/CONFIG_NODE .
  class-data INIT_STATUS type /GAL/CFW_AUTH_INIT_STATUS .
  class-data INT_IS_ENABLED_CACHE type /GAL/CFW_EXTENDED_FLAG .
  class-data CUSTOM_ALLOWED_CALLERS type /GAL/CFW_CUSTOM_ALLOW_CALLERS .
  class-data IGNORED_FUNCTIONS type /GAL/CFW_IGNORED_FUNCTIONS .
  class-data INT_PARAM_INIT_DONE type ABAP_BOOL .
  class-data LOCAL_ONLY_FUNCTIONS type /GAL/CFW_LOCAL_FUNCTIONS .
  class-data TRACE_VIOLATION_DETAILS type ABAP_BOOL .

  class-methods INIT_CONFIG_FOLDER
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  class-methods INIT_CONFIG_PARAMS
    raising
      /GAL/CX_CFW_AUTH_EXCEP_FW .
ENDCLASS.



CLASS /GAL/CFW_AUTH_CONFIG IMPLEMENTATION.


  METHOD constructor.

    DATA l_config_node             TYPE REF TO /gal/config_node.
    DATA l_disabled                TYPE abap_bool.
    DATA l_ex_conf                 TYPE REF TO /gal/cx_config_exception.
    DATA l_error_message           TYPE string.


    TRY.

        IF NOT int_is_enabled_cache IS INITIAL.
          IF int_is_enabled_cache = '0'.
            init_status = 'D'.
            RETURN.
          ENDIF.
        ELSE.
          init_config_folder( ).
          l_config_node = cfw_sec_config_folder->get_child_node( `Disable Enhanced Security Framework` ). "#EC NOTEXT
          l_config_node->get_value( IMPORTING value = l_disabled ).
          IF l_disabled = abap_true.
            init_status = 'D'.
            RETURN.
          ENDIF.
        ENDIF.

        init_config_params( ).

        init_status = 'E'.

      CATCH /gal/cx_config_exception INTO l_ex_conf.
        l_error_message = l_ex_conf->get_text( ).
        RAISE EXCEPTION TYPE /gal/cx_cfw_auth_excep_fw
          EXPORTING
            textid = /gal/cx_cfw_auth_excep_fw=>config_exception
            var1   = l_error_message.
    ENDTRY.


  ENDMETHOD.


  METHOD init_config_folder.
    DATA l_config_store            TYPE REF TO /gal/config_store_local.

    IF cfw_sec_config_folder IS INITIAL.
      CREATE OBJECT l_config_store.
      cfw_sec_config_folder = l_config_store->get_node( path = `/Galileo Group AG/Open Source Components/Communication Framework/Security` ). "#EC NOTEXT
    ENDIF.
  ENDMETHOD.


  METHOD init_config_params.

    DATA l_config_node             TYPE REF TO /gal/config_node.
    DATA l_ex_conf                 TYPE REF TO /gal/cx_config_exception.
    DATA l_error_message           TYPE string.


    IF int_param_init_done = abap_true.
      RETURN.
    ENDIF.

    TRY.

        init_config_folder( ).

        l_config_node = cfw_sec_config_folder->get_child_node( `Ignored Functions` ). "#EC NOTEXT
        l_config_node->get_value( IMPORTING value = ignored_functions ).
        DELETE ignored_functions WHERE table_line IS INITIAL.

        l_config_node = cfw_sec_config_folder->get_child_node( `Custom Allowed Callers` ). "#EC NOTEXT
        l_config_node->get_value( IMPORTING value = custom_allowed_callers ).
        DELETE custom_allowed_callers WHERE table_line IS INITIAL.

        l_config_node = cfw_sec_config_folder->get_child_node( `Local Only Functions` ). "#EC NOTEXT
        l_config_node->get_value( IMPORTING value = local_only_functions ).
        DELETE local_only_functions WHERE table_line IS INITIAL.

        l_config_node = cfw_sec_config_folder->get_child_node( `Trace Authentification Violation Details` ). "#EC NOTEXT
        l_config_node->get_value( IMPORTING value = trace_violation_details ).

        int_param_init_done = abap_true.

      CATCH /gal/cx_config_exception INTO l_ex_conf.
        l_error_message = l_ex_conf->get_text( ).
        RAISE EXCEPTION TYPE /gal/cx_cfw_auth_excep_fw
          EXPORTING
            textid = /gal/cx_cfw_auth_excep_fw=>config_exception
            var1   = l_error_message.
    ENDTRY.

  ENDMETHOD.


  METHOD is_active.

    DATA l_config_node             TYPE REF TO /gal/config_node.
    DATA l_disabled                TYPE abap_bool.
    DATA l_conf_ex                 TYPE REF TO /gal/cx_config_exception.
    DATA l_error_message           TYPE string.


    IF init_status = 'E'.

      active = abap_true.

    ELSEIF init_status IS INITIAL.

      IF int_is_enabled_cache IS INITIAL.

        TRY.

            init_config_folder( ).

            l_config_node = cfw_sec_config_folder->get_child_node( `Disable Enhanced Security Framework` ). "#EC NOTEXT
            l_config_node->get_value( IMPORTING value = l_disabled ).
            IF l_disabled = abap_true.
              active = abap_false.
              int_is_enabled_cache = '0'.
            ELSE.
              active = abap_true.
              int_is_enabled_cache = '1'.
            ENDIF.

          CATCH /gal/cx_config_exception INTO l_conf_ex.
            l_error_message = l_conf_ex->get_text( ).
            RAISE EXCEPTION TYPE /gal/cx_cfw_auth_excep_fw
              EXPORTING
                textid = /gal/cx_cfw_auth_excep_fw=>config_exception
                var1   = l_error_message.
        ENDTRY.

      ELSE.

        IF int_is_enabled_cache = '1'.
          active = abap_true.
        ELSE.
          active = abap_false.
        ENDIF.

      ENDIF.

    ELSE.

      active = abap_false.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
