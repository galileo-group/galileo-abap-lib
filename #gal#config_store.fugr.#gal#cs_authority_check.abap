FUNCTION /gal/cs_authority_check.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO
*"     REFERENCE(PATH) TYPE  STRING
*"     REFERENCE(ACTION) TYPE  I
*"     REFERENCE(CLIENT) TYPE  MANDT
*"     REFERENCE(USER_NAME) TYPE  UNAME
*"  EXCEPTIONS
*"      NODE_DOES_NOT_EXIST
*"      AUTHORITY_CHECK_FAILED
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception node_does_not_exist.
  cfw_pass_exception authority_check_failed.
  cfw_remote_coding.

  DATA l_config_store  TYPE REF TO /gal/config_store_local.
  DATA l_check_node    TYPE REF TO /gal/config_node.
  DATA l_node          TYPE REF TO /gal/config_node.
  DATA l_authenticator TYPE REF TO /gal/config_node_authenticator.

  DATA l_exception     TYPE REF TO cx_root.
  DATA l_message       TYPE string.

  FIELD-SYMBOLS <l_caller_info> LIKE LINE OF rfc_route_info-step_infos.

* Create local configuration store (on target system)
  CREATE OBJECT l_config_store.

* Get instance of current node
  TRY.
      l_check_node = l_config_store->get_node( path = path ).

    CATCH /gal/cx_config_exception INTO l_exception.
      l_message = l_exception->get_text( ).

      /gal/string=>string_to_message_vars( EXPORTING input = l_message
                                           IMPORTING msgv1 = sy-msgv1
                                                     msgv2 = sy-msgv2
                                                     msgv3 = sy-msgv3
                                                     msgv4 = sy-msgv4 ).

      MESSAGE e001 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                RAISING node_does_not_exist.

  ENDTRY.

* Determine authenticator class and create instance
  l_node = l_check_node.

  WHILE l_node IS NOT INITIAL AND l_node->authenticator_class IS INITIAL.
    l_node = l_node->parent.
  ENDWHILE.

  IF l_node IS NOT INITIAL.
    TRY.
        CREATE OBJECT l_authenticator TYPE (l_node->authenticator_class).

      CATCH cx_root.                                     "#EC CATCH_ALL
                                                        "#EC NO_HANDLER
        " Nothing needs to be done here

    ENDTRY.
  ENDIF.

  IF l_authenticator IS INITIAL.
    CREATE OBJECT l_authenticator TYPE /gal/config_node_auth_default.
  ENDIF.

* Perform authority check
  TRY.
      READ TABLE rfc_route_info-step_infos INDEX 1 ASSIGNING <l_caller_info>.

      IF sy-subrc = 0.
        l_authenticator->authority_check( original_system_id = <l_caller_info>-system_id
                                          original_client    = <l_caller_info>-client_id
                                          original_user      = <l_caller_info>-user_id
                                          node               = l_check_node
                                          action             = action
                                          client             = client
                                          user_name          = user_name ).
      ELSE.
        l_authenticator->authority_check( node      = l_check_node
                                          action    = action
                                          client    = client
                                          user_name = user_name ).
      ENDIF.

    CATCH /gal/cx_auth_check_exception INTO l_exception.
      l_message = l_exception->get_text( ).

      /gal/string=>string_to_message_vars( EXPORTING input = l_message
                                           IMPORTING msgv1 = sy-msgv1
                                                     msgv2 = sy-msgv2
                                                     msgv3 = sy-msgv3
                                                     msgv4 = sy-msgv4 ).

      MESSAGE e001 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                RAISING authority_check_failed.

  ENDTRY.
ENDFUNCTION.
