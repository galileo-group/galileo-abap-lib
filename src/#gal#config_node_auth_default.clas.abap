class /GAL/CONFIG_NODE_AUTH_DEFAULT definition
  public
  inheriting from /GAL/CONFIG_NODE_AUTHENTICATOR
  final
  create public .

public section.
  type-pools ABAP .

  methods AUTHORITY_CHECK
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /GAL/CONFIG_NODE_AUTH_DEFAULT IMPLEMENTATION.


METHOD authority_check.
  DATA l_var1 TYPE string.

* Restrict to current client
  IF client IS NOT INITIAL AND client <> sy-mandt.
    l_var1 = client.

    RAISE EXCEPTION TYPE /gal/cx_auth_check_exception
      EXPORTING
        textid = /gal/cx_auth_check_exception=>not_authorized_for_client
        var1   = l_var1.
  ENDIF.

* Authority checks
  CASE action.

    WHEN /gal/config_node_actions=>delete_default_value
      OR /gal/config_node_actions=>delete_documentation
      OR /gal/config_node_actions=>delete_node
      OR /gal/config_node_actions=>modify_default_value
      OR /gal/config_node_actions=>modify_documentation
      OR /gal/config_node_actions=>modify_node.

      AUTHORITY-CHECK OBJECT 'S_TABU_DIS'
                          ID 'ACTVT'     FIELD '02'
                          ID 'DICBERCLS' FIELD '&NC&'.
      IF sy-subrc = 0.
        AUTHORITY-CHECK OBJECT 'S_TABU_CLI'
                            ID 'CLIIDMAINT' FIELD 'X'.
      ENDIF.

    WHEN /gal/config_node_actions=>delete_value
      OR /gal/config_node_actions=>modify_value.

      AUTHORITY-CHECK OBJECT 'S_TABU_DIS'
                          ID 'ACTVT'     FIELD '02'
                          ID 'DICBERCLS' FIELD '&NC&'.

      IF sy-subrc = 0.
        IF node->type = /gal/config_node=>const_node_type_value_system.
          AUTHORITY-CHECK OBJECT 'S_TABU_CLI'
                              ID 'CLIIDMAINT' FIELD 'X'.
        ENDIF.
      ENDIF.

    WHEN OTHERS.
      RETURN.

  ENDCASE.

  IF sy-subrc <> 0 OR
     sy-sysid <> original_system_id OR
     sy-mandt <> original_client OR
     sy-uname <> original_user.

    RAISE EXCEPTION TYPE /gal/cx_auth_check_exception
      EXPORTING
        textid = /gal/cx_auth_check_exception=>not_authorized.
  ENDIF.
ENDMETHOD.
ENDCLASS.
