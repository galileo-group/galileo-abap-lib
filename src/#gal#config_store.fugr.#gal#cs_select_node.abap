FUNCTION /gal/cs_select_node.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO
*"     REFERENCE(ID) TYPE  /GAL/CONFIG_KEY_ID
*"  EXPORTING
*"     REFERENCE(PARENT_ID) TYPE  /GAL/CONFIG_PARENT_KEY_ID
*"     REFERENCE(NAME) TYPE  /GAL/CONFIG_KEY_NAME
*"     REFERENCE(NODE_TYPE) TYPE  /GAL/CONFIG_KEY_TYPE
*"     REFERENCE(FIXED_VALUE_TYPE) TYPE  /GAL/CONFIG_VALUE_TYPE
*"     REFERENCE(AUTH_CLASS) TYPE  /GAL/CONFIG_AUTH_CLASS_NAME
*"     REFERENCE(HAS_CHILD_NODES) TYPE  ABAP_BOOL
*"  EXCEPTIONS
*"      NODE_DOES_NOT_EXIST
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA l_dummy TYPE /gal/config_key_id.                     "#EC NEEDED

* Initialize result
  CLEAR parent_id.
  CLEAR name.
  CLEAR node_type.
  CLEAR fixed_value_type.
  CLEAR auth_class.
  CLEAR has_child_nodes.

* Follow RFC route
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception node_does_not_exist.
  cfw_remote_coding.

  CLEAR parent_id.
  CLEAR name.
  CLEAR node_type.

  SELECT SINGLE parent_id
                name
                type
                fixed_value_type
                auth_class
           FROM /gal/config_key
           INTO (parent_id, name, node_type, fixed_value_type, auth_class)
          WHERE id = id.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH id RAISING node_does_not_exist.
  ENDIF.

  IF has_child_nodes IS REQUESTED.
    IF node_type = /gal/config_node=>const_node_type_folder.
      SELECT id UP TO 1 ROWS
        INTO l_dummy
        FROM /gal/config_key
       WHERE parent_id = id.
        EXIT.
      ENDSELECT.

      IF sy-subrc = 0.
        has_child_nodes = abap_true.
      ELSE.
        has_child_nodes = abap_false.
      ENDIF.
    ELSE.
      has_child_nodes = abap_false.
    ENDIF.
  ENDIF.
ENDFUNCTION.
