FUNCTION /gal/cs_select_child_nodes.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO
*"     REFERENCE(ID) TYPE  /GAL/CONFIG_KEY_ID
*"  EXPORTING
*"     REFERENCE(DATA) TYPE  /GAL/CONFIG_KEYS
*"  EXCEPTIONS
*"      NODE_DOES_NOT_EXIST
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA l_dummy TYPE /gal/config_key_id.                     "#EC NEEDED

* Initialize result
  CLEAR data.

* Follow RFC route
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception node_does_not_exist.
  cfw_remote_coding.

* Select node (existence check)
  SELECT SINGLE id
           INTO l_dummy
           FROM /gal/config_key
          WHERE id = id.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH id RAISING node_does_not_exist.
  ENDIF.

* Select child nodes
  SELECT *
    FROM /gal/config_key
    INTO TABLE data
   WHERE parent_id = id.                                  "#EC CI_SUBRC
ENDFUNCTION.
