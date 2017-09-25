FUNCTION /gal/cs_select_node_descr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO
*"     REFERENCE(ID) TYPE  /GAL/CONFIG_KEY_ID
*"     REFERENCE(LANGUAGE) TYPE  LANGU
*"  EXPORTING
*"     REFERENCE(DESCRIPTION) TYPE  /GAL/CONFIG_KEY_TEXT
*"  EXCEPTIONS
*"      NODE_DOES_NOT_EXIST
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA l_dummy TYPE /gal/config_key_id.                     "#EC NEEDED

* Initialize result
  CLEAR description.

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

* Select node description
  SELECT SINGLE text
           FROM /gal/config_txt
           INTO description
          WHERE id    = id
            AND langu = language.                         "#EC CI_SUBRC
ENDFUNCTION.
