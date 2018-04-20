FUNCTION /gal/js_get_ext_conf_data.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO
*"  EXPORTING
*"     REFERENCE(DB_LAYER_VERSION) TYPE  /GAL/JS_DB_LAYER_VERSION
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA:
    l_value(255) TYPE c.

  CLEAR db_layer_version.

  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_remote_coding.

  SELECT SINGLE value INTO l_value FROM /gal/js_config WHERE attribute = 'DB_LAYER_VERSION'.
  IF sy-subrc <> 0.
    db_layer_version = '000'.
    RETURN.
  ENDIF.

  db_layer_version = l_value.

ENDFUNCTION.
