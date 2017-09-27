FUNCTION /gal/cs_select_node_value.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO
*"     REFERENCE(ID) TYPE  /GAL/CONFIG_KEY_ID
*"     REFERENCE(NODE_TYPE) TYPE  /GAL/CONFIG_KEY_TYPE
*"     REFERENCE(DEFAULT_VALUE) TYPE  ABAP_BOOL DEFAULT ABAP_FALSE
*"     REFERENCE(CLIENT) TYPE  MANDT DEFAULT 'ALL'
*"     REFERENCE(USER_NAME) TYPE  UNAME OPTIONAL
*"  EXPORTING
*"     REFERENCE(VALUE_TYPE) TYPE  /GAL/CONFIG_KEY_TEXT
*"     REFERENCE(VALUE) TYPE  /GAL/CONFIG_VALUE
*"  EXCEPTIONS
*"      NODE_DOES_NOT_EXIST
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA l_dummy TYPE /gal/config_key_id.                     "#EC NEEDED

* Initialize result
  CLEAR value_type.
  CLEAR value.

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

* Select requested value
  CLEAR value_type.
  CLEAR value.

  IF default_value = abap_true.
    SELECT SINGLE type value
             FROM /gal/config_val
             INTO (value_type, value)
            WHERE id = id.                                "#EC CI_SUBRC
  ELSE.
    CASE node_type.

      WHEN /gal/config_node=>const_node_type_value_client.
        SELECT SINGLE type value
                 FROM /gal/config_cval CLIENT SPECIFIED
                 INTO (value_type, value)
                WHERE client = client                    "#EC CI_CLIENT
                  AND id     = id.                        "#EC CI_SUBRC

      WHEN /gal/config_node=>const_node_type_value_system.
        SELECT SINGLE type value
                 FROM /gal/config_sval
                 INTO (value_type, value)
                WHERE id = id.                            "#EC CI_SUBRC

      WHEN /gal/config_node=>const_node_type_value_user.
        SELECT SINGLE type value
                 FROM /gal/config_uval CLIENT SPECIFIED
                 INTO (value_type, value)
                WHERE client    = client
                  AND user_name = user_name              "#EC CI_CLIENT
                  AND id        = id.                     "#EC CI_SUBRC

    ENDCASE.
  ENDIF.
ENDFUNCTION.
