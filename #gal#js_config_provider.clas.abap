class /GAL/JS_CONFIG_PROVIDER definition
  public
  create public .

*"* public components of class /GAL/JS_CONFIG_PROVIDER
*"* do not include other source files here!!!
public section.

  methods GET_STORE_DESTINATION
    returning
      value(RFC_DESTINATION) type /GAL/RFC_DESTINATION
    raising
      /GAL/CX_JS_EXCEPTION .
  methods GET_EXTENDED_DATA
    exporting
      value(DB_LAYER_VERSION) type /GAL/JS_DB_LAYER_VERSION
    raising
      /GAL/CX_JS_EXCEPTION .
protected section.
*"* protected components of class /GAL/JS_CONFIG_PROVIDER
*"* do not include other source files here!!!
private section.
*"* private components of class /GAL/JS_CONFIG_PROVIDER
*"* do not include other source files here!!!
ENDCLASS.



CLASS /GAL/JS_CONFIG_PROVIDER IMPLEMENTATION.


METHOD get_extended_data.

  DATA:
    l_store_dest     TYPE /gal/rfc_destination,
    l_rfc_route_info TYPE /gal/rfc_route_info,
    l_message        TYPE string.


  l_store_dest = get_store_destination( ).

  l_rfc_route_info = /gal/cfw_helper=>rfc_route_info_from_string(
      string = l_store_dest
  ).

  CALL FUNCTION '/GAL/JS_GET_EXT_CONF_DATA'
    EXPORTING
      rfc_route_info   = l_rfc_route_info
    IMPORTING
      db_layer_version = db_layer_version
    EXCEPTIONS
      rfc_exception    = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               INTO l_message.
    RAISE EXCEPTION TYPE /gal/cx_js_exception
      EXPORTING
        textid = /gal/cx_js_exception=>cannot_init_config
        var1   = l_message.
  ENDIF.

ENDMETHOD.


METHOD get_store_destination.
  DATA l_value TYPE /gal/js_config-value.

  SELECT SINGLE value
           FROM /gal/js_config
           INTO l_value
          WHERE attribute = 'CENTRAL_RFC_DEST'.

  IF sy-subrc <> 0 OR l_value IS INITIAL.
    RAISE EXCEPTION TYPE /gal/cx_js_exception
      EXPORTING
        textid = /gal/cx_js_exception=>cannot_read_config_param
        var1   = 'CENTRAL_RFC_DEST'.
  ENDIF.

  rfc_destination = l_value.
ENDMETHOD.
ENDCLASS.
