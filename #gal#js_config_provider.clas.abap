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
protected section.
*"* protected components of class /GAL/JS_CONFIG_PROVIDER
*"* do not include other source files here!!!
private section.
*"* private components of class /GAL/JS_CONFIG_PROVIDER
*"* do not include other source files here!!!
ENDCLASS.



CLASS /GAL/JS_CONFIG_PROVIDER IMPLEMENTATION.


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
