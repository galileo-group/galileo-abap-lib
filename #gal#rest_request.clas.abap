class /GAL/REST_REQUEST definition
  public
  final
  create public

  global friends /GAL/REST_CLIENT .

public section.

  data CONTENT_TYPE type STRING read-only .

  methods GET_BINARY_DATA
    returning
      value(DATA) type XSTRING .
  methods GET_STRING_DATA
    returning
      value(DATA) type STRING .
  methods SET_BINARY_DATA
    importing
      !DATA type XSTRING .
  methods SET_CONTENT_TYPE
    importing
      !CONTENT_TYPE type STRING .
  methods SET_JSON_DATA
    importing
      !DATA type ref to /GAL/JSON_ELEMENT
      !STRING_DELIMITER type C default ''''
    raising
      /GAL/CX_JSON_EXCEPTION .
  methods SET_STRING_DATA
    importing
      !DATA type STRING .
protected section.
private section.

  data BINARY_DATA type XSTRING .
  data STRING_DATA type STRING .

  methods GET_HTTP_REQUEST
    importing
      !CLIENT type ref to IF_HTTP_CLIENT
    returning
      value(REQUEST) type ref to IF_HTTP_REQUEST .
  methods GET_REST_REQUEST
    importing
      !CLIENT type ref to OBJECT
    returning
      value(REQUEST) type ref to OBJECT .
ENDCLASS.



CLASS /GAL/REST_REQUEST IMPLEMENTATION.


METHOD get_binary_data.
  DATA l_converter TYPE REF TO cl_abap_conv_out_ce.

  IF string_data IS INITIAL.
    data = binary_data.
  ELSE.
    l_converter = cl_abap_conv_out_ce=>create( ).
    l_converter->convert( EXPORTING data   = string_data
                          IMPORTING buffer = data ).
  ENDIF.
ENDMETHOD.


METHOD get_http_request.
  DATA l_binary_data TYPE xstring.

* Set content type
  client->request->set_content_type( content_type = content_type ).

* Set content
  IF binary_data IS NOT INITIAL.
    client->request->set_data( binary_data ).
  ELSEIF string_data IS NOT INITIAL.
    l_binary_data = get_binary_data( ).

    client->request->set_data( l_binary_data ).
  ENDIF.

* Return request object (usually not need in case of HTTP client)
  request = client->request.
ENDMETHOD.


METHOD get_rest_request.

* Create new REST request
  CALL METHOD client->('IF_REST_CLIENT~CREATE_REQUEST_ENTITY')
    RECEIVING
      ro_entity = request.

* Set content type
  CALL METHOD request->('IF_REST_ENTITY~SET_CONTENT_TYPE')
    EXPORTING
      iv_media_type = content_type.

* Set content
  IF binary_data IS NOT INITIAL.
    CALL METHOD request->('IF_REST_ENTITY~SET_BINARY_DATA')
      EXPORTING
        iv_data = binary_data.
  ELSEIF string_data IS NOT INITIAL.
    CALL METHOD request->('IF_REST_ENTITY~SET_STRING_DATA')
      EXPORTING
        iv_data = string_data.
  ENDIF.
ENDMETHOD.


METHOD get_string_data.
  DATA l_converter TYPE REF TO cl_abap_conv_in_ce.

  IF binary_data IS INITIAL.
    data = string_data.
  ELSE.
    l_converter = cl_abap_conv_in_ce=>create( input = binary_data ).
    l_converter->read( IMPORTING data = data ).
  ENDIF.
ENDMETHOD.


METHOD set_binary_data.
  binary_data = data.
  CLEAR string_data.
ENDMETHOD.


METHOD set_content_type.
  me->content_type = content_type.
ENDMETHOD.


METHOD set_json_data.
  DATA l_serializer TYPE REF TO /gal/json_serializer.

* Create JSON serializer
  CREATE OBJECT l_serializer
    EXPORTING
      string_delimiter = string_delimiter.

* Serialize request
  string_data = l_serializer->serialize( data ).

  CLEAR binary_data.
ENDMETHOD.


METHOD set_string_data.
  string_data = data.
  CLEAR binary_data.
ENDMETHOD.
ENDCLASS.
