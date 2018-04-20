class /GAL/REST_RESPONSE definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  data HEADER_FIELDS type TIHTTPNVP read-only .
  data LEGACY_MODE type ABAP_BOOL read-only .

  methods CONSTRUCTOR
    importing
      !HTTP_RESPONSE type ref to IF_HTTP_RESPONSE optional
      !REST_RESPONSE type ref to OBJECT optional
      !HEADER_FIELDS type TIHTTPNVP optional
    raising
      /GAL/CX_REST_CLIENT_EXCEPTION .
  methods GET_BINARY_DATA
    returning
      value(DATA) type XSTRING .
  methods GET_CONTENT_TYPE
    exporting
      !CONTENT_TYPE type STRING
      !CHARSET type STRING .
  methods GET_JSON_DATA
    returning
      value(DATA) type ref to /GAL/JSON_ELEMENT
    raising
      /GAL/CX_JSON_EXCEPTION .
  methods GET_STATUS_INFORMATION
    exporting
      !STATUS_CODE type STRING
      !STATUS_REASON type STRING
    raising
      /GAL/CX_REST_CLIENT_EXCEPTION .
  methods GET_STRING_DATA
    returning
      value(DATA) type STRING .
protected section.
private section.

  data HTTP_RESPONSE type XSTRING .
  data REST_RESPONSE type ref to OBJECT .
ENDCLASS.



CLASS /GAL/REST_RESPONSE IMPLEMENTATION.


METHOD constructor.

* Copy response content
  IF rest_response IS INITIAL.
    IF http_response IS INITIAL.
      RAISE EXCEPTION TYPE /gal/cx_rest_client_exception
        EXPORTING
          textid = /gal/cx_rest_client_exception=>missing_parameters
          var1   = '/GAL/REST_RESPONSE'
          var2   = 'CONSTRUCTOR'.                           "#EC NOTEXT
    ELSE.
      me->http_response = http_response->get_data( ).
      legacy_mode       = abap_true.
    ENDIF.
  ELSE.
    me->rest_response = rest_response.
    legacy_mode       = abap_false.
  ENDIF.

* Copy header fields
  me->header_fields = header_fields.
ENDMETHOD.


METHOD get_binary_data.
  IF legacy_mode = abap_true.
    data = http_response.
  ELSE.
    CALL METHOD rest_response->('IF_REST_ENTITY~GET_BINARY_DATA')
      RECEIVING
        rv_data = data.
  ENDIF.
ENDMETHOD.


METHOD get_content_type.
  FIELD-SYMBOLS <l_header_field> LIKE LINE OF header_fields.

* Initialize result
  CLEAR content_type.
  CLEAR charset.

* Get content type
  LOOP AT header_fields ASSIGNING <l_header_field>
       WHERE name CS if_http_header_fields=>content_type.

    SPLIT <l_header_field>-value AT ';' INTO content_type charset.

    content_type = /gal/string=>trim( content_type ).
    charset      = /gal/string=>trim( charset ).

    IF charset CP `charset=+*`.
      charset = charset+8.
    ELSE.
      CLEAR charset.
    ENDIF.

    RETURN.
  ENDLOOP.
ENDMETHOD.


METHOD get_json_data.
  DATA l_serializer TYPE REF TO /gal/json_serializer.
  DATA l_string     TYPE string.

* Create JSON serializer
  CREATE OBJECT l_serializer.

* Deserialize response
  l_string = get_string_data( ).
  data     = l_serializer->deserialize( l_string ).
ENDMETHOD.


METHOD get_status_information.
  FIELD-SYMBOLS <l_header_field> LIKE LINE OF header_fields.

* Initialize result
  CLEAR status_code.
  CLEAR status_reason.

* Get status code
  READ TABLE header_fields
        WITH TABLE KEY name = if_http_header_fields_sap=>status_code
             ASSIGNING <l_header_field>.
  IF sy-subrc = 0.
    status_code = /gal/string=>trim( <l_header_field>-value ).
  ENDIF.

  IF status_code IS INITIAL.
    RAISE EXCEPTION TYPE /gal/cx_rest_client_exception
      EXPORTING
        textid = /gal/cx_rest_client_exception=>no_status_code_received.
  ENDIF.

* Get status reason (if requested)
  IF status_reason IS REQUESTED.
    READ TABLE header_fields
          WITH TABLE KEY name = if_http_header_fields_sap=>status_reason
               ASSIGNING <l_header_field>.
    IF sy-subrc = 0.
      status_reason = /gal/string=>trim( <l_header_field>-value ).
    ENDIF.

    IF status_reason IS INITIAL.
      status_reason = text-e01.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD get_string_data.
  DATA l_converter TYPE REF TO cl_abap_conv_in_ce.

  IF legacy_mode = abap_true.
    l_converter = cl_abap_conv_in_ce=>create( input = http_response ).
    l_converter->read( IMPORTING data = data ).
  ELSE.
    CALL METHOD rest_response->('IF_REST_ENTITY~GET_STRING_DATA')
      RECEIVING
        rv_data = data.
  ENDIF.
ENDMETHOD.
ENDCLASS.
