"! <p class="shorttext synchronized" lang="en">Response from a Rest HTTP web service</p>
CLASS /gal/rest_response DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPE-POOLS abap.

    "! <p class="shorttext synchronized" lang="en">Header fields</p>
    DATA header_fields TYPE tihttpnvp READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Compatibility mode for old SAP releases</p>
    DATA legacy_mode TYPE abap_bool READ-ONLY .

    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    "!
    "! @parameter http_response                 | <p class="shorttext synchronized" lang="en">HTTP Response (for compatibility mode)</p>
    "! @parameter rest_response                 | <p class="shorttext synchronized" lang="en">REST Response</p>
    "! @parameter header_fields                 | <p class="shorttext synchronized" lang="en">Header fields</p>
    "! @raising   /gal/cx_rest_client_exception | <p class="shorttext synchronized" lang="en">Exception in connection with the HTTP REST Client</p>
    METHODS constructor
      IMPORTING
        !http_response TYPE REF TO if_http_response OPTIONAL
        !rest_response TYPE REF TO object OPTIONAL
        !header_fields TYPE tihttpnvp OPTIONAL
      RAISING
        /gal/cx_rest_client_exception .
    "! <p class="shorttext synchronized" lang="en">Return content as binary data</p>
    "!
    "! @parameter data | <p class="shorttext synchronized" lang="en">Binary Data</p>
    METHODS get_binary_data
      RETURNING
        VALUE(data) TYPE xstring .
    "! <p class="shorttext synchronized" lang="en">Determine content type</p>
    "!
    "! @parameter content_type | <p class="shorttext synchronized" lang="en">Content type</p>
    "! @parameter charset      | <p class="shorttext synchronized" lang="en">Charset</p>
    METHODS get_content_type
      EXPORTING
        !content_type TYPE string
        !charset      TYPE string .
    "! <p class="shorttext synchronized" lang="en">Return content as deserialized JSON data</p>
    "!
    "! @parameter data                   | <p class="shorttext synchronized" lang="en">JSON Element</p>
    "! @raising   /gal/cx_json_exception | <p class="shorttext synchronized" lang="en">JSON Serializer Exception</p>
    METHODS get_json_data
      RETURNING
        VALUE(data) TYPE REF TO /gal/json_element
      RAISING
        /gal/cx_json_exception .
    "! <p class="shorttext synchronized" lang="en">Return status information</p>
    "!
    "! @parameter status_code                   | <p class="shorttext synchronized" lang="en">HTTP status code</p>
    "! @parameter status_reason                 | <p class="shorttext synchronized" lang="en">HTTP status reason</p>
    "! @raising   /gal/cx_rest_client_exception | <p class="shorttext synchronized" lang="en">Exception in connection with the HTTP REST Client</p>
    METHODS get_status_information
      EXPORTING
        !status_code   TYPE string
        !status_reason TYPE string
      RAISING
        /gal/cx_rest_client_exception .
    "! <p class="shorttext synchronized" lang="en">Return content as string</p>
    "!
    "! @parameter data | <p class="shorttext synchronized" lang="en">String</p>
    METHODS get_string_data
      RETURNING
        VALUE(data) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    "! <p class="shorttext synchronized" lang="en">HTTP Response</p>
    DATA http_response TYPE xstring .
    "! <p class="shorttext synchronized" lang="en">REST Response</p>
    DATA rest_response TYPE REF TO object .
ENDCLASS.



CLASS /gal/rest_response IMPLEMENTATION.


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

* Parse response
    l_string = get_string_data( ).
    data     = l_serializer->parse( l_string ).
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
        status_reason = TEXT-e01.
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
