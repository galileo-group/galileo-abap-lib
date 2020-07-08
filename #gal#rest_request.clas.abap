"! <p class="shorttext synchronized" lang="en">Request for a Rest HTTP Webservices</p>
CLASS /gal/rest_request DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS /gal/rest_client.

  PUBLIC SECTION.

    TYPE-POOLS:
      abap.


    INTERFACES:
      if_serializable_object.


    DATA:

    "! <p class="shorttext synchronized" lang="en">Content type</p>
      content_type TYPE string READ-ONLY.


    METHODS:

      "! <p class="shorttext synchronized" lang="en">Return content as binary data</p>
      "!
      "! @parameter data | <p class="shorttext synchronized" lang="en">Binary Data</p>
      get_binary_data
        RETURNING
          VALUE(data) TYPE xstring,

      "! <p class="shorttext synchronized" lang="en">Return content as string</p>
      "!
      "! @parameter data | <p class="shorttext synchronized" lang="en">Binary Data</p>
      get_string_data
        RETURNING
          VALUE(data) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Set content as binary data</p>
      "!
      "! @parameter data | <p class="shorttext synchronized" lang="en">Binary Data</p>
      set_binary_data
        IMPORTING
          !data TYPE xstring,

      "! <p class="shorttext synchronized" lang="en">Set content type</p>
      "!
      "! @parameter content_type | <p class="shorttext synchronized" lang="en">Content type</p>
      set_content_type
        IMPORTING
          !content_type TYPE string,

      "! <p class="shorttext synchronized" lang="en">Set content as deserialized JSON data</p>
      "!
      "! @parameter data                   | <p class="shorttext synchronized" lang="en">JSON Element</p>
      "! @parameter string_delimiter       | <p class="shorttext synchronized" lang="en">String delimiter</p>
      "! @parameter serializer             | <p class="shorttext synchronized" lang="en">JSON Serializer (optional)</p>
      "! @raising   /gal/cx_json_exception | <p class="shorttext synchronized" lang="en">JSON Serializer Exception</p>
      set_json_data
        IMPORTING
          data             TYPE REF TO /gal/json_element
          string_delimiter TYPE c DEFAULT ''''
          serializer       TYPE REF TO /gal/json_serializer OPTIONAL
        RAISING
          /gal/cx_json_exception,

      "! <p class="shorttext synchronized" lang="en">Set content as string</p>
      "!
      "! @parameter data | <p class="shorttext synchronized" lang="en">String</p>
      set_string_data
        IMPORTING
          !data TYPE string.


  PROTECTED SECTION.


  PRIVATE SECTION.

    DATA:

      "! <p class="shorttext synchronized" lang="en">Binary Data</p>
      binary_data TYPE xstring,

      "! <p class="shorttext synchronized" lang="en">Data as string</p>
      string_data TYPE string.


    METHODS:

      "! <p class="shorttext synchronized" lang="en">Create a Request for HTTP Client</p>
      "!
      "! @parameter client  | <p class="shorttext synchronized" lang="en">HTTP Client</p>
      "! @parameter request | <p class="shorttext synchronized" lang="en">Request Object</p>
      get_http_request
        IMPORTING
          !client        TYPE REF TO if_http_client
        RETURNING
          VALUE(request) TYPE REF TO if_http_request,

      "! <p class="shorttext synchronized" lang="en">Create a Request for REST Client</p>
      "!
      "! @parameter client  | <p class="shorttext synchronized" lang="en">REST Client</p>
      "! @parameter request | <p class="shorttext synchronized" lang="en">Request Object</p>
      get_rest_request
        IMPORTING
          !client        TYPE REF TO object
        RETURNING
          VALUE(request) TYPE REF TO object.

ENDCLASS.



CLASS /gal/rest_request IMPLEMENTATION.

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

* Create JSON serializer (if required)
    IF serializer IS INITIAL.
      CREATE OBJECT l_serializer
        EXPORTING
          string_delimiter = string_delimiter.
    ELSE.
      l_serializer = serializer.
    ENDIF.

* Render request content
    string_data = l_serializer->render( data ).

    CLEAR binary_data.
  ENDMETHOD.


  METHOD set_string_data.
    string_data = data.
    CLEAR binary_data.
  ENDMETHOD.

ENDCLASS.
