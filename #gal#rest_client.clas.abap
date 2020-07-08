"! <p class="shorttext synchronized" lang="en">Client for Rest HTTP web services</p>
CLASS /gal/rest_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPE-POOLS:
      abap.


    DATA:

      "! <p class="shorttext synchronized" lang="en">Base URI</p>
      base_uri    TYPE string READ-ONLY,

      "! <p class="shorttext synchronized" lang="en">Host</p>
      host        TYPE string READ-ONLY,

      "! <p class="shorttext synchronized" lang="en">Compatibility mode for old SAP releases</p>
      legacy_mode TYPE abap_bool READ-ONLY,

      "! <p class="shorttext synchronized" lang="en">Protocol (HTTP/HTTPS)</p>
      scheme      TYPE i READ-ONLY,

      "! <p class="shorttext synchronized" lang="en">Service</p>
      service     TYPE string READ-ONLY,

      "! <p class="shorttext synchronized" lang="en">SSL client identity</p>
      ssl_id      TYPE ssfapplssl READ-ONLY.


    CLASS-METHODS:

      "! <p class="shorttext synchronized" lang="en">Create from web service name</p>
      "!
      "! @parameter path                          | <p class="shorttext synchronized" lang="en">Path in Configuration Store</p>
      "! @parameter name                          | <p class="shorttext synchronized" lang="en">Web Service Name</p>
      "! @parameter client                        | <p class="shorttext synchronized" lang="en">Web Service Client</p>
      "! @raising   /gal/cx_rest_client_exception | <p class="shorttext synchronized" lang="en">Exception in connection with the HTTP REST Client</p>
      create_by_name
        IMPORTING
          path          TYPE string DEFAULT `/Galileo Group AG/Open Source Components/Web Infrastructure/Web Services` "#EC NOTEXT
          name          TYPE string
        RETURNING
          VALUE(client) TYPE REF TO /gal/rest_client
        RAISING
          /gal/cx_rest_client_exception,

      "! <p class="shorttext synchronized" lang="en">Create from URI</p>
      "!
      "! @parameter uri               | <p class="shorttext synchronized" lang="en">URI</p>
      "! @parameter ssl_id            | <p class="shorttext synchronized" lang="en">SSL client identity</p>
      "! @parameter client            | <p class="shorttext synchronized" lang="en">Web Service Client</p>
      "! @parameter force_legacy_mode | <p class="shorttext synchronized" lang="en">Force compatibility mode (for test purposes only)</p>
      create_from_uri
        IMPORTING
          uri               TYPE csequence
          ssl_id            TYPE ssfapplssl DEFAULT 'ANONYM'
          force_legacy_mode TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(client)     TYPE REF TO /gal/rest_client.


    METHODS:

      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter host              | <p class="shorttext synchronized" lang="en">Host</p>
      "! @parameter service           | <p class="shorttext synchronized" lang="en">Service</p>
      "! @parameter scheme            | <p class="shorttext synchronized" lang="en">Protocol (HTTP/HTTPS)</p>
      "! @parameter base_uri          | <p class="shorttext synchronized" lang="en">Base URI</p>
      "! @parameter ssl_id            | <p class="shorttext synchronized" lang="en">SSL client identity</p>
      "! @parameter force_legacy_mode | <p class="shorttext synchronized" lang="en">Force compatibility mode (for test purposes only)</p>
      constructor
        IMPORTING
          host              TYPE string
          service           TYPE string OPTIONAL
          scheme            TYPE i DEFAULT cl_http_client=>schemetype_http
          base_uri          TYPE string OPTIONAL
          ssl_id            TYPE ssfapplssl DEFAULT 'ANONYM'
          force_legacy_mode TYPE abap_bool DEFAULT abap_false,

      "! <p class="shorttext synchronized" lang="en">Process DELETE requests</p>
      "!
      "! @parameter uri                           | <p class="shorttext synchronized" lang="en">URI</p>
      "! @parameter response                      | <p class="shorttext synchronized" lang="en">Response</p>
      "! @raising   /gal/cx_rest_client_exception | <p class="shorttext synchronized" lang="en">Exception in connection with the HTTP REST Client</p>
      delete
        IMPORTING
          !uri            TYPE string OPTIONAL
        RETURNING
          VALUE(response) TYPE REF TO /gal/rest_response
        RAISING
          /gal/cx_rest_client_exception,

      "! <p class="shorttext synchronized" lang="en">Process GET request</p>
      "!
      "! @parameter uri                           | <p class="shorttext synchronized" lang="en">URI</p>
      "! @parameter parameters                    | <p class="shorttext synchronized" lang="en">Parameters</p>
      "! @parameter response                      | <p class="shorttext synchronized" lang="en">Response</p>
      "! @raising   /gal/cx_rest_client_exception | <p class="shorttext synchronized" lang="en">Exception in connection with the HTTP REST Client</p>
      get
        IMPORTING
          !uri            TYPE string OPTIONAL
          !parameters     TYPE /gal/uri_parameters OPTIONAL
        RETURNING
          VALUE(response) TYPE REF TO /gal/rest_response
        RAISING
          /gal/cx_rest_client_exception,

      "! <p class="shorttext synchronized" lang="en">Process POST request</p>
      "!
      "! @parameter uri                           | <p class="shorttext synchronized" lang="en">URI</p>
      "! @parameter request                       | <p class="shorttext synchronized" lang="en">Request</p>
      "! @parameter response                      | <p class="shorttext synchronized" lang="en">Response</p>
      "! @raising   /gal/cx_rest_client_exception | <p class="shorttext synchronized" lang="en">Exception in connection with the HTTP REST Client</p>
      post
        IMPORTING
          !uri            TYPE string OPTIONAL
          !request        TYPE REF TO /gal/rest_request
        RETURNING
          VALUE(response) TYPE REF TO /gal/rest_response
        RAISING
          /gal/cx_rest_client_exception,

      "! <p class="shorttext synchronized" lang="en">Process PUT request</p>
      "!
      "! @parameter uri                           | <p class="shorttext synchronized" lang="en">URI</p>
      "! @parameter request                       | <p class="shorttext synchronized" lang="en">Request</p>
      "! @parameter response                      | <p class="shorttext synchronized" lang="en">Response</p>
      "! @raising   /gal/cx_rest_client_exception | <p class="shorttext synchronized" lang="en">Exception in connection with the HTTP REST Client</p>
      put
        IMPORTING
          !uri            TYPE string OPTIONAL
          !request        TYPE REF TO /gal/rest_request
        RETURNING
          VALUE(response) TYPE REF TO /gal/rest_response
        RAISING
          /gal/cx_rest_client_exception.


  PROTECTED SECTION.


  PRIVATE SECTION.

    DATA:

      "! <p class="shorttext synchronized" lang="en">Cache for HTTP client</p>
      http_client_cache TYPE REF TO if_http_client,

      "! <p class="shorttext synchronized" lang="en">Cache for REST Client</p>
      rest_client_cache TYPE REF TO object.                 "#EC NEEDED


    METHODS:

      "! <p class="shorttext synchronized" lang="en">Build query string for URI</p>
      "!
      "! @parameter parameters   | <p class="shorttext synchronized" lang="en">Parameters</p>
      "! @parameter query_string | <p class="shorttext synchronized" lang="en">Query string</p>
      build_query_string
        IMPORTING
          !parameters         TYPE /gal/uri_parameters
        RETURNING
          VALUE(query_string) TYPE string,
      "! <p class="shorttext synchronized" lang="en">Build full URI</p>
      "!
      "! @parameter uri        | <p class="shorttext synchronized" lang="en">URI</p>
      "! @parameter parameters | <p class="shorttext synchronized" lang="en">Parameters</p>
      "! @parameter output     | <p class="shorttext synchronized" lang="en">Full URI</p>
      build_uri
        IMPORTING
          !uri          TYPE string OPTIONAL
          !parameters   TYPE /gal/uri_parameters OPTIONAL
        RETURNING
          VALUE(output) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Create HTTP Client</p>
      "!
      "! @parameter http_client                   | <p class="shorttext synchronized" lang="en">HTTP Client</p>
      "! @raising   /gal/cx_rest_client_exception | <p class="shorttext synchronized" lang="en">Exception in connection with the HTTP REST Client</p>
      get_http_client
        RETURNING
          VALUE(http_client) TYPE REF TO if_http_client
        RAISING
          /gal/cx_rest_client_exception,

      "! <p class="shorttext synchronized" lang="en">Create HTTP REST Client</p>
      "!
      "! @parameter rest_client                   | <p class="shorttext synchronized" lang="en">REST Client</p>
      "! @raising   /gal/cx_rest_client_exception | <p class="shorttext synchronized" lang="en">Exception in connection with the HTTP REST Client</p>
      get_rest_client
        RETURNING
          VALUE(rest_client) TYPE REF TO object
        RAISING
          /gal/cx_rest_client_exception,

      "! <p class="shorttext synchronized" lang="en">Process HTTP Request (Compatibility Mode)</p>
      "!
      "! @parameter client                        | <p class="shorttext synchronized" lang="en">HTTP Client</p>
      "! @parameter method                        | <p class="shorttext synchronized" lang="en">HTTP Method</p>
      "! @parameter uri                           | <p class="shorttext synchronized" lang="en">URI</p>
      "! @parameter parameters                    | <p class="shorttext synchronized" lang="en">Parameters</p>
      "! @parameter request                       | <p class="shorttext synchronized" lang="en">Request</p>
      "! @parameter response                      | <p class="shorttext synchronized" lang="en">Response</p>
      "! @raising   /gal/cx_rest_client_exception | <p class="shorttext synchronized" lang="en">Exception in connection with the HTTP REST Client</p>
      process_http_request
        IMPORTING
          !client         TYPE REF TO object
          !method         TYPE string
          !uri            TYPE string OPTIONAL
          !parameters     TYPE /gal/uri_parameters OPTIONAL
          !request        TYPE REF TO /gal/rest_request OPTIONAL
        RETURNING
          VALUE(response) TYPE REF TO /gal/rest_response
        RAISING
          /gal/cx_rest_client_exception,                    "#EC CALLED

      "! <p class="shorttext synchronized" lang="en">Prepare REST Request</p>
      "!
      "! @parameter client                        | <p class="shorttext synchronized" lang="en">REST Client</p>
      "! @parameter method                        | <p class="shorttext synchronized" lang="en">HTTP Method</p>
      "! @parameter uri                           | <p class="shorttext synchronized" lang="en">URI</p>
      "! @parameter parameters                    | <p class="shorttext synchronized" lang="en">Parameters</p>
      "! @parameter request                       | <p class="shorttext synchronized" lang="en">Request</p>
      "! @parameter response                      | <p class="shorttext synchronized" lang="en">Response</p>
      "! @raising   /gal/cx_rest_client_exception | <p class="shorttext synchronized" lang="en">Exception in connection with the HTTP REST Client</p>
      process_rest_request
        IMPORTING
          !client         TYPE REF TO object
          !method         TYPE string
          !uri            TYPE string OPTIONAL
          !parameters     TYPE /gal/uri_parameters OPTIONAL
          !request        TYPE REF TO /gal/rest_request OPTIONAL
        RETURNING
          VALUE(response) TYPE REF TO /gal/rest_response
        RAISING
          /gal/cx_rest_client_exception,                    "#EC CALLED

      "! <p class="shorttext synchronized" lang="en">Validate HTTP Response</p>
      "!
      "! @parameter response                      | <p class="shorttext synchronized" lang="en">Response from a Rest HTTP web service</p>
      "! @raising   /gal/cx_rest_client_exception | <p class="shorttext synchronized" lang="en">Exception in connection with the HTTP REST Client</p>
      validate_response
        IMPORTING
          !response TYPE REF TO /gal/rest_response
        RAISING
          /gal/cx_rest_client_exception.

ENDCLASS.



CLASS /gal/rest_client IMPLEMENTATION.


  METHOD build_query_string.
    DATA l_escaped_value TYPE string.
    DATA l_is_first      TYPE abap_bool.

    FIELD-SYMBOLS <l_parameter> LIKE LINE OF parameters.

    l_is_first = abap_true.

    LOOP AT parameters ASSIGNING <l_parameter>.
      l_escaped_value = cl_http_utility=>escape_url( <l_parameter>-value ).

      IF l_is_first = abap_true.
        CONCATENATE <l_parameter>-name `=` l_escaped_value INTO query_string.
        l_is_first = abap_false.
      ELSE.
        CONCATENATE query_string `&` <l_parameter>-name `=` l_escaped_value INTO query_string.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_uri.
    DATA l_query_string TYPE string.
    DATA l_length       TYPE i.

    IF base_uri IS INITIAL.
      output = uri.
    ELSEIF uri IS INITIAL.
      output = base_uri.
    ELSEIF uri CP '/*'.
      CONCATENATE base_uri uri INTO output.
    ELSE.
      CONCATENATE base_uri uri INTO output SEPARATED BY '/'.
    ENDIF.

    IF parameters IS NOT INITIAL.
      l_query_string = build_query_string( parameters ).

      IF output CP '*/'.
        l_length = strlen( output ) - 1.
        output   = output(l_length).
      ENDIF.

      CONCATENATE output `?` l_query_string INTO output.
    ELSEIF output NP '*/'.
      CONCATENATE output '/' INTO output.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    DATA l_offset TYPE i.
    DATA l_length TYPE i.

* Initialize connection details
    me->host   = host.
    me->scheme = scheme.
    me->ssl_id = ssl_id.

* Determine service to be used
    IF service IS INITIAL.
      IF scheme = cl_http_client=>schemetype_http.
        me->service = `80`.
      ELSEIF scheme = cl_http_client=>schemetype_https.
        me->service = `443`.
      ENDIF.
    ELSE.
      me->service = service.
    ENDIF.

* Copy base URI
    l_offset = 0.
    l_length = strlen( base_uri ).

    IF base_uri(1) = '/'.
      l_offset = 1.
      l_length = l_length - 1.
    ENDIF.

    IF base_uri CP '*/'.
      l_length = l_length - 1.
    ENDIF.

    IF l_length > 0.
      CONCATENATE '/' base_uri+l_offset(l_length) INTO me->base_uri.
    ELSE.
      CLEAR me->base_uri.
    ENDIF.

* Check if legacy mode is required
    IF force_legacy_mode = abap_false.
      legacy_mode = abap_false.

      cl_abap_typedescr=>describe_by_name( EXPORTING  p_name = 'IF_REST_CLIENT'
                                           EXCEPTIONS OTHERS = 1 ).
      IF sy-subrc <> 0.
        legacy_mode = abap_true.
      ENDIF.
    ELSE.
      legacy_mode = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD create_by_name.
    DATA:
      l_config_store          TYPE REF TO /gal/config_store_local,
      l_config_base_folder    TYPE REF TO /gal/config_node,
      l_config_service_folder TYPE REF TO /gal/config_node,
      l_config_node           TYPE REF TO /gal/config_node,

      l_host                  TYPE string,
      l_port                  TYPE string VALUE `80`,
      l_protocol              TYPE string VALUE `HTTP`,
      l_base_uri              TYPE string VALUE ``,
      l_force_legacy_mode     TYPE abap_bool VALUE abap_false,

      l_scheme                TYPE i,

      l_ssl_id                TYPE ssfapplssl,

      l_exception             TYPE REF TO cx_root,
      l_message               TYPE string.

    TRY.

* Create instance of configuration store
        CREATE OBJECT l_config_store.

* Get instance of folder containing configuration values
        l_config_base_folder    = l_config_store->get_node( path = path ).
        l_config_service_folder = l_config_base_folder->get_child_node( name ).

* Get web service configuration values
        l_config_node = l_config_service_folder->get_child_node( `Host` ). "#EC NOTEXT
        l_config_node->get_value( IMPORTING value = l_host ).

        TRY.
            l_config_node = l_config_service_folder->get_child_node( `Port` ). "#EC NOTEXT
            l_config_node->get_value( EXPORTING default_value = l_port
                                      IMPORTING value         = l_port ).
          CATCH /gal/cx_config_exception.               "#EC NO_HANDLER
        ENDTRY.

        TRY.
            l_config_node = l_config_service_folder->get_child_node( `Protocol` ). "#EC NOTEXT
            l_config_node->get_value( EXPORTING default_value = l_protocol
                                      IMPORTING value         = l_protocol ).
          CATCH /gal/cx_config_exception.               "#EC NO_HANDLER
        ENDTRY.

        TRY.
            l_config_node = l_config_service_folder->get_child_node( `Base URI` ). "#EC NOTEXT
            l_config_node->get_value( EXPORTING default_value = l_base_uri
                                      IMPORTING value         = l_base_uri ).
          CATCH /gal/cx_config_exception.               "#EC NO_HANDLER
        ENDTRY.

        TRY.
            l_config_node = l_config_service_folder->get_child_node( `SSL Identity` ). "#EC NOTEXT
            l_config_node->get_value( EXPORTING default_value = l_ssl_id
                                      IMPORTING value         = l_ssl_id ).
          CATCH /gal/cx_config_exception.               "#EC NO_HANDLER
        ENDTRY.

        TRY.
            l_config_node = l_config_service_folder->get_child_node( `Force Legacy Mode` ). "#EC NOTEXT
            l_config_node->get_value( EXPORTING default_value = l_force_legacy_mode
                                      IMPORTING value         = l_force_legacy_mode ).
          CATCH /gal/cx_config_exception.               "#EC NO_HANDLER
        ENDTRY.

* Convert protocol
        IF l_protocol CP `HTTP`.
          l_scheme = cl_http_client=>schemetype_http.
        ELSEIF l_protocol CP `HTTPS`.
          l_scheme = cl_http_client=>schemetype_https.
        ELSE.
          RAISE EXCEPTION TYPE /gal/cx_rest_client_exception
            EXPORTING
              textid = /gal/cx_rest_client_exception=>unknown_protocol
              var1   = l_protocol.
        ENDIF.

* Create web service client
        CREATE OBJECT client
          EXPORTING
            host              = l_host
            service           = l_port
            scheme            = l_scheme
            base_uri          = l_base_uri
            ssl_id            = l_ssl_id
            force_legacy_mode = l_force_legacy_mode.

      CATCH /gal/cx_config_exception INTO l_exception.
        l_message = l_exception->get_text( ).

        RAISE EXCEPTION TYPE /gal/cx_rest_client_exception
          EXPORTING
            textid = /gal/cx_rest_client_exception=>custom_exception
            var1   = l_message.

    ENDTRY.
  ENDMETHOD.


  METHOD create_from_uri.
    DATA:
      l_host        TYPE string,
      l_service     TYPE i,
      l_service_str TYPE string,
      l_scheme      TYPE i,
      l_path        TYPE string.

* Parse URI
    /gal/uri_utilities=>parse_uri( EXPORTING uri = uri
                                   IMPORTING host = l_host
                                             service = l_service
                                             scheme  = l_scheme
                                             path    = l_path ).

* Create web service client
    l_service_str = l_service.
    l_service_str = /gal/string=>trim( l_service_str ).

    CREATE OBJECT client
      EXPORTING
        host              = l_host
        service           = l_service_str
        scheme            = l_scheme
        base_uri          = l_path
        ssl_id            = ssl_id
        force_legacy_mode = force_legacy_mode.
  ENDMETHOD.


  METHOD delete.
    DATA l_client TYPE REF TO object.
    DATA l_method TYPE string.

* Select implementation depending on SAP release
    IF legacy_mode = abap_true.
      l_client = get_http_client( ).
      l_method = `PROCESS_HTTP_REQUEST`.                    "#EC NOTEXT
    ELSE.
      l_client = get_rest_client( ).
      l_method = `PROCESS_REST_REQUEST`.                    "#EC NOTEXT
    ENDIF.

* Process request
    CALL METHOD (l_method)
      EXPORTING
        client   = l_client
        method   = /gal/http_constants=>method_delete
        uri      = uri
      RECEIVING
        response = response.
  ENDMETHOD.


  METHOD get.
    DATA l_client TYPE REF TO object.
    DATA l_method TYPE string.

* Select implementation depending on SAP release
    IF legacy_mode = abap_true.
      l_client = get_http_client( ).
      l_method = `PROCESS_HTTP_REQUEST`.                    "#EC NOTEXT
    ELSE.
      l_client = get_rest_client( ).
      l_method = `PROCESS_REST_REQUEST`.                    "#EC NOTEXT
    ENDIF.

* Process request
    CALL METHOD (l_method)
      EXPORTING
        client     = l_client
        method     = /gal/http_constants=>method_get
        uri        = uri
        parameters = parameters
      RECEIVING
        response   = response.
  ENDMETHOD.


  METHOD get_http_client.
    DATA:
      l_exception TYPE REF TO /gal/cx_rest_client_exception.

* Re-use existing HTTP client instance
    IF http_client_cache IS NOT INITIAL.
      http_client = http_client_cache.
    ELSE.

* Create new HTTP client
      cl_http_client=>create( EXPORTING  host    = host
                                         service = service
                                         scheme  = scheme
                                         ssl_id  = ssl_id
                              IMPORTING  client  = http_client
                              EXCEPTIONS OTHERS  = 1 ).
      IF sy-subrc <> 0.
        l_exception ?= /gal/cx_exception=>create_from_classic_exception( exception_class = `/GAL/CX_REST_CLIENT_EXCEPTION` ).
        RAISE EXCEPTION l_exception.
      ENDIF.

      http_client_cache = http_client.
    ENDIF.
  ENDMETHOD.


  METHOD get_rest_client.
    DATA l_http_client TYPE REF TO if_http_client.

*--------------------------------------------------------------------
* Caching has been disabled because of an error in the SAP coding of
* method CL_REST_HTTP_CLIENT=>SEND_RECEIVE:
*
* Sending a request of type PUT or POST sets the private attribute
* MO_REQUEST_ENTITY to the request instance. All subsequent GET or
* DELETE calls will then run into an error because these methods
* do not send a request an therefore the current request is always
* different from the private attribute MO_REQUEST_ENTITY.
*--------------------------------------------------------------------

** Re-use existing REST client instance
*  IF rest_client_cache IS NOT INITIAL.
*    rest_client = rest_client_cache.
*  ELSE.

* Get HTTP client
    l_http_client = get_http_client( ).

* Create new REST client
    CREATE OBJECT rest_client TYPE ('CL_REST_HTTP_CLIENT')
      EXPORTING
        io_http_client = l_http_client.

*    rest_client_cache = rest_client.
*  ENDIF.
  ENDMETHOD.


  METHOD post.
    DATA l_client TYPE REF TO object.
    DATA l_method TYPE string.

* Select implementation depending on SAP release
    IF legacy_mode = abap_true.
      l_client = get_http_client( ).
      l_method = `PROCESS_HTTP_REQUEST`.                    "#EC NOTEXT
    ELSE.
      l_client = get_rest_client( ).
      l_method = `PROCESS_REST_REQUEST`.                    "#EC NOTEXT
    ENDIF.

* Process request
    CALL METHOD (l_method)
      EXPORTING
        client   = l_client
        method   = /gal/http_constants=>method_post
        uri      = uri
        request  = request
      RECEIVING
        response = response.
  ENDMETHOD.


  METHOD process_http_request.
    DATA l_client        TYPE REF TO if_http_client.
    DATA l_uri           TYPE string.
    DATA l_header_fields TYPE tihttpnvp.
    DATA l_message       TYPE string.

* Build full URI
    l_uri = build_uri( uri        = uri
                       parameters = parameters ).

* Prepare request
    l_client ?= client.

    IF request IS NOT INITIAL.
      request->get_http_request( l_client ).
    ENDIF.

    l_client->request->set_method( method ).

    cl_http_utility=>set_request_uri( request = l_client->request
                                      uri     = l_uri ).

* Send request
    l_client->send( EXCEPTIONS http_communication_failure = 1
                               http_invalid_state         = 2
                               http_processing_failed     = 3
                               http_invalid_timeout       = 4
                               OTHERS                     = 5 ).
    IF sy-subrc <> 0.
      CASE sy-subrc.

        WHEN 1.
          l_message = TEXT-e01.

        WHEN 2.
          l_message = TEXT-e02.

        WHEN 3.
          l_message = TEXT-e03.

        WHEN 4.
          l_message = TEXT-e04.

        WHEN OTHERS.
          l_message = TEXT-e05.

      ENDCASE.

      RAISE EXCEPTION TYPE /gal/cx_rest_client_exception
        EXPORTING
          textid = /gal/cx_rest_client_exception=>error_sending_http_request
          var1   = l_message.
    ENDIF.

* Receive response
    l_client->receive( EXCEPTIONS http_communication_failure = 1
                                  http_invalid_state         = 2
                                  http_processing_failed     = 3
                                  OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      CASE sy-subrc.

        WHEN 1.
          l_message = TEXT-e01.

        WHEN 2.
          l_message = TEXT-e02.

        WHEN 3.
          l_message = TEXT-e03.

        WHEN OTHERS.
          l_message = TEXT-e05.

      ENDCASE.

      RAISE EXCEPTION TYPE /gal/cx_rest_client_exception
        EXPORTING
          textid = /gal/cx_rest_client_exception=>error_receiving_http_response
          var1   = l_message.
    ENDIF.

* Get header fields
    l_client->response->get_header_fields( CHANGING fields = l_header_fields ).

* Create response object
    CREATE OBJECT response
      EXPORTING
        http_response = l_client->response
        header_fields = l_header_fields.

* Validate response
    validate_response( response ).
  ENDMETHOD.


  METHOD process_rest_request.
    DATA l_uri               TYPE string.

    DATA l_request           TYPE REF TO object.
    DATA l_response          TYPE REF TO object.

    DATA l_request_interface TYPE REF TO data.

    DATA l_method            TYPE string.

    DATA l_header_fields     TYPE tihttpnvp.

    DATA l_message           TYPE string.
    DATA l_exception         TYPE REF TO cx_root.

    FIELD-SYMBOLS <l_request> TYPE any.

* Build full URI
    l_uri = build_uri( uri        = uri
                       parameters = parameters ).

* Prepare request header
    CALL METHOD client->('IF_REST_CLIENT~SET_REQUEST_HEADER')
      EXPORTING
        iv_name  = if_http_header_fields_sap=>request_uri
        iv_value = l_uri.

* Send request
    TRY.
        CONCATENATE `IF_REST_RESOURCE` method INTO l_method SEPARATED BY '~'.

        IF request IS INITIAL.
          CALL METHOD client->(l_method).
        ELSE.
          l_request = request->get_rest_request( client ).

          CREATE DATA l_request_interface TYPE REF TO ('IF_REST_ENTITY').
          ASSIGN l_request_interface->* TO <l_request>.

          <l_request> ?= l_request.

          CALL METHOD client->(l_method)
            EXPORTING
              io_entity = <l_request>.
        ENDIF.

      CATCH cx_root INTO l_exception.                    "#EC CATCH_ALL
        l_message = l_exception->get_text( ).

        RAISE EXCEPTION TYPE /gal/cx_rest_client_exception
          EXPORTING
            textid = /gal/cx_rest_client_exception=>custom_exception
            var1   = l_message.

    ENDTRY.

* Receive response
    CALL METHOD client->('IF_REST_CLIENT~GET_RESPONSE_ENTITY')
      RECEIVING
        ro_response_entity = l_response.

* Get header fields
    CALL METHOD client->('IF_REST_CLIENT~GET_RESPONSE_HEADERS')
      RECEIVING
        rt_header_fields = l_header_fields.

* Create response object
    CREATE OBJECT response
      EXPORTING
        rest_response = l_response
        header_fields = l_header_fields.

* Validate response
    validate_response( response ).
  ENDMETHOD.


  METHOD put.
    DATA l_client TYPE REF TO object.
    DATA l_method TYPE string.

* Select implementation depending on SAP release
    IF legacy_mode = abap_true.
      l_client = get_http_client( ).
      l_method = `PROCESS_HTTP_REQUEST`.                    "#EC NOTEXT
    ELSE.
      l_client = get_rest_client( ).
      l_method = `PROCESS_REST_REQUEST`.                    "#EC NOTEXT
    ENDIF.

* Process request
    CALL METHOD (l_method)
      EXPORTING
        client   = l_client
        method   = /gal/http_constants=>method_put
        uri      = uri
        request  = request
      RECEIVING
        response = response.
  ENDMETHOD.


  METHOD validate_response.
    DATA l_status_code   TYPE string.
    DATA l_status_reason TYPE string.

* Process status information
    response->get_status_information( IMPORTING status_code   = l_status_code
                                                status_reason = l_status_reason ).

* Statuscodes >= 300 means that something went wrong
    IF l_status_code >= '3'.
      RAISE EXCEPTION TYPE /gal/cx_rest_client_exception
        EXPORTING
          textid = /gal/cx_rest_client_exception=>http_error
          var1   = l_status_code
          var2   = l_status_reason.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
