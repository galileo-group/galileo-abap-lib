CLASS /gal/uri_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPE-POOLS:
      abap.

    CONSTANTS:
      type_absolute               TYPE i VALUE 1,
      type_relative_path_absolute TYPE i VALUE 2,
      type_relative_path_noscheme TYPE i VALUE 3,
      type_relative_path_empty    TYPE i VALUE 4,
      type_undefined              TYPE i VALUE 0.

    CLASS-METHODS:

      "! <p class="shorttext synchronized" lang="en">Combine two URIs</p>
      "!
      "! @parameter base_uri | <p class="shorttext synchronized" lang="en">Base URI</p>
      "! @parameter path     | <p class="shorttext synchronized" lang="en">Path (must be relative URI)</p>
      "! @parameter uri      | <p class="shorttext synchronized" lang="en">Result</p>
      combine_uris
        IMPORTING
          base_uri   TYPE csequence
          path       TYPE csequence
        RETURNING
          VALUE(uri) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Parse URI</p>
      "!
      "! @parameter uri         | <p class="shorttext synchronized" lang="en">URI</p>
      "! @parameter type        | <p class="shorttext synchronized" lang="en">URI type</p>
      "! @parameter scheme_name | <p class="shorttext synchronized" lang="en">Scheme name</p>
      "! @parameter scheme      | <p class="shorttext synchronized" lang="en">Scheme type for CL_HTTP_CLIENT</p>
      "! @parameter host        | <p class="shorttext synchronized" lang="en">Host</p>
      "! @parameter service     | <p class="shorttext synchronized" lang="en">Service</p>
      "! @parameter path        | <p class="shorttext synchronized" lang="en">Path</p>
      "! @parameter segments    | <p class="shorttext synchronized" lang="en">Path segments</p>
      "! @parameter fragment    | <p class="shorttext synchronized" lang="en">Fragment</p>
      "! @parameter query       | <p class="shorttext synchronized" lang="en">Encoded query string</p>
      parse_uri
        IMPORTING
          uri         TYPE csequence
        EXPORTING
          type        TYPE i
          scheme_name TYPE string
          scheme      TYPE i
          host        TYPE string
          service     TYPE i
          path        TYPE string
          segments    TYPE string_table
          fragment    TYPE string
          query       TYPE string.


  PROTECTED SECTION.


  PRIVATE SECTION.


ENDCLASS.



CLASS /gal/uri_utilities IMPLEMENTATION.

  METHOD combine_uris.
    DATA:
      l_base_uri               TYPE string,
      l_slash_at_end_of_uri1   TYPE abap_bool,
      l_slash_at_start_of_uri2 TYPE abap_bool.

* Nothing to do when there is just one URI
    IF base_uri IS INITIAL.
      uri = path.
    ELSEIF path IS INITIAL.
      uri = base_uri.
    ENDIF.

* Remove fragment and query from base URI (if any)
    IF base_uri CA '#?'.
      l_base_uri = base_uri(sy-fdpos).
    ELSE.
      l_base_uri = base_uri.
    ENDIF.

* Combine URIs
    l_slash_at_end_of_uri1 = /gal/string=>ends_with( input = l_base_uri
                                                     part  = `/` ).

    l_slash_at_start_of_uri2 = /gal/string=>starts_with( input = path
                                                         part  = `/` ).

    IF l_slash_at_end_of_uri1 = abap_true AND l_slash_at_start_of_uri2 = abap_true.
      CONCATENATE base_uri path+1 INTO uri. "Avoid duplicate slash
    ELSEIF l_slash_at_end_of_uri1 = abap_false AND l_slash_at_start_of_uri2 = abap_false.
      CONCATENATE base_uri path INTO uri SEPARATED BY '/'. "Add missing slash
    ELSE.
      CONCATENATE base_uri path INTO uri. "No conflict with slashes
    ENDIF.
  ENDMETHOD.


  METHOD parse_uri.
    DATA:
      l_offset  TYPE i.

    FIELD-SYMBOLS:
      <l_segment> LIKE LINE OF segments.

* Initialize result
    CLEAR:
      type,
      scheme_name,
      scheme,
      host,
      service,
      segments.

* Try to get scheme, host and service (absolute URIs only)
    IF uri CS `://`. "URI contains scheme -> absolute URI
      scheme_name = uri(sy-fdpos).
      type        = type_absolute.
      l_offset    = sy-fdpos + 3.

      IF scheme_name CP `HTTPS`.
        scheme = cl_http_client=>schemetype_https.
      ELSEIF scheme_name CP `HTTP`.
        scheme = cl_http_client=>schemetype_http.
      ENDIF.

      IF uri+l_offset CA '/:#?'. "Find end of host name
        host     = uri+l_offset(sy-fdpos).
        l_offset = l_offset + sy-fdpos.

        IF uri+l_offset(1) = ':'.
          l_offset = l_offset + 1.

          IF uri+l_offset CA '/#?'. "Find end of service
            IF sy-fdpos > 0.
              TRY.
                  service = uri+l_offset(sy-fdpos).

                  IF service < 1 OR service > 65535.     "#EC NUMBER_OK
                    CLEAR: host, service, type. "Service is not in valid range -> invalid URI
                  ENDIF.

                CATCH cx_sy_arithmetic_error cx_sy_move_cast_error.
                  CLEAR: host, service, type. "Service is not a valid number -> invalid URI

              ENDTRY.

              l_offset = l_offset + sy-fdpos.
            ELSE.
              type = type_undefined. "Missing service -> invalid URI
            ENDIF.
          ELSE."URI does not contain a path
            IF uri+l_offset IS NOT INITIAL.
              TRY.
                  service = uri+l_offset.

                  IF service < 1 OR service > 65535.     "#EC NUMBER_OK
                    CLEAR: host, service, type. "Service is not in valid range -> invalid URI
                  ENDIF.

                CATCH cx_sy_arithmetic_error cx_sy_move_cast_error.
                  CLEAR: host, service, type. "Service is not a valid number -> invalid URI

              ENDTRY.

              l_offset = strlen( uri ).
            ELSE.
              type = type_undefined. "Missing service -> invalid URI
            ENDIF.
          ENDIF.
        ELSEIF scheme = cl_http_client=>schemetype_https. "Use default service for HTTPS
          service = 443.                                 "#EC NUMBER_OK
        ELSEIF scheme = cl_http_client=>schemetype_http. "Use default service for HTTP
          service = 80.                                  "#EC NUMBER_OK
        ENDIF.
      ELSE. "URI does not contain a path
        host     = uri+l_offset.
        l_offset = strlen( uri ).

        IF host IS INITIAL.
          type = type_undefined.
        ENDIF.
      ENDIF.
    ELSEIF uri IS INITIAL.
      type = type_relative_path_empty.
    ELSEIF uri(1) = '/'.
      type = type_relative_path_absolute.
    ELSE.
      type = type_relative_path_noscheme.
    ENDIF.

* Get query from path
    SPLIT uri+l_offset AT '?' INTO path query.

* Get fragment from path
    SPLIT path AT '#' INTO path fragment.

* Split path into segments
    IF path IS NOT INITIAL AND segments IS REQUESTED.
      SPLIT path AT '/' INTO TABLE segments.

      LOOP AT segments ASSIGNING <l_segment>.
        CONCATENATE <l_segment> '/' INTO  <l_segment>.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
