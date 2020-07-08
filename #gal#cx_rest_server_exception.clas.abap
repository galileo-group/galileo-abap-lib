class /GAL/CX_REST_SERVER_EXCEPTION definition
  public
  inheriting from /GAL/CX_EXCEPTION
  final
  create public .

public section.
  type-pools ABAP .

  constants /GAL/CX_REST_SERVER_EXCEPTION type SOTR_CONC value '00155DF935091ED996F38F2BDA15D6CC'. "#EC NOTEXT
  constants HTTP_METHOD_NOT_SUPPORTED type SOTR_CONC value '00155DF935091ED997872195C74DB6CC'. "#EC NOTEXT
  constants INVALID_VALUE_FOR_URI_PARAM type SOTR_CONC value '00155DF935091ED997A64DCAB93316CC'. "#EC NOTEXT
  constants MISSING_VALUE_FOR_URI_PARAM type SOTR_CONC value '00155DF935091ED997A64DCAB93336CC'. "#EC NOTEXT
  data HTTP_STATUS_CODE type I read-only value 400. "#EC NOTEXT      "#EC NUMBER_OK

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !VAR1 type STRING optional
      !VAR2 type STRING optional
      !VAR3 type STRING optional
      !VAR4 type STRING optional
      !VAR5 type STRING optional
      !VAR6 type STRING optional
      !VAR7 type STRING optional
      !VAR8 type STRING optional
      !VAR9 type STRING optional
      !HTTP_STATUS_CODE type I default 400 .             "#EC NUMBER_OK
  methods TO_JSON
    returning
      value(RESULT) type STRING .
  methods TO_RESPONSE
    importing
      !RESPONSE type ref to OBJECT .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /GAL/CX_REST_SERVER_EXCEPTION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
VAR1 = VAR1
VAR2 = VAR2
VAR3 = VAR3
VAR4 = VAR4
VAR5 = VAR5
VAR6 = VAR6
VAR7 = VAR7
VAR8 = VAR8
VAR9 = VAR9
.
 IF textid IS INITIAL.
   me->textid = /GAL/CX_REST_SERVER_EXCEPTION .
 ENDIF.
me->HTTP_STATUS_CODE = HTTP_STATUS_CODE .
  endmethod.


  METHOD to_json.
    DATA:
      l_message      TYPE string,
      l_description  TYPE string,

      l_program_name TYPE syrepid,
      l_include_name TYPE syrepid,
      l_source_line  TYPE i,

      l_root_element TYPE REF TO /gal/json_element,

      l_serializer   TYPE REF TO /gal/json_serializer,

      l_exception    TYPE REF TO cx_root.

* Get exception information
    l_message     = get_text( ).
    l_description = get_longtext( ).

    get_source_position( IMPORTING program_name = l_program_name
                                   include_name = l_include_name
                                   source_line  = l_source_line ).

* Create JSON elements
    TRY.
        CREATE OBJECT l_root_element
          EXPORTING
            type = /gal/json_element=>type_object.

        l_root_element->add_child( name  = 'status'
                                   value = http_status_code ). "#EC NOTEXT

        l_root_element->add_child( name  = 'message'
                                   value = l_message ).     "#EC NOTEXT

        l_root_element->add_child( name  = 'description'
                                   value = l_description ). "#EC NOTEXT

        l_root_element->add_child( name  = 'abap-program'
                                   value = l_program_name ). "#EC NOTEXT

        l_root_element->add_child( name  = 'abap-include'
                                   value = l_include_name ). "#EC NOTEXT

        l_root_element->add_child( name  = 'abap-line'
                                   value = l_source_line ). "#EC NOTEXT

* Render JSON string
        CREATE OBJECT l_serializer
          EXPORTING
            pretty_print = abap_true.

        result = l_serializer->render( l_root_element ).

      CATCH /gal/cx_json_exception INTO l_exception.
        /gal/trace=>write_exception( l_exception ).

    ENDTRY.
  ENDMETHOD.


  METHOD to_response.
    DATA:
      l_message TYPE string,
      l_json    TYPE string,

      l_entity  TYPE REF TO object.

    l_message = get_text( ).
    l_json    = to_json( ).

    CALL METHOD response->('IF_REST_RESPONSE~SET_STATUS')
      EXPORTING
        iv_status        = http_status_code
        iv_reason_phrase = l_message.

    CALL METHOD response->('IF_REST_RESPONSE~SET_HEADER_FIELD')
      EXPORTING
        iv_name  = `Content-Type`
        iv_value = `application/json; charset=UTF-8`. "#EC NOTEXT

    CALL METHOD response->('IF_REST_RESPONSE~CREATE_ENTITY')
      EXPORTING
        iv_multipart = abap_false
      RECEIVING
        ro_entity    = l_entity.

    CALL METHOD l_entity->('IF_REST_ENTITY~SET_STRING_DATA')
      EXPORTING
        iv_data = l_json.
  ENDMETHOD.
ENDCLASS.
