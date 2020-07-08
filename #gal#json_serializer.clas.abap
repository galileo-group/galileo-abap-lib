"! <p class="shorttext synchronized" lang="en">JSON Element</p>
CLASS /gal/json_serializer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPE-POOLS:
      abap.


    CONSTANTS:

      "! <p class="shorttext synchronized" lang="en">Name transformation: None</p>
      transform_none          TYPE i VALUE 0,

      "! <p class="shorttext synchronized" lang="en">Name transformation: To lower-case</p>
      transform_to_lower      TYPE i VALUE 1,

      "! <p class="shorttext synchronized" lang="en">Name transformation: To upper-case</p>
      transform_to_upper      TYPE i VALUE 2,

      "! <p class="shorttext synchronized" lang="en">Name transformation: ABAP to CamelCase</p>
      transform_abap_to_camel TYPE i VALUE 3,

      "! <p class="shorttext synchronized" lang="en">Name transformation: CamelCase to ABAP</p>
      transform_camel_to_abap TYPE i VALUE 4.


    DATA:

      "! <p class="shorttext synchronized" lang="en">Converter for JavaScript Literals</p>
      converter              TYPE REF TO /gal/javascript_converter READ-ONLY,

      "! <p class="shorttext synchronized" lang="en">String used for indentation</p>
      indent                 TYPE string,

      "! <p class="shorttext synchronized" lang="en">String used as line break</p>
      line_break             TYPE string,

      "! <p class="shorttext synchronized" lang="en">Pretty print mode</p>
      pretty_print           TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">Name transformation applied when parsing</p>
      parse_transform        TYPE i,

      "! <p class="shorttext synchronized" lang="en">Name transformation applied when rendering</p>
      render_transform       TYPE i,

      "! <p class="shorttext synchronized" lang="en">Serialization handlers</p>
      serialization_handlers TYPE STANDARD TABLE OF REF TO /gal/json_serhnd_base.


    METHODS:

      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter string_delimiter | <p class="shorttext synchronized" lang="en">String delimiter</p>
      "! @parameter indent           | <p class="shorttext synchronized" lang="en">String used for indentation</p>
      "! @parameter line_break       | <p class="shorttext synchronized" lang="en">String used as line break</p>
      "! @parameter pretty_print     | <p class="shorttext synchronized" lang="en">Pretty print mode</p>
      "! @parameter parse_transform  | <p class="shorttext synchronized" lang="en">Name transformation applied when parsing</p>
      "! @parameter render_transform | <p class="shorttext synchronized" lang="en">Name transformation applied when rendering</p>
      constructor
        IMPORTING
          string_delimiter TYPE c DEFAULT '"'
          indent           TYPE string DEFAULT /gal/string=>tab
          line_break       TYPE string DEFAULT /gal/string=>line_break_unix
          pretty_print     TYPE abap_bool DEFAULT abap_false
          parse_transform  TYPE i DEFAULT transform_none
          render_transform TYPE i DEFAULT transform_none,

      "! <p class="shorttext synchronized" lang="en">Deserialize to JSON</p>
      "!
      "! @parameter input                  | <p class="shorttext synchronized" lang="en">JSON to be deserialized</p>
      "! @parameter output                 | <p class="shorttext synchronized" lang="en">Deserialization result</p>
      "! @raising   /gal/cx_json_exception | <p class="shorttext synchronized" lang="en">JSON Serializer Exception</p>
      deserialize
        IMPORTING
          input  TYPE REF TO /gal/json_element
        CHANGING
          output TYPE any
        RAISING
          /gal/cx_json_exception,

      "! <p class="shorttext synchronized" lang="en">Parse string in JSON format</p>
      "!
      "! @parameter input                  | <p class="shorttext synchronized" lang="en">String in JSON format</p>
      "! @parameter output                 | <p class="shorttext synchronized" lang="en">JSON Element</p>
      "! @raising   /gal/cx_json_exception | <p class="shorttext synchronized" lang="en">JSON Serializer Exception</p>
      parse
        IMPORTING
          input         TYPE string
        RETURNING
          VALUE(output) TYPE REF TO /gal/json_element
        RAISING
          /gal/cx_json_exception,

      "! <p class="shorttext synchronized" lang="en">Render JSON to string</p>
      "!
      "! @parameter input                  | <p class="shorttext synchronized" lang="en">JSON Element</p>
      "! @parameter output                 | <p class="shorttext synchronized" lang="en">String in JSON format</p>
      "! @raising   /gal/cx_json_exception | <p class="shorttext synchronized" lang="en">JSON Serializer Exception</p>
      render
        IMPORTING
          input         TYPE REF TO /gal/json_element
        RETURNING
          VALUE(output) TYPE string
        RAISING
          /gal/cx_json_exception,

      "! <p class="shorttext synchronized" lang="en">Serialize to JSON</p>
      "!
      "! @parameter input                  | <p class="shorttext synchronized" lang="en">Data to be serialized</p>
      "! @parameter output                 | <p class="shorttext synchronized" lang="en">Serialization result (JSON Element)</p>
      "! @raising   /gal/cx_json_exception | <p class="shorttext synchronized" lang="en">JSON Serializer Exception</p>
      serialize
        IMPORTING
          input         TYPE any
        RETURNING
          VALUE(output) TYPE REF TO /gal/json_element
        RAISING
          /gal/cx_json_exception.


  PROTECTED SECTION.


  PRIVATE SECTION.

    METHODS:

      "! <p class="shorttext synchronized" lang="en">Parse JSON</p>
      "!
      "! @parameter input                  | <p class="shorttext synchronized" lang="en">String in JSON format</p>
      "! @parameter output                 | <p class="shorttext synchronized" lang="en">JSON Element</p>
      "! @parameter position               | <p class="shorttext synchronized" lang="en">Position</p>
      "! @raising   /gal/cx_json_exception | <p class="shorttext synchronized" lang="en">JSON Serializer Exception</p>
      parse_int
        IMPORTING
          input    TYPE csequence
        EXPORTING
          output   TYPE REF TO /gal/json_element
        CHANGING
          position TYPE i
        RAISING
          /gal/cx_json_exception,

      "! <p class="shorttext synchronized" lang="en">Get next token</p>
      "!
      "! @parameter input                  | <p class="shorttext synchronized" lang="en">String in JSON format</p>
      "! @parameter output                 | <p class="shorttext synchronized" lang="en">Token</p>
      "! @parameter position               | <p class="shorttext synchronized" lang="en">Position</p>
      "! @raising   /gal/cx_json_exception | <p class="shorttext synchronized" lang="en">JSON Serializer Exception</p>
      get_next_token
        IMPORTING
          input         TYPE csequence
        EXPORTING
          VALUE(output) TYPE csequence
        CHANGING
          position      TYPE i
        RAISING
          /gal/cx_json_exception,

      "! <p class="shorttext synchronized" lang="en">Render JSON</p>
      "!
      "! @parameter input                  | <p class="shorttext synchronized" lang="en">JSON Element</p>
      "! @parameter indentation_level      | <p class="shorttext synchronized" lang="en">Indentation level</p>
      "! @parameter no_indentation         | <p class="shorttext synchronized" lang="en">Do not indent (only in pretty print mode)</p>
      "! @parameter no_line_break          | <p class="shorttext synchronized" lang="en">Do not add line break (only in pretty print mode)</p>
      "! @parameter output                 | <p class="shorttext synchronized" lang="en">String in JSON format</p>
      "! @raising   /gal/cx_json_exception | <p class="shorttext synchronized" lang="en">JSON Serializer Exception</p>
      render_int
        IMPORTING
          input             TYPE REF TO /gal/json_element
          indentation_level TYPE i
          no_indentation    TYPE abap_bool DEFAULT abap_false
          no_line_break     TYPE abap_bool DEFAULT abap_false
        CHANGING
          output            TYPE string
        RAISING
          /gal/cx_json_exception,

      "! <p class="shorttext synchronized" lang="en">Transform name</p>
      "!
      "! @parameter transformation | <p class="shorttext synchronized" lang="en">Transformation</p>
      "! @parameter name           | <p class="shorttext synchronized" lang="en">Name to transform</p>
      transform_name
        IMPORTING
          transformation TYPE i
        CHANGING
          name           TYPE string.

ENDCLASS.



CLASS /gal/json_serializer IMPLEMENTATION.


  METHOD constructor.
    DATA:
      l_default_serhnd TYPE REF TO /gal/json_serhnd_default.

* Initialize JSON style settings
    me->indent           = indent.
    me->line_break       = line_break.
    me->pretty_print     = pretty_print.
    me->parse_transform  = parse_transform.
    me->render_transform = render_transform.

* Create JSON converter and initialize JSON style settings
    CREATE OBJECT converter.

    converter->string_delimiter = string_delimiter.

* Create default serialization handler
    CREATE OBJECT l_default_serhnd.

    INSERT l_default_serhnd INTO TABLE serialization_handlers.
  ENDMETHOD.


  METHOD deserialize.
    DATA:
      l_type         TYPE REF TO cl_abap_typedescr,
      l_abs_type     TYPE string,

      l_deserialized TYPE abap_bool.

    FIELD-SYMBOLS:
      <l_serialization_handler> LIKE LINE OF serialization_handlers.

* Get type description for data to be serialized
    l_type = cl_abap_typedescr=>describe_by_data( output ).

* Try to serialize supplied value using known serialization handlers
    LOOP AT serialization_handlers ASSIGNING <l_serialization_handler>.
      <l_serialization_handler>->deserialize( EXPORTING serializer   = me
                                                        type         = l_type
                                                        input        = input
                                              CHANGING  output       = output
                                                        deserialized = l_deserialized ).
      IF l_deserialized = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

* No known serialization handler was able to deserialize supplied value
    l_abs_type = l_type->absolute_name.

    RAISE EXCEPTION TYPE /gal/cx_json_exception
      EXPORTING
        textid = /gal/cx_json_exception=>no_handler_for_data_type
        var1   = l_abs_type.
  ENDMETHOD.


  METHOD parse.
    DATA:
      l_position TYPE i,
      l_dummy    TYPE string.

* Parse root element
    l_position = 0.

    parse_int( EXPORTING  input    = input
               IMPORTING  output   = output
               CHANGING   position = l_position ).

* Make sure that end of JSON document is reached
    get_next_token( EXPORTING  input    = input
                    IMPORTING  output   = l_dummy
                    CHANGING   position = l_position ).
    IF l_dummy IS NOT INITIAL.
      RAISE EXCEPTION TYPE /gal/cx_json_exception
        EXPORTING
          textid = /gal/cx_json_exception=>invalid_document_structure.
    ENDIF.
  ENDMETHOD.


  METHOD parse_int.
    DATA:
      l_token           TYPE string,
      l_next_token      TYPE string,
      l_length          TYPE i,

      l_element         TYPE REF TO /gal/json_element,

      l_name            TYPE string,
      l_separator       TYPE string,

      l_value_boolean   TYPE abap_bool,
      l_value_date      TYPE d,
      l_value_number    TYPE /gal/javascript_number,
      l_value_string    TYPE string,
      l_value_time      TYPE t,
      l_value_timestamp TYPE timestampl.

* Initialize result
    CLEAR output.

* Get next token
    get_next_token( EXPORTING  input    = input
                    IMPORTING  output   = l_token
                    CHANGING   position = position ).

    l_length = strlen( l_token ).

* Add next token when token is 'NEW' (new Date...)
    IF l_length = 3 AND l_token CS 'NEW'.
      get_next_token( EXPORTING  input    = input
                      IMPORTING  output   = l_next_token
                      CHANGING   position = position ).

      CONCATENATE l_token l_next_token INTO l_token SEPARATED BY space.

      l_length = strlen( l_token ).
    ENDIF.

* Parse object
    IF l_token = '{'.
      CREATE OBJECT output
        EXPORTING
          type = /gal/json_element=>type_object.

      l_length = strlen( input ).

      IF position = l_length.
        RAISE EXCEPTION TYPE /gal/cx_json_exception
          EXPORTING
            textid = /gal/cx_json_exception=>unexpected_token
            var1   = l_token.
      ENDIF.

      IF input+position(1) = '}'.
        position = position + 1.
      ELSE.
        DO.
          get_next_token( EXPORTING  input    = input
                          IMPORTING  output   = l_name
                          CHANGING   position = position ).

          IF l_name(1) NA `'"`.
            RAISE EXCEPTION TYPE /gal/cx_json_exception
              EXPORTING
                textid = /gal/cx_json_exception=>invalid_attribute_name
                var1   = l_name.
          ENDIF.

          l_name = converter->javascript_to_string( l_name ).

          transform_name( EXPORTING transformation = parse_transform
                          CHANGING  name           = l_name ).

          get_next_token( EXPORTING  input    = input
                          IMPORTING  output   = l_separator
                          CHANGING   position = position ).

          IF l_separator <> ':'.
            RAISE EXCEPTION TYPE /gal/cx_json_exception
              EXPORTING
                textid = /gal/cx_json_exception=>unexpected_token
                var1   = l_separator.
          ENDIF.

          parse_int( EXPORTING input    = input
                     IMPORTING output   = l_element
                     CHANGING  position = position ).

          output->add_child( name    = l_name
                             element = l_element ).

          get_next_token( EXPORTING  input    = input
                          IMPORTING  output   = l_separator
                          CHANGING   position = position ).

          IF l_separator = ','.
            CONTINUE.
          ELSEIF l_separator = '}'.
            EXIT.
          ELSE.
            RAISE EXCEPTION TYPE /gal/cx_json_exception
              EXPORTING
                textid = /gal/cx_json_exception=>unexpected_token
                var1   = l_separator.
          ENDIF.
        ENDDO.
      ENDIF.

* Parse array
    ELSEIF l_token = '['.
      CREATE OBJECT output
        EXPORTING
          type = /gal/json_element=>type_array.

      l_length = strlen( input ).

      IF position = l_length.
        RAISE EXCEPTION TYPE /gal/cx_json_exception
          EXPORTING
            textid = /gal/cx_json_exception=>unexpected_token
            var1   = l_token.
      ENDIF.

      IF input+position(1) = ']'.
        position = position + 1.
      ELSE.
        DO.
          parse_int( EXPORTING input    = input
                     IMPORTING output   = l_element
                     CHANGING  position = position ).

          output->add_child( element = l_element ).

          get_next_token( EXPORTING  input    = input
                          IMPORTING  output   = l_separator
                          CHANGING   position = position ).

          IF l_separator = ','.
            CONTINUE.
          ELSEIF l_separator = ']'.
            EXIT.
          ELSE.
            RAISE EXCEPTION TYPE /gal/cx_json_exception
              EXPORTING
                textid = /gal/cx_json_exception=>unexpected_token
                var1   = l_separator.
          ENDIF.
        ENDDO.
      ENDIF.

* Parse timestamp (string based formats will be converted when target field is of type TIMESTAMP or TIMESTAMPL)
    ELSEIF l_token CP 'DATE(+*)'
        OR l_token CP '/DATE(+*)/'
        OR l_token CP '\/DATE(+*)\/'
        OR l_token CP 'NEW DATE(+*)'.

      l_value_timestamp = converter->javascript_to_timestamp( l_token ).

      CREATE OBJECT output
        EXPORTING
          type  = /gal/json_element=>type_datetime
          value = l_value_timestamp.

* Parse string literals
    ELSEIF l_length > 1 AND l_token(1) CA `'"`.
      l_value_string = converter->javascript_to_string( l_token ).

      CREATE OBJECT output
        EXPORTING
          type  = /gal/json_element=>type_string
          value = l_value_string.

* Parse boolean literals
    ELSEIF l_token = /gal/javascript_constants=>true OR l_token = /gal/javascript_constants=>false.
      l_value_boolean = converter->javascript_to_boolean( l_token ).

      CREATE OBJECT output
        EXPORTING
          type  = /gal/json_element=>type_boolean
          value = l_value_boolean.

* Parse null values
    ELSEIF l_token = /gal/javascript_constants=>null.
      CREATE OBJECT output
        EXPORTING
          type = /gal/json_element=>type_null.

* Parse date
    ELSEIF l_token CP '++++-++-++'.
      l_value_date = converter->javascript_to_date( l_token ).

      CREATE OBJECT output
        EXPORTING
          type  = /gal/json_element=>type_time
          value = l_value_date.

* Parse time
    ELSEIF l_token CP '++:++:++'.
      l_value_time = converter->javascript_to_time( l_token ).

      CREATE OBJECT output
        EXPORTING
          type  = /gal/json_element=>type_time
          value = l_value_time.

* Parse numbers
    ELSEIF l_token(1) >= '0' AND l_token(1) <= '9' OR l_token(1) = '-'.
      TRY.
          l_value_number = converter->javascript_to_number( l_token ).

          CREATE OBJECT output
            EXPORTING
              type  = /gal/json_element=>type_number
              value = l_value_number.

        CATCH cx_sy_conversion_overflow. "Fallback: Create string value from token
          CREATE OBJECT output
            EXPORTING
              type  = /gal/json_element=>type_string
              value = l_token.

      ENDTRY.
    ELSE.
      RAISE EXCEPTION TYPE /gal/cx_json_exception
        EXPORTING
          textid = /gal/cx_json_exception=>value_not_supported
          var1   = l_token.
    ENDIF.
  ENDMETHOD.


  METHOD get_next_token.
    DATA:
      l_length    TYPE i,
      l_start_pos TYPE i,
      l_end_pos   TYPE i,
      l_offset    TYPE i,

      l_quote     TYPE c,

      l_var1      TYPE string.

* Skip whitespace
    l_length = strlen( input ).

    WHILE position < l_length AND input+position(1) CA /gal/string=>whitespace_chars.
      position = position + 1.
    ENDWHILE.

    IF position = l_length.
      CLEAR output.
      RETURN.
    ENDIF.

* Find next token
    l_start_pos = position.

    IF input+position(1) CA `'"`.
      l_quote = input+position(1).

      DO.
        position = position + 1.

        IF input+position CA l_quote.
          position = position + sy-fdpos.
        ELSE.
          RAISE EXCEPTION TYPE /gal/cx_json_exception
            EXPORTING
              textid = /gal/cx_json_exception=>missing_closing_quote
              var1   = l_var1
              var2   = input+l_start_pos.
        ENDIF.

        l_offset = position - 1.

        IF input+l_offset(1) <> `\`.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

    WHILE position < l_length AND input+position(1) NA `{}[],:`.
      position = position + 1.
    ENDWHILE.

* If a token was found return the token
    IF l_start_pos = position.
      output   = input+position(1).
      position = position + 1.
      RETURN.
    ENDIF.

* Return everything before the next token (except whitespace)
    l_end_pos = position - 1.

    WHILE input+l_end_pos(1) CA /gal/string=>whitespace_chars.
      l_end_pos = l_end_pos - 1.
    ENDWHILE.

    l_length = l_end_pos - l_start_pos + 1.
    output = input+l_start_pos(l_length).
  ENDMETHOD.


  METHOD render.
    render_int( EXPORTING input             = input
                          indentation_level = 0
                CHANGING  output            = output ).
  ENDMETHOD.


  METHOD render_int.
    DATA:
      l_indent_string     TYPE string,
      l_output_string     TYPE string,

      l_value_boolean     TYPE abap_bool,
      l_value_date        TYPE d,
      l_value_datetime    TYPE timestampl,
      l_value_number(16)  TYPE p DECIMALS 14,
      l_value_string      TYPE string,
      l_value_time        TYPE t,

      l_var1              TYPE string,
      l_indentation_level TYPE i.

    FIELD-SYMBOLS:
      <l_child> LIKE LINE OF input->children.

* Build string for indentation
    IF pretty_print = abap_true.
      DO indentation_level TIMES.
        IF sy-index = 1.
          l_indent_string = indent.
        ELSE.
          CONCATENATE l_indent_string indent INTO l_indent_string.
        ENDIF.
      ENDDO.

      IF no_indentation = abap_false.
        l_output_string = l_indent_string.
      ENDIF.
    ENDIF.

* Render current value to output string
    CASE input->type.

      WHEN /gal/json_element=>type_null.
        CONCATENATE l_output_string /gal/javascript_constants=>null INTO l_output_string.


      WHEN /gal/json_element=>type_boolean.
        input->get_value( IMPORTING value = l_value_boolean ).

        l_value_string = converter->boolean_to_javascript( l_value_boolean ).
        CONCATENATE l_output_string l_value_string INTO l_output_string.


      WHEN /gal/json_element=>type_date.
        input->get_value( IMPORTING value = l_value_date ).

        l_value_string = converter->date_to_javascript( l_value_date ).
        CONCATENATE l_output_string l_value_string INTO l_output_string.


      WHEN /gal/json_element=>type_datetime.
        input->get_value( IMPORTING value = l_value_datetime ).

        l_value_string = converter->timestamp_to_javascript( l_value_datetime ).
        CONCATENATE l_output_string l_value_string INTO l_output_string.


      WHEN /gal/json_element=>type_number.
        input->get_value( IMPORTING value = l_value_number ).

        l_value_string = converter->number_to_javascript( l_value_number ).
        CONCATENATE l_output_string l_value_string INTO l_output_string.


      WHEN /gal/json_element=>type_string.
        input->get_value( IMPORTING value = l_value_string ).

        l_value_string = converter->string_to_javascript( l_value_string ).
        CONCATENATE l_output_string l_value_string INTO l_output_string.


      WHEN /gal/json_element=>type_time.
        input->get_value( IMPORTING value = l_value_time ).

        l_value_string = converter->time_to_javascript( l_value_time ).
        CONCATENATE l_output_string l_value_string INTO l_output_string.


      WHEN /gal/json_element=>type_object.
        CONCATENATE l_output_string '{' INTO l_output_string.

        LOOP AT input->children ASSIGNING <l_child>.
          AT FIRST.
            IF pretty_print = abap_true.
              CONCATENATE l_output_string line_break INTO l_output_string.
            ENDIF.
          ENDAT.

          l_value_string = <l_child>-name.

          transform_name( EXPORTING transformation = render_transform
                          CHANGING  name           = l_value_string ).

          l_value_string = converter->string_to_javascript( l_value_string ).

          IF pretty_print = abap_true.
            CONCATENATE l_output_string l_indent_string indent l_value_string `: ` INTO l_output_string RESPECTING BLANKS.
          ELSE.
            CONCATENATE l_output_string l_value_string `:` INTO l_output_string RESPECTING BLANKS.
          ENDIF.

          l_indentation_level = indentation_level + 1.

          IF <l_child>-element->type = /gal/json_element=>type_array OR <l_child>-element->type = /gal/json_element=>type_object.
            render_int( EXPORTING input             = <l_child>-element
                                  indentation_level = l_indentation_level
                                  no_indentation    = abap_true
                                  no_line_break     = abap_true
                        CHANGING  output            = l_output_string ).
          ELSE.
            render_int( EXPORTING input             = <l_child>-element
                                  indentation_level = l_indentation_level
                                  no_indentation    = abap_true
                                  no_line_break     = abap_true
                        CHANGING  output            = l_output_string ).
          ENDIF.

          AT LAST. "No comma required after last element
            IF pretty_print = abap_true.
              CONCATENATE l_output_string line_break l_indent_string INTO l_output_string.
            ENDIF.

            EXIT.
          ENDAT.

          IF pretty_print = abap_true.
            CONCATENATE l_output_string ',' line_break INTO l_output_string.
          ELSE.
            CONCATENATE l_output_string ',' INTO l_output_string.
          ENDIF.
        ENDLOOP.

        CONCATENATE l_output_string '}' INTO l_output_string.


      WHEN /gal/json_element=>type_array.
        CONCATENATE l_output_string '[' INTO l_output_string.

        LOOP AT input->children ASSIGNING <l_child>.
          AT FIRST.
            IF pretty_print = abap_true.
              CONCATENATE l_output_string line_break INTO l_output_string.
            ENDIF.
          ENDAT.

          l_indentation_level = indentation_level + 1.

          IF <l_child>-element->type = /gal/json_element=>type_array OR <l_child>-element->type = /gal/json_element=>type_object.
            render_int( EXPORTING input             = <l_child>-element
                                  indentation_level = l_indentation_level
                                  no_line_break     = abap_true
                        CHANGING  output            = l_output_string ).
          ELSE.
            render_int( EXPORTING input             = <l_child>-element
                                  indentation_level = l_indentation_level
                                  no_line_break     = abap_true
                        CHANGING  output            = l_output_string ).
          ENDIF.

          AT LAST. "No comma required after last element
            IF pretty_print = abap_true.
              CONCATENATE l_output_string line_break l_indent_string INTO l_output_string.
            ENDIF.

            EXIT.
          ENDAT.

          IF pretty_print = abap_true.
            CONCATENATE l_output_string ',' line_break INTO l_output_string.
          ELSE.
            CONCATENATE l_output_string ',' INTO l_output_string.
          ENDIF.
        ENDLOOP.

        CONCATENATE l_output_string ']' INTO l_output_string.


      WHEN OTHERS.
        l_var1 = input->type.

        RAISE EXCEPTION TYPE /gal/cx_json_exception
          EXPORTING
            textid = /gal/cx_json_exception=>element_type_not_supported
            var1   = l_var1.

    ENDCASE.

    IF pretty_print = abap_true AND no_line_break = abap_false.
      CONCATENATE output l_output_string line_break INTO output.
    ELSE.
      CONCATENATE output l_output_string INTO output.
    ENDIF.
  ENDMETHOD.


  METHOD serialize.
    DATA:
      l_type     TYPE REF TO cl_abap_typedescr,
      l_abs_type TYPE string.

    FIELD-SYMBOLS:
      <l_serialization_handler> LIKE LINE OF serialization_handlers.

* Get type description for data to be serialized
    l_type = cl_abap_typedescr=>describe_by_data( input ).

* Try to serialize supplied value using known serialization handlers
    LOOP AT serialization_handlers ASSIGNING <l_serialization_handler>.
      output = <l_serialization_handler>->serialize( serializer = me
                                                     type       = l_type
                                                     input      = input ).
      IF output IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDLOOP.

* No known serialization handler was able to serialize supplied value
    l_abs_type = l_type->absolute_name.

    RAISE EXCEPTION TYPE /gal/cx_json_exception
      EXPORTING
        textid = /gal/cx_json_exception=>no_handler_for_data_type
        var1   = l_abs_type.
  ENDMETHOD.


  METHOD transform_name.
    CONSTANTS:
      lc_alpha_lower TYPE string VALUE `abcdefghijklmnopqrstuvwxyz`, "#EC NOTEXT
      lc_alpha_upper TYPE string VALUE `ABCDEFGHIJKLMNOPQRSTUVWXYZ`. "#EC NOTEXT

    DATA:
      l_length      TYPE i,
      l_position    TYPE i,
      l_offset      TYPE i,
      l_next_offset TYPE i,

      l_input       TYPE string,
      l_output      TYPE string,

      l_upper_char  TYPE c.

    CASE transformation.

      WHEN transform_to_lower.
        TRANSLATE name TO LOWER CASE.

      WHEN transform_to_upper.
        TRANSLATE name TO UPPER CASE.

      WHEN transform_abap_to_camel.
        l_input = name.
        l_length = strlen( l_input ).

        TRANSLATE l_input TO LOWER CASE.

        WHILE l_offset < l_length AND l_input+l_offset CA '_'.
          l_position    = sy-fdpos.
          l_next_offset = l_offset + l_position + 1.

          IF l_next_offset < l_length.
            IF l_input+l_next_offset(1) CA lc_alpha_lower.
              l_upper_char = l_input+l_next_offset(1).
              TRANSLATE l_upper_char TO UPPER CASE.
              CONCATENATE l_output l_input+l_offset(l_position) l_upper_char INTO l_output RESPECTING BLANKS.
            ELSE.
              l_next_offset = l_next_offset - 1.
              CONCATENATE l_output l_input+l_offset(l_position) l_input+l_next_offset(1) INTO l_output RESPECTING BLANKS.
            ENDIF.

            l_offset = l_next_offset + 1.
          ELSE.
            EXIT.
          ENDIF.
        ENDWHILE.

        IF l_offset < l_length.
          CONCATENATE l_output l_input+l_offset INTO l_output RESPECTING BLANKS.
        ENDIF.

        name = l_output.

      WHEN transform_camel_to_abap.
        l_input = name.
        l_length = strlen( l_input ).

        WHILE l_offset < l_length AND l_input+l_offset CA lc_alpha_upper.
          l_next_offset = l_offset + sy-fdpos.
          CONCATENATE l_output l_input+l_offset(sy-fdpos) '_' l_input+l_next_offset(1) INTO l_output RESPECTING BLANKS.
          l_offset = l_next_offset + 1.
        ENDWHILE.

        IF l_offset < l_length.
          CONCATENATE l_output l_input+l_offset INTO l_output RESPECTING BLANKS.
        ENDIF.

        TRANSLATE l_output TO UPPER CASE.

        name = l_output.

    ENDCASE.
  ENDMETHOD.

ENDCLASS.
