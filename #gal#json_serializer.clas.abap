class /GAL/JSON_SERIALIZER definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  methods CONSTRUCTOR
    importing
      !STRING_DELIMITER type C default '''' .
  methods DESERIALIZE
    importing
      !INPUT type STRING
    returning
      value(OUTPUT) type ref to /GAL/JSON_ELEMENT
    raising
      /GAL/CX_JSON_EXCEPTION .
  methods SERIALIZE
    importing
      !INPUT type ref to /GAL/JSON_ELEMENT
    returning
      value(OUTPUT) type STRING
    raising
      /GAL/CX_JSON_EXCEPTION .
protected section.
private section.

  data CONVERTER type ref to /GAL/JAVASCRIPT_CONVERTER .

  methods DESERIALIZE_INT
    importing
      !INPUT type CSEQUENCE
    exporting
      !OUTPUT type ref to /GAL/JSON_ELEMENT
    changing
      !POSITION type I
    raising
      /GAL/CX_JSON_EXCEPTION .
  methods DESERIALIZE_INT_GET_TOKEN
    importing
      !INPUT type CSEQUENCE
    exporting
      value(OUTPUT) type CSEQUENCE
    changing
      !POSITION type I
    raising
      /GAL/CX_JSON_EXCEPTION .
  methods SERIALIZE_INT
    importing
      !INPUT type ref to /GAL/JSON_ELEMENT
    changing
      !OUTPUT type STRING
    raising
      /GAL/CX_JSON_EXCEPTION .
ENDCLASS.



CLASS /GAL/JSON_SERIALIZER IMPLEMENTATION.


METHOD constructor.
  CREATE OBJECT converter.

  converter->string_delimiter = string_delimiter.
ENDMETHOD.


METHOD deserialize.
  DATA l_position TYPE i.
  DATA l_dummy    TYPE string.

  l_position = 0.

  deserialize_int( EXPORTING  input    = input
                   IMPORTING  output   = output
                   CHANGING   position = l_position ).

  deserialize_int_get_token( EXPORTING  input    = input
                             IMPORTING  output   = l_dummy
                             CHANGING   position = l_position ).
  IF l_dummy IS NOT INITIAL.
    RAISE EXCEPTION TYPE /gal/cx_json_exception
      EXPORTING
        textid = /gal/cx_json_exception=>invalid_document_structure.
  ENDIF.
ENDMETHOD.


METHOD deserialize_int.
  DATA l_token         TYPE string.
  DATA l_length        TYPE i.

  DATA l_key_element   TYPE REF TO /gal/json_element.
  DATA l_element       TYPE REF TO /gal/json_element.

  DATA l_key           TYPE string.
  DATA l_separator     TYPE string.

  DATA l_value_string  TYPE string.
  DATA l_value_boolean TYPE abap_bool.
  DATA l_value_float   TYPE f.
  DATA l_value_integer TYPE i.

* Initialize result
  CLEAR output.

* Get next token
  deserialize_int_get_token( EXPORTING  input    = input
                             IMPORTING  output   = l_token
                             CHANGING   position = position ).

  l_length = strlen( l_token ).

* Deserialize object
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
        deserialize_int( EXPORTING input    = input
                         IMPORTING output   = l_key_element
                         CHANGING  position = position ).

        l_key_element->get_value( IMPORTING value = l_key ).

        IF l_key_element->type <> /gal/json_element=>type_string.
          RAISE EXCEPTION TYPE /gal/cx_json_exception
            EXPORTING
              textid = /gal/cx_json_exception=>invalid_attribute_name
              var1   = l_key.
        ENDIF.

        deserialize_int_get_token( EXPORTING  input    = input
                                   IMPORTING  output   = l_separator
                                   CHANGING   position = position ).

        IF l_separator <> ':'.
          RAISE EXCEPTION TYPE /gal/cx_json_exception
            EXPORTING
              textid = /gal/cx_json_exception=>unexpected_token
              var1   = l_separator.
        ENDIF.

        deserialize_int( EXPORTING input    = input
                         IMPORTING output   = l_element
                         CHANGING  position = position ).

        output->add_child( key     = l_key
                           element = l_element ).

        deserialize_int_get_token( EXPORTING  input    = input
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

* Deserialize array
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
        deserialize_int( EXPORTING input    = input
                         IMPORTING output   = l_element
                         CHANGING  position = position ).

        output->add_child( element = l_element ).

        deserialize_int_get_token( EXPORTING  input    = input
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


* Deserialize string literals
  ELSEIF l_length > 0 AND l_token(1) CA `'"`.
    l_value_string = converter->javascript_to_string( l_token ).

    CREATE OBJECT output
      EXPORTING
        type  = /gal/json_element=>type_string
        value = l_value_string.

* Deserialize boolean literals
  ELSEIF l_token = /gal/javascript_constants=>true OR  l_token = /gal/javascript_constants=>false.
    l_value_boolean = converter->javascript_to_boolean( l_token ).

    CREATE OBJECT output
      EXPORTING
        type  = /gal/json_element=>type_boolean
        value = l_value_boolean.

* Deserialize null values
  ELSEIF l_token = /gal/javascript_constants=>null.
    CREATE OBJECT output
      EXPORTING
        type = /gal/json_element=>type_null.

* Deserialize numbers
  ELSEIF l_token(1) >= '0' AND l_token(1) <= '9' OR l_token(1) = '-'.
    TRY.
        IF l_token CA '.'.
          l_value_float = converter->javascript_to_float( l_token ).

          CREATE OBJECT output
            EXPORTING
              type  = /gal/json_element=>type_float
              value = l_value_float.
        ELSE.
          l_value_integer = converter->javascript_to_integer( l_token ).

          CREATE OBJECT output
            EXPORTING
              type  = /gal/json_element=>type_integer
              value = l_value_integer.
        ENDIF.

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


METHOD DESERIALIZE_INT_GET_TOKEN.
  DATA l_length    TYPE i.
  DATA l_start_pos TYPE i.
  DATA l_end_pos   TYPE i.
  DATA l_offset    TYPE i.

  DATA l_quote     TYPE c.

  DATA l_var1      TYPE string.

* Skip whitespaces
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

  WHILE position < l_length and input+position(1) NA `{}[],:`.
    position = position + 1.
  ENDWHILE.

* If a token was found return the token
  IF l_start_pos = position.
    output   = input+position(1).
    position = position + 1.
    RETURN.
  ENDIF.

* Return everything before the next token (except whitespaces)
  l_end_pos = position - 1.

  WHILE input+l_end_pos(1) CA /gal/string=>whitespace_chars.
    l_end_pos = l_end_pos - 1.
  ENDWHILE.

  l_length = l_end_pos - l_start_pos + 1.
  output = input+l_start_pos(l_length).
ENDMETHOD.


METHOD serialize.
  serialize_int( EXPORTING input  = input
                 CHANGING  output = output ).
ENDMETHOD.


METHOD serialize_int.
  DATA l_value_boolean TYPE abap_bool.
  DATA l_value_float   TYPE f.
  DATA l_value_integer TYPE i.
  DATA l_value_string  TYPE string.
  DATA l_var1          TYPE string.

  FIELD-SYMBOLS <l_child> LIKE LINE OF input->children.

  CASE input->type.

    WHEN /gal/json_element=>type_null.
      CONCATENATE output /gal/javascript_constants=>null INTO output.

    WHEN /gal/json_element=>type_boolean.
      input->get_value( IMPORTING value = l_value_boolean ).

      l_value_string = converter->boolean_to_javascript( l_value_boolean ).
      CONCATENATE output l_value_string INTO output.

    WHEN /gal/json_element=>type_float.
      input->get_value( IMPORTING value = l_value_float ).

      l_value_string = converter->float_to_javascript( l_value_float ).
      CONCATENATE output l_value_string INTO output.

    WHEN /gal/json_element=>type_integer.
      input->get_value( IMPORTING value = l_value_integer ).

      l_value_string = converter->integer_to_javascript( l_value_integer ).
      CONCATENATE output l_value_string INTO output.

    WHEN /gal/json_element=>type_string.
      input->get_value( IMPORTING value = l_value_string ).

      l_value_string = converter->string_to_javascript( l_value_string ).
      CONCATENATE output l_value_string INTO output.

    WHEN /gal/json_element=>type_object.
      CONCATENATE output '{' INTO output.

      LOOP AT input->children ASSIGNING <l_child>.
        IF sy-tabix > 1.
          CONCATENATE output ',' INTO output.
        ENDIF.

        l_value_string = converter->string_to_javascript( <l_child>-key ).
        CONCATENATE output l_value_string ':' INTO output.

        serialize_int( EXPORTING input  = <l_child>-element
                       CHANGING  output = output ).
      ENDLOOP.

      CONCATENATE output '}' INTO output.

    WHEN /gal/json_element=>type_array.
      CONCATENATE output '[' INTO output.

      LOOP AT input->children ASSIGNING <l_child>.
        IF sy-tabix > 1.
          CONCATENATE output ',' INTO output.
        ENDIF.

        serialize_int( EXPORTING input  = <l_child>-element
                       CHANGING  output = output ).
      ENDLOOP.

      CONCATENATE output ']' INTO output.

    WHEN OTHERS.
      l_var1 = input->type.

      RAISE EXCEPTION TYPE /gal/cx_json_exception
        EXPORTING
          textid = /gal/cx_json_exception=>element_type_not_supported
          var1   = l_var1.

  ENDCASE.
ENDMETHOD.
ENDCLASS.
