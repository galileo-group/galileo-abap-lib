CLASS /gal/json_serhnd_default DEFINITION
  PUBLIC
  INHERITING FROM /gal/json_serhnd_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPE-POOLS:
      abap.

    METHODS:
      deserialize REDEFINITION,
      serialize REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS /gal/json_serhnd_default IMPLEMENTATION.

  METHOD deserialize.
    DATA:
      l_table_descr    TYPE REF TO cl_abap_tabledescr,
      l_type_descr     TYPE REF TO cl_abap_typedescr,

      l_value_boolean  TYPE abap_bool,

      l_workarea       TYPE REF TO data,

      l_value_number   TYPE /gal/javascript_number,
      l_value_string   TYPE string,

      l_value_element  TYPE REF TO /gal/json_element,
      l_value          TYPE REF TO data,

      l_type_element   TYPE REF TO /gal/json_element,
      l_type           TYPE string,

      l_javascript     TYPE string,

      l_exception      TYPE REF TO cx_root,
      l_json_exception TYPE REF TO /gal/cx_json_exception.

    FIELD-SYMBOLS:
      <l_child>    TYPE /gal/json_element_child,
      <l_table>    TYPE ANY TABLE,
      <l_workarea> TYPE any,
      <l_value>    TYPE any.

    TRY.
        CASE input->type.

          WHEN /gal/json_element=>type_boolean.
            IF type->absolute_name = '\TYPE-POOL=ABAP\TYPE=ABAP_BOOL' OR
               type->absolute_name = '\TYPE=/GAL/ABAP_BOOL'           OR
               type->absolute_name = '\TYPE=FLAG'.

              input->get_value( IMPORTING value = output ).
            ELSE.
              input->get_value( IMPORTING value = l_value_boolean ).
              output = serializer->converter->boolean_to_javascript( l_value_boolean ).
            ENDIF.

            deserialized = abap_true.

          WHEN /gal/json_element=>type_date.
            input->get_value( IMPORTING value = output ).
            deserialized = abap_true.

          WHEN /gal/json_element=>type_datetime.

          WHEN /gal/json_element=>type_null.
            CLEAR output.
            deserialized = abap_true.

          WHEN /gal/json_element=>type_number.
            IF type->type_kind = cl_abap_typedescr=>typekind_date OR
               type->type_kind = cl_abap_typedescr=>typekind_time OR
               type->absolute_name = '\TYPE=TIMESTAMP'            OR
               type->absolute_name = '\TYPE=TIMESTAMPL'.

              input->get_value( IMPORTING value = l_value_number ).
              l_javascript = serializer->converter->number_to_javascript( l_value_number ).
              CONCATENATE `Date(` l_javascript `)` INTO l_javascript. "#EC NOTEXT
              output = serializer->converter->javascript_to_timestamp( l_javascript ).
              deserialized = abap_true.
            ELSE.
              input->get_value( IMPORTING value = output ).
              deserialized = abap_true.
            ENDIF.

          WHEN /gal/json_element=>type_string.
            IF type->type_kind = cl_abap_typedescr=>typekind_date.
              input->get_value( IMPORTING value = l_value_string ).
              output = serializer->converter->javascript_to_date( l_value_string ).
              deserialized = abap_true.
            ELSEIF type->type_kind = cl_abap_typedescr=>typekind_time.
              input->get_value( IMPORTING value = l_value_string ).
              output = serializer->converter->javascript_to_time( l_value_string ).
              deserialized = abap_true.
            ELSEIF type->absolute_name = '\TYPE=TIMESTAMP' OR type->absolute_name = '\TYPE=TIMESTAMPL'.
              input->get_value( IMPORTING value = l_value_string ).
              output = serializer->converter->javascript_to_timestamp( l_value_string ).
              deserialized = abap_true.
            ELSE.
              input->get_value( IMPORTING value = output ).
              deserialized = abap_true.
            ENDIF.

          WHEN /gal/json_element=>type_time.
            input->get_value( IMPORTING value = output ).
            deserialized = abap_true.

          WHEN /gal/json_element=>type_array.
            IF type->type_kind = cl_abap_typedescr=>typekind_table.
              l_table_descr ?= type.
              l_type_descr   = l_table_descr->get_table_line_type( ).

              CLEAR output.

              CREATE DATA l_workarea TYPE (l_type_descr->absolute_name).
              ASSIGN l_workarea->* TO <l_workarea>.

              ASSIGN output TO <l_table>.

              LOOP AT input->children ASSIGNING <l_child>.
                CLEAR <l_workarea>.

                serializer->deserialize( EXPORTING input  = <l_child>-element
                                         CHANGING  output = <l_workarea> ).

                INSERT <l_workarea> INTO TABLE <l_table>.
              ENDLOOP.

              deserialized = abap_true.
            ENDIF.

          WHEN /gal/json_element=>type_object.
            IF type->type_kind = cl_abap_typedescr=>typekind_struct1 OR type->type_kind = cl_abap_typedescr=>typekind_struct2.
              CLEAR output.

              LOOP AT input->children ASSIGNING <l_child>.
                ASSIGN COMPONENT <l_child>-name OF STRUCTURE output TO <l_value>.
                IF sy-subrc = 0.
                  serializer->deserialize( EXPORTING input  = <l_child>-element
                                           CHANGING  output = <l_value> ).
                ELSE.
                  l_type = type->absolute_name.

                  RAISE EXCEPTION TYPE /gal/cx_json_exception
                    EXPORTING
                      textid = /gal/cx_json_exception=>unknown_structure_field
                      var1   = <l_child>-name
                      var2   = l_type.
                ENDIF.
              ENDLOOP.

              deserialized = abap_true.
            ELSEIF type->type_kind = cl_abap_typedescr=>typekind_dref.
              l_type_element = input->get_child( name = `TYPE` ).
              l_type_element->get_value( IMPORTING value = l_type ).

              l_value_element = input->get_child( name = `VALUE` ).
              CREATE DATA l_value TYPE (l_type).
              ASSIGN l_value->* TO <l_value>.

              serializer->deserialize( EXPORTING input  = l_value_element
                                       CHANGING  output = <l_value> ).

              output = l_value.

              deserialized = abap_true.
            ENDIF.

        ENDCASE.

* Handle conversion errors
      CATCH cx_sy_arithmetic_overflow
            cx_sy_conversion_overflow
            cx_sy_move_cast_error INTO l_exception.

        l_json_exception ?= /gal/cx_exception=>create_from_exception( exception_class = `/GAL/CX_JASON_EXCEPTION`
                                                                      previous        = l_exception ).
        RAISE EXCEPTION l_json_exception.

    ENDTRY.
  ENDMETHOD.


  METHOD serialize.
    DATA:
      l_value_descr  TYPE REF TO cl_abap_typedescr,
      l_struct_descr TYPE REF TO cl_abap_structdescr,
      l_element      TYPE REF TO /gal/json_element.

    FIELD-SYMBOLS:
      <l_component_descr> TYPE abap_compdescr,
      <l_table>           TYPE ANY TABLE,
      <l_value>           TYPE any.

* Type dependent serialization
    CASE type->type_kind.

      WHEN cl_abap_typedescr=>typekind_char
        OR cl_abap_typedescr=>typekind_date
        OR cl_abap_typedescr=>typekind_float
        OR cl_abap_typedescr=>typekind_num
        OR cl_abap_typedescr=>typekind_hex
        OR cl_abap_typedescr=>typekind_int
        OR cl_abap_typedescr=>typekind_int1
        OR cl_abap_typedescr=>typekind_int2
        OR cl_abap_typedescr=>typekind_packed
        OR cl_abap_typedescr=>typekind_string
        OR cl_abap_typedescr=>typekind_xstring
        OR cl_abap_typedescr=>typekind_time.

        CREATE OBJECT output
          EXPORTING
            value = input.


      WHEN cl_abap_typedescr=>typekind_struct1
        OR cl_abap_typedescr=>typekind_struct2.

        CREATE OBJECT output
          EXPORTING
            type = /gal/json_element=>type_object.

        l_struct_descr ?= type.

        LOOP AT l_struct_descr->components ASSIGNING <l_component_descr>.
          ASSIGN COMPONENT <l_component_descr>-name OF STRUCTURE input TO <l_value>.

          l_element = serializer->serialize( <l_value> ).

          output->add_child( name    = <l_component_descr>-name
                             element = l_element ).
        ENDLOOP.


      WHEN cl_abap_typedescr=>typekind_table.

        CREATE OBJECT output
          EXPORTING
            type = /gal/json_element=>type_array.

        ASSIGN input TO <l_table>.

        LOOP AT <l_table> ASSIGNING <l_value>.
          l_element = serializer->serialize( <l_value> ).

          output->add_child( element = l_element ).
        ENDLOOP.


      WHEN cl_abap_typedescr=>typekind_dref.

        ASSIGN input        TO <l_value>.
        ASSIGN <l_value>->* TO <l_value>.

        l_value_descr = cl_abap_typedescr=>describe_by_data( <l_value> ).

        CREATE OBJECT output
          EXPORTING
            type = /gal/json_element=>type_object.

        l_element = serializer->serialize( <l_value> ).

        output->add_child( name  = `TYPE`
                           type  = /gal/json_element=>type_string
                           value = l_value_descr->absolute_name ).

        output->add_child( name    = `VALUE`
                           element = l_element ).

    ENDCASE.
  ENDMETHOD.

ENDCLASS.
