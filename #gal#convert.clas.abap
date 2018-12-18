"! <p class="shorttext synchronized" lang="en">Value Conversion Helper</p>
CLASS /gal/convert DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    "! <p class="shorttext synchronized" lang="en">Convert integer value to hexadecimal string</p>
    "!
    "! @parameter input                        | <p class="shorttext synchronized" lang="en">Input value</p>
    "! @parameter input                        | <p class="shorttext synchronized" lang="en">Input value</p>
    "! @parameter output                       | <p class="shorttext synchronized" lang="en">Output value</p>
    "! @raising   /gal/cx_conversion_exception | <p class="shorttext synchronized" lang="en">Conversion Exception</p>
    CLASS-METHODS integer_to_hex_string
      IMPORTING
        !input        TYPE i
        !length       TYPE i OPTIONAL
      RETURNING
        VALUE(output) TYPE string
      RAISING
        /gal/cx_conversion_exception .
    "! <p class="shorttext synchronized" lang="en">Convert hexadecimal string to integer value</p>
    "!
    "! @parameter input                        | <p class="shorttext synchronized" lang="en">Input value</p>
    "! @parameter output                       | <p class="shorttext synchronized" lang="en">Output value</p>
    "! @raising   /gal/cx_conversion_exception | <p class="shorttext synchronized" lang="en">Conversion Exception</p>
    CLASS-METHODS hex_string_to_integer
      IMPORTING
        !input        TYPE csequence
      RETURNING
        VALUE(output) TYPE i
      RAISING
        /gal/cx_conversion_exception .
    "! <p class="shorttext synchronized" lang="en">Convert SAP language key to ISO language key</p>
    "!
    "! @parameter language_key_sap             | <p class="shorttext synchronized" lang="en">Language key (SAP)</p>
    "! @parameter language_key_iso             | <p class="shorttext synchronized" lang="en">Language key (ISO)</p>
    "! @raising   /gal/cx_conversion_exception | <p class="shorttext synchronized" lang="en">Conversion Exception</p>
    CLASS-METHODS language_key_sap_to_iso
      IMPORTING
        !language_key_sap       TYPE langu
      RETURNING
        VALUE(language_key_iso) TYPE laiso
      RAISING
        /gal/cx_conversion_exception .
    "! <p class="shorttext synchronized" lang="en">Move to corresponding fields (by field names)</p>
    "!
    "! @parameter input  | <p class="shorttext synchronized" lang="en">Input value</p>
    "! @parameter output | <p class="shorttext synchronized" lang="en">Output value</p>
    CLASS-METHODS move_corresponding
      IMPORTING
        !input  TYPE any
      EXPORTING
        !output TYPE any .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /GAL/CONVERT IMPLEMENTATION.


  METHOD hex_string_to_integer.
    CONSTANTS lc_digits(16) TYPE c VALUE '0123456789ABCDEF'.

    DATA: l_hex    TYPE string,
          l_length TYPE i,
          l_offset TYPE i.

    l_hex = input.

    TRANSLATE l_hex TO UPPER CASE.

    l_length = strlen( l_hex ).

    WHILE l_offset < l_length AND lc_digits CA l_hex+l_offset(1).
      output = 16 * output + sy-fdpos.
      l_offset = l_offset + 1.
    ENDWHILE.

    IF l_offset < l_length.
      RAISE EXCEPTION TYPE /gal/cx_conversion_exception
        EXPORTING
          textid = /gal/cx_conversion_exception=>input_no_hex_string
          var1   = input.
    ENDIF.
  ENDMETHOD.


  METHOD integer_to_hex_string.
    CONSTANTS lc_digits(16) TYPE c VALUE '0123456789ABCDEF'.

    DATA: l_length    TYPE i,
          l_pos       TYPE i,
          l_integer   TYPE i,
          l_exception TYPE REF TO /gal/cx_conversion_exception.

    l_integer = input.

    IF length IS INITIAL.
      IF l_integer < 0.
        l_exception ?= /gal/cx_exception=>create_with_generic_types( exception_class = `/GAL/CX_CONVERSION_EXCEPTION`
                                                                     textid          = /gal/cx_conversion_exception=>neg_int_to_hex_string
                                                                     var1            = input ).
        RAISE EXCEPTION l_exception.
      ELSEIF l_integer < 16.
        l_length = 1.
      ELSE.
        l_length = floor( log( l_integer ) / log( 16 ) ) + 1.
      ENDIF.
    ELSE.
      l_length = length.
    ENDIF.

    DO l_length TIMES.
      l_pos     = l_integer MOD 16.
      l_integer = l_integer DIV 16.
      CONCATENATE lc_digits+l_pos(1) output INTO output.
    ENDDO.

    IF l_integer > 0.
      l_exception ?= /gal/cx_exception=>create_with_generic_types( exception_class = `/GAL/CX_CONVERSION_EXCEPTION`
                                                                   textid          = /gal/cx_conversion_exception=>input_too_high_for_hex_string
                                                                   var1            = input
                                                                   var2            = length ).
      RAISE EXCEPTION l_exception.
    ENDIF.
  ENDMETHOD.


  METHOD language_key_sap_to_iso.
    DATA l_sap_language_key TYPE string.

    SELECT SINGLE laiso FROM t002
                        INTO language_key_iso
                       WHERE spras = language_key_sap.
    IF sy-subrc <> 0.
      l_sap_language_key = language_key_sap.

      RAISE EXCEPTION TYPE /gal/cx_conversion_exception
        EXPORTING
          textid = /gal/cx_conversion_exception=>unknown_language_key
          var1   = l_sap_language_key.
    ENDIF.
  ENDMETHOD.


  METHOD move_corresponding.
    DATA: l_input_type         TYPE REF TO cl_abap_typedescr,
          l_input_struct_type  TYPE REF TO cl_abap_structdescr,
          l_output_type        TYPE REF TO cl_abap_typedescr,
          l_output_struct_type TYPE REF TO cl_abap_structdescr,
          l_wa_output_table    TYPE REF TO data.

    FIELD-SYMBOLS: <l_components>       TYPE LINE OF abap_compdescr_tab,
                   <l_input_component>  TYPE any,
                   <l_output_component> TYPE any,
                   <l_input_table>      TYPE ANY TABLE,
                   <l_output_table>     TYPE ANY TABLE,
                   <l_wa_input_table>   TYPE any,
                   <l_wa_output_table>  TYPE any.

* Initialize result
    CLEAR output.

* Get type description for source and target
    l_input_type  = cl_abap_typedescr=>describe_by_data( input ).
    l_output_type = cl_abap_typedescr=>describe_by_data( output ).

* Just copy input to output if types match
    IF l_input_type->absolute_name = l_output_type->absolute_name.
      output = input.
      RETURN.
    ENDIF.

* Move structure to structure (match by name)
    IF ( l_input_type->type_kind = cl_abap_typedescr=>typekind_struct1 OR
         l_input_type->type_kind = cl_abap_typedescr=>typekind_struct2 ) AND
       ( l_output_type->type_kind = cl_abap_typedescr=>typekind_struct1 OR
         l_output_type->type_kind = cl_abap_typedescr=>typekind_struct2 ).

      l_input_struct_type ?= l_input_type.
      l_output_struct_type ?= l_output_type.

      LOOP AT l_input_struct_type->components ASSIGNING <l_components>.
        READ TABLE l_output_struct_type->components
              WITH KEY name = <l_components>-name
                   TRANSPORTING NO FIELDS.
        CHECK sy-subrc = 0.

        ASSIGN COMPONENT <l_components>-name OF STRUCTURE input TO <l_input_component>.
        ASSIGN COMPONENT <l_components>-name OF STRUCTURE output TO <l_output_component>.

        move_corresponding( EXPORTING input  = <l_input_component>
                            IMPORTING output = <l_output_component> ).
      ENDLOOP.

* Move structure to table (insert single row)
    ELSEIF ( l_input_type->type_kind = cl_abap_typedescr=>typekind_struct1 OR
             l_input_type->type_kind = cl_abap_typedescr=>typekind_struct2 ) AND
           l_output_type->type_kind = cl_abap_typedescr=>typekind_table.

      ASSIGN output TO <l_output_table>.

      CREATE DATA l_wa_output_table LIKE LINE OF <l_output_table>.
      ASSIGN l_wa_output_table->* TO <l_wa_output_table>.

      move_corresponding( EXPORTING input  = input
                          IMPORTING output = <l_wa_output_table> ).

      INSERT <l_wa_output_table> INTO TABLE <l_output_table>.

* Move table to structure (first table entry only)
    ELSEIF l_input_type->type_kind = cl_abap_typedescr=>typekind_table AND
           ( l_output_type->type_kind = cl_abap_typedescr=>typekind_struct1 OR
             l_output_type->type_kind = cl_abap_typedescr=>typekind_struct2 ).

      ASSIGN input TO <l_input_table>.

      LOOP AT <l_input_table> ASSIGNING <l_wa_input_table>.
        move_corresponding( EXPORTING input  = <l_wa_input_table>
                            IMPORTING output = output ).
        EXIT.
      ENDLOOP.

* Move table to table (line by line)
    ELSEIF l_input_type->type_kind = cl_abap_typedescr=>typekind_table AND
           l_output_type->type_kind = cl_abap_typedescr=>typekind_table.

      ASSIGN input TO <l_input_table>.
      ASSIGN output TO <l_output_table>.

      CREATE DATA l_wa_output_table LIKE LINE OF <l_output_table>.
      ASSIGN l_wa_output_table->* TO <l_wa_output_table>.

      LOOP AT <l_input_table> ASSIGNING <l_wa_input_table>.
        move_corresponding( EXPORTING input  = <l_wa_input_table>
                            IMPORTING output = <l_wa_output_table> ).

        INSERT <l_wa_output_table> INTO TABLE <l_output_table>.
      ENDLOOP.

* Move object reference to object reference (casting)
    ELSEIF l_input_type->type_kind = cl_abap_typedescr=>typekind_oref AND
           l_output_type->type_kind = cl_abap_typedescr=>typekind_oref.

      output ?= input.

* Use built-in conversion for anything else
    ELSE.
      output = input.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
