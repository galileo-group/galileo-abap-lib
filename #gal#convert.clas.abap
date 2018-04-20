class /GAL/CONVERT definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  class-methods LANGUAGE_KEY_SAP_TO_ISO
    importing
      !LANGUAGE_KEY_SAP type LANGU
    returning
      value(LANGUAGE_KEY_ISO) type LAISO
    raising
      /GAL/CX_CONVERSION_EXCEPTION .
  class-methods MOVE_CORRESPONDING
    importing
      !INPUT type ANY
    exporting
      !OUTPUT type ANY .
protected section.
private section.
ENDCLASS.



CLASS /GAL/CONVERT IMPLEMENTATION.


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
  DATA l_input_type         TYPE REF TO cl_abap_typedescr.
  DATA l_input_struct_type  TYPE REF TO cl_abap_structdescr.
  DATA l_output_type        TYPE REF TO cl_abap_typedescr.
  DATA l_output_struct_type TYPE REF TO cl_abap_structdescr.

  DATA l_wa_output_table TYPE REF TO data.

  FIELD-SYMBOLS <l_components>       TYPE LINE OF abap_compdescr_tab.
  FIELD-SYMBOLS <l_input_component>  TYPE any.
  FIELD-SYMBOLS <l_output_component> TYPE any.
  FIELD-SYMBOLS <l_input_table>      TYPE ANY TABLE.
  FIELD-SYMBOLS <l_output_table>     TYPE ANY TABLE.
  FIELD-SYMBOLS <l_wa_input_table>   TYPE any.
  FIELD-SYMBOLS <l_wa_output_table>  TYPE any.

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
