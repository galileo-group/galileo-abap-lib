class /GAL/CFW_CONVERTER definition
  public
  final
  create public .

public section.

  class-methods CLASS_CONSTRUCTOR .
  class-methods COMPRESS_STRUCTURE
    importing
      !INPUT type ANY
      !TYPE type ref to CL_ABAP_STRUCTDESCR
    exporting
      !OUTPUT type STRING
    raising
      /GAL/CX_CFW_EXCEPTION .
  class-methods COMPRESS_TABLE
    importing
      !INPUT type ANY TABLE
      !TYPE type ref to CL_ABAP_TABLEDESCR
    exporting
      !OUTPUT type /GAL/STRINGTABLE
    raising
      /GAL/CX_CFW_EXCEPTION .
  class-methods COMPRESS_VALUE
    importing
      !INPUT type ANY
      !TYPE type ref to CL_ABAP_TYPEDESCR
    exporting
      !OUTPUT type STRING
    raising
      /GAL/CX_CFW_EXCEPTION .
  class-methods DECOMPRESS_STRUCTURE
    importing
      !INPUT type STRING
      !TYPE type ref to CL_ABAP_STRUCTDESCR
    exporting
      !OUTPUT type ANY
    raising
      /GAL/CX_CFW_EXCEPTION .
  class-methods DECOMPRESS_TABLE
    importing
      !INPUT type /GAL/STRINGTABLE
      !TYPE type ref to CL_ABAP_TABLEDESCR
    exporting
      !OUTPUT type ANY TABLE
    raising
      /GAL/CX_CFW_EXCEPTION .
  class-methods DECOMPRESS_VALUE
    importing
      !INPUT type STRING
      !TYPE type ref to CL_ABAP_TYPEDESCR
    exporting
      !OUTPUT type ANY
    raising
      /GAL/CX_CFW_EXCEPTION .
  class-methods TEXT_TO_XSTRING
    importing
      !INPUT type CSEQUENCE
    exporting
      !OUTPUT type XSTRING .
  class-methods XSTRING_TO_TEXT
    importing
      !INPUT type XSTRING
    exporting
      !OUTPUT type CSEQUENCE .
  PROTECTED SECTION.
private section.

  constants DELIMITER type C value '"'. "#EC NOTEXT
  constants ESCAPED_DELIMITER type STRING value `""`. "#EC NOTEXT
  constants SEPARATOR type C value ';'. "#EC NOTEXT
  class-data TEXT_DECODER type ref to CL_ABAP_CONV_IN_CE .
  class-data TEXT_ENCODER type ref to CL_ABAP_CONV_OUT_CE .
ENDCLASS.



CLASS /GAL/CFW_CONVERTER IMPLEMENTATION.


  METHOD class_constructor.
    text_encoder = cl_abap_conv_out_ce=>create( encoding = 'UTF-8'
                                                endian   = 'L' ).

    text_decoder = cl_abap_conv_in_ce=>create( encoding    = 'UTF-8'
                                               endian      = 'L'
                                               ignore_cerr = abap_true ).
  ENDMETHOD.


  METHOD compress_structure.
    DATA l_component_descrs TYPE abap_component_tab.
    DATA l_value            TYPE string.

    DATA l_offset           TYPE i.

    FIELD-SYMBOLS <l_component_descr> LIKE LINE OF l_component_descrs.
    FIELD-SYMBOLS <l_component_value> TYPE any.

    CLEAR output.

    l_component_descrs = type->get_components( ).

    LOOP AT l_component_descrs ASSIGNING <l_component_descr>.
      ASSIGN COMPONENT <l_component_descr>-name OF STRUCTURE input TO <l_component_value>.
      CHECK sy-subrc = 0. " This is always true!

      compress_value( EXPORTING input  = <l_component_value>
                                type   = <l_component_descr>-type
                      IMPORTING output = l_value ).

      IF l_value IS NOT INITIAL.
        l_offset = strlen( l_value ) - 1.

        IF l_value CA separator OR l_value(1) = delimiter AND l_value+l_offset(1) = delimiter.
          REPLACE ALL OCCURRENCES OF delimiter IN l_value WITH escaped_delimiter.
          CONCATENATE delimiter l_value delimiter INTO l_value.
        ENDIF.
      ENDIF.

      IF output IS INITIAL.
        output = l_value.
      ELSE.
        CONCATENATE output l_value INTO output SEPARATED BY separator RESPECTING BLANKS.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD compress_table.
    DATA l_line_type   TYPE REF TO cl_abap_typedescr.
    DATA l_struct_type TYPE REF TO cl_abap_structdescr.

    DATA l_target      TYPE string.

    DATA l_message     TYPE string.

    FIELD-SYMBOLS <l_source> TYPE any.

    CLEAR output.

    l_line_type = type->get_table_line_type( ).

    CASE l_line_type->type_kind.

      WHEN cl_abap_typedescr=>typekind_struct1.
        l_struct_type ?= l_line_type.

        LOOP AT input ASSIGNING <l_source>.
          compress_structure( EXPORTING type   = l_struct_type
                                        input  = <l_source>
                              IMPORTING output = l_target ).

          INSERT l_target INTO TABLE output.
        ENDLOOP.

      WHEN cl_abap_typedescr=>typekind_string.
        LOOP AT input ASSIGNING <l_source>.
          compress_value( EXPORTING type   = l_line_type
                                    input  = <l_source>
                          IMPORTING output = l_target ).

          INSERT l_target INTO TABLE output.
        ENDLOOP.

      WHEN OTHERS.
        l_message = /gal/string=>replace_variables( input = TEXT-e00
                                                    var01 = l_line_type->absolute_name ).

        RAISE EXCEPTION TYPE /gal/cx_cfw_exception
          EXPORTING
            textid = /gal/cx_cfw_exception=>compress_error
            var1   = l_message.

    ENDCASE.
  ENDMETHOD.


  METHOD compress_value.
    output = input.
  ENDMETHOD.


  METHOD decompress_structure.
    DATA l_component_descrs TYPE abap_component_tab.
    DATA l_value            TYPE string.

    DATA l_offset           TYPE i.
    DATA l_max_length       TYPE i.

    DATA l_val_offset       TYPE i.
    DATA l_val_length       TYPE i.

    FIELD-SYMBOLS <l_component_descr> LIKE LINE OF l_component_descrs.
    FIELD-SYMBOLS <l_component_value> TYPE any.

    CLEAR output.

    l_component_descrs = type->get_components( ).

    l_max_length       = strlen( input ).

    LOOP AT l_component_descrs ASSIGNING <l_component_descr>.
      IF l_offset >= l_max_length.
        CLEAR l_value.
      ELSE.
        IF input+l_offset NA separator.
          l_value      = input+l_offset.
          l_val_length = l_max_length - l_offset.
          l_offset     = l_max_length.
        ELSE.
          l_value      = input+l_offset(sy-fdpos).
          l_val_length = sy-fdpos.
          l_offset     = l_offset + sy-fdpos + 1.
        ENDIF.

        IF l_val_length > 1 AND l_value(1) = delimiter.
          l_val_offset = l_val_length - 1.

          IF l_value+l_val_offset(1) = delimiter.
            l_val_length = l_val_length - 2.
            l_value      = l_value+1(l_val_length).

            REPLACE ALL OCCURRENCES OF escaped_delimiter IN l_value WITH delimiter.
          ENDIF.
        ENDIF.
      ENDIF.

      ASSIGN COMPONENT <l_component_descr>-name OF STRUCTURE output TO <l_component_value>.
      CHECK sy-subrc = 0. " This is always true!

      decompress_value( EXPORTING input  = l_value
                                  type   = <l_component_descr>-type
                        IMPORTING output = <l_component_value> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD decompress_table.
    DATA l_line_type   TYPE REF TO cl_abap_typedescr.
    DATA l_struct_type TYPE REF TO cl_abap_structdescr.

    DATA l_target      TYPE REF TO data.

    DATA l_message     TYPE string.

    FIELD-SYMBOLS <l_source> TYPE any.
    FIELD-SYMBOLS <l_target> TYPE any.

    CLEAR output.

    l_line_type = type->get_table_line_type( ).

    CASE l_line_type->type_kind.

      WHEN cl_abap_typedescr=>typekind_struct1.
        CREATE DATA l_target TYPE (l_line_type->absolute_name).
        ASSIGN l_target->* TO <l_target>.

        l_struct_type ?= l_line_type.

        LOOP AT input ASSIGNING <l_source>.
          decompress_structure( EXPORTING type   = l_struct_type
                                          input  = <l_source>
                                IMPORTING output = <l_target> ).

          INSERT <l_target> INTO TABLE output.
        ENDLOOP.

      WHEN cl_abap_typedescr=>typekind_string.
        CREATE DATA l_target TYPE (l_line_type->absolute_name).
        ASSIGN l_target->* TO <l_target>.

        LOOP AT input ASSIGNING <l_source>.
          decompress_value( EXPORTING type   = l_line_type
                                      input  = <l_source>
                            IMPORTING output = <l_target> ).

          INSERT <l_target> INTO TABLE output.
        ENDLOOP.

      WHEN OTHERS.
        l_message = /gal/string=>replace_variables( input = TEXT-e00
                                                    var01 = l_line_type->absolute_name ).

        RAISE EXCEPTION TYPE /gal/cx_cfw_exception
          EXPORTING
            textid = /gal/cx_cfw_exception=>decompress_error
            var1   = l_message.

    ENDCASE.
  ENDMETHOD.


  METHOD decompress_value.
    output = input.
  ENDMETHOD.


  METHOD text_to_xstring.
    text_encoder->convert( EXPORTING data = input
                           IMPORTING buffer = output ).
  ENDMETHOD.


  METHOD xstring_to_text.
    text_decoder->convert( EXPORTING input = input
                           IMPORTING data  = output ).
  ENDMETHOD.
ENDCLASS.
