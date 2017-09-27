class /GAL/UUID definition
  public
  final
  create public .

*"* public components of class /GAL/UUID
*"* do not include other source files here!!!
public section.

  class-methods CLASS_CONSTRUCTOR .
  class-methods CREATE_BASE64
    returning
      value(UUID) type GUID_22 .
  class-methods CREATE_CHAR
    returning
      value(UUID) type GUID_32 .
  class-methods CREATE_RAW
    returning
      value(UUID) type GUID_16 .
protected section.
*"* protected components of class /GAL/UUID
*"* do not include other source files here!!!
private section.
*"* private components of class /GAL/UUID
*"* do not include other source files here!!!

  constants MODE_ABAP_OO type I value 2. "#EC NOTEXT
  constants MODE_LEGACY type I value 1. "#EC NOTEXT
  class-data MODE type I .
ENDCLASS.



CLASS /GAL/UUID IMPLEMENTATION.


METHOD class_constructor.

* Check if class CL_SYSTEM_UUID is available in the system
  cl_abap_typedescr=>describe_by_name( EXPORTING  p_name = `CL_SYSTEM_UUID`
                                       EXCEPTIONS OTHERS = 1 ).
  IF sy-subrc = 0.
    mode = mode_abap_oo.
  ELSE.
    mode = mode_legacy.
  ENDIF.
ENDMETHOD.


METHOD create_base64.
  DATA l_function  TYPE string.

  DATA l_exception TYPE REF TO cx_root.
  DATA l_message   TYPE string.

  CASE mode.

    WHEN mode_legacy.
      l_function = 'GUID_CREATE'.

      CALL FUNCTION l_function
        IMPORTING
          ev_guid_22 = uuid.

    WHEN mode_abap_oo.
      TRY.
          CALL METHOD ('CL_SYSTEM_UUID')=>create_uuid_c22_static
            RECEIVING
              uuid = uuid.

        CATCH cx_root INTO l_exception.                  "#EC CATCH_ALL
          l_message = l_exception->get_text( ).

          MESSAGE l_message TYPE 'X'.

      ENDTRY.

  ENDCASE.
ENDMETHOD.


METHOD create_char.
  DATA l_function  TYPE string.

  DATA l_exception TYPE REF TO cx_root.
  DATA l_message   TYPE string.

  CASE mode.

    WHEN mode_legacy.
      l_function = 'GUID_CREATE'.

      CALL FUNCTION l_function
        IMPORTING
          ev_guid_32 = uuid.

    WHEN mode_abap_oo.
      TRY.
          CALL METHOD ('CL_SYSTEM_UUID')=>create_uuid_c32_static
            RECEIVING
              uuid = uuid.

        CATCH cx_root INTO l_exception.                  "#EC CATCH_ALL
          l_message = l_exception->get_text( ).

          MESSAGE l_message TYPE 'X'.

      ENDTRY.

  ENDCASE.
ENDMETHOD.


METHOD create_raw.
  DATA l_function  TYPE string.

  DATA l_exception TYPE REF TO cx_root.
  DATA l_message   TYPE string.

  CASE mode.

    WHEN mode_legacy.
      l_function = 'GUID_CREATE'.

      CALL FUNCTION l_function
        IMPORTING
          ev_guid_16 = uuid.

    WHEN mode_abap_oo.
      TRY.
          CALL METHOD ('CL_SYSTEM_UUID')=>create_uuid_x16_static
            RECEIVING
              uuid = uuid.

        CATCH cx_root INTO l_exception.                  "#EC CATCH_ALL
          l_message = l_exception->get_text( ).

          MESSAGE l_message TYPE 'X'.

      ENDTRY.

  ENDCASE.
ENDMETHOD.
ENDCLASS.
