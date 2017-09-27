class /GAL/SEARCH_HELP_UTILITIES definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  class-methods GET_CONTEXT_INFO
    exporting
      !IS_SELECTION_SCREEN type ABAP_BOOL .
  class-methods GET_DYNPRO_FIELD_VALUE
    importing
      !NAME type CSEQUENCE default `*`
    changing
      !VALUE type ANY .
  class-methods GET_HELP_INFO
    returning
      value(HELP_INFO) type PHELP .
  class-methods SET_DYNPRO_FIELD_VALUE
    importing
      !NAME type CSEQUENCE default `*`
      !VALUE type ANY .
protected section.
private section.
ENDCLASS.



CLASS /GAL/SEARCH_HELP_UTILITIES IMPLEMENTATION.


METHOD get_context_info.
  DATA l_help_info TYPE phelp.

  l_help_info = get_help_info( ).

  IF l_help_info-kz_sydyn = 'S'.
    is_selection_screen = abap_true.
  ELSE.
    is_selection_screen = abap_false.
  ENDIF.
ENDMETHOD.


METHOD get_dynpro_field_value.
  DATA l_help_info     TYPE phelp.

  DATA l_type_descr    TYPE REF TO cl_abap_typedescr.
  DATA l_field_descr   TYPE REF TO cl_abap_typedescr.

  DATA l_dynpro_fields TYPE STANDARD TABLE OF dynpread.
  DATA l_field_name    TYPE string.
  DATA l_hit_counter   TYPE i.

  FIELD-SYMBOLS <l_field>         TYPE any.
  FIELD-SYMBOLS <l_dynpro_fields> LIKE LINE OF l_dynpro_fields.

* Initialize target field
  CLEAR value.

* Determine data element of target field
  l_type_descr = cl_abap_typedescr=>describe_by_data( value ).

* Get Help Processor information
  l_help_info = /gal/search_help_utilities=>get_help_info( ).
  IF l_help_info IS INITIAL.
    RETURN.
  ENDIF.

* Get fields and values of current dynpro
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = l_help_info-dynpprog
      dynumb               = l_help_info-dynpro
      request              = 'A'
      determine_loop_index = abap_true
    TABLES
      dynpfields           = l_dynpro_fields
    EXCEPTIONS
      OTHERS               = 1.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* Remove fields that do not belong to current step loop index
  IF l_help_info-stepl > 0.
    DELETE l_dynpro_fields WHERE stepl > 0 AND stepl <> l_help_info-stepl.
  ELSE.
    DELETE l_dynpro_fields WHERE stepl > 0.
  ENDIF.

* Find matching dynpro field by name and data element. Abort if result is not unique
  LOOP AT l_dynpro_fields ASSIGNING <l_dynpro_fields> WHERE fieldname CP name.
    CONCATENATE '(' l_help_info-dynpprog ')' <l_dynpro_fields>-fieldname INTO l_field_name.

    ASSIGN (l_field_name) TO <l_field>.
    CHECK sy-subrc = 0.

    l_field_descr = cl_abap_typedescr=>describe_by_data( <l_field> ).

    CHECK l_field_descr->absolute_name = l_type_descr->absolute_name.

    l_hit_counter = l_hit_counter + 1.

    IF l_hit_counter = 1.
      value = <l_dynpro_fields>-fieldvalue.
    ELSE.
      CLEAR value.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDMETHOD.


METHOD get_help_info.
  FIELD-SYMBOLS <l_help_info> TYPE phelp.

  ASSIGN ('(SAPMSHLP)PHELP') TO <l_help_info>.
  IF sy-subrc = 0.
    help_info = <l_help_info>.
  ENDIF.
ENDMETHOD.


METHOD set_dynpro_field_value.
  DATA l_help_info     TYPE phelp.

  DATA l_type_descr    TYPE REF TO cl_abap_typedescr.
  DATA l_field_descr   TYPE REF TO cl_abap_typedescr.

  DATA l_dynpro_fields TYPE STANDARD TABLE OF dynpread.
  DATA l_field_name    TYPE string.
  DATA l_hit_counter   TYPE i.

  FIELD-SYMBOLS <l_field>         TYPE any.
  FIELD-SYMBOLS <l_dynpro_fields> LIKE LINE OF l_dynpro_fields.

* Determine data element of source field
  l_type_descr = cl_abap_typedescr=>describe_by_data( value ).

* Get Help Processor information
  l_help_info = /gal/search_help_utilities=>get_help_info( ).
  IF l_help_info IS INITIAL.
    RETURN.
  ENDIF.

* Get fields and values of current dynpro
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = l_help_info-dynpprog
      dynumb     = l_help_info-dynpro
      request    = 'A'
    TABLES
      dynpfields = l_dynpro_fields
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* Find matching dynpro field by name and data element. Abort if result is not unique
  LOOP AT l_dynpro_fields ASSIGNING <l_dynpro_fields> WHERE fieldname CP name.
    CONCATENATE '(' l_help_info-dynpprog ')' <l_dynpro_fields>-fieldname INTO l_field_name.

    ASSIGN (l_field_name) TO <l_field>.
    IF sy-subrc = 0.
      l_field_descr = cl_abap_typedescr=>describe_by_data( <l_field> ).

      IF l_field_descr->absolute_name = l_type_descr->absolute_name.
        l_hit_counter = l_hit_counter + 1.

        IF l_hit_counter = 1.
          CONTINUE.
        ELSE.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

    DELETE l_dynpro_fields.
  ENDLOOP.

* Update dynpro field if a unique field was found
  IF l_hit_counter = 1.
    READ TABLE l_dynpro_fields INDEX 1 ASSIGNING <l_dynpro_fields>.
    IF sy-subrc = 0 AND <l_dynpro_fields>-fieldinp = abap_true.
      <l_dynpro_fields>-fieldvalue = value.

      CALL FUNCTION 'DYNP_UPDATE_FIELDS'
        EXPORTING
          dyname     = l_help_info-dynpprog
          dynumb     = l_help_info-dynpro
          request    = 'A'
        TABLES
          dynpfields = l_dynpro_fields
        EXCEPTIONS
          OTHERS     = 0.
    ENDIF.
  ENDIF.
ENDMETHOD.
ENDCLASS.
