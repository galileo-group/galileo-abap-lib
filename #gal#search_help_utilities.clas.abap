"! <p class="shorttext synchronized" lang="en">Utilities for Search Helps/Search Help Exits</p>
class /GAL/SEARCH_HELP_UTILITIES definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  "! <p class="shorttext synchronized" lang="en">Get context information (for Search Help Exits only!)</p>
  "!
  "! @parameter is_selection_screen | <p class="shorttext synchronized" lang="en">Flag: Current screen is a selection screen</p>
  class-methods GET_CONTEXT_INFO
    exporting
      !IS_SELECTION_SCREEN type ABAP_BOOL .
  "! <p class="shorttext synchronized" lang="en">Get value of a dynpro field</p>
  "!
  "! @parameter name  | <p class="shorttext synchronized" lang="en">Dynpro field name (Wildcard)</p>
  "! @parameter value | <p class="shorttext synchronized" lang="en">Target field for dynpro field value</p>
  class-methods GET_DYNPRO_FIELD_VALUE
    importing
      !NAME type CSEQUENCE default `*`
    exporting
      !VALUE type ANY .
  "! <p class="shorttext synchronized" lang="en">Get a copy of current help info structure</p>
  "!
  "! @parameter help_info | <p class="shorttext synchronized" lang="en">Interface between Help-Processor and C-Basis</p>
  class-methods GET_HELP_INFO
    returning
      value(HELP_INFO) type PHELP .
  "! <p class="shorttext synchronized" lang="en">Set value of a dynpro field</p>
  "!
  "! @parameter name  | <p class="shorttext synchronized" lang="en">Dynpro field name (Wildcard)</p>
  "! @parameter force | <p class="shorttext synchronized" lang="en">Force change for output-only fields</p>
  "! @parameter value | <p class="shorttext synchronized" lang="en">Source field for dynpro field value</p>
  class-methods SET_DYNPRO_FIELD_VALUE
    importing
      !NAME type CSEQUENCE default `*`
      !FORCE type ABAP_BOOL default ABAP_FALSE
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
  DATA l_callstack TYPE sys_callst.

  FIELD-SYMBOLS <l_help_info> TYPE phelp.
  FIELD-SYMBOLS <l_callstack> LIKE LINE OF l_callstack.

* Try to get help info from system (works during search help exits)
  ASSIGN ('(SAPMSHLP)PHELP') TO <l_help_info>.
  IF sy-subrc = 0 AND <l_help_info> IS NOT INITIAL.
    help_info = <l_help_info>.
    RETURN.
  ENDIF.

* Build own help info
  CALL FUNCTION 'SYSTEM_CALLSTACK'
    IMPORTING
      et_callstack = l_callstack.

  LOOP AT l_callstack ASSIGNING <l_callstack> WHERE eventtype CP 'MODULE*'.
    help_info-dynpprog = <l_callstack>-progname.
    help_info-dynpro   = sy-dynnr.
    EXIT.
  ENDLOOP.
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
  DELETE l_dynpro_fields WHERE fieldname NP name.

  LOOP AT l_dynpro_fields ASSIGNING <l_dynpro_fields>.
    CONCATENATE '(' l_help_info-dynpprog ')' <l_dynpro_fields>-fieldname INTO l_field_name.

    ASSIGN (l_field_name) TO <l_field>.
    IF sy-subrc = 0.
      l_field_descr = cl_abap_typedescr=>describe_by_data( <l_field> ).

      IF l_field_descr->absolute_name = l_type_descr->absolute_name.
        l_hit_counter = l_hit_counter + 1.
        CONTINUE.
      ENDIF.
    ENDIF.

    DELETE l_dynpro_fields.
  ENDLOOP.

* Update dynpro field if a unique field was found
  IF l_hit_counter = 1.
    READ TABLE l_dynpro_fields INDEX 1 ASSIGNING <l_dynpro_fields>.
    IF sy-subrc = 0 AND ( <l_dynpro_fields>-fieldinp = abap_true OR force = abap_true ).
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
