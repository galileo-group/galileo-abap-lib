"! <p class="shorttext synchronized" lang="en">JSON Element</p>
CLASS /gal/json_element DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPE-POOLS:
      abap.

    CONSTANTS:

      "! <p class="shorttext synchronized" lang="en">Type: Array</p>
      type_array     TYPE c VALUE 'A',                      "#EC NOTEXT

      "! <p class="shorttext synchronized" lang="en">Type: Boolean value</p>
      type_boolean   TYPE c VALUE 'B',                      "#EC NOTEXT

      "! <p class="shorttext synchronized" lang="en">Type: Date</p>
      type_date      TYPE c VALUE 'D',                      "#EC NOTEXT

      "! <p class="shorttext synchronized" lang="en">Type: Timestamp</p>
      type_datetime  TYPE c VALUE '#',                      "#EC NOTEXT

      "! <p class="shorttext synchronized" lang="en">Type: Number</p>
      type_number    TYPE c VALUE 'N',                      "#EC NOTEXT

      "! <p class="shorttext synchronized" lang="en">Type: Null value</p>
      type_null      TYPE c VALUE ' ',                      "#EC NOTEXT

      "! <p class="shorttext synchronized" lang="en">Type: Object</p>
      type_object    TYPE c VALUE 'O',                      "#EC NOTEXT

      "! <p class="shorttext synchronized" lang="en">Type: String</p>
      type_string    TYPE c VALUE 'S',                      "#EC NOTEXT

      "! <p class="shorttext synchronized" lang="en">Type: Time</p>
      type_time      TYPE c VALUE 'T',                      "#EC NOTEXT

      "! <p class="shorttext synchronized" lang="en">Type: Undefined</p>
      type_undefined TYPE c VALUE '?'.                      "#EC NOTEXT


    DATA:

      "! <p class="shorttext synchronized" lang="en">Child elements</p>
      children TYPE /gal/json_element_children READ-ONLY,

      "! <p class="shorttext synchronized" lang="en">Type</p>
      type     TYPE c READ-ONLY,

      "! <p class="shorttext synchronized" lang="en">Value</p>
      value    TYPE REF TO data READ-ONLY.


    METHODS:

      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter type                   | <p class="shorttext synchronized" lang="en">Type</p>
      "! @parameter value                  | <p class="shorttext synchronized" lang="en">Value</p>
      "! @raising   /gal/cx_json_exception | <p class="shorttext synchronized" lang="en">JSON Serializer Exception</p>
      constructor
        IMPORTING
          !type  TYPE c DEFAULT /gal/json_element=>type_undefined
          !value TYPE any OPTIONAL
        RAISING
          /gal/cx_json_exception,

      "! <p class="shorttext synchronized" lang="en">Add child element</p>
      "!
      "! @parameter name                   | <p class="shorttext synchronized" lang="en">Name</p>
      "! @parameter element                | <p class="shorttext synchronized" lang="en">JSON Element</p>
      "! @parameter type                   | <p class="shorttext synchronized" lang="en">Type</p>
      "! @parameter value                  | <p class="shorttext synchronized" lang="en">Value</p>
      "! @parameter child                  | <p class="shorttext synchronized" lang="en">Added child element</p>
      "! @raising   /gal/cx_json_exception | <p class="shorttext synchronized" lang="en">JSON Serializer Exception</p>
      add_child
        IMPORTING
          !name        TYPE csequence OPTIONAL
          !element     TYPE REF TO /gal/json_element OPTIONAL
          !type        TYPE c DEFAULT type_undefined
          !value       TYPE any OPTIONAL
        RETURNING
          VALUE(child) TYPE REF TO /gal/json_element
        RAISING
          /gal/cx_json_exception,

      "! <p class="shorttext synchronized" lang="en">Return value</p>
      "!
      "! @parameter value | <p class="shorttext synchronized" lang="en">Value</p>
      get_value
        EXPORTING
          !value TYPE any,

      "! <p class="shorttext synchronized" lang="en">Return child elements</p>
      "!
      "! @parameter name                   | <p class="shorttext synchronized" lang="en">Name</p>
      "! @parameter index                  | <p class="shorttext synchronized" lang="en">Index</p>
      "! @parameter throw_exception        | <p class="shorttext synchronized" lang="en">Throw exception in case of an error</p>
      "! @parameter child                  | <p class="shorttext synchronized" lang="en">Child JSON element</p>
      "! @raising   /gal/cx_json_exception | <p class="shorttext synchronized" lang="en">JSON Serializer Exception</p>
      get_child
        IMPORTING
          !name            TYPE csequence OPTIONAL
          !index           TYPE i OPTIONAL
          !throw_exception TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(child)     TYPE REF TO /gal/json_element
        RAISING
          /gal/cx_json_exception,

      "! <p class="shorttext synchronized" lang="en">Sort named child elements</p>
      sort_children.


  PROTECTED SECTION.


  PRIVATE SECTION.

ENDCLASS.



CLASS /gal/json_element IMPLEMENTATION.


  METHOD add_child.
    DATA:
      l_child LIKE LINE OF children.

* Convert key
    l_child-name = name.

* Check for unique object member name
    IF me->type = type_object.
      READ TABLE children
            WITH KEY name = l_child-name
                 TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        RAISE EXCEPTION TYPE /gal/cx_json_exception
          EXPORTING
            textid = /gal/cx_json_exception=>duplicate_attribute_name
            var1   = l_child-name.
      ENDIF.
    ENDIF.

* Add child
    IF element IS INITIAL.
      IF value IS SUPPLIED.
        CREATE OBJECT l_child-element
          EXPORTING
            type  = type
            value = value.
      ELSE.
        CREATE OBJECT l_child-element
          EXPORTING
            type = type.
      ENDIF.
    ELSE.
      l_child-element = element.
    ENDIF.

    INSERT l_child INTO TABLE children.

* Return new child
    child = l_child-element.
  ENDMETHOD.


  METHOD constructor.
    DATA:
      l_type       TYPE c,
      l_type_abs   TYPE string,

      l_type_descr TYPE REF TO cl_abap_typedescr,
      l_ref_descr  TYPE REF TO cl_abap_typedescr,

      l_element    TYPE REF TO /gal/json_element.

    FIELD-SYMBOLS:
      <l_value> TYPE any.

    IF type <> type_undefined.
      l_type = type.
    ELSEIF value IS SUPPLIED.
      cl_abap_typedescr=>describe_by_data( EXPORTING  p_data      = value
                                           RECEIVING  p_descr_ref = l_type_descr
                                           EXCEPTIONS OTHERS      = 1 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE /gal/cx_json_exception
          EXPORTING
            textid = /gal/cx_json_exception=>cannot_determine_type.
      ENDIF.

      IF l_type_descr->absolute_name = '\TYPE-POOL=ABAP\TYPE=ABAP_BOOL' OR
         l_type_descr->absolute_name = '\TYPE=/GAL/ABAP_BOOL'           OR
         l_type_descr->absolute_name = '\TYPE=FLAG'.

        l_type = type_boolean.

      ELSEIF l_type_descr->absolute_name = '\TYPE=TIMESTAMP' OR
             l_type_descr->absolute_name = '\TYPE=TIMESTAMPL'.

        IF value IS NOT INITIAL.
          l_type = type_datetime.
        ELSE.
          l_type = type_null.
        ENDIF.

      ELSE.
        CASE l_type_descr->type_kind.

          WHEN cl_abap_typedescr=>typekind_date.
            IF value IS NOT INITIAL.
              l_type = type_date.
            ELSE.
              l_type = type_null.
            ENDIF.

          WHEN cl_abap_typedescr=>typekind_time.
            l_type = type_time.

          WHEN cl_abap_typedescr=>typekind_float
            OR cl_abap_typedescr=>typekind_int
            OR cl_abap_typedescr=>typekind_int1
            OR cl_abap_typedescr=>typekind_int2
            OR cl_abap_typedescr=>typekind_packed.
            l_type = type_number.

          WHEN cl_abap_typedescr=>typekind_char
            OR cl_abap_typedescr=>typekind_num
            OR cl_abap_typedescr=>typekind_hex
            OR cl_abap_typedescr=>typekind_string
            OR cl_abap_typedescr=>typekind_xstring.
            l_type = type_string.

          WHEN cl_abap_typedescr=>typekind_oref.
            l_ref_descr = cl_abap_typedescr=>describe_by_object_ref( value ).
            l_type_abs  = l_ref_descr->absolute_name.

            IF l_type_abs = `\CLASS=/GAL/JSON_ELEMENT`.
              l_element   ?= value.
              me->type     = l_element->type.
              me->value    = l_element->value.
              me->children = l_element->children.
              RETURN.
            ELSE.
              RAISE EXCEPTION TYPE /gal/cx_json_exception
                EXPORTING
                  textid = /gal/cx_json_exception=>cannot_determine_json_type
                  var1   = l_type_abs.
            ENDIF.

          WHEN OTHERS.
            l_type_abs = l_type_descr->absolute_name.

            RAISE EXCEPTION TYPE /gal/cx_json_exception
              EXPORTING
                textid = /gal/cx_json_exception=>cannot_determine_json_type
                var1   = l_type_abs.

        ENDCASE.
      ENDIF.
    ELSE.
      l_type = type_object.
    ENDIF.

    me->type = l_type.

* Create local copy of value (if supplied)
    IF value IS SUPPLIED AND l_type <> type_null.
      CREATE DATA me->value LIKE value.
      ASSIGN me->value->* TO <l_value>.
      <l_value> = value.
    ENDIF.
  ENDMETHOD.


  METHOD get_child.
    DATA:
      l_name TYPE string.

    FIELD-SYMBOLS:
      <l_child> LIKE LINE OF children.

* Convert name to string
    l_name = name.

* Calculate search index range
    IF index > 0.
      READ TABLE children INDEX index ASSIGNING <l_child>.
      IF sy-subrc = 0 AND name IS SUPPLIED AND <l_child>-name <> l_name.
        UNASSIGN <l_child>.
      ENDIF.
    ELSE.
      READ TABLE children
            WITH KEY name = l_name
                 ASSIGNING <l_child>.
    ENDIF.

* Find matching child
    IF <l_child> IS ASSIGNED.
      child = <l_child>-element.
    ELSEIF throw_exception = abap_true.
      RAISE EXCEPTION TYPE /gal/cx_json_exception
        EXPORTING
          textid = /gal/cx_json_exception=>no_matching_child_element.
    ENDIF.
  ENDMETHOD.


  METHOD get_value.
    FIELD-SYMBOLS:
      <l_value> TYPE any.

    CLEAR value.

    IF me->value IS NOT INITIAL.
      ASSIGN me->value->* TO <l_value>.
      value = <l_value>.
    ENDIF.
  ENDMETHOD.


  METHOD sort_children.
    IF type = type_object.
      SORT children BY name.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
