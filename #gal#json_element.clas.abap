class /GAL/JSON_ELEMENT definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  constants TYPE_ARRAY type C value 'A'. "#EC NOTEXT
  constants TYPE_BOOLEAN type C value 'B'. "#EC NOTEXT
  constants TYPE_FLOAT type C value 'F'. "#EC NOTEXT
  constants TYPE_INTEGER type C value 'I'. "#EC NOTEXT
  constants TYPE_NULL type C value ' '. "#EC NOTEXT
  constants TYPE_OBJECT type C value 'O'. "#EC NOTEXT
  constants TYPE_STRING type C value 'S'. "#EC NOTEXT
  constants TYPE_UNDEFINED type C value '?'. "#EC NOTEXT
  data CHILDREN type /GAL/JSON_ELEMENT_CHILDREN read-only .
  data TYPE type C read-only .
  data VALUE type ref to DATA read-only .

  methods CONSTRUCTOR
    importing
      !TYPE type C default /GAL/JSON_ELEMENT=>TYPE_UNDEFINED
      !VALUE type ANY optional .
  methods ADD_CHILD
    importing
      !KEY type STRING optional
      !ELEMENT type ref to /GAL/JSON_ELEMENT optional
      !TYPE type C default TYPE_UNDEFINED
      !VALUE type ANY optional
    returning
      value(CHILD) type ref to /GAL/JSON_ELEMENT
    raising
      /GAL/CX_JSON_EXCEPTION .
  methods GET_VALUE
    exporting
      !VALUE type ANY .
  methods GET_CHILD
    importing
      !NAME type STRING optional
      !INDEX type I optional
      !THROW_EXCEPTION type ABAP_BOOL default ABAP_FALSE
    returning
      value(CHILD) type ref to /GAL/JSON_ELEMENT
    raising
      /GAL/CX_JSON_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS /GAL/JSON_ELEMENT IMPLEMENTATION.


METHOD add_child.
  DATA l_child LIKE LINE OF children.

* Check for unique object member name
  IF type = type_object.
    READ TABLE children
          WITH KEY key = key
               TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE /gal/cx_json_exception
        EXPORTING
          textid = /gal/cx_json_exception=>duplicate_attribute_name
          var1   = key.
    ENDIF.
  ENDIF.

* Add child
  l_child-key = key.

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
  DATA l_type       TYPE c.
  DATA l_type_descr TYPE REF TO cl_abap_typedescr.

  FIELD-SYMBOLS <l_value> TYPE any.

  IF type <> type_undefined.
    l_type = type.
  ELSEIF value IS SUPPLIED.
    cl_abap_typedescr=>describe_by_data( EXPORTING  p_data      = value
                                         RECEIVING  p_descr_ref = l_type_descr
                                         EXCEPTIONS OTHERS      = 1 ).
    IF sy-subrc = 0.
      CASE l_type_descr->type_kind.

        WHEN cl_abap_typedescr=>typekind_float.
          l_type = type_float.

        WHEN cl_abap_typedescr=>typekind_int.
          l_type = type_integer.

        WHEN OTHERS.
          l_type = type_string.

      ENDCASE.
    ELSE.
      l_type = type_string.
    ENDIF.
  ELSE.
    l_type = type_object.
  ENDIF.

  me->type = l_type.

* Create local copy of value (if supplied)
  IF value IS SUPPLIED.
    CREATE DATA me->value LIKE value.
    ASSIGN me->value->* TO <l_value>.
    <l_value> = value.
  ENDIF.
ENDMETHOD.


METHOD get_child.
  FIELD-SYMBOLS <l_child> LIKE LINE OF children.

* Calculate search index range
  IF index > 0.
    READ TABLE children INDEX index ASSIGNING <l_child>.
    IF sy-subrc = 0 AND name IS SUPPLIED AND <l_child>-key <> name.
      UNASSIGN <l_child>.
    ENDIF.
  ELSE.
    READ TABLE children
          WITH KEY key = name
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
  FIELD-SYMBOLS <l_value> TYPE any.

  CLEAR value.

  IF me->value IS NOT INITIAL.
    ASSIGN me->value->* TO <l_value>.
    value = <l_value>.
  ENDIF.
ENDMETHOD.
ENDCLASS.
