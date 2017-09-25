class /GAL/CONFIG_STORE definition
  public
  abstract
  create public

  global friends /GAL/CONFIG_NODE .

*"* public components of class /GAL/CONFIG_STORE
*"* do not include other source files here!!!
public section.
  type-pools ABAP .

  data ROOT type ref to /GAL/CONFIG_NODE read-only .

  methods AUTHORITY_CHECK
  abstract
    importing
      !NODE type ref to /GAL/CONFIG_NODE
      !ACTION type I
      !CLIENT type MANDT optional
      !USER_NAME type UNAME optional
    raising
      /GAL/CX_AUTH_CHECK_EXCEPTION .
  methods GET_NODE
    importing
      !PATH type STRING
    returning
      value(NODE) type ref to /GAL/CONFIG_NODE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
protected section.
*"* protected components of class /GAL/CONFIG_STORE
*"* do not include other source files here!!!

  methods DELETE_NODE
  abstract
    importing
      !ID type /GAL/CONFIG_KEY_ID
      !FORCE type ABAP_BOOL default ABAP_FALSE
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods DELETE_NODE_DESCRIPTION
  abstract
    importing
      !ID type /GAL/CONFIG_KEY_ID
      !LANGUAGE type LANGU default SY-LANGU
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods DELETE_NODE_VALUE
  abstract
    importing
      !ID type /GAL/CONFIG_KEY_ID
      !TYPE type /GAL/CONFIG_KEY_TYPE
      !DEFAULT type ABAP_BOOL default ABAP_FALSE
      !CLIENT type MANDT default SY-MANDT
      !USER_NAME type UNAME default SY-UNAME
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods GET_NODE_ID
    importing
      !PATH type STRING
    returning
      value(ID) type /GAL/CONFIG_KEY_ID .
  methods GET_NODE_PATH
  abstract
    importing
      value(ID) type /GAL/CONFIG_KEY_ID
    returning
      value(PATH) type STRING
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods INSERT_NODE
  abstract
    importing
      !ID type /GAL/CONFIG_KEY_ID
      !PARENT_ID type /GAL/CONFIG_PARENT_KEY_ID
      !NAME type /GAL/CONFIG_KEY_NAME
      !TYPE type /GAL/CONFIG_KEY_TYPE
      !FIXED_VALUE_TYPE type /GAL/CONFIG_VALUE_TYPE optional
      !AUTH_CLASS type /GAL/CONFIG_AUTH_CLASS_NAME optional
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods SELECT_CHILD_NODES
  abstract
    importing
      !ID type /GAL/CONFIG_KEY_ID
    returning
      value(DATA) type /GAL/CONFIG_KEYS
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods SELECT_NODE
  abstract
    importing
      !ID type /GAL/CONFIG_KEY_ID
    exporting
      !PARENT_ID type /GAL/CONFIG_PARENT_KEY_ID
      !NAME type /GAL/CONFIG_KEY_NAME
      !TYPE type /GAL/CONFIG_KEY_TYPE
      !FIXED_VALUE_TYPE type /GAL/CONFIG_VALUE_TYPE
      !AUTH_CLASS type /GAL/CONFIG_AUTH_CLASS_NAME
      !HAS_CHILD_NODES type ABAP_BOOL
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods SELECT_NODE_DESCRIPTION
  abstract
    importing
      !ID type /GAL/CONFIG_KEY_ID
      !LANGUAGE type LANGU default SY-LANGU
    returning
      value(DESCRIPTION) type /GAL/CONFIG_KEY_TEXT
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods SELECT_NODE_VALUE
  abstract
    importing
      !ID type /GAL/CONFIG_KEY_ID
      !TYPE type /GAL/CONFIG_KEY_TYPE
      !DEFAULT type ABAP_BOOL default ABAP_FALSE
      !CLIENT type MANDT default SY-MANDT
      !USER_NAME type UNAME default SY-UNAME
    exporting
      !VALUE_TYPE type /GAL/CONFIG_VALUE_TYPE
      !VALUE type /GAL/CONFIG_VALUE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods UPDATE_NODE
  abstract
    importing
      !ID type /GAL/CONFIG_KEY_ID
      !TYPE type /GAL/CONFIG_KEY_TYPE
      !FIXED_VALUE_TYPE type /GAL/CONFIG_VALUE_TYPE
      !AUTH_CLASS type /GAL/CONFIG_AUTH_CLASS_NAME
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods UPDATE_NODE_DESCRIPTION
  abstract
    importing
      !ID type /GAL/CONFIG_KEY_ID
      !LANGUAGE type LANGU default SY-LANGU
      !DESCRIPTION type /GAL/CONFIG_KEY_TEXT
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods UPDATE_NODE_VALUE
  abstract
    importing
      !ID type /GAL/CONFIG_KEY_ID
      !TYPE type /GAL/CONFIG_KEY_TYPE
      !DEFAULT type ABAP_BOOL default ABAP_FALSE
      !CLIENT type MANDT default SY-MANDT
      !USER_NAME type UNAME optional
      !VALUE_TYPE type /GAL/CONFIG_VALUE_TYPE
      !VALUE type /GAL/CONFIG_VALUE
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods UPDATE_NODE_NAME
  abstract
    importing
      !ID type /GAL/CONFIG_KEY_ID
      !NEW_NAME type /GAL/CONFIG_KEY_NAME
      !FORCE type ABAP_BOOL default ABAP_FALSE
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    returning
      value(NEW_ID) type /GAL/CONFIG_KEY_ID
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods GET_NODE_VALUES
  abstract
    importing
      !ID type /GAL/CONFIG_KEY_ID
      !WITH_CHILD_NODES type ABAP_BOOL default ABAP_FALSE
    returning
      value(VALUE_LIST) type /GAL/CONFIG_KEY_VALUE_LIST
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods UPDATE_SUBTREE_NODE_NAMES
  abstract
    importing
      !PARENT_ID type /GAL/CONFIG_KEY_ID
      !NEW_PARENT_ID type /GAL/CONFIG_KEY_ID
      !ORIGINAL_PATH type STRING
      !REPLACE_PATH type STRING
    changing
      !VALUE_LIST type /GAL/CONFIG_KEY_VALUE_LIST
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods INSERT_SUBTREE
  abstract
    importing
      !PARENT_ID type /GAL/CONFIG_KEY_ID
      !VALUE_LIST type /GAL/CONFIG_KEY_VALUE_LIST
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
private section.
*"* private components of class /GAL/CONFIG_STORE
*"* do not include other source files here!!!
ENDCLASS.



CLASS /GAL/CONFIG_STORE IMPLEMENTATION.


METHOD get_node.
  DATA l_node TYPE REF TO /gal/config_node.
  DATA l_names TYPE STANDARD TABLE OF string.

  FIELD-SYMBOLS <l_name> TYPE string.

  SPLIT path AT '/' INTO TABLE l_names.

  LOOP AT l_names ASSIGNING <l_name>.
    AT FIRST.
      IF <l_name> IS NOT INITIAL.
        RAISE EXCEPTION TYPE /gal/cx_config_exception
              EXPORTING textid = /gal/cx_config_exception=>invalid_node_path
                        var1   = path.
      ENDIF.

      l_node = root.
      CONTINUE.
    ENDAT.

    l_node = l_node->get_child_node( name = <l_name> ).
  ENDLOOP.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gal/cx_config_exception
          EXPORTING textid = /gal/cx_config_exception=>invalid_node_path
                    var1   = path.
  ENDIF.

  node = l_node.
ENDMETHOD.


METHOD get_node_id.
  CALL FUNCTION 'MD5_CALCULATE_HASH_FOR_CHAR'
    EXPORTING
      data = path
    IMPORTING
      hash = id.
ENDMETHOD.
ENDCLASS.
