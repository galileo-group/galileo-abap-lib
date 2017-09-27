class /GAL/CONFIG_STORE_REMOTE definition
  public
  inheriting from /GAL/CONFIG_STORE
  final
  create public .

public section.

  data RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO read-only .

  methods CONSTRUCTOR
    importing
      !RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO
    raising
      /GAL/CX_CONFIG_EXCEPTION .

  methods AUTHORITY_CHECK
    redefinition .
protected section.

  methods DELETE_NODE
    redefinition .
  methods DELETE_NODE_DESCRIPTION
    redefinition .
  methods DELETE_NODE_VALUE
    redefinition .
  methods GET_NODE_PATH
    redefinition .
  methods GET_NODE_VALUES
    redefinition .
  methods INSERT_NODE
    redefinition .
  methods SELECT_CHILD_NODES
    redefinition .
  methods SELECT_NODE
    redefinition .
  methods SELECT_NODE_DESCRIPTION
    redefinition .
  methods SELECT_NODE_VALUE
    redefinition .
  methods UPDATE_NODE
    redefinition .
  methods UPDATE_NODE_DESCRIPTION
    redefinition .
  methods UPDATE_NODE_NAME
    redefinition .
  methods UPDATE_NODE_VALUE
    redefinition .
  methods UPDATE_SUBTREE_NODE_NAMES
    redefinition .
  methods INSERT_SUBTREE
    redefinition .
private section.
ENDCLASS.



CLASS /GAL/CONFIG_STORE_REMOTE IMPLEMENTATION.


METHOD authority_check.
  DATA l_message TYPE string.

  CALL FUNCTION '/GAL/CS_AUTHORITY_CHECK'
    EXPORTING
      rfc_route_info = rfc_route_info
      path           = node->path
      action         = action
      client         = client
      user_name      = user_name
    EXCEPTIONS
      OTHERS         = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    RAISE EXCEPTION TYPE /gal/cx_auth_check_exception
      EXPORTING
        textid = /gal/cx_auth_check_exception=>custom_exception
        var1   = l_message.
  ENDIF.
ENDMETHOD.


METHOD constructor.
  DATA l_root_id TYPE /gal/config_key_id.

  super->constructor( ).

  me->rfc_route_info = rfc_route_info.

  l_root_id = get_node_id( `/` ).

  CREATE OBJECT root
    EXPORTING
      store = me
      id    = l_root_id.

ENDMETHOD.


METHOD delete_node.
  RAISE EXCEPTION TYPE /gal/cx_config_exception
    EXPORTING
      textid = /gal/cx_config_exception=>not_supported_for_remote_cs.
ENDMETHOD.


METHOD delete_node_description.
  RAISE EXCEPTION TYPE /gal/cx_config_exception
    EXPORTING
      textid = /gal/cx_config_exception=>not_supported_for_remote_cs.
ENDMETHOD.


METHOD delete_node_value.
  RAISE EXCEPTION TYPE /gal/cx_config_exception
    EXPORTING
      textid = /gal/cx_config_exception=>not_supported_for_remote_cs.
ENDMETHOD.


METHOD get_node_path.
  RAISE EXCEPTION TYPE /gal/cx_config_exception
    EXPORTING
      textid = /gal/cx_config_exception=>not_supported_for_remote_cs.
ENDMETHOD.


  METHOD get_node_values.
    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>not_supported_for_remote_cs.
  ENDMETHOD.


METHOD insert_node.
  RAISE EXCEPTION TYPE /gal/cx_config_exception
    EXPORTING
      textid = /gal/cx_config_exception=>not_supported_for_remote_cs.
ENDMETHOD.


  METHOD insert_subtree.
    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>not_supported_for_remote_cs.
  ENDMETHOD.


METHOD select_child_nodes.
  DATA l_message TYPE string.

  CALL FUNCTION '/GAL/CS_SELECT_CHILD_NODES'
    EXPORTING
      rfc_route_info = rfc_route_info
      id             = id
    IMPORTING
      data           = data
    EXCEPTIONS
      OTHERS         = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>custom_exception
        var1   = l_message.
  ENDIF.
ENDMETHOD.


METHOD select_node.
  DATA l_message TYPE string.

  CALL FUNCTION '/GAL/CS_SELECT_NODE'
    EXPORTING
      rfc_route_info   = rfc_route_info
      id               = id
    IMPORTING
      parent_id        = parent_id
      name             = name
      node_type        = type
      fixed_value_type = fixed_value_type
      auth_class       = auth_class
      has_child_nodes  = has_child_nodes
    EXCEPTIONS
      OTHERS           = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>custom_exception
        var1   = l_message.
  ENDIF.
ENDMETHOD.


METHOD select_node_description.
  DATA l_message TYPE string.

  CALL FUNCTION '/GAL/CS_SELECT_NODE_DESCR'
    EXPORTING
      rfc_route_info = rfc_route_info
      id             = id
      language       = language
    IMPORTING
      description    = description
    EXCEPTIONS
      OTHERS         = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>custom_exception
        var1   = l_message.
  ENDIF.
ENDMETHOD.


METHOD select_node_value.
  DATA l_message TYPE string.

  CALL FUNCTION '/GAL/CS_SELECT_NODE_VALUE'
    EXPORTING
      rfc_route_info = rfc_route_info
      id             = id
      node_type      = type
      default_value  = default
      client         = client
      user_name      = user_name
    IMPORTING
      value_type     = value_type
      value          = value
    EXCEPTIONS
      OTHERS         = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>custom_exception
        var1   = l_message.
  ENDIF.
ENDMETHOD.


METHOD update_node.
  RAISE EXCEPTION TYPE /gal/cx_config_exception
    EXPORTING
      textid = /gal/cx_config_exception=>not_supported_for_remote_cs.
ENDMETHOD.


method UPDATE_NODE_DESCRIPTION.
  RAISE EXCEPTION TYPE /gal/cx_config_exception
    EXPORTING
      textid = /gal/cx_config_exception=>not_supported_for_remote_cs.
endmethod.


METHOD update_node_name.
  RAISE EXCEPTION TYPE /gal/cx_config_exception
    EXPORTING
      textid = /gal/cx_config_exception=>not_supported_for_remote_cs.
ENDMETHOD.


METHOD update_node_value.
  RAISE EXCEPTION TYPE /gal/cx_config_exception
    EXPORTING
      textid = /gal/cx_config_exception=>not_supported_for_remote_cs.
ENDMETHOD.


  METHOD update_subtree_node_names.
    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>not_supported_for_remote_cs.
  ENDMETHOD.
ENDCLASS.
