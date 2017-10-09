class /GAL/CONFIG_NODE definition
  public
  final
  create private

  global friends /GAL/CONFIG_STORE
                 /GAL/CONFIG_STORE_LOCAL .

*"* public components of class /GAL/CONFIG_NODE
*"* do not include other source files here!!!
public section.
  type-pools ABAP .

  constants CONST_NODE_TYPE_FOLDER type /GAL/CONFIG_KEY_TYPE value 'FOLDER'. "#EC NOTEXT
  constants CONST_NODE_TYPE_VALUE_CLIENT type /GAL/CONFIG_KEY_TYPE value 'VAL_CLIENT'. "#EC NOTEXT
  constants CONST_NODE_TYPE_VALUE_SYSTEM type /GAL/CONFIG_KEY_TYPE value 'VAL_SYSTEM'. "#EC NOTEXT
  constants CONST_NODE_TYPE_VALUE_USER type /GAL/CONFIG_KEY_TYPE value 'VAL_USER'. "#EC NOTEXT
  data AUTHENTICATOR_CLASS type /GAL/CONFIG_AUTH_CLASS_NAME read-only .
  data FIXED_VALUE_TYPE type /GAL/CONFIG_VALUE_TYPE read-only .
  data ID type /GAL/CONFIG_KEY_ID read-only .
  data IS_CLIENT_DEPENDENT type ABAP_BOOL read-only .
  data IS_FOLDER type ABAP_BOOL read-only .
  data IS_PARENT type ABAP_BOOL read-only .
  data IS_VALUE type ABAP_BOOL read-only .
  data NAME type /GAL/CONFIG_KEY_NAME read-only .
  data PARENT type ref to /GAL/CONFIG_NODE read-only .
  data PATH type STRING read-only .
  data TYPE type /GAL/CONFIG_KEY_TYPE read-only .

  methods SET_NAME
    importing
      !NAME type /GAL/CONFIG_KEY_NAME
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods CREATE_CHILD_NODE
    importing
      !NAME type /GAL/CONFIG_KEY_NAME
      !TYPE type /GAL/CONFIG_KEY_TYPE
      !FIXED_VALUE_TYPE type /GAL/CONFIG_VALUE_TYPE optional
    returning
      value(NODE) type ref to /GAL/CONFIG_NODE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods DELETE
    importing
      !FORCE type ABAP_BOOL default ABAP_FALSE
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods DELETE_DESCRIPTION
    importing
      !LANGUAGE type LANGU default SY-LANGU
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods DELETE_VALUE
    importing
      !DEFAULT type ABAP_BOOL default ABAP_FALSE
      !CLIENT type MANDT default SY-MANDT
      !USER_NAME type UNAME default SY-UNAME
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods GET_CHILD_NODE
    importing
      !NAME type /GAL/CONFIG_KEY_NAME
    returning
      value(NODE) type ref to /GAL/CONFIG_NODE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods GET_CHILD_NODES
    importing
      !TYPE type /GAL/CONFIG_KEY_TYPE optional
    returning
      value(NODES) type /GAL/CONFIG_NODES
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods GET_DESCRIPTION
    importing
      !LANGUAGE type LANGU default SY-LANGU
    preferred parameter LANGUAGE
    returning
      value(DESCRIPTION) type STRING
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods GET_VALUE
    importing
      !CLIENT type MANDT default SY-MANDT
      !USER_NAME type UNAME default SY-UNAME
      !DEFAULT_VALUE type ANY optional
    exporting
      !TYPE type /GAL/CONFIG_VALUE_TYPE
      !VALUE type ANY
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods GET_VALUE_RAW
    importing
      !DEFAULT type ABAP_BOOL default ABAP_FALSE
      !CLIENT type MANDT default SY-MANDT
      !USER_NAME type UNAME default SY-UNAME
    exporting
      !TYPE type /GAL/CONFIG_VALUE_TYPE
      !VALUE_RAW type ANY
      !IS_XML type ABAP_BOOL
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods SET_AUTHENTICATOR_CLASS
    importing
      !AUTHENTICATOR_CLASS type /GAL/CONFIG_AUTH_CLASS_NAME optional
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods SET_DESCRIPTION
    importing
      !LANGUAGE type LANGU default SY-LANGU
      !DESCRIPTION type STRING
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods SET_FIXED_VALUE_TYPE
    importing
      !FIXED_VALUE_TYPE type /GAL/CONFIG_VALUE_TYPE optional
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods SET_TYPE
    importing
      !TYPE type /GAL/CONFIG_KEY_TYPE
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods SET_VALUE
    importing
      !CLIENT type MANDT default SY-MANDT
      !USER_NAME type UNAME default SY-UNAME
      !VALUE type ANY
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods SET_VALUE_RAW
    importing
      !DEFAULT type ABAP_BOOL default ABAP_FALSE
      !CLIENT type MANDT default SY-MANDT
      !USER_NAME type UNAME default SY-UNAME
      !TYPE type /GAL/CONFIG_VALUE_TYPE
      !VALUE_RAW type ANY
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods ENQUEUE_NODE
    importing
      !ENQUEUE_CHILD_NODES type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods DEQUEUE_NODE
    raising
      /GAL/CX_LOCK_EXCEPTION .
  methods IS_ENQUEUED
    returning
      value(IS_ENQUEUED) type ABAP_BOOL .
  methods COPY_NODE
    importing
      !COPY_TARGET_ID type /GAL/CONFIG_KEY_ID
      !COPY_TARGET_PATH type STRING
      !NO_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      /GAL/CX_CONFIG_EXCEPTION .
protected section.
*"* protected components of class /GAL/CONFIG_NODE
*"* do not include other source files here!!!
private section.
*"* private components of class /GAL/CONFIG_NODE
*"* do not include other source files here!!!

  data STORE type ref to /GAL/CONFIG_STORE .
  data CONFIG_LOCK type ref to /GAL/CONFIG_LOCK .
  data ENQUEUED_CHILD_NODES type /GAL/CONFIG_NODES .

  methods CHECK_NODE_NAME
    importing
      !NAME type /GAL/CONFIG_KEY_NAME
    raising
      /GAL/CX_CONFIG_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !STORE type ref to /GAL/CONFIG_STORE
      !ID type /GAL/CONFIG_KEY_ID optional
      !PARENT type ref to /GAL/CONFIG_NODE optional
      !NAME type /GAL/CONFIG_KEY_NAME optional
    raising
      /GAL/CX_CONFIG_EXCEPTION .
ENDCLASS.



CLASS /GAL/CONFIG_NODE IMPLEMENTATION.


METHOD check_node_name.
  DATA l_name LIKE name.

  l_name = /gal/string=>trim( name ).

  IF l_name <> name OR l_name CA '/'.
    RAISE EXCEPTION TYPE /gal/cx_config_exception
          EXPORTING textid = /gal/cx_config_exception=>invalid_node_name
                    var1   = name.
  ENDIF.
ENDMETHOD.


METHOD constructor.
  DATA l_parent_id TYPE /gal/config_parent_key_id.
  DATA l_id        TYPE /gal/config_key_id.

  DATA l_variable1 TYPE string.
  DATA l_variable2 TYPE string.

* Initilize attributes
  me->store  = store.
  me->parent = parent.

* Read node data
  IF id IS NOT INITIAL.
    store->select_node( EXPORTING id               = id
                        IMPORTING parent_id        = l_parent_id
                                  name             = me->name
                                  type             = me->type
                                  fixed_value_type = me->fixed_value_type
                                  auth_class       = authenticator_class
                                  has_child_nodes  = is_parent ).

    me->id = id.

    IF parent IS INITIAL AND l_parent_id IS NOT INITIAL.
      RAISE EXCEPTION TYPE /gal/cx_config_exception
        EXPORTING
          textid = /gal/cx_config_exception=>parent_node_required.
    ELSEIF parent IS INITIAL.
      path = `/`.
    ELSEIF parent->path = `/`.
      CONCATENATE parent->path name INTO path.
    ELSE.
      CONCATENATE parent->path name INTO path SEPARATED BY `/`.
    ENDIF.
  ELSE.
    IF parent IS INITIAL.
      RAISE EXCEPTION TYPE /gal/cx_config_exception
        EXPORTING
          textid = /gal/cx_config_exception=>parent_node_required.
    ENDIF.

    check_node_name( name ).

    me->name = name.

    IF parent->path = `/`.
      CONCATENATE parent->path name INTO path.
    ELSE.
      CONCATENATE parent->path me->name INTO path SEPARATED BY `/`.
    ENDIF.

    l_id = store->get_node_id( path ).

    store->select_node( EXPORTING id               = l_id
                        IMPORTING parent_id        = l_parent_id
                                  name             = me->name
                                  type             = me->type
                                  fixed_value_type = me->fixed_value_type
                                  auth_class       = authenticator_class
                                  has_child_nodes  = is_parent ).

    me->id = l_id.
  ENDIF.

* Check parent object
  IF parent IS NOT INITIAL AND parent->id <> l_parent_id.
    l_variable1 = parent->id.
    l_variable2 = l_parent_id.

    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>invalid_parent_node
        var1   = l_variable1
        var2   = l_variable2.
  ENDIF.

* Initialize type flags
  IF type = const_node_type_folder.
    is_folder = abap_true.
    is_value  = abap_false.
  ELSE.
    is_folder = abap_false.
    is_value  = abap_true.
  ENDIF.
ENDMETHOD.


  METHOD copy_node.

    DATA l_target_node    TYPE REF TO /gal/config_node.
    DATA l_value_list     TYPE /gal/config_key_value_list.
    DATA l_upd_value_list TYPE /gal/config_key_value_list.
    DATA l_original_path  TYPE string.
    DATA l_replace_path   TYPE string.
    DATA l_exception      TYPE REF TO cx_root.
    DATA l_string_len     TYPE i.
    DATA l_message        TYPE string.

* Existence check
    store->select_node( copy_target_id ).

    l_target_node = store->get_node( path = copy_target_path ).

    IF l_target_node->is_folder = abap_false.
      RAISE EXCEPTION TYPE /gal/cx_config_exception
        EXPORTING
          textid = /gal/cx_config_exception=>no_child_nodes
          var1   = l_target_node->path.
    ENDIF.

    IF ( copy_target_id = parent->id ) OR ( copy_target_id = id ).
      RAISE EXCEPTION TYPE /gal/cx_config_exception
        EXPORTING
          textid = /gal/cx_config_exception=>parent_no_copy_target
          var1   = path.
    ENDIF.

    l_original_path = path.
    l_string_len = strlen( l_target_node->path ) - 1.
    IF l_string_len >= 0 AND l_target_node->path+l_string_len(1) = '/'.
      CONCATENATE l_target_node->path name INTO l_replace_path.
    ELSE.
      CONCATENATE l_target_node->path name INTO l_replace_path SEPARATED BY '/'.
    ENDIF.

* Get all node values including child node values
    l_value_list = store->get_node_values(
        with_child_nodes = abap_true
        id               = id
    ).                                                  "#EC CI_CONV_OK

    l_upd_value_list = l_value_list.                    "#EC CI_CONV_OK

* Update node names in value list according the target path
    store->update_subtree_node_names( EXPORTING parent_id     = parent->id
                                                new_parent_id = copy_target_id
                                                original_path = l_original_path
                                                replace_path  = l_replace_path
                                      CHANGING  value_list    = l_upd_value_list ).

    TRY.
* Insert the nodes to be copied as childs of the target node
        IF l_upd_value_list <> l_value_list.
          store->insert_subtree( EXPORTING parent_id  = copy_target_id
                                           value_list = l_upd_value_list
                                           no_commit  = abap_true ).
        ENDIF.
      CATCH /gal/cx_config_exception INTO l_exception.

        ROLLBACK WORK.                                 "#EC CI_ROLLBACK

        l_message = l_exception->get_text( ).
        RAISE EXCEPTION TYPE /gal/cx_config_exception
          EXPORTING
            textid = /gal/cx_config_exception=>custom_exception
            var1   = l_message.
    ENDTRY.

* Commit changes (if requested)
    IF no_commit <> abap_true.
      CALL FUNCTION 'DB_COMMIT'.
    ENDIF.

  ENDMETHOD.


METHOD create_child_node.
  DATA l_path TYPE string.
  DATA l_id   TYPE /gal/config_key_id.

  IF me->is_folder = abap_false.
    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>no_child_nodes
        var1   = path.
  ELSE.
    check_node_name( name ).
  ENDIF.

  IF path = `/`.
    CONCATENATE path name INTO l_path.
  ELSE.
    CONCATENATE path name INTO l_path SEPARATED BY `/`.
  ENDIF.

  l_id = store->get_node_id( l_path ).

  store->insert_node( id               = l_id
                      parent_id        = id
                      name             = name
                      type             = type
                      fixed_value_type = fixed_value_type ).

  CREATE OBJECT node
    EXPORTING
      store  = store
      parent = me
      name   = name.
ENDMETHOD.


METHOD delete.

  DATA l_exception TYPE REF TO cx_root.

  store->delete_node( id        = id
                      force     = force
                      no_commit = no_commit ).
  TRY.
      dequeue_node( ).
    CATCH /gal/cx_lock_exception INTO l_exception.
      /gal/trace=>write_exception( EXPORTING exception = l_exception ).
  ENDTRY.

ENDMETHOD.


METHOD delete_description.
  store->delete_node_description( id        = id
                                  language  = language
                                  no_commit = no_commit ).
ENDMETHOD.


METHOD delete_value.
  store->delete_node_value( id        = id
                            type      = type
                            default   = default
                            client    = client
                            user_name = user_name
                            no_commit = no_commit ).
ENDMETHOD.


METHOD dequeue_node.

  DATA l_tabix TYPE sytabix.

  FIELD-SYMBOLS <l_child_node> TYPE REF TO /gal/config_node.


* Dequeue child nodes if necessary
  LOOP AT enqueued_child_nodes ASSIGNING <l_child_node>.
    l_tabix = sy-tabix.
    <l_child_node>->dequeue_node( ).

    DELETE enqueued_child_nodes INDEX l_tabix.
  ENDLOOP.

  IF is_enqueued( ) = abap_true.
    config_lock->dequeue( ).
  ENDIF.

ENDMETHOD.


METHOD enqueue_node.

  DATA l_exception        TYPE REF TO cx_root.
  DATA l_inner_exception  TYPE REF TO cx_root.
  DATA l_config_exception TYPE REF TO /gal/cx_config_exception.
  DATA l_message          TYPE string.
  DATA l_remote_store     TYPE REF TO /gal/config_store_remote.
  DATA l_child_nodes      TYPE /gal/config_nodes.

  FIELD-SYMBOLS <l_child_node> TYPE REF TO /gal/config_node.

  IF is_enqueued( ) = abap_false.

    IF config_lock IS INITIAL.

      TRY.
          l_remote_store ?= store.

          CREATE OBJECT config_lock
            EXPORTING
              id             = me->id
              rfc_route_info = l_remote_store->rfc_route_info.

        CATCH cx_sy_move_cast_error.
          CREATE OBJECT config_lock
            EXPORTING
              id = me->id.
      ENDTRY.
    ENDIF.

    TRY.

* try to enqueue all child nodes if requested
        IF ( enqueue_child_nodes = abap_true ) AND ( is_folder = abap_true ).
          l_child_nodes = get_child_nodes( ).

          LOOP AT l_child_nodes ASSIGNING <l_child_node>.
            <l_child_node>->enqueue_node( EXPORTING enqueue_child_nodes = enqueue_child_nodes ).
            INSERT <l_child_node> INTO TABLE enqueued_child_nodes.
          ENDLOOP.
        ENDIF.

        config_lock->enqeueue( ).

      CATCH /gal/cx_lock_exception
            /gal/cx_config_exception INTO l_exception.

* In case of errors rollback any successful aquired locks
        TRY.
            IF ( enqueue_child_nodes = abap_true ) OR ( enqueued_child_nodes IS NOT INITIAL ).
              dequeue_node( ).
            ENDIF.
          CATCH /gal/cx_lock_exception INTO l_inner_exception.

            /gal/trace=>write_exception( EXPORTING exception = l_inner_exception ).

        ENDTRY.

        TRY.

* If a child node raised a lock error, just pass the prior message through
            l_config_exception ?= l_exception.
            l_message = l_config_exception->get_text( ).
            RAISE EXCEPTION TYPE /gal/cx_config_exception
              EXPORTING
                textid = /gal/cx_config_exception=>custom_exception
                var1   = l_message.
          CATCH cx_sy_move_cast_error.

* The current node could not been locked => raise a suitable error message
            l_message = l_exception->get_text( ).
            RAISE EXCEPTION TYPE /gal/cx_config_exception
              EXPORTING
                textid = /gal/cx_config_exception=>cannot_enqueue_node
                var1   = name
                var2   = l_message.
        ENDTRY.
    ENDTRY.
  ENDIF.

ENDMETHOD.


METHOD get_child_node.
  IF is_folder = abap_false.
    RAISE EXCEPTION TYPE /gal/cx_config_exception
          EXPORTING textid = /gal/cx_config_exception=>no_child_nodes
                    var1   = path.
  ENDIF.

  CREATE OBJECT node
    EXPORTING
      store  = store
      parent = me
      name   = name.
ENDMETHOD.


METHOD get_child_nodes.
  DATA l_nodes     TYPE STANDARD TABLE OF /gal/config_key.
  DATA l_node      TYPE REF TO /gal/config_node.
  DATA l_exception TYPE REF TO cx_root.
  DATA l_message   TYPE string.

  FIELD-SYMBOLS <l_nodes> LIKE LINE OF l_nodes.

  IF is_folder = abap_false.
    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>no_child_nodes
        var1   = path.
  ENDIF.

  l_nodes = store->select_child_nodes( id = id ).

  LOOP AT l_nodes ASSIGNING <l_nodes>.
    CHECK type IS NOT SUPPLIED OR <l_nodes>-type = type.

    TRY.
        l_node = get_child_node( name = <l_nodes>-name ).

        INSERT l_node INTO TABLE nodes.
      CATCH /gal/cx_config_exception INTO l_exception.

* In case of errors for a single child node just show the error
* whithout skipping valid child nodes
        l_message = l_exception->get_text( ).
        MESSAGE l_message TYPE 'S'.

    ENDTRY.
  ENDLOOP.

  SORT nodes BY table_line->type table_line->name.
ENDMETHOD.


METHOD get_description.
  description = store->select_node_description( id       = id
                                                language = language ).
ENDMETHOD.


METHOD get_value.
  DATA l_value_raw TYPE /gal/config_value.
  DATA l_value     TYPE REF TO data.
  DATA l_is_xml    TYPE abap_bool.

  FIELD-SYMBOLS <l_value> TYPE any.

  CLEAR type.
  CLEAR value.

* Get node value
  get_value_raw( EXPORTING client    = client
                           user_name = user_name
                 IMPORTING type      = type
                           value_raw = l_value_raw
                           is_xml    = l_is_xml ).

* Fallback to node default value
  IF type IS INITIAL.
    get_value_raw( EXPORTING default   = abap_true
                   IMPORTING type      = type
                             value_raw = l_value_raw
                             is_xml    = l_is_xml ).
  ENDIF.

* Fallback to parameter default value
  IF type IS INITIAL.
    IF default_value IS SUPPLIED.
      value = default_value.
      RETURN.
    ELSE.
      RAISE EXCEPTION TYPE /gal/cx_config_ex_no_value
        EXPORTING
          textid = /gal/cx_config_ex_no_value=>no_value_defined
          var1   = path.
    ENDIF.
  ENDIF.

* Deserialize and/or convert value
  CREATE DATA l_value TYPE (type).
  ASSIGN l_value->* TO <l_value>.

  IF l_is_xml = abap_true.
    TRY.
        CALL TRANSFORMATION id
             OPTIONS    value_handling = 'default'
             SOURCE XML l_value_raw
             RESULT     value = <l_value>.                  "#EC NOTEXT

      CATCH cx_root.                                     "#EC CATCH_ALL
        RAISE EXCEPTION TYPE /gal/cx_config_exception
          EXPORTING
            textid = /gal/cx_config_exception=>invalid_xml_for_type
            var1   = type
            var2   = path.

    ENDTRY.
  ELSE.
    TRY.
        <l_value> = l_value_raw.

      CATCH cx_root.                                     "#EC CATCH_ALL
        RAISE EXCEPTION TYPE /gal/cx_config_exception
          EXPORTING
            textid = /gal/cx_config_exception=>invalid_value_for_type
            var1   = type
            var2   = path.

    ENDTRY.
  ENDIF.

  TRY.
* If value is not requested, there is a possibility that runtime errors occur
      IF value IS SUPPLIED.
        value = <l_value>.
      ENDIF.

    CATCH cx_root.                                       "#EC CATCH_ALL
      RAISE EXCEPTION TYPE /gal/cx_config_exception
        EXPORTING
          textid = /gal/cx_config_exception=>type_conflict
          var1   = type
          var2   = path.
  ENDTRY.
ENDMETHOD.


METHOD get_value_raw.
  DATA l_type_descr TYPE REF TO cl_abap_typedescr.

  CLEAR type.
  CLEAR value_raw.
  CLEAR is_xml.

  IF is_folder = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>no_value
        var1   = path.
  ENDIF.

  store->select_node_value( EXPORTING type       = me->type
                                      id         = id
                                      default    = default
                                      client     = client
                                      user_name  = user_name
                            IMPORTING value_type = type
                                      value      = value_raw ).

  IF type IS INITIAL.
    RETURN.
  ENDIF.

  cl_abap_typedescr=>describe_by_name( EXPORTING  p_name      = type
                                       RECEIVING  p_descr_ref = l_type_descr
                                       EXCEPTIONS OTHERS      = 1 ).
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>unknown_type
        var1   = type.
  ENDIF.

  IF l_type_descr->type_kind = cl_abap_typedescr=>typekind_string OR
     l_type_descr->type_kind = cl_abap_typedescr=>typekind_num    OR
     l_type_descr->type_kind = cl_abap_typedescr=>typekind_date   OR
     l_type_descr->type_kind = cl_abap_typedescr=>typekind_packed OR
     l_type_descr->type_kind = cl_abap_typedescr=>typekind_time   OR
     l_type_descr->type_kind = cl_abap_typedescr=>typekind_char   OR
     l_type_descr->type_kind = cl_abap_typedescr=>typekind_hex    OR
     l_type_descr->type_kind = cl_abap_typedescr=>typekind_float  OR
     l_type_descr->type_kind = cl_abap_typedescr=>typekind_int.

    is_xml = abap_false.
  ELSE.
    is_xml = abap_true.
  ENDIF.

  type = l_type_descr->absolute_name.

  IF type CP `\TYPE=*`.
    type = type+6.
  ENDIF.
ENDMETHOD.


METHOD is_enqueued.

  is_enqueued = abap_false.

  IF config_lock IS NOT INITIAL.
    is_enqueued = config_lock->is_enqueued( ).
  ENDIF.

ENDMETHOD.


METHOD set_authenticator_class.
  store->update_node( id               = id
                      type             = type
                      fixed_value_type = fixed_value_type
                      auth_class       = authenticator_class
                      no_commit        = no_commit ).

  me->authenticator_class = authenticator_class.
ENDMETHOD.


METHOD set_description.
  store->update_node_description( id          = id
                                  language    = language
                                  description = description
                                  no_commit   = no_commit ).
ENDMETHOD.


METHOD set_fixed_value_type.
  store->update_node( id               = id
                      type             = type
                      fixed_value_type = fixed_value_type
                      auth_class       = authenticator_class
                      no_commit        = no_commit ).

  me->fixed_value_type = fixed_value_type.
ENDMETHOD.


METHOD set_name.


  IF parent IS INITIAL.
    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>parent_node_required.
  ENDIF.

  check_node_name( name ).

  store->update_node_name( EXPORTING id = id
                                     force = abap_true
                                     new_name = name
                                     no_commit = no_commit ).

ENDMETHOD.


METHOD set_type.
  store->update_node( id               = id
                      type             = type
                      fixed_value_type = fixed_value_type
                      auth_class       = authenticator_class
                      no_commit        = no_commit ).

  me->type = type.
ENDMETHOD.


METHOD set_value.
  DATA l_type_descr TYPE REF TO cl_abap_typedescr.
  DATA l_type       TYPE string.
  DATA l_value_raw  TYPE /gal/config_value.

  l_type_descr = cl_abap_typedescr=>describe_by_data( value ).

  l_type = l_type_descr->absolute_name.

  IF l_type_descr->type_kind = cl_abap_typedescr=>typekind_string OR
     l_type_descr->type_kind = cl_abap_typedescr=>typekind_num    OR
     l_type_descr->type_kind = cl_abap_typedescr=>typekind_date   OR
     l_type_descr->type_kind = cl_abap_typedescr=>typekind_packed OR
     l_type_descr->type_kind = cl_abap_typedescr=>typekind_time   OR
     l_type_descr->type_kind = cl_abap_typedescr=>typekind_char   OR
     l_type_descr->type_kind = cl_abap_typedescr=>typekind_hex    OR
     l_type_descr->type_kind = cl_abap_typedescr=>typekind_float  OR
     l_type_descr->type_kind = cl_abap_typedescr=>typekind_int.

    l_value_raw = value.
  ELSE.
    TRY.
        CALL TRANSFORMATION id
             OPTIONS    data_refs          = 'heap-or-create'
                        initial_components = 'include'
                        technical_types    = 'error'
                        value_handling     = 'default'
                        xml_header         = 'full'
             SOURCE     value              = value
             RESULT XML l_value_raw.                        "#EC NOTEXT

      CATCH cx_root.                                     "#EC CATCH_ALL
        RAISE EXCEPTION TYPE /gal/cx_config_exception
          EXPORTING
            textid = /gal/cx_config_exception=>cannot_serialize_type
            var1   = l_type.

    ENDTRY.
  ENDIF.

* Write raw value to configuration store
  CASE me->type.

    WHEN const_node_type_value_client.
      set_value_raw( client    = client
                     user_name = ''
                     type      = l_type
                     value_raw = l_value_raw
                     no_commit = no_commit ).

    WHEN const_node_type_value_system.
      set_value_raw( client    = 'ALL'
                     user_name = ''
                     type      = l_type
                     value_raw = l_value_raw
                     no_commit = no_commit ).

    WHEN const_node_type_value_user.
      set_value_raw( client    = client
                     user_name = user_name
                     type      = l_type
                     value_raw = l_value_raw
                     no_commit = no_commit ).

  ENDCASE.
ENDMETHOD.


METHOD set_value_raw.
  DATA l_type_descr TYPE REF TO cl_abap_typedescr.
  DATA l_type       TYPE /gal/config_value_type.
  DATA l_value      TYPE REF TO data.

  DATA l_exception  TYPE REF TO cx_root.
  DATA l_message    TYPE string.

  FIELD-SYMBOLS <l_value> TYPE any.

  IF is_folder = abap_true.
    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>no_value
        var1   = path.
  ENDIF.

  cl_abap_typedescr=>describe_by_name( EXPORTING  p_name      = type
                                       RECEIVING  p_descr_ref = l_type_descr
                                       EXCEPTIONS OTHERS      = 1 ).
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>unknown_type
        var1   = type.
  ENDIF.

  l_type = l_type_descr->absolute_name.

  TRY.
      CREATE DATA l_value TYPE (type).

    CATCH cx_root INTO l_exception.                      "#EC CATCH_ALL
      l_message = l_exception->get_text( ).

      RAISE EXCEPTION TYPE /gal/cx_config_exception
        EXPORTING
          textid = /gal/cx_config_exception=>custom_exception
          var1   = l_message.

  ENDTRY.

  ASSIGN l_value->* TO <l_value>.

  IF l_type_descr->type_kind <> cl_abap_typedescr=>typekind_string AND
     l_type_descr->type_kind <> cl_abap_typedescr=>typekind_num    AND
     l_type_descr->type_kind <> cl_abap_typedescr=>typekind_date   AND
     l_type_descr->type_kind <> cl_abap_typedescr=>typekind_packed AND
     l_type_descr->type_kind <> cl_abap_typedescr=>typekind_time   AND
     l_type_descr->type_kind <> cl_abap_typedescr=>typekind_char   AND
     l_type_descr->type_kind <> cl_abap_typedescr=>typekind_hex    AND
     l_type_descr->type_kind <> cl_abap_typedescr=>typekind_float  AND
     l_type_descr->type_kind <> cl_abap_typedescr=>typekind_int.

    TRY.
        CALL TRANSFORMATION id
             OPTIONS    value_handling = 'default'
             SOURCE XML value_raw
             RESULT     value = <l_value>.                  "#EC NOTEXT

      CATCH cx_root.                                     "#EC CATCH_ALL
        RAISE EXCEPTION TYPE /gal/cx_config_exception
          EXPORTING
            textid = /gal/cx_config_exception=>invalid_xml_for_type
            var1   = l_type
            var2   = path.

    ENDTRY.
  ELSE.
    TRY.
        <l_value> = value_raw.

      CATCH cx_root.                                     "#EC CATCH_ALL
        RAISE EXCEPTION TYPE /gal/cx_config_exception
          EXPORTING
            textid = /gal/cx_config_exception=>invalid_value_for_type
            var1   = l_type
            var2   = path.

    ENDTRY.
  ENDIF.

  IF l_type CP `\TYPE=*`.
    l_type = l_type+6.
  ENDIF.

  store->update_node_value( id         = id
                            type       = me->type
                            default    = default
                            client     = client
                            user_name  = user_name
                            value_type = l_type
                            value      = value_raw ).
ENDMETHOD.
ENDCLASS.
