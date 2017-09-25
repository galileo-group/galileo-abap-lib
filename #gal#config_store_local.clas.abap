class /GAL/CONFIG_STORE_LOCAL definition
  public
  inheriting from /GAL/CONFIG_STORE
  final
  create public

  global friends /GAL/CONFIG_NODE .

*"* public components of class /GAL/CONFIG_STORE_LOCAL
*"* do not include other source files here!!!
public section.
  type-pools ABAP .

  methods CONSTRUCTOR .

  methods AUTHORITY_CHECK
    redefinition .
protected section.
*"* protected components of class /GAL/CONFIG_STORE_LOCAL
*"* do not include other source files here!!!

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
*"* private components of class /GAL/CONFIG_STORE_LOCAL
*"* do not include other source files here!!!
ENDCLASS.



CLASS /GAL/CONFIG_STORE_LOCAL IMPLEMENTATION.


METHOD authority_check.
  DATA l_node          TYPE REF TO /gal/config_node.
  DATA l_authenticator TYPE REF TO /gal/config_node_authenticator.

* Determine authenticator class and create instance
  l_node = node.

  WHILE l_node IS NOT INITIAL AND l_node->authenticator_class IS INITIAL.
    l_node = l_node->parent.
  ENDWHILE.

  IF l_node IS NOT INITIAL.
    TRY.
        CREATE OBJECT l_authenticator TYPE (l_node->authenticator_class).

      CATCH cx_root.                                        "#EC *
        " Nothing needs to be done here

    ENDTRY.
  ENDIF.

  IF l_authenticator IS INITIAL.
    CREATE OBJECT l_authenticator TYPE /gal/config_node_auth_default.
  ENDIF.

* Perform authority check
  l_authenticator->authority_check( node      = node
                                    action    = action
                                    client    = client
                                    user_name = user_name ).
ENDMETHOD.


METHOD constructor.
  DATA l_root_id TYPE /gal/config_key_id.

  super->constructor( ).

  l_root_id = get_node_id( `/` ).

  TRY.
      CREATE OBJECT root
        EXPORTING
          store = me
          id    = l_root_id.

    CATCH /gal/cx_config_exception.                     "#EC NO_HANDLER
      " Cannot occur during instantiation of root node

  ENDTRY.
ENDMETHOD.


METHOD delete_node.
  DATA l_child_nodes TYPE /gal/config_keys.
  DATA l_path        TYPE string.

  FIELD-SYMBOLS <l_child_nodes> LIKE LINE OF l_child_nodes.

* Select node (existence check)
  select_node( id ).

* Select existing child nodes
  l_child_nodes = select_child_nodes( id ).

* Check if node has child nodes
  IF force <> abap_true AND l_child_nodes IS NOT INITIAL.
    l_path = get_node_path( id ).

    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>child_nodes_exist
        var1   = l_path.
  ENDIF.

* Delete child nodes
  LOOP AT l_child_nodes ASSIGNING <l_child_nodes>.
    delete_node( id        = <l_child_nodes>-id
                 force     = abap_true
                 no_commit = abap_true ).
  ENDLOOP.

* Delete node
  DELETE FROM /gal/config_key
        WHERE id = id.                                    "#EC CI_SUBRC

  DELETE FROM /gal/config_txt
        WHERE id = id.                                    "#EC CI_SUBRC

  DELETE FROM /gal/config_val
        WHERE id = id.                                    "#EC CI_SUBRC

  DELETE FROM /gal/config_cval CLIENT SPECIFIED
        WHERE id = id.                                    "#EC CI_SUBRC
                                                         "#EC CI_CLIENT
                                                        "#EC CI_NOFIRST

  DELETE FROM /gal/config_sval
        WHERE id = id.                                    "#EC CI_SUBRC

  DELETE FROM /gal/config_uval CLIENT SPECIFIED
        WHERE id = id.                                    "#EC CI_SUBRC
                                                         "#EC CI_CLIENT
                                                        "#EC CI_NOFIRST

  IF no_commit <> abap_true.
    CALL FUNCTION 'DB_COMMIT'.
  ENDIF.
ENDMETHOD.


METHOD delete_node_description.

* Select node (existence check)
  select_node( id ).

* Delete node description
  IF language IS INITIAL OR language = '*'.
    DELETE FROM /gal/config_txt
          WHERE id = id.                                  "#EC CI_SUBRC
  ELSE.
    DELETE FROM /gal/config_txt
          WHERE id    = id
            AND langu = language.                         "#EC CI_SUBRC
  ENDIF.

  IF no_commit <> abap_true.
    CALL FUNCTION 'DB_COMMIT'.
  ENDIF.
ENDMETHOD.


METHOD delete_node_value.

* Select node (existence check)
  select_node( id ).

* Delete node value(s)
  IF default = abap_true.
    DELETE FROM /gal/config_val
          WHERE id = id.                                  "#EC CI_SUBRC
  ELSE.
    CASE type.

      WHEN /gal/config_node=>const_node_type_value_client.
        DELETE FROM /gal/config_cval CLIENT SPECIFIED
              WHERE client = client                      "#EC CI_CLIENT
                AND id     = id.                          "#EC CI_SUBRC

      WHEN /gal/config_node=>const_node_type_value_system.
        DELETE FROM /gal/config_sval
              WHERE id     = id.                          "#EC CI_SUBRC

      WHEN /gal/config_node=>const_node_type_value_user.
        DELETE FROM /gal/config_uval CLIENT SPECIFIED
              WHERE client    = client
                AND user_name = user_name                "#EC CI_CLIENT
                AND id        = id.                       "#EC CI_SUBRC

    ENDCASE.
  ENDIF.

  IF no_commit <> abap_true.
    CALL FUNCTION 'DB_COMMIT'.
  ENDIF.
ENDMETHOD.


METHOD get_node_path.
  DATA l_id        TYPE /gal/config_key_id.
  DATA l_parent_id TYPE /gal/config_parent_key_id.
  DATA l_name      TYPE /gal/config_key_name.

  DATA l_ids       TYPE HASHED TABLE OF /gal/config_key_id
                   WITH UNIQUE KEY table_line.

  DATA l_var       TYPE string.

  DATA l_exception TYPE REF TO /gal/cx_config_exception.

* Build path
  l_id = id.

  WHILE l_id IS NOT INITIAL.
    TRY.
        select_node( EXPORTING id        = l_id
                     IMPORTING parent_id = l_parent_id
                               name      = l_name ).

      CATCH /gal/cx_config_exception INTO l_exception.
        IF l_id = id.
          RAISE EXCEPTION l_exception.
        ELSE.
          EXIT.
        ENDIF.
    ENDTRY.

    IF l_parent_id IS INITIAL.
      EXIT.
    ENDIF.

    READ TABLE l_ids
          WITH TABLE KEY table_line = l_id
               TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.

*********************************************************************
*
* !!! Configuration store currupt !!!
*
* If this happens the requested node of the configuration store
* is nor a child of the root node. Its liks to the parent nodes
* for a circular relationship that would cause this program to
* run in an endless loop until there is no more memory to extend
* table l_ids.
*
* To resolve this set a break-point at the message statement and
* check the contents of the internal table l_ids. Then remove all
* entries from all tables beginning with /GAL/CONFIG... that have
* one of the values from the internal table l_ids sotred in the
* field ID.
*
*********************************************************************

      MESSAGE text-x00 TYPE 'X'.
    ELSE.
      INSERT l_id INTO TABLE l_ids.
    ENDIF.

*    CONCATENATE l_name path INTO path SEPARATED BY `/`.
    CONCATENATE '/' l_name path INTO path.

    l_id = l_parent_id.
  ENDWHILE.

* Check node hierarchy
  IF l_id <> root->id.
    l_var = id.

    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>node_not_connected_to_root
        var1   = l_var.
  ENDIF.

* Check consistency
  l_id = get_node_id( path ).

  IF l_id <> id.
    l_var = id.

    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>node_id_and_path_do_not_match
        var1   = l_var
        var2   = path.
  ENDIF.
ENDMETHOD.


  METHOD get_node_values.

    DATA l_node_values TYPE /gal/config_key_values.
    DATA l_node        TYPE REF TO /gal/config_node.
    DATA l_child_nodes TYPE /gal/config_keys.
    DATA l_path        TYPE string.
    DATA l_value_list  TYPE /gal/config_key_value_list.

    FIELD-SYMBOLS <l_child_node> TYPE /gal/config_key.

* Existence check
    select_node( id ).
    l_path = get_node_path( id ).
    l_node = get_node( path = l_path ).

* Collect all node values
    CLEAR l_node_values.
    l_node_values-id = l_node->id.
    l_node_values-node = l_node.
    l_node_values-parent_id = l_node->parent->id.
    l_node_values-name = l_node->name.
    l_node_values-type = l_node->type.
    l_node_values-fixed_value_type = l_node->fixed_value_type.
    l_node_values-auth_class = l_node->authenticator_class.

    SELECT  SINGLE id type value
      FROM  /gal/config_val
      INTO  l_node_values-value_default
      WHERE id = l_node->id.                              "#EC CI_SUBRC

    SELECT  client id type value
      FROM  /gal/config_cval CLIENT SPECIFIED
      INTO  TABLE l_node_values-values_client_dependent
      WHERE id = l_node->id.                             "#EC CI_CLIENT
                                                          "#EC CI_SUBRC
                                                     "#EC CI_BUFFCLIENT

    SELECT  SINGLE id type value
      FROM  /gal/config_sval
      INTO  l_node_values-value_cross_client
      WHERE id = l_node->id.                              "#EC CI_SUBRC

    SELECT  client id user_name type value
      FROM  /gal/config_uval CLIENT SPECIFIED
      INTO  TABLE l_node_values-values_user_dependent
      WHERE id = l_node->id.                              "#EC CI_SUBRC
                                                     "#EC CI_BUFFCLIENT
                                                         "#EC CI_CLIENT
                                                        "#EC CI_GENBUFF

    SELECT  langu text
      FROM  /gal/config_txt
      INTO  TABLE l_node_values-texts
      WHERE id = l_node->id.                              "#EC CI_SUBRC
                                                        "#EC CI_GENBUFF

* If values from child nodes are requested, collect them recursively
    IF with_child_nodes = abap_true.
      SELECT * FROM /gal/config_key
        INTO TABLE l_child_nodes
        WHERE parent_id = l_node->id.                     "#EC CI_SUBRC

      LOOP AT l_child_nodes ASSIGNING <l_child_node>.

        CLEAR l_value_list.
        l_value_list = get_node_values(
            id               = <l_child_node>-id
            with_child_nodes = with_child_nodes ).      "#EC CI_CONV_OK
        APPEND LINES OF l_value_list TO value_list.     "#EC CI_CONV_OK
      ENDLOOP.

    ELSE.
* Also if no values from child nodes are requested, check if any are present
      SELECT * FROM /gal/config_key
        INTO TABLE l_child_nodes UP TO 1 ROWS
        WHERE parent_id = l_node->id.                     "#EC CI_SUBRC
    ENDIF.

    IF l_child_nodes IS NOT INITIAL.
      l_node_values-child_node_exists = abap_true.
    ELSE.
      l_node_values-child_node_exists = abap_false.
    ENDIF.

* Insert node values to return list
    INSERT l_node_values INTO TABLE value_list.

  ENDMETHOD.


METHOD insert_node.
  DATA l_wa_config_key TYPE /gal/config_key.
  DATA l_path          TYPE string.

  l_wa_config_key-id               = id.
  l_wa_config_key-parent_id        = parent_id.
  l_wa_config_key-name             = name.
  l_wa_config_key-type             = type.
  l_wa_config_key-fixed_value_type = fixed_value_type.
  l_wa_config_key-auth_class       = auth_class.

  INSERT /gal/config_key FROM l_wa_config_key.
  IF sy-subrc <> 0.
    l_path = get_node_path( id ).

    RAISE EXCEPTION TYPE /gal/cx_config_exception
          EXPORTING textid = /gal/cx_config_exception=>node_already_exists
                    var1   = l_path.
  ENDIF.

  IF no_commit <> abap_true.
    CALL FUNCTION 'DB_COMMIT'.
  ENDIF.
ENDMETHOD.


  METHOD insert_subtree.

    FIELD-SYMBOLS <l_node_values>  TYPE /gal/config_key_values.
    FIELD-SYMBOLS <l_config_cval>  TYPE /gal/config_cval.
    FIELD-SYMBOLS <l_config_uval>  TYPE /gal/config_uval.
    FIELD-SYMBOLS <l_config_txt>   TYPE /gal/config_key_text_ext.


    LOOP AT value_list ASSIGNING <l_node_values>
      WHERE parent_id = parent_id.

* Insert node with new id and new name
      insert_node( id               = <l_node_values>-id
                   parent_id        = <l_node_values>-parent_id
                   name             = <l_node_values>-name
                   type             = <l_node_values>-type
                   fixed_value_type = <l_node_values>-fixed_value_type
                   auth_class       = <l_node_values>-auth_class
                   no_commit        = no_commit ).

* Update default value
      IF <l_node_values>-value_default IS NOT INITIAL.
        update_node_value( id         = <l_node_values>-id
                           type       = <l_node_values>-type
                           default    = abap_true
                           value_type = <l_node_values>-value_default-type
                           value      = <l_node_values>-value_default-value
                           no_commit  = no_commit ).
      ENDIF.

* Update system value
      IF <l_node_values>-value_cross_client IS NOT INITIAL.
        update_node_value( id         = <l_node_values>-id
                           type       = <l_node_values>-type
                           value_type = <l_node_values>-value_cross_client-type
                           value      = <l_node_values>-value_cross_client-value
                           no_commit  = no_commit ).
      ENDIF.

* Update client dependent values
      LOOP AT <l_node_values>-values_client_dependent ASSIGNING <l_config_cval>.
        update_node_value( id         = <l_node_values>-id
                           type       = <l_node_values>-type
                           client     = <l_config_cval>-client
                           value_type = <l_config_cval>-type
                           value      = <l_config_cval>-value
                           no_commit  = no_commit ).
      ENDLOOP.

* Update user dependent values
      LOOP AT <l_node_values>-values_user_dependent ASSIGNING <l_config_uval>.
        update_node_value( id         = <l_node_values>-id
                           type       = <l_node_values>-type
                           client     = <l_config_uval>-client
                           user_name  = <l_config_uval>-user_name
                           value_type = <l_config_uval>-type
                           value      = <l_config_uval>-value
                           no_commit  = no_commit ).
      ENDLOOP.

* Update descriptions
      LOOP AT <l_node_values>-texts ASSIGNING <l_config_txt>.
        update_node_description( id          = <l_node_values>-id
                                 language    = <l_config_txt>-langu
                                 description = <l_config_txt>-text
                                 no_commit   = no_commit ).
      ENDLOOP.


      IF <l_node_values>-child_node_exists = abap_true.
        insert_subtree( EXPORTING parent_id  = <l_node_values>-id
                                  value_list = value_list
                                  no_commit  = no_commit ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


METHOD select_child_nodes.

* Select node (existence check)
  select_node( id ).

* Select child nodes
  SELECT *
    FROM /gal/config_key
    INTO TABLE data
   WHERE parent_id = id.                                  "#EC CI_SUBRC
ENDMETHOD.


METHOD select_node.
  DATA l_dummy TYPE /gal/config_key_id.                     "#EC NEEDED
  DATA l_id    TYPE string.

  CLEAR parent_id.
  CLEAR name.
  CLEAR type.

  SELECT SINGLE parent_id
                name
                type
                fixed_value_type
                auth_class
           FROM /gal/config_key
           INTO (parent_id, name, type, fixed_value_type, auth_class)
          WHERE id = id.
  IF sy-subrc <> 0.
    IF id = get_node_id( `/` ).
      name = `Root`.                                        "#EC NOTEXT
      type = /gal/config_node=>const_node_type_folder.

      insert_node( id        = id
                   parent_id = parent_id
                   name      = name
                   type      = type ).
    ELSE.
      l_id = id.

      RAISE EXCEPTION TYPE /gal/cx_config_exception
        EXPORTING
          textid = /gal/cx_config_exception=>node_does_not_exist
          var1   = l_id.
    ENDIF.
  ENDIF.

  IF has_child_nodes IS REQUESTED.
    IF type = /gal/config_node=>const_node_type_folder.
      SELECT id UP TO 1 ROWS
        INTO l_dummy
        FROM /gal/config_key
       WHERE parent_id = id.
        EXIT.
      ENDSELECT.

      IF sy-subrc = 0.
        has_child_nodes = abap_true.
      ELSE.
        has_child_nodes = abap_false.
      ENDIF.
    ELSE.
      has_child_nodes = abap_false.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD select_node_description.

* Select node (existence check)
  select_node( id ).

* Select node description
  SELECT SINGLE text
           FROM /gal/config_txt
           INTO description
          WHERE id    = id
            AND langu = language.                         "#EC CI_SUBRC
ENDMETHOD.


METHOD select_node_value.

* Select node (existence check)
  select_node( id ).

* Select requested value
  CLEAR value_type.
  CLEAR value.

  IF default = abap_true.
    SELECT SINGLE type value
             FROM /gal/config_val
             INTO (value_type, value)
            WHERE id = id.                                "#EC CI_SUBRC
  ELSE.
    CASE type.

      WHEN /gal/config_node=>const_node_type_value_client.
        SELECT SINGLE type value
                 FROM /gal/config_cval CLIENT SPECIFIED
                 INTO (value_type, value)
                WHERE client = client                    "#EC CI_CLIENT
                  AND id     = id.                        "#EC CI_SUBRC

      WHEN /gal/config_node=>const_node_type_value_system.
        SELECT SINGLE type value
                 FROM /gal/config_sval
                 INTO (value_type, value)
                WHERE id = id.                            "#EC CI_SUBRC

      WHEN /gal/config_node=>const_node_type_value_user.
        SELECT SINGLE type value
                 FROM /gal/config_uval CLIENT SPECIFIED
                 INTO (value_type, value)
                WHERE client    = client
                  AND user_name = user_name              "#EC CI_CLIENT
                  AND id        = id.                     "#EC CI_SUBRC

    ENDCASE.
  ENDIF.
ENDMETHOD.


METHOD update_node.
  DATA l_type TYPE /gal/config_key_type.

* Select node (existence check)
  select_node( EXPORTING id   = id
               IMPORTING type = l_type ).

* Delete existing values if type is changed
  IF type <> l_type.
    DELETE FROM /gal/config_cval CLIENT SPECIFIED
          WHERE id = id.                                  "#EC CI_SUBRC
                                                        "#EC CI_NOFIRST
                                                         "#EC CI_CLIENT

    DELETE FROM /gal/config_sval
          WHERE id = id.                                  "#EC CI_SUBRC

    DELETE FROM /gal/config_uval CLIENT SPECIFIED
          WHERE id = id.                                  "#EC CI_SUBRC
                                                         "#EC CI_CLIENT
                                                        "#EC CI_NOFIRST
  ENDIF.

* Update node definition
  UPDATE /gal/config_key SET type             = type
                             fixed_value_type = fixed_value_type
                             auth_class       = auth_class
                       WHERE id               = id.       "#EC CI_SUBRC

* Commit changes (if requeste)
  IF no_commit <> abap_true.
    CALL FUNCTION 'DB_COMMIT'.
  ENDIF.
ENDMETHOD.


METHOD update_node_description.
  DATA l_wa_config_txt TYPE /gal/config_txt.

* Select node (existence check)
  select_node( id ).

* Update node description
  l_wa_config_txt-id    = id.
  l_wa_config_txt-langu = language.
  l_wa_config_txt-text  = description.

  MODIFY /gal/config_txt FROM l_wa_config_txt.            "#EC CI_SUBRC

  IF no_commit <> abap_true.
    CALL FUNCTION 'DB_COMMIT'.
  ENDIF.
ENDMETHOD.


METHOD update_node_name.

  DATA l_node            TYPE REF TO /gal/config_node.
  DATA l_value_list      TYPE /gal/config_key_value_list.
  DATA l_upd_value_list  TYPE /gal/config_key_value_list.
  DATA l_original_path   TYPE string.
  DATA l_replace_path    TYPE string.
  DATA l_path_elements   TYPE STANDARD TABLE OF string.
  DATA l_lines           TYPE i.
  DATA l_message         TYPE string.
  DATA l_exception       TYPE REF TO /gal/cx_config_exception.

  FIELD-SYMBOLS <l_node_values>  TYPE /gal/config_key_values.
  FIELD-SYMBOLS <l_path_element> TYPE string.

* Existence check
  select_node( id ).

  l_original_path = get_node_path( id ).
  l_node = get_node( path = l_original_path ).

  l_value_list = get_node_values(
      id               = id
      with_child_nodes = force ).                       "#EC CI_CONV_OK
  l_upd_value_list = l_value_list.                      "#EC CI_CONV_OK

  CLEAR l_replace_path.
  READ TABLE l_upd_value_list ASSIGNING <l_node_values>
    WITH KEY id = l_node->id.
  IF sy-subrc IS INITIAL.

    IF ( force = abap_false ) AND ( <l_node_values>-child_node_exists = abap_true ).
      RAISE EXCEPTION TYPE /gal/cx_config_exception
        EXPORTING
          textid = /gal/cx_config_exception=>node_cannot_renamed
          var1   = l_original_path.
    ENDIF.

    <l_node_values>-name = new_name.

    SPLIT l_original_path AT '/' INTO TABLE l_path_elements.
    DESCRIBE TABLE l_path_elements LINES l_lines.
    IF l_lines > 1.
      READ TABLE l_path_elements INDEX l_lines ASSIGNING <l_path_element>.
      IF sy-subrc IS INITIAL.
        <l_path_element> = new_name.
      ENDIF.

      LOOP AT l_path_elements ASSIGNING <l_path_element>.
        IF <l_path_element> IS NOT INITIAL.
          IF l_replace_path <> '/'.
            CONCATENATE l_replace_path <l_path_element> INTO l_replace_path SEPARATED BY '/'.
          ELSE.
            CONCATENATE l_replace_path <l_path_element> INTO l_replace_path.
          ENDIF.
        ELSE.
          l_replace_path = '/'.
        ENDIF.
      ENDLOOP.

      IF ( l_original_path IS NOT INITIAL ) AND ( l_replace_path IS NOT INITIAL ).
        update_subtree_node_names( EXPORTING parent_id     = l_node->parent->id
                                             new_parent_id = space
                                             original_path = l_original_path
                                             replace_path  = l_replace_path
                                   CHANGING  value_list    = l_upd_value_list ).
      ENDIF.
    ENDIF.
  ELSE.
* This statement should never been reached
    l_message = TEXT-x00.
    RAISE EXCEPTION TYPE /gal/cx_config_exception
      EXPORTING
        textid = /gal/cx_config_exception=>custom_exception
        var1   = l_message.
  ENDIF.

  TRY.

      IF l_upd_value_list <> l_value_list.
        delete_node( EXPORTING id        = id
                               force     = force
                               no_commit = abap_true ).

        insert_subtree( EXPORTING parent_id  = l_node->parent->id
                                  value_list = l_upd_value_list
                                  no_commit  = abap_true ).
      ENDIF.


    CATCH /gal/cx_config_exception INTO l_exception.
* Rollback work
      ROLLBACK WORK.                                   "#EC CI_ROLLBACK

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


METHOD update_node_value.
  DATA l_wa_config_val  TYPE /gal/config_val.
  DATA l_wa_config_cval TYPE /gal/config_cval.
  DATA l_wa_config_sval TYPE /gal/config_sval.
  DATA l_wa_config_uval TYPE /gal/config_uval.

* Select node (existence check)
  select_node( id ).

* Update node value
  IF default = abap_true.
    l_wa_config_val-id    = id.
    l_wa_config_val-type  = value_type.
    l_wa_config_val-value = value.
    MODIFY /gal/config_val FROM l_wa_config_val.          "#EC CI_SUBRC
  ELSE.
    CASE type.

      WHEN /gal/config_node=>const_node_type_value_client.
        l_wa_config_cval-client = client.
        l_wa_config_cval-id     = id.
        l_wa_config_cval-type   = value_type.
        l_wa_config_cval-value  = value.
        MODIFY /gal/config_cval CLIENT SPECIFIED         "#EC CI_CLIENT
          FROM l_wa_config_cval.                          "#EC CI_SUBRC

      WHEN /gal/config_node=>const_node_type_value_system.
        l_wa_config_sval-id    = id.
        l_wa_config_sval-type  = value_type.
        l_wa_config_sval-value = value.
        MODIFY /gal/config_sval FROM l_wa_config_sval.    "#EC CI_SUBRC

      WHEN /gal/config_node=>const_node_type_value_user.
        l_wa_config_uval-client    = client.
        l_wa_config_uval-user_name = user_name.
        l_wa_config_uval-id        = id.
        l_wa_config_uval-type      = value_type.
        l_wa_config_uval-value     = value.
        MODIFY /gal/config_uval CLIENT SPECIFIED         "#EC CI_CLIENT
          FROM l_wa_config_uval.                          "#EC CI_SUBRC

    ENDCASE.
  ENDIF.

  IF no_commit <> abap_true.
    CALL FUNCTION 'DB_COMMIT'.
  ENDIF.
ENDMETHOD.


  METHOD update_subtree_node_names.

    DATA l_update_path TYPE string.
    DATA l_id          TYPE /gal/config_key_id.

    FIELD-SYMBOLS <l_node_values> TYPE /gal/config_key_values.

    LOOP AT value_list ASSIGNING <l_node_values>
      WHERE parent_id = parent_id.

      l_id = <l_node_values>-id.
      l_update_path = <l_node_values>-node->path.
      REPLACE FIRST OCCURRENCE OF original_path IN l_update_path WITH replace_path.

      <l_node_values>-id = get_node_id( path = l_update_path ).

      IF new_parent_id IS NOT INITIAL.
        <l_node_values>-parent_id = new_parent_id.
      ENDIF.

      update_subtree_node_names( EXPORTING parent_id     = l_id
                                           new_parent_id = <l_node_values>-id
                                           original_path = original_path
                                           replace_path  = replace_path
                                 CHANGING  value_list    = value_list ).

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
