class /GAL/DROPDOWN_HELPER definition
  public
  create public .

*"* public components of class /GAL/DROPDOWN_HELPER
*"* do not include other source files here!!!
public section.
  type-pools ABAP .
  type-pools VRM .

  class-methods INIT_BY_FIELD
    importing
      !FIELD_NAME type STRING
      !FIELD type ANY optional
      !SORT_BY_KEY type ABAP_BOOL optional
      !SORT_BY_VALUE type ABAP_BOOL optional
      !LANGUAGE_VECTOR type STRING optional
      !SET_DEFAULT_VALUE type ABAP_BOOL optional
    returning
      value(DROPDOWN_VALUES) type VRM_VALUES
    raising
      /GAL/CX_DD_HELPER_EXCEPTION .
  class-methods INIT_BY_DATA_ELEMENT
    importing
      !FIELD_NAME type STRING
      !DATA_ELEMENT type ROLLNAME
      !SORT_BY_KEY type ABAP_BOOL optional
      !SORT_BY_VALUE type ABAP_BOOL optional
      !LANGUAGE_VECTOR type STRING optional
      !SET_DEFAULT_VALUE type ABAP_BOOL optional
    returning
      value(DROPDOWN_VALUES) type VRM_VALUES
    raising
      /GAL/CX_DD_HELPER_EXCEPTION .
  class-methods INIT_BY_DOMAIN
    importing
      !FIELD_NAME type STRING
      !DOMAIN type DOMNAME
      !SORT_BY_KEY type ABAP_BOOL optional
      !SORT_BY_VALUE type ABAP_BOOL optional
      !LANGUAGE_VECTOR type STRING optional
      !SET_DEFAULT_VALUE type ABAP_BOOL optional
    returning
      value(DROPDOWN_VALUES) type VRM_VALUES
    raising
      /GAL/CX_DD_HELPER_EXCEPTION .
  class-methods INIT_BY_VALUE_TABLE
    importing
      !FIELD_NAME type STRING
      !VALUE_TABLE type ANY TABLE
      !KEY_FIELD type STRING optional
      !VALUE_FIELD type STRING optional
      !LANGUAGE_FIELD type STRING optional
      !TEXT_FIELD type STRING optional
      !SORT_BY_KEY type ABAP_BOOL optional
      !SORT_BY_VALUE type ABAP_BOOL optional
      !LANGUAGE_VECTOR type STRING optional
      !SET_DEFAULT_VALUE type ABAP_BOOL optional
    returning
      value(DROPDOWN_VALUES) type VRM_VALUES
    raising
      /GAL/CX_DD_HELPER_EXCEPTION .
protected section.
*"* protected components of class /GAL/DROPDOWN_HELPER
*"* do not include other source files here!!!
private section.
*"* private components of class /GAL/DROPDOWN_HELPER
*"* do not include other source files here!!!
ENDCLASS.



CLASS /GAL/DROPDOWN_HELPER IMPLEMENTATION.


METHOD init_by_data_element.
  DATA l_domain    TYPE domname.
  DATA l_variable1 TYPE string.

* Determind domain
  SELECT SINGLE domname
           FROM dd04l
           INTO l_domain
          WHERE rollname = data_element
            AND as4local = 'A'
            AND as4vers  = '0000'.
  IF NOT sy-subrc = 0.
    l_variable1 = data_element.

    RAISE EXCEPTION TYPE /gal/cx_dd_helper_exception
      EXPORTING
        textid = /gal/cx_dd_helper_exception=>cannot_determine_domain
        var1   = l_variable1.
  ENDIF.

* Initialize dropdown field
  IF NOT dropdown_values IS REQUESTED.
    init_by_domain( field_name        = field_name
                    domain            = l_domain
                    sort_by_key       = sort_by_key
                    sort_by_value     = sort_by_value
                    language_vector   = language_vector
                    set_default_value = set_default_value ).
  ELSE.
    dropdown_values = init_by_domain( field_name        = field_name
                                      domain            = l_domain
                                      sort_by_key       = sort_by_key
                                      sort_by_value     = sort_by_value
                                      language_vector   = language_vector
                                      set_default_value = set_default_value ).
  ENDIF.
ENDMETHOD.


METHOD init_by_domain.
  TYPES BEGIN OF lt_text_tab.
  TYPES   langu TYPE langu.
  TYPES   text  TYPE val_text.
  TYPES END OF lt_text_tab.

  DATA BEGIN OF l_wa_value_tab.
  DATA   valpos TYPE valpos.
  DATA   value  TYPE domvalue_l.
  DATA   texts  TYPE STANDARD TABLE OF lt_text_tab.
  DATA END OF l_wa_value_tab.

  DATA l_value_tab        LIKE STANDARD TABLE OF l_wa_value_tab.

  DATA l_value_db_tab     TYPE entitytab.
  DATA l_value_db_tab_txt TYPE tabname.
  DATA l_value_db_key_txt TYPE fieldname.

  DATA l_field_table      TYPE STANDARD TABLE OF string.
  DATA l_field_table_txt  TYPE STANDARD TABLE OF string.
  DATA l_where            TYPE string.

  DATA l_wa_field_table   TYPE string.

  DATA l_field_infos      TYPE STANDARD TABLE OF dfies.

  DATA l_variable1        TYPE string.
  DATA l_variable2        TYPE string.

  FIELD-SYMBOLS <l_field_info> TYPE dfies.

* Read domain info
  SELECT SINGLE entitytab
    FROM dd01l
    INTO l_value_db_tab
   WHERE domname  =  domain
     AND as4local = 'A'
     AND as4vers  = '0000'.                               "#EC CI_SUBRC

  IF NOT l_value_db_tab IS INITIAL.

* Read checktables
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = l_value_db_tab
      TABLES
        dfies_tab = l_field_infos
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      l_variable1 = l_value_db_tab.

      RAISE EXCEPTION TYPE /gal/cx_dd_helper_exception
        EXPORTING
          textid = /gal/cx_dd_helper_exception=>error_analyzing_checktable
          var1   = l_variable1.
    ENDIF.

    READ TABLE l_field_infos
          WITH KEY keyflag = 'X'
                   domname = domain
               ASSIGNING <l_field_info>.
    IF sy-subrc <> 0.
      l_variable1 = l_value_db_tab.

      RAISE EXCEPTION TYPE /gal/cx_dd_helper_exception
        EXPORTING
          textid = /gal/cx_dd_helper_exception=>error_analyzing_checktable
          var1   = l_variable1.
    ELSEIF <l_field_info>-leng > 10.
      l_variable1 = l_value_db_tab.
      l_variable2 = <l_field_info>-fieldname.

      RAISE EXCEPTION TYPE /gal/cx_dd_helper_exception
        EXPORTING
          textid = /gal/cx_dd_helper_exception=>field_exceeds_maximum_length
          var1   = l_variable1
          var2   = l_variable2.
    ENDIF.

    l_wa_field_table = <l_field_info>-fieldname.
    INSERT l_wa_field_table INTO TABLE l_field_table.

    CALL FUNCTION 'DDUT_TEXTTABLE_GET'
      EXPORTING
        tabname    = l_value_db_tab
      IMPORTING
        texttable  = l_value_db_tab_txt
        checkfield = l_value_db_key_txt.

    IF NOT l_value_db_tab_txt IS INITIAL.
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname   = l_value_db_tab_txt
        TABLES
          dfies_tab = l_field_infos
        EXCEPTIONS
          OTHERS    = 1.

      IF sy-subrc = 0.
        READ TABLE l_field_infos
              WITH KEY keyflag  = 'X'
                       datatype = 'LANG'
                   ASSIGNING <l_field_info>.
      ENDIF.

      IF sy-subrc = 0.
        l_wa_field_table = <l_field_info>-fieldname.
        INSERT l_wa_field_table INTO TABLE l_field_table_txt.

        READ TABLE l_field_infos
              WITH KEY keyflag = space
                       inttype = 'C'
                   ASSIGNING <l_field_info>.
      ENDIF.

      IF sy-subrc = 0.
        l_wa_field_table = <l_field_info>-fieldname.
        INSERT l_wa_field_table INTO TABLE l_field_table_txt.
      ELSE.
        CLEAR l_value_db_tab_txt.
      ENDIF.
    ENDIF.

    SELECT (l_field_table)
      FROM (l_value_db_tab)
      INTO l_wa_value_tab-value
     ORDER BY (l_field_table).                           "#EC CI_DYNTAB

      IF NOT l_value_db_tab_txt IS INITIAL.
        CONCATENATE l_value_db_key_txt
                    ' = ''' l_wa_value_tab-value ''''
               INTO l_where.

        SELECT (l_field_table_txt)
          FROM (l_value_db_tab_txt)
          INTO TABLE l_wa_value_tab-texts              "#EC CI_DYNWHERE
         WHERE (l_where)                                 "#EC CI_DYNTAB
         ORDER BY (l_field_table_txt).                    "#EC CI_SUBRC
      ENDIF.

      INSERT l_wa_value_tab INTO TABLE l_value_tab.
    ENDSELECT.                                            "#EC CI_SUBRC
  ELSE.

* Read domain fixed values
    SELECT valpos domvalue_l
      FROM dd07l
      INTO (l_wa_value_tab-valpos, l_wa_value_tab-value)
     WHERE domname  =  domain
       AND as4local = 'A'
       AND as4vers  = '0000'.

      SELECT ddlanguage ddtext
        FROM dd07t
        INTO TABLE l_wa_value_tab-texts
       WHERE domname    = domain
         AND as4local   = 'A'
         AND as4vers    = '0000'                        "#EC CI_GENBUFF
         AND domvalue_l = l_wa_value_tab-value.           "#EC CI_SUBRC

      SORT l_wa_value_tab-texts BY langu.

      INSERT l_wa_value_tab INTO TABLE l_value_tab.
    ENDSELECT.                                            "#EC CI_SUBRC

    SORT l_value_tab BY valpos.

    IF sy-subrc <> 0.
      l_variable1 = domain.

      RAISE EXCEPTION TYPE /gal/cx_dd_helper_exception
        EXPORTING
          textid = /gal/cx_dd_helper_exception=>no_fixed_values_defined
          var1   = l_variable1.
    ENDIF.
  ENDIF.

* Initialize dropdown field
  IF NOT dropdown_values IS REQUESTED.
    init_by_value_table( field_name        = field_name
                         value_table       = l_value_tab
                         key_field         = `VALUE`
                         value_field       = `TEXTS`
                         sort_by_key       = sort_by_key
                         sort_by_value     = sort_by_value
                         language_vector   = language_vector
                         set_default_value = set_default_value ).
  ELSE.
    dropdown_values = init_by_value_table( field_name        = field_name
                                           value_table       = l_value_tab
                                           key_field         = `VALUE`
                                           value_field       = `TEXTS`
                                           sort_by_key       = sort_by_key
                                           sort_by_value     = sort_by_value
                                           language_vector   = language_vector
                                           set_default_value = set_default_value ).
  ENDIF.
ENDMETHOD.


METHOD init_by_field.
  DATA l_type_descr   TYPE REF TO cl_abap_typedescr.
  DATA l_data_element TYPE rollname.
  DATA l_field_name   TYPE string.

  DATA l_callstack       TYPE sys_callst.

  FIELD-SYMBOLS <l_callstack> LIKE LINE OF l_callstack.
  FIELD-SYMBOLS <l_field>     TYPE any.

* Determine data element
  IF field IS SUPPLIED.
    ASSIGN field TO <l_field>.
  ELSE.
    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        et_callstack = l_callstack.

    LOOP AT l_callstack ASSIGNING <l_callstack>
         WHERE NOT progname = sy-repid.

      CONCATENATE '(' <l_callstack>-progname ')' field_name
             INTO l_field_name.

      ASSIGN (l_field_name) TO <l_field>.

      CHECK sy-subrc = 0.
      EXIT.
    ENDLOOP.

    IF <l_field> IS NOT ASSIGNED.
      RAISE EXCEPTION TYPE /gal/cx_dd_helper_exception
        EXPORTING
          textid = /gal/cx_dd_helper_exception=>cannot_access_field
          var1   = l_field_name.
    ENDIF.
  ENDIF.

  l_type_descr = cl_abap_typedescr=>describe_by_data( <l_field> ).

* Try to determine data element and domain
  IF l_type_descr->absolute_name   CP '\TYPE=*' AND
     l_type_descr->absolute_name+6 CN '\'.

    l_data_element = l_type_descr->absolute_name+6.
  ELSE.
    RAISE EXCEPTION TYPE /gal/cx_dd_helper_exception
      EXPORTING
        textid = /gal/cx_dd_helper_exception=>cannot_determine_data_element
        var1   = field_name.
  ENDIF.

* Initialize dropdown field
  IF NOT dropdown_values IS REQUESTED.
    init_by_data_element( field_name        = field_name
                          data_element      = l_data_element
                          sort_by_key       = sort_by_key
                          sort_by_value     = sort_by_value
                          language_vector   = language_vector
                          set_default_value = set_default_value ).
  ELSE.
    dropdown_values = init_by_data_element( field_name        = field_name
                                            data_element      = l_data_element
                                            sort_by_key       = sort_by_key
                                            sort_by_value     = sort_by_value
                                            language_vector   = language_vector
                                            set_default_value = set_default_value ).
  ENDIF.
ENDMETHOD.


METHOD init_by_value_table.
  DATA l_field_name      TYPE vrm_id.
  DATA l_field_name_ext  TYPE string.
  DATA l_type            TYPE c.

  DATA l_wa_values       LIKE LINE OF dropdown_values.

  DATA l_language_vector LIKE language_vector.

  DATA l_callstack       TYPE sys_callst.

  FIELD-SYMBOLS <l_key_value_pair>  TYPE any.
  FIELD-SYMBOLS <l_key>             TYPE any.
  FIELD-SYMBOLS <l_value>           TYPE any.

  FIELD-SYMBOLS <l_value_tab>       TYPE ANY TABLE.

  FIELD-SYMBOLS <l_langu_text_pair> TYPE any.
  FIELD-SYMBOLS <l_langu>           TYPE any.
  FIELD-SYMBOLS <l_text>            TYPE any.

  FIELD-SYMBOLS <l_field>           TYPE any.

  FIELD-SYMBOLS <l_callstack>       LIKE LINE OF l_callstack.
  FIELD-SYMBOLS <l_dropdown_values> LIKE LINE OF dropdown_values.

* Convert value table
  LOOP AT value_table ASSIGNING <l_key_value_pair>.

* Determine key field
    UNASSIGN <l_key>.

    IF NOT key_field IS INITIAL.
      ASSIGN COMPONENT key_field
          OF STRUCTURE <l_key_value_pair> TO <l_key>.
    ELSE.
      ASSIGN COMPONENT 1
          OF STRUCTURE <l_key_value_pair> TO <l_key>.
      IF sy-subrc <> 0.
        ASSIGN <l_key_value_pair> TO <l_key>. " Support internal tables without structure
      ENDIF.
    ENDIF.

    IF <l_key> IS NOT ASSIGNED.
      RAISE EXCEPTION TYPE /gal/cx_dd_helper_exception
        EXPORTING
          textid = /gal/cx_dd_helper_exception=>cannot_determine_key_field.
    ENDIF.

* Determine text field
    UNASSIGN <l_value>.

    IF NOT value_field IS INITIAL.
      ASSIGN COMPONENT value_field
          OF STRUCTURE <l_key_value_pair> TO <l_value>.
    ELSE.
      ASSIGN COMPONENT 2
          OF STRUCTURE <l_key_value_pair> TO <l_value>.
    ENDIF.

* Support tables with multi-lingual texts
    IF <l_value> IS ASSIGNED.
      DESCRIBE FIELD <l_value> TYPE l_type.

      IF l_type = 'h'.
        ASSIGN   <l_value> TO <l_value_tab>.
        UNASSIGN <l_value>.

* Initialize language vector
        IF language_vector IS INITIAL.
          CONCATENATE sy-langu '*' INTO l_language_vector.
        ELSE.
          l_language_vector = language_vector.
        ENDIF.

        WHILE NOT l_language_vector IS INITIAL
          AND NOT <l_value> IS ASSIGNED.

          LOOP AT <l_value_tab> ASSIGNING <l_langu_text_pair>.

* Determine language field
            IF NOT language_field IS INITIAL.
              ASSIGN COMPONENT language_field
                  OF STRUCTURE <l_langu_text_pair> TO <l_langu>.
            ELSE.
              ASSIGN COMPONENT 1
                  OF STRUCTURE <l_langu_text_pair> TO <l_langu>.
            ENDIF.

            IF NOT sy-subrc = 0.
              RAISE EXCEPTION TYPE /gal/cx_dd_helper_exception
                EXPORTING
                  textid = /gal/cx_dd_helper_exception=>cannot_determine_lang_field.
            ENDIF.

* Determine text field
            IF NOT text_field IS INITIAL.
              ASSIGN COMPONENT text_field
                  OF STRUCTURE <l_langu_text_pair> TO <l_text>.
            ELSE.
              ASSIGN COMPONENT 2
                  OF STRUCTURE <l_langu_text_pair> TO <l_text>.
            ENDIF.

            IF NOT sy-subrc = 0.
              RAISE EXCEPTION TYPE /gal/cx_dd_helper_exception
                EXPORTING
                  textid = /gal/cx_dd_helper_exception=>cannot_determine_text_field.
            ENDIF.

* Check language
            IF l_language_vector(1) = <l_langu> OR
               l_language_vector(1) = '*'.

              ASSIGN <l_text> TO <l_value>.
              EXIT.
            ENDIF.
          ENDLOOP.

          SHIFT l_language_vector LEFT.
        ENDWHILE.
      ENDIF.
    ENDIF.

* Fallback: Use key as text
    IF NOT <l_value> IS ASSIGNED.
      ASSIGN <l_key> TO <l_value>.
    ENDIF.

* Create record in value table
    l_wa_values-key  = <l_key>.
    l_wa_values-text = <l_value>.
    INSERT l_wa_values INTO TABLE dropdown_values.
  ENDLOOP.

* Sort value table (if necessary)
  IF NOT sort_by_key IS INITIAL.
    SORT dropdown_values BY key.
  ENDIF.

  IF NOT sort_by_value IS INITIAL.
    SORT dropdown_values STABLE BY text.
  ENDIF.

* Initialize dropdown and set default values
  IF NOT dropdown_values IS REQUESTED.
    l_field_name = field_name.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = l_field_name
        values = dropdown_values
      EXCEPTIONS
        OTHERS = 1.
    IF NOT sy-subrc = 0.
      RAISE EXCEPTION TYPE /gal/cx_dd_helper_exception
        EXPORTING
          textid = /gal/cx_dd_helper_exception=>error_calling_vrm_set_values.
    ENDIF.

    IF set_default_value <> abap_false.
      CALL FUNCTION 'SYSTEM_CALLSTACK'
        IMPORTING
          et_callstack = l_callstack.

      LOOP AT l_callstack ASSIGNING <l_callstack>
           WHERE NOT progname = sy-repid.

        CONCATENATE '(' <l_callstack>-progname ')' field_name
               INTO l_field_name_ext.

        ASSIGN (l_field_name_ext) TO <l_field>.

        CHECK sy-subrc = 0.
        EXIT.
      ENDLOOP.

      IF <l_field> IS ASSIGNED.
        READ TABLE dropdown_values
              WITH KEY key = <l_field>
                   TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          IF set_default_value = '1'.
            DESCRIBE TABLE dropdown_values LINES sy-tfill.

            IF sy-tfill = 1.
              READ TABLE dropdown_values INDEX 1
                   ASSIGNING <l_dropdown_values>.
            ENDIF.
          ELSE.
            READ TABLE dropdown_values INDEX 1
                 ASSIGNING <l_dropdown_values>.
          ENDIF.

          IF <l_dropdown_values> IS ASSIGNED.
            TRY.
                <l_field> = <l_dropdown_values>-key.

              CATCH cx_root.                            "#EC NO_HANDLER
                                                         "#EC CATCH_ALL
            ENDTRY.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMETHOD.
ENDCLASS.
