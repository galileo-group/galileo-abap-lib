CLASS /gal/search_help DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .
    TYPE-POOLS shlp .
CLASS cl_abap_typedescr DEFINITION LOAD .

  DATA search_help_description TYPE shlp_descr_t READ-ONLY .

  CLASS-METHODS get_by_data_element
    IMPORTING
      !data_element      TYPE rollname
    RETURNING
      VALUE(search_help) TYPE REF TO /gal/search_help
    RAISING
      /gal/cx_search_help_exception .
  CLASS-METHODS get_by_name_and_type
    IMPORTING
      !search_help_name  TYPE shlpname
      !search_help_type  TYPE ddshlptyp DEFAULT 'SH'
    RETURNING
      VALUE(search_help) TYPE REF TO /gal/search_help
    RAISING
      /gal/cx_search_help_exception .
  METHODS add_select_option
    IMPORTING
      !field  TYPE ddshselopt-shlpfield
      !sign   TYPE ddshselopt-sign DEFAULT 'I'
      !option TYPE ddshselopt-option DEFAULT 'EQ'
      !low    TYPE ddshselopt-low OPTIONAL
      !high   TYPE ddshselopt-high OPTIONAL .
  METHODS constructor
    IMPORTING
      !search_help_description TYPE shlp_descr_t .
  METHODS get_select_options
    RETURNING
      VALUE(select_options) TYPE /gal/search_help_sel_options .
  METHODS remove_select_options
    IMPORTING
      !field TYPE ddshselopt-shlpfield DEFAULT '*' .
  METHODS set_select_options
    IMPORTING
      !select_options TYPE /gal/search_help_sel_options .
  METHODS show
    IMPORTING
      !max_no_of_records TYPE i DEFAULT 500              "#EC NUMBER_OK
      !read_only         TYPE abap_bool DEFAULT abap_false
    EXPORTING
      !result            TYPE any
    RAISING
      /gal/cx_search_help_exception .
protected section.
private section.
ENDCLASS.



CLASS /GAL/SEARCH_HELP IMPLEMENTATION.


METHOD ADD_SELECT_OPTION.
  DATA l_wa_select_options TYPE ddshselopt.

* Add single select option to search help description
  l_wa_select_options-shlpfield = field.
  l_wa_select_options-sign      = sign.
  l_wa_select_options-option    = option.
  l_wa_select_options-low       = low.
  l_wa_select_options-high      = high.

  INSERT l_wa_select_options INTO TABLE search_help_description-selopt.
ENDMETHOD.


METHOD constructor.

* Store Search Help description
  me->search_help_description = search_help_description.
ENDMETHOD.


METHOD get_by_data_element.
  DATA l_tabname           TYPE tabname.
  DATA l_fieldname         TYPE fieldname.

  DATA l_field_infos       TYPE STANDARD TABLE OF dfies.
  DATA l_data_element_info TYPE dtelinfo.
  DATA l_domain_info       TYPE dd01v.
  DATA l_search_help_descr TYPE shlp_descr_t.

  DATA l_message_stack     TYPE REF TO /gal/message_stack.
  DATA l_message           TYPE string.
  DATA l_data_element      TYPE string.
  DATA l_error_flag        TYPE abap_bool.

  FIELD-SYMBOLS <l_field_info> TYPE dfies.

* Note: Data Elements need to be store in table name field!
  l_tabname = data_element.

* 1. Try: SAP default method
  CALL FUNCTION 'F4IF_DETERMINE_SEARCHHELP'
    EXPORTING
      tabname   = l_tabname
      fieldname = l_fieldname
    IMPORTING
      shlp      = l_search_help_descr
    EXCEPTIONS
      OTHERS    = 1.

* 2. Try: Consider check tables
  IF sy-subrc <> 0.
    CREATE OBJECT l_message_stack.

    l_message_stack->push( ).

    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname     = data_element
        all_types   = abap_true
      IMPORTING
        dtelinfo_wa = l_data_element_info
      EXCEPTIONS
        OTHERS      = 1.

    IF sy-subrc = 0.
      CALL FUNCTION 'DDIF_DOMA_GET'
        EXPORTING
          name     = l_data_element_info-refname
        IMPORTING
          dd01v_wa = l_domain_info
        EXCEPTIONS
          OTHERS   = 1.
    ENDIF.

    IF sy-subrc = 0.
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname   = l_domain_info-entitytab
        TABLES
          dfies_tab = l_field_infos
        EXCEPTIONS
          OTHERS    = 1.
    ENDIF.

    IF sy-subrc = 0.
      READ TABLE l_field_infos
            WITH KEY keyflag  = abap_true
                     rollname = data_element
                 ASSIGNING <l_field_info>.
      IF sy-subrc <> 0.
        READ TABLE l_field_infos
              WITH KEY keyflag = abap_true
                       domname = l_domain_info-domname
                   ASSIGNING <l_field_info>.
      ENDIF.
    ENDIF.

    IF sy-subrc = 0.
      CALL FUNCTION 'DD_SHLP_GET_HELPMETHOD'
        EXPORTING
          tabname       = l_domain_info-entitytab
          fieldname     = <l_field_info>-fieldname
          get_entitytab = 'X'
        CHANGING
          shlp          = l_search_help_descr
        EXCEPTIONS
          OTHERS        = 1.
    ENDIF.

    IF sy-subrc <> 0.
      l_error_flag = abap_true.
    ENDIF.

    l_message_stack->pop( ).
  ENDIF.

* Fehlerbehandlung
  IF l_error_flag = abap_true.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    l_data_element = data_element.

    RAISE EXCEPTION TYPE /gal/cx_search_help_exception
      EXPORTING
        textid = /gal/cx_search_help_exception=>no_search_help_for_data_elem
        var1   = l_data_element
        var2   = l_message.
  ENDIF.

* Create instance of Search Help
  CREATE OBJECT search_help
    EXPORTING
      search_help_description = l_search_help_descr.
ENDMETHOD.


METHOD get_by_name_and_type.
  DATA l_search_help_name  TYPE string.
  DATA l_search_help_type  TYPE string.

  DATA l_search_help_descr TYPE shlp_descr_t.

* Read Search Help Description
  CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
    EXPORTING
      shlpname = search_help_name
      shlptype = search_help_type
    IMPORTING
      shlp     = l_search_help_descr.
  IF l_search_help_descr-interface  IS INITIAL AND
     l_search_help_descr-fielddescr IS INITIAL AND
     l_search_help_descr-fieldprop  IS INITIAL.

    l_search_help_name = search_help_name.
    l_search_help_type = search_help_type.

    RAISE EXCEPTION TYPE /gal/cx_search_help_exception
      EXPORTING
        textid = /gal/cx_search_help_exception=>search_help_not_found
        var1   = l_search_help_name
        var2   = l_search_help_type.
  ENDIF.

* Create instance of Search Help
  CREATE OBJECT search_help
    EXPORTING
      search_help_description = l_search_help_descr.
ENDMETHOD.


METHOD get_select_options.

* Return all select options from search help description
  select_options = search_help_description-selopt.
ENDMETHOD.


METHOD remove_select_options.
  IF field <> '*'.
* Remove select option(s) from search help description
    DELETE search_help_description-selopt WHERE shlpfield = field.
  ELSE.
* Remove all select options from search help description
    CLEAR search_help_description-selopt.
  ENDIF.
ENDMETHOD.


METHOD set_select_options.

* Replace select options stored in search help description
  search_help_description-selopt = select_options.
ENDMETHOD.


METHOD show.
  DATA l_values    TYPE STANDARD TABLE OF ddshretval.
  DATA l_wa        TYPE REF TO data.

  DATA l_type_kind TYPE c.

  DATA l_multi     TYPE abap_bool.
  DATA l_ret_code  TYPE sysubrc.

  DATA l_message   TYPE string.

  FIELD-SYMBOLS <l_value> TYPE ddshretval.
  FIELD-SYMBOLS <l_wa>    TYPE any.
  FIELD-SYMBOLS <l_table> TYPE ANY TABLE.

* Initialize result
  CLEAR result.

* Determine type of target field
  DESCRIBE FIELD result TYPE l_type_kind.

  IF l_type_kind = cl_abap_typedescr=>typekind_table.
    l_multi = abap_true.
  ENDIF.

* Show Search Help
  CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
    EXPORTING
      shlp          = search_help_description
      disponly      = read_only
      maxrecords    = max_no_of_records
      multisel      = l_multi
    IMPORTING
      rc            = l_ret_code
    TABLES
      return_values = l_values
    EXCEPTIONS
      OTHERS        = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    RAISE EXCEPTION TYPE /gal/cx_search_help_exception
      EXPORTING
        textid = /gal/cx_search_help_exception=>custom_exception
        var1   = l_message.
  ELSEIF l_ret_code <> 0 OR l_values IS INITIAL.
    RAISE EXCEPTION TYPE /gal/cx_search_help_exception
      EXPORTING
        textid  = /gal/cx_search_help_exception=>aborted_by_user
        aborted = abap_true.
  ENDIF.

* Return result
  IF l_type_kind = cl_abap_typedescr=>typekind_table.
    ASSIGN result TO <l_table>.

    CREATE DATA l_wa LIKE LINE OF <l_table>.

    ASSIGN l_wa->* TO <l_wa>.

    LOOP AT l_values ASSIGNING <l_value>.
      <l_wa> = <l_value>-fieldval.
      INSERT <l_wa> INTO TABLE <l_table>.
    ENDLOOP.
  ELSE.
    LOOP AT l_values ASSIGNING <l_value>.
      result = <l_value>-fieldval.
    ENDLOOP.
  ENDIF.
ENDMETHOD.
ENDCLASS.
