FUNCTION /gal/js_db_select_single.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(TABLE_NAME) TYPE  TABNAME
*"     REFERENCE(ID) TYPE  GUID_32
*"  EXPORTING
*"     REFERENCE(TABLE_LINE) TYPE  /GAL/DB_DATAS
*"  EXCEPTIONS
*"      NO_DATA_FOUND
*"      UNKNOWN_TABLE
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA:
    l_data            TYPE REF TO data,
    l_dd03l           TYPE dd03l,
    lt_dd03l          TYPE TABLE OF dd03l,
    l_table_line_elem TYPE /gal/db_data.

  FIELD-SYMBOLS:
    <l_structure> TYPE any,
    <l_field>     TYPE any.

* Initialize result
  CLEAR table_line.

  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception unknown_table.
  cfw_pass_exception no_data_found.
  cfw_remote_coding.

  SELECT * FROM dd03l INTO TABLE lt_dd03l WHERE tabname = table_name.
  IF sy-subrc <> 0.
    MESSAGE e002 WITH table_name RAISING unknown_table.
  ENDIF.

  CREATE DATA l_data TYPE (table_name).
  ASSIGN l_data->* TO <l_structure>.

  SELECT SINGLE * FROM (table_name) INTO <l_structure> WHERE id = id. "#EC CI_DYNTAB
  IF sy-subrc <> 0.
    MESSAGE e003 WITH table_name id RAISING no_data_found.
  ENDIF.

  LOOP AT lt_dd03l INTO l_dd03l.
    ASSIGN COMPONENT l_dd03l-fieldname OF STRUCTURE <l_structure> TO <l_field>.
*   This assign cannot go wrong since information is taken from data dictionary
    l_table_line_elem-attribute = l_dd03l-fieldname.
    l_table_line_elem-value = <l_field>.
    INSERT l_table_line_elem INTO TABLE table_line.
  ENDLOOP.



ENDFUNCTION.
