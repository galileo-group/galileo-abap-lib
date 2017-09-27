FUNCTION /gal/js_db_write.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(TABLE_NAME) TYPE  TABNAME
*"     REFERENCE(TABLE_LINE) TYPE  /GAL/DB_DATAS
*"     REFERENCE(MODIFY_ONLY) TYPE  ABAP_BOOL DEFAULT ABAP_FALSE
*"  EXCEPTIONS
*"      WRONG_CONTENT_DATA
*"      CANNOT_WRITE_TO_DB
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA:
    l_data            TYPE REF TO data,
    l_table_line_elem TYPE /gal/db_data.

  FIELD-SYMBOLS:
    <l_structure> TYPE any,
    <l_field>     TYPE any.


  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception wrong_content_data.
  cfw_pass_exception cannot_write_to_db.
  cfw_remote_coding.

  IF table_name NP '/GAL/JOBDATA*'.
    MESSAGE e034 WITH table_name RAISING cannot_write_to_db.
  ENDIF.

  CREATE DATA l_data TYPE (table_name).
  ASSIGN l_data->* TO <l_structure>.

  LOOP AT table_line INTO l_table_line_elem.
    ASSIGN COMPONENT l_table_line_elem-attribute OF STRUCTURE <l_structure> TO <l_field>.
    IF sy-subrc = 0.
      <l_field> = l_table_line_elem-value.
    ELSE.
      MESSAGE e000 WITH l_table_line_elem-attribute table_name RAISING wrong_content_data.
    ENDIF.
  ENDLOOP.

  IF modify_only = abap_false. "Caution: naming is confusing.
    "MODIFY_ONLY leads to the behavior that the table is only mofified (no values inserted).
    "So only UPDATE and _not_ MODIFY is executed.
    MODIFY (table_name) FROM <l_structure>.              "#EC CI_DYNTAB
  ELSE.
    UPDATE (table_name) FROM <l_structure>.              "#EC CI_DYNTAB
  ENDIF.
  IF sy-subrc <> 0.
    MESSAGE e001 WITH table_name RAISING cannot_write_to_db.
  ENDIF.

  COMMIT WORK.


ENDFUNCTION.
