FUNCTION /gal/js_db_copy_entry_to_hist.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(HIST_TABLE_NAME) TYPE  TABNAME
*"     REFERENCE(TABLE_NAME) TYPE  TABNAME
*"     REFERENCE(KEY_FIELD) TYPE  TFIELDNAME
*"     REFERENCE(KEY_VALUE) TYPE  STRING
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"      CANNOT_CREATE_HIST_ENTRY
*"----------------------------------------------------------------------

  DATA:
    l_data            TYPE REF TO data,
    l_data_hist       TYPE REF TO data,
    l_where           TYPE string,
    l_ex              TYPE REF TO cx_root,
    l_timestamp       TYPE timestamp,
    l_message         TYPE string.

  FIELD-SYMBOLS:
    <l_structure>      TYPE any,
    <l_structure_hist> TYPE any,
    <l_timestamp>      TYPE /gal/mod_timestamp.


  cfw_custom_auth /gal/cfw_auth=>const_cab_no_check.
  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception rfc_exception.
  cfw_pass_exception cannot_create_hist_entry.
  cfw_remote_coding.

  IF table_name NP '/GAL/*' OR hist_table_name NP '/GAL/*'.
    RETURN.
  ENDIF.

  TRY.
      CREATE DATA l_data TYPE (table_name).
      ASSIGN l_data->* TO <l_structure>.

      CREATE DATA l_data_hist TYPE (hist_table_name).
      ASSIGN l_data_hist->* TO <l_structure_hist>.

      CONCATENATE key_field ' = ''' key_value '''' INTO l_where.

      SELECT SINGLE * FROM (table_name) INTO <l_structure> WHERE (l_where). "#EC CI_DYNWHERE
                                                         "#EC CI_DYNTAB
      IF sy-subrc <> 0.
        MESSAGE e028 WITH table_name key_field key_value RAISING cannot_create_hist_entry.
      ENDIF.

      ASSIGN COMPONENT 'HIST_MOD_TIMESTAMP' OF STRUCTURE <l_structure_hist> TO <l_timestamp>.

      GET TIME STAMP FIELD l_timestamp.
      <l_timestamp> = l_timestamp.
      MOVE-CORRESPONDING <l_structure> TO <l_structure_hist>.

      INSERT (hist_table_name) FROM <l_structure_hist>.  "#EC CI_DYNTAB
      IF sy-subrc <> 0.
        MESSAGE e029 WITH hist_table_name key_field key_value RAISING cannot_create_hist_entry.
      ENDIF.

    CATCH cx_root INTO l_ex.                             "#EC CATCH_ALL
      l_message = l_ex->get_text( ).

      /gal/string=>string_to_message_vars( EXPORTING input = l_message
                                           IMPORTING msgv1 = sy-msgv1
                                                     msgv2 = sy-msgv2
                                                     msgv3 = sy-msgv3
                                                     msgv4 = sy-msgv4 ).

      MESSAGE e012(/gal/js) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                         RAISING cannot_create_hist_entry.

  ENDTRY.

  COMMIT WORK.
ENDFUNCTION.
