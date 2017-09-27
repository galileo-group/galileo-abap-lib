class /GAL/TRACE_CONTEXT definition
  public
  final
  create private

  global friends /GAL/TRACE .

*"* public components of class /GAL/TRACE_CONTEXT
*"* do not include other source files here!!!
public section.

  data CONTEXT_ID type GUID_16 read-only .
  data TIMESTAMP type TIMESTAMPL read-only .
  data SYSTEM_ID type /GAL/SYSTEM_ID read-only .
  data CLIENT_ID type MANDT read-only .
  data USER_ID type SYUNAME read-only .
  data USER_LANGUAGE type SPRAS read-only .
  data USER_COUNTRY type LAND1 read-only .
  data USER_LOCALE_MODIFIER type CPSUBLOCAL read-only .
  data USER_TIME_ZONE type SYSTZONLO read-only .
protected section.
*"* protected components of class /GAL/TRACE_CONTEXT
*"* do not include other source files here!!!
PRIVATE SECTION.
*"* private components of class /GAL/TRACE_CONTEXT
*"* do not include other source files here!!!

  METHODS constructor .
ENDCLASS.



CLASS /GAL/TRACE_CONTEXT IMPLEMENTATION.


METHOD constructor.
  context_id = /gal/uuid=>create_raw( ).

  GET TIME STAMP FIELD timestamp.

  system_id = sy-sysid.
  client_id = sy-mandt.
  user_id   = sy-uname.

  GET LOCALE LANGUAGE user_language
             COUNTRY  user_country
             MODIFIER user_locale_modifier.

  user_time_zone = sy-zonlo.
ENDMETHOD.
ENDCLASS.
