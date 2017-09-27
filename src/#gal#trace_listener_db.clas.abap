class /GAL/TRACE_LISTENER_DB definition
  public
  inheriting from /GAL/TRACE_LISTENER
  final
  create public .

*"* public components of class /GAL/TRACE_LISTENER_DB
*"* do not include other source files here!!!
public section.

  methods CONSTRUCTOR .
  methods INITIALIZE .

  methods CLEANUP
    redefinition .
  methods FLUSH
    redefinition .
  methods WRITE
    redefinition .
protected section.
*"* private components of class /GAL/TRACE_LISTENER_DB
*"* do not include other source files here!!!
private section.
*"* private components of class /GAL/TRACE_LISTENER_DB
*"* do not include other source files here!!!

  type-pools ABAP .
  data IS_INITIALIZED type ABAP_BOOL value ABAP_FALSE. "#EC NOTEXT .
  data RETENTION_PERIOD type I value 14. "#EC NOTEXT .
ENDCLASS.



CLASS /GAL/TRACE_LISTENER_DB IMPLEMENTATION.


METHOD cleanup.
  DATA l_ref_date      TYPE d.
  DATA l_ref_timestamp TYPE timestampl.

  DATA l_context_id    TYPE guid_16.

  GET TIME.

  l_ref_date = sy-datlo - retention_period.

  CONVERT DATE l_ref_date TIME '000000'
     INTO TIME STAMP l_ref_timestamp TIME ZONE sy-zonlo.

  SELECT context_id
    FROM /gal/trace_ctx
    INTO l_context_id
   WHERE timestamp <= l_ref_timestamp.

    DELETE FROM /gal/trace_cnt WHERE context_id = l_context_id. "#EC CI_SUBRC
    DELETE FROM /gal/trace_ctx WHERE context_id = l_context_id. "#EC CI_SUBRC
  ENDSELECT.                                              "#EC CI_SUBRC
ENDMETHOD.


METHOD constructor.
  DATA l_config_store  TYPE REF TO /gal/config_store_local.
  DATA l_config_folder TYPE REF TO /gal/config_node.
  DATA l_config_node   TYPE REF TO /gal/config_node.

  super->constructor( ).

* Get retention period from configuration store
  TRY.
      CREATE OBJECT l_config_store.

      l_config_folder = l_config_store->get_node( path = `/Galileo Group AG/Open Source Components/Tracing/Listeners/Database` ). "#EC NOTEXT
      l_config_node = l_config_folder->get_child_node( `Retention Period` ). "#EC NOTEXT
      l_config_node->get_value( IMPORTING value = retention_period ).

    CATCH /gal/cx_config_exception.                     "#EC NO_HANDLER
      " Ignore configuration errors

  ENDTRY.
ENDMETHOD.


METHOD flush.
  CALL FUNCTION 'DB_COMMIT'.
ENDMETHOD.


METHOD initialize.
  DATA l_wa_context TYPE /gal/trace_ctx.

* Write trace context information to database
  l_wa_context-context_id       = /gal/trace=>context->context_id.
  l_wa_context-timestamp        = /gal/trace=>context->timestamp.
  l_wa_context-system_id        = /gal/trace=>context->system_id.
  l_wa_context-client_id        = /gal/trace=>context->client_id.
  l_wa_context-user_id          = /gal/trace=>context->user_id.
  l_wa_context-user_language    = /gal/trace=>context->user_language.
  l_wa_context-user_country     = /gal/trace=>context->user_country.
  l_wa_context-user_locale_modi = /gal/trace=>context->user_locale_modifier.
  l_wa_context-user_time_zone   = /gal/trace=>context->user_time_zone.
  INSERT /gal/trace_ctx FROM l_wa_context.                "#EC CI_SUBRC

  is_initialized = abap_true.
ENDMETHOD.


METHOD write.
  DATA l_wa_trace_cnt TYPE /gal/trace_cnt.

* Initialize listener
  IF is_initialized = abap_false.
    CALL METHOD initialize.
  ENDIF.

* Write line to trace
  l_wa_trace_cnt-context_id   = /gal/trace=>context->context_id.
  l_wa_trace_cnt-text         = text.
  l_wa_trace_cnt-indent_level = /gal/trace=>indent_level.
  l_wa_trace_cnt-context_info = context_info.
  l_wa_trace_cnt-caller_info  = caller_info.

  DO.
    GET TIME STAMP FIELD l_wa_trace_cnt-timestamp.

    INSERT /gal/trace_cnt FROM l_wa_trace_cnt.

    IF sy-subrc = 0.
      EXIT.
    ENDIF.
  ENDDO.
ENDMETHOD.
ENDCLASS.
