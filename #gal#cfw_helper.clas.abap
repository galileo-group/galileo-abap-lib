class /GAL/CFW_HELPER definition
  public
  final
  create public .

*"* public components of class /GAL/CFW_HELPER
*"* do not include other source files here!!!
public section.
  type-pools ABAP .

  class-methods CHECK_RFC_AUTHORIZATION
    importing
      !FUNCTION_NAME type STRING optional
      !FUNCTION_GROUP type STRING optional
    raising
      /GAL/CX_CFW_EXCEPTION .
  class-methods COMBINE_RFC_ROUTE_INFOS
    importing
      !RFC_ROUTE_INFO1 type /GAL/RFC_ROUTE_INFO
      !RFC_ROUTE_INFO2 type /GAL/RFC_ROUTE_INFO
    returning
      value(RFC_ROUTE_INFO_COMBINED) type /GAL/RFC_ROUTE_INFO .
  class-methods CREATE_DATA
    importing
      !TYPE_NAME type STRING
    returning
      value(DATA) type ref to DATA
    raising
      /GAL/CX_CFW_EXCEPTION .
  class-methods DESERIALIZE_FUNCTION_PARAMS
    importing
      !XML type STRING
    exporting
      !PARAMETER_BINDINGS type ABAP_FUNC_PARMBIND_TAB
    raising
      CX_TRANSFORMATION_ERROR .
  class-methods GET_EXCEPTION
    importing
      !EXCEPTION_INFO type /GAL/EXCEPTION_INFO
      !FUNCTION_NAME type CSEQUENCE optional
    returning
      value(EXCEPTION) type ref to CX_STATIC_CHECK .
  class-methods GET_EXCEPTION_INFO
    importing
      !EXCEPTION type ref to CX_ROOT
    returning
      value(EXCEPTION_INFO) type /GAL/EXCEPTION_INFO .
  class-methods GET_FUNCTION_INFO
    importing
      !FUNCTION_NAME type STRING
    exporting
      !FUNCTION_GROUP type STRING
      !CLASS_BASED_EXCEPTIONS type FLAG
    raising
      /GAL/CX_CFW_EXCEPTION .
  class-methods GET_FUNCTION_INTERFACE
    importing
      !FUNCTION_NAME type STRING
      !INCLUDE_IMPORTING type FLAG optional
      !INCLUDE_EXPORTING type FLAG optional
      !INCLUDE_CHANGING type FLAG optional
      !INCLUDE_TABLES type FLAG optional
      !INCLUDE_EXCEPTIONS type FLAG optional
      !CHECK_COMPATIBILITY type FLAG optional
    returning
      value(FUNCTION_INTERFACE) type /GAL/FUNC_INTERFACE_INFO
    raising
      /GAL/CX_CFW_EXCEPTION .
  class-methods GET_FUNCTION_NAME
    returning
      value(FUNCTION_NAME) type STRING
    raising
      /GAL/CX_CFW_EXCEPTION .
  class-methods GET_FUNCTION_NAME_EXT
    importing
      !MAX_CALL_STACK_DEPTH type I default 10
    exporting
      !FUNCTION_NAME type STRING
      !CALL_STACK type ABAP_CALLSTACK
    raising
      /GAL/CX_CFW_EXCEPTION .
  class-methods GET_TYPE_POOL
    importing
      !TYPE_NAME type STRING
    returning
      value(TYPE_POOL) type STRING
    raising
      /GAL/CX_CFW_EXCEPTION .
  class-methods INITIALIZE_RFC_ROUTE_INFO
    changing
      !RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO .
  class-methods INITIALIZE_RFC_ROUTE_STEP_INFO
    changing
      !RFC_ROUTE_STEP_INFO type /GAL/RFC_ROUTE_STEP_INFO .
  class-methods PING
    importing
      !RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO
    exporting
      !RFC_ROUTE_STEP_INFOS type /GAL/RFC_ROUTE_STEP_INFOS
      !EXCEPTION_INFO type /GAL/EXCEPTION_INFO .
  class-methods RAISE_EXCEPTION
    importing
      !EXCEPTION_INFO type /GAL/EXCEPTION_INFO
      !FUNCTION_NAME type CSEQUENCE optional
    raising
      CX_STATIC_CHECK .
  class-methods RESTORE_USER_ENVIRONMENT
    importing
      !LANGUAGE type SPRAS
      !COUNTRY type LAND1
      !LOCALE_MODIFIER type CPSUBLOCAL .
  class-methods RFC_ROUTE_INFO_FROM_STRING
    importing
      !STRING type CSEQUENCE
      !SEPARATOR type CSEQUENCE default `;`
    returning
      value(RFC_ROUTE_INFO) type /GAL/RFC_ROUTE_INFO .
  class-methods RFC_ROUTE_INFO_TO_STRING
    importing
      !RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO
      !SEPARATOR type STRING default `;`
    returning
      value(STRING) type STRING .
  class-methods SERIALIZE_FUNCTION_PARAMS
    importing
      !PARAMETER_BINDINGS type ABAP_FUNC_PARMBIND_TAB
    exporting
      !XML type STRING
    raising
      CX_TRANSFORMATION_ERROR .
  class-methods UPDATE_RFC_ROUTE_STEP_INFO
    changing
      !RFC_ROUTE_STEP_INFO type /GAL/RFC_ROUTE_STEP_INFO .
  class-methods WRITE_TO_TRACE
    importing
      !CONTEXT type STRING
      !FUNCTION_NAME type STRING
      !RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO
      !INCLUDE_CALL_STACK type ABAP_BOOL optional .
protected section.
*"* protected components of class /GAL/COMM_FW_HELPER
*"* do not include other source files here!!!
private section.
*"* private components of class /GAL/CFW_HELPER
*"* do not include other source files here!!!

  types:
    BEGIN OF lt_wa_abs_type_cache.
  TYPES   type_name     TYPE string.
  TYPES   abs_type_name TYPE string.
  TYPES END OF lt_wa_abs_type_cache .
  types:
    lt_abs_type_cache TYPE HASHED TABLE OF lt_wa_abs_type_cache
                          WITH UNIQUE KEY type_name .

  class-data ABS_TYPE_CACHE type LT_ABS_TYPE_CACHE .
ENDCLASS.



CLASS /GAL/CFW_HELPER IMPLEMENTATION.


METHOD check_rfc_authorization.

* This method should be called on the target system of the RFC

  DATA l_function_group      TYPE string.
  DATA l_function_group_auth TYPE authb-rfc_name.

* Verify parameters and get Function Group
  IF function_name IS SUPPLIED.
    CALL METHOD get_function_info
      EXPORTING
        function_name  = function_name
      IMPORTING
        function_group = l_function_group.

    IF function_group IS SUPPLIED AND function_group <> l_function_group.
      RAISE EXCEPTION TYPE /gal/cx_cfw_exception
            EXPORTING textid = /gal/cx_cfw_exception=>function_not_in_function_group
                      var1   = function_name
                      var2   = function_group
                      var3   = l_function_group.
    ENDIF.
  ELSEIF function_group IS SUPPLIED.
    l_function_group = function_group.
  ELSE.
    RAISE EXCEPTION TYPE /gal/cx_cfw_exception
          EXPORTING textid = /gal/cx_cfw_exception=>check_rfc_auth_invalid_call.
  ENDIF.

* Perform authority check
  l_function_group_auth = l_function_group.

  AUTHORITY-CHECK OBJECT 'S_RFC'
                      ID 'RFC_TYPE' FIELD 'FUGR'
                      ID 'RFC_NAME' FIELD l_function_group_auth
                      ID 'ACTVT'    FIELD '16'.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gal/cx_cfw_exception
          EXPORTING textid = /gal/cx_cfw_exception=>no_rfc_auth_for_function_group
                    var1   = l_function_group.
  ENDIF.
ENDMETHOD.


METHOD combine_rfc_route_infos.

  INSERT LINES OF rfc_route_info1-step_infos INTO TABLE rfc_route_info_combined-step_infos.
  INSERT LINES OF rfc_route_info2-step_infos INTO TABLE rfc_route_info_combined-step_infos.

ENDMETHOD.


METHOD create_data.
  DATA l_exception         TYPE REF TO cx_root.
  DATA l_message           TYPE string.

  DATA l_wa_abs_type_cache LIKE LINE OF abs_type_cache.

  DATA l_type_pool         TYPE string.
  DATA l_abs_type_name     TYPE string.

  FIELD-SYMBOLS <l_abs_type_cache> LIKE LINE OF abs_type_cache.

  TRY.
      READ TABLE abs_type_cache
            WITH TABLE KEY type_name = type_name
                 ASSIGNING <l_abs_type_cache>.
      IF sy-subrc = 0.
        CREATE DATA data TYPE (<l_abs_type_cache>-abs_type_name).
        RETURN.
      ELSE.
        CREATE DATA data TYPE (type_name).
        RETURN.
      ENDIF.

    CATCH cx_sy_create_data_error INTO l_exception.
      l_message = l_exception->get_text( ).

  ENDTRY.

  IF sy-saprl < '721'.
    TRY.
        l_type_pool = get_type_pool( type_name ).

        CONCATENATE `\TYPE-POOL=` l_type_pool `\TYPE=` type_name
               INTO l_abs_type_name.

        CREATE DATA data TYPE (l_abs_type_name).

        l_wa_abs_type_cache-type_name     = type_name.
        l_wa_abs_type_cache-abs_type_name = l_abs_type_name.
        INSERT l_wa_abs_type_cache INTO TABLE abs_type_cache.

        RETURN.

      CATCH /gal/cx_cfw_exception.                      "#EC NO_HANDLER
        "Exception is thrown later

    ENDTRY.
  ENDIF.

  RAISE EXCEPTION TYPE /gal/cx_cfw_exception
    EXPORTING
      textid = /gal/cx_cfw_exception=>create_data_exception
      var1   = l_message.
ENDMETHOD.


METHOD deserialize_function_params.
  CLEAR parameter_bindings.
  IF xml IS NOT INITIAL.
    CALL TRANSFORMATION id
         OPTIONS    value_handling = 'default'
         SOURCE XML xml
         RESULT     parameter_bindings = parameter_bindings. "#EC NOTEXT
  ENDIF.
ENDMETHOD.


  METHOD get_exception.
    TRY.
        raise_exception( EXPORTING exception_info = exception_info
                                   function_name  = function_name ).

      CATCH cx_static_check INTO exception.             "#EC NO_HANDLER
        "Nothing needs to be done here!

    ENDTRY.
  ENDMETHOD.


METHOD get_exception_info.
  DATA l_class_descr  TYPE REF TO cl_abap_classdescr.

  DATA l_program_name TYPE syrepid.
  DATA l_include_name TYPE syrepid.

  l_class_descr ?= cl_abap_typedescr=>describe_by_object_ref( exception ).

  SPLIT l_class_descr->absolute_name
     AT '\CLASS=' INTO exception_info-exception_name
                       exception_info-exception_name.

  exception_info-system_id       = sy-sysid.
  exception_info-client_id       = sy-mandt.
  exception_info-exception_type  = 'C'.
  exception_info-message_type    = 'E'.
  exception_info-message_text    = exception->get_text( ).

  exception->get_source_position( IMPORTING program_name = l_program_name
                                            include_name = l_include_name
                                            source_line  = exception_info-source_line ).

  exception_info-program_name = l_program_name.
  exception_info-include_name = l_include_name.

  CALL TRANSFORMATION id
       OPTIONS    data_refs          = 'heap-or-create'
                  initial_components = 'include'
                  technical_types    = 'error'
                  value_handling     = 'default'
                  xml_header         = 'full'
       SOURCE     exception          = exception
       RESULT XML exception_info-xml.                       "#EC NOTEXT
ENDMETHOD.


METHOD get_function_info.
  SELECT SINGLE area
                exten3
           FROM enlfdir
           INTO (function_group,
                 class_based_exceptions)
          WHERE funcname = function_name.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gal/cx_cfw_exception
          EXPORTING textid = /gal/cx_cfw_exception=>function_does_not_exist
                    var1   = function_name.
  ENDIF.
ENDMETHOD.


METHOD get_function_interface.
  DATA l_param_type_range      TYPE RANGE OF fupararef-paramtype.
  DATA l_wa_param_type_range   LIKE LINE OF l_param_type_range.

  DATA l_wa_function_interface TYPE /gal/func_interface_param_info.
  DATA lt_func_param_infos     TYPE /gal/func_interface_par_infos.


* Build range for parameter type restriction
  l_wa_param_type_range-sign   = 'I'.
  l_wa_param_type_range-option = 'EQ'.

  IF include_importing IS NOT INITIAL.
    l_wa_param_type_range-low = 'I'.
    INSERT l_wa_param_type_range INTO TABLE l_param_type_range.
  ENDIF.

  IF include_exporting IS NOT INITIAL.
    l_wa_param_type_range-low = 'E'.
    INSERT l_wa_param_type_range INTO TABLE l_param_type_range.
  ENDIF.

  IF include_changing IS NOT INITIAL.
    l_wa_param_type_range-low = 'C'.
    INSERT l_wa_param_type_range INTO TABLE l_param_type_range.
  ENDIF.

  IF include_tables IS NOT INITIAL.
    l_wa_param_type_range-low = 'T'.
    INSERT l_wa_param_type_range INTO TABLE l_param_type_range.
  ENDIF.

  IF include_exceptions IS NOT INITIAL.
    l_wa_param_type_range-low = 'X'.
    INSERT l_wa_param_type_range INTO TABLE l_param_type_range.
  ENDIF.

* Determine function module interface
  SELECT parameter
         paramtype
         structure
         optional
    FROM fupararef
    INTO (l_wa_function_interface-parameter_name,
          l_wa_function_interface-parameter_kind,
          l_wa_function_interface-parameter_type,
          l_wa_function_interface-is_optional)
   WHERE funcname = function_name
     AND r3state  = 'A'
     AND paramtype IN l_param_type_range.

    CASE l_wa_function_interface-parameter_kind.

      WHEN 'C'.
        l_wa_function_interface-parameter_kind_int = abap_func_changing.

      WHEN 'E'.
        l_wa_function_interface-parameter_kind_int = abap_func_importing.

      WHEN 'I'.
        l_wa_function_interface-parameter_kind_int = abap_func_exporting.

      WHEN 'T'.
        l_wa_function_interface-parameter_kind_int = abap_func_tables.

    ENDCASE.

    INSERT l_wa_function_interface INTO TABLE lt_func_param_infos.
  ENDSELECT.                                              "#EC CI_SUBRC

  " In the following Select it may not be checked for ACTIVE 'X',
  " since functions being worked on would not be found even if already an active version existed.
  SELECT SINGLE exten3 FROM enlfdir INTO function_interface-uses_exception_classes
         WHERE funcname = function_name.                  "#EC CI_SUBRC

  function_interface-param_info = lt_func_param_infos.

  IF check_compatibility IS NOT INITIAL.
    READ TABLE function_interface-param_info
          WITH KEY parameter_type = 'T'
               TRANSPORTING NO FIELDS.                   "#EC CI_STDSEQ
    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE /gal/cx_cfw_exception
        EXPORTING
          textid = /gal/cx_cfw_exception=>table_parameters_not_supported.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD get_function_name.
  get_function_name_ext( IMPORTING function_name = function_name ).
ENDMETHOD.


METHOD get_function_name_ext.
  DATA l_max_depth TYPE i.
  DATA l_index     TYPE i.

  FIELD-SYMBOLS <l_callstack_entry> LIKE LINE OF call_stack.

* Initialize return value
  CLEAR function_name.

* Try fast version (direct call from Function Module)
  l_max_depth = max_call_stack_depth + 2.

  CALL FUNCTION 'SYSTEM_CALLSTACK'
    EXPORTING
      max_level = l_max_depth
    IMPORTING
      callstack = call_stack.

  READ TABLE call_stack INDEX 2 ASSIGNING <l_callstack_entry>.

  IF sy-subrc = 0 AND <l_callstack_entry>-blocktype = 'FUNCTION'.
    function_name = <l_callstack_entry>-blockname.

    DELETE call_stack TO 2.
    RETURN.
  ENDIF.

* Analyze full callstack
  CALL FUNCTION 'SYSTEM_CALLSTACK'
    IMPORTING
      callstack = call_stack.

  LOOP AT call_stack FROM 3
       ASSIGNING <l_callstack_entry>
           WHERE blocktype = 'FUNCTION'.

    function_name = <l_callstack_entry>-blockname.

    l_index = max_call_stack_depth + 1.

    DELETE call_stack TO   sy-tabix.
    DELETE call_stack FROM l_index.
    EXIT.
  ENDLOOP.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gal/cx_cfw_exception
      EXPORTING
        textid = /gal/cx_cfw_exception=>get_func_name_invalid_call.
  ENDIF.
ENDMETHOD.


METHOD get_type_pool.
  DATA l_type_name TYPE ttypename.
  DATA l_type_pool TYPE progname.

  l_type_name = type_name.
  l_type_pool = type_pool.

  CALL FUNCTION 'S_PARA_INT_TYPEPOOL_GET'
    EXPORTING
      iv_typename = l_type_name
    IMPORTING
      ev_typepool = l_type_pool
    EXCEPTIONS
      OTHERS      = 1.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gal/cx_cfw_exception
      EXPORTING
        textid = /gal/cx_cfw_exception=>type_not_found_in_type_pool
        var1   = type_name.
  ENDIF.

  type_pool = l_type_pool.
ENDMETHOD.


METHOD initialize_rfc_route_info.
  FIELD-SYMBOLS <l_rfc_route_step_info> TYPE /gal/rfc_route_step_info.

  LOOP AT rfc_route_info-step_infos ASSIGNING <l_rfc_route_step_info>.
    IF <l_rfc_route_step_info>-rfc_destination IS INITIAL OR
       <l_rfc_route_step_info>-rfc_destination = `NONE`.
      DELETE rfc_route_info-step_infos.
    ELSE.
      /gal/cfw_helper=>initialize_rfc_route_step_info( CHANGING rfc_route_step_info = <l_rfc_route_step_info> ).
    ENDIF.
  ENDLOOP.

  CLEAR rfc_route_info-call_stack.
  CLEAR rfc_route_info-current_step.
ENDMETHOD.


METHOD initialize_rfc_route_step_info.
  CLEAR rfc_route_step_info-system_id.
  CLEAR rfc_route_step_info-client_id.
  CLEAR rfc_route_step_info-user_id.
  CLEAR rfc_route_step_info-user_time_zone.
  CLEAR rfc_route_step_info-user_language.
  CLEAR rfc_route_step_info-user_country.
  CLEAR rfc_route_step_info-user_locale_modifier.
ENDMETHOD.


METHOD ping.
  CALL FUNCTION '/GAL/RFC_ROUTE_PING'
    EXPORTING
      rfc_route_info       = rfc_route_info
    IMPORTING
      rfc_route_step_infos = rfc_route_step_infos
      exception_info       = exception_info.
ENDMETHOD.


  METHOD raise_exception.

* Please be aware that this coding may raise any type of exception!
* To be sure to handle all exceptions, catch exception CX_STATIC_CHECK.

    DATA l_function_name TYPE string.

    IF function_name IS NOT INITIAL.
      l_function_name = function_name.
    ELSE.
      TRY.
          l_function_name = get_function_name( ).

        CATCH /gal/cx_cfw_exception.
          l_function_name = TEXT-d00.

      ENDTRY.
    ENDIF.

    PERFORM cfw_raise_new_exception IN PROGRAM /gal/cfw_forms
      USING exception_info l_function_name.
  ENDMETHOD.


METHOD restore_user_environment.
  CATCH SYSTEM-EXCEPTIONS OTHERS = 1.
    SET LOCALE LANGUAGE language
               COUNTRY  country
               MODIFIER locale_modifier.
  ENDCATCH.

  IF NOT sy-subrc = 0.

* A system exception of type TEXTENV_CODEPAGE_NOT_ALLOWED may occur in
* case of incompatible locale or codepages settings. As a fallback solution
* only the language will be adjusted in this case.
    CATCH SYSTEM-EXCEPTIONS OTHERS = 1.
      SET LOCALE LANGUAGE language.
    ENDCATCH.                                             "#EC CI_SUBRC
  ENDIF.
ENDMETHOD.


METHOD rfc_route_info_from_string.
  DATA l_rfc_destinations       TYPE STANDARD TABLE OF string.
  DATA l_wa_rfc_route_step_info TYPE /gal/rfc_route_step_info.

  SPLIT string AT separator INTO TABLE l_rfc_destinations.

  LOOP AT l_rfc_destinations INTO l_wa_rfc_route_step_info-rfc_destination.
    SHIFT l_wa_rfc_route_step_info-rfc_destination RIGHT DELETING TRAILING space.
    SHIFT l_wa_rfc_route_step_info-rfc_destination LEFT DELETING LEADING space.

    INSERT l_wa_rfc_route_step_info INTO TABLE rfc_route_info-step_infos.
  ENDLOOP.
ENDMETHOD.


METHOD rfc_route_info_to_string.
  DATA l_rfc_route_info LIKE rfc_route_info.

  FIELD-SYMBOLS <l_rfc_route_step_info> TYPE /gal/rfc_route_step_info.

  l_rfc_route_info = rfc_route_info.

  initialize_rfc_route_info( CHANGING rfc_route_info = l_rfc_route_info ).

  LOOP AT l_rfc_route_info-step_infos ASSIGNING <l_rfc_route_step_info>.
    AT FIRST.
      string = <l_rfc_route_step_info>-rfc_destination.
      CONTINUE.
    ENDAT.

    CONCATENATE string <l_rfc_route_step_info>-rfc_destination
           INTO string SEPARATED BY separator.
  ENDLOOP.
ENDMETHOD.


METHOD serialize_function_params.
  CALL TRANSFORMATION id
       OPTIONS    data_refs          = 'heap-or-create'
                  initial_components = 'include'
                  technical_types    = 'error'
                  value_handling     = 'default'
                  xml_header         = 'full'
       SOURCE     parameter_bindings = parameter_bindings
       RESULT XML xml.                                      "#EC NOTEXT
ENDMETHOD.


METHOD update_rfc_route_step_info.
  rfc_route_step_info-host           = sy-host.
  rfc_route_step_info-system_id      = sy-sysid.
  rfc_route_step_info-client_id      = sy-mandt.
  rfc_route_step_info-user_id        = sy-uname.
  rfc_route_step_info-user_time_zone = sy-zonlo.

  GET LOCALE LANGUAGE rfc_route_step_info-user_language
             COUNTRY  rfc_route_step_info-user_country
             MODIFIER rfc_route_step_info-user_locale_modifier.

  rfc_route_step_info-batch = sy-batch.
ENDMETHOD.


METHOD write_to_trace.
  TYPES BEGIN OF lt_step_info.
  TYPES   act(2)          TYPE c.
  TYPES   rfc_destination TYPE /gal/rfc_destination.
  TYPES   host            TYPE syhost.
  TYPES   system_id       TYPE /gal/system_id.
  TYPES   client_id       TYPE mandt.
  TYPES   user_id         TYPE syuname.
  TYPES   user_language   TYPE spras.
  TYPES   user_time_zone  TYPE systzonlo.
  TYPES END OF lt_step_info.

  TYPES lt_step_infos TYPE STANDARD TABLE OF lt_step_info.

  DATA l_step_infos    TYPE lt_step_infos.
  DATA l_wa_step_infos LIKE LINE OF l_step_infos.

  FIELD-SYMBOLS <l_step_info> LIKE LINE OF rfc_route_info-step_infos.

* Convert step infos for trace
  LOOP AT rfc_route_info-step_infos ASSIGNING <l_step_info>.
    MOVE-CORRESPONDING <l_step_info> TO l_wa_step_infos.

    IF rfc_route_info-current_step = sy-tabix.
      l_wa_step_infos-act = '->'.                           "#EC NOTEXT
    ELSE.
      l_wa_step_infos-act = '  '.                           "#EC NOTEXT
    ENDIF.

    INSERT l_wa_step_infos INTO TABLE l_step_infos.
  ENDLOOP.

* Write to trace
  /gal/trace=>write_text( text     = context
                          no_flush = abap_true ).

  /gal/trace=>indent( ).

  /gal/trace=>write_text( text     = `Function name: {1}`   "#EC NOTEXT
                          var01    = function_name
                          no_flush = abap_true ).

  /gal/trace=>write_table( table    = l_step_infos
                           no_flush = abap_true ).

  IF include_call_stack = abap_true.
    /gal/trace=>write_table( table    = rfc_route_info-call_stack
                             no_flush = abap_true ).
  ENDIF.

  /gal/trace=>unindent( ).
  /gal/trace=>flush( ).
ENDMETHOD.
ENDCLASS.
