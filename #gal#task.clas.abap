class /GAL/TASK definition
  public
  create public .

public section.
  type-pools ABAP .

  constants STATUS_ABORTED type C value 'A'. "#EC NOTEXT
  constants STATUS_COMPLETED type C value 'C'. "#EC NOTEXT
  constants STATUS_RUNNING type C value 'R'. "#EC NOTEXT
  constants STATUS_WAITING type C value 'W'. "#EC NOTEXT
  data CONTEXT_ID type /GAL/TASK_CONTEXT_ID read-only .
  data EXCEPTION_INFO type /GAL/EXCEPTION_INFO read-only .
  data FUNCTION type STRING read-only .
  data PARAMETERS_IN type ABAP_FUNC_PARMBIND_TAB read-only .
  data PARAMETERS_OUT type ABAP_FUNC_PARMBIND_TAB read-only .
  data RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO read-only .
  data STATUS type C read-only .

  methods CONSTRUCTOR
    importing
      !RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO optional
      !FUNCTION type CSEQUENCE optional
      !PARAMETERS type ABAP_FUNC_PARMBIND_TAB optional
      !CONTEXT_ID type /GAL/TASK_CONTEXT_ID optional .
  methods GET_PARAMETER
    importing
      !KIND type I optional
      !NAME type CSEQUENCE
    exporting
      !VALUE type ANY
    raising
      /GAL/CX_CFW_EXCEPTION .
  methods GET_PARAMETERS
    exporting
      !PARAMETERS type ABAP_FUNC_PARMBIND_TAB
    raising
      /GAL/CX_CFW_EXCEPTION .
  methods GET_RESULT
    importing
      !TIMEOUT type I optional
    exporting
      !RESULT type ABAP_FUNC_PARMBIND_TAB
    raising
      /GAL/CX_CFW_EXCEPTION .
  methods HANDLE_TASK_COMPLETION
    importing
      !P_TASK type CLIKE .
  methods RESET
    importing
      !TIMEOUT type I default 5
    raising
      /GAL/CX_CFW_EXCEPTION .
  methods SET_FUNCTION
    importing
      !FUNCTION type CSEQUENCE optional
    raising
      /GAL/CX_CFW_EXCEPTION .
  methods SET_PARAMETER
    importing
      !KIND type I default 0
      !NAME type CSEQUENCE
      !VALUE type ANY
    raising
      /GAL/CX_CFW_EXCEPTION .
  methods SET_PARAMETERS
    importing
      !PARAMETERS type ABAP_FUNC_PARMBIND_TAB optional
    raising
      /GAL/CX_CFW_EXCEPTION .
  methods SET_RFC_ROUTE_INFO
    importing
      !RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO optional
    raising
      /GAL/CX_CFW_EXCEPTION .
  methods START_ASYNC
    importing
      !KEEP_CONTEXT type ABAP_BOOL default ABAP_FALSE .
  methods START_SYNC .
  methods WAIT
    importing
      !TIMEOUT type I optional
    raising
      /GAL/CX_CFW_EXCEPTION .
protected section.
private section.

  data KEEP_CONTEXT type ABAP_BOOL .
ENDCLASS.



CLASS /GAL/TASK IMPLEMENTATION.


METHOD constructor.
  me->rfc_route_info   = rfc_route_info.
  me->function         = function.
  me->parameters_in    = parameters.

  me->status         = status_waiting.

  IF context_id IS NOT INITIAL.
    me->context_id = context_id.
  ELSE.
    me->context_id = /gal/uuid=>create_char( ).
  ENDIF.
ENDMETHOD.


METHOD get_parameter.
  DATA l_wa_param_binding LIKE LINE OF parameters_out.

  FIELD-SYMBOLS <l_param_binding> LIKE LINE OF parameters_out.
  FIELD-SYMBOLS <l_param_value>   TYPE any.

  IF status <> status_completed.
    RAISE EXCEPTION TYPE /gal/cx_cfw_exception
      EXPORTING
        textid = /gal/cx_cfw_exception=>task_not_completed.
  ENDIF.

  l_wa_param_binding-name = name.
  TRANSLATE l_wa_param_binding-name TO UPPER CASE.

  IF kind IS SUPPLIED.
    READ TABLE parameters_out
          WITH TABLE KEY kind = kind
                         name = l_wa_param_binding-name
               ASSIGNING <l_param_binding>.
  ELSE.
    READ TABLE parameters_out
          WITH KEY name = l_wa_param_binding-name
               ASSIGNING <l_param_binding>.             "#EC CI_SORTSEQ
  ENDIF.

  IF sy-subrc = 0.
    ASSIGN <l_param_binding>-value->* TO <l_param_value>.
    value = <l_param_value>.
  ELSE.
    CLEAR value.
  ENDIF.
ENDMETHOD.


METHOD get_parameters.
  CLEAR parameters.

  IF status <> status_completed.
    RAISE EXCEPTION TYPE /gal/cx_cfw_exception
      EXPORTING
        textid = /gal/cx_cfw_exception=>task_not_completed.
  ENDIF.

  parameters = me->parameters_out.
ENDMETHOD.


METHOD get_result.

  DATA l_exception_text TYPE string.


* RESULT is declared as an EXPORTING parameter instead of a RETURNING parameter
* because call by value would result in unnecessary copying of the large result
* structure!

* Initialize result
  CLEAR result.

* Execute synchronously if execution has not been started yet,
* otherwise wait for task running in separate task
  IF status = status_waiting.
    start_sync( ).
  ELSE.
    wait( ).
  ENDIF.

* Return result
  IF status = status_completed.
    result = parameters_out.
  ELSE.
    IF NOT exception_info-message_id     IS INITIAL AND
       NOT exception_info-message_type   IS INITIAL AND
       NOT exception_info-message_number IS INITIAL.
      MESSAGE ID     exception_info-message_id
              TYPE   exception_info-message_type
              NUMBER exception_info-message_number
              WITH   exception_info-message_var1
                     exception_info-message_var2
                     exception_info-message_var3
                     exception_info-message_var4
              INTO l_exception_text.
    ELSE.
      l_exception_text = exception_info-message_text.
    ENDIF.
    RAISE EXCEPTION TYPE /gal/cx_cfw_exception
      EXPORTING
        textid = /gal/cx_cfw_exception=>custom_exception
        var1   = l_exception_text.
  ENDIF.
ENDMETHOD.


METHOD handle_task_completion.
  DATA l_parameters_out TYPE string.
  DATA l_message(200)   TYPE c.

* Get results from asynchronous function call
  IF keep_context = abap_true.
    RECEIVE RESULTS FROM FUNCTION '/GAL/RFC_PROXY_FUNCTION' KEEPING TASK
      IMPORTING
        parameters_out        = l_parameters_out
        exception_info        = exception_info
      CHANGING
        rfc_route_info        = rfc_route_info
      EXCEPTIONS
        system_failure             = 1  MESSAGE l_message
        communication_failure      = 2  MESSAGE l_message
        authority_integrity_failed = 3
        no_authorization           = 4.
  ELSE.
    RECEIVE RESULTS FROM FUNCTION '/GAL/RFC_PROXY_FUNCTION'
      IMPORTING
        parameters_out        = l_parameters_out
        exception_info        = exception_info
      CHANGING
        rfc_route_info        = rfc_route_info
      EXCEPTIONS
        system_failure             = 1  MESSAGE l_message
        communication_failure      = 2  MESSAGE l_message
        authority_integrity_failed = 3
        no_authorization           = 4.
  ENDIF.

  IF sy-subrc <> 0.
    CLEAR exception_info.

    IF sy-subrc = 1 OR sy-subrc = 2.
      exception_info-message_text = l_message.
    ELSE.
      exception_info-message_id      = sy-msgid.
      exception_info-message_type    = sy-msgty.
      exception_info-message_number  = sy-msgno.
      exception_info-message_var1    = sy-msgv1.
      exception_info-message_var2    = sy-msgv2.
      exception_info-message_var3    = sy-msgv3.
      exception_info-message_var4    = sy-msgv4.
    ENDIF.

    exception_info-system_id       = sy-sysid.
    exception_info-client_id       = sy-mandt.
    exception_info-exception_type  = 'E'.

    IF sy-subrc = 1.
      exception_info-exception_name  = 'SYSTEM_FAILURE'.
    ELSEIF sy-subrc = 2.
      exception_info-exception_name  = 'COMMUNICATION_FAILURE'.
    ELSEIF sy-subrc = 3.
      exception_info-exception_name  = 'AUTHORITY_INTEGRITY_FAILED'.
    ELSEIF sy-subrc = 4.
      exception_info-exception_name  = 'NO_AUTHORIZATION'.
    ENDIF.

    status = status_aborted.
  ELSE.
    /gal/cfw_helper=>deserialize_function_params( EXPORTING xml                = l_parameters_out
                                                  IMPORTING parameter_bindings = parameters_out ).

    status = status_completed.
  ENDIF.
ENDMETHOD.


METHOD reset.
  IF status <> status_waiting.

* Wait for asynchronous execution to complete before resetting task
    IF status = status_running.
      wait( timeout ).
    ENDIF.

* Reset task
    /gal/cfw_helper=>initialize_rfc_route_info( CHANGING rfc_route_info = rfc_route_info ).

    CLEAR parameters_out.
    status = status_waiting.
  ENDIF.
ENDMETHOD.


METHOD set_function.
  IF status <> status_waiting.
    RAISE EXCEPTION TYPE /gal/cx_cfw_exception
      EXPORTING
        textid = /gal/cx_cfw_exception=>task_not_modifiable.
  ENDIF.

  me->function = function.
ENDMETHOD.


METHOD set_parameter.
  DATA l_wa_param_binding LIKE LINE OF parameters_in.

  IF status <> status_waiting.
    RAISE EXCEPTION TYPE /gal/cx_cfw_exception
      EXPORTING
        textid = /gal/cx_cfw_exception=>task_not_modifiable.
  ENDIF.

  l_wa_param_binding-name = name.
  TRANSLATE l_wa_param_binding-name TO UPPER CASE.

  DELETE parameters_in WHERE name = l_wa_param_binding-name. "#EC CI_SORTSEQ

  l_wa_param_binding-kind = kind.

  GET REFERENCE OF value INTO l_wa_param_binding-value.

  INSERT l_wa_param_binding INTO TABLE parameters_in.
ENDMETHOD.


METHOD set_parameters.
  IF status <> status_waiting.
    RAISE EXCEPTION TYPE /gal/cx_cfw_exception
      EXPORTING
        textid = /gal/cx_cfw_exception=>task_not_modifiable.
  ENDIF.

  me->parameters_in = parameters.
ENDMETHOD.


METHOD set_rfc_route_info.
  IF status <> status_waiting.
    RAISE EXCEPTION TYPE /gal/cx_cfw_exception
      EXPORTING
        textid = /gal/cx_cfw_exception=>task_not_modifiable.
  ENDIF.

  me->rfc_route_info = rfc_route_info.
ENDMETHOD.


METHOD start_async.
  DATA l_parameters_in TYPE string.
  DATA l_message(200)  TYPE c.

* Save information for context handling
  me->keep_context = keep_context.

* Serialize inbound parameters
  /gal/cfw_helper=>serialize_function_params( EXPORTING parameter_bindings = parameters_in
                                              IMPORTING xml                = l_parameters_in ).

* Invoke asynchronous function call
  CALL FUNCTION '/GAL/RFC_PROXY_FUNCTION'
    STARTING NEW TASK context_id
    CALLING handle_task_completion ON END OF TASK
    EXPORTING
      function_name         = function
      parameters_in         = l_parameters_in
    CHANGING
      rfc_route_info        = rfc_route_info
    EXCEPTIONS
      system_failure        = 1 MESSAGE l_message
      communication_failure = 2 MESSAGE l_message.
  IF sy-subrc <> 0.
    CLEAR exception_info.

    exception_info-message_text    = l_message.
    exception_info-system_id       = sy-sysid.
    exception_info-client_id       = sy-mandt.
    exception_info-exception_type  = 'E'.

    IF sy-subrc = 1.
      exception_info-exception_name  = 'SYSTEM_FAILURE'.
    ELSE.
      exception_info-exception_name  = 'COMMUNICATION_FAILURE'.
    ENDIF.

    status = status_aborted.
  ELSE.
    status = status_running.
  ENDIF.
ENDMETHOD.


METHOD start_sync.
  DATA l_parameters_in  TYPE string.
  DATA l_parameters_out TYPE string.

* Serialize inbound parameters
  /gal/cfw_helper=>serialize_function_params( EXPORTING parameter_bindings = parameters_in
                                              IMPORTING xml                = l_parameters_in ).

* Invoke synchronous function call
  CALL FUNCTION '/GAL/RFC_PROXY_FUNCTION'
    EXPORTING
      function_name              = function
      parameters_in              = l_parameters_in
    IMPORTING
      parameters_out             = l_parameters_out
      exception_info             = exception_info
    CHANGING
      rfc_route_info             = rfc_route_info
    EXCEPTIONS
      no_authorization           = 1
      authority_integrity_failed = 2.
  IF sy-subrc <> 0.
    CLEAR exception_info.
    IF sy-subrc = 1.
      exception_info-exception_name  = 'NO_AUTHORIZATION'.
    ELSE.
      exception_info-exception_name  = 'AUTHORITY_INTEGRITY_FAILED'.
    ENDIF.
    exception_info-message_id      = sy-msgid.
    exception_info-message_type    = sy-msgty.
    exception_info-message_number  = sy-msgno.
    exception_info-message_var1    = sy-msgv1.
    exception_info-message_var2    = sy-msgv2.
    exception_info-message_var3    = sy-msgv3.
    exception_info-message_var4    = sy-msgv4.
    exception_info-system_id       = sy-sysid.
    exception_info-client_id       = sy-mandt.
    exception_info-rfc_destination = '__ANY__'.
    exception_info-exception_type  = 'E'.
    status = status_aborted.
  ELSE.
    IF exception_info-exception_type IS INITIAL.
      /gal/cfw_helper=>deserialize_function_params( EXPORTING xml                = l_parameters_out
                                                    IMPORTING parameter_bindings = parameters_out ).
      status = status_completed.
    ELSE.
      status = status_aborted.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD wait.
  IF status = status_running.
    IF timeout IS INITIAL.
      WAIT UNTIL status <> status_running.
    ELSE.
      WAIT UNTIL status <> status_running UP TO timeout SECONDS.
      IF sy-subrc = 8.
        RAISE EXCEPTION TYPE /gal/cx_cfw_exception
          EXPORTING
            textid = /gal/cx_cfw_exception=>async_rfc_wait_timeout
            var1   = function.
      ENDIF.
    ENDIF.

    IF sy-subrc <> 0.
      status = status_aborted.

      RAISE EXCEPTION TYPE /gal/cx_cfw_exception
        EXPORTING
          textid = /gal/cx_cfw_exception=>async_rfc_wait_error
          var1   = function.
    ENDIF.
  ENDIF.
ENDMETHOD.
ENDCLASS.
