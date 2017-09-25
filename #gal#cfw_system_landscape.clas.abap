class /GAL/CFW_SYSTEM_LANDSCAPE definition
  public
  create public .

*"* public components of class /GAL/CFW_SYSTEM_LANDSCAPE
*"* do not include other source files here!!!
public section.

  interfaces /GAL/IF_RFC_ROUTE_PROVIDER .

  data APPLICATION type /GAL/CFW_APPLICATION read-only .

  methods CONSTRUCTOR
    importing
      !APPLICATION type /GAL/CFW_APPLICATION default `ANY` .
protected section.
*"* protected components of class /GAL/CFW_SYSTEM_ENV_BASE
*"* do not include other source files here!!!
private section.
*"* private components of class /GAL/CFW_SYSTEM_LANDSCAPE
*"* do not include other source files here!!!

  aliases GET_RFC_ROUTE_INFO
    for /GAL/IF_RFC_ROUTE_PROVIDER~GET_RFC_ROUTE_INFO .
ENDCLASS.



CLASS /GAL/CFW_SYSTEM_LANDSCAPE IMPLEMENTATION.


METHOD /gal/if_rfc_route_provider~get_rfc_route_info.
  DATA l_rfc_route TYPE string.

  DATA l_var1 TYPE string.
  DATA l_var2 TYPE string.
  DATA l_var3 TYPE string.

  IF target_client_id <> 'ANY'.
    SELECT SINGLE rfc_route
             FROM /gal/cfw_rfc01
             INTO l_rfc_route
            WHERE application      = application
              AND purpose          = purpose
              AND target_system_id = target_system_id
              AND target_client_id = target_client_id.
  ELSE.
    SELECT rfc_route
      FROM /gal/cfw_rfc01 UP TO 1 ROWS
      INTO l_rfc_route
     WHERE application      = application
       AND purpose          = purpose
       AND target_system_id = target_system_id.
      EXIT.
    ENDSELECT.
  ENDIF.

  IF sy-subrc = 0.
    CALL METHOD /gal/cfw_helper=>rfc_route_info_from_string
      EXPORTING
        string         = l_rfc_route
      RECEIVING
        rfc_route_info = rfc_route_info.
  ELSE.
    l_var1 = target_system_id.
    l_var2 = target_client_id.
    l_var3 = purpose.

    RAISE EXCEPTION TYPE /gal/cx_cfw_exception
          EXPORTING textid = /gal/cx_cfw_exception=>no_rfc_route_found
                    var1   = l_var1
                    var2   = l_var2
                    var3   = l_var3.
  ENDIF.
ENDMETHOD.


METHOD constructor.
  me->application = application.
ENDMETHOD.
ENDCLASS.
