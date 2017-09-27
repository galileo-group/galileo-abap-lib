*&---------------------------------------------------------------------*
*& Report  /GAL/CFW_DEBUG
*&---------------------------------------------------------------------*
*& Debugging Server for the Galileo Communication Framework
*&---------------------------------------------------------------------*

REPORT /gal/cfw_debug.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
PARAMETERS p_timout TYPE i       DEFAULT 300.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02.
PARAMETERS p_event  TYPE string  DEFAULT '*'.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-b03.
PARAMETERS p_srcsys TYPE /gal/system_id DEFAULT '*'.
PARAMETERS p_srccli TYPE mandt          DEFAULT '*'.
PARAMETERS p_srcusr TYPE syuname        DEFAULT '*'.
SELECTION-SCREEN END OF BLOCK b03.

SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE TEXT-b04.
PARAMETERS p_tarsys TYPE /gal/system_id DEFAULT sy-sysid.
PARAMETERS p_tarcli TYPE mandt          DEFAULT '*'.
PARAMETERS p_tarusr TYPE syuname        DEFAULT '*'.
SELECTION-SCREEN END OF BLOCK b04.

INITIALIZATION.
  PERFORM authority_check.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_TARSYS'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.
  PERFORM main USING p_event  p_timout
                     p_srcsys p_srccli p_srcusr
                     p_tarsys p_tarcli p_tarusr.

*&---------------------------------------------------------------------*
*&      Form  main
*&---------------------------------------------------------------------*
*       Main logic
*----------------------------------------------------------------------*
FORM main
     USING p_event  TYPE string
           p_timout TYPE i
           p_srcsys TYPE /gal/system_id
           p_srccli TYPE mandt
           p_srcusr TYPE syuname
           p_tarsys TYPE /gal/system_id
           p_tarcli TYPE mandt
           p_tarusr TYPE syuname.

  DATA l_server_id TYPE guid_16.

  DATA l_end_date  TYPE d.
  DATA l_end_time  TYPE t.

  DATA l_requests  TYPE STANDARD TABLE OF /gal/cfw_debug02.

  FIELD-SYMBOLS <l_requests> LIKE LINE OF l_requests.

* Remove obsolete Debugging Server entries from registry
  PERFORM cleanup_registry.

* Calculate end date and end time
  l_end_date = sy-datum + ( p_timout DIV 86400 ).        "#EC NUMBER_OK
  l_end_time = sy-uzeit + ( p_timout MOD 86400 ).        "#EC NUMBER_OK

  IF l_end_time < sy-uzeit.
    l_end_date = l_end_date + 1.
  ENDIF.

* Create Debugging Server ID and register Debugging Server
  PERFORM create_server_id CHANGING l_server_id.

  PERFORM register_server USING l_server_id p_event
                                l_end_date  l_end_time
                                p_srcsys p_srccli p_srcusr
                                p_tarsys p_tarcli p_tarusr.

* Main loop: Wait for Debugging request
  DO.
    GET TIME.

    IF l_end_date < sy-datum OR l_end_date = sy-datum AND l_end_time < sy-uzeit.
      EXIT.
    ENDIF.

    SELECT * FROM /gal/cfw_debug02
             INTO TABLE l_requests
            WHERE server_id = l_server_id.
    IF sy-subrc = 0.
      LOOP AT l_requests ASSIGNING <l_requests>.
        IF sy-saprl < '740'.
          CALL FUNCTION 'TH_DEBUG_WP'
            EXPORTING
              wp_no    = <l_requests>-process_no
              wp_index = <l_requests>-process_index
            EXCEPTIONS
              OTHERS   = 0.
        ELSE.
          CALL FUNCTION 'TH_DEBUG_WP'
            EXPORTING
              wp_index = <l_requests>-process_index
              logon_id = <l_requests>-process_logon_id
            EXCEPTIONS
              OTHERS   = 0.
        ENDIF.

        DELETE FROM /gal/cfw_debug02
              WHERE server_id     = <l_requests>-server_id
                AND process_id    = <l_requests>-process_id
                AND process_no    = <l_requests>-process_no
                AND process_index = <l_requests>-process_index. "#EC CI_SUBRC

        CALL FUNCTION 'DB_COMMIT'.
      ENDLOOP.                                            "#EC CI_SUBRC
    ENDIF.

    WAIT UP TO 1 SECONDS.
  ENDDO.

* Unregister Debugging Server
  PERFORM unregister_server USING l_server_id.
ENDFORM.                    "main

*&---------------------------------------------------------------------*
*&      Form  authority_check
*&---------------------------------------------------------------------*
*       Check if user is authorized for process debugging
*----------------------------------------------------------------------*
FORM authority_check.
  AUTHORITY-CHECK OBJECT 'S_ADMI_FCD'
    ID 'S_ADMI_FCD' FIELD 'PADM'.
  IF sy-subrc = 0.
    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
              ID     'OBJTYPE'  FIELD 'DEBUG'
              ID     'ACTVT'    FIELD '03'
              ID     'DEVCLASS' DUMMY
              ID     'OBJNAME'  DUMMY
              ID     'P_GROUP'  DUMMY.
  ENDIF.

  IF sy-subrc <> 0.
    LEAVE TO TRANSACTION 'SU53'.
  ENDIF.
ENDFORM.                    "authority_check

*&---------------------------------------------------------------------*
*&      Form  create_debug_server_id
*&---------------------------------------------------------------------*
*       Create Debugging Server ID
*----------------------------------------------------------------------*
FORM create_server_id
     CHANGING VALUE(p_id) TYPE guid_16.

  p_id = /gal/uuid=>create_raw( ).
ENDFORM.                    "create_server_id

*&---------------------------------------------------------------------*
*&      Form  register_debug_server
*&---------------------------------------------------------------------*
*       Register Debugging Server
*----------------------------------------------------------------------*
FORM register_server
     USING p_id     TYPE guid_16
           p_event  TYPE string
           p_enddat TYPE d
           p_endtim TYPE t
           p_srcsys TYPE /gal/system_id
           p_srccli TYPE mandt
           p_srcusr TYPE syuname
           p_tarsys TYPE /gal/system_id
           p_tarcli TYPE mandt
           p_tarusr TYPE syuname.

  DATA l_wa_registry TYPE /gal/cfw_debug01.

  l_wa_registry-server_id        = p_id.
  l_wa_registry-end_date         = p_enddat.
  l_wa_registry-end_time         = p_endtim.
  l_wa_registry-source_system_id = p_srcsys.
  l_wa_registry-source_client_id = p_srccli.
  l_wa_registry-source_user_id   = p_srcusr.
  l_wa_registry-target_system_id = p_tarsys.
  l_wa_registry-target_client_id = p_tarcli.
  l_wa_registry-target_user_id   = p_tarusr.
  l_wa_registry-event            = p_event.

  INSERT /gal/cfw_debug01 FROM l_wa_registry.             "#EC CI_SUBRC
  COMMIT WORK.
ENDFORM.                    "register_server

*&---------------------------------------------------------------------*
*&      Form  unregister_debug_server
*&---------------------------------------------------------------------*
*       Unregister Debugging Server
*----------------------------------------------------------------------*
FORM unregister_server
     USING VALUE(p_id) TYPE guid_16.

  DELETE FROM /gal/cfw_debug01 WHERE server_id = p_id.    "#EC CI_SUBRC
  COMMIT WORK.
ENDFORM.                    "unregister_server

*&---------------------------------------------------------------------*
*&      Form  cleanup_registry
*&---------------------------------------------------------------------*
*       Cleanup Debugging Server Registry
*----------------------------------------------------------------------*
FORM cleanup_registry.
  DELETE
    FROM /gal/cfw_debug01
   WHERE end_date < sy-datum
      OR end_date = sy-datum AND end_time < sy-uzeit.     "#EC CI_SUBRC

  DELETE                                                "#EC CI_NOFIELD
    FROM /gal/cfw_debug02                                 "#EC CI_SUBRC
   WHERE server_id NOT IN ( SELECT server_id FROM /gal/cfw_debug01 ). "#EC CI_NOWHERE
ENDFORM.                    "cleanup_registry
