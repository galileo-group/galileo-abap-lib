*&---------------------------------------------------------------------*
*& Report  /GAL/CONFIG_EDITOR
*&---------------------------------------------------------------------*
*& This program can be used to edit the configuration settings stored
*& using the Galileo Configuration Store
*&---------------------------------------------------------------------*

INCLUDE /gal/config_editortop.
INCLUDE /gal/config_editoro01.
INCLUDE /gal/config_editori01.
INCLUDE /gal/config_editorf01.


START-OF-SELECTION.
  PERFORM main.

*&---------------------------------------------------------------------*
*&      Form  main
*&---------------------------------------------------------------------*
*       Run configuration editor
*----------------------------------------------------------------------*
FORM main.

* Perform authority check
  PERFORM authority_check.

* Create application object
  CREATE OBJECT g_application.

* Run application
  g_application->run( ).
ENDFORM.                    "authority_check

*&---------------------------------------------------------------------*
*&      Form  authority_check
*&---------------------------------------------------------------------*
*       Authority check
*
*       This coding is intentionally duplicated from the RUN method of
*       the application object in order to allow Virtual Forge Code
*       Profiler to pick up the authority checks.
*----------------------------------------------------------------------*
FORM authority_check.
  DATA l_exception TYPE REF TO /gal/cx_auth_check_exception.
  DATA l_message   TYPE string.

* Very basic authority check only!
* All further check are done by the object model when trying to actually
* modify data.
  TRY.
      CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
        EXPORTING
          tcode  = '/GAL/CONFIG_EDITOR'
        EXCEPTIONS
          ok     = 0
          OTHERS = 1.
      IF sy-subrc <> 0.
        CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
          EXPORTING
            tcode  = 'SA38'
          EXCEPTIONS
            ok     = 0
            OTHERS = 1.
        IF sy-subrc = 0.
          AUTHORITY-CHECK OBJECT 'S_PROGRAM'
                              ID 'P_GROUP'  DUMMY
                              ID 'P_ACTION' FIELD 'SUBMIT'.
        ENDIF.
      ENDIF.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE /gal/cx_auth_check_exception
          EXPORTING
            textid = /gal/cx_auth_check_exception=>not_authorized.
      ENDIF.

    CATCH /gal/cx_auth_check_exception INTO l_exception.
      l_message = l_exception->get_text( ).

      MESSAGE l_message TYPE 'I'.
      LEAVE PROGRAM.
  ENDTRY.
ENDFORM.                    "authority_check
