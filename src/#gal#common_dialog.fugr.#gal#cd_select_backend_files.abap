FUNCTION /gal/cd_select_backend_files.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO
*"     REFERENCE(ACCESS) TYPE  X DEFAULT '01'
*"     REFERENCE(BASE_DIRECTORY) TYPE  STRING OPTIONAL
*"     REFERENCE(INITIAL_DIRECTORY) TYPE  STRING OPTIONAL
*"     REFERENCE(INITIAL_FILE) TYPE  STRING OPTIONAL
*"     REFERENCE(INITIAL_FILTER) TYPE  STRING OPTIONAL
*"     REFERENCE(FILTER) TYPE  STRING OPTIONAL
*"     REFERENCE(TITLE) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     REFERENCE(SELECTED_FILE) TYPE  STRING
*"     REFERENCE(ABORTED) TYPE  ABAP_BOOL
*"     REFERENCE(EXCEPTION) TYPE REF TO  CX_ROOT
*"----------------------------------------------------------------------

  CLEAR g_dynp_2000.

  g_dynp_2000-rfc_route_info    = rfc_route_info.
  g_dynp_2000-base_directory    = base_directory.
  g_dynp_2000-initial_directory = initial_directory.
  g_dynp_2000-initial_file      = initial_file.
  g_dynp_2000-file_type         = initial_filter.

  IF filter IS INITIAL.
    g_dynp_2000-filter = text-c00.

    IF filter IS INITIAL.
      g_dynp_2000-filter = `All files (*.*)|*.*`.           "#EC NOTEXT
    ENDIF.
  ELSE.
    g_dynp_2000-filter = filter.
  ENDIF.

  g_dynp_2000-access = access.

  IF title IS INITIAL.
    IF access = /gal/backend_file=>access_read.
      g_dynp_2000-title = text-t00.
    ELSE.
      g_dynp_2000-title = text-t01.
    ENDIF.
  ELSE.
    g_dynp_2000-title = title.
  ENDIF.

  CALL SCREEN 2000 STARTING AT 1 1.

  selected_file = g_dynp_2000-selected_file.

  IF g_dynp_2000-backend_file_browser IS NOT INITIAL.
    g_dynp_2000-backend_file_browser->free( EXCEPTIONS OTHERS = 0 ).
    CLEAR g_dynp_2000-backend_file_browser.
  ENDIF.

  IF g_dynp_2000-container IS NOT INITIAL.
    g_dynp_2000-container->free( EXCEPTIONS OTHERS = 0 ).
    CLEAR g_dynp_2000-container.
  ENDIF.

  cl_gui_cfw=>flush( EXCEPTIONS OTHERS = 0 ).

  IF selected_file IS INITIAL.
    exception = g_dynp_2000-exception.
    aborted   = abap_true.
  ELSE.
    CLEAR exception.
    aborted = abap_false.
  ENDIF.
ENDFUNCTION.
