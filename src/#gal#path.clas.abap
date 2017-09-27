class /GAL/PATH definition
  public
  create public .

public section.
  type-pools ABAP .

  class-data CLIENT_PATH_IS_CASE_SENSITIVE type ABAP_BOOL read-only .
  class-data CLIENT_PATH_SEPARATOR type STRING read-only .
  class-data SERVER_PATH_IS_CASE_SENSITIVE type ABAP_BOOL read-only .
  class-data SERVER_PATH_SEPARATOR type STRING read-only .

  class-methods APPEND_SEPARATOR
    importing
      !PATH type CSEQUENCE
      !SEPARATOR type CSEQUENCE
    returning
      value(RESULT) type STRING .
  class-methods CLASS_CONSTRUCTOR .
  class-methods COMBINE
    importing
      !PATH1 type CSEQUENCE
      !PATH2 type CSEQUENCE
      !SEPARATOR type CSEQUENCE
    returning
      value(RESULT) type STRING .
  class-methods GET_CLIENT_PATH_INFO
    exporting
      value(SEPARATOR) type STRING
      value(IS_CASE_SENSITIVE) type ABAP_BOOL .
  class-methods GET_CLIENT_PATH_SEPARATOR
    returning
      value(SEPARATOR) type STRING .
  class-methods GET_FILE_NAME
    importing
      !PATH type CSEQUENCE
      !SEPARATOR type CSEQUENCE
    returning
      value(FILE_NAME) type STRING .
  class-methods GET_ROOT_PATH
    importing
      !PATH type CSEQUENCE
      !SEPARATOR type CSEQUENCE
    returning
      value(ROOT_PATH) type STRING .
  class-methods GET_SERVER_PATH_INFO
    importing
      !RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO optional
    exporting
      value(SEPARATOR) type STRING
      value(IS_CASE_SENSITIVE) type ABAP_BOOL
    raising
      /GAL/CX_IO_EXCEPTION .
  class-methods GET_SERVER_PATH_SEPARATOR
    importing
      !RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO optional
    returning
      value(SEPARATOR) type STRING
    raising
      /GAL/CX_IO_EXCEPTION .
  class-methods REMOVE_SEPARATOR
    importing
      !PATH type CSEQUENCE
      !SEPARATOR type CSEQUENCE
    returning
      value(RESULT) type STRING .
  class-methods STARTS_WITH
    importing
      !PATH type CSEQUENCE
      !PART type CSEQUENCE
      !SEPARATOR type CSEQUENCE
      !CASE_SENSITIVE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RESULT) type ABAP_BOOL .
protected section.
private section.
ENDCLASS.



CLASS /GAL/PATH IMPLEMENTATION.


METHOD append_separator.
  DATA l_path       TYPE string.
  DATA l_separator  TYPE string.
  DATA l_result     TYPE abap_bool.

  l_path      = path.
  l_separator = separator.

* Check if separator needs to be appended
  l_result = /gal/string=>ends_with( input = l_path
                                     part  = l_separator ).

* Append separator when needed
  IF l_result = abap_true.
    result = l_path.
  ELSE.
    CONCATENATE l_path l_separator INTO result.
  ENDIF.
ENDMETHOD.


METHOD class_constructor.
  get_client_path_info( ).

  TRY.
      get_server_path_separator( ).

    CATCH /gal/cx_io_exception.                         "#EC NO_HANDLER
      " Cannot occur because no RFC_ROUTE_INFO is supplied

  ENDTRY.
ENDMETHOD.


METHOD combine.
  DATA l_path1 TYPE string.
  DATA l_path2 TYPE string.

  l_path1 = append_separator( path      = path1
                              separator = separator ).
  l_path2 = path2.

  CONCATENATE l_path1 l_path2 INTO result.
ENDMETHOD.


METHOD get_client_path_info.
  DATA l_platform TYPE i.

  IF client_path_separator IS INITIAL.
    cl_gui_frontend_services=>get_platform( RECEIVING  platform = l_platform
                                            EXCEPTIONS OTHERS   = 1 ).

    IF sy-subrc = 0 AND ( l_platform = cl_gui_frontend_services=>platform_windows95 OR
                          l_platform = cl_gui_frontend_services=>platform_windows98 OR
                          l_platform = cl_gui_frontend_services=>platform_nt351     OR
                          l_platform = cl_gui_frontend_services=>platform_nt40      OR
                          l_platform = cl_gui_frontend_services=>platform_nt50      OR
                          l_platform = cl_gui_frontend_services=>platform_windowsxp ).
      client_path_separator         = '\'.
      client_path_is_case_sensitive = abap_false.
    ELSE.
      client_path_separator         = '/'.
      client_path_is_case_sensitive = abap_true.
    ENDIF.
  ENDIF.

  separator         = client_path_separator.
  is_case_sensitive = client_path_is_case_sensitive.
ENDMETHOD.


METHOD get_client_path_separator.
  get_client_path_info( IMPORTING separator = separator ).
ENDMETHOD.


METHOD get_file_name.
  DATA l_path       TYPE string.
  DATA l_separator  TYPE string.

  DATA l_components TYPE STANDARD TABLE OF string.

  IF path IS INITIAL.
    RETURN.
  ENDIF.

* Get file name (last component of path)
  l_path      = path.
  l_separator = separator.

  SPLIT l_path AT l_separator INTO TABLE l_components.

  DESCRIBE TABLE l_components LINES sy-tfill.

  READ TABLE l_components INDEX sy-tfill INTO file_name.
ENDMETHOD.


METHOD get_root_path.
  DATA l_path       TYPE string.
  DATA l_separator  TYPE string.

  DATA l_components TYPE STANDARD TABLE OF string.
  DATA l_component1 TYPE string.
  DATA l_component2 TYPE string.
  DATA l_component3 TYPE string.
  DATA l_component4 TYPE string.

  IF path IS INITIAL.
    RETURN.
  ENDIF.

  l_path      = path.
  l_separator = separator.

  SPLIT l_path AT l_separator INTO TABLE l_components.

  READ TABLE l_components INDEX 1 INTO l_component1.

  IF l_component1 IS NOT INITIAL. "Simple path
    IF l_separator = `\`    AND
       l_component1 CP `+:` AND
       l_component1(1) CA `ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz`.
      CONCATENATE l_component1 l_separator INTO root_path.
    ELSE.
      root_path = l_component1.
    ENDIF.

    RETURN.
  ENDIF.

  READ TABLE l_components INDEX 2 INTO l_component2.

  IF l_component2 IS NOT INITIAL. "Rooted path
    root_path = l_separator.
    RETURN.
  ENDIF.

  READ TABLE l_components INDEX 3 INTO l_component3.
  READ TABLE l_components INDEX 4 INTO l_component4.

  IF l_component3 IS NOT INITIAL AND l_component4 IS NOT INITIAL. "UNC path
    CONCATENATE l_separator l_separator l_component3 l_separator l_component4 INTO root_path.
    RETURN.
  ENDIF.

  RETURN. "Unknown path - cannot determine root path
ENDMETHOD.


METHOD get_server_path_info.
  DATA l_message TYPE string.

  IF rfc_route_info IS INITIAL.
    IF server_path_separator IS INITIAL.
      CALL FUNCTION '/GAL/FILE_GET_DIR_SEPARATOR'
        IMPORTING
          separator            = server_path_separator
          case_sensistive_path = server_path_is_case_sensitive
        EXCEPTIONS
          OTHERS               = 0.
    ENDIF.

    separator         = server_path_separator.
    is_case_sensitive = server_path_is_case_sensitive.
  ELSE.
    CALL FUNCTION '/GAL/FILE_GET_DIR_SEPARATOR'
      EXPORTING
        rfc_route_info       = rfc_route_info
      IMPORTING
        separator            = separator
        case_sensistive_path = is_case_sensitive
      EXCEPTIONS
        OTHERS               = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO l_message.

      RAISE EXCEPTION TYPE /gal/cx_io_exception
        EXPORTING
          textid = /gal/cx_io_exception=>communication_error
          var1   = l_message.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD get_server_path_separator.
  get_server_path_info( EXPORTING rfc_route_info = rfc_route_info
                        IMPORTING separator      = separator ).
ENDMETHOD.


METHOD remove_separator.
  DATA l_path       TYPE string.
  DATA l_separator  TYPE string.
  DATA l_result     TYPE abap_bool.
  DATA l_length     TYPE i.

  l_path      = path.
  l_separator = separator.

  l_result = /gal/string=>ends_with( input = l_path
                                     part  = l_separator ).

  IF l_result = abap_true.
    l_length = strlen( l_path ) - strlen( l_separator ).
    result = l_path(l_length).
  ELSE.
    result = l_path.
  ENDIF.
ENDMETHOD.


METHOD starts_with.
  DATA l_path TYPE string.
  DATA l_part TYPE string.

  l_path = append_separator( path      = path
                             separator = separator ).

  l_part = append_separator( path      = part
                             separator = separator ).

  IF case_sensitive = abap_false.
    TRANSLATE l_path TO UPPER CASE.
    TRANSLATE l_part TO UPPER CASE.
  ENDIF.

  result = /gal/string=>starts_with( input = l_path
                                     part  = l_part ).
ENDMETHOD.
ENDCLASS.
