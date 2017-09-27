*----------------------------------------------------------------------*
*       CLASS /GAL/TRACE DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class /GAL/TRACE definition
  public
  final
  create public .

*"* public components of class /GAL/TRACE
*"* do not include other source files here!!!
*"* protected components of class /GAL/TRACE
*"* do not include other source files here!!!
public section.
  type-pools ABAP .

  class-data CONTEXT type ref to /GAL/TRACE_CONTEXT read-only .
  class-data INDENT_LEVEL type I read-only .
  class-data LISTENERS type /GAL/TRACE_LISTENERS read-only .

  class-methods ADD_LISTENER
    importing
      !LISTENER type ref to /GAL/TRACE_LISTENER .
  class-methods CLASS_CONSTRUCTOR .
  class-methods CLEANUP .
  class-methods FLUSH .
  class-methods INDENT .
  class-methods REMOVE_LISTENER
    importing
      !LISTENER type ref to /GAL/TRACE_LISTENER .
  class-methods SET_INDENT_LEVEL
    importing
      !INDENT_LEVEL type I .
  class-methods UNINDENT .
  class-methods WRITE_ANY
    importing
      !VALUE type ANY optional
      !CONTEXT_INFO type STRING optional
      !CALLSTACK_OFFSET type I optional
      !NO_CALLER_DETERMINATION type ABAP_BOOL optional
      !NO_FLUSH type ABAP_BOOL optional .
  class-methods WRITE_CALLSTACK
    importing
      !CUSTOM_CALLSTACK type ABAP_CALLSTACK optional
      !CALLSTACK_OFFSET type I optional
      !NUMBER_OF_LINES type I optional
      !NO_FLUSH type ABAP_BOOL optional .
  class-methods WRITE_ERROR
    importing
      !CALLSTACK_OFFSET type I optional
      !NO_CALLER_DETERMINATION type ABAP_BOOL optional
      !NO_FLUSH type ABAP_BOOL optional .
  class-methods WRITE_EXCEPTION
    importing
      !EXCEPTION type ref to CX_ROOT
      !CALLSTACK_OFFSET type I optional
      !NO_CALLER_DETERMINATION type ABAP_BOOL optional
      !NO_FLUSH type ABAP_BOOL optional .
  class-methods WRITE_OBJECT
    importing
      !OBJECT type ref to OBJECT
      !CONTEXT_INFO type STRING optional
      !CALLSTACK_OFFSET type I optional
      !NO_CALLER_DETERMINATION type ABAP_BOOL optional
      !NO_FLUSH type ABAP_BOOL optional .
  class-methods WRITE_STRUCTURE
    importing
      !STRUCTURE type ANY
      !CONTEXT_INFO type STRING optional
      !CALLSTACK_OFFSET type I optional
      !NO_CALLER_DETERMINATION type ABAP_BOOL optional
      !NO_FLUSH type ABAP_BOOL optional .
  class-methods WRITE_TABLE
    importing
      !TABLE type ANY TABLE
      !CONTEXT_INFO type STRING optional
      !CALLSTACK_OFFSET type I optional
      !NO_CALLER_DETERMINATION type ABAP_BOOL optional
      !NO_FLUSH type ABAP_BOOL optional .
  class-methods WRITE_TEXT
    importing
      !TEXT type CSEQUENCE optional
      !VAR01 type ANY optional
      !VAR02 type ANY optional
      !VAR03 type ANY optional
      !VAR04 type ANY optional
      !VAR05 type ANY optional
      !VAR06 type ANY optional
      !VAR07 type ANY optional
      !VAR08 type ANY optional
      !VAR09 type ANY optional
      !VAR10 type ANY optional
      !CONTEXT_INFO type STRING optional
      !CALLSTACK_OFFSET type I optional
      !NO_CALLER_DETERMINATION type ABAP_BOOL optional
      !NO_FLUSH type ABAP_BOOL optional .
  PROTECTED SECTION.
*"* private components of class /GAL/TRACE
*"* do not include other source files here!!!
private section.

  class-data MESSAGE_STACK type ref to /GAL/MESSAGE_STACK .

  class-methods GET_CALLER_INFO
    importing
      !CALLSTACK_OFFSET type I optional
    returning
      value(CALLER_INFO) type STRING .
  class-methods GET_FIELD_INFO
    importing
      !TYPE_DESCR type ref to CL_ABAP_TYPEDESCR
    returning
      value(FIELD_INFO) type DFIES .
  class-methods SEND_TO_LISTENERS
    importing
      !TEXT type STRING
      !CONTEXT_INFO type STRING optional
      !CALLER_INFO type STRING optional
      !NO_FLUSH type ABAP_BOOL optional .
ENDCLASS.



CLASS /GAL/TRACE IMPLEMENTATION.


  METHOD add_listener.
    INSERT listener INTO TABLE listeners.
  ENDMETHOD.                    "add_listener


METHOD class_constructor.
  DATA l_config_store   TYPE REF TO /gal/config_store_local.
  DATA l_config_folders TYPE /gal/config_nodes.
  DATA l_config_folder  TYPE REF TO /gal/config_node.
  DATA l_config_node    TYPE REF TO /gal/config_node.

  DATA l_class          TYPE string.
  DATA l_is_default     TYPE abap_bool.

  DATA l_listener       TYPE REF TO /gal/trace_listener.

* Create default listeners defined in configuration store
  TRY.
      CREATE OBJECT l_config_store.

      l_config_folder  = l_config_store->get_node( path = `/Galileo Group AG/Open Source Components/Tracing/Listeners` ). "#EC NOTEXT
      l_config_folders = l_config_folder->get_child_nodes( /gal/config_node=>const_node_type_folder ).

      LOOP AT l_config_folders INTO l_config_folder.
        TRY.
            l_config_node = l_config_folder->get_child_node( `Class` ). "#EC NOTEXT
            l_config_node->get_value( IMPORTING value = l_class ).

            l_config_node = l_config_folder->get_child_node( `Is Default` ). "#EC NOTEXT
            l_config_node->get_value( IMPORTING value = l_is_default ).

            IF l_is_default = abap_true.
              CREATE OBJECT l_listener TYPE (l_class).

              add_listener( l_listener ).
            ENDIF.

          CATCH /gal/cx_config_exception.               "#EC NO_HANDLER
            " Ignore invalid configuration entries

        ENDTRY.
      ENDLOOP.

    CATCH /gal/cx_config_exception.                     "#EC NO_HANDLER
      " Ignore configuration errors

  ENDTRY.

  CREATE OBJECT context.
  CREATE OBJECT message_stack.

  cleanup( ).
ENDMETHOD.                    "class_constructor


  METHOD cleanup.
    FIELD-SYMBOLS <l_listener> TYPE REF TO /gal/trace_listener.

    LOOP AT listeners ASSIGNING <l_listener>.
      CALL METHOD <l_listener>->cleanup.
    ENDLOOP.
  ENDMETHOD.                    "CLEANUP


METHOD flush.
  FIELD-SYMBOLS <l_listener> TYPE REF TO /gal/trace_listener.

  LOOP AT listeners ASSIGNING <l_listener>.
    <l_listener>->flush( ).
  ENDLOOP.
ENDMETHOD.                    "flush


METHOD get_caller_info.
  DATA l_callstack        TYPE abap_callstack.
  DATA l_callstack_offset TYPE i.

  DATA l_program          TYPE string.
  DATA l_line             TYPE string.

  FIELD-SYMBOLS <l_callstack> LIKE LINE OF l_callstack.

* Determine caller information
  l_callstack_offset = callstack_offset + 3. "Ignore function SYSTEM_CALLSTACK, method GET_CALLER_INFO and caller

  CALL FUNCTION 'SYSTEM_CALLSTACK'
    EXPORTING
      max_level = l_callstack_offset
    IMPORTING
      callstack = l_callstack.

  READ TABLE l_callstack INDEX l_callstack_offset
       ASSIGNING <l_callstack>.
  IF sy-subrc = 0.

* Get program
    IF <l_callstack>-include IS INITIAL OR <l_callstack>-include CO '?'.
      l_program = <l_callstack>-mainprogram.
    ELSE.
      l_program = <l_callstack>-include.
    ENDIF.

* Get source code line
    l_line = <l_callstack>-line.

    SHIFT l_line LEFT DELETING LEADING space.

* Build caller info string
    IF <l_callstack>-blocktype IS INITIAL.
      CONCATENATE `PROGRAM=` l_program
                `, LINE=`    l_line
             INTO caller_info.
    ELSE.
      CONCATENATE `PROGRAM=` l_program
                `, LINE=`    l_line
                `, ` <l_callstack>-blocktype `=` <l_callstack>-blockname
             INTO caller_info.
    ENDIF.
  ENDIF.
ENDMETHOD.                    "write_text


METHOD get_field_info.
  DATA l_elem_descr TYPE REF TO cl_abap_elemdescr.

* Try to get field info from data dictionary
  CATCH SYSTEM-EXCEPTIONS OTHERS = 0.
    l_elem_descr ?= type_descr.

    l_elem_descr->get_ddic_field( RECEIVING  p_flddescr = field_info
                                  EXCEPTIONS OTHERS     = 1 ).
    IF sy-subrc = 0.
      RETURN.
    ENDIF.
  ENDCATCH.                                               "#EC CI_SUBRC

* Build field info from type description
  field_info-inttype   = type_descr->type_kind.
  field_info-intlen    = type_descr->length.
  field_info-decimals  = type_descr->decimals.

* Determine output length
  CASE field_info-inttype.

    WHEN cl_abap_typedescr=>typekind_int  OR
         cl_abap_typedescr=>typekind_int1 OR
         cl_abap_typedescr=>typekind_int2.

      field_info-outputlen = 10.


    WHEN cl_abap_typedescr=>typekind_packed OR
         cl_abap_typedescr=>typekind_hex    OR
         cl_abap_typedescr=>typekind_xstring.

      field_info-outputlen = type_descr->length * 2.


    WHEN OTHERS.
      field_info-outputlen = type_descr->length.

  ENDCASE.
ENDMETHOD.                    "get_field_info


METHOD indent.
  indent_level = indent_level + 1.
ENDMETHOD.                    "indent


METHOD remove_listener.
  DELETE listeners WHERE table_line = listener.
ENDMETHOD.                    "remove_listener


METHOD send_to_listeners.
  FIELD-SYMBOLS <l_listener> TYPE REF TO /gal/trace_listener.

* Send message information to listeners
  LOOP AT listeners ASSIGNING <l_listener>.
    CALL METHOD <l_listener>->write
      EXPORTING
        text         = text
        context_info = context_info
        caller_info  = caller_info.

    IF no_flush = abap_false.
      <l_listener>->flush( ).
    ENDIF.
  ENDLOOP.
ENDMETHOD.                    "send_to_listeners


METHOD set_indent_level.
  IF indent_level >= 0.
    /gal/trace=>indent_level = indent_level.
  ENDIF.
ENDMETHOD.                    "set_indent_level


METHOD unindent.
  IF indent_level > 0.
    indent_level = indent_level - 1.
  ENDIF.
ENDMETHOD.                    "unindent


METHOD write_any.
  DATA l_callstack_offset TYPE i.

  DATA l_text             TYPE string.
  DATA l_caller_info      TYPE string.

  DATA l_type             TYPE c.

* Check if there is somebody listening
  IF listeners IS INITIAL.
    RETURN.
  ENDIF.

* Save message variables
  message_stack->push( ).

* Call corresponding implementations for tables, structures and objects
  DESCRIBE FIELD value TYPE l_type.

  CASE l_type.

    WHEN 'h'. "Table
      l_callstack_offset = l_callstack_offset + 1.

      write_table( table                   = value
                   context_info            = context_info
                   callstack_offset        = l_callstack_offset
                   no_caller_determination = no_caller_determination
                   no_flush                = no_flush ).

      RETURN.

    WHEN 'u'. "Structure
      l_callstack_offset = l_callstack_offset + 1.

      write_structure( structure               = value
                       context_info            = context_info
                       callstack_offset        = l_callstack_offset
                       no_caller_determination = no_caller_determination
                       no_flush                = no_flush ).

      RETURN.

    WHEN 'v'. "Deep structure
      l_callstack_offset = l_callstack_offset + 1.

      write_structure( structure               = value
                       context_info            = context_info
                       callstack_offset        = l_callstack_offset
                       no_caller_determination = no_caller_determination
                       no_flush                = no_flush ).

      RETURN.

    WHEN 'r'.
      l_callstack_offset = l_callstack_offset + 1.

      write_object( object                  = value
                    context_info            = context_info
                    callstack_offset        = l_callstack_offset
                    no_caller_determination = no_caller_determination
                    no_flush                = no_flush ).

      RETURN.

  ENDCASE.

* Determine caller information
  IF no_caller_determination = abap_false.
    l_caller_info = get_caller_info( callstack_offset ).
  ENDIF.

* Write value to trace
  l_text = /gal/string=>any_to_string( input = value ).

  send_to_listeners( text         = l_text
                     context_info = context_info
                     caller_info  = l_caller_info
                     no_flush     = no_flush ).

* Restore message variables
  message_stack->pop( ).
ENDMETHOD.                    "write_any


METHOD write_callstack.
  DATA l_context_info  TYPE string.
  DATA l_callstack     TYPE abap_callstack.
  DATA l_del_from_line TYPE i.


* Check if there is somebody listening
  IF listeners IS INITIAL.
    RETURN.
  ENDIF.

* Save message variables
  message_stack->push( ).

  IF custom_callstack IS SUPPLIED.
    l_callstack = custom_callstack.
  ELSE.
    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = l_callstack.
    DELETE l_callstack INDEX 1.
  ENDIF.


  IF callstack_offset > 0.
    DELETE l_callstack TO callstack_offset.
  ENDIF.

  IF number_of_lines > 0.
    l_del_from_line = number_of_lines + 1.
    DELETE l_callstack FROM l_del_from_line.
  ENDIF.



  IF l_callstack IS NOT INITIAL.
    l_context_info = callstack_offset.

    SHIFT l_context_info LEFT DELETING LEADING space.

    CONCATENATE `CALLSTACK: OFFSET=` l_context_info
           INTO l_context_info RESPECTING BLANKS.

    write_table( table                   = l_callstack
                 context_info            = 'CALLSTACK'
                 no_caller_determination = abap_true
                 no_flush                = no_flush ).
  ENDIF.

* Restore message variables
  message_stack->pop( ).
ENDMETHOD.                    "write_callstack


METHOD write_error.
  DATA l_context_info     TYPE string.
  DATA l_error            TYPE string.
  DATA l_callstack_offset TYPE i.

* Check if there is somebody listening
  IF listeners IS INITIAL.
    RETURN.
  ENDIF.

* Build message text
  CONCATENATE `MESSAGE=` sy-msgty sy-msgno `(` sy-msgid `)`
         INTO l_context_info RESPECTING BLANKS.

  IF NOT sy-msgid IS INITIAL AND NOT sy-msgty IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_error.
  ELSEIF sy-msgv4 IS NOT INITIAL.
    CONCATENATE sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
           INTO l_error SEPARATED BY '/'.
  ELSEIF sy-msgv3 IS NOT INITIAL.
    CONCATENATE sy-msgv1 sy-msgv2 sy-msgv3
           INTO l_error SEPARATED BY '/'.
  ELSEIF sy-msgv2 IS NOT INITIAL.
    CONCATENATE sy-msgv1 sy-msgv2
           INTO l_error SEPARATED BY '/'.
  ELSEIF sy-msgv1 IS NOT INITIAL.
    l_error = sy-msgv1.
  ELSE.
    l_error = text-000.
  ENDIF.

* Write message text to trace
  l_callstack_offset = callstack_offset + 1.

  write_text( text                    = l_error
              context_info            = l_context_info
              callstack_offset        = l_callstack_offset
              no_caller_determination = no_caller_determination
              no_flush                = no_flush ).
ENDMETHOD.                    "write_error


METHOD write_exception.
  DATA l_context_info     TYPE string.
  DATA l_error            TYPE string.
  DATA l_callstack_offset TYPE i.

  DATA l_class_descr      TYPE REF TO cl_abap_classdescr.

* Check if there is somebody listening
  IF listeners IS INITIAL.
    RETURN.
  ENDIF.

* Get exception message
  l_class_descr ?= cl_abap_typedescr=>describe_by_object_ref( exception ).

  CONCATENATE `EXCEPTION=` l_class_descr->absolute_name
         INTO l_context_info.

  l_error = exception->get_text( ).

* Write message to trace
  l_callstack_offset = callstack_offset + 1.

  write_text( text                    = l_error
              context_info            = l_context_info
              callstack_offset        = l_callstack_offset
              no_caller_determination = no_caller_determination
              no_flush                = no_flush ).
ENDMETHOD.                    "write_exception


METHOD write_object.
  DATA l_text        TYPE string.
  DATA l_caller_info TYPE string.

* Check if there is somebody listening
  IF listeners IS INITIAL.
    RETURN.
  ENDIF.

* Save message variables
  message_stack->push( ).

* Determine caller information
  IF no_caller_determination = abap_false.
    l_caller_info = get_caller_info( callstack_offset ).
  ENDIF.

* Write object to trace
  l_text = /gal/string=>any_to_string( input = object ).

  send_to_listeners( text         = l_text
                     context_info = context_info
                     caller_info  = l_caller_info
                     no_flush     = no_flush ).

* Restore message variables
  message_stack->pop( ).
ENDMETHOD.                    "write_object


METHOD write_structure.
  DATA l_table            TYPE REF TO data.

  DATA l_callstack_offset TYPE i.

  FIELD-SYMBOLS <l_table> TYPE ANY TABLE.

* Check if there is somebody listening
  IF listeners IS INITIAL.
    RETURN.
  ENDIF.

* Save message variables
  message_stack->push( ).

* Create internal table with a sigle line
  CREATE DATA l_table LIKE STANDARD TABLE OF structure.

  ASSIGN l_table->* TO <l_table>.

  INSERT structure INTO TABLE <l_table>.

* User WRITE_TABLE to display structure
  l_callstack_offset = callstack_offset + 1.

  write_table( table                   = <l_table>
               context_info            = context_info
               callstack_offset        = l_callstack_offset
               no_caller_determination = no_caller_determination
               no_flush                = no_flush ).

* Restore message variables
  message_stack->pop( ).
ENDMETHOD.                    "write_structure


METHOD write_table.
  CONSTANTS lc_max_field_width     TYPE i VALUE 80.

  DATA l_caller_info               TYPE string.

  DATA l_table_descr               TYPE REF TO cl_abap_tabledescr.
  DATA l_row_descr                 TYPE REF TO cl_abap_typedescr.
  DATA l_struct_descr              TYPE REF TO cl_abap_structdescr.

  DATA l_components                TYPE abap_component_tab.

  DATA l_field_infos               TYPE ddfields.
  DATA l_field_info                LIKE LINE OF l_field_infos.

  DATA l_no_structure              TYPE abap_bool VALUE abap_false.

  DATA l_length                    TYPE i.

  DATA l_header                    TYPE string.
  DATA l_field                     TYPE string.
  DATA l_hline                     TYPE string.
  DATA l_line                      TYPE string.

  DATA l_value(lc_max_field_width) TYPE c.

  FIELD-SYMBOLS <l_component>  LIKE LINE OF l_components.
  FIELD-SYMBOLS <l_field_info> LIKE LINE OF l_field_infos.
  FIELD-SYMBOLS <l_table_line> TYPE any.
  FIELD-SYMBOLS <l_field>      TYPE any.

* Check if there is somebody listening
  IF listeners IS INITIAL.
    RETURN.
  ENDIF.

* Save message variables
  message_stack->push( ).

* Determine caller information
  IF no_caller_determination = abap_false.
    l_caller_info = get_caller_info( callstack_offset ).
  ENDIF.

* Get the structure of the table
  l_table_descr ?= cl_abap_typedescr=>describe_by_data( table ).
  l_row_descr    = l_table_descr->get_table_line_type( ).

  CATCH SYSTEM-EXCEPTIONS OTHERS = 1.
    l_struct_descr ?= l_row_descr.
  ENDCATCH.

  IF sy-subrc = 0.
    l_struct_descr->get_ddic_field_list( RECEIVING  p_field_list = l_field_infos
                                         EXCEPTIONS OTHERS       = 1 ).
    IF NOT sy-subrc = 0.
      l_components = l_struct_descr->get_components( ).

      LOOP AT l_components ASSIGNING <l_component>.
        l_field_info            = get_field_info( <l_component>-type ).
        l_field_info-lfieldname = <l_component>-name.

        INSERT l_field_info INTO TABLE l_field_infos.
      ENDLOOP.
    ENDIF.
  ELSE.
    l_no_structure          = abap_true.
    l_field_info            = get_field_info( l_row_descr ).
    l_field_info-lfieldname = 'TABLE_LINE'.

    INSERT l_field_info INTO TABLE l_field_infos.
  ENDIF.

* Adjustments for nested structures and tables and header preparation
  l_header = `|`.

  LOOP AT l_field_infos ASSIGNING <l_field_info>.
    IF <l_field_info>-fieldname IS INITIAL.
      IF <l_field_info>-lfieldname IS NOT INITIAL.
        <l_field_info>-fieldname = <l_field_info>-lfieldname.
      ELSEIF <l_field_info>-tabname IS NOT INITIAL.
        <l_field_info>-fieldname = <l_field_info>-tabname.
      ELSE.
        <l_field_info>-fieldname = 'FIELD'.
        WRITE sy-tabix LEFT-JUSTIFIED NO-SIGN TO <l_field_info>-fieldname+5.
      ENDIF.
    ENDIF.

    IF <l_field_info>-inttype = cl_abap_typedescr=>typekind_oref    OR
       <l_field_info>-inttype = cl_abap_typedescr=>typekind_struct1 OR
       <l_field_info>-inttype = cl_abap_typedescr=>typekind_struct2 OR
       <l_field_info>-inttype = cl_abap_typedescr=>typekind_table.
      <l_field_info>-outputlen = 10.
    ENDIF.

    IF <l_field_info>-outputlen > lc_max_field_width OR <l_field_info>-outputlen = 0.
      <l_field_info>-outputlen = lc_max_field_width.
    ENDIF.

    l_length = <l_field_info>-outputlen - strlen( <l_field_info>-fieldname ).

    IF l_length > 0.
      l_field = <l_field_info>-fieldname.

      SHIFT l_field RIGHT BY l_length PLACES.
      SHIFT l_field LEFT BY l_length PLACES CIRCULAR.

      CONCATENATE l_header ` ` l_field ` |`
             INTO l_header RESPECTING BLANKS.
    ELSE.
      CONCATENATE l_header ` ` <l_field_info>-fieldname(<l_field_info>-outputlen) ` |`
             INTO l_header RESPECTING BLANKS.
    ENDIF.
  ENDLOOP.

  l_length = strlen( l_header ).

  SHIFT l_hline RIGHT BY l_length PLACES.
  TRANSLATE l_hline USING ' -'.

* Create header
  send_to_listeners( text         = l_hline
                     context_info = context_info
                     caller_info  = l_caller_info
                     no_flush     = abap_true ).

  send_to_listeners( text     = l_header
                     no_flush = abap_true ).

  send_to_listeners( text     = l_hline
                     no_flush = abap_true ).

* Process rows
  LOOP AT table ASSIGNING <l_table_line>.
    l_line = `|`.

    LOOP AT l_field_infos ASSIGNING <l_field_info>.
      IF l_no_structure = abap_false.
        ASSIGN COMPONENT sy-tabix OF STRUCTURE <l_table_line> TO <l_field>.
      ELSE.
        ASSIGN <l_table_line> TO <l_field>.
      ENDIF.

      IF     <l_field_info>-inttype = cl_abap_typedescr=>typekind_oref.
        CONCATENATE l_line ` [REF]      |` INTO l_line RESPECTING BLANKS.
      ELSEIF <l_field_info>-inttype = cl_abap_typedescr=>typekind_struct1 OR
             <l_field_info>-inttype = cl_abap_typedescr=>typekind_struct2.
        CONCATENATE l_line ` [STRUCT]   |` INTO l_line RESPECTING BLANKS.
      ELSEIF <l_field_info>-inttype = cl_abap_typedescr=>typekind_table.
        CONCATENATE l_line ` [TABLE]    |` INTO l_line RESPECTING BLANKS.
      ELSE.
        l_value = <l_field>.

        CONCATENATE l_line ` ` l_value(<l_field_info>-outputlen) ` |`
               INTO l_line RESPECTING BLANKS.
      ENDIF.
    ENDLOOP.

    send_to_listeners( text     = l_line
                       no_flush = abap_true ).
  ENDLOOP.

* Create footer
  send_to_listeners( text     = l_hline
                     no_flush = abap_true ).

  send_to_listeners( text     = ``
                     no_flush = no_flush ).

* Restore message variables
  message_stack->pop( ).
ENDMETHOD.                    "write_table


METHOD write_text.
  DATA l_text             TYPE string.
  DATA l_caller_info      TYPE string.

* Check if there is somebody listening
  IF listeners IS INITIAL.
    RETURN.
  ENDIF.

* Save message variables
  message_stack->push( ).

* Determine caller information
  IF no_caller_determination = abap_false.
    l_caller_info = get_caller_info( callstack_offset ).
  ENDIF.

* Write text to trace
  l_text = /gal/string=>replace_variables( input  = text
                                           var01  = var01
                                           var02  = var02
                                           var03  = var03
                                           var04  = var04
                                           var05  = var05
                                           var06  = var06
                                           var07  = var07
                                           var08  = var08
                                           var09  = var09
                                           var10  = var10 ).

  send_to_listeners( text         = l_text
                     context_info = context_info
                     caller_info  = l_caller_info
                     no_flush     = no_flush ).

* Restore message variables
  message_stack->pop( ).
ENDMETHOD.                    "write_text
ENDCLASS.
