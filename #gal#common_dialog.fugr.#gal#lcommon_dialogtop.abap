FUNCTION-POOL /gal/common_dialog.           "MESSAGE-ID ..

TYPE-POOLS abap.
TYPE-POOLS vrm.

TYPES BEGIN OF gt_subscreen_info.
TYPES   program   TYPE syrepid.
TYPES   dynpro_no TYPE sydynnr.
TYPES END OF gt_subscreen_info.

DATA BEGIN OF g_dynp_01xx.
DATA   subscreen01       TYPE gt_subscreen_info.
DATA   subscreen02       TYPE gt_subscreen_info.
DATA   subscreen03       TYPE gt_subscreen_info.
DATA   subscreen04       TYPE gt_subscreen_info.

DATA   title             TYPE string.
DATA   icon              TYPE icon_d.
DATA   message           TYPE string.
DATA   style             TYPE string.
DATA   default_result    TYPE string.
DATA   allowed_results   TYPE STANDARD TABLE OF string.

DATA   button1_icon      TYPE icon_d.
DATA   button1_text      TYPE string.
DATA   button2_icon      TYPE icon_d.
DATA   button2_text      TYPE string.
DATA   button3_icon      TYPE icon_d.
DATA   button3_text      TYPE string.

DATA   input_style       TYPE string.
DATA   prompt            TYPE string.
DATA   value             TYPE REF TO data.
DATA   max_length        TYPE i.
DATA   is_input_required TYPE abap_bool.

DATA   options_style     TYPE string.
DATA   options           TYPE /gal/cdlg_options.

DATA   user_command      TYPE syucomm.
DATA   result            TYPE string.
DATA END OF g_dynp_01xx.

DATA BEGIN OF g_dynp_0200.
DATA   icon      TYPE icon_d.
DATA   message   TYPE string.

DATA   message01 TYPE string.
DATA   message02 TYPE string.
DATA   message03 TYPE string.
DATA   message04 TYPE string.
DATA   message05 TYPE string.
DATA   message06 TYPE string.
DATA   message07 TYPE string.
DATA   message08 TYPE string.
DATA   message09 TYPE string.
DATA   message10 TYPE string.
DATA END OF g_dynp_0200.

DATA BEGIN OF g_dynp_0300.                                  "#EC NEEDED
DATA   input_style TYPE string.
DATA END OF g_dynp_0300.

DATA BEGIN OF g_dynp_0301.                                  "#EC NEEDED
DATA   input_style TYPE string.
DATA END OF g_dynp_0301.

DATA BEGIN OF g_dynp_0302.                                  "#EC NEEDED
DATA   input_style     TYPE string.
DATA   default_result  TYPE string.
DATA   allowed_results TYPE STANDARD TABLE OF string.
DATA END OF g_dynp_0302.

DATA BEGIN OF g_dynp_0303.                                  "#EC NEEDED
DATA   input_style     TYPE string.
DATA   default_result  TYPE string.
DATA   allowed_results TYPE STANDARD TABLE OF string.
DATA END OF g_dynp_0303.

DATA BEGIN OF g_dynp_0304.                                  "#EC NEEDED
DATA   input_style     TYPE string.
DATA   default_result  TYPE string.
DATA   allowed_results TYPE STANDARD TABLE OF string.
DATA END OF g_dynp_0304.

DATA BEGIN OF g_dynp_0305.
DATA   input_style     TYPE string.
DATA   default_result  TYPE string.
DATA   allowed_results TYPE STANDARD TABLE OF string.

DATA   option1_icon    TYPE icon_d.
DATA   option1_text    TYPE string.
DATA   option2_icon    TYPE icon_d.
DATA   option2_text    TYPE string.

DATA   button_1(40)    TYPE c.
DATA   button_2(40)    TYPE c.
DATA END OF g_dynp_0305.

DATA BEGIN OF g_dynp_0306.
DATA   input_style     TYPE string.
DATA   default_result  TYPE string.
DATA   allowed_results TYPE STANDARD TABLE OF string.

DATA   button1_icon    TYPE icon_d.
DATA   button1_text    TYPE string.
DATA   button2_icon    TYPE icon_d.
DATA   button2_text    TYPE string.
DATA   button3_icon    TYPE icon_d.
DATA   button3_text    TYPE string.

DATA   button1(40)     TYPE c.
DATA   button2(40)     TYPE c.
DATA   button3(40)     TYPE c.
DATA END OF g_dynp_0306.

DATA BEGIN OF g_dynp_0400.
DATA   prompt            TYPE string.
DATA   value             TYPE REF TO data.
DATA   input             TYPE string.
DATA   max_length        TYPE i.
DATA   is_input_required TYPE abap_bool.
DATA   is_initialized    TYPE abap_bool.
DATA END OF g_dynp_0400.

DATA BEGIN OF g_dynp_0401.
DATA   prompt            TYPE string.
DATA   value             TYPE REF TO data.
DATA   input             TYPE string.
DATA   max_length        TYPE i.
DATA   is_input_required TYPE abap_bool.
DATA   is_initialized    TYPE abap_bool.
DATA END OF g_dynp_0401.

DATA BEGIN OF g_dynp_0500.
DATA   options          TYPE /gal/cdlg_options.
DATA   options_style    TYPE string.

DATA   option01         TYPE abap_bool.
DATA   option02         TYPE abap_bool.
DATA   option03         TYPE abap_bool.
DATA   option04         TYPE abap_bool.
DATA   option05         TYPE abap_bool.
DATA   option06         TYPE abap_bool.
DATA   option07         TYPE abap_bool.
DATA   option08         TYPE abap_bool.
DATA   option09         TYPE abap_bool.
DATA   option10         TYPE abap_bool.
DATA   label01          TYPE string.
DATA   label02          TYPE string.
DATA   label03          TYPE string.
DATA   label04          TYPE string.
DATA   label05          TYPE string.
DATA   label06          TYPE string.
DATA   label07          TYPE string.
DATA   label08          TYPE string.
DATA   label09          TYPE string.
DATA   label10          TYPE string.
DATA END OF g_dynp_0500.

DATA BEGIN OF g_dynp_0501.
DATA   options          TYPE /gal/cdlg_options.
DATA   options_style    TYPE string.

DATA   option01         TYPE abap_bool.
DATA   option02         TYPE abap_bool.
DATA   option03         TYPE abap_bool.
DATA   option04         TYPE abap_bool.
DATA   option05         TYPE abap_bool.
DATA   option06         TYPE abap_bool.
DATA   option07         TYPE abap_bool.
DATA   option08         TYPE abap_bool.
DATA   option09         TYPE abap_bool.
DATA   option10         TYPE abap_bool.
DATA   label01          TYPE string.
DATA   label02          TYPE string.
DATA   label03          TYPE string.
DATA   label04          TYPE string.
DATA   label05          TYPE string.
DATA   label06          TYPE string.
DATA   label07          TYPE string.
DATA   label08          TYPE string.
DATA   label09          TYPE string.
DATA   label10          TYPE string.
DATA END OF g_dynp_0501.

DATA BEGIN OF g_dynp_2000.
DATA   container             TYPE REF TO cl_gui_custom_container.
DATA   backend_file_browser  TYPE REF TO /gal/backend_file_browser.
DATA   access                TYPE x.
DATA   title                 TYPE string.
DATA   file_type(80)         TYPE c.
DATA   selected_file         TYPE string.
DATA   rfc_route_info        TYPE /gal/rfc_route_info.
DATA   base_directory        TYPE string.
DATA   initial_directory     TYPE string.
DATA   initial_file          TYPE string.
DATA   filter                TYPE string.
DATA   exception             TYPE REF TO cx_root.
DATA END OF g_dynp_2000.

CLASS lcl_folder_tree_dialog DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING container_name TYPE c
                store          TYPE REF TO /gal/config_store OPTIONAL.
    METHODS populate_tree.
    METHODS get_node
      IMPORTING node_key    TYPE tm_nodekey
      RETURNING VALUE(node) TYPE REF TO /gal/config_node.
    METHODS handle_user_command
      IMPORTING user_command TYPE syucomm.
    METHODS set_ui_field_value
      IMPORTING field_name  TYPE clike
                field_value TYPE any.
    METHODS free.
    METHODS handle_expand_no_children FOR EVENT expand_no_children OF cl_simple_tree_model
      IMPORTING node_key.
    METHODS handle_double_click FOR EVENT node_double_click OF cl_simple_tree_model
      IMPORTING node_key.


    DATA tree_container TYPE REF TO cl_gui_custom_container.
    DATA tree           TYPE REF TO cl_simple_tree_model.
    DATA store          TYPE REF TO /gal/config_store.
    DATA selected_node  TYPE REF TO /gal/config_node.
    DATA root_node      TYPE REF TO /gal/config_node.
    DATA ui_program     TYPE progname.

ENDCLASS.

DATA BEGIN OF g_dynp_3000.
DATA title            TYPE string.
DATA store            TYPE REF TO /gal/config_store.
DATA folder_tree      TYPE REF TO lcl_folder_tree_dialog.
DATA target_id        TYPE /gal/config_key_id.
DATA target_name      TYPE /gal/config_key_name.
DATA target_path      TYPE string.
DATA exit_ucomm       TYPE syucomm.
DATA END OF g_dynp_3000.
