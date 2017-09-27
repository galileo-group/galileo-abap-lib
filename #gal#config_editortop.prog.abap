*&---------------------------------------------------------------------*
*&  Include           /GAL/CONFIG_EDITORTOP
*&---------------------------------------------------------------------*

REPORT /gal/config_editor.

TYPE-POOLS abap.

CONTROLS g_tabstrip TYPE TABSTRIP.

DATA g_application TYPE REF TO /gal/app_config_editor.      "#EC NEEDED

DATA BEGIN OF g_dynp_0110.
DATA   type       TYPE /gal/config_key_type.
DATA   type_text  TYPE string.
DATA   fixed_type TYPE /gal/config_value_type.
DATA   auth_class TYPE /gal/config_auth_class_name.
DATA   btn_change TYPE c LENGTH 30.
DATA END OF g_dynp_0110.

DATA BEGIN OF g_dynp_0120.
DATA   langu      TYPE langu.
DATA   btn_change TYPE c LENGTH 30.
DATA END OF g_dynp_0120.

DATA BEGIN OF g_dynp_0130.
DATA   scope(10)  TYPE c.
DATA   client     TYPE mandt.
DATA   user       TYPE uname.
DATA   type       TYPE /gal/config_value_type.
DATA   btn_change TYPE c LENGTH 30.
DATA END OF g_dynp_0130.

DATA BEGIN OF g_dynp_0150.
DATA   value TYPE c LENGTH 255.
DATA END OF g_dynp_0150.
