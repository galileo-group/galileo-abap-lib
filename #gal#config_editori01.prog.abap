*&---------------------------------------------------------------------*
*&  Include           /GAL/CONFIG_EDITORI01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  PAI_0100_EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       Process exit command
*----------------------------------------------------------------------*
MODULE pai_0100_exit_command INPUT.
  g_application->pai_0100_exit_command( ).
ENDMODULE.                 " PAI_0100_EXIT_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0100_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       Process user command
*----------------------------------------------------------------------*
MODULE pai_0100_user_command INPUT.
  g_application->pai_0100_user_command( sy-ucomm ).
ENDMODULE.                 " PAI_0100_USER_COMMAND  INPUT