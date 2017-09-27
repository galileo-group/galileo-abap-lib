*----------------------------------------------------------------------*
***INCLUDE /GAL/LCOMMON_DIALOGI01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  EXIT_01XX  INPUT
*&---------------------------------------------------------------------*
*       Cancel dialog
*----------------------------------------------------------------------*
MODULE exit_01xx INPUT.
  g_dynp_01xx-result = g_dynp_01xx-user_command.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " EXIT_01XX  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT_2000  INPUT
*&---------------------------------------------------------------------*
*       Cancel backend file browser
*----------------------------------------------------------------------*
MODULE exit_2000 INPUT.
  CLEAR g_dynp_2000-selected_file.

  LEAVE TO SCREEN 0.
ENDMODULE.                 " EXIT_2000  INPUT

*&---------------------------------------------------------------------*
*&      Module  PROCESS_USER_COMMAND_01XX  INPUT
*&---------------------------------------------------------------------*
*       Close dialog when a button has been pressed
*----------------------------------------------------------------------*
MODULE process_user_command_01xx INPUT.
  PERFORM process_user_command_01xx.
ENDMODULE.                 " PROCESS_USER_COMMAND_01XX  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_VALUE_0400  INPUT
*&---------------------------------------------------------------------*
*       Get input value
*----------------------------------------------------------------------*
MODULE get_value_0400 INPUT.
  PERFORM get_value_0400.
ENDMODULE.                 " GET_VALUE_0400  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_VALUE_0401  INPUT
*&---------------------------------------------------------------------*
*       Get input value
*----------------------------------------------------------------------*
MODULE get_value_0401 INPUT.
  PERFORM get_value_0401.
ENDMODULE.                 " GET_VALUE_0401  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_OPTIONS_0500  INPUT
*&---------------------------------------------------------------------*
*       Get selected options
*----------------------------------------------------------------------*
MODULE get_options_0500 INPUT.
  PERFORM get_options_500.
ENDMODULE.                 " GET_OPTIONS_0500  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_OPTIONS_0501  INPUT
*&---------------------------------------------------------------------*
*       Get selected options
*----------------------------------------------------------------------*
MODULE get_options_0501 INPUT.
  PERFORM get_options_501.
ENDMODULE.                 " GET_OPTIONS_0501  INPUT

*&---------------------------------------------------------------------*
*&      Module  pai_0100_user_command  INPUT
*&---------------------------------------------------------------------*
*       Handle user command of backend file browser
*----------------------------------------------------------------------*
MODULE process_user_command_2000 INPUT.
  PERFORM process_user_command_2000.
ENDMODULE.                    "process_user_command_2000 INPUT

*&---------------------------------------------------------------------*
*&      Module  process_user_command_3000  INPUT
*&---------------------------------------------------------------------*
*       Handle user command of choose folder dialog
*----------------------------------------------------------------------*
MODULE process_user_command_3000 INPUT.
  PERFORM process_user_command_3000.
ENDMODULE.
