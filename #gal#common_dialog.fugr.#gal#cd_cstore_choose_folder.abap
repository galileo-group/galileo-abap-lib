FUNCTION /gal/cd_cstore_choose_folder.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(TITLE) TYPE  STRING
*"     REFERENCE(CONFIG_STORE) TYPE REF TO  /GAL/CONFIG_STORE
*"  EXPORTING
*"     REFERENCE(RESULT) TYPE  STRING
*"     REFERENCE(TARGET_ID) TYPE  /GAL/CONFIG_KEY_ID
*"     REFERENCE(TARGET_NAME) TYPE  /GAL/CONFIG_KEY_NAME
*"     REFERENCE(TARGET_PATH) TYPE  STRING
*"----------------------------------------------------------------------

  CLEAR target_id.
  CLEAR target_name.
  CLEAR target_path.

  g_dynp_3000-title = title.
  g_dynp_3000-store = config_store.

  CALL SCREEN '3000' STARTING AT 80 5 ENDING AT 180 30.

  result = g_dynp_3000-exit_ucomm.

  IF result = /gal/common_dialog=>dlg_result_ok.
    target_id   = g_dynp_3000-target_id.
    target_name = g_dynp_3000-target_name.
    target_path = g_dynp_3000-target_path.
  ENDIF.

  CLEAR g_dynp_3000-folder_tree.

ENDFUNCTION.
