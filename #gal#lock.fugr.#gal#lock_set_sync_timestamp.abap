FUNCTION /gal/lock_set_sync_timestamp.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(LOCK_ID) TYPE  /GAL/LOCK_ID
*"     REFERENCE(CLIENT) TYPE  MANDT OPTIONAL
*"     REFERENCE(SYNC_TIMESTAMP) TYPE  TIMESTAMPL OPTIONAL
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA l_wa_step_info LIKE LINE OF rfc_route_info-step_infos.
  DATA l_wa_lock01    TYPE /gal/lock01.

  cfw_follow_rfc_route rfc_route_info.
  cfw_remote_coding.

* Prepare record for lock table
  l_wa_lock01-lock_id        = lock_id.
  l_wa_lock01-client         = client.
  l_wa_lock01-sync_timestamp = sync_timestamp.

  READ TABLE rfc_route_info-step_infos INDEX 1 INTO l_wa_step_info.
  IF sy-subrc <> 0.
    /gal/cfw_helper=>update_rfc_route_step_info( CHANGING rfc_route_step_info = l_wa_step_info ).
  ENDIF.

  MOVE-CORRESPONDING l_wa_step_info TO l_wa_lock01.

  MODIFY /gal/lock01 FROM l_wa_lock01.                    "#EC CI_SUBRC

  CALL FUNCTION 'DB_COMMIT'.
ENDFUNCTION.
