FUNCTION /gal/lock_get_sync_timestamp.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(LOCK_ID) TYPE  /GAL/LOCK_ID
*"     REFERENCE(CLIENT) TYPE  MANDT OPTIONAL
*"  EXPORTING
*"     REFERENCE(SYNC_TIMESTAMP) TYPE  TIMESTAMPL
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

* Initialize result
  CLEAR sync_timestamp.

* Follow RFC route
  cfw_follow_rfc_route rfc_route_info.
  cfw_remote_coding.

* Get timestamp from lock table
  SELECT SINGLE sync_timestamp FROM /gal/lock01 INTO sync_timestamp
                              WHERE lock_id = lock_id
                                AND client  = client.     "#EC CI_SUBRC
ENDFUNCTION.
