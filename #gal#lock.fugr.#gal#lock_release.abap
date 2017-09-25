FUNCTION /gal/lock_release.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(LOCK_ID) TYPE  /GAL/LOCK_ID
*"     REFERENCE(CLIENT) TYPE  MANDT OPTIONAL
*"     REFERENCE(SESSION_ID) TYPE  /GAL/SESSION_ID DEFAULT
*"       /GAL/SESSION=>ID
*"     REFERENCE(FORCE) TYPE  ABAP_BOOL DEFAULT ABAP_FALSE
*"  EXCEPTIONS
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA l_timeout TYPE REF TO /gal/timestamp_short.

  cfw_follow_rfc_route rfc_route_info.
  cfw_remote_coding.

* Make sure to get exclusive access to lock information (when called via RFC)
  IF rfc_route_info-current_step > 0.
    CALL FUNCTION 'ENQUEUE_/GAL/ELOCK00'
      EXPORTING
        lock_id      = lock_id
        client       = client
        _scope       = '1'
        _wait        = abap_true
      EXCEPTIONS
        foreign_lock = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      /gal/trace=>write_error( ). "Someone else has access to the lock, but try to release it anyway.
    ENDIF.
  ENDIF.

* Remove record from lock table
  IF force = abap_false.
    DELETE FROM /gal/lock00 WHERE lock_id    = lock_id
                              AND client     = client
                              AND session_id = session_id. "#EC CI_SUBRC
  ELSE.
    DELETE FROM /gal/lock00 WHERE lock_id    = lock_id
                              AND client     = client.    "#EC CI_SUBRC
  ENDIF.

* Remove locks that have timed out
  l_timeout = /gal/timestamp_short=>now( ).

  DELETE FROM /gal/lock00 WHERE timeout < l_timeout->value. "#EC CI_SUBRC

  CALL FUNCTION 'DB_COMMIT'.

* Release SAP lock
  CALL FUNCTION 'DEQUEUE_/GAL/ELOCK00'
    EXPORTING
      lock_id = lock_id
      client  = client.
ENDFUNCTION.
