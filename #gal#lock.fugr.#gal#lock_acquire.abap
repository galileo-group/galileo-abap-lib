FUNCTION /gal/lock_acquire.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO OPTIONAL
*"     REFERENCE(LOCK_ID) TYPE  /GAL/LOCK_ID
*"     REFERENCE(CLIENT) TYPE  MANDT OPTIONAL
*"     REFERENCE(SESSION_ID) TYPE  /GAL/SESSION_ID DEFAULT
*"       /GAL/SESSION=>ID
*"     REFERENCE(LOCK_TIMEOUT) TYPE  I DEFAULT 0
*"     REFERENCE(WAIT_TIMEOUT) TYPE  I DEFAULT 5
*"     REFERENCE(INFO) TYPE  STRING OPTIONAL
*"  EXCEPTIONS
*"      CANNOT_ACQUIRE_LOCK
*"      RFC_EXCEPTION
*"----------------------------------------------------------------------

  DATA l_timer           TYPE REF TO /gal/stopwatch.
  DATA l_elapsed_seconds TYPE i.
  DATA l_timestamp       TYPE timestamp.
  DATA l_timeout         TYPE REF TO /gal/timestamp_short.

  DATA l_wa_step_info    LIKE LINE OF rfc_route_info-step_infos.
  DATA l_wa_lock00_curr  TYPE /gal/lock00.
  DATA l_wa_lock00_new   TYPE /gal/lock00.

  cfw_follow_rfc_route rfc_route_info.
  cfw_pass_exception cannot_acquire_lock.
  cfw_remote_coding.

* Prepare record for lock table
  l_wa_lock00_new-lock_id    = lock_id.
  l_wa_lock00_new-client     = client.
  l_wa_lock00_new-session_id = session_id.
  l_wa_lock00_new-info       = /gal/string=>limit_length( input  = info
                                                          length = 200 ).

  READ TABLE rfc_route_info-step_infos INDEX 1 INTO l_wa_step_info.
  IF sy-subrc <> 0.
    /gal/cfw_helper=>update_rfc_route_step_info( CHANGING rfc_route_step_info = l_wa_step_info ).
  ENDIF.

  MOVE-CORRESPONDING l_wa_step_info TO l_wa_lock00_new.

* Ensure exclusive access to lock table and read current lock information
  CREATE OBJECT l_timer.

  l_timer->start( ).

  DO.
    CALL FUNCTION 'ENQUEUE_/GAL/ELOCK00'
      EXPORTING
        lock_id      = lock_id
        client       = client
        _scope       = '1'
        _wait        = abap_false
      EXCEPTIONS
        foreign_lock = 1
        OTHERS       = 2.
    IF sy-subrc = 1.
      l_timer->get_elapsed_time( IMPORTING elapsed_seconds = l_elapsed_seconds ).

      IF l_elapsed_seconds < wait_timeout.
        WAIT UP TO 1 SECONDS.
        CONTINUE.
      ELSE.
        SELECT SINGLE * FROM /gal/lock00 INTO l_wa_lock00_curr
                       WHERE lock_id = lock_id
                         AND client  = client.
        IF sy-subrc = 0.
          MESSAGE e001 WITH l_wa_lock00_curr-lock_id
                            l_wa_lock00_curr-system_id
                            l_wa_lock00_curr-client_id
                            l_wa_lock00_curr-user_id
                    RAISING cannot_acquire_lock.
        ELSE.
          cfw_pass_message cannot_acquire_lock.
        ENDIF.
      ENDIF.
    ELSEIF sy-subrc > 1.
      cfw_pass_message cannot_acquire_lock.
    ENDIF.

    IF rfc_route_info-current_step > 0.
      SELECT SINGLE * FROM /gal/lock00 INTO l_wa_lock00_curr
                     WHERE lock_id = lock_id
                       AND client  = client.
      IF sy-subrc = 0.
        GET TIME STAMP FIELD l_timestamp.

        IF l_wa_lock00_curr-timeout     > l_timestamp AND
           l_wa_lock00_curr-session_id <> session_id.

          CALL FUNCTION 'DEQUEUE_/GAL/ELOCK00'
            EXPORTING
              lock_id = lock_id
              client  = client
              _scope  = '1'.

          l_timer->get_elapsed_time( IMPORTING elapsed_seconds = l_elapsed_seconds ).

          IF l_elapsed_seconds < wait_timeout.
            WAIT UP TO 1 SECONDS.
            CONTINUE.
          ENDIF.

          MESSAGE e001 WITH l_wa_lock00_curr-lock_id
                            l_wa_lock00_curr-system_id
                            l_wa_lock00_curr-client_id
                            l_wa_lock00_curr-user_id
                    RAISING cannot_acquire_lock.
        ENDIF.
      ENDIF.
    ENDIF.

    EXIT.
  ENDDO.

* Update lock information
  l_timeout = /gal/timestamp_short=>now( ).

  IF lock_timeout > 0.
    l_timeout->add_interval( seconds = lock_timeout ).
  ELSE.
    l_timeout->add_interval( hours = 48 ).
  ENDIF.

  l_wa_lock00_new-timeout = l_timeout->value.

  MODIFY /gal/lock00 FROM l_wa_lock00_new.                "#EC CI_SUBRC

  CALL FUNCTION 'DB_COMMIT'.

* Dequeue when called via RFC, otherwise keep lock until release is called
  IF rfc_route_info-current_step > 0.
    CALL FUNCTION 'DEQUEUE_/GAL/ELOCK00'
      EXPORTING
        lock_id = lock_id
        client  = client
        _scope  = '1'.
  ENDIF.
ENDFUNCTION.
