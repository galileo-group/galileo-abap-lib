class /GAL/SYNC_LOCK definition
  public
  create public .

public section.

  type-pools ABAP .
  data ACCESS_GRANTED type ABAP_BOOL read-only .
  data CLIENT type MANDT read-only .
  data ID type /GAL/LOCK_ID read-only .
  data INFO type STRING read-only .
  data NAME type STRING read-only .
  data RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO read-only .

  methods ACQUIRE
    importing
      !LOCK_TIMEOUT type I default 600
      !WAIT_TIMEOUT type I default 5
    returning
      value(SYNC_TIMESTAMP) type ref to /GAL/TIMESTAMP_LONG
    raising
      /GAL/CX_LOCK_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO optional
      !CLIENT type MANDT optional
      !NAME type STRING optional
      !INFO type STRING optional .
  methods GET_SYNC_TIMESTAMP
    returning
      value(SYNC_TIMESTAMP) type ref to /GAL/TIMESTAMP_LONG
    raising
      /GAL/CX_LOCK_EXCEPTION .
  methods RELEASE
    importing
      !SYNC_TIMESTAMP type ref to /GAL/TIMESTAMP_LONG optional
    raising
      /GAL/CX_LOCK_EXCEPTION .
  methods SET_SYNC_TIMESTAMP
    importing
      value(SYNC_TIMESTAMP) type ref to /GAL/TIMESTAMP_LONG
    raising
      /GAL/CX_LOCK_EXCEPTION .
protected section.
private section.

  data MUTEX type ref to /GAL/MUTEX .
ENDCLASS.



CLASS /GAL/SYNC_LOCK IMPLEMENTATION.


METHOD acquire.

* Acquire mutex
  mutex->acquire( lock_timeout = lock_timeout
                  wait_timeout = wait_timeout ).

  access_granted = abap_true.

* Get synchronization timestamp
  IF sync_timestamp IS REQUESTED.
    sync_timestamp = get_sync_timestamp( ).
  ENDIF.
ENDMETHOD.


METHOD constructor.
  DATA l_callstack TYPE abap_callstack.
  DATA l_line(8)   TYPE n.

  FIELD-SYMBOLS <l_callstack> LIKE LINE OF l_callstack.

* Copy sync lock name or create a new one from the caller information
  IF name IS INITIAL.
    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING
        max_level = 2
      IMPORTING
        callstack = l_callstack.

    READ TABLE l_callstack INDEX 2 ASSIGNING <l_callstack>.

    l_line = <l_callstack>-line.

    CONCATENATE `SYNC`
                <l_callstack>-mainprogram
                <l_callstack>-include
                l_line
                <l_callstack>-blocktype
                <l_callstack>-blockname
           INTO me->name SEPARATED BY ` `.
  ELSE.
    me->name = name.
  ENDIF.

* Create mutex
  CREATE OBJECT mutex
    EXPORTING
      rfc_route_info = rfc_route_info
      client         = client
      name           = me->name.

* Set attributes
  me->client         = mutex->client.
  me->id             = mutex->id.
  me->info           = mutex->info.
  me->rfc_route_info = mutex->rfc_route_info.
ENDMETHOD.


METHOD get_sync_timestamp.
  DATA l_sync_timestamp TYPE timestampl.
  DATA l_message        TYPE string.

  CALL FUNCTION '/GAL/LOCK_GET_SYNC_TIMESTAMP'
    EXPORTING
      rfc_route_info = rfc_route_info
      lock_id        = id
      client         = client
    IMPORTING
      sync_timestamp = l_sync_timestamp
    EXCEPTIONS
      OTHERS         = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    RAISE EXCEPTION TYPE /gal/cx_lock_exception
      EXPORTING
        textid = /gal/cx_lock_exception=>rfc_exception
        var1   = l_message.
  ENDIF.

  CREATE OBJECT sync_timestamp
    EXPORTING
      value = l_sync_timestamp.
ENDMETHOD.


METHOD release.

* Get synchronization timestamp
  IF sync_timestamp IS SUPPLIED.
    set_sync_timestamp( sync_timestamp ).
  ENDIF.

* Release mutex
  mutex->release( ).

  access_granted = abap_false.
ENDMETHOD.


METHOD set_sync_timestamp.
  DATA l_message TYPE string.

* Modification are only possible for owner of the lock
  IF access_granted = abap_false.
    RAISE EXCEPTION TYPE /gal/cx_lock_exception
      EXPORTING
        textid = /gal/cx_lock_exception=>not_locked_set_sync_timestamp
        var1   = name.
  ENDIF.

* Set synchronization timestamp
  CALL FUNCTION '/GAL/LOCK_SET_SYNC_TIMESTAMP'
    EXPORTING
      rfc_route_info = rfc_route_info
      lock_id        = id
      client         = client
      sync_timestamp = sync_timestamp->value
    EXCEPTIONS
      OTHERS         = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    RAISE EXCEPTION TYPE /gal/cx_lock_exception
      EXPORTING
        textid = /gal/cx_lock_exception=>rfc_exception
        var1   = l_message.
  ENDIF.
ENDMETHOD.
ENDCLASS.
