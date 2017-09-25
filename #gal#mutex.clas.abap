class /GAL/MUTEX definition
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
    raising
      /GAL/CX_LOCK_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO optional
      !CLIENT type MANDT optional
      !NAME type STRING optional
      !INFO type STRING optional .
  methods RELEASE
    raising
      /GAL/CX_LOCK_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS /GAL/MUTEX IMPLEMENTATION.


METHOD acquire.
  DATA l_message TYPE string.

  CHECK access_granted = abap_false.

  CALL FUNCTION '/GAL/LOCK_ACQUIRE'
    EXPORTING
      rfc_route_info      = rfc_route_info
      lock_id             = id
      client              = client
      lock_timeout        = lock_timeout
      wait_timeout        = wait_timeout
      info                = info
    EXCEPTIONS
      cannot_acquire_lock = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO l_message.

    IF sy-subrc = 1.
      RAISE EXCEPTION TYPE /gal/cx_lock_exception
        EXPORTING
          textid = /gal/cx_lock_exception=>cannot_acquire_lock
          var1   = l_message.
    ELSEIF sy-subrc > 1.
      RAISE EXCEPTION TYPE /gal/cx_lock_exception
        EXPORTING
          textid = /gal/cx_lock_exception=>rfc_exception
          var1   = l_message.
    ENDIF.
  ENDIF.

  access_granted = abap_true.
ENDMETHOD.


METHOD constructor.
  DATA l_callstack TYPE abap_callstack.
  DATA l_line(8)   TYPE n.
  DATA l_id        TYPE md5_fields-hash.

  FIELD-SYMBOLS <l_callstack> LIKE LINE OF l_callstack.

* Copy RFC route (only necessary if locks should be managed on a central system)
  IF rfc_route_info IS NOT INITIAL.
    me->rfc_route_info = rfc_route_info.
  ELSE.
    me->client = client.
  ENDIF.

* Copy mutex name or create a new one from the caller information
  IF name IS INITIAL.
    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING
        max_level = 2
      IMPORTING
        callstack = l_callstack.

    READ TABLE l_callstack INDEX 2 ASSIGNING <l_callstack>.

    l_line = <l_callstack>-line.

    CONCATENATE `MUTEX`
                <l_callstack>-mainprogram
                <l_callstack>-include
                l_line
                <l_callstack>-blocktype
                <l_callstack>-blockname
           INTO me->name SEPARATED BY ` `.
  ELSE.
    me->name = name.
  ENDIF.

* Create mutex id from mutex name
  CALL FUNCTION 'MD5_CALCULATE_HASH_FOR_CHAR'
    EXPORTING
      data = me->name
    IMPORTING
      hash = l_id.

  me->id = l_id.

* Create information text
  IF info IS INITIAL.
    me->info = me->name.
  ELSE.
    me->info = info.
  ENDIF.
ENDMETHOD.


METHOD release.
  DATA l_message TYPE string.

  CHECK access_granted = abap_true.

  CALL FUNCTION '/GAL/LOCK_RELEASE'
    EXPORTING
      rfc_route_info = rfc_route_info
      lock_id        = id
      client         = client
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

  access_granted = abap_false.
ENDMETHOD.
ENDCLASS.
