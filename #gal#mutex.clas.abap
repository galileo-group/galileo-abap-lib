CLASS /gal/mutex DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPE-POOLS abap .
    DATA access_granted TYPE abap_bool READ-ONLY .
    DATA client TYPE mandt READ-ONLY .
    DATA id TYPE /gal/lock_id READ-ONLY .
    DATA info TYPE string READ-ONLY .
    DATA name TYPE string READ-ONLY .
    DATA rfc_route_info TYPE /gal/rfc_route_info READ-ONLY .

    METHODS acquire
      IMPORTING
        !lock_timeout TYPE i DEFAULT 600                 "#EC NUMBER_OK
        !wait_timeout TYPE i DEFAULT 5                   "#EC NUMBER_OK
      RAISING
        /gal/cx_lock_exception .
    METHODS constructor
      IMPORTING
        !rfc_route_info TYPE /gal/rfc_route_info OPTIONAL
        !client         TYPE mandt OPTIONAL
        !name           TYPE string OPTIONAL
        !info           TYPE string OPTIONAL .
    METHODS release
      RAISING
        /gal/cx_lock_exception .
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
