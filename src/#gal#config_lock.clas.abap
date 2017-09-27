class /GAL/CONFIG_LOCK definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  data ID type /GAL/CONFIG_KEY_ID read-only .
  data RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO read-only .

  methods CONSTRUCTOR
    importing
      !ID type /GAL/CONFIG_KEY_ID
      !RFC_ROUTE_INFO type /GAL/RFC_ROUTE_INFO optional .
  methods ENQEUEUE
    returning
      value(ENQUEUED) type ABAP_BOOL
    raising
      /GAL/CX_LOCK_EXCEPTION .
  methods DEQUEUE
    raising
      /GAL/CX_LOCK_EXCEPTION .
  methods IS_ENQUEUED
    returning
      value(ENQUEUED) type ABAP_BOOL .
protected section.
private section.

  data ENQUEUED type ABAP_BOOL .
ENDCLASS.



CLASS /GAL/CONFIG_LOCK IMPLEMENTATION.


  METHOD constructor.

    me->id = id.
    me->rfc_route_info = rfc_route_info.

  ENDMETHOD.


  METHOD dequeue.

    DATA l_message TYPE string.

    IF enqueued = abap_true.

      CALL FUNCTION '/GAL/CONFIG_LOCK_RELEASE'
        EXPORTING
          rfc_route_info = rfc_route_info
          id             = id
        EXCEPTIONS
          rfc_exception  = 1
          OTHERS         = 2.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO l_message.

        RAISE EXCEPTION TYPE /gal/cx_lock_exception
          EXPORTING
            textid = /gal/cx_lock_exception=>rfc_exception
            var1   = l_message.
      ENDIF.

      enqueued = abap_false.

    ENDIF.


  ENDMETHOD.


  METHOD enqeueue.

    DATA l_message TYPE string.

    IF me->enqueued = abap_false.

      CALL FUNCTION '/GAL/CONFIG_LOCK_ACQUIRE'
        EXPORTING
          rfc_route_info      = rfc_route_info
          id                  = id
        EXCEPTIONS
          cannot_acquire_lock = 1
          rfc_exception       = 2
          OTHERS              = 3.
      IF sy-subrc IS NOT INITIAL.
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
      ENDIF.

      me->enqueued = abap_true.

    ENDIF.

    enqueued = is_enqueued( ).


  ENDMETHOD.


  METHOD is_enqueued.

    enqueued = me->enqueued.

  ENDMETHOD.
ENDCLASS.
