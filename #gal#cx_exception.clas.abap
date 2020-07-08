class /GAL/CX_EXCEPTION definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.
  type-pools ABAP .

  constants /GAL/CX_EXCEPTION type SOTR_CONC value '00155DF935091ED8A1B67279507DD6BB'. "#EC NOTEXT
  constants CUSTOM_EXCEPTION type SOTR_CONC value '00155DF935091ED8A1B67C695C91D6BB'. "#EC NOTEXT
  data VAR1 type STRING read-only .
  data VAR2 type STRING read-only .
  data VAR3 type STRING read-only .
  data VAR4 type STRING read-only .
  data VAR5 type STRING read-only .
  data VAR6 type STRING read-only .
  data VAR7 type STRING read-only .
  data VAR8 type STRING read-only .
  data VAR9 type STRING read-only .

  class-methods CREATE_FROM_CLASSIC_EXCEPTION
    importing
      !EXCEPTION_CLASS type CSEQUENCE optional
      !PREVIOUS type ref to CX_ROOT optional
      !MESSAGE_ID type SY-MSGID default SY-MSGID
      !MESSAGE_NUMBER type SY-MSGNO default SY-MSGNO
      !MESSAGE_TYPE type SY-MSGTY default SY-MSGTY
      !MESSAGE_VAR1 type SY-MSGV1 default SY-MSGV1
      !MESSAGE_VAR2 type SY-MSGV2 default SY-MSGV2
      !MESSAGE_VAR3 type SY-MSGV3 default SY-MSGV3
      !MESSAGE_VAR4 type SY-MSGV4 default SY-MSGV4
    returning
      value(EXCEPTION) type ref to /GAL/CX_EXCEPTION .
  class-methods CREATE_FROM_EXCEPTION
    importing
      !EXCEPTION_CLASS type CSEQUENCE optional
      !PREVIOUS type ref to CX_ROOT
    returning
      value(EXCEPTION) type ref to /GAL/CX_EXCEPTION .
  class-methods CREATE_WITH_GENERIC_TYPES
    importing
      !EXCEPTION_CLASS type CSEQUENCE optional
      !TEXTID type SOTR_CONC optional
      !PREVIOUS type ref to CX_ROOT optional
      !VAR1 type ANY optional
      !VAR2 type ANY optional
      !VAR3 type ANY optional
      !VAR4 type ANY optional
      !VAR5 type ANY optional
      !VAR6 type ANY optional
      !VAR7 type ANY optional
      !VAR8 type ANY optional
      !VAR9 type ANY optional
    returning
      value(EXCEPTION) type ref to /GAL/CX_EXCEPTION .
  class-methods RAISE_FROM_CLASSIC_EXCEPTION
    importing
      !EXCEPTION_CLASS type CSEQUENCE optional
      !PREVIOUS type ref to CX_ROOT optional
      !MESSAGE_ID type SY-MSGID default SY-MSGID
      !MESSAGE_NUMBER type SY-MSGNO default SY-MSGNO
      !MESSAGE_TYPE type SY-MSGTY default SY-MSGTY
      !MESSAGE_VAR1 type SY-MSGV1 default SY-MSGV1
      !MESSAGE_VAR2 type SY-MSGV2 default SY-MSGV2
      !MESSAGE_VAR3 type SY-MSGV3 default SY-MSGV3
      !MESSAGE_VAR4 type SY-MSGV4 default SY-MSGV4
    raising
      /GAL/CX_EXCEPTION .
  class-methods RAISE_WITH_GENERIC_TYPES
    importing
      !EXCEPTION_CLASS type CSEQUENCE optional
      !TEXTID type SOTR_CONC optional
      !PREVIOUS type ref to CX_ROOT optional
      !VAR1 type ANY optional
      !VAR2 type ANY optional
      !VAR3 type ANY optional
      !VAR4 type ANY optional
      !VAR5 type ANY optional
      !VAR6 type ANY optional
      !VAR7 type ANY optional
      !VAR8 type ANY optional
      !VAR9 type ANY optional
    raising
      /GAL/CX_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !VAR1 type STRING optional
      !VAR2 type STRING optional
      !VAR3 type STRING optional
      !VAR4 type STRING optional
      !VAR5 type STRING optional
      !VAR6 type STRING optional
      !VAR7 type STRING optional
      !VAR8 type STRING optional
      !VAR9 type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS /GAL/CX_EXCEPTION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = /GAL/CX_EXCEPTION .
 ENDIF.
me->VAR1 = VAR1 .
me->VAR2 = VAR2 .
me->VAR3 = VAR3 .
me->VAR4 = VAR4 .
me->VAR5 = VAR5 .
me->VAR6 = VAR6 .
me->VAR7 = VAR7 .
me->VAR8 = VAR8 .
me->VAR9 = VAR9 .
  endmethod.


  METHOD create_from_classic_exception.
    DATA l_message TYPE string.

* Get text from classic exception
    IF message_type CA 'SIWE'.
      MESSAGE ID message_id TYPE message_type NUMBER message_number
            WITH message_var1 message_var2 message_var3 message_var4
            INTO l_message.
    ELSE.
      MESSAGE ID message_id TYPE 'E' NUMBER message_number
            WITH message_var1 message_var2 message_var3 message_var4
            INTO l_message.
    ENDIF.

* Create class based exception from classic exception
    IF exception_class IS INITIAL.
      CREATE OBJECT exception
        EXPORTING
          textid   = /gal/cx_exception=>custom_exception
          previous = previous
          var1     = l_message.
    ELSE.
      CREATE OBJECT exception TYPE (exception_class)
        EXPORTING
          textid   = /gal/cx_exception=>custom_exception
          previous = previous
          var1     = l_message.
    ENDIF.
  ENDMETHOD.


  METHOD create_from_exception.
    DATA l_message TYPE string.

* Get text from inner exception
    l_message = previous->get_text( ).

* Create class based exception from classic exception
    IF exception_class IS INITIAL.
      CREATE OBJECT exception
        EXPORTING
          textid   = /gal/cx_exception=>custom_exception
          previous = previous
          var1     = l_message.
    ELSE.
      CREATE OBJECT exception TYPE (exception_class)
        EXPORTING
          textid   = /gal/cx_exception=>custom_exception
          previous = previous
          var1     = l_message.
    ENDIF.
  ENDMETHOD.


  METHOD create_with_generic_types.
    DATA: l_var1 TYPE string,
          l_var2 TYPE string,
          l_var3 TYPE string,
          l_var4 TYPE string,
          l_var5 TYPE string,
          l_var6 TYPE string,
          l_var7 TYPE string,
          l_var8 TYPE string,
          l_var9 TYPE string.

* Convert variables
    IF var1 IS SUPPLIED.
      l_var1 = /gal/string=>any_to_string( var1 ).
    ENDIF.

    IF var2 IS SUPPLIED.
      l_var2 = /gal/string=>any_to_string( var2 ).
    ENDIF.

    IF var3 IS SUPPLIED.
      l_var3 = /gal/string=>any_to_string( var3 ).
    ENDIF.

    IF var4 IS SUPPLIED.
      l_var4 = /gal/string=>any_to_string( var4 ).
    ENDIF.

    IF var5 IS SUPPLIED.
      l_var5 = /gal/string=>any_to_string( var5 ).
    ENDIF.

    IF var6 IS SUPPLIED.
      l_var6 = /gal/string=>any_to_string( var6 ).
    ENDIF.

    IF var7 IS SUPPLIED.
      l_var7 = /gal/string=>any_to_string( var7 ).
    ENDIF.

    IF var8 IS SUPPLIED.
      l_var8 = /gal/string=>any_to_string( var8 ).
    ENDIF.

    IF var9 IS SUPPLIED.
      l_var9 = /gal/string=>any_to_string( var9 ).
    ENDIF.

* Create new class based exception
    IF exception_class IS INITIAL.
      CREATE OBJECT exception
        EXPORTING
          textid   = textid
          previous = previous
          var1     = l_var1
          var2     = l_var2
          var3     = l_var3
          var4     = l_var4
          var5     = l_var5
          var6     = l_var6
          var7     = l_var7
          var8     = l_var8
          var9     = l_var9.
    ELSE.
      CREATE OBJECT exception TYPE (exception_class)
        EXPORTING
          textid   = textid
          previous = previous
          var1     = l_var1
          var2     = l_var2
          var3     = l_var3
          var4     = l_var4
          var5     = l_var5
          var6     = l_var6
          var7     = l_var7
          var8     = l_var8
          var9     = l_var9.
    ENDIF.
  ENDMETHOD.


  METHOD raise_from_classic_exception.
    DATA l_exception TYPE REF TO /gal/cx_exception.

* Create class based exception from classic exception
    l_exception = create_from_classic_exception( exception_class = exception_class
                                                 previous        = previous
                                                 message_id      = message_id
                                                 message_number  = message_number
                                                 message_type    = message_type
                                                 message_var1    = message_var1
                                                 message_var2    = message_var2
                                                 message_var3    = message_var3
                                                 message_var4    = message_var4 ).

* Raise class based exception
    RAISE EXCEPTION l_exception.
  ENDMETHOD.


  METHOD raise_with_generic_types.
    DATA l_exception TYPE REF TO /gal/cx_exception.

* Create class based exception from classic exception
    l_exception = create_with_generic_types( exception_class = exception_class
                                             textid          = textid
                                             previous        = previous
                                             var1            = var1
                                             var2            = var2
                                             var3            = var3
                                             var4            = var4
                                             var5            = var5
                                             var6            = var6
                                             var7            = var7
                                             var8            = var8
                                             var9            = var9 ).

* Raise class based exception
    RAISE EXCEPTION l_exception.
  ENDMETHOD.
ENDCLASS.
