class /GAL/MESSAGE_STACK definition
  public
  final
  create public .

*"* public components of class /GAL/MESSAGE_STACK
*"* do not include other source files here!!!
public section.

  methods CLEAR .
  methods POP .
  methods PUSH .
protected section.
*"* protected components of class /GAL/MESSAGE_STACK
*"* do not include other source files here!!!
private section.
*"* private components of class /GAL/MESSAGE_STACK
*"* do not include other source files here!!!

  types:
    BEGIN OF lt_message_stack_entry.
  TYPES   message_id     TYPE symsgid.
  TYPES   message_type   TYPE symsgty.
  TYPES   message_number TYPE symsgno.
  TYPES   message_var1   TYPE symsgv.
  TYPES   message_var2   TYPE symsgv.
  TYPES   message_var3   TYPE symsgv.
  TYPES   message_var4   TYPE symsgv.
  TYPES END OF lt_message_stack_entry .
  types:
    lt_message_stack TYPE STANDARD TABLE OF lt_message_stack_entry .

  data MESSAGE_STACK type LT_MESSAGE_STACK .
ENDCLASS.



CLASS /GAL/MESSAGE_STACK IMPLEMENTATION.


METHOD clear.
  CLEAR message_stack.
ENDMETHOD.


METHOD pop.
  FIELD-SYMBOLS <l_message_stack> LIKE LINE OF message_stack.

  READ TABLE message_stack INDEX 1 ASSIGNING <l_message_stack>.
  IF sy-subrc = 0.
    sy-msgid = <l_message_stack>-message_id.
    sy-msgty = <l_message_stack>-message_type.
    sy-msgno = <l_message_stack>-message_number.
    sy-msgv1 = <l_message_stack>-message_var1.
    sy-msgv2 = <l_message_stack>-message_var2.
    sy-msgv3 = <l_message_stack>-message_var3.
    sy-msgv4 = <l_message_stack>-message_var4.

    DELETE message_stack INDEX 1.
  ENDIF.
ENDMETHOD.


METHOD push.
  DATA l_wa_message_stack LIKE LINE OF message_stack.

  l_wa_message_stack-message_id     = sy-msgid.
  l_wa_message_stack-message_type   = sy-msgty.
  l_wa_message_stack-message_number = sy-msgno.
  l_wa_message_stack-message_var1   = sy-msgv1.
  l_wa_message_stack-message_var2   = sy-msgv2.
  l_wa_message_stack-message_var3   = sy-msgv3.
  l_wa_message_stack-message_var4   = sy-msgv4.

  INSERT l_wa_message_stack INTO message_stack INDEX 1.
ENDMETHOD.
ENDCLASS.
