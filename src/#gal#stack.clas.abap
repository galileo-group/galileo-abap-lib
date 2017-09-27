class /GAL/STACK definition
  public
  create public .

public section.
  type-pools ABAP .

  data COUNT type I read-only .

  methods POP
    returning
      value(OBJECT) type ref to OBJECT .
  methods PUSH
    importing
      !OBJECT type ref to OBJECT .
protected section.

  types:
    lt_stack_data TYPE STANDARD TABLE OF REF TO object .

  data DATA type LT_STACK_DATA .
private section.
ENDCLASS.



CLASS /GAL/STACK IMPLEMENTATION.


METHOD pop.
  READ TABLE data INDEX 1 INTO object.
  DELETE data INDEX 1.
  count = count - 1.
ENDMETHOD.


METHOD push.
  INSERT object INTO data INDEX 1.
  count = count + 1.
ENDMETHOD.
ENDCLASS.
