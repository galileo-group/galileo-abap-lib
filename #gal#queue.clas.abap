class /GAL/QUEUE definition
  public
  create public .

public section.
  type-pools ABAP .

  data COUNT type I read-only .

  methods DEQUEUE
    returning
      value(OBJECT) type ref to OBJECT .
  methods ENQUEUE
    importing
      !OBJECT type ref to OBJECT .
protected section.

  types:
    lt_queue_data TYPE STANDARD TABLE OF REF TO object .

  data DATA type LT_QUEUE_DATA .
private section.
ENDCLASS.



CLASS /GAL/QUEUE IMPLEMENTATION.


METHOD dequeue.
  READ TABLE data INDEX 1 INTO object.
  DELETE data INDEX 1.
  count = count - 1.
ENDMETHOD.


METHOD ENQUEUE.
  INSERT object INTO TABLE data.
  count = count + 1.
ENDMETHOD.
ENDCLASS.
