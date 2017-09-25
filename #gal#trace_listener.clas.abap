class /GAL/TRACE_LISTENER definition
  public
  abstract
  create public .

*"* public components of class /GAL/TRACE_LISTENER
*"* do not include other source files here!!!
public section.

  methods FLUSH .
  methods CLEANUP .
  methods WRITE
    importing
      !TEXT type STRING
      !CONTEXT_INFO type STRING optional
      !CALLER_INFO type STRING optional .
protected section.
*"* protected components of class /GAL/TRACE_LISTENER
*"* do not include other source files here!!!
private section.
*"* private components of class /GAL/TRACE_LISTENER
*"* do not include other source files here!!!
ENDCLASS.



CLASS /GAL/TRACE_LISTENER IMPLEMENTATION.


METHOD cleanup.
  RETURN.
ENDMETHOD.


METHOD flush.
  RETURN.
ENDMETHOD.


METHOD write.
  RETURN.
ENDMETHOD.
ENDCLASS.
