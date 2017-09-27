class /GAL/SESSION definition
  public
  create public .

public section.

  class-data ID type /GAL/SESSION_ID read-only .

  class-methods CLASS_CONSTRUCTOR .
  class-methods KILL .
protected section.
private section.
ENDCLASS.



CLASS /GAL/SESSION IMPLEMENTATION.


METHOD class_constructor.
  id = /gal/uuid=>create_raw( ).
ENDMETHOD.


METHOD kill.
  CALL FUNCTION 'TH_DELETE_MODE'.
ENDMETHOD.
ENDCLASS.
