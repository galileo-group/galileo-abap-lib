class /GAL/REGEX definition
  public
  create public .

public section.
  type-pools ABAP .

  class-methods ABAP_WILDCARD_TO_REGEX
    importing
      !INPUT type CSEQUENCE
      !OMIT_ANCHORS type ABAP_BOOL default ABAP_FALSE
    returning
      value(RESULT) type STRING .
  class-methods ESCAPE
    importing
      !INPUT type CSEQUENCE
    returning
      value(RESULT) type STRING .
  class-methods SQL_WILDCARD_TO_REGEX
    importing
      !INPUT type CSEQUENCE
    returning
      value(RESULT) type STRING .
protected section.
private section.
ENDCLASS.



CLASS /GAL/REGEX IMPLEMENTATION.


METHOD abap_wildcard_to_regex.
  result = input.

  REPLACE ALL OCCURRENCES OF REGEX `([.?^${}()|\[\]\/\\])` IN result WITH `\\$1`.
  REPLACE ALL OCCURRENCES OF `*` IN result WITH `.*`.
  REPLACE ALL OCCURRENCES OF `+` IN result WITH `.`.

  IF omit_anchors = abap_false.
    CONCATENATE `^` result `\s*$` INTO result.
  ELSEIF result IS INITIAL.
    result = `.*`.
  ENDIF.
ENDMETHOD.


METHOD escape.
  result = input.

  REPLACE ALL OCCURRENCES OF REGEX `([.*+?^${}()|\[\]\/\\])` IN result WITH `\\$1`.
ENDMETHOD.


METHOD sql_wildcard_to_regex.
  result = input.

  REPLACE ALL OCCURRENCES OF REGEX `([.*+?^${}()|\[\]\/\\])` IN result WITH `\\$1`.
  REPLACE ALL OCCURRENCES OF `%` IN result WITH `.*`.
  REPLACE ALL OCCURRENCES OF `_` IN result WITH `.`.
ENDMETHOD.
ENDCLASS.
