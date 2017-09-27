class /GAL/STOPWATCH definition
  public
  create public .

public section.

  data ELAPSED_HOURS type I read-only .
  data ELAPSED_MILLISECONDS type I read-only .
  data ELAPSED_MINUTES type I read-only .
  data ELAPSED_SECONDS type I read-only .
  type-pools ABAP .
  data IS_ACTIVE type ABAP_BOOL read-only .
  data START_TIMESTAMP type ref to /GAL/TIMESTAMP_LONG read-only .
  data STOP_TIMESTAMP type ref to /GAL/TIMESTAMP_LONG read-only .

  methods CONSTRUCTOR .
  methods GET_ELAPSED_TIME
    importing
      !CURRENT_INTERVAL type ABAP_BOOL default ABAP_FALSE
    exporting
      !ELAPSED_MILLISECONDS type I
      !ELAPSED_SECONDS type I
      !ELAPSED_MINUTES type I
      !ELAPSED_HOURS type I .
  methods RESET .
  methods RESTART .
  methods START .
  methods STOP .
protected section.

  methods CALCULATE_TIME_SPAN
    importing
      !START type ref to /GAL/TIMESTAMP_LONG
      !END type ref to /GAL/TIMESTAMP_LONG
    returning
      value(TIME_SPAN) type I .
private section.
ENDCLASS.



CLASS /GAL/STOPWATCH IMPLEMENTATION.


METHOD calculate_time_span.
  DATA l_start_date TYPE d.
  DATA l_start_time TYPE t.

  DATA l_end_date   TYPE d.
  DATA l_end_time   TYPE t.

  CONVERT TIME STAMP start->value TIME ZONE /gal/timestamp_long=>time_zone_utc
     INTO DATE l_start_date TIME l_start_time.

  CONVERT TIME STAMP end->value TIME ZONE /gal/timestamp_long=>time_zone_utc
     INTO DATE l_end_date TIME l_end_time.

  time_span = ( l_end_time - l_start_time ) * 1000 +
              ( l_end_date - l_start_date ) * 86400000 +
              ( ( end->value - floor( end->value ) ) - ( start->value - floor( start->value ) ) ) * 1000.
ENDMETHOD.


METHOD constructor.
  RETURN.
ENDMETHOD.


METHOD get_elapsed_time.
  DATA l_current_timestamp TYPE REF TO /gal/timestamp_long.

* Use attributes when stopwatch is stopped
  IF is_active = abap_false AND current_interval = abap_false.
    elapsed_milliseconds = me->elapsed_milliseconds.
    elapsed_seconds      = me->elapsed_seconds.
    elapsed_minutes      = me->elapsed_minutes.
    elapsed_hours        = me->elapsed_hours.
    RETURN.
  ENDIF.

* Calculate elapsed time while stopwatch is running
  IF is_active = abap_false.
    l_current_timestamp = stop_timestamp.
  ELSE.
    l_current_timestamp = /gal/timestamp_long=>now( ).
  ENDIF.

  elapsed_milliseconds = calculate_time_span( start = start_timestamp
                                              end   = l_current_timestamp ).

  IF current_interval = abap_false.
    elapsed_milliseconds = elapsed_milliseconds + me->elapsed_milliseconds.
  ENDIF.

  elapsed_seconds = elapsed_milliseconds DIV 1000.
  elapsed_minutes = elapsed_seconds      DIV 60.
  elapsed_hours   = elapsed_minutes      DIV 60.
ENDMETHOD.


METHOD reset.
  CLEAR start_timestamp.
  CLEAR stop_timestamp.

  CLEAR elapsed_milliseconds.
  CLEAR elapsed_seconds.
  CLEAR elapsed_minutes.
  CLEAR elapsed_hours.

  is_active = abap_false.
ENDMETHOD.


METHOD restart.
  reset( ).
  start( ).
ENDMETHOD.


METHOD start.
  IF is_active = abap_false.
    start_timestamp = /gal/timestamp_long=>now( ).

    CLEAR stop_timestamp.

    is_active = abap_true.
  ENDIF.
ENDMETHOD.


METHOD stop.
  DATA l_time_span TYPE i.

  IF is_active = abap_true.
    stop_timestamp = /gal/timestamp_long=>now( ).

    is_active = abap_false.

    l_time_span = calculate_time_span( start = start_timestamp
                                       end   = stop_timestamp ).

    elapsed_milliseconds = elapsed_milliseconds + l_time_span.
    elapsed_seconds      = elapsed_milliseconds DIV 1000.
    elapsed_minutes      = elapsed_seconds DIV 60.
    elapsed_hours        = elapsed_minutes DIV 60.
  ENDIF.
ENDMETHOD.
ENDCLASS.
