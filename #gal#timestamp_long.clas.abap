"! <p class="shorttext synchronized" lang="en">Long Timestamp</p>
class /GAL/TIMESTAMP_LONG definition
  public
  inheriting from /GAL/TIMESTAMP_BASE
  create public .

public section.
  type-pools ABAP .

    "! <p class="shorttext synchronized" lang="en">Timestamp with maximum value</p>
  class-data MAX_VALUE type ref to /GAL/TIMESTAMP_LONG read-only .
    "! <p class="shorttext synchronized" lang="en">Timestamp with minimum value</p>
  class-data MIN_VALUE type ref to /GAL/TIMESTAMP_LONG read-only .
    "! <p class="shorttext synchronized" lang="en">UTC Time Stamp in Short Form (YYYYMMDDhhmmss)</p>
  data VALUE type TIMESTAMPL read-only .

    "! <p class="shorttext synchronized" lang="en">Class Constructor</p>
  class-methods CLASS_CONSTRUCTOR .
    "! <p class="shorttext synchronized" lang="en">Create instance from date, time and time zone</p>
    "!
    "! @parameter date      | <p class="shorttext synchronized" lang="en">Date</p>
    "! @parameter time      | <p class="shorttext synchronized" lang="en">Time</p>
    "! @parameter time_zone | <p class="shorttext synchronized" lang="en">Time zone</p>
    "! @parameter instance  | <p class="shorttext synchronized" lang="en">Long Timestamp</p>
  class-methods FROM_DATE_TIME
    importing
      !DATE type D
      !TIME type T
      !TIME_ZONE type TTZZ-TZONE
    returning
      value(INSTANCE) type ref to /GAL/TIMESTAMP_LONG .
    "! <p class="shorttext synchronized" lang="en">Get timestamp for current time</p>
    "!
    "! @parameter instance | <p class="shorttext synchronized" lang="en">Long Timestamp</p>
  class-methods NOW
    returning
      value(INSTANCE) type ref to /GAL/TIMESTAMP_LONG .
    "! <p class="shorttext synchronized" lang="en">Add interval to timestamp</p>
    "!
    "! @parameter microseconds | <p class="shorttext synchronized" lang="en">Microseconds</p>
    "! @parameter milliseconds | <p class="shorttext synchronized" lang="en">Milliseconds</p>
    "! @parameter seconds      | <p class="shorttext synchronized" lang="en">Seconds</p>
    "! @parameter minutes      | <p class="shorttext synchronized" lang="en">Minutes</p>
    "! @parameter hours        | <p class="shorttext synchronized" lang="en">Hours</p>
    "! @parameter days         | <p class="shorttext synchronized" lang="en">Days</p>
  methods ADD_INTERVAL
    importing
      !MICROSECONDS type I default 0
      !MILLISECONDS type I default 0
      !SECONDS type I default 0
      !MINUTES type I default 0
      !HOURS type I default 0
      !DAYS type I default 0 .
  methods CLONE
    returning
      value(INSTANCE) type ref to /GAL/TIMESTAMP_LONG .
    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    "!
    "! @parameter value | <p class="shorttext synchronized" lang="en">UTC Time Stamp in Long Form (YYYYMMDDhhmmssmmmuuun)</p>
  methods CONSTRUCTOR
    importing
      !VALUE type TIMESTAMPL optional .
    "! <p class="shorttext synchronized" lang="en">Subtract interval from timestamp</p>
    "!
    "! @parameter microseconds | <p class="shorttext synchronized" lang="en">Microseconds</p>
    "! @parameter milliseconds | <p class="shorttext synchronized" lang="en">Milliseconds</p>
    "! @parameter seconds      | <p class="shorttext synchronized" lang="en">Seconds</p>
    "! @parameter minutes      | <p class="shorttext synchronized" lang="en">Minutes</p>
    "! @parameter hours        | <p class="shorttext synchronized" lang="en">Hours</p>
    "! @parameter days         | <p class="shorttext synchronized" lang="en">Days</p>
  methods SUBTRACT_INTERVAL
    importing
      !MICROSECONDS type I default 0
      !MILLISECONDS type I default 0
      !SECONDS type I default 0
      !MINUTES type I default 0
      !HOURS type I default 0
      !DAYS type I default 0 .
    "! <p class="shorttext synchronized" lang="en">Convert to date and time</p>
    "!
    "! @parameter time_zone | <p class="shorttext synchronized" lang="en">Time zone</p>
    "! @parameter date      | <p class="shorttext synchronized" lang="en">Date</p>
    "! @parameter time      | <p class="shorttext synchronized" lang="en">Time</p>
    "! @parameter dst       | <p class="shorttext synchronized" lang="en">Flag: Daylight savings time</p>
  methods TO_DATE_TIME
    importing
      !TIME_ZONE type TTZZ-TZONE
    exporting
      !DATE type D
      !TIME type T
      !DST type ABAP_BOOL .
    "! <p class="shorttext synchronized" lang="en">Convert to short timestamp</p>
    "!
    "! @parameter round_down | <p class="shorttext synchronized" lang="en">Flag: Round down</p>
    "! @parameter round_up   | <p class="shorttext synchronized" lang="en">Flag: Round up</p>
    "! @parameter instance   | <p class="shorttext synchronized" lang="en">Long Timestamp</p>
  methods TO_SHORT_TIMESTAMP
    importing
      !ROUND_DOWN type ABAP_BOOL default ABAP_FALSE
      !ROUND_UP type ABAP_BOOL default ABAP_FALSE
    returning
      value(INSTANCE) type ref to /GAL/TIMESTAMP_SHORT .
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS /GAL/TIMESTAMP_LONG IMPLEMENTATION.


  METHOD add_interval.
    DATA l_short_timestamp TYPE REF TO /gal/timestamp_short.

    DATA l_carry_seconds   TYPE i.
    DATA l_split_seconds   TYPE timestampl.

    l_short_timestamp = to_short_timestamp( round_down = abap_true ).
    l_split_seconds   = value - l_short_timestamp->value + milliseconds / 1000 + microseconds / 1000000.

    l_short_timestamp->add_interval( seconds = seconds
                                     minutes = minutes
                                     hours   = hours
                                     days    = days ).

    IF l_split_seconds >= 1.
      l_carry_seconds = floor( l_split_seconds ).
      l_split_seconds = l_split_seconds - l_carry_seconds.
    ELSEIF l_split_seconds <= -1.
      l_carry_seconds = ceil( l_split_seconds ).
      l_split_seconds = l_split_seconds - l_carry_seconds.
    ENDIF.

    IF l_carry_seconds <> 0.
      l_short_timestamp->add_interval( seconds = l_carry_seconds ).
    ENDIF.

    value = l_short_timestamp->value + l_split_seconds.
  ENDMETHOD.


  METHOD class_constructor.

* Note: There must still be space for a 12 hour offset in order to support all time zones
    CREATE OBJECT min_value
      EXPORTING
        value = '00000101115959.5000000'.

    CREATE OBJECT max_value
      EXPORTING
        value = '99991231125959.4999999'.
  ENDMETHOD.


  METHOD clone.
    CREATE OBJECT instance
      EXPORTING
        value = value.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    IF value IS SUPPLIED.
      me->value = value.
    ELSE.
      GET TIME STAMP FIELD me->value.
    ENDIF.
  ENDMETHOD.


  METHOD from_date_time.
    DATA l_time_zone TYPE ttzz-tzone.
    DATA l_timestamp TYPE timestampl.

    l_time_zone = parse_time_zone( time_zone ).

    CONVERT DATE date TIME time
       INTO TIME STAMP l_timestamp TIME ZONE l_time_zone.

    CREATE OBJECT instance
      EXPORTING
        value = l_timestamp.
  ENDMETHOD.


  METHOD now.
    CREATE OBJECT instance.
  ENDMETHOD.


  METHOD subtract_interval.
    DATA l_microseconds TYPE i.
    DATA l_milliseconds TYPE i.
    DATA l_seconds      TYPE i.
    DATA l_minutes      TYPE i.
    DATA l_hours        TYPE i.
    DATA l_days         TYPE i.

    l_microseconds = - microseconds.
    l_milliseconds = - milliseconds.
    l_seconds      = - seconds.
    l_minutes      = - minutes.
    l_hours        = - hours.
    l_days         = - days.

    add_interval( microseconds = l_microseconds
                  milliseconds = l_milliseconds
                  seconds      = l_seconds
                  minutes      = l_minutes
                  hours        = l_hours
                  days         = l_days ).
  ENDMETHOD.


  METHOD to_date_time.
    DATA l_time_zone TYPE ttzz-tzone.

    l_time_zone = parse_time_zone( time_zone ).

    CONVERT TIME STAMP value TIME ZONE l_time_zone
       INTO DATE date TIME time DAYLIGHT SAVING TIME dst.
  ENDMETHOD.


  METHOD to_short_timestamp.
    DATA l_value TYPE timestamp.

    IF round_down = abap_true AND round_up = abap_false.
      l_value = floor( value ).
    ELSEIF round_down = abap_false AND round_up = abap_true.
      l_value = ceil( value ).
    ELSE.
      l_value = value.
    ENDIF.

    CREATE OBJECT instance
      EXPORTING
        value = l_value.
  ENDMETHOD.
ENDCLASS.
