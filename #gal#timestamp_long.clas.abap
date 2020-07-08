"! <p class="shorttext synchronized" lang="en">Long Timestamp</p>
class /GAL/TIMESTAMP_LONG definition
  public
  inheriting from /GAL/TIMESTAMP_BASE
  create public .

PUBLIC SECTION.
  TYPE-POOLS abap .

  "! <p class="shorttext synchronized" lang="en">Timestamp with maximum value</p>
  CLASS-DATA max_value TYPE REF TO /gal/timestamp_long READ-ONLY .
  "! <p class="shorttext synchronized" lang="en">Timestamp with minimum value</p>
  CLASS-DATA min_value TYPE REF TO /gal/timestamp_long READ-ONLY .
  "! <p class="shorttext synchronized" lang="en">UTC Time Stamp in Short Form (YYYYMMDDhhmmss)</p>
  DATA value TYPE timestampl READ-ONLY .

  "! <p class="shorttext synchronized" lang="en">Class Constructor</p>
  CLASS-METHODS class_constructor .
  "! <p class="shorttext synchronized" lang="en">Create instance from date, time and time zone</p>
  "!
  "! @parameter date      | <p class="shorttext synchronized" lang="en">Date</p>
  "! @parameter time      | <p class="shorttext synchronized" lang="en">Time</p>
  "! @parameter time_zone | <p class="shorttext synchronized" lang="en">Time zone</p>
  "! @parameter instance  | <p class="shorttext synchronized" lang="en">Long Timestamp</p>
  CLASS-METHODS from_date_time
    IMPORTING
      !date           TYPE d
      !time           TYPE t
      !time_zone      TYPE ttzz-tzone
    RETURNING
      VALUE(instance) TYPE REF TO /gal/timestamp_long .
  "! <p class="shorttext synchronized" lang="en">Get timestamp for current time</p>
  "!
  "! @parameter instance | <p class="shorttext synchronized" lang="en">Long Timestamp</p>
  CLASS-METHODS now
    RETURNING
      VALUE(instance) TYPE REF TO /gal/timestamp_long .
  "! <p class="shorttext synchronized" lang="en">Add interval to timestamp</p>
  "!
  "! @parameter microseconds | <p class="shorttext synchronized" lang="en">Microseconds</p>
  "! @parameter milliseconds | <p class="shorttext synchronized" lang="en">Milliseconds</p>
  "! @parameter seconds      | <p class="shorttext synchronized" lang="en">Seconds</p>
  "! @parameter minutes      | <p class="shorttext synchronized" lang="en">Minutes</p>
  "! @parameter hours        | <p class="shorttext synchronized" lang="en">Hours</p>
  "! @parameter days         | <p class="shorttext synchronized" lang="en">Days</p>
  "! @parameter months       | <p class="shorttext synchronized" lang="en">Months</p>
  "! @parameter years        | <p class="shorttext synchronized" lang="en">Years</p>
  METHODS add_interval
    IMPORTING
      !microseconds TYPE i DEFAULT 0
      !milliseconds TYPE i DEFAULT 0
      !seconds      TYPE i DEFAULT 0
      !minutes      TYPE i DEFAULT 0
      !hours        TYPE i DEFAULT 0
      !days         TYPE i DEFAULT 0
      !months       TYPE i DEFAULT 0
      !years        TYPE i DEFAULT 0 .
  METHODS clone
    RETURNING
      VALUE(instance) TYPE REF TO /gal/timestamp_long .
  "! <p class="shorttext synchronized" lang="en">Constructor</p>
  "!
  "! @parameter value | <p class="shorttext synchronized" lang="en">UTC Time Stamp in Long Form (YYYYMMDDhhmmssmmmuuun)</p>
  METHODS constructor
    IMPORTING
      !value TYPE timestampl OPTIONAL .
  "! <p class="shorttext synchronized" lang="en">Subtract interval from timestamp</p>
  "!
  "! @parameter microseconds | <p class="shorttext synchronized" lang="en">Microseconds</p>
  "! @parameter milliseconds | <p class="shorttext synchronized" lang="en">Milliseconds</p>
  "! @parameter seconds      | <p class="shorttext synchronized" lang="en">Seconds</p>
  "! @parameter minutes      | <p class="shorttext synchronized" lang="en">Minutes</p>
  "! @parameter hours        | <p class="shorttext synchronized" lang="en">Hours</p>
  "! @parameter days         | <p class="shorttext synchronized" lang="en">Days</p>
  "! @parameter months       | <p class="shorttext synchronized" lang="en">Months</p>
  "! @parameter years        | <p class="shorttext synchronized" lang="en">Years</p>"!
  METHODS subtract_interval
    IMPORTING
      !microseconds TYPE i DEFAULT 0
      !milliseconds TYPE i DEFAULT 0
      !seconds      TYPE i DEFAULT 0
      !minutes      TYPE i DEFAULT 0
      !hours        TYPE i DEFAULT 0
      !days         TYPE i DEFAULT 0
      !months       TYPE i DEFAULT 0
      !years        TYPE i DEFAULT 0 .
  "! <p class="shorttext synchronized" lang="en">Convert to date and time</p>
  "!
  "! @parameter time_zone | <p class="shorttext synchronized" lang="en">Time zone</p>
  "! @parameter date      | <p class="shorttext synchronized" lang="en">Date</p>
  "! @parameter time      | <p class="shorttext synchronized" lang="en">Time</p>
  "! @parameter dst       | <p class="shorttext synchronized" lang="en">Flag: Daylight savings time</p>
  METHODS to_date_time
    IMPORTING
      !time_zone TYPE ttzz-tzone
    EXPORTING
      !date      TYPE d
      !time      TYPE t
      !dst       TYPE abap_bool .
  "! <p class="shorttext synchronized" lang="en">Convert to short timestamp</p>
  "!
  "! @parameter round_down | <p class="shorttext synchronized" lang="en">Flag: Round down</p>
  "! @parameter round_up   | <p class="shorttext synchronized" lang="en">Flag: Round up</p>
  "! @parameter instance   | <p class="shorttext synchronized" lang="en">Long Timestamp</p>
  METHODS to_short_timestamp
    IMPORTING
      !round_down     TYPE abap_bool DEFAULT abap_false
      !round_up       TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(instance) TYPE REF TO /gal/timestamp_short .
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
                                     days    = days
                                     months  = months
                                     years   = years ).

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
    DATA:
      l_microseconds TYPE i,
      l_milliseconds TYPE i,
      l_seconds      TYPE i,
      l_minutes      TYPE i,
      l_hours        TYPE i,
      l_days         TYPE i,
      l_months       TYPE i,
      l_years        TYPE i.

    l_microseconds = - microseconds.
    l_milliseconds = - milliseconds.
    l_seconds      = - seconds.
    l_minutes      = - minutes.
    l_hours        = - hours.
    l_days         = - days.
    l_months       = - months.
    l_years        = - years.

    add_interval( microseconds = l_microseconds
                  milliseconds = l_milliseconds
                  seconds      = l_seconds
                  minutes      = l_minutes
                  hours        = l_hours
                  days         = l_days
                  months       = l_months
                  years        = l_years ).
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
