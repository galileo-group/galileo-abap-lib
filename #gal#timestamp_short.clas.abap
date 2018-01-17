"! <p class="shorttext synchronized" lang="en">Short Timestamp</p>
class /GAL/TIMESTAMP_SHORT definition
  public
  inheriting from /GAL/TIMESTAMP_BASE
  create public .

public section.
  type-pools ABAP .

    "! <p class="shorttext synchronized" lang="en">Timestamp with maximum value</p>
  class-data MAX_VALUE type ref to /GAL/TIMESTAMP_SHORT read-only .
    "! <p class="shorttext synchronized" lang="en">Timestamp with minimum value</p>
  class-data MIN_VALUE type ref to /GAL/TIMESTAMP_SHORT read-only .
    "! <p class="shorttext synchronized" lang="en">UTC Time Stamp in Short Form (YYYYMMDDhhmmss)</p>
  data VALUE type TIMESTAMP read-only .

    "! <p class="shorttext synchronized" lang="en">Class Constructor</p>
  class-methods CLASS_CONSTRUCTOR .
    "! <p class="shorttext synchronized" lang="en">Create instance from date, time and time zone</p>
    "!
    "! @parameter date      | <p class="shorttext synchronized" lang="en">Date</p>
    "! @parameter time      | <p class="shorttext synchronized" lang="en">Time</p>
    "! @parameter time_zone | <p class="shorttext synchronized" lang="en">Time zone</p>
    "! @parameter instance  | <p class="shorttext synchronized" lang="en">Short Timestamp</p>
  class-methods FROM_DATE_TIME
    importing
      !DATE type D
      !TIME type T
      !TIME_ZONE type TTZZ-TZONE
    returning
      value(INSTANCE) type ref to /GAL/TIMESTAMP_SHORT .
    "! <p class="shorttext synchronized" lang="en">Get timestamp for current time</p>
    "!
    "! @parameter instance | <p class="shorttext synchronized" lang="en">Short Timestamp</p>
  class-methods NOW
    returning
      value(INSTANCE) type ref to /GAL/TIMESTAMP_SHORT .
    "! <p class="shorttext synchronized" lang="en">Add interval to timestamp</p>
    "!
    "! @parameter seconds | <p class="shorttext synchronized" lang="en">Seconds</p>
    "! @parameter minutes | <p class="shorttext synchronized" lang="en">Minutes</p>
    "! @parameter hours   | <p class="shorttext synchronized" lang="en">Hours</p>
    "! @parameter days    | <p class="shorttext synchronized" lang="en">Days</p>
  methods ADD_INTERVAL
    importing
      !SECONDS type I default 0
      !MINUTES type I default 0
      !HOURS type I default 0
      !DAYS type I default 0 .
  methods CLONE
    returning
      value(INSTANCE) type ref to /GAL/TIMESTAMP_SHORT .
    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    "!
    "! @parameter value | <p class="shorttext synchronized" lang="en">UTC Time Stamp in Short Form (YYYYMMDDhhmmss)</p>
  methods CONSTRUCTOR
    importing
      !VALUE type TIMESTAMP optional .
    "! <p class="shorttext synchronized" lang="en">Subtract interval from timestamp</p>
    "!
    "! @parameter seconds | <p class="shorttext synchronized" lang="en">Seconds</p>
    "! @parameter minutes | <p class="shorttext synchronized" lang="en">Minutes</p>
    "! @parameter hours   | <p class="shorttext synchronized" lang="en">Hours</p>
    "! @parameter days    | <p class="shorttext synchronized" lang="en">Days</p>
  methods SUBTRACT_INTERVAL
    importing
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
    "! <p class="shorttext synchronized" lang="en">Convert to long timestamp</p>
    "!
    "! @parameter instance | <p class="shorttext synchronized" lang="en">Short Timestamp</p>
  methods TO_LONG_TIMESTAMP
    returning
      value(INSTANCE) type ref to /GAL/TIMESTAMP_LONG .
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS /GAL/TIMESTAMP_SHORT IMPLEMENTATION.


  METHOD add_interval.
    DATA l_total_seconds TYPE i.
    DATA l_total_days    TYPE i.

    DATA l_date_old      TYPE d.
    DATA l_date_new      TYPE d.
    DATA l_time_old      TYPE t.
    DATA l_time_new      TYPE t.

    l_total_days    = days.
    l_total_seconds = seconds + 60 * minutes + 3600 * hours. "#EC NUMBER_OK

    IF l_total_days = 0 AND l_total_seconds = 0.
      RETURN.
    ENDIF.

    l_total_days    = l_total_days + sign( l_total_seconds ) * ( abs( l_total_seconds ) DIV 86400 ). "#EC NUMBER_OK
    l_total_seconds = sign( l_total_seconds ) * ( abs( l_total_seconds ) MOD 86400 ). "#EC NUMBER_OK

    IF l_total_days = 0 AND l_total_seconds = 0.
      RETURN.
    ENDIF.

    CONVERT TIME STAMP value TIME ZONE time_zone_utc
       INTO DATE l_date_old TIME l_time_old.

    IF l_total_seconds <> 0.
      l_time_new = l_time_old + l_total_seconds.

      IF l_total_seconds > 0.
        IF l_time_new < l_time_old.
          l_total_days = l_total_days + 1.
        ENDIF.
      ELSE.
        IF l_time_new > l_time_old.
          l_total_days = l_total_days - 1.
        ENDIF.
      ENDIF.
    ELSE.
      l_time_new = l_time_old.
    ENDIF.

    IF l_total_days <> 0.
      l_date_new = l_date_old + l_total_days.
    ELSE.
      l_date_new = l_date_old.
    ENDIF.

    CONVERT DATE l_date_new TIME l_time_new
       INTO TIME STAMP value TIME ZONE time_zone_utc.
  ENDMETHOD.


  METHOD class_constructor.

* Note: There must still be space for a 12 hour offset in order to support all time zones
    CREATE OBJECT min_value
      EXPORTING
        value = '00000101120000'.

    CREATE OBJECT max_value
      EXPORTING
        value = '99991231125959'.
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
    DATA l_timestamp TYPE timestamp.

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
    DATA l_seconds TYPE i.
    DATA l_minutes TYPE i.
    DATA l_hours   TYPE i.
    DATA l_days    TYPE i.

    l_seconds = - seconds.
    l_minutes = - minutes.
    l_hours   = - hours.
    l_days    = - days.

    add_interval( seconds = l_seconds
                  minutes = l_minutes
                  hours   = l_hours
                  days    = l_days ).
  ENDMETHOD.


  METHOD to_date_time.
    DATA l_time_zone TYPE ttzz-tzone.

    l_time_zone = parse_time_zone( time_zone ).

    CONVERT TIME STAMP value TIME ZONE l_time_zone
       INTO DATE date TIME time DAYLIGHT SAVING TIME dst.
  ENDMETHOD.


  METHOD to_long_timestamp.
    DATA l_value TYPE timestampl.

    l_value = value.

    CREATE OBJECT instance
      EXPORTING
        value = l_value.
  ENDMETHOD.
ENDCLASS.
