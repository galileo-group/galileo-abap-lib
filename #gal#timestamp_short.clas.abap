"! <p class="shorttext synchronized" lang="en">Short Timestamp</p>
CLASS /gal/timestamp_short DEFINITION
  PUBLIC
  INHERITING FROM /gal/timestamp_base
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    "! <p class="shorttext synchronized" lang="en">Timestamp with maximum value</p>
    CLASS-DATA max_value TYPE REF TO /gal/timestamp_short READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">Timestamp with minimum value</p>
    CLASS-DATA min_value TYPE REF TO /gal/timestamp_short READ-ONLY .
    "! <p class="shorttext synchronized" lang="en">UTC Time Stamp in Short Form (YYYYMMDDhhmmss)</p>
    DATA value TYPE timestamp READ-ONLY .

    "! <p class="shorttext synchronized" lang="en">Class Constructor</p>
    CLASS-METHODS class_constructor .
    "! <p class="shorttext synchronized" lang="en">Create instance from date, time and time zone</p>
    "!
    "! @parameter date      | <p class="shorttext synchronized" lang="en">Date</p>
    "! @parameter time      | <p class="shorttext synchronized" lang="en">Time</p>
    "! @parameter time_zone | <p class="shorttext synchronized" lang="en">Time zone</p>
    "! @parameter instance  | <p class="shorttext synchronized" lang="en">Short Timestamp</p>
    CLASS-METHODS from_date_time
      IMPORTING
        !date           TYPE d
        !time           TYPE t
        !time_zone      TYPE ttzz-tzone
      RETURNING
        VALUE(instance) TYPE REF TO /gal/timestamp_short .
    "! <p class="shorttext synchronized" lang="en">Get timestamp for current time</p>
    "!
    "! @parameter instance | <p class="shorttext synchronized" lang="en">Short Timestamp</p>
    CLASS-METHODS now
      RETURNING
        VALUE(instance) TYPE REF TO /gal/timestamp_short .
    "! <p class="shorttext synchronized" lang="en">Add interval to timestamp</p>
    "!
    "! @parameter seconds | <p class="shorttext synchronized" lang="en">Seconds</p>
    "! @parameter minutes | <p class="shorttext synchronized" lang="en">Minutes</p>
    "! @parameter hours   | <p class="shorttext synchronized" lang="en">Hours</p>
    "! @parameter days    | <p class="shorttext synchronized" lang="en">Days</p>
    "! @parameter months  | <p class="shorttext synchronized" lang="en">Months</p>
    "! @parameter years   | <p class="shorttext synchronized" lang="en">Years</p>
    METHODS add_interval
      IMPORTING
        !seconds TYPE i DEFAULT 0
        !minutes TYPE i DEFAULT 0
        !hours   TYPE i DEFAULT 0
        !days    TYPE i DEFAULT 0
        !months  TYPE i DEFAULT 0
        !years   TYPE i DEFAULT 0 .
    METHODS clone
      RETURNING
        VALUE(instance) TYPE REF TO /gal/timestamp_short .
    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    "!
    "! @parameter value | <p class="shorttext synchronized" lang="en">UTC Time Stamp in Short Form (YYYYMMDDhhmmss)</p>
    METHODS constructor
      IMPORTING
        !value TYPE timestamp OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Subtract interval from timestamp</p>
    "!
    "! @parameter seconds | <p class="shorttext synchronized" lang="en">Seconds</p>
    "! @parameter minutes | <p class="shorttext synchronized" lang="en">Minutes</p>
    "! @parameter hours   | <p class="shorttext synchronized" lang="en">Hours</p>
    "! @parameter days    | <p class="shorttext synchronized" lang="en">Days</p>
    "! @parameter months  | <p class="shorttext synchronized" lang="en">Months</p>
    "! @parameter years   | <p class="shorttext synchronized" lang="en">Years</p>
    METHODS subtract_interval
      IMPORTING
        !seconds TYPE i DEFAULT 0
        !minutes TYPE i DEFAULT 0
        !hours   TYPE i DEFAULT 0
        !days    TYPE i DEFAULT 0
        !months  TYPE i DEFAULT 0
        !years   TYPE i DEFAULT 0 .
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
    "! <p class="shorttext synchronized" lang="en">Convert to long timestamp</p>
    "!
    "! @parameter instance | <p class="shorttext synchronized" lang="en">Short Timestamp</p>
    METHODS to_long_timestamp
      RETURNING
        VALUE(instance) TYPE REF TO /gal/timestamp_long .
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS /gal/timestamp_short IMPLEMENTATION.


  METHOD add_interval.
    DATA:
      l_total_seconds TYPE i,
      l_total_days    TYPE i,
      l_date_old      TYPE d,
      l_date_new      TYPE d,
      l_time_old      TYPE t,
      l_time_new      TYPE t,
      l_new_value_str TYPE string,
      l_year_n(4)     TYPE n,
      l_month_n(2)    TYPE n,
      l_year          TYPE i,
      l_month         TYPE i.

    l_total_days    = days.
    l_total_seconds = seconds + 60 * minutes + 3600 * hours. "#EC NUMBER_OK

    IF l_total_days <> 0 OR l_total_seconds <> 0.

      l_total_days    = l_total_days + sign( l_total_seconds ) * ( abs( l_total_seconds ) DIV 86400 ). "#EC NUMBER_OK
      l_total_seconds = sign( l_total_seconds ) * ( abs( l_total_seconds ) MOD 86400 ). "#EC NUMBER_OK

      IF l_total_days <> 0 OR l_total_seconds <> 0.

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

      ENDIF.
    ENDIF.

    IF months IS NOT INITIAL OR years IS NOT INITIAL.

      l_new_value_str = value.

      l_month_n = l_new_value_str+4(2).
      l_year_n  = l_new_value_str(4).
      l_month = l_month_n.
      l_year  = l_year_n.

      l_year  = l_year  + years.
      l_month = l_month + months.
      WHILE l_month > 12.                                "#EC NUMBER_OK
        l_month = l_month - 12.                          "#EC NUMBER_OK
        l_year  = l_year + 1.
      ENDWHILE.
      WHILE l_month < 1.
        l_month = 12 + l_month.                          "#EC NUMBER_OK
        l_year  = l_year - 1.
      ENDWHILE.

      l_month_n = l_month.
      l_year_n  = l_year.

      REPLACE SECTION OFFSET 0 LENGTH 4 OF l_new_value_str WITH l_year_n.
      REPLACE SECTION OFFSET 4 LENGTH 2 OF l_new_value_str WITH l_month_n.

      value = l_new_value_str.

    ENDIF.

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
    DATA:
      l_seconds TYPE i,
      l_minutes TYPE i,
      l_hours   TYPE i,
      l_days    TYPE i,
      l_months  TYPE i,
      l_years   TYPE i.

    l_seconds = - seconds.
    l_minutes = - minutes.
    l_hours   = - hours.
    l_days    = - days.
    l_months  = - months.
    l_years   = - years.

    add_interval( seconds = l_seconds
                  minutes = l_minutes
                  hours   = l_hours
                  days    = l_days
                  months  = l_months
                  years   = l_years ).
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
