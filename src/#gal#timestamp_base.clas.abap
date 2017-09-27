"! <p class="shorttext synchronized" lang="en">Base functionality for Timestamps</p>
class /GAL/TIMESTAMP_BASE definition
  public
  abstract
  create public .

public section.
  type-pools ABAP .

    "! <p class="shorttext synchronized" lang="en">Constant: Use default time zone</p>
  constants TIME_ZONE_DEFAULT type TTZZ-TZONE value '{*}'. "#EC NOTEXT
    "! <p class="shorttext synchronized" lang="en">Constant: Use time zone of Operating System</p>
  constants TIME_ZONE_OS type TTZZ-TZONE value '{OS}'. "#EC NOTEXT
    "! <p class="shorttext synchronized" lang="en">Constant: Use time zone of SAP System</p>
  constants TIME_ZONE_SYSTEM type TTZZ-TZONE value '{SYST}'. "#EC NOTEXT
    "! <p class="shorttext synchronized" lang="en">Constant: Use time zone of current user</p>
  constants TIME_ZONE_USER type TTZZ-TZONE value '{USER}'. "#EC NOTEXT
    "! <p class="shorttext synchronized" lang="en">Constant: Time zone UTC</p>
  constants TIME_ZONE_UTC type TTZZ-TZONE value 'UTC'. "#EC NOTEXT

    "! <p class="shorttext synchronized" lang="en">Class Constructor</p>
  class-methods CLASS_CONSTRUCTOR .
  class-methods PARSE_TIME_ZONE
    importing
      !INPUT type TTZZ-TZONE
    returning
      value(OUTPUT) type TTZZ-TZONE .
protected section.
  PRIVATE SECTION.

    "! <p class="shorttext synchronized" lang="en">Default Time Zone</p>
    CLASS-DATA time_zone_cache_default TYPE ttzz-tzone.

    "! <p class="shorttext synchronized" lang="en">Time Zone of Operating System</p>
    CLASS-DATA time_zone_cache_os TYPE ttzz-tzone.

    "! <p class="shorttext synchronized" lang="en">Time Zone of SAP System</p>
    CLASS-DATA time_zone_cache_system TYPE ttzz-tzone.

    "! <p class="shorttext synchronized" lang="en">Time Zone of current user</p>
    CLASS-DATA time_zone_cache_user TYPE ttzz-tzone.

ENDCLASS.



CLASS /GAL/TIMESTAMP_BASE IMPLEMENTATION.


  METHOD class_constructor.
    DATA l_config_store TYPE REF TO /gal/config_store_local.
    DATA l_config_node  TYPE REF TO /gal/config_node.

* Determine OS time zones (Caution: This function module is unreliable because it returns the alphabetically first time zone matching the UTC offset of the OS!)
    CALL FUNCTION 'TZON_GET_OS_TIMEZONE'
      IMPORTING
        ef_timezone = time_zone_cache_os.

* Determine system time zone
    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone = time_zone_cache_system
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      time_zone_cache_system = time_zone_cache_os.
    ENDIF.

* Determine current user's time zone
    time_zone_cache_user = sy-zonlo.

* Determine default time zone
    TRY.
        CREATE OBJECT l_config_store.

        l_config_node = l_config_store->get_node( path = `/Galileo Group AG/Open Source Components/General Settings/Default Time Zone` ). "#EC NOTEXT

        l_config_node->get_value( EXPORTING default_value = time_zone_cache_os
                                  IMPORTING value         = time_zone_cache_default ).

      CATCH /gal/cx_config_exception.                   "#EC NO_HANDLER
        " Nothing to do here, default value will be used.

    ENDTRY.

    IF time_zone_cache_default(1) CA '{*'.
      time_zone_cache_default = parse_time_zone( time_zone_cache_default ).
    ENDIF.

    IF time_zone_cache_default IS INITIAL.
      time_zone_cache_default = time_zone_cache_os.
    ENDIF.
  ENDMETHOD.


  METHOD parse_time_zone.
    CASE input.

      WHEN time_zone_default OR '*' OR space.
        output = time_zone_cache_system.

      WHEN time_zone_os.
        output = time_zone_cache_os.

      WHEN time_zone_system.
        output = time_zone_cache_system.

      WHEN time_zone_user.
        output = time_zone_cache_user.

      WHEN OTHERS.
        output = input.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
