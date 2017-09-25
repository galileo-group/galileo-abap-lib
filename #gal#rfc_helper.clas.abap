class /GAL/RFC_HELPER definition
  public
  create public .

public section.

  class-methods ANALYZE_RFC_DESTINATION
    importing
      !RFC_DESTINATION type CSEQUENCE
    exporting
      !HOST type STRING
      !SERVICE type STRING
      !GW_HOST type STRING
      !GW_SERVICE type STRING
      !CLIENT type STRING
      !USER type STRING
      !LANGUAGE type STRING .
protected section.
private section.
ENDCLASS.



CLASS /GAL/RFC_HELPER IMPLEMENTATION.


METHOD analyze_rfc_destination.
  DATA l_rfc_table  TYPE rfcdes.
  DATA l_rfc_data   TYPE rfcoptionall.

  DATA l_parameters TYPE STANDARD TABLE OF string.
  DATA l_parameter  TYPE string.
  DATA l_value      TYPE string.

  FIELD-SYMBOLS <l_parameter> LIKE LINE OF l_parameters.

* Initialize exporting parameters
  CLEAR host.
  CLEAR service.
  CLEAR gw_host.
  CLEAR gw_service.
  CLEAR client.
  CLEAR user.
  CLEAR language.

* Get parameter string
  IF rfc_destination NP `%%*`.
    SELECT SINGLE * FROM rfcdes INTO l_rfc_table
                   WHERE rfcdest = rfc_destination.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING l_rfc_table TO l_rfc_data.

      SPLIT l_rfc_data AT ',' INTO TABLE l_parameters.

      LOOP AT l_parameters ASSIGNING <l_parameter>.
        SPLIT <l_parameter> AT '=' INTO l_parameter l_value.

        CASE l_parameter.
          WHEN 'H'. host       = l_value.
          WHEN 'S'. service    = l_value.
          WHEN 'G'. gw_host    = l_value.
          WHEN 'g'. gw_service = l_value.
          WHEN 'M'. client     = l_value.
          WHEN 'U'. user       = l_value.
          WHEN 'L'. language   = l_value.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ELSE.
    SPLIT rfc_destination+2 AT ',' INTO TABLE l_parameters.

    LOOP AT l_parameters ASSIGNING <l_parameter>.
      SPLIT <l_parameter> AT '=' INTO l_parameter l_value.

      TRANSLATE l_parameter TO LOWER CASE.

      CASE l_parameter.
        WHEN 'ashost'. host       = l_value.
        WHEN 'sysnr'.  service    = l_value.
        WHEN 'gwhost'. gw_host    = l_value.
        WHEN 'gwserv'. gw_service = l_value.
        WHEN 'client'. client     = l_value.
        WHEN 'user'.   user       = l_value.
        WHEN 'lang'.   language   = l_value.
      ENDCASE.
    ENDLOOP.
  ENDIF.
ENDMETHOD.
ENDCLASS.
