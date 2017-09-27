FUNCTION /gal/rfc_route_ping.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(RFC_ROUTE_INFO) TYPE  /GAL/RFC_ROUTE_INFO
*"  EXPORTING
*"     REFERENCE(RFC_ROUTE_STEP_INFOS) TYPE  /GAL/RFC_ROUTE_STEP_INFOS
*"     REFERENCE(EXCEPTION_INFO) TYPE  /GAL/EXCEPTION_INFO
*"----------------------------------------------------------------------

  DATA l_rfc_route_info TYPE /gal/rfc_route_info.

  CLEAR rfc_route_step_infos.
  CLEAR exception_info.

  cfw_follow_rfc_route rfc_route_info.

  cfw_handle all.
  cfw_get_rfc_route_info l_rfc_route_info.
  cfw_get_exception_info exception_info.
  rfc_route_step_infos = l_rfc_route_info-step_infos.
  cfw_endhandle.
ENDFUNCTION.
