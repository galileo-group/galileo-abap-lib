*----------------------------------------------------------------------*
***INCLUDE /GAL/_MONITOR_O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PBO_0101_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100_status OUTPUT.
  PERFORM pbo_0100_status.
  PERFORM pbo_0100_init_rfc_route_info.
ENDMODULE.                    "pbo_0100_status OUTPUT

*----------------------------------------------------------------------*
*  MODULE pbo_0101_status OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pbo_0101_init OUTPUT.
  PERFORM pbo_0101_init.
ENDMODULE.                 " PBO_0101_STATUS  OUTPUT

*----------------------------------------------------------------------*
*  MODULE pbo_0102_init OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pbo_0102_init OUTPUT.
  PERFORM pbo_0102_init.
ENDMODULE.                    "pbo_0102_init OUTPUT
