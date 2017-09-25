
*----------------------------------------------------------------------*
*       CLASS abap_Unit_Testclass DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS abap_unit_testclass DEFINITION FOR TESTING "#AU Duration Long
                                                 "#AU Risk_Level Dangerous
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>abap_Unit_Testclass
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>/GAL/COLOR_HSV
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
* ================
    DATA:
      f_cut TYPE REF TO /gal/color_hsv.  "class under test

    METHODS: from_color_rgb FOR TESTING.
    METHODS: to_color_rgb FOR TESTING.
ENDCLASS.       "abap_Unit_Testclass


*----------------------------------------------------------------------*
*       CLASS abap_Unit_Testclass IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS abap_unit_testclass IMPLEMENTATION.
* =========================================

  METHOD from_color_rgb.
* ======================
    DATA l_count     TYPE i.
    DATA l_color_in  TYPE /gal/color_rgb24_hex.
    DATA l_color_out TYPE /gal/color_rgb24_hex.
    DATA l_rgb       TYPE REF TO /gal/color_rgb.
    DATA l_hsl       TYPE REF TO /gal/color_hsl.

    l_count = 2 ** 23.

    DO l_count TIMES.
      l_color_in = 2 * sy-index - 1.

      l_rgb = /gal/color_rgb=>from_rgb24_hex( l_color_in ).
      l_hsl = /gal/color_hsl=>from_color_rgb( l_rgb ).
      l_rgb = l_hsl->to_color_rgb( ).

      l_color_out = l_rgb->to_rgb24_hex( ).

      cl_aunit_assert=>assert_equals(
        act   = l_color_out
        exp   = l_color_in
        msg   = `Color conversion failed.`
      ).
    ENDDO.
  ENDMETHOD.       "from_Color_Rgb

  METHOD to_color_rgb.
* ====================
    DATA l_count     TYPE i.
    DATA l_color_in  TYPE /gal/color_rgb24_hex.
    DATA l_color_out TYPE /gal/color_rgb24_hex.
    DATA l_rgb       TYPE REF TO /gal/color_rgb.
    DATA l_hsl       TYPE REF TO /gal/color_hsl.

    l_count = 2 ** 23.

    DO l_count TIMES.
      l_color_in = 2 * sy-index - 2.

      l_rgb = /gal/color_rgb=>from_rgb24_hex( l_color_in ).
      l_hsl = /gal/color_hsl=>from_color_rgb( l_rgb ).
      l_rgb = l_hsl->to_color_rgb( ).

      l_color_out = l_rgb->to_rgb24_hex( ).

      cl_aunit_assert=>assert_equals(
        act   = l_color_out
        exp   = l_color_in
        msg   = `Color conversion failed.`
      ).
    ENDDO.
  ENDMETHOD.       "to_Color_Rgb




ENDCLASS.       "abap_Unit_Testclass
