
CLASS abap_unit_testclass DEFINITION FOR TESTING FINAL"#AU Duration Short
                                                      "#AU Risk_Level Harmless
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>abap_Unit_Testclass
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>/GAL/CONVERT
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
    METHODS: hex_string_to_integer FOR TESTING RAISING /gal/cx_conversion_exception.
    METHODS: integer_to_hex_string FOR TESTING RAISING /gal/cx_conversion_exception.
    METHODS: language_key_sap_to_iso FOR TESTING RAISING /gal/cx_conversion_exception.
ENDCLASS.       "abap_Unit_Testclass


CLASS abap_unit_testclass IMPLEMENTATION.

  METHOD hex_string_to_integer.
    DATA l_output TYPE i.

    l_output = /gal/convert=>hex_string_to_integer( input = `0` ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `0`
      msg   = `Hex string to integer conversion failed: 0 -> 0`
    ).

    l_output = /gal/convert=>hex_string_to_integer( input = `000F0` ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `240`
      msg   = `Hex string to integer conversion failed: 000F0 -> 240`
    ).

    TRY.
        l_output = /gal/convert=>hex_string_to_integer( input = `00XF0` ).

        cl_aunit_assert=>fail(
          msg = `Hex string to integer conversion should have failed for value 00XF0`
        ).

      CATCH /gal/cx_conversion_exception.               "#EC NO_HANDLER
        "Nothing needs to be done here, this exception is expected and test case was successful

    ENDTRY.
  ENDMETHOD.


  METHOD integer_to_hex_string.
    DATA l_output TYPE string.

    l_output = /gal/convert=>integer_to_hex_string( input = 0 ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `0`
      msg   = `Integer to hex string conversion failed: 0 -> 0`
    ).

    l_output = /gal/convert=>integer_to_hex_string( input = 15 ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `F`
      msg   = `Integer to hex string conversion failed: 15 -> F`
    ).

    l_output = /gal/convert=>integer_to_hex_string( input = 16 ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `10`
      msg   = `Integer to hex string conversion failed: 16 -> 10`
    ).

    l_output = /gal/convert=>integer_to_hex_string( input = 65535 ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `FFFF`
      msg   = `Integer to hex string conversion failed: 65535 -> FFFF`
    ).

    l_output = /gal/convert=>integer_to_hex_string( input = 65536 ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `10000`
      msg   = `Integer to hex string conversion failed: 65535 -> 10000`
    ).

    l_output = /gal/convert=>integer_to_hex_string( input  = 17
                                                    length = 3 ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `011`
      msg   = `Integer to hex string conversion failed: 17, Length = 3 -> 011`
    ).

    l_output = /gal/convert=>integer_to_hex_string( input  = 128
                                                    length = 8 ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `00000080`
      msg   = `Integer to hex string conversion failed: 128, Length = 8 -> 00000080`
    ).

    l_output = /gal/convert=>integer_to_hex_string( input  = -1
                                                    length = 4 ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `FFFF`
      msg   = `Integer to hex string conversion failed: -1, Length = 4 -> FFFF`
    ).

    l_output = /gal/convert=>integer_to_hex_string( input  = -255
                                                    length = 8 ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `FFFFFF01`
      msg   = `Integer to hex string conversion failed: -255, Length = 8 -> FFFFFF01`
    ).

    TRY.
        l_output = /gal/convert=>integer_to_hex_string( input  = -1 ).

        cl_aunit_assert=>fail(
          msg = `Integer to hex string conversion should have failed for negative value (-1) without specification of output length`
        ).

      CATCH /gal/cx_conversion_exception.               "#EC NO_HANDLER
        "Nothing needs to be done here, this exception is expected and test case was successful

    ENDTRY.
  ENDMETHOD.


  METHOD language_key_sap_to_iso.
    DATA l_output TYPE laiso.

    l_output = /gal/convert=>language_key_sap_to_iso( language_key_sap = 'E' ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `EN`
      msg   = `Language key conversion failed: E -> EN`
    ).

    l_output = /gal/convert=>language_key_sap_to_iso( language_key_sap = 'D' ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `DE`
      msg   = `Language key conversion failed: D -> DE`
    ).

    TRY.
        l_output = /gal/convert=>language_key_sap_to_iso( language_key_sap = '?' ).

        cl_aunit_assert=>fail(
          msg = `Language key '?' should not support conversion`
        ).

      CATCH /gal/cx_conversion_exception.               "#EC NO_HANDLER
        "Nothing needs to be done here, this exception is expected and test case was successful

    ENDTRY.
  ENDMETHOD.




ENDCLASS.
