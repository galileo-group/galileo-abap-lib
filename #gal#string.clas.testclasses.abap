
*----------------------------------------------------------------------*
*       CLASS abap_Unit_Testclass DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS abap_unit_testclass DEFINITION FOR TESTING FINAL "#AU Duration Short
                                                 "#AU Risk_Level Harmless
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>abap_Unit_Testclass
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>/GAL/STRING
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
      f_cut TYPE REF TO /gal/string.                        "#EC NEEDED

    METHODS: date_to_string FOR TESTING.
    METHODS: limit_length FOR TESTING.
    METHODS: replace_variables FOR TESTING.
    METHODS: stringtable_to_texttable FOR TESTING.
    METHODS: texttable_to_stringtable FOR TESTING.
    METHODS: timestamp_to_string FOR TESTING.
    METHODS: time_to_string FOR TESTING.
ENDCLASS.       "abap_Unit_Testclass


*----------------------------------------------------------------------*
*       CLASS abap_Unit_Testclass IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS abap_unit_testclass IMPLEMENTATION.
* =========================================

  METHOD date_to_string.
* ======================
    DATA l_output TYPE string.

    l_output = /gal/string=>date_to_string(
       input    = '00000000'
       language = 'D'
    ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = ``
      msg   = `Date conversion failed: DATE=Initial, LANGUAGE=D`
    ).

    l_output = /gal/string=>date_to_string(
       input    = '19000101'
       language = 'D'
    ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `Montag, 1. Januar 1900`
      msg   = `Date conversion failed: DATE=1900/01/01, LANGUAGE=D`
    ).

    l_output = /gal/string=>date_to_string(
       input    = '19000101'
       language = 'E'
    ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `Monday, January 1, 1900`
      msg   = `Date conversion failed: DATE=1900/01/01, LANGUAGE=E`
    ).

    l_output = /gal/string=>date_to_string(
       input    = sy-datum
       format   = `[PREFIX]`
       language = 'D'
    ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `Heute, `
      msg   = `Date conversion failed: DATE=SY-DATUM, FORMAT=[PREFIX], LANGUAGE=D`
    ).

    l_output = /gal/string=>date_to_string(
       input    = sy-datum
       format   = `[PREFIX]`
       language = 'E'
    ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `Today, `
      msg   = `Date conversion failed: DATE=SY-DATUM, FORMAT=[PREFIX], LANGUAGE=E`
    ).
  ENDMETHOD.       "date_To_String


  METHOD limit_length.
* ====================
    DATA l_output TYPE string.

    l_output = /gal/string=>limit_length(
        input  = `This is a test case.`
        length = 10
    ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `This is...`
      msg   = `String length limitation failed (Test case 1).`
    ).

    l_output = /gal/string=>limit_length(
        input  = `This is a test case.`
        length = 11
    ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `This is...`
      msg   = `String length limitation failed (Test case 2).`
    ).

    l_output = /gal/string=>limit_length(
        input  = `This is a test case.`
        suffix = `... `
        length = 12
    ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `This is a...`
      msg   = `String length limitation failed (Test case 3).`
    ).

    l_output = /gal/string=>limit_length(
        input  = `This is a test case.`
        suffix = `...very very long suffix...`
        length = 19
    ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `This is a test case`
      msg   = `String length limitation failed (Test case 4).`
    ).
  ENDMETHOD.       "limit_Length


  METHOD replace_variables.
* =========================
    DATA: l_output   TYPE string,

          l_date     TYPE d VALUE '20000101',
          l_time     TYPE t VALUE '135959',

          l_numc(10) TYPE n VALUE '0000000123'.

    l_output = /gal/string=>replace_variables(
       input    = `Date: {1} / Time: {2} / Year: {1:[YYYY]} / Hour: {2:[hh24]}`
       var01    = l_date
       var02    = l_time
       language = 'E'
    ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `Date: 2000/01/01 / Time: 1:59:59 PM / Year: 2000 / Hour: 13`
      msg   = `Variable replacement failed (Test case: Date/time formatting - English).`
    ).

    l_output = /gal/string=>replace_variables(
       input    = `Date: {1} / Time: {2} / Year: {1:[YYYY]} / Hour: {2:[hh24]}`
       var01    = l_date
       var02    = l_time
       language = 'D'
    ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `Date: 01.01.2000 / Time: 13:59:59 / Year: 2000 / Hour: 13`
      msg   = `Variable replacement failed (Test case: Date/time formatting - German).`
    ).

    l_output = /gal/string=>replace_variables(
       input    = `}}{{0}{\\}{1}{\\}{\1}}{{99}{123}}{ABC}{XYZ}{{`
       var01    = l_numc
       language = 'D'
    ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `}}{{0}{\}0000000123{\}{1}}{{99}{123}}{ABC}{XYZ}{{`
      msg   = `Variable replacement failed (Test case: Invalid variables, escaping and nesting).`
    ).

    l_output = /gal/string=>replace_variables(
       input    = `{1}/{1:ConvExit=ALPHA}/{1:NoConvExit}/{2}/{2:EditMask=__-__-__}/{2:NoConvExit}`
       var01    = l_numc
       var02    = l_time
       language = 'E'
    ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `0000000123/123/0000000123/1:59:59 PM/13-59-59/135959`
      msg   = `Variable replacement failed (Test case: Conversion exits and edit masks).`
    ).

    l_output = /gal/string=>replace_variables(
       input    = 'Locking job {1}'
       var01    = '00155D0122031ED3AFDEF4542D595EC6'
       language = 'D'
    ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `Locking job 00155D0122031ED3AFDEF4542D595EC6`
      msg   = `Variable replacement failed (Test case: Simple Job ID).`
    ).

    l_output = /gal/string=>replace_variables(
       input    = 'Text without any variables'
       var01    = 'myvar'
       language = 'D'
    ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `Text without any variables`
      msg   = `Text case without any variables failed.`
    ).
  ENDMETHOD.       "replace_Variables


  METHOD stringtable_to_texttable.
* ===========================
    DATA: l_input    TYPE /gal/stringtable,
          l_output   TYPE STANDARD TABLE OF text10,
          l_expected TYPE STANDARD TABLE OF text10.

    INSERT `Line 1` INTO TABLE l_input.
    INSERT `Line 2 ` INTO TABLE l_input.
    INSERT `Line 3  ` INTO TABLE l_input.

    INSERT 'Line 1    ' INTO TABLE l_expected.
    INSERT 'Line 2    ' INTO TABLE l_expected.
    INSERT 'Line 3    ' INTO TABLE l_expected.

    /gal/string=>stringtable_to_texttable( EXPORTING input  = l_input
                                           IMPORTING output = l_output ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = l_expected
      msg   = `Conversion of Stringtable to Test table failed.`
    ).
  ENDMETHOD.


  METHOD texttable_to_stringtable.
* ===========================
    DATA: l_input    TYPE STANDARD TABLE OF text10,
          l_output   TYPE /gal/stringtable,
          l_expected TYPE /gal/stringtable.

    INSERT 'Line 1    ' INTO TABLE l_input.
    INSERT 'Line 2    ' INTO TABLE l_input.
    INSERT 'Line 3    ' INTO TABLE l_input.

    INSERT `Line 1` INTO TABLE l_expected.
    INSERT `Line 2` INTO TABLE l_expected.
    INSERT `Line 3` INTO TABLE l_expected.

    l_output = /gal/string=>texttable_to_stringtable( l_input ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = l_expected
      msg   = `Conversion of Text table to Stringtable failed.`
    ).
  ENDMETHOD.


  METHOD timestamp_to_string.
* ===========================
    DATA: l_output   TYPE string,
          l_expected TYPE string.

    l_output = /gal/string=>timestamp_to_string(
        timestamp = 20001231000000
        timezone  = 'UTC'
        language  = 'D'
    ).

    l_expected = `Sonntag, 31. Dezember 2000 - 00:00:00`.

    IF sy-zonlo <> 'UTC'.
      CONCATENATE l_expected 'UTC' INTO l_expected SEPARATED BY space.
    ENDIF.

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = l_expected
      msg   = `Timestamp conversion failed: TIMESTAMP=20001231000000, LANGUAGE=D`
    ).

    l_output = /gal/string=>timestamp_to_string(
        timestamp = 20001231000000
        timezone  = 'UTC'
        language  = 'E'
    ).

    l_expected = `Sunday, December 31, 2000 - 12:00:00 AM`.

    IF sy-zonlo <> 'UTC'.
      CONCATENATE l_expected 'UTC' INTO l_expected SEPARATED BY space.
    ENDIF.

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = l_expected
      msg   = `Timestamp conversion failed: TIMESTAMP=20001231000000, LANGUAGE=E`
    ).
  ENDMETHOD.       "timestamp_To_String


  METHOD time_to_string.
* ======================
    DATA l_output TYPE string.

    l_output = /gal/string=>time_to_string(
       input    = '130000'
       language = 'D'
    ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `13:00:00`
      msg   = `Time conversion failed: TIME=130000, LANGUAGE=D`
    ).

    l_output = /gal/string=>time_to_string(
       input    = '130000'
       language = 'E'
    ).

    cl_aunit_assert=>assert_equals(
      act   = l_output
      exp   = `1:00:00 PM`
      msg   = `Time conversion failed: TIME=130000, LANGUAGE=E`
    ).
  ENDMETHOD.       "time_To_String

ENDCLASS.       "abap_Unit_Testclass
