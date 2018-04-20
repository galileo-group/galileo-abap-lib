CLASS abap_unit_testclass DEFINITION FOR TESTING "#AU Duration Short
                                                 "#AU Risk_Level Harmless
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>abap_Unit_Testclass
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>/GAL/TRANSLATOR_DEEPL
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
    DATA:
      f_cut TYPE REF TO /gal/translator_deepl.  "#EC NEEDED

    METHODS: translate FOR TESTING.
ENDCLASS.       "abap_Unit_Testclass


CLASS abap_unit_testclass IMPLEMENTATION.

  METHOD translate.
    DATA l_translator TYPE REF TO /gal/translator_deepl.

    DATA l_text_de    TYPE string.
    DATA l_text_en    TYPE string.

    DATA l_regex      TYPE string.

    DATA l_exception  TYPE REF TO cx_root.
    DATA l_message    TYPE string.

* Create instance of translator
    CREATE OBJECT l_translator.

* Test translation of more complex text
    CONCATENATE `Das ist der erste Satz im ersten Absatz. Jetzt kommt der zweite Satz! Und hier ist der dritte...`
                `Das ist der erste Satz im zweiten Absatz. Jetzt kommt der zweite Satz! Und hier ist der dritte...`
                ``
                `Eine Aufzählung:`
                `- Das ist Unterpunkt eins`
                `- Das ist Unterpunkt zwei`
                `- Das ist Unterpunkt drei`
                ``
                `Weitere Testfälle:`
                `A`
                `"A"` INTO l_text_de SEPARATED BY /gal/string=>line_break_windows. "#EC NOTEXT

    TRY.
        l_translator->translate( EXPORTING source_language_id = 'D'
                                           target_language_id = 'E'
                                           input              = l_text_de
                                 RECEIVING output             = l_text_en ).

      CATCH /gal/cx_translation_exception INTO l_exception.
        l_message = l_exception->get_text( ).

        cl_aunit_assert=>assert_subrc( exp = 0
                                       act = sy-subrc
                                       msg = l_message ).
    ENDTRY.

    CONCATENATE `^.+first.+second.+third.*\.\.\.`
                `.+first.+second.+third.*\.\.\.`
                ``
                `.+:`
                `- .+one`
                `- .+two`
                `- .+three`
                ``
                `.+ test cases:`
                `A`
                `"A"$` INTO l_regex SEPARATED BY /gal/string=>line_break_windows. "#EC NOTEXT

    FIND REGEX l_regex IN l_text_en.
    IF sy-subrc <> 0.
      cl_aunit_assert=>assert_subrc( exp = 0
                                     act = sy-subrc
                                     msg = `Unexpected translation result` ). "#EC NOTEXT
    ENDIF.
  ENDMETHOD.

ENDCLASS.
