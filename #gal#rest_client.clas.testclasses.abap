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
*?<OBJECT_UNDER_TEST>/GAL/REST_CLIENT
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
    METHODS: delete FOR TESTING RAISING /gal/cx_rest_client_exception.
    METHODS: get FOR TESTING RAISING /gal/cx_rest_client_exception.
    METHODS: post FOR TESTING RAISING /gal/cx_rest_client_exception.
    METHODS: put FOR TESTING RAISING /gal/cx_rest_client_exception.
ENDCLASS.       "abap_Unit_Testclass


*----------------------------------------------------------------------*
*       CLASS abap_Unit_Testclass IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS abap_unit_testclass IMPLEMENTATION.
* =========================================

  METHOD delete.
* ==============
    DATA l_client   TYPE REF TO /gal/rest_client.
    DATA l_response TYPE REF TO /gal/rest_response.
    DATA l_content  TYPE string.

    l_client = /gal/rest_client=>create_by_name( name = `Unit Test` ). "#EC NOTEXT

    l_response = l_client->delete( uri = `1` ).
    l_content  = l_response->get_string_data( ).

    cl_aunit_assert=>assert_equals( act = l_content
                                    exp = `["DELETE","1"]`
                                    msg = `Unexpected result.` ). "#EC NOTEXT

    l_client = /gal/rest_client=>create_by_name( name = `Unit Test (Legacy Mode)` ). "#EC NOTEXT

    l_response = l_client->delete( uri = `1` ).
    l_content  = l_response->get_string_data( ).

    cl_aunit_assert=>assert_equals( act = l_content
                                    exp = `["DELETE","1"]`
                                    msg = `Unexpected result.` ). "#EC NOTEXT
  ENDMETHOD.       "delete


  METHOD get.
* ===========
    DATA l_client   TYPE REF TO /gal/rest_client.
    DATA l_response TYPE REF TO /gal/rest_response.
    DATA l_content  TYPE string.

    l_client = /gal/rest_client=>create_by_name( name = `Unit Test` ). "#EC NOTEXT

    l_response = l_client->get( ).
    l_content  = l_response->get_string_data( ).

    cl_aunit_assert=>assert_equals( act = l_content
                                    exp = `["GET"]`
                                    msg = `Unexpected result.` ). "#EC NOTEXT

    l_response = l_client->get( uri = `1` ).
    l_content  = l_response->get_string_data( ).

    cl_aunit_assert=>assert_equals( act = l_content
                                    exp = `["GET","1"]`
                                    msg = `Unexpected result.` ). "#EC NOTEXT

    l_client = /gal/rest_client=>create_by_name( name = `Unit Test (Legacy Mode)` ). "#EC NOTEXT

    l_response = l_client->get( ).
    l_content  = l_response->get_string_data( ).

    cl_aunit_assert=>assert_equals( act = l_content
                                    exp = `["GET"]`
                                    msg = `Unexpected result.` ). "#EC NOTEXT

    l_response = l_client->get( uri = `1` ).
    l_content  = l_response->get_string_data( ).

    cl_aunit_assert=>assert_equals( act = l_content
                                    exp = `["GET","1"]`
                                    msg = `Unexpected result.` ). "#EC NOTEXT
  ENDMETHOD.       "get


  METHOD post.
* ============
    DATA l_client   TYPE REF TO /gal/rest_client.
    DATA l_request  TYPE REF TO /gal/rest_request.
    DATA l_response TYPE REF TO /gal/rest_response.
    DATA l_content  TYPE string.

    l_client = /gal/rest_client=>create_by_name( name = `Unit Test` ). "#EC NOTEXT

    CREATE OBJECT l_request.

    l_request->set_string_data( `"TEST"` ).
    l_request->set_content_type( /gal/http_constants=>content_type_json ).

    l_response = l_client->post( uri     = `1`
                                 request = l_request ).

    l_content  = l_response->get_string_data( ).

    cl_aunit_assert=>assert_equals( act = l_content
                                    exp = `["POST","1","TEST"]`
                                    msg = `Unexpected result.` ). "#EC NOTEXT

    l_client = /gal/rest_client=>create_by_name( name = `Unit Test (Legacy Mode)` ). "#EC NOTEXT

    l_response = l_client->post( uri     = `1`
                                 request = l_request ).

    l_content  = l_response->get_string_data( ).

    cl_aunit_assert=>assert_equals( act = l_content
                                    exp = `["POST","1","TEST"]`
                                    msg = `Unexpected result.` ). "#EC NOTEXT
  ENDMETHOD.       "post


  METHOD put.
* ===========
    DATA l_client   TYPE REF TO /gal/rest_client.
    DATA l_request  TYPE REF TO /gal/rest_request.
    DATA l_response TYPE REF TO /gal/rest_response.
    DATA l_content  TYPE string.

    l_client = /gal/rest_client=>create_by_name( name = `Unit Test` ). "#EC NOTEXT

    CREATE OBJECT l_request.

    l_request->set_string_data( `"TEST"` ).
    l_request->set_content_type( /gal/http_constants=>content_type_json ).

    l_response = l_client->put( uri     = `1`
                                request = l_request ).

    l_content  = l_response->get_string_data( ).

    cl_aunit_assert=>assert_equals( act = l_content
                                    exp = `["PUT","1","TEST"]`
                                    msg = `Unexpected result.` ). "#EC NOTEXT

    l_client = /gal/rest_client=>create_by_name( name = `Unit Test (Legacy Mode)` ). "#EC NOTEXT

    l_response = l_client->put( uri     = `1`
                                request = l_request ).

    l_content  = l_response->get_string_data( ).

    cl_aunit_assert=>assert_equals( act = l_content
                                    exp = `["PUT","1","TEST"]`
                                    msg = `Unexpected result.` ). "#EC NOTEXT
  ENDMETHOD.       "put

ENDCLASS.       "abap_Unit_Testclass
