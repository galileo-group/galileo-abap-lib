CLASS /gal/json_serhnd_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPE-POOLS:
      abap.

    METHODS:

      "! <p class="shorttext synchronized" lang="en">Deserialize from JSON</p>
      "!
      "! @parameter serializer             | <p class="shorttext synchronized" lang="en">JSON serializer</p>
      "! @parameter type                   | <p class="shorttext synchronized" lang="en">Type description</p>
      "! @parameter input                  | <p class="shorttext synchronized" lang="en">JSON to be deserialized</p>
      "! @parameter output                 | <p class="shorttext synchronized" lang="en">Deserialization result (or initial)</p>
      "! @parameter deserialized           | <p class="shorttext synchronized" lang="en">Flag: JSON deserialized successfully</p>
      "! @raising   /gal/cx_json_exception | <p class="shorttext synchronized" lang="en">JSON Serializer Exception</p>
      deserialize
        IMPORTING
          serializer   TYPE REF TO /gal/json_serializer
          type         TYPE REF TO cl_abap_typedescr
          input        TYPE REF TO /gal/json_element
        CHANGING
          output       TYPE any
          deserialized TYPE abap_bool
        RAISING
          /gal/cx_json_exception,

      "! <p class="shorttext synchronized" lang="en">Serialize to JSON</p>
      "!
      "! @parameter serializer             | <p class="shorttext synchronized" lang="en">JSON serializer</p>
      "! @parameter type                   | <p class="shorttext synchronized" lang="en">Type description</p>
      "! @parameter input                  | <p class="shorttext synchronized" lang="en">Data to be serialized</p>
      "! @parameter output                 | <p class="shorttext synchronized" lang="en">Serialization result (or initial)</p>
      "! @raising   /gal/cx_json_exception | <p class="shorttext synchronized" lang="en">JSON Serializer Exception</p>
      serialize
        IMPORTING
          serializer    TYPE REF TO /gal/json_serializer
          type          TYPE REF TO cl_abap_typedescr
          input         TYPE any
        RETURNING
          VALUE(output) TYPE REF TO /gal/json_element
        RAISING
          /gal/cx_json_exception.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS /gal/json_serhnd_base IMPLEMENTATION.

  METHOD deserialize.
    RETURN.
  ENDMETHOD.

  METHOD serialize.
    RETURN.
  ENDMETHOD.

ENDCLASS.
