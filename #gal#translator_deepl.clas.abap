class /GAL/TRANSLATOR_DEEPL definition
  public
  final
  create public .

public section.

  interfaces /GAL/IF_TRANSLATION_PROVIDER .

  aliases TRANSLATION_MODE_HTML
    for /GAL/IF_TRANSLATION_PROVIDER~TRANSLATION_MODE_HTML .
  aliases TRANSLATION_MODE_TEXT
    for /GAL/IF_TRANSLATION_PROVIDER~TRANSLATION_MODE_TEXT .
  aliases TRANSLATION_MODE_XML
    for /GAL/IF_TRANSLATION_PROVIDER~TRANSLATION_MODE_XML .
  aliases TRANSLATE
    for /GAL/IF_TRANSLATION_PROVIDER~TRANSLATE .

  methods CONSTRUCTOR .
protected section.
private section.

  data AUTH_KEY type STRING .
  data CLIENT type ref to /GAL/REST_CLIENT .
ENDCLASS.



CLASS /GAL/TRANSLATOR_DEEPL IMPLEMENTATION.


  METHOD /gal/if_translation_provider~translate.
    DATA l_line_break    TYPE string.

    DATA l_parameters    TYPE /gal/uri_parameters.
    DATA l_parameter     LIKE LINE OF l_parameters.

    DATA l_language_iso  TYPE laiso.

    DATA l_response      TYPE REF TO /gal/rest_response.

    DATA l_json_response TYPE REF TO /gal/json_element.
    DATA l_translations  TYPE REF TO /gal/json_element.
    DATA l_translation   TYPE REF TO /gal/json_element.
    DATA l_text          TYPE REF TO /gal/json_element.

    DATA l_exception     TYPE REF TO cx_root.
    DATA l_message       TYPE string.

* Determine type of line breaks to be used
    IF input CS /gal/string=>line_break_windows.
      l_line_break = /gal/string=>line_break_windows.
    ELSE.
      l_line_break = /gal/string=>line_break_unix.
    ENDIF.

* Add text to URI parameters (always convert to UNIX style line breaks!)
    l_parameter-name  = `text`.                             "#EC NOTEXT
    l_parameter-value = input.

    IF l_line_break <> /gal/string=>line_break_unix.
      REPLACE ALL OCCURRENCES OF l_line_break IN l_parameter-value WITH /gal/string=>line_break_unix.
    ENDIF.

    INSERT l_parameter INTO TABLE l_parameters.

* Convert source language to ISO
    TRY.
        l_language_iso = /gal/convert=>language_key_sap_to_iso( source_language_id ).

        l_parameter-name  = `source_lang`.                  "#EC NOTEXT
        l_parameter-value = l_language_iso.
        INSERT l_parameter INTO TABLE l_parameters.

      CATCH /gal/cx_conversion_exception INTO l_exception. "#EC NO_HANDLER
        "Use automatic source language detection as fallback

    ENDTRY.

* Convert target language to ISO
    TRY.
        l_language_iso = /gal/convert=>language_key_sap_to_iso( target_language_id ).

        l_parameter-name  = `target_lang`.                  "#EC NOTEXT
        l_parameter-value = l_language_iso.
        INSERT l_parameter INTO TABLE l_parameters.

      CATCH /gal/cx_conversion_exception INTO l_exception.
        l_message = l_exception->get_text( ).

        RAISE EXCEPTION TYPE /gal/cx_translation_exception
          EXPORTING
            textid   = /gal/cx_translation_exception=>custom_exception
            var1     = l_message
            previous = l_exception.

    ENDTRY.

* Analyze mode
    CASE mode.

      WHEN translation_mode_text.
        l_parameter-name  = `tag_handling`.                 "#EC NOTEXT
        l_parameter-value = ``.                             "#EC NOTEXT
        INSERT l_parameter INTO TABLE l_parameters.

      WHEN translation_mode_xml.
        l_parameter-name  = `tag_handling`.                 "#EC NOTEXT
        l_parameter-value = `xml`.                          "#EC NOTEXT
        INSERT l_parameter INTO TABLE l_parameters.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE /gal/cx_translation_exception
          EXPORTING
            textid = /gal/cx_translation_exception=>translation_mode_not_supported
            var1   = mode.

    ENDCASE.

* Split sentences
    l_parameter-name  = `split_sentences`.                  "#EC NOTEXT
    l_parameter-value = `1`.                                "#EC NOTEXT
    INSERT l_parameter INTO TABLE l_parameters.

* Split sentences
    l_parameter-name  = `preserve_formatting`.              "#EC NOTEXT
    l_parameter-value = `1`.                                "#EC NOTEXT
    INSERT l_parameter INTO TABLE l_parameters.

* Add auth key
    l_parameter-name  = `auth_key`.                         "#EC NOTEXT
    l_parameter-value = auth_key.
    INSERT l_parameter INTO TABLE l_parameters.

* Send JSON request and get JSON response
    TRY.
        l_response = client->get( parameters = l_parameters ).

        l_json_response = l_response->get_json_data( ).

* Parse JSON response to get translation
        l_translations = l_json_response->get_child( name = `translations` ). "#EC NOTEXT

        DO.
          IF l_translations IS NOT INITIAL.
            l_translation = l_translations->get_child( index = 1 ).

            IF l_translation IS NOT INITIAL.
              l_text = l_translation->get_child( name = `text` ). "#EC NOTEXT

              IF l_text IS NOT INITIAL.
                l_text->get_value( IMPORTING value = output ).
                EXIT.
              ENDIF.
            ENDIF.
          ENDIF.

          l_message = l_response->get_string_data( ).

          RAISE EXCEPTION TYPE /gal/cx_translation_exception
            EXPORTING
              textid = /gal/cx_translation_exception=>unexpected_response
              var1   = l_message.
        ENDDO.

* Handle exceptions
      CATCH /gal/cx_json_exception
            /gal/cx_rest_client_exception INTO l_exception.

        l_message = l_exception->get_text( ).

        RAISE EXCEPTION TYPE /gal/cx_translation_exception
          EXPORTING
            textid   = /gal/cx_translation_exception=>custom_exception
            var1     = l_message
            previous = l_exception.

    ENDTRY.

* Convert line break style to match source string
    IF l_line_break <> /gal/string=>line_break_unix.
      REPLACE ALL OCCURRENCES OF /gal/string=>line_break_unix IN output WITH l_line_break.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    DATA l_config_store  TYPE REF TO /gal/config_store_local.
    DATA l_config_folder TYPE REF TO /gal/config_node.
    DATA l_config_node   TYPE REF TO /gal/config_node.

* Get settings from configuration store
    TRY.
        CREATE OBJECT l_config_store.

        l_config_folder = l_config_store->get_node( path = `/Galileo Group AG/Open Source Components/Translation/Providers/DeepL` ). "#EC NOTEXT

        l_config_node = l_config_folder->get_child_node( `API Key` ). "#EC NOTEXT
        l_config_node->get_value( EXPORTING default_value = `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx` "#EC NOTEXT
                                  IMPORTING value         = auth_key ).

      CATCH /gal/cx_config_exception.                   "#EC NO_HANDLER
        " Nothing needs to be done here: Default value are used

    ENDTRY.

* Create REST client
    CREATE OBJECT client
      EXPORTING
        base_uri = `v1/translate`                        "#EC NOTEXT
        host     = `api.deepl.com`                  "#EC NOTEXT
        scheme   = cl_http_client=>schemetype_https
        ssl_id   = 'ANONYM'.                        "#EC NOTEXT
  ENDMETHOD.
ENDCLASS.
