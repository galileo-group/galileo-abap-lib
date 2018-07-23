class /GAL/TRANSLATOR definition
  public
  abstract
  create public .

public section.
  type-pools ABAP .

  interfaces /GAL/IF_TRANSLATION_PROVIDER
      abstract methods TRANSLATE .

  aliases TRANSLATION_MODE_HTML
    for /GAL/IF_TRANSLATION_PROVIDER~TRANSLATION_MODE_HTML .
  aliases TRANSLATION_MODE_TEXT
    for /GAL/IF_TRANSLATION_PROVIDER~TRANSLATION_MODE_TEXT .
  aliases TRANSLATION_MODE_XML
    for /GAL/IF_TRANSLATION_PROVIDER~TRANSLATION_MODE_XML .
  aliases TRANSLATE
    for /GAL/IF_TRANSLATION_PROVIDER~TRANSLATE .

  class-data TRANSLATION_CACHE_MAX_SIZE type I value 5000. "#EC NUMBER_OK

  methods ADD_TRANSLATION_TO_CACHE
    importing
      !MODE type STRING default TRANSLATION_MODE_TEXT
      !SOURCE_LANGUAGE_ID type LANGU
      !TARGET_LANGUAGE_ID type LANGU
      !SOURCE_TEXT type CSEQUENCE
      !TARGET_TEXT type CSEQUENCE
      !KEEP_IN_CACHE type ABAP_BOOL default ABAP_FALSE .
  methods GET_TRANSLATION_FROM_CACHE
    importing
      !MODE type STRING default TRANSLATION_MODE_TEXT
      !SOURCE_LANGUAGE_ID type LANGU
      !TARGET_LANGUAGE_ID type LANGU
      !SOURCE_TEXT type CSEQUENCE
    returning
      value(TARGET_TEXT) type STRING .
  methods REMOVE_TRANSLATION_FROM_CACHE
    importing
      !MODE type STRING default TRANSLATION_MODE_TEXT
      !SOURCE_LANGUAGE_ID type LANGU
      !TARGET_LANGUAGE_ID type LANGU
      !SOURCE_TEXT type CSEQUENCE .
protected section.
private section.

  types:
    BEGIN OF t_translation_cache_entry,
      mode               TYPE string,
      source_language_id TYPE langu,
      target_language_id TYPE langu,
      source_text        TYPE string,
      target_text        TYPE string,
      keep_in_cache      TYPE abap_bool,
    END OF t_translation_cache_entry .
  types:
    t_translation_cache TYPE HASHED TABLE OF t_translation_cache_entry WITH UNIQUE KEY mode source_language_id target_language_id source_text .

  class-data TRANSLATION_CACHE type T_TRANSLATION_CACHE .
ENDCLASS.



CLASS /GAL/TRANSLATOR IMPLEMENTATION.


  METHOD add_translation_to_cache.
    DATA: l_size     TYPE i,
          l_wa_cache LIKE LINE OF translation_cache.

    FIELD-SYMBOLS: <l_translation_cache> LIKE LINE OF translation_cache.

* Clean-up translation cache
    l_size = lines( translation_cache ).

    IF l_size >= translation_cache_max_size.
      LOOP AT translation_cache ASSIGNING <l_translation_cache> WHERE keep_in_cache = abap_false. "#EC CI_HASHSEQ
        DELETE TABLE translation_cache FROM <l_translation_cache>.
        l_size = l_size - 1.

        IF l_size < translation_cache_max_size.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

* Add translation to translation cache
    l_wa_cache-mode               = mode.
    l_wa_cache-source_language_id = source_language_id.
    l_wa_cache-target_language_id = target_language_id.
    l_wa_cache-source_text        = source_text.
    l_wa_cache-target_text        = target_text.
    l_wa_cache-keep_in_cache      = keep_in_cache.
    INSERT l_wa_cache INTO TABLE translation_cache.
    IF sy-subrc <> 0.
      MODIFY TABLE translation_cache FROM l_wa_cache.
    ENDIF.
  ENDMETHOD.


  METHOD get_translation_from_cache.
    FIELD-SYMBOLS: <l_translation_cache> LIKE LINE OF translation_cache.

    READ TABLE translation_cache
          WITH TABLE KEY mode               = mode
                         source_language_id = source_language_id
                         target_language_id = target_language_id
                         source_text        = source_text
               ASSIGNING <l_translation_cache>.
    IF sy-subrc = 0.
      target_text = <l_translation_cache>-target_text.
    ENDIF.
  ENDMETHOD.


  METHOD remove_translation_from_cache.
    DELETE TABLE translation_cache
            WITH TABLE KEY mode               = mode
                           source_language_id = source_language_id
                           target_language_id = target_language_id
                           source_text        = source_text.
  ENDMETHOD.
ENDCLASS.
