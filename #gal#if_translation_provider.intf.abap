interface /GAL/IF_TRANSLATION_PROVIDER
  public .


  constants TRANSLATION_MODE_HTML type STRING value `HTML`. "#EC NOTEXT
  constants TRANSLATION_MODE_TEXT type STRING value `TEXT`. "#EC NOTEXT
  constants TRANSLATION_MODE_XML type STRING value `XML`. "#EC NOTEXT

  methods TRANSLATE
    importing
      !MODE type STRING default TRANSLATION_MODE_TEXT
      !SOURCE_LANGUAGE_ID type LANGU
      !TARGET_LANGUAGE_ID type LANGU
      !INPUT type CSEQUENCE
    returning
      value(OUTPUT) type STRING
    raising
      /GAL/CX_TRANSLATION_EXCEPTION .
endinterface.
