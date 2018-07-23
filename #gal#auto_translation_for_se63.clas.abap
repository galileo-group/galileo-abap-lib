class /GAL/AUTO_TRANSLATION_FOR_SE63 definition
  public
  final
  create public .

public section.
  type-pools ABAP .
  type-pools LXESH .

  class-methods CREATE_PROPOSALS
    importing
      !EDITOR_SETTINGS type LXESH_EDITOR_SETTING
      !EDITOR_STORE type LXESH_TEXT_STORE
      !PROPOSALS type LXESH_PROPOSAL_TAB
    changing
      !EDITOR_DATA type LXESH_TEXT_EDITOR .
protected section.
private section.

  class-methods CONVERT_LANGUAGE_ID
    importing
      !LANGUAGE_ISO type LXEISOLANG
    returning
      value(LANGUAGE_ID) type LANGU .
ENDCLASS.



CLASS /GAL/AUTO_TRANSLATION_FOR_SE63 IMPLEMENTATION.


  METHOD convert_language_id.
    DATA l_language_iso TYPE laiso.

* Convert language ID from ISO to SAP
    l_language_iso = language_iso.

    TRANSLATE l_language_iso TO UPPER CASE.

    SELECT spras FROM t002 UP TO 1 ROWS INTO language_id
                WHERE laiso = l_language_iso.
      EXIT.
    ENDSELECT.                                            "#EC CI_SUBRC
  ENDMETHOD.


  METHOD create_proposals.
    CONSTANTS: lc_parameter_id     TYPE memoryid VALUE '/GAL/AUTO_TRANSLATE', "#EC NOTEXT
               lc_option_id_yes    TYPE string VALUE `YES`, "#EC NOTEXT
               lc_option_id_no     TYPE string VALUE `NO`,  "#EC NOTEXT
               lc_option_id_always TYPE string VALUE `ALWAYS`, "#EC NOTEXT
               lc_option_id_never  TYPE string VALUE `NEVER`. "#EC NOTEXT

    DATA: l_translator TYPE REF TO /gal/translator_deepl,

          BEGIN OF l_wa_worklist,
            source_text TYPE REF TO data,
            target_text TYPE REF TO data,
          END OF l_wa_worklist,

          l_worklist        LIKE STANDARD TABLE OF l_wa_worklist,

          l_src_lang_id     TYPE langu,
          l_tar_lang_id     TYPE langu,

          l_transl_settings TYPE xuvalue,

          l_option          TYPE /gal/cdlg_option,
          l_options         TYPE /gal/cdlg_options,

          l_exception       TYPE REF TO cx_root.

    FIELD-SYMBOLS: <l_editor_data>  LIKE LINE OF editor_data,
                   <l_editor_store> TYPE transl255,
                   <l_proposal>     LIKE LINE OF proposals,
                   <l_worklist>     LIKE LINE OF l_worklist,
                   <l_source_text>  TYPE csequence,
                   <l_target_text>  TYPE csequence.

* Create proposals for empty target texts
    LOOP AT editor_data ASSIGNING <l_editor_data> WHERE targettext IS INITIAL.

* Check if there already is a proposal
      READ TABLE proposals INDEX <l_editor_data>-proposal_tabix ASSIGNING <l_proposal>.
      IF sy-subrc = 0 AND <l_proposal>-ttext IS NOT INITIAL.
        <l_editor_data>-targettext = <l_proposal>-ttext.
        CONTINUE.
      ENDIF.

* Add text to worklist
      READ TABLE editor_store-transl255 INDEX <l_editor_data>-transl_tabix ASSIGNING <l_editor_store>.
      CHECK sy-subrc = 0.

      GET REFERENCE OF <l_editor_store>-sourcetext INTO l_wa_worklist-source_text.
      GET REFERENCE OF <l_editor_data>-targettext INTO l_wa_worklist-target_text.
      INSERT l_wa_worklist INTO TABLE l_worklist.
    ENDLOOP.

* Prepare translation
    IF l_worklist IS NOT INITIAL.

* Create translator
      CREATE OBJECT l_translator.

* Get language ID of source and target language
      l_src_lang_id = convert_language_id( editor_store-lxewrkob-sour_lang ).
      l_tar_lang_id = convert_language_id( editor_store-lxewrkob-targ_lang ).

      IF l_src_lang_id IS INITIAL OR l_tar_lang_id IS INITIAL.
        RETURN.
      ENDIF.

      GET PARAMETER ID lc_parameter_id FIELD l_transl_settings.
      IF l_transl_settings = lc_option_id_never.
        RETURN.
      ELSEIF l_transl_settings <> lc_option_id_always.

* Prompt for confirmation
        TRY.
            l_option-id          = lc_option_id_yes.
            l_option-is_selected = abap_true.
            l_option-text        = TEXT-o01.
            INSERT l_option INTO TABLE l_options.

            l_option-id          = lc_option_id_no.
            l_option-is_selected = abap_true.
            l_option-text        = TEXT-o02.
            INSERT l_option INTO TABLE l_options.

            l_option-id          = lc_option_id_always.
            l_option-is_selected = abap_true.
            l_option-text        = TEXT-o03.
            INSERT l_option INTO TABLE l_options.

            l_option-id          = lc_option_id_never.
            l_option-is_selected = abap_true.
            l_option-text        = TEXT-o04.
            INSERT l_option INTO TABLE l_options.

            /gal/common_dialog=>show_options_dialog( EXPORTING title         = TEXT-t01
                                                               message       = TEXT-m01
                                                               options_style = /gal/common_dialog=>dlg_options_style_single
                                                     CHANGING  options       = l_options ).

            READ TABLE l_options INTO l_option WITH KEY is_selected = abap_true.
            IF l_option-id = lc_option_id_always OR l_option-id = lc_option_id_never.
              l_transl_settings = l_option-id.
              SET PARAMETER ID lc_parameter_id FIELD l_transl_settings.
            ENDIF.

            IF l_option-id <> lc_option_id_yes AND l_option-id <> lc_option_id_always.
              RETURN.
            ENDIF.

          CATCH /gal/cx_dialog_exception.
            RETURN.

        ENDTRY.
      ENDIF.

* Create proposals for worklist
      LOOP AT l_worklist ASSIGNING <l_worklist>.
        ASSIGN <l_worklist>-source_text->* TO <l_source_text>.
        ASSIGN <l_worklist>-target_text->* TO <l_target_text>.

        TRY.
            <l_target_text> = l_translator->translate( source_language_id = l_src_lang_id
                                                       target_language_id = l_tar_lang_id
                                                       input              = <l_source_text> ).

          CATCH /gal/cx_translation_exception INTO l_exception.
            /gal/trace=>write_exception( l_exception ).

        ENDTRY.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
