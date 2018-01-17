*&---------------------------------------------------------------------*
*& Report  /GAL/DEVTOOLS_FIND_TEXT
*&---------------------------------------------------------------------*
*& Find text fragments
*&---------------------------------------------------------------------*

REPORT /gal/devtools_find_text LINE-SIZE 255 LINE-COUNT 0.

TYPE-POOLS abap.
TYPE-POOLS swbm.

TABLES t002.
TABLES tdevc.

DATA BEGIN OF g_cts_object.                                 "#EC NEEDED
INCLUDE TYPE cts_object.
DATA position TYPE eu_name.
DATA END OF g_cts_object.

SELECTION-SCREEN BEGIN OF BLOCK b00 WITH FRAME TITLE TEXT-b00.
SELECT-OPTIONS p_devc  FOR tdevc-devclass.
SELECT-OPTIONS p_langu FOR t002-spras DEFAULT sy-langu.

SELECTION-SCREEN SKIP.

PARAMETERS p_mess   AS CHECKBOX DEFAULT abap_true.
PARAMETERS p_otr    AS CHECKBOX DEFAULT abap_true.
PARAMETERS p_source AS CHECKBOX DEFAULT abap_true.
PARAMETERS p_tpool  AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK b00.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
PARAMETERS p_wildc RADIOBUTTON GROUP ptyp.
PARAMETERS p_regex RADIOBUTTON GROUP ptyp.                  "#EC NEEDED

SELECTION-SCREEN SKIP.

PARAMETERS p_patt  TYPE string LOWER CASE.

SELECTION-SCREEN SKIP.

PARAMETERS p_case  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b01.

START-OF-SELECTION.
  PERFORM main USING p_devc[] p_langu[] p_patt p_wildc p_case.

AT LINE-SELECTION.
  PERFORM navigate.

*&---------------------------------------------------------------------*
*&      Form  main
*&---------------------------------------------------------------------*
*       Main logic
*----------------------------------------------------------------------*
FORM main USING p_package_range  TYPE /gal/abap_repository=>t_package_range
                p_language_range TYPE /gal/abap_repository=>t_language_range
                p_pattern        TYPE string
                p_is_wildcard    TYPE abap_bool
                p_case_sensitive TYPE abap_bool.

  DATA l_repository     TYPE REF TO /gal/abap_repository.
  DATA l_languages      TYPE /gal/abap_repository=>t_languages.
  DATA l_tadir_objects  TYPE cts_objects.

  DATA l_regex          TYPE string.

* Convert pattern to regular expression
  IF p_is_wildcard = abap_true.
    l_regex = /gal/regex=>abap_wildcard_to_regex( input        = p_pattern
                                                  omit_anchors = abap_true ).
  ELSEIF p_pattern IS NOT INITIAL.
    l_regex = p_pattern.
  ELSE.
    l_regex = `.*`.
  ENDIF.

* Prepare repository access
  CREATE OBJECT l_repository.

* Find matching languages
  l_languages = l_repository->find_languages( language_range = p_language_range ).

  IF l_languages IS INITIAL.
    RETURN.
  ENDIF.

* Find matching TADIR objects
  l_tadir_objects = l_repository->find_tadir_objects( package_range = p_package_range ).

* Write list header
  ULINE /(223).

  WRITE: / sy-vline, (10)  TEXT-h01,                     "#EC NUMBER_OK
           sy-vline, (60)  TEXT-h02,                     "#EC NUMBER_OK
           sy-vline, (40)  TEXT-h03,                     "#EC NUMBER_OK
           sy-vline, (100) TEXT-h04,                     "#EC NUMBER_OK
           sy-vline.

  ULINE /(223).

* Find text in messages
  IF p_mess = abap_true.
    PERFORM find_message_short_texts USING l_tadir_objects l_languages l_regex p_case_sensitive.
  ENDIF.

* Find OTR short texts
  IF p_otr = abap_true.
    PERFORM find_otr_short_texts USING p_package_range l_languages l_regex p_case_sensitive.
  ENDIF.

* Find text in ABAP sources
  IF p_source = abap_true.
    PERFORM find_abap_source USING l_repository l_tadir_objects l_regex p_case_sensitive.
  ENDIF.

* Find text in text pools
  IF p_tpool = abap_true.
    PERFORM find_textpool_texts USING l_repository l_tadir_objects l_languages l_regex p_case_sensitive.
  ENDIF.

* Write list footer
  ULINE /(223).

* Initialize navigation
  CLEAR g_cts_object.
ENDFORM.                    "main

*&---------------------------------------------------------------------*
*&      Form  find_message_short_texts
*&---------------------------------------------------------------------*
*       Find message short texts
*----------------------------------------------------------------------*
FORM find_message_short_texts USING p_tadir_objects  TYPE cts_objects
                                    p_languages      TYPE /gal/abap_repository=>t_languages
                                    p_regex          TYPE string
                                    p_case_sensitive TYPE abap_bool.

  DATA l_message_number TYPE msgnr.
  DATA l_message_text   TYPE natxt.

  FIELD-SYMBOLS <l_tadir_object> LIKE LINE OF p_tadir_objects.
  FIELD-SYMBOLS <l_language>     LIKE LINE OF p_languages.

  LOOP AT p_tadir_objects ASSIGNING <l_tadir_object> WHERE pgmid = 'R3TR' AND object = 'MSAG'.
    LOOP AT p_languages ASSIGNING <l_language>.
      SELECT msgnr text
        FROM t100
        INTO (l_message_number, l_message_text)
       WHERE sprsl = <l_language>
         AND arbgb = <l_tadir_object>-obj_name           "#EC CI_BYPASS
       ORDER BY msgnr.                                  "#EC CI_GENBUFF

        g_cts_object-pgmid  = 'LIMU'.
        g_cts_object-object = 'MESS'.
        CONCATENATE <l_tadir_object>-obj_name l_message_number INTO g_cts_object-obj_name.
        CLEAR g_cts_object-position.

        PERFORM process_line USING `Message` <l_tadir_object>-obj_name l_message_number l_message_text p_regex p_case_sensitive. "#EC NOTEXT
      ENDSELECT.                                          "#EC CI_SUBRC
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    "find_message_short_texts

*&---------------------------------------------------------------------*
*&      Form  find_otr_short_texts
*&---------------------------------------------------------------------*
*       Find OTR short texts
*----------------------------------------------------------------------*
FORM find_otr_short_texts USING p_package_range  TYPE /gal/abap_repository=>t_package_range
                                p_languages      TYPE /gal/abap_repository=>t_languages
                                p_regex          TYPE string
                                p_case_sensitive TYPE abap_bool.
  DATA l_concept  TYPE sotr_conc.
  DATA l_type     TYPE string.

  DATA l_pgmid    TYPE pgmid.
  DATA l_object   TYPE trobjtype.
  DATA l_obj_name TYPE trobj_name.

  DATA l_text     TYPE string.

  FIELD-SYMBOLS <l_language> LIKE LINE OF p_languages.

  LOOP AT p_languages ASSIGNING <l_language>.
    SELECT sotr_head~concept sotr_use~pgmid sotr_use~object sotr_use~obj_name sotr_text~text
      FROM sotr_head
           INNER JOIN sotr_text
                   ON sotr_text~concept    = sotr_head~concept
                  AND sotr_text~flag_cntxt = 'X'
           INNER JOIN sotr_use
                   ON sotr_use~concept     = sotr_head~concept
      INTO (l_concept, l_pgmid, l_object, l_obj_name, l_text)
     WHERE paket IN p_package_range
       AND langu    = <l_language>
       AND flag_com = space
     ORDER BY sotr_use~pgmid sotr_use~object sotr_use~obj_name. "#EC CI_BUFFJOIN

      CONCATENATE `OTR (` l_object `)` INTO l_type.         "#EC NOTEXT

      g_cts_object-pgmid    = l_pgmid.
      g_cts_object-object   = l_object.
      g_cts_object-obj_name = l_obj_name.
      g_cts_object-position = l_concept.

      PERFORM process_line USING l_type l_obj_name l_concept l_text p_regex p_case_sensitive.
    ENDSELECT.                                            "#EC CI_SUBRC
  ENDLOOP.
ENDFORM.                    "find_otr_short_texts

*&---------------------------------------------------------------------*
*&      Form  find_textpool_texts
*&---------------------------------------------------------------------*
*       Find textpool texts
*----------------------------------------------------------------------*
FORM find_abap_source USING p_repository     TYPE REF TO /gal/abap_repository
                            p_tadir_objects  TYPE cts_objects
                            p_regex          TYPE string
                            p_case_sensitive TYPE abap_bool.

  DATA l_program      TYPE progname.
  DATA l_programs     TYPE /gal/abap_repository=>t_programs.
  DATA l_includes     TYPE /gal/abap_repository=>t_programs.
  DATA l_source_lines TYPE /gal/stringtable.

  FIELD-SYMBOLS <l_tadir_object> LIKE LINE OF p_tadir_objects.
  FIELD-SYMBOLS <l_program>      LIKE LINE OF l_programs.
  FIELD-SYMBOLS <l_source_line>  LIKE LINE OF l_source_lines.

  LOOP AT p_tadir_objects ASSIGNING <l_tadir_object>.
    l_program = p_repository->get_program( pgmid    = <l_tadir_object>-pgmid
                                           object   = <l_tadir_object>-object
                                           obj_name = <l_tadir_object>-obj_name ).

    CHECK l_program IS NOT INITIAL.

    INSERT l_program INTO TABLE l_programs.

    l_includes = p_repository->get_program_includes( l_program ).

    INSERT LINES OF l_includes INTO TABLE l_programs.
  ENDLOOP.

  SORT l_programs.
  DELETE ADJACENT DUPLICATES FROM l_programs.

  LOOP AT l_programs ASSIGNING <l_program>.
    READ REPORT <l_program> INTO l_source_lines.
    CHECK sy-subrc = 0.

    LOOP AT l_source_lines ASSIGNING <l_source_line>.
      g_cts_object-pgmid    = 'LIMU'.
      g_cts_object-object   = 'REPS'.
      g_cts_object-obj_name = <l_program>.
      g_cts_object-position = sy-tabix.

      PERFORM process_line USING `ABAP` <l_program> `` <l_source_line> p_regex p_case_sensitive. "#EC NOTEXT
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    "find_textpool_texts

*&---------------------------------------------------------------------*
*&      Form  find_textpool_texts
*&---------------------------------------------------------------------*
*       Find textpool texts
*----------------------------------------------------------------------*
FORM find_textpool_texts USING p_repository     TYPE REF TO /gal/abap_repository
                               p_tadir_objects  TYPE cts_objects
                               p_languages      TYPE /gal/abap_repository=>t_languages
                               p_regex          TYPE string
                               p_case_sensitive TYPE abap_bool.

  DATA l_program  TYPE progname.
  DATA l_textpool TYPE STANDARD TABLE OF textpool.
  DATA l_key      TYPE string.
  DATA l_text     TYPE string.

  FIELD-SYMBOLS <l_tadir_object> LIKE LINE OF p_tadir_objects.
  FIELD-SYMBOLS <l_language>     LIKE LINE OF p_languages.
  FIELD-SYMBOLS <l_text>         LIKE LINE OF l_textpool.

  LOOP AT p_tadir_objects ASSIGNING <l_tadir_object>.
    LOOP AT p_languages ASSIGNING <l_language>.
      l_program = p_repository->get_program( pgmid    = <l_tadir_object>-pgmid
                                             object   = <l_tadir_object>-object
                                             obj_name = <l_tadir_object>-obj_name ).

      CHECK l_program IS NOT INITIAL.

      READ TEXTPOOL l_program INTO l_textpool LANGUAGE <l_language>.
      IF sy-subrc = 0.
        LOOP AT l_textpool ASSIGNING <l_text>.
          CASE <l_text>-id.

            WHEN 'H'.
              CONCATENATE `HEADER` <l_text>-key(3) INTO l_key SEPARATED BY `-`.
              l_text = <l_text>-entry.

            WHEN 'I'.
              CONCATENATE `TEXT` <l_text>-key(3) INTO l_key SEPARATED BY `-`.
              l_text = <l_text>-entry.

            WHEN 'R'.
              l_key  = `PROGRAM TITLE`.
              l_text = <l_text>-entry.

            WHEN 'S'.
              IF <l_text>-entry(1) <> 'D'.
                l_key  = <l_text>-key.
                l_text = <l_text>-entry+8.
              ELSE.
                CONTINUE. "Text from dictionary
              ENDIF.

            WHEN 'T'.
              l_key  = `LIST HEADER`.
              l_text = <l_text>-entry.

            WHEN OTHERS.
              l_key  = `UNKNOWN`.
              l_text = <l_text>-entry.

          ENDCASE.

          g_cts_object-pgmid    = 'LIMU'.
          g_cts_object-object   = 'REPT'.
          g_cts_object-obj_name = l_program.
          CONCATENATE <l_text>-id <l_text>-key INTO g_cts_object-position.

          PERFORM process_line USING `Textpol` l_program l_key l_text p_regex p_case_sensitive. "#EC NOTEXT
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    "find_textpool_texts

*&---------------------------------------------------------------------*
*&      Form  process_line
*&---------------------------------------------------------------------*
FORM process_line USING p_type           TYPE csequence
                        p_name           TYPE csequence
                        p_key            TYPE csequence
                        p_text           TYPE csequence
                        p_regex          TYPE string
                        p_case_sensitive TYPE abap_bool.

  CONSTANTS lc_max_text_width     TYPE i VALUE 100.
  CONSTANTS lc_min_prefix_length  TYPE i VALUE 5.
  CONSTANTS lc_min_match_length   TYPE i VALUE 10.
  CONSTANTS lc_min_postfix_length TYPE i VALUE 5.

  DATA l_original_prefix_length  TYPE i.
  DATA l_original_match_length   TYPE i.
  DATA l_original_postfix_length TYPE i.
  DATA l_original_total_length   TYPE i.

  DATA l_display_prefix_length   TYPE i.
  DATA l_display_match_length    TYPE i.
  DATA l_display_postfix_length  TYPE i.
  DATA l_display_total_length    TYPE i.
  DATA l_display_gap_length      TYPE i.

  DATA l_min_prefix_length       TYPE i.
  DATA l_min_match_length        TYPE i.
  DATA l_min_postfix_length      TYPE i.

  DATA l_temp_length             TYPE i.
  DATA l_temp_offset             TYPE i.

  DATA l_text                    TYPE string.

* Ignore initial texts
  IF p_text IS INITIAL.
    RETURN.
  ENDIF.

  l_text = p_text.

* Check if text matches the pattern
  IF p_case_sensitive = abap_true.
    FIND FIRST OCCURRENCE OF REGEX p_regex IN l_text RESPECTING CASE MATCH OFFSET l_original_prefix_length MATCH LENGTH l_original_match_length.
  ELSE.
    FIND FIRST OCCURRENCE OF REGEX p_regex IN l_text IGNORING CASE MATCH OFFSET l_original_prefix_length MATCH LENGTH l_original_match_length.
  ENDIF.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* Calculate display length of prefix, postfix and match
  l_original_total_length   = strlen( l_text ).
  l_original_postfix_length = l_original_total_length - l_original_prefix_length - l_original_match_length.

  DO.

* Calculate minimum part length
    IF l_original_prefix_length > lc_min_prefix_length.
      l_min_prefix_length = lc_min_prefix_length.
    ELSE.
      l_min_prefix_length = l_original_prefix_length.
    ENDIF.

    IF l_original_match_length > lc_min_match_length.
      l_min_match_length = lc_min_match_length.
    ELSE.
      l_min_match_length = l_original_match_length.
    ENDIF.

    IF l_original_postfix_length > lc_min_postfix_length.
      l_min_postfix_length = lc_min_postfix_length.
    ELSE.
      l_min_postfix_length = l_original_postfix_length.
    ENDIF.

* Check if full text can be displayed
    IF l_original_total_length <= lc_max_text_width.
      l_display_prefix_length  = l_original_prefix_length.
      l_display_match_length   = l_original_match_length.
      l_display_postfix_length = l_original_postfix_length.
      l_display_total_length   = l_original_total_length.
      EXIT.
    ENDIF.

* Check if text can be displayed with abbreviated postfix
    l_temp_length = l_original_prefix_length + l_original_match_length + l_min_postfix_length.

    IF l_temp_length <= lc_max_text_width.
      l_display_prefix_length  = l_original_prefix_length.
      l_display_match_length   = l_original_match_length.
      l_display_postfix_length = lc_max_text_width - l_original_prefix_length - l_original_match_length.
      l_display_total_length   = lc_max_text_width.
      EXIT.
    ENDIF.

* Check if text can be displayed with abbreviated prefix and postfix
    l_temp_length = l_min_prefix_length + l_original_match_length + l_min_postfix_length.

    IF l_temp_length <= lc_max_text_width.
      l_display_prefix_length  = lc_max_text_width - l_original_match_length - l_min_postfix_length.
      l_display_match_length   = l_original_match_length.
      l_display_postfix_length = l_min_postfix_length.
      l_display_total_length   = lc_max_text_width.
      EXIT.
    ENDIF.

* Prefix, postfix and match need to be abbreviated
    l_temp_length = l_min_prefix_length + l_min_match_length + l_min_postfix_length.

    IF l_temp_length <= lc_max_text_width.
      l_display_prefix_length  = l_min_prefix_length.
      l_display_match_length   = lc_max_text_width - l_min_prefix_length - l_min_postfix_length.
      l_display_postfix_length = l_min_postfix_length.
      l_display_total_length   = lc_max_text_width.
      EXIT.
    ENDIF.

* No possible layout due to inconsistent setting of constants
    RETURN.
  ENDDO.

* Write object details
  WRITE: / sy-vline, (10) p_type,                        "#EC NUMBER_OK
           sy-vline, (60) p_name,                        "#EC NUMBER_OK
           sy-vline, (40) p_key,                         "#EC NUMBER_OK
           sy-vline NO-GAP.

* Write leading gap (in order to avoid early change fo color)
  WRITE space NO-GAP.

* Write prefix
  IF l_display_prefix_length < l_original_prefix_length.
    l_temp_length = l_display_prefix_length - 3.
    l_temp_offset = l_original_prefix_length - l_temp_length.
    WRITE `...` NO-GAP.
    WRITE l_text+l_temp_offset(l_temp_length) NO-GAP.
  ELSEIF l_display_prefix_length > 0.
    l_temp_length = l_display_prefix_length.
    l_temp_offset = 0.
    WRITE l_text+l_temp_offset(l_temp_length) NO-GAP.
  ENDIF.

* Write match
  FORMAT COLOR COL_KEY.

  IF l_display_match_length < l_original_match_length.
    l_temp_length = ( l_display_match_length - 3 ) / 2.
    l_temp_offset = l_original_prefix_length.
    WRITE l_text+l_temp_offset(l_temp_length) NO-GAP.
    WRITE `...` NO-GAP.
    l_temp_length = l_display_match_length - 3 - l_temp_length.
    l_temp_offset = l_original_prefix_length + l_original_match_length - l_temp_length.
    WRITE l_text+l_temp_offset(l_temp_length) NO-GAP.
  ELSEIF l_display_match_length > 0.
    l_temp_length = l_display_match_length.
    l_temp_offset = l_original_prefix_length.
    WRITE l_text+l_temp_offset(l_temp_length) NO-GAP.
  ENDIF.

  FORMAT COLOR COL_BACKGROUND.

* Write postfix
  IF l_display_postfix_length < l_original_postfix_length.
    l_temp_length = l_display_postfix_length - 3.
    l_temp_offset = l_original_prefix_length + l_original_match_length.
    WRITE l_text+l_temp_offset(l_temp_length) NO-GAP.
    WRITE `...` NO-GAP.
  ELSEIF l_display_postfix_length > 0.
    l_temp_length = l_display_postfix_length.
    l_temp_offset = l_original_prefix_length + l_original_match_length.
    WRITE l_text+l_temp_offset(l_temp_length) NO-GAP.
  ENDIF.

* Write trailing gap
  l_display_gap_length = lc_max_text_width - l_display_total_length.

  IF l_display_gap_length > 0.
    WRITE AT (l_display_gap_length) space.
  ELSE.
    WRITE space NO-GAP.
  ENDIF.

  WRITE sy-vline.

* Hide navigation data
  HIDE g_cts_object-pgmid.
  HIDE g_cts_object-object.
  HIDE g_cts_object-obj_name.
  HIDE g_cts_object-position.
ENDFORM.                    "process_line

*&---------------------------------------------------------------------*
*&      Form  navigate
*&---------------------------------------------------------------------*
*       Navigation handler
*----------------------------------------------------------------------*
FORM navigate.
  DATA l_obj_name          TYPE seu_objkey.

  DATA l_wb_request        TYPE REF TO cl_wb_request.
  DATA l_wb_requests       LIKE STANDARD TABLE OF l_wb_request.

  DATA l_wb_textpool_state TYPE REF TO cl_wb_textpool_state.

  IF g_cts_object IS NOT INITIAL.
    CASE g_cts_object-object.

      WHEN 'REPS'.
        CALL FUNCTION 'RS_TOOL_ACCESS'
          EXPORTING
            operation   = 'SHOW'
            object_type = g_cts_object-object
            object_name = g_cts_object-obj_name
            position    = g_cts_object-position
          EXCEPTIONS
            OTHERS      = 0.

      WHEN 'REPT'.
        CREATE OBJECT l_wb_textpool_state.

        l_wb_textpool_state->textkey  = g_cts_object-position+1.
        l_wb_textpool_state->texttype = g_cts_object-position(1).

        l_obj_name = g_cts_object-obj_name.

        CREATE OBJECT l_wb_request
          EXPORTING
            p_object_type  = 'PX'
            p_object_name  = l_obj_name
            p_operation    = swbm_c_op_display
            p_object_state = l_wb_textpool_state.

        INSERT l_wb_request INTO TABLE l_wb_requests.

        cl_wb_startup=>start( EXPORTING p_wb_request_set = l_wb_requests ).

      WHEN OTHERS.
        CALL FUNCTION 'TR_OBJECT_JUMP_TO_TOOL'
          EXPORTING
            iv_pgmid    = g_cts_object-pgmid
            iv_object   = g_cts_object-object
            iv_obj_name = g_cts_object-obj_name
          EXCEPTIONS
            OTHERS      = 0.

    ENDCASE.

    CLEAR g_cts_object.
  ENDIF.
ENDFORM.                    "navigate
