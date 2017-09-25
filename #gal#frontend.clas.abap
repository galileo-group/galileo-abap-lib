class /GAL/FRONTEND definition
  public
  create public .

public section.
  type-pools ABAP .
  type-pools CNTL .

  class-data COLOR_AFFIRMATIVE_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_AFFIRMATIVE_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_APPL_TOOLBAR_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_APPL_TOOLBAR_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_BOTTOM_BORDER_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_BOTTOM_BORDER_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_BUTTON_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_BUTTON_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_CHECKBOX_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_CHECKBOX_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_DIALOG_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_DIALOG_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_DIALOG_TITLE_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_DIALOG_TITLE_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_DROPDOWN_ENTRY_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_DROPDOWN_ENTRY_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_ENTRY_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_ENTRY_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_ENTRY_READONLY_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_ENTRY_READONLY_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_ERROR_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_ERROR_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_FRAME_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_FRAME_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_FRAME_TITLE_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_FRAME_TITLE_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_LABEL_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_LABEL_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_MENUITEM_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_MENUITEM_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_MENU_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_MENU_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_RADIO_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_RADIO_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_SCROLLBAR_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_SCROLLBAR_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_STATUSBAR_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_STATUSBAR_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_SUBSCR_INNERBORDER_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_SUBSCR_INNERBORDER_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_SUBSCR_OUTERBORDER_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_SUBSCR_OUTERBORDER_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_TABSTRIP_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_TABSTRIP_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_TAB_BUTTON_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_TAB_BUTTON_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_TITLE_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_TITLE_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_TOOLBAR_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_TOOLBAR_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_WARNING_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_WARNING_FG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_WINDOW_BG type ref to /GAL/COLOR_RGB read-only .
  class-data COLOR_WINDOW_FG type ref to /GAL/COLOR_RGB read-only .
  class-data FONT_FIXED type ref to /GAL/FONT_INFO read-only .
  class-data FONT_PROPORTIONAL type ref to /GAL/FONT_INFO read-only .

  class-methods CLASS_CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS /GAL/FRONTEND IMPLEMENTATION.


METHOD class_constructor.
  DATA BEGIN OF l_wa_colors.
  DATA   id         TYPE i.
  DATA   name_fg    TYPE string.
  DATA   name_bg    TYPE string.
  DATA   default_fg TYPE i.
  DATA   default_bg TYPE i.
  DATA END OF l_wa_colors.

  DATA l_colors LIKE HASHED TABLE OF l_wa_colors WITH UNIQUE KEY id.

  DATA l_gui_available            TYPE abap_bool.

  DATA l_background_colors        TYPE cntl_col_value_tab.
  DATA l_wa_background_colors     TYPE cntl_col_value.

  DATA l_foreground_colors        TYPE cntl_col_value_tab.
  DATA l_wa_foreground_colors     TYPE cntl_col_value.

  DATA l_fontname(80)             TYPE c.

  DATA l_fontname_fixed           TYPE char80 VALUE `Courier New`. "#EC NOTEXT
  DATA l_fontsize_fixed           TYPE i VALUE 90000.
  DATA l_fontsize_fixed_pt        TYPE f.

  DATA l_fontname_proportional    TYPE string VALUE `Tahoma`. "#EC NOTEXT
  DATA l_fontsize_proportional    TYPE i VALUE 90000.
  DATA l_fontsize_proportional_pt TYPE f.

  FIELD-SYMBOLS <l_color_id>         TYPE i.
  FIELD-SYMBOLS <l_color>            LIKE l_wa_colors.
  FIELD-SYMBOLS <l_foreground_color> TYPE cntl_col_value.
  FIELD-SYMBOLS <l_background_color> TYPE cntl_col_value.
  FIELD-SYMBOLS <l_color_attribute>  TYPE REF TO /gal/color_rgb.

* Macro for color declaration
  DEFINE define_color.
    assign (&1) to <l_color_id>.

    if sy-subrc = 0.
      l_wa_colors-id = <l_color_id>.
    else.
      l_wa_colors-id = &2.
    endif.

    l_wa_colors-default_fg = &4.
    l_wa_colors-default_bg = &5.

    concatenate &3 `_FG` into l_wa_colors-name_fg.
    concatenate &3 `_BG` into l_wa_colors-name_bg.

    insert l_wa_colors into table l_colors.

    if l_wa_colors-id >= 0.
      l_wa_foreground_colors-state = 0.
      l_wa_foreground_colors-id    = l_wa_colors-id.
      l_wa_foreground_colors-value = -1.
      insert l_wa_foreground_colors into table l_foreground_colors.

      l_wa_background_colors-state = 0.
      l_wa_background_colors-id    = l_wa_colors-id.
      l_wa_background_colors-value = -1.
      insert l_wa_background_colors into table l_background_colors.
    endif.
  END-OF-DEFINITION.

* Check if there is a GUI
  CALL FUNCTION 'GUI_IS_AVAILABLE'
    IMPORTING
      return = l_gui_available.

* Build table of colors to be determined
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_LABEL`               30 `COLOR_LABEL`                     0 16183786.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_ENTRY`               31 `COLOR_ENTRY`                     0 16777215.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_BUTTON`              32 `COLOR_BUTTON`                    0 14199939.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_RADIO`               33 `COLOR_RADIO`                     0 16250098.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_CHECKBOX`            34 `COLOR_CHECKBOX`                  0 16250098.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_FRAME`               35 `COLOR_FRAME`              16051166 16051166.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_WINDOW`              36 `COLOR_WINDOW`             16183786 16183786.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_DIALOG`              37 `COLOR_DIALOG`             16183786 16183786.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_SCROLLBAR`           38 `COLOR_SCROLLBAR`                 0 16183786.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_MENU`                39 `COLOR_MENU`                6699043 16250098.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_TITLE`               40 `COLOR_TITLE`                     0 14991779.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_APPL_TOOLBAR`        41 `COLOR_APPL_TOOLBAR`              0 14199939.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_TOOL_TB`             42 `COLOR_TOOLBAR`                   0 14199939.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_STATUSBAR`           43 `COLOR_STATUSBAR`                 0 16579836.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_MENUITEM`            44 `COLOR_MENUITEM`            6699043 16250098.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_DLGTITLE`            45 `COLOR_DIALOG_TITLE`              0 14991779.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_AFFIRMATIV`          46 `COLOR_AFFIRMATIVE`             255      255.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_WARNING`             47 `COLOR_WARNING`                 255      255.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_ERROR`               48 `COLOR_ERROR`                   255      255.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_ENTRY_READONLY`      49 `COLOR_ENTRY_READONLY`            0 16777215.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_TABBUTTON`           50 `COLOR_TAB_BUTTON`                0 15591392.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_FRAME_TITLE`         51 `COLOR_FRAME_TITLE`               0 15591392.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_DROPDOWN_ENTRY`      52 `COLOR_DROPDOWN_ENTRY`            0 16777215.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_SUBSCR_OUTERBORDER`  53 `COLOR_SUBSCR_OUTERBORDER` 15391691      255.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_SUBSCR_INNERBORDER`  54 `COLOR_SUBSCR_INNERBORDER` 15391691      255.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_TABSTRIP`            55 `COLOR_TABSTRIP`                255      255.
  define_color `CL_GUI_RESOURCES=>COLOR_FORM_ENTRY_BOTTOM_BORDER` 56 `COLOR_BOTTOM_BORDER`      12303032      255.

* Read GUI settings
  IF l_gui_available = abap_true.
    cl_gui_resources=>get_colors( CHANGING   table_of_bg_colors = l_background_colors
                                             table_of_fg_colors = l_foreground_colors
                                  EXCEPTIONS OTHERS = 0 ).

    cl_gui_resources=>get_fontname( IMPORTING  fontname = l_fontname_proportional
                                    EXCEPTIONS OTHERS   = 0 ).

    cl_gui_resources=>get_fontsize( IMPORTING  fontsize = l_fontsize_proportional
                                    EXCEPTIONS OTHERS   = 0 ).

    cl_gui_resources=>get_font_infos( IMPORTING  fontname   = l_fontname_fixed
                                                 fontsize   = l_fontsize_fixed
                                      EXCEPTIONS OTHERS     = 0 ).

    cl_gui_cfw=>flush( ).
  ENDIF.

* Create font info objects
  l_fontsize_fixed_pt        = l_fontsize_fixed / 10000.
  l_fontsize_proportional_pt = l_fontsize_proportional / 10000.

  CREATE OBJECT font_fixed
    EXPORTING
      name       = l_fontname_fixed
      size_value = l_fontsize_fixed_pt
      size_unit  = /gal/length=>unit_pt.

  CREATE OBJECT font_proportional
    EXPORTING
      name       = l_fontname_proportional
      size_value = l_fontsize_proportional_pt
      size_unit  = /gal/length=>unit_pt.

* Create color objects
  LOOP AT l_colors ASSIGNING <l_color>.
    ASSIGN (<l_color>-name_bg) TO <l_color_attribute>.
    IF sy-subrc = 0.
      READ TABLE l_background_colors
            WITH KEY id = <l_color>-id
                 ASSIGNING <l_background_color>.         "#EC CI_STDSEQ
      IF sy-subrc = 0 AND <l_background_color>-value >= 0.
        <l_color_attribute> = /gal/color_rgb=>from_rgb24_int( <l_background_color>-value ).
      ELSE.
        <l_color_attribute> = /gal/color_rgb=>from_rgb24_int( <l_color>-default_bg ).
      ENDIF.
    ENDIF.

    ASSIGN (<l_color>-name_fg) TO <l_color_attribute>.
    IF sy-subrc = 0.
      READ TABLE l_foreground_colors
            WITH KEY id = <l_color>-id
                 ASSIGNING <l_foreground_color>.         "#EC CI_STDSEQ
      IF sy-subrc = 0 AND <l_foreground_color>-value >= 0.
        <l_color_attribute> = /gal/color_rgb=>from_rgb24_int( <l_foreground_color>-value ).
      ELSE.
        <l_color_attribute> = /gal/color_rgb=>from_rgb24_int( <l_color>-default_fg ).
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDMETHOD.
ENDCLASS.
