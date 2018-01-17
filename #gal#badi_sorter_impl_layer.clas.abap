class /GAL/BADI_SORTER_IMPL_LAYER definition
  public
  final
  create public .

*"* public components of class /GAL/BADI_SORTER_IMPL_LAYER
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_BADI_SORTER .
protected section.
*"* protected components of class CL_BADI_SORTER_IMPL_LAYER
*"* do not include other source files here!!!
private section.
*"* private components of class CL_BADI_SORTER_IMPL_LAYER
*"* do not include other source files here!!!

  data CHANGEABLE type ENHBOOLEAN .
ENDCLASS.



CLASS /GAL/BADI_SORTER_IMPL_LAYER IMPLEMENTATION.


METHOD if_badi_sorter~get_data_from_screen.
  DATA l_layer TYPE i.

  CALL FUNCTION 'BADI_SORTER_LAYER_GET'
    IMPORTING
      layer = l_layer.

  EXPORT layer = l_layer TO DATA BUFFER data.
ENDMETHOD.


method IF_BADI_SORTER~IS_LAYER_CHANGEABLE.
  changeable = me->changeable.
endmethod.


METHOD if_badi_sorter~put_data_to_screen.
  DATA l_layer TYPE i.

  IF data IS NOT INITIAL.
    TRY.
        IMPORT layer = l_layer FROM DATA BUFFER data.
      CATCH cx_sy_import_format_error.                  "#EC NO_HANDLER
    ENDTRY.
  ENDIF.

  CALL FUNCTION 'BADI_SORTER_LAYER_PUT'
    EXPORTING
      layer = l_layer.
ENDMETHOD.


method IF_BADI_SORTER~SET_LAYER_CHANGEABLE.
  CALL FUNCTION 'BADI_SORTER_LAYER_SET_CHANGE'
    EXPORTING
      changeable       = changeable.

endmethod.


METHOD if_badi_sorter~sort_impls.
  TYPES: BEGIN OF lt_int_struc,
           include TYPE badiimpl_sort_line,
           layer   TYPE i,
         END OF lt_int_struc.

  DATA: l_int_tab TYPE TABLE OF lt_int_struc,
        l_wa_ext  TYPE badiimpl_sort_line,
        l_wa_int  TYPE lt_int_struc.

  LOOP AT impls_to_sort INTO l_wa_ext.
    MOVE-CORRESPONDING l_wa_ext TO l_wa_int-include.
    IF l_wa_ext-sorter_data IS NOT INITIAL.
      TRY.
          IMPORT layer = l_wa_int-layer FROM DATA BUFFER l_wa_ext-sorter_data.
        CATCH cx_sy_import_format_error.                "#EC NO_HANDLER
      ENDTRY.
    ENDIF.
    APPEND l_wa_int TO l_int_tab.
  ENDLOOP.

  SORT l_int_tab BY layer.

  REFRESH impls_to_sort.

  LOOP AT l_int_tab INTO l_wa_int.
    MOVE-CORRESPONDING l_wa_int-include TO l_wa_ext.
    INSERT l_wa_ext INTO TABLE impls_to_sort.
  ENDLOOP.
ENDMETHOD.
ENDCLASS.
