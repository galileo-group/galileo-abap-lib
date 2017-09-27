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


METHOD IF_BADI_SORTER~GET_DATA_FROM_SCREEN.
  DATA layer TYPE i.

  CALL FUNCTION 'BADI_SORTER_LAYER_GET'
    IMPORTING
      layer = layer.

  EXPORT layer = layer TO DATA BUFFER data.

ENDMETHOD.


method IF_BADI_SORTER~IS_LAYER_CHANGEABLE.
  changeable = me->changeable.
endmethod.


METHOD IF_BADI_SORTER~PUT_DATA_TO_SCREEN.

  DATA layer TYPE i.

  IF data IS NOT INITIAL.
    TRY.
        IMPORT layer = layer FROM DATA BUFFER data.
      CATCH cx_sy_import_format_error.  "#EC NO_HANDLER
    ENDTRY.
  ENDIF.

  CALL FUNCTION 'BADI_SORTER_LAYER_PUT'
    EXPORTING
      layer = layer.

ENDMETHOD.


method IF_BADI_SORTER~SET_LAYER_CHANGEABLE.
  CALL FUNCTION 'BADI_SORTER_LAYER_SET_CHANGE'
    EXPORTING
      changeable       = changeable.

endmethod.


METHOD IF_BADI_SORTER~SORT_IMPLS.
  TYPES: BEGIN OF int_struc,
         include TYPE  badiimpl_sort_line,
         layer TYPE i,
        END OF int_struc.
  DATA: int_tab TYPE TABLE OF int_struc,
        wa_ext type badiimpl_sort_line,
        wa_int type int_struc.

  loop at impls_to_sort into wa_ext.
    move-corresponding wa_ext to wa_int-include.
    IF wa_ext-sorter_data IS NOT INITIAL.
    TRY.
        IMPORT layer = wa_int-layer FROM DATA BUFFER wa_ext-sorter_data.
      CATCH cx_sy_import_format_error.  "#EC NO_HANDLER
    ENDTRY.
    ENDIF.
    append wa_int to int_tab.
  endloop.

  sort int_tab by layer.

  refresh impls_to_sort.

  loop at int_tab into wa_int.
    move-corresponding wa_int-include to wa_ext.
    append wa_ext to impls_to_sort.
  endloop.

ENDMETHOD.
ENDCLASS.
