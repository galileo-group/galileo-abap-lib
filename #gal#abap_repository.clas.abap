*----------------------------------------------------------------------*
*       CLASS /GAL/ABAP_REPOSITORY DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class /GAL/ABAP_REPOSITORY definition
  public
  final
  create public .

public section.

  types:
    t_language_range TYPE RANGE OF langu .
  types:
    t_package_range TYPE RANGE OF devclass .
  types:
    t_program_range TYPE RANGE OF progname .
  types:
    t_languages TYPE STANDARD TABLE OF langu WITH DEFAULT KEY .
  types:
    t_programs TYPE STANDARD TABLE OF progname WITH DEFAULT KEY .

  methods FIND_LANGUAGES
    importing
      !LANGUAGE_RANGE type T_LANGUAGE_RANGE
    returning
      value(LANGUAGES) type T_LANGUAGES .
  methods FIND_PROGRAMS
    importing
      !PROGRAM_RANGE type T_PROGRAM_RANGE
      !PACKAGE_RANGE type T_PACKAGE_RANGE
    returning
      value(PROGRAMS) type T_PROGRAMS .
  methods FIND_TADIR_OBJECTS
    importing
      !PACKAGE_RANGE type T_PACKAGE_RANGE
    returning
      value(OBJECTS) type CTS_OBJECTS .
  methods GET_PROGRAM
    importing
      !PGMID type PGMID
      !OBJECT type TROBJTYPE
      !OBJ_NAME type TROBJ_NAME
    returning
      value(PROGRAM) type PROGNAME .
  methods GET_PROGRAM_INCLUDES
    importing
      !PROGRAM type PROGNAME
    returning
      value(INCLUDES) type T_PROGRAMS .
protected section.
private section.
ENDCLASS.



CLASS /GAL/ABAP_REPOSITORY IMPLEMENTATION.


METHOD find_languages.

* Find known languages matching selection criteria
  SELECT spras
    FROM t002
    INTO TABLE languages
   WHERE spras IN language_range.                         "#EC CI_SUBRC

* Sort result
  SORT languages.
ENDMETHOD.


METHOD find_programs.
  DATA l_wa_e071 TYPE e071.
  DATA l_tadir   TYPE tadir.

  FIELD-SYMBOLS <l_program> LIKE LINE OF programs.

* Build list of matching program names
  SELECT name
    FROM trdir
    INTO TABLE programs
   WHERE name IN program_range.                           "#EC CI_SUBRC

* Restrict by  package
  IF package_range IS NOT INITIAL.
    l_wa_e071-pgmid  = 'LIMU'.
    l_wa_e071-object = 'REPS'.

    LOOP AT programs ASSIGNING <l_program>.
      l_wa_e071-obj_name = <l_program>.

      CALL FUNCTION 'TR_CHECK_TYPE'
        EXPORTING
          wi_e071  = l_wa_e071
        IMPORTING
          we_tadir = l_tadir.

      SELECT SINGLE devclass
               FROM tadir
               INTO l_tadir
              WHERE pgmid    = l_tadir-pgmid
                AND object   = l_tadir-object
                AND obj_name = l_tadir-obj_name.

      CHECK sy-subrc <> 0
         OR l_tadir-devclass NOT IN package_range.

      DELETE programs.                                    "#EC CI_SUBRC
    ENDLOOP.
  ENDIF.

* Sort result
  SORT programs.
ENDMETHOD.


METHOD find_tadir_objects.

* Select TADIR objects
  SELECT pgmid object obj_name
    FROM tadir
    INTO TABLE objects                                    "#EC CI_SUBRC
   WHERE devclass IN package_range.                     "#EC CI_GENBUFF

* Sort result
  SORT objects.
ENDMETHOD.


METHOD get_program.
  DATA l_namespace      TYPE namespace.
  DATA l_function_group TYPE rs38l_area.

  DATA l_class          TYPE seocpdkey-clsname.
  DATA l_method         TYPE seocpdkey-cpdname.

  DATA l_incl_naming    TYPE REF TO if_oo_class_incl_naming.
  DATA l_cifref         TYPE REF TO if_oo_clif_incl_naming.

  CASE pgmid.

    WHEN 'R3TR'.
      CASE object.

        WHEN 'CLAS'.
          l_class        = obj_name(30).
          cl_oo_include_naming=>get_instance_by_name(
            EXPORTING
              name   = l_class
            RECEIVING
              cifref = l_cifref
            EXCEPTIONS
              OTHERS = 0
          ). "Handle Exception OTHERS for Objects deleted but not yet released.
          IF NOT l_cifref IS INITIAL.
            l_incl_naming ?= l_cifref.
            program        = l_incl_naming->class_pool.
          ENDIF.

        WHEN 'FUGR'.
          l_function_group = obj_name.

          CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
            EXPORTING
              complete_area = l_function_group
            IMPORTING
              namespace     = l_namespace
              group         = l_function_group
            EXCEPTIONS
              OTHERS        = 1.
          IF sy-subrc = 0.
            CONCATENATE l_namespace 'SAPL' l_function_group INTO program.
          ENDIF.

        WHEN 'PROG'.
          program = obj_name.

        WHEN OTHERS.
          CLEAR program. "Currently not supported.

      ENDCASE.

    WHEN 'LIMU'.
      CASE object.

        WHEN 'CPRI' OR 'CPRO' OR 'CPUB'.
          l_class        = obj_name(30).
          cl_oo_include_naming=>get_instance_by_name(
            EXPORTING
              name   = l_class
            RECEIVING
              cifref = l_cifref
            EXCEPTIONS
              OTHERS = 0
          ). "Handle Exception OTHERS for Objects deleted but not yet released.
          IF NOT l_cifref IS INITIAL.
            l_incl_naming ?= l_cifref.
            program        = l_incl_naming->get_include_by_section( object ).
          ENDIF.

        WHEN 'METH'.
          l_class        = obj_name(30).
          cl_oo_include_naming=>get_instance_by_name(
            EXPORTING
              name   = l_class
            RECEIVING
              cifref = l_cifref
            EXCEPTIONS
              OTHERS = 0
          ). "Handle Exception OTHERS for Objects deleted but not yet released.
          IF NOT l_cifref IS INITIAL.
            l_incl_naming ?= l_cifref.
            l_method       = obj_name+30.
            program        = l_incl_naming->get_include_by_mtdname( l_method ).
          ENDIF.

        WHEN 'REPO' OR 'REPS' OR 'REPT'.
          program = obj_name.

        WHEN OTHERS.
          CLEAR program. "Currently not supported.

      ENDCASE.

    WHEN OTHERS.
      CLEAR program. "Currently not supported.

  ENDCASE.
ENDMETHOD.


METHOD get_program_includes.
  CALL FUNCTION 'RS_GET_ALL_INCLUDES'
    EXPORTING
      program    = program
    TABLES
      includetab = includes
    EXCEPTIONS
      OTHERS     = 0.
ENDMETHOD.
ENDCLASS.
