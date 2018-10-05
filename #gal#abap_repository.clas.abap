*----------------------------------------------------------------------*
*       CLASS /GAL/ABAP_REPOSITORY DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
"! <p class="shorttext synchronized" lang="en">ABAP Repository</p>
CLASS /gal/abap_repository DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Classes (Range)</p>
      t_class_range TYPE RANGE OF seoclsname.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Classe categories (Range)</p>
      t_class_category_range TYPE RANGE OF seocategry.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Languages (Range)</p>
      t_language_range TYPE RANGE OF langu.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Packages (Range)</p>
      t_package_range TYPE RANGE OF devclass.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Programs (Range)</p>
      t_program_range TYPE RANGE OF progname.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Languages (Table of language IDs)</p>
      t_languages TYPE STANDARD TABLE OF langu WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Classes (Table of class names)</p>
      t_classes TYPE STANDARD TABLE OF seoclsname WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Packages (Table of package names)</p>
      t_packages TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Programs (Table of program names)</p>
      t_programs TYPE STANDARD TABLE OF progname WITH DEFAULT KEY.

    "! <p class="shorttext synchronized" lang="en">Find ABAP classes</p>
    "!
    "! @parameter class_range          | <p class="shorttext synchronized" lang="en">Classes (Range)</p>
    "! @parameter class_category_range | <p class="shorttext synchronized" lang="en">Classe categories (Range)</p>
    "! @parameter package_range        | <p class="shorttext synchronized" lang="en">Packages (Range)</p>
    "! @parameter classes              | <p class="shorttext synchronized" lang="en">ABAP class names</p>
    METHODS find_classes
      IMPORTING
        !class_range          TYPE t_class_range          OPTIONAL
        !class_category_range TYPE t_class_category_range OPTIONAL
        !package_range        TYPE t_package_range        OPTIONAL
      RETURNING
        VALUE(classes)        TYPE t_classes.

    "! <p class="shorttext synchronized" lang="en">Find ABAP interfaces</p>
    "!
    "! @parameter interface_range | <p class="shorttext synchronized" lang="en">Interfaces (Range)</p>
    "! @parameter package_range   | <p class="shorttext synchronized" lang="en">Packages (Range)</p>
    "! @parameter interfaces      | <p class="shorttext synchronized" lang="en">ABAP interface names</p>
    METHODS find_interfaces
      IMPORTING
        !interface_range  TYPE t_class_range   OPTIONAL
        !package_range    TYPE t_package_range OPTIONAL
      RETURNING
        VALUE(interfaces) TYPE t_classes.

    "! <p class="shorttext synchronized" lang="en">Find known languages</p>
    "!
    "! @parameter language_range | <p class="shorttext synchronized" lang="en">Languages (Range)</p>
    "! @parameter languages      | <p class="shorttext synchronized" lang="en">Languages</p>
    METHODS find_languages
      IMPORTING
        !language_range  TYPE t_language_range OPTIONAL
      RETURNING
        VALUE(languages) TYPE t_languages.

    "! <p class="shorttext synchronized" lang="en">Find ABAP sources</p>
    "!
    "! @parameter package_range | <p class="shorttext synchronized" lang="en">Packages (Range)</p>
    "! @parameter packages      | <p class="shorttext synchronized" lang="en">Packages</p>
    METHODS find_packages
      IMPORTING
        !package_range  TYPE t_package_range OPTIONAL
      RETURNING
        VALUE(packages) TYPE t_packages.

    "! <p class="shorttext synchronized" lang="en">Find ABAP sources</p>
    "!
    "! @parameter program_range | <p class="shorttext synchronized" lang="en">Programs (Range)</p>
    "! @parameter package_range | <p class="shorttext synchronized" lang="en">Packages (Range)</p>
    "! @parameter programs      | <p class="shorttext synchronized" lang="en">ABAP program names</p>
    METHODS find_programs
      IMPORTING
        !program_range  TYPE t_program_range OPTIONAL
        !package_range  TYPE t_package_range OPTIONAL
      RETURNING
        VALUE(programs) TYPE t_programs.

    "! <p class="shorttext synchronized" lang="en">Find TADIR objects</p>
    "!
    "! @parameter package_range | <p class="shorttext synchronized" lang="en">Packages (Range)</p>
    "! @parameter objects       | <p class="shorttext synchronized" lang="en">TADIR objects</p>
    METHODS find_tadir_objects
      IMPORTING
        !package_range TYPE t_package_range OPTIONAL
      RETURNING
        VALUE(objects) TYPE cts_objects.

    "! <p class="shorttext synchronized" lang="en">Get program for TADIR/CTS object</p>
    "!
    "! @parameter pgmid    | <p class="shorttext synchronized" lang="en">Program ID in Requests and Tasks</p>
    "! @parameter object   | <p class="shorttext synchronized" lang="en">Object Type</p>
    "! @parameter obj_name | <p class="shorttext synchronized" lang="en">Object Name in Object List</p>
    "! @parameter program  | <p class="shorttext synchronized" lang="en">Program</p>
    METHODS get_program
      IMPORTING
        !pgmid         TYPE pgmid
        !object        TYPE trobjtype
        !obj_name      TYPE trobj_name
      RETURNING
        VALUE(program) TYPE progname.

    "! <p class="shorttext synchronized" lang="en">Get includes of a specific program</p>
    "!
    "! @parameter program  | <p class="shorttext synchronized" lang="en">Program</p>
    "! @parameter includes | <p class="shorttext synchronized" lang="en">Includes</p>
    METHODS get_program_includes
      IMPORTING
        !program        TYPE progname
      RETURNING
        VALUE(includes) TYPE t_programs.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /GAL/ABAP_REPOSITORY IMPLEMENTATION.


  METHOD find_classes.
    TYPES: BEGIN OF lt_class_info,
             clsname  TYPE seoclassdf-clsname,
             version  TYPE seoclassdf-version,
             category TYPE seoclassdf-category,
           END OF lt_class_info.

    DATA: l_class_info TYPE HASHED TABLE OF lt_class_info WITH UNIQUE KEY clsname version.

    FIELD-SYMBOLS: <l_class>      LIKE LINE OF classes,
                   <l_class_info> LIKE LINE OF l_class_info.

* Find classes matching the search criteria
    IF package_range IS INITIAL.
      SELECT clsname
        FROM seoclass
        INTO TABLE classes
       WHERE clsname IN class_range                     "#EC CI_GENBUFF
         AND clstype = '0'.                               "#EC CI_SUBRC
    ELSE.
      SELECT clsname
        FROM seoclass INNER JOIN tadir
                              ON tadir~pgmid    = 'R3TR' AND
                                 tadir~object   = 'CLAS' AND
                                 tadir~obj_name = seoclass~clsname
        INTO TABLE classes
       WHERE devclass IN package_range
         AND clsname  IN class_range                   "#EC CI_BUFFJOIN
         AND clstype  =  '0'.                             "#EC CI_SUBRC
    ENDIF.

    IF classes IS NOT INITIAL AND class_category_range IS NOT INITIAL.
      SELECT clsname version category
        FROM seoclassdf
        INTO TABLE l_class_info
         FOR ALL ENTRIES IN classes
       WHERE clsname = classes-table_line.                "#EC CI_SUBRC

      LOOP AT classes ASSIGNING <l_class>.
        READ TABLE l_class_info
              WITH TABLE KEY clsname = <l_class>
                             version = '1' ASSIGNING <l_class_info>. "Try to use active version
        IF sy-subrc <> 0.
          READ TABLE l_class_info
                WITH TABLE KEY clsname = <l_class>
                               version = '0' ASSIGNING <l_class_info>. "Use inactive if there is no active version
        ENDIF.

        IF sy-subrc <> 0 OR <l_class_info>-category NOT  IN class_category_range.
          DELETE classes.
        ENDIF.
      ENDLOOP.
    ENDIF.

* Sort result
    SORT classes.
  ENDMETHOD.


  METHOD find_interfaces.

* Find classes matching the search criteria
    IF package_range IS INITIAL.
      SELECT clsname
        FROM seoclass
        INTO TABLE interfaces
       WHERE clsname IN interface_range                 "#EC CI_GENBUFF
         AND clstype = '1'.                               "#EC CI_SUBRC
    ELSE.
      SELECT clsname
        FROM seoclass INNER JOIN tadir
                              ON tadir~pgmid    = 'R3TR' AND
                                 tadir~object   = 'INTF' AND
                                 tadir~obj_name = seoclass~clsname
        INTO TABLE interfaces
       WHERE devclass IN package_range
         AND clsname  IN interface_range               "#EC CI_BUFFJOIN
         AND clstype  =  '1'.                             "#EC CI_SUBRC
    ENDIF.

* Sort result
    SORT interfaces.
  ENDMETHOD.


  METHOD find_languages.

* Find known languages matching selection criteria
    SELECT spras
      FROM t002
      INTO TABLE languages
     WHERE spras IN language_range.                       "#EC CI_SUBRC

* Sort result
    SORT languages.
  ENDMETHOD.


  METHOD find_packages.
    SELECT devclass
      FROM tdevc
      INTO TABLE packages                               "#EC CI_GENBUFF
     WHERE devclass IN package_range.                     "#EC CI_SUBRC

    SORT packages.
  ENDMETHOD.


  METHOD find_programs.
    DATA l_wa_e071 TYPE e071.
    DATA l_tadir   TYPE tadir.

    FIELD-SYMBOLS <l_program> LIKE LINE OF programs.

* Build list of matching program names
    SELECT name
      FROM trdir
      INTO TABLE programs
     WHERE name IN program_range.                         "#EC CI_SUBRC

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

        DELETE programs.                                  "#EC CI_SUBRC
      ENDLOOP.
    ENDIF.

* Sort result
    SORT programs.
  ENDMETHOD.


  METHOD find_tadir_objects.

* Select TADIR objects
    SELECT pgmid object obj_name
      FROM tadir
      INTO TABLE objects                                  "#EC CI_SUBRC
     WHERE devclass IN package_range.                   "#EC CI_GENBUFF

* Sort result
    SORT objects.
  ENDMETHOD.


  METHOD get_program.
    DATA: l_namespace      TYPE namespace,
          l_function_group TYPE rs38l_area,

          l_class          TYPE seocpdkey-clsname,
          l_method         TYPE seocpdkey-cpdname,

          l_function_pool  TYPE pname,
          l_include_no     TYPE includenr,

          l_offset         TYPE i,

          l_incl_naming    TYPE REF TO if_oo_class_incl_naming,
          l_cifref         TYPE REF TO if_oo_clif_incl_naming.

    CASE pgmid.

      WHEN 'R3TR'.
        CASE object.

          WHEN 'CLAS'.
            l_class = obj_name(30).

            cl_oo_include_naming=>get_instance_by_name( EXPORTING  name   = l_class
                                                        RECEIVING  cifref = l_cifref
                                                        EXCEPTIONS OTHERS = 1 ).
            IF sy-subrc = 0 AND l_cifref IS NOT INITIAL.
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
            l_class = obj_name(30).

            cl_oo_include_naming=>get_instance_by_name( EXPORTING  name   = l_class
                                                        RECEIVING  cifref = l_cifref
                                                        EXCEPTIONS OTHERS = 1 ).
            IF sy-subrc = 0 AND l_cifref IS NOT INITIAL.
              l_incl_naming ?= l_cifref.
              program        = l_incl_naming->get_include_by_section( object ).
            ENDIF.

          WHEN 'FUNC'.
            SELECT SINGLE pname include
                     FROM tfdir
                     INTO (l_function_pool,
                           l_include_no)
                    WHERE funcname = obj_name.
            IF sy-subrc = 0.
              WHILE l_function_pool+l_offset CA '/'.
                l_offset = l_offset + sy-fdpos + 1.
              ENDWHILE.

              IF l_function_pool+l_offset CP 'SAPL*'.
                SHIFT l_function_pool+l_offset LEFT BY 3 PLACES.

                CONCATENATE l_function_pool 'U' l_include_no INTO program.
              ENDIF.
            ENDIF.

          WHEN 'METH'.
            l_class = obj_name(30).

            cl_oo_include_naming=>get_instance_by_name( EXPORTING  name   = l_class
                                                        RECEIVING  cifref = l_cifref
                                                        EXCEPTIONS OTHERS = 1 ).
            IF sy-subrc = 0 AND l_cifref IS NOT INITIAL.
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
