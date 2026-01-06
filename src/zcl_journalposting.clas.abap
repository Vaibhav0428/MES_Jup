CLASS zcl_journalposting DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_apj_dt_exec_object .
  INTERFACES if_apj_rt_exec_object .

    INTERFACES if_oo_adt_classrun.
    CLASS-METHODS getCID RETURNING VALUE(cid) TYPE abp_behv_cid.

    CLASS-METHODS runjob
      IMPORTING paramcmno TYPE c.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_JOURNALPOSTING IMPLEMENTATION.


METHOD if_apj_rt_exec_object~execute.
  DATA p_descr TYPE c LENGTH 80.

  " Getting the actual parameter values
  LOOP AT it_parameters INTO DATA(ls_parameter).
    CASE ls_parameter-selname.
      WHEN 'P_DESCR'. p_descr = ls_parameter-low.
    ENDCASE.
  ENDLOOP.
  runjob( p_descr ).
ENDMETHOD.


METHOD if_oo_adt_classrun~main.
 runJob( 'ABCD' ).
ENDMETHOD.


METHOD runjob.

  DATA: error   TYPE string,
        success TYPE abap_boolean.

  SELECT FROM zr_transferjournal
    FIELDS headerid
    WHERE posted IS INITIAL
         AND errorlog IS INITIAL
    GROUP BY headerid, lastchangedat
    ORDER BY lastchangedat ASCENDING
    INTO TABLE @DATA(lt_header).


  LOOP AT lt_header INTO DATA(ls_header).
    SELECT * FROM zr_transferjournal
    WHERE headerid = @ls_header-headerid
          AND posted IS INITIAL
    ORDER BY sno ASCENDING
    INTO TABLE @DATA(lt_docitems) .

    DATA(is_valid) = abap_true.

    LOOP AT lt_docitems INTO DATA(ls_docitems).
      IF ls_docitems-material IS INITIAL.
        error = 'Material is missing in line item'.
      ELSEIF ls_docitems-plant IS INITIAL.
        error = 'Plant is missing in line item'.
      ELSEIF ls_docitems-fromlocation IS INITIAL.
        error = 'From Location is missing in line item'.
      ELSEIF ls_docitems-tolocation IS INITIAL.
        error = 'To Location is missing in line item'.
      ELSEIF ls_docitems-quantity IS INITIAL.
        error = 'Quantity is missing in line item'.
      ELSEIF ls_docitems-entryunit IS INITIAL.
        error = 'Entry Unit in missing in line item' .
      ENDIF.

      SELECT SINGLE FROM i_product AS material
         FIELDS material~product, material~isbatchmanagementrequired
         WHERE material~product = @ls_docitems-material
         INTO @DATA(res1).

      IF res1-isbatchmanagementrequired  = 'X' AND ( ls_docitems-frombatch IS INITIAL OR ls_docitems-tobatch IS INITIAL ).
        error = |Error: Batch is mandatory for material { ls_docitems-material }|.
      ENDIF.

      IF error IS NOT INITIAL.
        UPDATE ztransferjournal
        SET errorlog = @error
        WHERE header_id = @ls_docitems-headerid
              AND material = @ls_docitems-material .

        is_valid = abap_false.
        CLEAR error.
      ENDIF.
    ENDLOOP.

    IF is_valid = abap_false.
      CONTINUE.
    ENDIF.

    DATA(migocid) = getcid(  ).

    SPLIT ls_header-headerid AT '/' INTO DATA(part1) DATA(part2) DATA(part3) DATA(part4).

    MODIFY ENTITIES OF i_materialdocumenttp
    ENTITY materialdocument
    CREATE FROM VALUE #( (
        %cid                          =  migocid
        postingdate                   =  cl_abap_context_info=>get_system_date( )
        documentdate                  =  cl_abap_context_info=>get_system_date( )
        goodsmovementcode             =  '04'
        materialdocumentheadertext    =  part4
        %control = VALUE #(
            postingdate                         = cl_abap_behv=>flag_changed
            documentdate                        = cl_abap_behv=>flag_changed
            referencedocument                   = cl_abap_behv=>flag_changed
            goodsmovementcode                   = cl_abap_behv=>flag_changed
            materialdocumentheadertext          = cl_abap_behv=>flag_changed
            )
        ) )
        CREATE BY \_materialdocumentitem
        FROM VALUE #( (
            %cid_ref = migocid
            %target  =  VALUE #( FOR ls_docitem IN lt_docitems INDEX INTO i (
                          %cid =  |{ migocid }{ i WIDTH = 3 ALIGN = RIGHT PAD = '0' }|
                          plant = ls_docitem-plant
                          material = ls_docitem-material
                          goodsmovementtype = '311'
                          batch = ls_docitem-frombatch
                          storagelocation = ls_docitem-fromlocation
                          issuingorreceivingplant = ls_docitem-plant
                          issgorrcvgbatch = ls_docitem-tobatch
                          issuingorreceivingstorageloc = ls_docitem-tolocation
                          quantityinentryunit = ls_docitem-quantity
                          entryunit = ls_docitem-postinguom
                          %control = VALUE #(
                              plant                              = cl_abap_behv=>flag_changed
                              material                           = cl_abap_behv=>flag_changed
                              goodsmovementtype                  = cl_abap_behv=>flag_changed
                              storagelocation                    = cl_abap_behv=>flag_changed
                              issuingorreceivingplant            = cl_abap_behv=>flag_changed
                              issgorrcvgbatch                    = cl_abap_behv=>flag_changed
                              issuingorreceivingstorageloc       = cl_abap_behv=>flag_changed
                              batch                              = cl_abap_behv=>flag_changed
                              quantityinentryunit                = cl_abap_behv=>flag_changed
                              entryunit                          = cl_abap_behv=>flag_changed
                          )
                      ) )
        ) )
    MAPPED   DATA(ls_create_mappedi2)
    FAILED   DATA(ls_create_failedi2)
    REPORTED DATA(ls_create_reportedi2).

    COMMIT ENTITIES BEGIN
    RESPONSE OF i_materialdocumenttp
    FAILED DATA(commit_failedi2)
    REPORTED DATA(commit_reportedi2).
    ...
    COMMIT ENTITIES END.

    IF ls_create_failedi2 IS NOT INITIAL OR commit_failedi2 IS NOT INITIAL.

      CLEAR error.

      LOOP AT ls_create_reportedi2-materialdocument ASSIGNING FIELD-SYMBOL(<fs_error>).
        error = | { <fs_error>-%msg->if_message~get_text( ) } |.
      ENDLOOP.

      LOOP AT ls_create_reportedi2-materialdocumentitem ASSIGNING FIELD-SYMBOL(<fs_error2>).
        error = | { <fs_error2>-%msg->if_message~get_text( ) } |.
      ENDLOOP.

      LOOP AT commit_reportedi2-materialdocument ASSIGNING FIELD-SYMBOL(<fs_error3>).
        error = | { <fs_error3>-%msg->if_message~get_text( ) } |.
      ENDLOOP.

      LOOP AT commit_reportedi2-materialdocumentitem ASSIGNING FIELD-SYMBOL(<fs_error4>).
        error = | { <fs_error4>-%msg->if_message~get_text( ) } |.
      ENDLOOP.

      DATA(current_timestamp) = cl_abap_context_info=>get_system_date(  ) && cl_abap_context_info=>get_system_time(  ).

      UPDATE ztransferjournal
      SET errorlog = @error,
          last_changed_at = @current_timestamp
      WHERE header_id = @ls_header-headerid.

      EXIT.

    ELSE.

      SELECT SINGLE FROM i_materialdocumentheader_2
        FIELDS  materialdocument,materialdocumentyear
        WHERE materialdocumentheadertext = @part4
        INTO @DATA(migo_no).
      IF migo_no IS NOT INITIAL.
        UPDATE ztransferjournal
        SET posted = 'X',
            document_number = @migo_no-materialdocument,
            document_year = @migo_no-materialdocumentyear,
            errorlog = ''
        WHERE header_id = @ls_docitems-headerid.

      ENDIF.
    ENDIF.

    CLEAR: error, success.
  ENDLOOP.


ENDMETHOD.


METHOD if_apj_dt_exec_object~get_parameters.
    et_parameter_def = VALUE #(
      ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter     datatype = 'C' length = 80 param_text = 'Header Id'   lowercase_ind = abap_true changeable_ind = abap_true )
    ).

    " Return the default parameters values here
    et_parameter_val = VALUE #(
      ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter     sign = 'I' option = 'EQ' low = '' )

    ).

ENDMETHOD.


  METHOD getCID.
        TRY.
            cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
          CATCH cx_uuid_error.
            ASSERT 1 = 0.
        ENDTRY.
      ENDMETHOD.
ENDCLASS.
