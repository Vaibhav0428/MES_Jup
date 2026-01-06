CLASS zcl_mvttransferposting DEFINITION
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



CLASS ZCL_MVTTRANSFERPOSTING IMPLEMENTATION.


  METHOD if_apj_dt_exec_object~get_parameters.
    " Return the supported selection parameters here
    et_parameter_def = VALUE #(
      ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter     datatype = 'C' length = 80 param_text = 'Production Order'   lowercase_ind = abap_true changeable_ind = abap_true )
    ).

    " Return the default parameters values here
    et_parameter_val = VALUE #(
      ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter     sign = 'I' option = 'EQ' low = '' )
    ).

  ENDMETHOD.


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


  METHOD if_oo_adt_classrun~main .
    runjob( 'ABCD' ).
  ENDMETHOD.


  METHOD runjob.

    DATA: error   TYPE string,
          success TYPE abap_boolean.

    DATA orderno TYPE c LENGTH 12.
    orderno = paramcmno.


    IF orderno IS INITIAL.
      SELECT FROM zr_texturization AS a
      INNER JOIN zr_texturization AS b ON a~productionorder = b~productionorder AND a~orderdate = b~orderdate AND a~transid = b~transid
      FIELDS a~productionorder, a~orderdate, a~transid,a~Refdocno
         WHERE a~posted = '' AND a~type IN ( '311' )
             AND b~type = 'Output' AND b~posted = 'X'
     GROUP BY a~productionorder, a~orderdate, a~transid,a~Refdocno
     INTO TABLE @DATA(lt_header).
    ELSE.
      SELECT FROM zr_texturization AS a
      INNER JOIN zr_texturization AS b ON a~productionorder = b~productionorder AND a~orderdate = b~orderdate AND a~transid = b~transid
       FIELDS a~productionorder, a~orderdate, a~transid,a~Refdocno
          WHERE a~posted = '' AND a~type IN ( '311' )
              AND a~productionorder = @orderno
              AND b~type = 'Output' AND b~posted = 'X'
          GROUP BY a~productionorder, a~orderdate, a~transid,a~Refdocno
          INTO TABLE @lt_header.
    ENDIF.

    LOOP AT lt_header INTO DATA(ls_header).

      SELECT FROM zr_texturization
      FIELDS batch, material, quantity,storagelocation, tolocation, uom
      WHERE productionorder = @ls_header-productionorder
            AND orderdate = @ls_header-orderdate
            AND transid = @ls_header-transid
            AND posted = ''
            AND type = '311'
      INTO TABLE @DATA(grn_lines).

      DATA(is_valid) = abap_true.


      LOOP AT grn_lines INTO DATA(ls_line).
        IF ls_line-material IS INITIAL.
          error = 'Material is missing in line item'.
        ELSEIF ls_line-quantity IS INITIAL.
          error = 'Quantity is missing in line item'.
        ELSEIF ls_line-storagelocation IS INITIAL.
          error = 'Storage Location is missing in line item'.
        ELSEIF ls_line-tolocation IS INITIAL.
          error = 'To Location is missing in line item'.
        ELSEIF ls_line-uom IS INITIAL.
          error = 'UOM is missing in line item'.
        ENDIF.

        SELECT SINGLE FROM i_product AS material
         FIELDS material~product, material~isbatchmanagementrequired
         WHERE material~product = @ls_line-material
         INTO @DATA(res1).

        IF res1-isbatchmanagementrequired  = 'X' AND ls_line-batch IS INITIAL.
          error = |Error: Batch is mandatory for material { ls_line-material }|.
        ENDIF.

        IF error IS NOT INITIAL.
          UPDATE ztexturization
          SET errorlog = @error
          WHERE productionorder = @ls_header-productionorder
                  AND orderdate = @ls_header-orderdate
                  AND transid = @ls_header-transid
                  AND type = '311'.

          is_valid = abap_false.
          CLEAR error.
        ENDIF.
      ENDLOOP.

      IF is_valid = abap_false.
        CONTINUE.
      ENDIF.

      SELECT SINGLE FROM i_productionorder
      FIELDS productionplant
      WHERE productionorder = @ls_header-productionorder
      INTO @DATA(plant).


*      DATA(posting_date) = ls_header-orderdate.
*      REPLACE ALL OCCURRENCES OF '-' IN posting_date WITH ''.
      data(posting_date) = ls_header-Refdocno+0(8).

      IF posting_date < '20250901'.
        posting_date = '20250901'. "
      ENDIF.

      DATA(refno) = ls_header-productionorder && posting_date && plant.
      DATA(migocid) = getcid(  ).

      MODIFY ENTITIES OF i_materialdocumenttp
      ENTITY materialdocument
      CREATE FROM VALUE #( (
          %cid                          =  migocid
          postingdate                   =  posting_date
          documentdate                  =  posting_date
          goodsmovementcode             =  '04'
          materialdocumentheadertext    =  refno
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
              %target  =  VALUE #( FOR po_line IN grn_lines INDEX INTO i (
                            %cid =  |{ migocid }{ i WIDTH = 3 ALIGN = RIGHT PAD = '0' }|
                            plant = plant
                            material = po_line-material
                            goodsmovementtype = '311'
                            batch = po_line-batch
                            storagelocation = po_line-storagelocation
                            issuingorreceivingplant = plant
                            issgorrcvgbatch = po_line-batch
                            issuingorreceivingstorageloc = po_line-tolocation
                            quantityinentryunit = po_line-quantity
                            entryunit = po_line-uom
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

        UPDATE ztexturization
        SET errorlog = @error
        WHERE productionorder = @ls_header-productionorder
              AND orderdate = @ls_header-orderdate
              AND transid = @ls_header-transid
              AND type = '311'.

*            IF lines( ls_save_reported-purchaseorder ) > 0.
*              lv_error = |{ Sy-msgid } { sy-msgno }|.
*            ENDIF.

      ELSE.

        SELECT SINGLE FROM i_materialdocumentheader_2
          FIELDS  materialdocument
          WHERE materialdocumentheadertext = @refno
          INTO @DATA(migo_no).
        IF migo_no IS NOT INITIAL.
          UPDATE ztexturization
          SET posted = 'X',
              document = @migo_no,
              errorlog = ''
          WHERE productionorder = @ls_header-productionorder
                AND orderdate = @ls_header-orderdate
                AND transid = @ls_header-transid
                AND type = '311'.
        ENDIF.

*        IF lines( ls_create_mappedi2-materialdocument ) > 0.
*          LOOP AT ls_create_mappedi2-materialdocument ASSIGNING FIELD-SYMBOL(<fs_migo>).
*            CONVERT KEY OF i_materialdocumenttp FROM <fs_migo>-%pid TO <fs_migo>-%key.
*            DATA(migo_no) = <fs_migo>-%key-materialdocument.
*          ENDLOOP.
*        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD getcid.
    TRY.
        cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
      CATCH cx_uuid_error.
        ASSERT 1 = 0.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
