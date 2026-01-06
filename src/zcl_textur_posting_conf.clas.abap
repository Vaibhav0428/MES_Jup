CLASS zcl_textur_posting_conf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_apj_dt_exec_object .
    INTERFACES if_apj_rt_exec_object .


    INTERFACES if_oo_adt_classrun.

    CLASS-METHODS runjob
      IMPORTING paramcmno TYPE c.
    CLASS-METHODS update_document.

     CLASS-METHODS sendActivity
      IMPORTING
        quantity TYPE p
        over_qty TYPE p
        activity TYPE lstar
        consumption TYPE P
      RETURNING
        VALUE(qty) TYPE menge_d.

     CLASS-METHODS findUnit_E
      IMPORTING
        unit TYPE erfme
      RETURNING
        VALUE(e_unit) TYPE erfme.


    CLASS-METHODS validate
      IMPORTING
        VALUE(productionorder) TYPE aufnr
        VALUE(orderdate)       TYPE c
        VALUE(transid)         TYPE c
        VALUE(plant)           TYPE werks_d
      RETURNING
        VALUE(isvalidated)     TYPE abap_boolean.

PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TEXTUR_POSTING_CONF IMPLEMENTATION.


  METHOD if_apj_dt_exec_object~get_parameters.
    " Return the supported selection parameters here
    et_parameter_def = VALUE #(
      ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter     datatype = 'C' length = 80 param_text = 'Production Confirmation of Texturization'   lowercase_ind = abap_true changeable_ind = abap_true )
    ).

    " Return the default parameters values here
    et_parameter_val = VALUE #(
      ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter     sign = 'I' option = 'EQ' low = 'Production Confirmation of Texturization' )
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
    runJob( p_descr ).
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main .
    runJob( 'ABCD' ).
  ENDMETHOD.


  METHOD runjob.

    DATA: error      TYPE string,
          success    TYPE abap_boolean,
          prod_order TYPE aufnr.

    update_document( ).

    SELECT FROM zr_texturization
     FIELDS productionorder, orderdate,transid,refdocno
*      WHERE posted = '' AND Type in ( 'Output', '261', '531' )
      WHERE posted = '' AND type IN ( 'Output' )
      ORDER BY createdat ASCENDING
      INTO TABLE @DATA(lt_header).

    DATA(count) = 0.
    LOOP AT lt_header INTO DATA(ls_header).

      IF prod_order IS INITIAL OR prod_order NE ls_header-productionorder.
        prod_order = ls_header-productionorder.
      ELSE.
        CONTINUE.
      ENDIF.


      IF count = 1.
        EXIT.
      ENDIF.

      success = abap_true.

      SELECT SINGLE FROM i_manufacturingorder AS a
           INNER JOIN i_manufacturingorderoperation AS c ON a~mfgorderinternalid = c~mfgorderinternalid
           INNER JOIN i_workcenter AS d ON c~workcentertypecode_2 = d~workcentertypecode
                                      AND c~workcenterinternalid = d~workcenterinternalid
           FIELDS c~operationconfirmation,c~manufacturingorderoperation_2 AS manufacturingorderoperation, a~planningplant, d~workcenter
           WHERE a~manufacturingorder = @ls_header-productionorder
           INTO @DATA(confirm_group).

      DATA(isvalid) = validate(
                productionorder = ls_header-productionorder
                orderdate = ls_header-orderdate
                transid = ls_header-transid
                plant = confirm_group-planningplant
         ).
      IF isvalid = abap_false.
        success = abap_false.
        CONTINUE.
      ENDIF.

      DATA lt_confirmation TYPE TABLE FOR CREATE i_productionordconfirmationtp.
      DATA lt_matldocitm TYPE TABLE FOR CREATE i_productionordconfirmationtp\_prodnordconfmatldocitm.
      FIELD-SYMBOLS <ls_matldocitm> LIKE LINE OF lt_matldocitm.
      DATA lt_target LIKE <ls_matldocitm>-%target.

      " read proposals and corresponding times for given quantity
      READ ENTITIES OF i_productionordconfirmationtp
       ENTITY productionorderconfirmation
       EXECUTE getconfproposal
       FROM VALUE #( (
              confirmationgroup = confirm_group-operationconfirmation
              %param-confirmationyieldquantity = 0
        ) )
       RESULT DATA(lt_confproposal)
       REPORTED DATA(lt_reported_conf).

      SELECT SINGLE FROM zr_texturization
      FIELDS SUM( quantity ) AS quantity
      WHERE type = 'Output' AND productionorder = @ls_header-productionorder
            AND orderdate = @ls_header-orderdate AND transid = @ls_header-transid
      INTO @DATA(consumption_quantity).

      LOOP AT lt_confproposal ASSIGNING FIELD-SYMBOL(<ls_confproposal>).


*        DATA(posting_date) = ls_header-orderdate.
*        REPLACE ALL OCCURRENCES OF '-' IN posting_date WITH ''.
        DATA(posting_date) = ls_header-refdocno+0(8).

        IF posting_date < '20250901'.
          posting_date = '20250901'. "
        ENDIF.

*        get oncly 10 char from ref doc no

        APPEND INITIAL LINE TO lt_confirmation ASSIGNING FIELD-SYMBOL(<ls_confirmation>).
        <ls_confirmation>-%cid = 'Conf' && sy-tabix..
        <ls_confirmation>-%data = CORRESPONDING #( <ls_confproposal>-%param ).
        <ls_confirmation>-postingdate = posting_date.
        <ls_confirmation>-confirmationreworkquantity = 0.
        <ls_confirmation>-confirmationtext = ls_header-transid.
        <ls_confirmation>-confirmationyieldquantity = consumption_quantity.

*        Shift is blank for now
*        <ls_confirmation>-%data-shiftdefinition = '1' .
*        <ls_confirmation>-%data-shiftgrouping = '01' .


*       Activity Calculation
        SELECT SINGLE FROM i_productionorderoperationtp
        FIELDS operationreferencequantity,workcenterstandardworkqtyunit1,workcenterstandardworkqty1,costctractivitytype1,
                 workcenterstandardworkqtyunit2,workcenterstandardworkqty2,costctractivitytype2,
                workcenterstandardworkqtyunit3,workcenterstandardworkqty3,costctractivitytype3,
                workcenterstandardworkqtyunit4,workcenterstandardworkqty4,costctractivitytype4,
                workcenterstandardworkqtyunit5,workcenterstandardworkqty5,costctractivitytype5,
                workcenterstandardworkqtyunit6,workcenterstandardworkqty6,costctractivitytype6
           WHERE productionorder = @ls_header-productionorder
             AND productionorderoperation = @confirm_group-manufacturingorderoperation
             AND plant = @confirm_group-planningplant
             AND workcenter = @confirm_group-workcenter
             INTO @DATA(bill_of_operations_op_basic).

        <ls_confirmation>-%data-opconfirmedworkquantity1 = sendactivity(
                                                   consumption = consumption_quantity
                                                   quantity = bill_of_operations_op_basic-workcenterstandardworkqty1
                                                   activity = bill_of_operations_op_basic-costctractivitytype1
                                                   over_qty = bill_of_operations_op_basic-operationreferencequantity ).

        IF <ls_confirmation>-%data-opconfirmedworkquantity1 IS NOT INITIAL.
          <ls_confirmation>-%data-opworkquantityunit1 = findunit_e( unit = bill_of_operations_op_basic-workcenterstandardworkqtyunit1 ).
        ENDIF.

        <ls_confirmation>-%data-opconfirmedworkquantity2 = sendactivity(
                                                   consumption = consumption_quantity
                                                   quantity = bill_of_operations_op_basic-workcenterstandardworkqty2
                                                   activity = bill_of_operations_op_basic-costctractivitytype2
                                                   over_qty = bill_of_operations_op_basic-operationreferencequantity ).

        IF <ls_confirmation>-%data-opconfirmedworkquantity2 IS NOT INITIAL.
          <ls_confirmation>-%data-opworkquantityunit2 = findunit_e( unit = bill_of_operations_op_basic-workcenterstandardworkqtyunit2 ).
        ENDIF.

        <ls_confirmation>-%data-opconfirmedworkquantity3 = sendactivity(
                                               consumption = consumption_quantity
                                               quantity = bill_of_operations_op_basic-workcenterstandardworkqty3
                                               activity = bill_of_operations_op_basic-costctractivitytype3
                                               over_qty = bill_of_operations_op_basic-operationreferencequantity ).

        IF <ls_confirmation>-%data-opconfirmedworkquantity3 IS NOT INITIAL.
          <ls_confirmation>-%data-opworkquantityunit3 = findunit_e( unit = bill_of_operations_op_basic-workcenterstandardworkqtyunit3 ).
        ENDIF.

        <ls_confirmation>-%data-opconfirmedworkquantity4 = sendactivity(
                                                consumption = consumption_quantity
                                                quantity = bill_of_operations_op_basic-workcenterstandardworkqty4
                                                activity = bill_of_operations_op_basic-costctractivitytype4
                                                over_qty = bill_of_operations_op_basic-operationreferencequantity ).

        IF <ls_confirmation>-%data-opconfirmedworkquantity4 IS NOT INITIAL.
          <ls_confirmation>-%data-opworkquantityunit4 = findunit_e( unit = bill_of_operations_op_basic-workcenterstandardworkqtyunit4 ).
        ENDIF.

        <ls_confirmation>-%data-opconfirmedworkquantity5 = sendactivity(
                                                consumption = consumption_quantity
                                                quantity = bill_of_operations_op_basic-workcenterstandardworkqty5
                                                activity = bill_of_operations_op_basic-costctractivitytype5
                                                over_qty = bill_of_operations_op_basic-operationreferencequantity ).

        IF <ls_confirmation>-%data-opconfirmedworkquantity5 IS NOT INITIAL.
          <ls_confirmation>-%data-opworkquantityunit5 = findunit_e( unit = bill_of_operations_op_basic-workcenterstandardworkqtyunit5 ).
        ENDIF.

        <ls_confirmation>-%data-opconfirmedworkquantity6 = sendactivity(
                                                consumption = consumption_quantity
                                                quantity = bill_of_operations_op_basic-workcenterstandardworkqty6
                                                activity = bill_of_operations_op_basic-costctractivitytype6
                                                over_qty = bill_of_operations_op_basic-operationreferencequantity ).

        IF <ls_confirmation>-%data-opconfirmedworkquantity6 IS NOT INITIAL.
          <ls_confirmation>-%data-opworkquantityunit6 = findunit_e( unit = bill_of_operations_op_basic-workcenterstandardworkqtyunit6 ).
        ENDIF.




        " read proposals for corresponding goods movements for proposed quantity
        READ ENTITIES OF i_productionordconfirmationtp
          ENTITY productionorderconfirmation
          EXECUTE getgdsmvtproposal
          FROM VALUE #( ( confirmationgroup               = <ls_confproposal>-confirmationgroup
                         %param-confirmationyieldquantity = <ls_confproposal>-%param-confirmationyieldquantity
                          ) )
          RESULT DATA(lt_gdsmvtproposal)
          REPORTED DATA(lt_reported_gdsmvt).

        CHECK lt_gdsmvtproposal[] IS NOT INITIAL.

        CLEAR lt_target[].
        LOOP AT lt_gdsmvtproposal ASSIGNING FIELD-SYMBOL(<ls_gdsmvtproposal>)
            WHERE confirmationgroup = <ls_confproposal>-confirmationgroup AND %param-goodsmovementtype = '101'.

          SELECT FROM zr_texturization
            FIELDS material, batch, uom, quantity, storagelocation, type
            WHERE productionorder = @ls_header-productionorder
                  AND orderdate = @ls_header-orderdate
                  AND type IN ( 'Output', '261', '531' )
                  AND transid = @ls_header-transid
            INTO TABLE @DATA(lt_textconsump_lines).

          LOOP AT lt_textconsump_lines INTO DATA(filled_details_goodsmovement) WHERE quantity > 0.
            APPEND INITIAL LINE TO lt_target ASSIGNING FIELD-SYMBOL(<ls_target>).
            <ls_target> = CORRESPONDING #( <ls_gdsmvtproposal>-%param ).
            <ls_target>-%cid = 'Item' && sy-tabix.
            <ls_target>-material = filled_details_goodsmovement-material.
            <ls_target>-batch = filled_details_goodsmovement-batch.
            <ls_target>-quantityinentryunit = filled_details_goodsmovement-quantity.
            <ls_target>-storagelocation = filled_details_goodsmovement-storagelocation.
            <ls_target>-entryunit = filled_details_goodsmovement-uom.

            IF filled_details_goodsmovement-type = 'Output'.
              <ls_target>-goodsmovementtype = '101'.
            ELSE.
              <ls_target>-goodsmovementtype = filled_details_goodsmovement-type.
            ENDIF.

*        Mandatory for confirmation
            IF <ls_target>-goodsmovementtype = '101' OR
               <ls_target>-goodsmovementtype = '102'.
              <ls_target>-orderitem = '1'.
            ELSEIF <ls_target>-goodsmovementtype = '261' OR
                   <ls_target>-goodsmovementtype = '262' OR
                   <ls_target>-goodsmovementtype = '531' OR
                   <ls_target>-goodsmovementtype = '532'.
              <ls_target>-goodsmovementrefdoctype = ''.
              <ls_target>-inventoryusabilitycode = ''.
              <ls_target>-inventoryspecialstocktype = ''.
              <ls_target>-orderitem = ''.
            ENDIF.


          ENDLOOP.
        ENDLOOP.


        APPEND VALUE #( %cid_ref = <ls_confirmation>-%cid
          %target = lt_target
          confirmationgroup = <ls_confproposal>-confirmationgroup ) TO lt_matldocitm.
      ENDLOOP.

      MODIFY ENTITIES OF i_productionordconfirmationtp
       ENTITY productionorderconfirmation
       CREATE FROM lt_confirmation
       CREATE BY \_prodnordconfmatldocitm FROM lt_matldocitm
       MAPPED DATA(lt_mapped)
       FAILED DATA(lt_failed)
       REPORTED DATA(lt_reported).

      COMMIT ENTITIES.

      IF sy-msgty = 'E' OR ( sy-msgty = 'I' AND sy-msgid = 'RU' AND sy-msgno = '505' ).
        error = |Error during confirmation: { sy-msgid } { sy-msgno } { sy-msgv1 } { sy-msgv2 } { sy-msgv3 } { sy-msgv4 }|.
        success = abap_false.
      ENDIF.

      IF success = abap_true.
        SELECT FROM i_mfgorderconfirmation
          FIELDS mfgorderconfirmation
          WHERE manufacturingorder = @ls_header-productionorder
          AND manufacturingorderoperation_2 = @confirm_group-manufacturingorderoperation
          AND isreversed = '' AND isreversal = ''
          ORDER BY mfgorderconfirmation DESCENDING
          INTO TABLE @DATA(mfg_order_confirmation).

        IF mfg_order_confirmation IS INITIAL.
          error = |Error: No confirmation found for manufacturing order { ls_header-productionorder } and operation { confirm_group-manufacturingorderoperation }| .
          success = abap_false.
        ELSE.
          error = |Confirmation successful for manufacturing order { ls_header-productionorder } and operation { confirm_group-manufacturingorderoperation } with confirmation number { mfg_order_confirmation[ 1 ]-mfgorderconfirmation }|.
          success = abap_true.


          DATA(confirmation) = mfg_order_confirmation[ 1 ]-mfgorderconfirmation.

*           updating posted boolean and confirmation number in custom table
          UPDATE ztexturization SET posted = 'X', errorlog = @confirmation
            WHERE productionorder = @ls_header-productionorder AND orderdate = @ls_header-orderdate
                  AND type IN ( 'Output', '261', '531' ) AND transid = @ls_header-transid.

        ENDIF.
      ENDIF.


      IF success = abap_false.
        UPDATE ztexturization SET posted = '',errorlog = @error
            WHERE productionorder = @ls_header-productionorder
                  AND orderdate = @ls_header-orderdate
                  AND type IN ( 'Output', '261', '531' )
                  AND transid = @ls_header-transid.
      ENDIF.

      count = count + 1.

    ENDLOOP.

  ENDMETHOD.


  METHOD findUnit_E.

   SELECT SINGLE FROM I_UnitOfMeasure
          FIELDS UnitOfMeasure_E
          WHERE UnitOfMeasure = @unit
          INTO @e_unit.

  ENDMETHOD.


  METHOD update_document.


    SELECT FROM i_mfgorderconfmatldocitem AS a
    LEFT JOIN zr_texturization AS b ON a~material = b~material AND a~manufacturingorder = b~productionorder AND a~confirmationtext = b~transid
    FIELDS a~materialdocument,b~productionorder,b~orderdate,b~type,b~shift, b~sno, b~material, b~transid, a~mfgorderconfirmation
    WHERE  b~document = ''
          AND a~isreversed = '' AND a~isreversal = ''
          AND b~type IN ( 'Output', '261', '531' )
    INTO TABLE @DATA(lt_matdoc).

    LOOP AT lt_matdoc INTO DATA(ls_matdoc).
      UPDATE ztexturization
          SET posted = 'X',
              document = @ls_matdoc-materialdocument,
              errorlog = @ls_matdoc-mfgorderconfirmation
          WHERE productionorder = @ls_matdoc-productionorder
              AND orderdate = @ls_matdoc-orderdate
              AND type = @ls_matdoc-type
              AND shift = @ls_matdoc-shift
              AND material = @ls_matdoc-material
              AND transid = @ls_matdoc-transid
              AND sno = @ls_matdoc-sno.
    clear : ls_matdoc.
    ENDLOOP.


*   saving mat doc for this confirmation
*    SELECT FROM i_mfgorderconfmatldocitem AS a
*    LEFT JOIN zr_texturization AS b ON a~material = b~material AND a~manufacturingorder = b~productionorder AND a~mfgorderconfirmation = b~errorlog
*    FIELDS a~materialdocument,b~productionorder,b~orderdate,b~type,b~shift, b~sno, b~material, b~transid
*    WHERE b~posted = 'X' AND b~document = ''
*          AND a~isreversed = '' AND a~isreversal = ''
*          AND b~type IN ( 'Output', '261', '531' )
*    INTO TABLE @lt_matdoc.
*
*
*
*    LOOP AT lt_matdoc INTO ls_matdoc.
*      UPDATE ztexturization
*          SET posted = 'X',
*              document = @ls_matdoc-materialdocument
*          WHERE productionorder = @ls_matdoc-productionorder
*              AND orderdate = @ls_matdoc-orderdate
*              AND type = @ls_matdoc-type
*              AND shift = @ls_matdoc-shift
*              AND material = @ls_matdoc-material
*              AND transid = @ls_matdoc-transid
*              AND sno = @ls_matdoc-sno.
*
*    ENDLOOP.

  ENDMETHOD.


    METHOD sendactivity.
      DATA: op_multiplier TYPE p LENGTH 10 DECIMALS 8.
      IF activity IS NOT INITIAL.
        op_multiplier = quantity / over_qty.
        qty = op_multiplier * ( consumption ).

        RETURN qty.
      ENDIF.

    ENDMETHOD.


  METHOD validate.

    isvalidated = abap_true.
    DATA message TYPE string.

    SELECT SINGLE FROM i_productionorder
    FIELDS orderconfirmedyieldqty, orderplannedtotalqty
    WHERE productionorder = @productionorder
    INTO @DATA(order).

    SELECT SINGLE FROM zr_texturization
            FIELDS SUM( quantity ) AS quantity, material
            WHERE productionorder = @productionorder
                  AND orderdate = @orderdate
                  AND type IN ( 'Output' )
                  AND transid = @transid
            GROUP BY material
            INTO @DATA(qty).

    IF ( order-orderconfirmedyieldqty + qty-quantity ) > order-orderplannedtotalqty.
      message = |Error: Yield quantity exceeds the planned total quantity for manufacturing order|.
      UPDATE ztexturization SET  errorlog = @message
          WHERE productionorder = @productionorder
                AND orderdate = @orderdate
                AND type IN ( 'Output' )
                AND material = @qty-material
                AND transid = @transid.
*      RETURN.
    ENDIF.
    clear : message.

    SELECT FROM zr_texturization
              FIELDS material, batch, uom, quantity, storagelocation, type,sno
              WHERE productionorder = @productionorder
                    AND orderdate = @orderdate
                    AND type IN ( 'Output', '261', '531' )
                    AND transid = @transid
              INTO TABLE @DATA(lt_textconsump_lines1).

    LOOP AT lt_textconsump_lines1 INTO DATA(ls_goodsmovement) WHERE quantity GT 0.

      IF ls_goodsmovement-storagelocation IS INITIAL.
        message = |Error: Storage Location is mandatory for material { ls_goodsmovement-material }| .
        isvalidated = abap_false.
      ENDIF.

      IF ls_goodsmovement-type NE 'Output' AND ls_goodsmovement-type NE '531'.

        SELECT SINGLE FROM i_stockquantitycurrentvalue_2( p_displaycurrency = 'INR' ) AS stock
           FIELDS  SUM( stock~matlwrhsstkqtyinmatlbaseunit ) AS stockqty
           WHERE stock~valuationareatype = '1'
           AND stock~product = @ls_goodsmovement-material
           AND stock~plant = @plant
           AND stock~storagelocation = @ls_goodsmovement-storagelocation
           AND stock~batch = @ls_goodsmovement-batch
           INTO @DATA(result).

        IF result IS INITIAL.
          message = |Error: Material { ls_goodsmovement-material ALPHA = OUT } not found in stock for plant { plant } and storage location { ls_goodsmovement-storagelocation } Quantity { ls_goodsmovement-quantity }|.
          isvalidated = abap_false.
*          RETURN.
        ELSEIF result < ls_goodsmovement-quantity.
          message = |Error: Insufficient stock for material { ls_goodsmovement-material ALPHA = OUT } in plant { plant } and storage location { ls_goodsmovement-storagelocation } Quantity { ls_goodsmovement-quantity - result }|.
          isvalidated = abap_false.
*          RETURN.
        ENDIF.
      ELSE.

        SELECT SINGLE FROM i_product AS material
          FIELDS material~product, material~isbatchmanagementrequired
          WHERE material~product = @ls_goodsmovement-material
          INTO @DATA(res1).

        IF res1-isbatchmanagementrequired  = 'X' AND ls_goodsmovement-batch IS INITIAL.
          message = |Error: Batch is mandatory for material { ls_goodsmovement-material }|.
          isvalidated = abap_false.
        ENDIF.

      ENDIF.

      IF isvalidated = abap_false.
        UPDATE ztexturization SET  errorlog = @message
            WHERE productionorder = @productionorder
                  AND orderdate = @orderdate
                  AND type IN ( 'Output', '261', '531' )
                  AND sno = @ls_goodsmovement-sno
                  AND material = @ls_goodsmovement-material
                  AND transid = @transid.
      ENDIF.
      CLEAR : message,ls_goodsmovement,result,res1.
    ENDLOOP.
   clear : order,qty.
  ENDMETHOD.
ENDCLASS.
