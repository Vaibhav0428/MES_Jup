CLASS zcl_update_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

  INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.

  DATA: lv_po TYPE ztexturization-productionorder VALUE ''.
   DATA: lv_mat TYPE c length 18 VALUE ''.
   DATA: lv_tid TYPE ztexturization-transid VALUE ''.
   DATA: lv_batch TYPE ztexturization-batch.
   data : lv_header type ztransferjournal-header_id.
   data: check type i value '0'.
ENDCLASS.



CLASS ZCL_UPDATE_CLASS IMPLEMENTATION.


METHOD if_oo_adt_classrun~main.


  IF check = '1'.
    lv_po = |{ lv_po ALPHA = IN }|.

    lv_mat = |{ lv_mat ALPHA = IN }|.
    SELECT FROM ztexturization AS a
    FIELDS a~productionorder,a~transid,a~sno,a~batch,a~posted,a~errorlog,a~sendtomes,a~document
    WHERE a~productionorder = @lv_po AND a~transid = @lv_tid AND a~material = @lv_mat AND a~batch = @lv_batch
    INTO TABLE @DATA(it_batch).



    LOOP AT it_batch INTO DATA(wa_batch) .
      wa_batch-batch = |{ wa_batch-batch ALPHA = OUT }|.
      UPDATE ztexturization
      SET errorlog = '',
      posted = '',
      sendtomes = '',
      batch = @wa_batch-batch,
      document = ''
      WHERE productionorder = @wa_batch-productionorder AND transid = @wa_batch-transid AND sno = @wa_batch-sno.
      CLEAR : wa_batch.
    ENDLOOP.

  ENDIF.

  IF check = '2'.
    DELETE FROM ztransferjournal WHERE header_id = @lv_header.
  ENDIF.

  IF check = '3'.

    lv_po = |{ lv_po ALPHA = IN }|.

    SELECT FROM ztexturization AS a
    FIELDS a~productionorder,a~transid,a~sno,a~batch,a~posted,a~errorlog,a~sendtomes,a~document
    WHERE a~productionorder = @lv_po AND a~transid = @lv_tid AND a~type = 'Output'
    INTO TABLE @DATA(it).

    SORT it BY sno.

    LOOP AT it INTO DATA(wa).
      UPDATE ztexturization
      SET errorlog = '',
      posted = '',
      sendtomes = '',
      document = ''
      WHERE productionorder = @wa-productionorder AND transid = @wa-transid AND sno = @wa-sno.
      CLEAR : wa.
    ENDLOOP.
  ENDIF.

ENDMETHOD.
ENDCLASS.
