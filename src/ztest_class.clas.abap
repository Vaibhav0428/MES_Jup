    CLASS ztest_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZTEST_CLASS IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

*    update ztransferjournal set entry_unit = 'PC', posting_uom = 'ST',material = 'FG00001'  WHERE header_id = 'JI01/7/SF01/638946370567212552' .
*    update ztransferjournal set entry_unit = 'PC', posting_uom = 'ST',material = 'FG00001'  WHERE header_id = 'JI01/7/SF01/638946371492409247' .

* delete from ztransferjournal where material = 'FG00002' and sno = '1'.
*    delete from ztransferjournal  WHERE sno is INITIAL.
*    DATA(productionorder) = 'd4292054-b3a3-4081-ad67-9163117c941f'.
*
*    IF productionorder IS NOT INITIAL.
*      DELETE FROM ztexturization  WHERE transid = '4a62742e-96c7-42c0-99a7-a86bf77a55bf'.
*    ENDIF.
*
*    DATA(transid) = 'd4292054-b3a3-4081-ad67-9163117c941f'.
*
*    IF transid IS NOT INITIAL.
      DELETE FROM ztexturization  WHERE posted = '' .
*    ENDIF.


*    DATA(productionorder) = 'JI02/1/SF01/638936144473851464'.
*
*    IF productionorder IS NOT INITIAL.
*      update ztransferjournal set entry_unit = 'PC', posting_uom = 'ST' ." WHERE header_id = 'JI02/1/SF01/638936197678402075' and sno = '1'.
*    ENDIF.



  ENDMETHOD.
ENDCLASS.
