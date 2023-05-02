FUNCTION zfm_contract_clauses.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_GET) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(I_POST) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(I_ALL_CLAUSES) TYPE  CHAR1 DEFAULT SPACE
*"  TABLES
*"      T_CLAUSES_ASSIGNED STRUCTURE  ZST_CLAUSES_ASSIGNED OPTIONAL
*"      T_CLAUSES_INPUT STRUCTURE  ZST_CLAUSES_INPUT OPTIONAL
*"      T_CLAUSES_ALL STRUCTURE  /CGDC/_CLSHDR OPTIONAL
*"      T_MESSAGES STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      DB_UPDATE
*"----------------------------------------------------------------------



**********************************************
*Constans
**********************************************

  CONSTANTS: lc_fcode_read TYPE fcode VALUE 'READ',
             lc_tcode      TYPE c LENGTH 4 VALUE 'VA43',
             lc_mtype_E    VALUE 'E',
             lc_mtype_w    VALUE 'W',
             lc_mtype_s    VALUE 'S',
             lc_posnr      TYPE c LENGTH 6 VALUE '000000'.



**********************************************
*Data Declerations
**********************************************

  DATA: it_clauses_assigned  TYPE TABLE OF /cgdc/clsacdo,
        it_messages          TYPE TABLE OF bapiret2,
        it_clauses_all       TYPE TABLE OF /cgdc/_clshdr,
        iT_zCLAUSES_ASSIGNED TYPE TABLE OF zst_clauses_assigned,
        it_CLAUSES_INPUT     TYPE TABLE OF zst_clauses_input.

  DATA: wa                  TYPE /cgdc/clsacdo,
        wa_clauses_assigned TYPE /cgdc/clsacdo,
        wa_clauses_input    TYPE zst_clauses_input,
        wa_vbak             TYPE vbak,
        wa_vbap             TYPE vbap,
        wa_180              TYPE t180,
        wa_message          TYPE bapiret2,
        wa_clauses          TYPE /cgdc/_clshdr.

  DATA: lv_ctype        TYPE /cgdc/ctype,
        lv_sales_doc    TYPE vbeln,
        lv_sales_itemno TYPE posnr,
        lv_vbeln        TYPE vbeln.


**********************************************
*Data Assignment
**********************************************

  CLEAR: wa_vbak , wa_vbap, lv_ctype, it_clauses_assigned, t_clauses_all, t_clauses_assigned, t_clauses_input, it_messages.
  it_clauses_input[] = t_clauses_input[] .

*  wa_vbak-vbeln = i_sales_document.
*  wa_vbap-vbeln = i_sales_document.
*  wa_vbap-posnr = i_sales_item_no.
*  wa_180-tcode = lc_tcode.


**********************************************
*Validations
**********************************************
  LOOP AT it_clauses_input INTO wa_clauses_input.

    lv_sales_doc = wa_clauses_input-contract_no+2.
    lv_sales_itemno = wa_clauses_input-item_no.
    "Sales order Validation:
    SELECT SINGLE vbeln INTO lv_vbeln FROM vbak WHERE vbeln = lv_sales_doc.
    IF sy-subrc NE 0.
      wa_message-type = lc_mtype_E.
      wa_message-message = TEXT-001.
      INSERT wa_message INTO TABLE it_messages.
    ENDIF.

    "Sales order and Item number Validation:
    IF wa_clauses_input-item_no NE lc_PosNR.
      SELECT SINGLE vbeln INTO lv_vbeln FROM vbap WHERE vbeln = lv_sales_doc AND posnr = lv_sales_itemno.
      IF sy-subrc NE 0.
        wa_message-type = lc_mtype_E.
        wa_message-message = TEXT-002 .
        INSERT wa_message INTO TABLE it_messages.
        RETURN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  READ TABLE it_messages INTO wa_message WITH KEY type = 'E'.
  IF sy-subrc = 0.
    "Assign Messages to output
    t_messages[] = it_messages[].
    RETURN.
  ENDIF.

**********************************************
*Get All Clauses
*that can be assigned to a Sales Document
**********************************************

  " get all clauses
  SELECT *
         FROM /cgdc/_clshdr
         INTO CORRESPONDING FIELDS OF TABLE it_clauses_all.

  "delete the clauses which are not the actual clauses of the child nodes
  SORT it_clauses_all  BY clnum.
  LOOP AT it_clauses_all INTO wa_clauses.
    DELETE it_clauses_all WHERE clnum = wa_clauses-prncl.
  ENDLOOP.

  SORT it_clauses_all  BY cltyp clnum.



**********************************************
*Method: GET
*Get all assigned Clauses to a Sales Document
**********************************************

  IF i_get IS NOT INITIAL AND it_clauses_input[] IS NOT INITIAL.

    PERFORM get_all_assigned_clauses
      USING it_clauses_input
      CHANGING it_zclauses_assigned
               it_messages.

    SORT it_zclauses_assigned BY docno.
    t_clauses_assigned[] = it_zclauses_assigned[].

**********************************************
*Method: POST
*Get all assigned Clauses to a Sales Document
**********************************************
  ELSEIF i_post IS NOT INITIAL.


    IF it_clauses_input[] IS INITIAL.
      wa_message-type = 'S'.
      wa_message-message = TEXT-004.
      INSERT wa_message INTO TABLE it_messages.
      RETURN.
    ENDIF.

    "Read the Input Clauses and find the Subsequent Nodes
    LOOP AT it_clauses_input INTO wa_clauses_input.

      lv_sales_doc = wa_clauses_input-contract_no+2.
      lv_sales_itemno = wa_clauses_input-item_no.

      "Check if Contract/Project type is maintained
      SELECT SINGLE /cgdc/ctype INTO lv_ctype FROM /cgdc/cf_sd_hd WHERE vbeln = lv_sales_doc.
      IF sy-subrc NE 0 OR lv_ctype IS INITIAL.
        wa_message-type = lc_mtype_E.
        wa_message-message = TEXT-003.
        INSERT wa_message INTO TABLE it_messages.
        CONTINUE.
      ENDIF.

      "Check if the clause has already been posted against the Sales document
      SELECT SINGLE * INTO wa
        FROM /cgdc/clsacdo
        WHERE docno = wa_clauses_input-contract_no+2 AND
              posnr = wa_clauses_input-item_no AND
              clnum = wa_clauses_input-clnum.
      IF sy-subrc = 0.
        wa_message-type = lc_mtype_E.
        wa_message-message = 'Clause: ' && wa_clauses_input-clnum  &&  ' has already been posted against Sales Contract:' &&  wa_clauses_input-contract_no+2.
        INSERT wa_message INTO TABLE it_messages.
        CONTINUE.
      ENDIF.

      "Find the input clause from the availble cluases
      READ TABLE it_clauses_all INTO wa_clauses WITH KEY clnum = wa_clauses_input-clnum.
      IF sy-subrc NE 0.
        wa_message-type = lc_mtype_E.
        wa_message-message = 'Clause: ' && wa_clauses_input-clnum  &&  ' invalid'.
        INSERT wa_message INTO TABLE it_messages.
        CONTINUE.
      ENDIF.

      "Fill out data according to the Hierarchy of Subsequent Nodes of a Clause
      PERFORM get_subsequent_nodes
        USING wa_clauses_input
        CHANGING
              it_clauses_assigned
              it_messages.

      "Post the data into SAP
      IF it_clauses_Assigned IS NOT INITIAL.
        TRY.
            INSERT /cgdc/clsacdo FROM TABLE it_clauses_Assigned.
            IF sy-subrc = 0.
              COMMIT WORK AND WAIT.
              wa_message-type = lc_mtype_s.
              wa_message-message = 'Clause: ' && wa_clauses_input-clnum &&  ' has been posted against Sales Contract:' &&  wa_clauses_input-contract_no+2.
              INSERT wa_message INTO TABLE it_messages.
            ELSE.
              wa_message-type = lc_mtype_e.
              wa_message-message = 'Clause: ' && wa_clauses_input-clnum &&  ' Error while DB Update'.
              INSERT wa_message INTO TABLE it_messages.
              t_clauses_all[] = it_clauses_all[].
              RAISE db_update.
            ENDIF.
        ENDTRY.
      ENDIF.

      CLEAR: it_clauses_assigned, wa_clauses_input.

    ENDLOOP.



  ENDIF.


*Assign All Clauses to output
  IF i_all_clauses IS NOT INITIAL. " get ALL clauses that can be assigned
    t_clauses_all[] = it_clauses_all[].
  ENDIF.


*Assign Messages to output
  t_messages[] = it_messages[].

ENDFUNCTION.


FORM get_subsequent_nodes
  USING wa_clauses_input    TYPE zst_clauses_input
  CHANGING
        lt_clauses_assigned TYPE ANY TABLE
        lt_messages TYPE ANY TABLE.

**********************************************
*Constants
**********************************************

  CONSTANTS: lc_I     VALUE 'I',
             lc_eq    TYPE c LENGTH 2 VALUE 'EQ',
             lc_docct TYPE c LENGTH 2 VALUE '00'.

**********************************************
*Data Declerations
**********************************************

  DATA: wa_clauses_assigned TYPE /cgdc/clsacdo,
        wa_message          TYPE bapiret2,
        lt_clauses          TYPE TABLE OF /cgdc/_clshdr,
        wa_clauses          TYPE /cgdc/_clshdr,
        parent_clause       TYPE /cgdc/_clshdr-clnum.


**********************************************
*Data Assignment
**********************************************
  CLEAR: parent_clause,wa_clauses, lt_clauses, wa_message, wa_clauses_assigned.

  "Assign the Actual Clause from the Child node without the Parent
  wa_clauses_assigned-mandt = sy-mandt.
  wa_clauses_assigned-docno = wa_clauses_input-contract_no+2.
  wa_clauses_assigned-itmno = wa_clauses_input-item_no.
  wa_clauses_assigned-docct = lc_docct.
  wa_clauses_assigned-clnum = wa_clauses_input-clnum.
  wa_clauses_assigned-fdt01 = wa_clauses_input-flow_so.
  wa_clauses_assigned-fdt02 = wa_clauses_input-flow_delv.
  wa_clauses_assigned-fdt03 = wa_clauses_input-flow_prod_ord.
  wa_clauses_assigned-fdt04 = wa_clauses_input-flow_pr.
  wa_clauses_assigned-fdt05 = wa_clauses_input-flow_po.
  wa_clauses_assigned-fdt06 = wa_clauses_input-flow_wbs.
  wa_clauses_assigned-fdt07 = wa_clauses_input-flow_billing.
  wa_clauses_assigned-fdt08 = wa_clauses_input-flow_accounting.
  wa_clauses_assigned-fdt09 = wa_clauses_input-flow_sch_agree.
  wa_clauses_assigned-prt01 = wa_clauses_input-print_so.
  wa_clauses_assigned-prt02 = wa_clauses_input-print_delv.
  wa_clauses_assigned-prt03 = wa_clauses_input-print_prod_ord.
  wa_clauses_assigned-prt04 = wa_clauses_input-print_pr.
  wa_clauses_assigned-prt05 = wa_clauses_input-print_po.
  wa_clauses_assigned-prt06 = wa_clauses_input-print_wbs.
  wa_clauses_assigned-prt07 = wa_clauses_input-print_billing.
  wa_clauses_assigned-prt08 = wa_clauses_input-print_accounting.
  wa_clauses_assigned-prt09 = wa_clauses_input-print_sch_agree.
  INSERT wa_clauses_assigned INTO TABLE lt_clauses_assigned.
  CLEAR wa_clauses_assigned.



**********************************************
*Get Data
**********************************************

  " get all clauses to build the hierarchy
  SELECT *
         FROM /cgdc/_clshdr
         INTO CORRESPONDING FIELDS OF TABLE lt_Clauses.


**********************************************
*"Do Recursive calls till the top node
**********************************************

  parent_clause = wa_clauses_input-clnum.
  LOOP AT lt_Clauses INTO wa_clauses.

    READ TABLE lt_Clauses INTO wa_clauses WITH KEY clnum = parent_clause.
    IF sy-subrc = 0 AND wa_clauses-prncl IS NOT INITIAL.
      wa_clauses_assigned-mandt = sy-mandt.
      wa_clauses_assigned-docno = wa_clauses_input-contract_no+2.
      wa_clauses_assigned-itmno = wa_clauses_input-item_no.
      wa_clauses_assigned-docct = lc_docct.
      wa_clauses_assigned-clnum = wa_clauses-prncl.
      wa_clauses_assigned-aafor =  wa_clauses_input-clnum.
      wa_clauses_assigned-vldfr = wa_clauses-vldfr.
      INSERT wa_clauses_assigned INTO TABLE lt_clauses_assigned.
      CLEAR wa_clauses_assigned.
      parent_clause = wa_clauses-prncl. " ASSIGN NEW PARENT
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.



ENDFORM.

FORM get_all_assigned_clauses
  USING it_clauses_input TYPE ANY TABLE
        CHANGING it_zclauses_assigned TYPE ANY TABLE
        it_messages TYPE ANY TABLE.

**********************************************
*Data Declerations
**********************************************
  DATA: it_clauses_header   TYPE TABLE OF /cgdc/_clshdr,
        it_clauses_headert  TYPE TABLE OF /cgdc/_clshdrt,
        it_clauses_assigned TYPE TABLE OF /cgdc/clsacdo,
        lt_clauses_input    TYPE TABLE OF zst_clauses_input.

  DATA: wa_clauses_header    TYPE /cgdc/_clshdr,
        wa_clauses_headert   TYPE /cgdc/_clshdrt,
        wa_clauses_assigned  TYPE /cgdc/clsacdo,
        wa_zclauses_assigned TYPE zst_clauses_assigned,
        wa_message           TYPE bapiret2,
        wa_clauses_input     TYPE zst_clauses_input.


**********************************************
*Get Data
**********************************************
  lt_clauses_input[] = it_clauses_input[].

  "get assigned clauses of a sales document
  SELECT *
    INTO TABLE  it_clauses_assigned
    FROM /cgdc/clsacdo
    FOR ALL ENTRIES IN lt_clauses_input
    WHERE docno = lt_clauses_input-contract_no AND
          itmno = lt_clauses_input-item_no AND
          aafor = ''. " getting only actual clauses- not the heirarchy

  IF sy-subrc = 0.

    "    DELETE it_clauses_assigned WHERE itmno NE Sales_item.

    "get the header data of the clauses
    SELECT *
      INTO TABLE it_clauses_header
      FROM /cgdc/_clshdr
      FOR ALL ENTRIES IN it_clauses_assigned
      WHERE clnum = it_clauses_assigned-clnum.

    IF sy-subrc = 0.
      SELECT *
      INTO TABLE it_clauses_headert
      FROM /CGDC/_CLSHDRt
      FOR ALL ENTRIES IN it_clauses_header
      WHERE clnum = it_clauses_header-clnum.

    ENDIF.

**********************************************
*Move the Data to output internal table
**********************************************
    LOOP AT it_clauses_assigned INTO wa_clauses_assigned.
      MOVE-CORRESPONDING wa_clauses_assigned TO wa_zclauses_assigned.
      wa_zclauses_assigned-flow_so = wa_clauses_assigned-fdt01.
      wa_zclauses_assigned-print_so = wa_clauses_assigned-prt01.
      wa_zclauses_assigned-flow_delv = wa_clauses_assigned-fdt02.
      wa_zclauses_assigned-print_delv = wa_clauses_assigned-prt02.
      wa_zclauses_assigned-flow_prod_ord = wa_clauses_assigned-fdt03.
      wa_zclauses_assigned-print_prod_ord = wa_clauses_assigned-prt03.
      wa_zclauses_assigned-flow_pr = wa_clauses_assigned-fdt04.
      wa_zclauses_assigned-print_pr = wa_clauses_assigned-prt04.
      wa_zclauses_assigned-flow_po = wa_clauses_assigned-fdt05.
      wa_zclauses_assigned-print_po = wa_clauses_assigned-prt05.
      wa_zclauses_assigned-flow_wbs = wa_clauses_assigned-fdt06.
      wa_zclauses_assigned-print_wbs = wa_clauses_assigned-prt06.
      wa_zclauses_assigned-flow_billing = wa_clauses_assigned-fdt07.
      wa_zclauses_assigned-print_billing = wa_clauses_assigned-prt07.
      wa_zclauses_assigned-flow_accounting = wa_clauses_assigned-fdt08.
      wa_zclauses_assigned-print_accounting = wa_clauses_assigned-prt08.
      wa_zclauses_assigned-flow_sch_agree = wa_clauses_assigned-fdt09.
      wa_zclauses_assigned-print_sch_agree = wa_clauses_assigned-prt09.

      READ TABLE it_clauses_header INTO wa_clauses_header WITH KEY clnum = wa_clauses_assigned-clnum.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING wa_clauses_header  TO wa_zclauses_assigned.
        READ TABLE it_clauses_headert INTO wa_clauses_headert WITH KEY clnum = wa_clauses_assigned-clnum.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING wa_clauses_headert  TO wa_zclauses_assigned.
        ENDIF.
        INSERT wa_zclauses_assigned INTO TABLE it_zclauses_assigned.
      ENDIF.

    ENDLOOP.

  ELSE.
    wa_message-type = 'S'.
    wa_message-message = TEXT-007.
    INSERT wa_message INTO TABLE it_messages.
    RETURN.

  ENDIF.




ENDFORM.
