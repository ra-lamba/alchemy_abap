FUNCTION zfm_contract_clauses_buy.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_POST) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(I_ALL_CLAUSES) TYPE  CHAR1 DEFAULT SPACE
*"  TABLES
*"      T_CLAUSES_INPUT STRUCTURE  ZST_CLAUSES_INPUT_BUY OPTIONAL
*"      T_CLAUSES_ALL STRUCTURE  /CGDC/_CLSHDR OPTIONAL
*"      T_MESSAGES STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      DB_UPDATE
*"----------------------------------------------------------------------



**********************************************
*Constans
**********************************************

  CONSTANTS:
    lc_mtype_E VALUE 'E',
    lc_mtype_w VALUE 'W',
    lc_mtype_s VALUE 'S',
    lc_posnr   TYPE c LENGTH 6 VALUE '000000'.



**********************************************
*Data Declerations
**********************************************

  DATA: it_clauses_assigned TYPE TABLE OF /cgdc/_clsfdab,
        it_hierarchy        TYPE TABLE OF /cgdc/_clsactb,
        it_messages         TYPE TABLE OF bapiret2,
        it_clauses_all      TYPE TABLE OF /cgdc/_clshdr,
        "        iT_zCLAUSES_ASSIGNED TYPE TABLE OF zst_clauses_assigned,
        it_CLAUSES_INPUT    TYPE TABLE OF zst_clauses_input_BUY.

  DATA: wa                  TYPE /cgdc/_clsfdab,
        wa_clauses_assigned TYPE /cgdc/_clsfdab,
        wa_clauses_input    TYPE zst_clauses_input_BUY,
        wa_180              TYPE t180,
        wa_message          TYPE bapiret2,
        wa_clauses          TYPE /cgdc/_clshdr,
        wa_CLSABHDR         TYPE /cgdc/_clsabhdr,
        lv_matnr            TYPE n LENGTH '18'.

  DATA: lv_ctype        TYPE /cgdc/ctype.

**********************************************
*Data Assignment
**********************************************

  CLEAR: lv_ctype, it_clauses_assigned, t_clauses_all,  t_clauses_input, it_messages, it_hierarchy.
  it_clauses_input[] = t_clauses_input[] .



**********************************************
*Get All Clauses
*that can be assigned
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
*Method: POST
*Get all assigned Clauses
**********************************************
  IF i_post IS NOT INITIAL.


    IF it_clauses_input[] IS INITIAL.
      wa_message-type = 'S'.
      wa_message-message = 'No Input data entered'.
      INSERT wa_message INTO TABLE it_messages.
      RETURN.
    ENDIF.

    "Read the Input Clauses and find the Subsequent Nodes
    LOOP AT it_clauses_input INTO wa_clauses_input.

      "data Validation:
      SELECT SINGLE *
        INTO wa_CLSABHDR FROM /cgdc/_clsabhdr
        WHERE     werks = wa_clauses_input-plant AND
      lifnr = wa_clauses_input-vendor  AND
      sub_type = wa_clauses_input-contract_type  AND
      mconn = wa_clauses_input-prime_contract_number  AND
      matkl = wa_clauses_input-material_group  AND
      matnr = wa_clauses_input-material_number AND
      knttp = wa_clauses_input-account_assignment_category  .

      IF sy-subrc = 0.
        wa_clauses_input-variable_key = wa_CLSABHDR-vakey. " get the variable key and assign into the input table
        MODIFY it_clauses_input FROM wa_clauses_input.
      ELSE.
        wa_message-type = lc_mtype_E.
        wa_message-message = 'Record number does not exist'.
        INSERT wa_message INTO TABLE it_messages.
        CLEAR: it_clauses_assigned, wa_clauses_input, it_hierarchy.
        CONTINUE.
      ENDIF.


      "Check if the clause has already been posted
      SELECT SINGLE * INTO wa
        FROM /cgdc/_clsfdab
        WHERE vakey = wa_clauses_input-variable_key AND
              clnum = wa_clauses_input-clnum.
      IF sy-subrc = 0.
        "perform update.
        wa-prt04 = wa_clauses_input-print_pr.
        wa-prt05 = wa_clauses_input-print_po.
        wa-prt09 = wa_clauses_input-print_sch_agree.
        wa-fdt04 = wa_clauses_input-flow_pr.
        wa-fdt05 = wa_clauses_input-flow_po.
        wa-fdt09 = wa_clauses_input-flow_sch_agree.

        TRY.
            UPDATE /cgdc/_clsfdab FROM wa.
            IF sy-subrc = 0.
              COMMIT WORK AND WAIT.
              wa_message-type = lc_mtype_s.
              wa_message-message = 'Variable key:' &&  wa_clauses_input-variable_key && 'has been updated'.
              INSERT wa_message INTO TABLE it_messages.
            ELSE.
              wa_message-type = lc_mtype_e.
              wa_message-message = 'Variable key:' &&  wa_clauses_input-variable_key && 'Error while DB Update'.
              INSERT wa_message INTO TABLE it_messages.
            ENDIF.
        ENDTRY.


        CLEAR: it_clauses_assigned, wa_clauses_input, it_hierarchy.
        CONTINUE.
      ENDIF.

      "Find the input clause from the availble cluases
      READ TABLE it_clauses_all INTO wa_clauses WITH KEY clnum = wa_clauses_input-clnum.
      IF sy-subrc NE 0.
        wa_message-type = lc_mtype_E.
        wa_message-message = 'Clause: ' && wa_clauses_input-clnum  &&  ' invalid'.
        INSERT wa_message INTO TABLE it_messages.
        CLEAR: it_clauses_assigned, wa_clauses_input, it_hierarchy.
        CONTINUE.
      ENDIF.

      "Fill out data according to the Hierarchy of Subsequent Nodes of a Clause
      PERFORM get_subsequent_nodes_buy
        USING wa_clauses_input
        CHANGING
              it_clauses_assigned
              it_hierarchy
              it_messages.

      "Post the data into SAP
      IF it_clauses_Assigned IS NOT INITIAL AND it_hierarchy IS NOT INITIAL.
        TRY.
            INSERT /cgdc/_clsfdab FROM TABLE it_clauses_Assigned.
            IF sy-subrc = 0.
              COMMIT WORK AND WAIT.
              wa_message-type = lc_mtype_s.
              wa_message-message = 'Clause: ' && wa_clauses_input-clnum &&  ' has been posted against Variable key:' &&  wa_clauses_input-variable_key.
              INSERT wa_message INTO TABLE it_messages.
            ELSE.
              wa_message-type = lc_mtype_e.
              wa_message-message = 'Clause: ' && wa_clauses_input-clnum &&  ' Error while DB Update'.
              INSERT wa_message INTO TABLE it_messages.
              t_clauses_all[] = it_clauses_all[].
              "              RAISE db_update.
            ENDIF.
        ENDTRY.
        TRY.
            INSERT /cgdc/_clsactb FROM TABLE it_hierarchy.
            IF sy-subrc = 0.
              COMMIT WORK AND WAIT.
              wa_message-type = lc_mtype_s.
*              wa_message-message = 'Clause: ' && wa_clauses_input-clnum &&  ' has been posted against Variable key:' &&  wa_clauses_input-variable_key.
*              INSERT wa_message INTO TABLE it_messages.
            ELSE.
*              wa_message-type = lc_mtype_e.
*              wa_message-message = 'Clause: ' && wa_clauses_input-clnum &&  ' Error while DB Update'.
*              INSERT wa_message INTO TABLE it_messages.
*              t_clauses_all[] = it_clauses_all[].
*              RAISE db_update.
            ENDIF.
        ENDTRY.

      ENDIF.


      CLEAR: it_clauses_assigned, wa_clauses_input, it_hierarchy.

    ENDLOOP.



  ENDIF.


*Assign All Clauses to output
  IF i_all_clauses IS NOT INITIAL. " get ALL clauses that can be assigned
    t_clauses_all[] = it_clauses_all[].
  ENDIF.


*Assign Messages to output
  t_messages[] = it_messages[].

ENDFUNCTION.


FORM get_subsequent_nodes_buy
  USING wa_clauses_input    TYPE zst_clauses_input_buy
  CHANGING
        lt_clauses_assigned TYPE ANY TABLE
        lt_hierarchy        TYPE ANY TABLE
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

  DATA: wa_clauses_assigned TYPE /cgdc/_clsfdab,
        wa_message          TYPE bapiret2,
        lt_clauses          TYPE TABLE OF /cgdc/_clshdr,
        wa_clauses          TYPE /cgdc/_clshdr,
        parent_clause       TYPE /cgdc/_clshdr-clnum,
        wa_hierarchy        TYPE /cgdc/_clsactb.



**********************************************
*clear Data
**********************************************
  CLEAR: parent_clause,wa_clauses, lt_clauses, wa_message, wa_clauses_assigned, lt_hierarchy.


**********************************************
*Get Data
**********************************************

  " get all clauses to build the hierarchy
  SELECT *
         FROM /cgdc/_clshdr
         INTO CORRESPONDING FIELDS OF TABLE lt_Clauses.


**********************************************
*Data Assignment
**********************************************

  "Assign the Actual Clause from the Child node without the Parent
  wa_clauses_assigned-mandt = sy-mandt.
  wa_clauses_assigned-vakey = wa_clauses_input-variable_key.
  wa_clauses_assigned-clnum = wa_clauses_input-clnum.
  READ TABLE lt_Clauses INTO wa_clauses WITH KEY clnum = wa_clauses_input-clnum.
  IF sy-subrc = 0 .
    wa_clauses_assigned-vldfr = wa_clauses-vldfr.
  ENDIF.
  wa_clauses_assigned-fdt04 = wa_clauses_input-flow_pr.
  wa_clauses_assigned-fdt05 = wa_clauses_input-flow_po.
  wa_clauses_assigned-fdt09 = wa_clauses_input-flow_sch_agree.
  wa_clauses_assigned-prt04 = wa_clauses_input-print_pr.
  wa_clauses_assigned-prt05 = wa_clauses_input-print_po.
  wa_clauses_assigned-prt09 = wa_clauses_input-print_sch_agree.
  INSERT wa_clauses_assigned INTO TABLE lt_clauses_assigned.
  CLEAR wa_clauses_assigned.

  wa_hierarchy-mandt = sy-mandt.
  wa_hierarchy-vakey = wa_clauses_input-variable_key.
  wa_hierarchy-clnum = wa_clauses_input-clnum.
  wa_hierarchy-aspec = 'X'.
  INSERT wa_hierarchy INTO TABLE lt_hierarchy.
  CLEAR wa_hierarchy.



**********************************************
*"Do Recursive calls till the top node
**********************************************

  parent_clause = wa_clauses_input-clnum.
  LOOP AT lt_Clauses INTO wa_clauses.

    READ TABLE lt_Clauses INTO wa_clauses WITH KEY clnum = parent_clause.
    IF sy-subrc = 0 AND wa_clauses-prncl IS NOT INITIAL.
      wa_hierarchy-mandt = sy-mandt.
      wa_hierarchy-vakey = wa_clauses_input-variable_key.
      wa_hierarchy-clnum = wa_clauses-prncl.
      wa_hierarchy-aafor =  wa_clauses_input-clnum.
      INSERT wa_hierarchy INTO TABLE lt_hierarchy.
      CLEAR wa_hierarchy.
      parent_clause = wa_clauses-prncl. " ASSIGN NEW PARENT
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.



ENDFORM.
