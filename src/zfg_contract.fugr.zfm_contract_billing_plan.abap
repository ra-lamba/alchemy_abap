FUNCTION zfm_contract_billing_plan.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_COMMIT) TYPE  AS4FLAG
*"  TABLES
*"      T_BILLING_PLAN STRUCTURE  ZFPLTVB
*"      T_MESSAGES STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      DB_UPDATE
*"      DBSQL_DUPLICATE_KEY_ERROR
*"----------------------------------------------------------------------



**********************************************
*Data Declerations
**********************************************
  DATA:
    ls_plan       TYPE zfplavb_ttyp_test,
    ls_bufferread TYPE sado_buf_flagstring,

    ld_fplnr_head TYPE fplnr,  " Kopf-Fakturaplan
    ld_fplnr_pos  TYPE fplnr,  " Position-Fakturaplan
    ls_vbak       TYPE vbak,
    ls_vbkd       TYPE vbkd,
    ls_vbkdvb     TYPE vbkdvb,
    ls_vbrk       TYPE vbrk,
    lt_vbkd       TYPE STANDARD TABLE OF vbkd,
    lt_vbkdvb     TYPE STANDARD TABLE OF vbkdvb,
*
    ls_fpla       TYPE fpla,
    ls_fplt       TYPE fplt,
    ls_zfpla      TYPE fplavb,
    ls_zfplt      TYPE fpltvb,
    lt_zfpla_old  TYPE STANDARD TABLE OF fplavb,
    lt_zfplt_old  TYPE STANDARD TABLE OF fpltvb,
    lt_zfpla_new  TYPE STANDARD TABLE OF fplavb,
    lt_zfplt_new  TYPE STANDARD TABLE OF fpltvb,
    t_MSTSD       TYPE TABLE OF mstsd,
    wa_plan       TYPE zfpltvb,
    wa_message    TYPE bapiret2.


  LOOP AT t_billing_plan INTO wa_plan.

    CLEAR: lt_vbkdvb, lt_vbkd, lt_zfpla_old, lt_zfplt_old, lt_zfpla_new, lt_zfplt_new.

**********************************************
*Read Contract
**********************************************
    CALL FUNCTION 'SD_VBKD_READ_WITH_VBELN'
      EXPORTING
        i_vbeln          = wa_plan-contract_no
      TABLES
        et_vbkdvb        = lt_vbkdvb
        et_vbkd          = lt_vbkd
      EXCEPTIONS
        record_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      wa_message-type = 'E'.
      CONCATENATE 'Sales Contract:' wa_plan-contract_no 'does not exist' INTO wa_message-message SEPARATED BY space.
      INSERT wa_message INTO TABLE t_messages.

      RETURN.  " leave function module
    ENDIF.

**********************************************
*Get billing plan number of header
**********************************************
    LOOP AT lt_vbkd INTO ls_vbkd
                    WHERE ( posnr IS INITIAL ).
      EXIT.
    ENDLOOP.
    IF ( syst-subrc = 0 ).
      ld_fplnr_head = ls_vbkd-fplnr.
    ELSE.
      CLEAR: ld_fplnr_head.
    ENDIF.



**********************************************
*Read billing plans (header and/or position)
**********************************************
    LOOP AT lt_vbkd INTO ls_vbkd.
      CALL FUNCTION 'BILLING_SCHEDULE_READ'
        EXPORTING
          fplnr         = ls_vbkd-fplnr
*         I_VFKDAT      =
*         I_BFKDAT      =
        TABLES
          zfpla         = lt_zfpla_old
          zfplt         = lt_zfplt_old
        EXCEPTIONS
          error_message = 1.
      IF sy-subrc <> 0.

        wa_message-type = 'E'.
        wa_message-message = 'Billing Schedule does not exist'.
        INSERT wa_message INTO TABLE t_messages.

        RETURN.  " leave function module
      ENDIF.
    ENDLOOP.

**********************************************
*Assign new with old data
**********************************************
    lt_zfpla_new = lt_zfpla_old.
    lt_zfplt_new = lt_zfplt_old.
    CLEAR: ls_zfpla, ls_zfplt.



**********************************************
*Update new data with input value
**********************************************

    CLEAR : ls_zfplt, ls_zfplA.
    READ TABLE lt_vbkd INTO ls_vbkd
         WITH KEY posnr = wa_plan-itEM_NO.
    IF ( syst-subrc NE 0 ).
      wa_message-type = 'E'.
      CONCATENATE 'Error ! Sales Contract:' wa_plan-contract_no 'Item No' wa_plan-item_no
                            'does not exist' INTO wa_message-message SEPARATED BY space.
      INSERT wa_message INTO TABLE t_messages.
      CONTINUE.
    ELSE.

      IF wa_plan-itEM_NO = '000000'.
        wa_message-type = 'E'.
        CONCATENATE 'Error ! Sales Contract:' wa_plan-contract_no 'Item No' wa_plan-item_no
                              'Invalid' INTO wa_message-message SEPARATED BY space.
        INSERT wa_message INTO TABLE t_messages.
        CONTINUE.
      ENDIF.


      READ TABLE lt_zfplt_old INTO ls_zfplt WITH KEY fplnr = ls_vbkd-fplnr fpltr = wa_plan-billing_item_no .
      IF sy-subrc = 0.
        wa_message-type = 'E'.
        CONCATENATE 'Error ! Sales Contract:' wa_plan-contract_no 'Item No' wa_plan-item_no
                              'Bill Item NO:' wa_plan-billing_item_no 'already exist' INTO wa_message-message SEPARATED BY space.
        INSERT wa_message INTO TABLE t_messages.
        CONTINUE.
      ENDIF.


      CLEAR : ls_zfplt, ls_zfplA.
      IF wa_plan-type = 'I'.
        ls_zfplt-fplnr = ls_vbkd-fplnr.
        ls_zfplt-fpltr = wa_plan-billing_item_no.           " '000003'.
        ls_zfplt-fkdat = wa_plan-billing_date." '20230520'.
        ls_zfplt-tetxt = wa_plan-date_descrption.
        "        ls_zfplt-kzmei = wa_plan-usage.
        ls_zfplt-fproz = wa_plan-percentage.
        ls_zfplt-fakwr = wa_plan-billing_value.
        ls_zfplt-waers = wa_plan-currency.
        ls_zfplt-faksp = wa_plan-billing_block.
        "       ls_zfplt-mlbez = wa_plan-milestone.
        ls_zfplt-fareg = wa_plan-billing_rule.
        ls_zfplt-fksaf = wa_plan-billing_status.
        ls_zfplt-fpttp = wa_plan-date_cateogry.
        ls_zfplt-fkarv = wa_plan-order_related_billing_type.
        ls_zfplt-kurfp = wa_plan-exchange_rate.
        ls_zfplt-mlstn = wa_plan-milestone_number.
        ls_zfplt-zterm = wa_plan-payment_terms.
        ls_zfplt-fpfix = wa_plan-fixed_date.
        ls_zfplt-/cgdc/event = wa_plan-event_id.
        ls_zfplt-/cgdc/event_desc = wa_plan-event_description.
        ls_zfplt-/cgdc/contlineno = wa_plan-clin.
        ls_zfplt-/cgdc/acrn = wa_plan-acrn.
        ls_zfplt-/cgdc/contsev = wa_plan-Continuous_cs.

        ls_zfplt-updkz = 'I'.
        INSERT ls_zfplt INTO TABLE lt_zfplt_new.

      ELSE.
        wa_message-type = 'E'.
        CONCATENATE 'Error !' 'Please maintain Type as I ' INTO wa_message-message SEPARATED BY space.
        INSERT wa_message INTO TABLE t_messages.
        CONTINUE.

*      ELSEIF wa_plan-type = 'U'.
*        ls_zfplt-updkz = 'U'.  " update
*        READ TABLE lt_zfplt_new INTO ls_zfplt WITH KEY fplnr = ls_vbkd-fplnr .
*        IF sy-subrc = 0.
*
*        ENDIF.
*
*        MODIFY TABLE lt_zfplt_new FROM ls_zfplt.

      ENDIF.

    ENDIF.




**********************************************
*Post data
**********************************************
    TRY.
        CALL FUNCTION 'BILLING_SCHEDULE_SAVE'
          TABLES
            fpla_new = lt_zfpla_new
            fpla_old = lt_zfpla_old
            fplt_new = lt_zfplt_new
            fplt_old = lt_zfplt_old.

      CATCH: cx_sy_open_sql_db.
        wa_message-type = 'E'.
        wa_message-message = 'Error ! Duplicate records'.
        INSERT wa_message INTO TABLE t_messages.
    ENDTRY.

    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      wa_message-type = 'I'.
      wa_message-message = 'Data Posted Succesfully'.
      INSERT wa_message INTO TABLE t_messages.
    ELSE.
      wa_message-type = 'E'.
      wa_message-message = 'Error ! cannot post the Data'.
      INSERT wa_message INTO TABLE t_messages.
    ENDIF.

*    CALL FUNCTION 'SD_ORDER_BILLING_SCHEDULE'
*      EXPORTING
*        i_vbeln                    = wa_plan-contract_no
*        i_beleg_lesen              = 'X'
*        i_commit                   = i_commit  " committed later
*        i_termine_bis_zum_horizont = 'X'
**       I_MEILENSTEINE_ZURUECKMELDEN       =
*        i_aktualisieren            = 'X'
*      TABLES
*        tmstsd                     = t_MSTSD.


  ENDLOOP.

ENDFUNCTION.
