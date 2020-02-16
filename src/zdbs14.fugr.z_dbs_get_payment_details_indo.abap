FUNCTION Z_DBS_GET_PAYMENT_DETAILS_INDO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_REGUH) TYPE  REGUH
*"     REFERENCE(I_DD_PM)
*"     REFERENCE(I_CHW_PM)
*"     REFERENCE(I_RTGS_PM)
*"     REFERENCE(I_NEFT_PM)
*"  TABLES
*"      I_REGUP STRUCTURE  REGUP
*"      I_BSEG STRUCTURE  BSEG
*"      E_MACRO STRUCTURE  ZPDETL0
*"      E_MICRO STRUCTURE  ZPDETL
*"----------------------------------------------------------------------
*Email----------------------------------------------------*
  TYPES: BEGIN OF ty_mail,
        addrnumber TYPE adr6-addrnumber,
        smtp_addr TYPE adr6-smtp_addr,
        END OF ty_mail.
  DATA: it_mail TYPE STANDARD TABLE OF ty_mail,
         wa_mail TYPE ty_mail.

  DATA:first_addr TYPE string,sec_addr TYPE string ,third_addr TYPE string,
       forth_addr TYPE string,fifth_addr TYPE string,no_invoices TYPE i,no_emails TYPE i,no_lines TYPE i,
       main_addr1 TYPE string,main_addr2 TYPE string,main_addr3 TYPE string,main_addr4 TYPE string,
       lv_colon TYPE c VALUE ';',lv_len_addr1 TYPE i,lv_len_addr2 TYPE i,lv_len_addr3 TYPE i,
       lv_len_addr4 TYPE i,lv_tem TYPE i,lv_pos TYPE i,ld_index TYPE sytabix,lv_no TYPE i,
        gv_p1(60),gv_p2(20),gv_lenth type i,gvv_at(1) type c value '@',gvv_dot(1) type c value '.',
       gv_pos type i,gv_ename(60) type c value 'abc@indofil.com'.

if not i_reguh-zadnr is initial and i_reguh-adrnr is not initial.

    select addrnumber smtp_addr from adr6
    into table it_mail
    where addrnumber = i_reguh-adrnr.

  elseif not i_reguh-zadnr is initial and i_reguh-adrnr is initial.

    select addrnumber smtp_addr from adr6
    into table it_mail
    where addrnumber = i_reguh-zadnr.

  elseif i_reguh-zadnr is initial and i_reguh-adrnr is not initial.

    select addrnumber smtp_addr from adr6
    into table it_mail
    where addrnumber = i_reguh-adrnr.

  elseif i_reguh-zadnr is initial and i_reguh-adrnr is initial.

    select tlfxs
    into table it_mail from lfb1
    where lifnr = i_reguh-lifnr
    and   bukrs = i_reguh-zbukr.

  endif.

loop at it_mail into wa_mail.

    search wa_mail-smtp_addr for gvv_at.
    if sy-subrc eq 0.
      split wa_mail-smtp_addr at '@' into gv_p1 gv_p2.
      condense gv_p2.
      if gv_p2 ca gvv_dot.
        gv_lenth = strlen( gv_p2 ).
        gv_pos = gv_lenth - 1.
        if gv_p2+gv_pos(1) eq '.'.
          wa_mail-smtp_addr = gv_ename.
          condense wa_mail-smtp_addr no-gaps.
        endif.
      else.
        wa_mail-smtp_addr = gv_ename.
        condense wa_mail-smtp_addr no-gaps.
      endif.
    else.
      wa_mail-smtp_addr = gv_ename.
      condense wa_mail-smtp_addr no-gaps.
    endif.

    if wa_mail-smtp_addr is initial.
      move gv_ename to wa_mail-smtp_addr.
      condense wa_mail-smtp_addr no-gaps.
    endif.
    modify it_mail from wa_mail index sy-tabix.
  endloop.
  clear:gv_pos,gv_lenth,gv_p1,gv_p2.

 if sy-subrc eq 0.
    LOOP AT it_mail INTO wa_mail.
      IF sy-tabix EQ 1.
        MOVE wa_mail-smtp_addr TO first_addr.
      ELSEIF sy-tabix EQ 2.
        MOVE wa_mail-smtp_addr TO sec_addr.
      ELSEIF sy-tabix EQ 3.
        MOVE wa_mail-smtp_addr TO third_addr.
      ELSEIF sy-tabix EQ 4.
        MOVE wa_mail-smtp_addr TO forth_addr.
      ELSE.
        MOVE wa_mail-smtp_addr TO fifth_addr.
      ENDIF.
    ENDLOOP.
  ENDIF.
*----------------------------------------------------------------*
  DATA: temp_dmbtr(12), temp_budat(10), temp_bldat(10),vflag TYPE c.
  DATA: temp_wt_qbshh(09) , temp_wskto(09),lv_flag TYPE c,lv_final(70),
             lv_docno(10),
             lv_invno(10),
             lv_date(10),
             lv_buss(4),
             lv_gross(12),
             lv_tds(09),
             lv_ded(09),
             lv_text(70).
  DATA: first_invoice_flag(1).
  DATA : lincnt TYPE i.
  TABLES : with_item.
  DATA lv_wt_qbshh LIKE with_item-wt_qbshh.
  DATA : lv_delimiter VALUE '|'.

  first_invoice_flag = '1'.

  LOOP AT i_regup.

    AT NEW lifnr.
      CLEAR : lincnt.
      CLEAR e_micro.
    ENDAT.

    CLEAR lv_wt_qbshh .
    SELECT  * FROM with_item WHERE bukrs = i_regup-bukrs
                               AND belnr = i_regup-belnr
                               AND gjahr = i_regup-gjahr
                               AND buzei = i_regup-buzei.
      lv_wt_qbshh = lv_wt_qbshh + with_item-wt_qbshh .
    ENDSELECT.

    WRITE lv_wt_qbshh TO temp_wt_qbshh NO-SIGN NO-GROUPING
    CURRENCY i_regup-waers DECIMALS 2 RIGHT-JUSTIFIED.

*   Prepare Micro Payment Details for payment methods 'CO'/'DD'/'CHW'
    IF i_reguh-rzawe CA i_dd_pm OR
       i_reguh-rzawe CA i_chw_pm OR
       i_reguh-rzawe CA i_rtgs_pm OR
       i_reguh-rzawe CA i_neft_pm.


      WRITE i_regup-dmbtr TO temp_dmbtr NO-SIGN NO-GROUPING
      CURRENCY i_regup-waers DECIMALS 2 RIGHT-JUSTIFIED.
      CONDENSE temp_dmbtr.
      IF i_regup-shkzg EQ 'S'.
        CONCATENATE '-' temp_dmbtr INTO temp_dmbtr.
      ENDIF.

      WRITE i_regup-wskto TO temp_wskto NO-SIGN NO-GROUPING
      CURRENCY i_regup-waers DECIMALS 2 RIGHT-JUSTIFIED.

      WRITE i_regup-budat TO temp_budat DD/MM/YYYY.
      WRITE i_regup-bldat TO temp_bldat DD/MM/YYYY.

      READ TABLE i_bseg WITH KEY bukrs = i_regup-bukrs
                                 belnr = i_regup-belnr
                                 gjahr = i_regup-gjahr
                                 buzei = i_regup-buzei.
      IF sy-subrc = 0.
        lincnt = lincnt + 1.
        IF lincnt = 1.

          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.

          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.

            e_micro-block1+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.
          ENDIF.
*----------------------------------------------------------*
        ELSEIF lincnt = 2.
          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.

          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.
            e_micro-block2+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.
          ENDIF.
*---------------------------------------------------------------*
        ELSEIF lincnt = 3.
          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.

          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.

            e_micro-block3+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.

          ENDIF.
*-------------------------------------------------------------------*

        ELSEIF lincnt = 4.

          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.

          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.
            e_micro-block4+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.

          ENDIF.
*-----------------------------------------------------------------*

        ELSEIF lincnt = 5.

          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.

          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.

            e_micro-block5+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.
          ENDIF.
*--------------------------------------------------------------------*
        ELSEIF lincnt = 6.

          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.

          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.

            e_micro-block6+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.
          ENDIF.
*---------------------------------------------------------------------*
        ELSEIF lincnt = 7.

          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.

          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.
            e_micro-block7+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.

          ENDIF.
*--------------------------------------------------------------*
        ELSEIF lincnt = 8.

          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.

          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.

            e_micro-block8+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.

          ENDIF.
*-------------------------------------------------------------------*

        ELSEIF lincnt = 9.

          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.

          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.
            e_micro-block9+0(70) = lv_final+0(70).

            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.

          ENDIF.
*------------------------------------------------------------*
        ELSEIF lincnt = 10.

          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.

          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.

            e_micro-block10+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.

          ENDIF.
*-------------------------------------------------------------------*

        ELSEIF lincnt = 11.

          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.

          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.

            e_micro-block11+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.

          ENDIF.
*------------------------------------------------------------------*
        ELSEIF lincnt = 12.

          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.

          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.

            e_micro-block12+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.

          ENDIF.
*-----------------------------------------------------------------*
        ELSEIF lincnt = 13.

          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.
          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.
            e_micro-block13+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.

          ENDIF.
*-----------------------------------------------------------------*
        ELSEIF lincnt = 14.

          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.
          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.

            e_micro-block14+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.

          ENDIF.
*-----------------------------------------------------------------*
        ELSEIF lincnt = 15.

          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.
          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.

            e_micro-block15+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.

          ENDIF.
*-----------------------------------------------------------------*
        ELSEIF lincnt = 16.

          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.
          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.

            e_micro-block16+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.

          ENDIF.
*-----------------------------------------------------------------*
        ELSEIF lincnt = 17.

          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.
          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.

            e_micro-block17+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.
          ENDIF.
*-----------------------------------------------------------------*
        ELSEIF lincnt = 18.

          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.
          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.

            e_micro-block18+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.

          ENDIF.
*-----------------------------------------------------------------*
        ELSEIF lincnt = 19.
          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.
          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.

            e_micro-block19+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.
          ENDIF.
*-----------------------------------------------------------------*
        ELSEIF lincnt = 20.
*
          AT NEW vblnr.
            lv_flag = 'X'.
            IF lv_flag EQ 'X'.
*             e_micro-block1+0(10) = 'DocumentNo'.
*             e_micro-block1+10(1) = '|'.
*             E_MICRO-BLOCK1+11(10) = 'InvoiceNum'.
*             e_micro-block1+21(1)  = '|'.
*             E_MICRO-BLOCK1+22(10) = 'InvoiceDat'.
*             e_micro-block1+32  = '|'.
*             e_micro-block1+33(4) = 'Buss'.
*             e_micro-block1+37  = '|'.
*             e_micro-block1+38(10) = 'Gross Amt '.
*             e_micro-block1+48  = '|'.
*             e_micro-block1+49(10) = ' TDS Amt  '.
*             e_micro-block1+59  = '|'.
*             e_micro-block1+60(10) = 'Deductions'.
              CLEAR lv_flag.
            ENDIF.
          ENDAT.
          IF lv_flag EQ space.
            lv_docno = i_regup-belnr.
            lv_invno = i_regup-xblnr.
            lv_date = temp_bldat.
            lv_buss = i_reguh-srtgb.
            IF lv_buss IS INITIAL.
              lv_buss = i_regup-gsber.
            ENDIF.
            lv_gross = temp_dmbtr.
            lv_tds = temp_wt_qbshh.
            lv_ded = temp_wskto.
            WRITE lv_ded TO lv_ded LEFT-JUSTIFIED.

            AT END OF vblnr.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.

              vflag = 'X'.
              IF vflag = 'X'.
*                do nothing.
              ENDIF.
            ENDAT.

            IF vflag EQ space.
              lv_final+0(10) = lv_docno.
              lv_final+10 = '|'.
              lv_final+11(10) = lv_invno.
              lv_final+21  = '|'.
              lv_final+22(10) = lv_date.
              lv_final+32  = '|'.
              lv_final+33(4) = lv_buss.
              lv_final+37  = '|'.
              lv_final+38(12) = lv_gross.
              lv_final+50  = '|'.
              lv_final+51(09) = lv_tds.
              lv_final+60  = '|'.
              lv_final+61 = lv_ded.
            ENDIF.

            e_micro-block20+0(70) = lv_final+0(70).
            CLEAR:lv_invno,lv_docno,lv_ded,lv_tds,lv_gross,lv_buss.

          ENDIF.
*-----------------------------------------------------------------*

          APPEND e_micro.
          CLEAR lincnt.
          CLEAR e_micro.
        ENDIF.

      ENDIF.
    ENDIF.

    AT LAST.
      IF NOT e_micro IS INITIAL.
        APPEND e_micro.
      ENDIF.

    ENDAT.

  ENDLOOP.
ENDFUNCTION.
