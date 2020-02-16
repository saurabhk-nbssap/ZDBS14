*&---------------------------------------------------------------------*
*& Include ZDBS_INDOFILL_TOP                 Report ZDBS_INDOFILL
*&
*&---------------------------------------------------------------------*
*report ZDBS_INDOFILL_TOP .
TABLES:  REGUH, REGUP, BSEG, LFA1, T005T, T012K, T042Z, BNKA, T001,
         ZDBSPN, ZDBSCP, ZDBSPR,  LFBK, KNBK, F110V,
         SSCRFIELDS, TCURC, LFB1 , BKPF ,PAYR , ADRC , ZDBSTL."zdbsrc ,
*-----------------------------------------------------------------------*
*  Data Definitions - Internal Tables
*-----------------------------------------------------------------------*
*  Internal table for Cheque - File Header Record
DATA:  BEGIN OF PTABH OCCURS 1,
         RECTYP(3),                        " Record type
         CRDATE  LIKE SY-DATUM,            " Creation date
         CRTIME  LIKE SY-UZEIT,            " Creation time
         REFER(16),                        " Reference file name
         BUKRS   LIKE REGUH-ZBUKR,         " Company ID
         ENTIT(70),                        " Entity
         DESC(20),                         " File Description
         ISSUE(1),                         " Issuance Type
         GIROREF(5),                       " GIRO Reference
         FILL1(600),                                        " Filler 1
         RECVD(8),                         " Received Date (space)
         RECVT(6),                         " Receive Time (space)
         FILL2(2),                                          " Filler 2
         ORGAN(4),                         " Organization Code
         BANK(4),                          " Bank ID
         FILL3(735),                                        " Filler 3
       END OF PTABH.

*  Internal table for DD - File Header Record
DATA:  BEGIN OF DTABH OCCURS 1,
         RECTYP(3),                        " Record type
         CRDATE  LIKE SY-DATUM,            " Creation date
         CRTIME  LIKE SY-UZEIT,            " Creation time
         REFER(16),                        " Reference file name
         BUKRS   LIKE REGUH-ZBUKR,         " Company ID
         ENTIT(70),                        " Entity
         DESC(20),                         " File Description
         ISSUE(1),                         " Issuance Type
         GIROREF(5),                       " GIRO Reference
         FILL1(600),                                        " Filler 1
         RECVD(8),                         " Received Date (space)
         RECVT(6),                         " Receive Time (space)
         FILL2(2),                                          " Filler 2
         ORGAN(4),                         " Organization Code
         BANK(4),                          " Bank ID
         FILL3(735),                                        " Filler 3
       END OF DTABH.

*  Internal table for Cheque - Payment Record
DATA:  BEGIN OF PTAB OCCURS 1,
         RECTYP(3),                        " Record type
         PAYTYP(3),                        " Payment Type
         CUSREF(16),                       " Customer Reference
         CHEC_NO(16),                      " Checque number
         CURRCY(3),                        " Payment Currency
         AMOUNT(17),                       " Payment Amount
         VALUT  LIKE REGUH-VALUT,          " Value Date
         RECVD(70),                        " Receiving Party Name
         TEXT1(40),                        " Customer specified field
         TEXT2(40),                        " Customer specified field
         TEXT3(40),                        " Customer specified field
         TEXT4(40),                        " Customer specified field
         SCURR(3),                         " Settlement currency
         DRACCT(34),                       " Debit Account
         RCACCT(34),                       " Receiving account
         RCBANK(4),                        " Receiving Party bank
         RCBRCH(4),                        " Receiving Party branch
         BANKC(3),                         " Bank charges
         DELIND(1),                        " Delivery Indicator
         MAILTO(140),                      " Mail-to party name & add
         POSTAL(8),                        " Postal Code
         PAYDTL(280),                      " Payment Details
         BEMAIL(60),                       " Beneficiary email
         BFAXNO(12),                       " Beneficiary fax
         PRATLOC(16),                      " Print at location
         DDAREF(12),                       " DDA Reference
         ORIGST(3),                        " Original settlement
         ORIGDR(34),                       " Original Debit Account
         ORIGCQ(10),                       " Original Check
         ORIGAMT  LIKE REGUH-RWBTR,        " Original payment amount
         ORIGNA(20),                       " Originating name
         TCODE(2),                         " GIRO transaction code
         PARTI(12),                        " Particulars
         ADVISE(1),                        " Send beneficiary advise by
         PYBLAT(16),                       " Payable at location
         FILLER(494),                      " Filler
       END OF PTAB.

*  Internal table for DD - Payment Record
DATA:  BEGIN OF DTAB OCCURS 1,
         RECTYP(3),                        " Record type
         PAYTYP(3),                        " Payment Type
         CUSREF(16),                       " Customer Reference
         CHEC_NO(16),                      " Checque number
         CURRCY(3),                        " Payment Currency
         AMOUNT(17),                       " Payment Amount
         VALUT  LIKE REGUH-VALUT,          " Value Date
         RECVD(70),                        " Receiving Party Name
         TEXT1(40),                        " Customer specified field
         TEXT2(40),                        " Customer specified field
         TEXT3(40),                        " Customer specified field
         TEXT4(40),                        " Customer specified field
         SCURR(3),                         " Settlement currency
         DRACCT(34),                       " Debit Account
         RCACCT(34),                       " Receiving account
         RCBANK(4),                        " Receiving Party bank
         RCBRCH(4),                        " Receiving Party branch
         BANKC(3),                         " Bank charges
         DELIND(1),                        " Delivery Indicator
         MAILTO(140),                      " Mail-to party name & add
         POSTAL(8),                        " Postal Code
         PAYDTL(280),                      " Payment Details
         BEMAIL(60),                       " Beneficiary email
         BFAXNO(12),                       " Beneficiary fax
         PRATLOC(16),                      " Print at location
         DDAREF(12),                       " DDA Reference
         ORIGST(3),                        " Original settlement
         ORIGDR(34),                       " Original Debit Account
         ORIGCQ(10),                       " Original Check
         ORIGAMT  LIKE REGUH-RWBTR,        " Original payment amount
         ORIGNA(20),                       " Originating name
         TCODE(2),                         " GIRO transaction code
         PARTI(12),                        " Particulars
         ADVISE(1),                        " Send beneficiary advise by
         PYBLAT(16),                       " Payable at location
         FILLER(494),                      " Filler
       END OF DTAB.

*  Internal table for Cheque - File Trailer record
DATA:  BEGIN OF PTABT OCCURS 1,
         RECTYP(3),                        " Record type
         TOTTRN(8)  TYPE I,                " Total number of trans
         TOTPAY(9)  TYPE P DECIMALS 4,     " Total payment amount
         FILLER(1471),                     " Filler
       END OF PTABT.

*  Internal table for DD - File Trailer record
DATA:  BEGIN OF DTABT OCCURS 1,
         RECTYP(3),                        " Record type
         TOTTRN(8)  TYPE I,                " Total number of trans
         TOTPAY(9)  TYPE P DECIMALS 4,     " Total payment amount
         FILLER(1471),                     " Filler
       END OF DTABT.

*  Internal table for RTGS
DATA: BEGIN OF IT_RTGS OCCURS 1,
         PNAME(1),                         " Product name
         VBLNR(16),                        " customer Ref no
         UBKNT(35),                        " Debit Account Number
         VALUT(11),                        " Value Date
         VALUTS(11),                       " Value Date In string
         RWBTR(14),                        " Payment Amount
         WAERS(3),                         " Payment currency
         NAME(35),                         " Bene Name
         BEN_ADD1(35),                     " Bene address1
         BEN_ADD2(35),                     " Bene address2
         BEN_ADD3(35),                     " Bene address3
         ZBNKN(34),                        " Bene Account Number
         BANKA(35),                        " Bene Bank name
         BK_ADD1(35),                      " Bene Bank address1
         BK_ADD2(35),                      " Bene Bank address2
         BK_ADD3(35),                      " Bene Bank address3
         IFSC(11),                         " IFSC Code
         PAY_DTL(140),                     " Payment detail
         DELI_MODE(1),                     " Delivery mode
         ADV_NAME(100),                    " Mail to party name
         MAIL_ADD1(50),                    " Mail to party address1
         MAIL_ADD2(50),                    " Mail to party address2
         MAIL_ADD3(50),                    " Mail to party address3
         MAIL_ADD4(50),                    " Mail to party address4
         EMAIL(60),                        " E-mail address
         FAX(60),                          " Fax number
         ADVICE(2800),                     " Payment Details
      END OF IT_RTGS.

*  Final Output file for RTGS
DATA:  BEGIN OF OUTTAB_RTGS OCCURS 1,
         LINE_DTLS(4200),                   " Data in Ascii format
       END OF OUTTAB_RTGS.

*     Internal table for EFT Details
TYPES: BEGIN OF TYP_EFT,
        VAL_PROCS_DATE TYPE SY-DATUM,
        " Value Date or Processing Date
        CURRENCY(3),                       " Payment Currency
        COMPY_CODE(4),                     " Company Code
        PYMT_TYP(3),                       " Payment Type
        CUST_REF_NO(16),                   " Customer Reference Number
        AMOUNT(14), " TYPE wrbtr,          " Payment Amount
        AMOUNT_SUM TYPE RWBTR,
        PYMT_DTLS(140),                    " Payment Detalis
        BEN_NAME(35),
        " Beneficiary Name - TT / ACT / MEPS
        BEN_ADDR(105),
        " Beneficiary Address - TT / ACT / MEPS
        BANK_CODE(4),                      " Bank Code
        CNTRY_PAYBL(2),                    " Country Payable
        BEN_ACC_NO(34),                    " Beneficiary Account Number
        BEN_BNK_NAME_ADDR(140),
        " Beneficiary Bank Name & Address
        BEN_BNK_SWIFT_BIC(11),             " Beneficiary Bank SWIFT BIC
        DBS_CHRGS(3),                      " DBS Bank's Charges
        AGENT_CHRGS(3),                    " Agent Bank's Charges
        STTL_ACC_NO(34),                   " Settlement Account Number
        STTL_CURRY(3),                     " Settlement Currency
        DELVRY_MODE(2),                    " Delivery Method
        MAIL_TO_PARTY(3),                  " Mail to Party
       END OF TYP_EFT.

DATA: ITAB_EFT TYPE STANDARD TABLE OF TYP_EFT,
      WA_EFT TYPE TYP_EFT.

TYPES: BEGIN OF TYP_EFT_FINAL,
        RECORD(1500),
        END_OF_LINE,
       END OF TYP_EFT_FINAL.

DATA: ITAB_EFT_FINAL TYPE STANDARD TABLE OF TYP_EFT_FINAL,
      WA_EFT_FINAL TYPE TYP_EFT_FINAL.

DATA: BEGIN OF ADRS OCCURS 0.
        INCLUDE STRUCTURE ADRS.
DATA: END OF ADRS.

DATA: BEGIN OF IT_PAYR OCCURS 0.
        INCLUDE STRUCTURE PAYR.
DATA: END OF IT_PAYR.

TYPES: BEGIN OF TYP_TIBAN,
       BANKS TYPE TIBAN-BANKS,
       BANKL TYPE TIBAN-BANKL,
       BANKN TYPE TIBAN-BANKN,
       IBAN TYPE TIBAN-IBAN,
       END OF TYP_TIBAN.


DATA: ITAB_TIBAN TYPE STANDARD TABLE OF TYP_TIBAN,
      WA_TIBAN TYPE TYP_TIBAN.

TYPES:  BEGIN OF TYP_TEMP_REGUH,
        BANK_NO(35).
        INCLUDE STRUCTURE REGUH.
TYPES:  END OF TYP_TEMP_REGUH.

DATA: ITAB_TEMP_REGUH TYPE STANDARD TABLE OF TYP_TEMP_REGUH,
      WA_TEMP_REGUH TYPE TYP_TEMP_REGUH.

* Error log table
DATA: BEGIN OF IT_ERR OCCURS 0,
        VBLNR LIKE REGUH-VBLNR,
        ERROR(100),
      END OF IT_ERR.

TYPES: BEGIN OF TYP_UTAB,
          WORD(20),
        END OF TYP_UTAB.

DATA: ITAB_UTAB TYPE STANDARD TABLE OF TYP_UTAB,
      WA_UTAB TYPE TYP_UTAB.

DATA: BEGIN OF IT_FILIST OCCURS 0,
        FILENAME(128),                    " Used for file output
        FULLNAME(128),                    " Used for encryption
        OUTNAME(128),                     " Used for encrytion outname
        OUTPUTFOLDER(128),
      END OF IT_FILIST.

DATA:  BEGIN OF IREGUH OCCURS 100.
        INCLUDE STRUCTURE REGUH.
DATA:  END OF IREGUH.

DATA:  BEGIN OF IREGUP OCCURS 100.
        INCLUDE STRUCTURE REGUP.
DATA:  END OF IREGUP.

DATA:  BEGIN OF IREGUH2 OCCURS 100.
        INCLUDE STRUCTURE REGUH.
DATA:  END OF IREGUH2.

DATA:  BEGIN OF IREGUP2 OCCURS 100.
        INCLUDE STRUCTURE REGUP.
DATA:  END OF IREGUP2.

DATA:  BEGIN OF IBSEG OCCURS 100.
        INCLUDE STRUCTURE BSEG.
DATA:  END OF IBSEG.

DATA:  BEGIN OF IBSEG2 OCCURS 100.
        INCLUDE STRUCTURE BSEG.
DATA:  END OF IBSEG2.

DATA:  BEGIN OF IREGUP3 OCCURS 100.
        INCLUDE STRUCTURE REGUP.
DATA:  END OF IREGUP3.

DATA:  BEGIN OF IT_T012K OCCURS 100.
        INCLUDE STRUCTURE T012K.
DATA:  END OF IT_T012K.

DATA: BEGIN OF UTAB OCCURS 1,
          WORD(20),
        END OF UTAB.

* Final output file for chw and DD
DATA:  BEGIN OF OUTTAB OCCURS 100,
         LINE_DTLS(1500),                  " Data in Ascii format
       END OF OUTTAB.

*  Internal table for Invoice details for CHW and DD
DATA:  BEGIN OF IT_INV1 OCCURS 0,
         REC_TYPE(3),                      " RECORD TYPE
         INV_FILLER1(6) VALUE SPACE,       " SPACES
         CREA_DATE LIKE SY-DATUM,          " CREATION DATE
         CREA_TIME LIKE SY-UZEIT,          " CREATION TIME
         INV_FILLER2(6) VALUE SPACE,       " SPACES
         CUST_VBLNR LIKE REGUH-VBLNR,      " CUSTOMER REFERENCE
         PAYM_DTLS(1400),                  " PAYMENT DETAILS
         INV_FILLER3(618) VALUE SPACE,     " SPACES
       END OF IT_INV1.

DATA:  BEGIN OF BUFFERTAB OCCURS 1,
         LINE_DTLS(1500),                  " Data in Ascii format
       END OF BUFFERTAB.

DATA:  BEGIN OF BUFFERTAB2 OCCURS 1,
         LINE_DTLS(1500),                  " Data in Ascii format
       END OF BUFFERTAB2.

*  Internal table for Batch Summary Record
DATA:  BEGIN OF BSRTAB OCCURS 0,
         REC_TYPE(3),
         BSR_FILLER1(3) VALUE SPACE,
         CREA_DATE(8),
         CREA_TIME(6),
         TOTAL_TRAN(8) TYPE N,
         TOTAL_PAYM(18),
         BSR_FILLER2(1454),
       END OF BSRTAB.

*  Internal table for Batch Summary Record(Cheque)
DATA:  BEGIN OF P_BSRTAB OCCURS 0,
         REC_TYPE(3),
         TOTAL_TRAN(8) TYPE N,
         TOTAL_PAYM(18),
         BSR_FILLER2(1471),
       END OF P_BSRTAB.

*  Internal table for Batch Summary Record(DD)
DATA:  BEGIN OF D_BSRTAB OCCURS 0,
         REC_TYPE(3),
         TOTAL_TRAN(8) TYPE N,
         TOTAL_PAYM(18),
         BSR_FILLER2(1471),
       END OF D_BSRTAB.

DATA:  BEGIN OF OUTTAB_PRM OCCURS 100,
         LINE_DTLS(1500),                  " Data in Ascii format
       END OF OUTTAB_PRM.

TYPES: BEGIN OF TY_RTGS_NEW,
         PNAME(1),                         " Product name
         VBLNR(16),                        " customer Ref no
         UBKNT(35),                        " Debit Account Number
         VALUT(8),                         " Value Date
         VALUTS(11),                       " Value Date
         RWBTR(14),                        " Payment Amount
         WAERS(3),                         " Payment currency
         NAME(140),                        " Bene Name
         BEN_ADD1(35),                     " Bene address1
         BEN_ADD2(35),                     " Bene address2
         BEN_ADD3(35),                     " Bene address3
         ZBNKN(35),                        " Bene Account Number
         BANKA(35),                        " Bene Bank name
         BK_ADD1(35),                      " Bene Bank address1
         BK_ADD2(35),                      " Bene Bank address2
         BK_ADD3(35),                      " Bene Bank address3
         IFSC(11),                         " IFSC Code
         PAY_DTL(140),                     " Payment detail
         DELI_MODE(1),                     " Delivery mode
         ADV_NAME(100),                    " Mail to party name
         MAIL_ADD1(50),                    " Mail to party address1
         MAIL_ADD2(50),                    " Mail to party address2
         MAIL_ADD3(50),                    " Mail to party address3
         MAIL_ADD4(50),                    " Mail to party address4
         EMAIL(60),                        " E-mail address
         FAX(60),                          " Fax number
         ADVICE(2800),                     " Payment Details
      END OF TY_RTGS_NEW.

DATA: IT_RTGS_NEW TYPE STANDARD TABLE OF TY_RTGS_NEW,
      WA_RTGS_NEW TYPE TY_RTGS_NEW.

FIELD-SYMBOLS: <FS_RTGS_NEW> TYPE TY_RTGS_NEW.

*  Internal table for display Proposal list
DATA:  BEGIN OF IT_LIST OCCURS 0,
         RZAWE LIKE T042Z-ZLSCH,           " Payment Method
         LIFNR LIKE REGUP-LIFNR,           " Vendor Number
         NAME(70),                         " Vendor Name
         TEXT1 LIKE T042Z-TEXT1,           " Payment Method Text
         ZALDT LIKE REGUH-ZALDT,           " Posting Date of Payment Doc
         BUDAT LIKE REGUP-BUDAT,           " Posting Date
         BELNR LIKE REGUP-BELNR,           " Document Number
         XBLNR LIKE REGUP-XBLNR,           " Reference Document Number
         VBLNR LIKE REGUP-VBLNR,           " Payment Document Number
         ZFBDT LIKE BSEG-ZFBDT,            " Baseline Date
         SGTXT LIKE BSEG-SGTXT,            " Description
         RWBTR LIKE REGUH-RWBTR,           " Document Amount
         WRBTR LIKE REGUP-WRBTR,           " Document Amount
         WAERS LIKE REGUP-WAERS,           " Currency Key
         VALUT LIKE REGUH-VALUT,           " Value Date
       END OF IT_LIST.

* Address table
TYPES: BEGIN OF TY_ADRS.
        INCLUDE STRUCTURE ADRS.
TYPES: END OF TY_ADRS.

DATA: IT_ADRS TYPE STANDARD TABLE OF TY_ADRS,
      WA_ADRS TYPE TY_ADRS.

TYPES:  BEGIN OF TY_ZDBSPR.
        INCLUDE STRUCTURE ZDBSPR.
TYPES:  END OF TY_ZDBSPR.

DATA: IT_ZDBSPR TYPE STANDARD TABLE OF TY_ZDBSPR,
      WA_ZDBSPR TYPE TY_ZDBSPR.

TYPES: BEGIN OF TY_NEFT_FINAL,
        RECORD(2314),
        END_OF_LINE,
       END OF TY_NEFT_FINAL.

DATA: IT_NEFT_FINAL TYPE STANDARD TABLE OF TY_NEFT_FINAL,
      WA_NEFT_FINAL TYPE TY_NEFT_FINAL.

TYPES: BEGIN OF TY_ADRC,
        ADDRNUMBER TYPE AD_ADDRNUM,
        FAX_NUMBER TYPE AD_FXNMBR1,
       END OF TY_ADRC.

DATA: IT_ADRC TYPE STANDARD TABLE OF TY_ADRC,
      WA_ADRC TYPE TY_ADRC.

TYPES: BEGIN OF TY_ADR6,
        ADDRNUMBER TYPE AD_ADDRNUM,
        SMTP_ADDR TYPE AD_SMTPADR,
       END OF TY_ADR6.

DATA: IT_ADR6 TYPE STANDARD TABLE OF TY_ADR6,
      WA_ADR6 TYPE TY_ADR6.

DATA: IT_ZDBSPN TYPE STANDARD TABLE OF ZDBSPN,
      IT_ZDBSCP TYPE STANDARD TABLE OF ZDBSCP,
      WA_ZDBSPN TYPE ZDBSPN.

FIELD-SYMBOLS: <FS_ZDBSCP> TYPE ZDBSCP.

TYPES: BEGIN OF TY_T001,
        BUKRS TYPE BUKRS,
        BUTXT TYPE BUTXT,
       END OF TY_T001.

DATA:  IT_T001 TYPE STANDARD TABLE OF TY_T001,
       WA_T001 TYPE TY_T001.

TYPES: BEGIN OF TY_REGUH_PM,
        PYMT_MTD(4),
        SL_NO TYPE I,
        BANK_NO(35).
INCLUDE TYPE REGUH.
TYPES  END OF TY_REGUH_PM.

DATA: IT_REGUH_PM TYPE STANDARD TABLE OF TY_REGUH_PM,
      WA_REGUH_PM TYPE TY_REGUH_PM.

TYPES: BEGIN OF TY_BNKA,
        BANKS TYPE BANKS,
        BANKL TYPE BANKK,
        BANKA TYPE BANKA,
        BNKLZ TYPE BANKL,
        STRAS TYPE STRAS_GP,
        ORT01 TYPE ORT01_GP,
        SWIFT TYPE SWIFT,
        BRNCH TYPE BNKA-BRNCH,
       END OF TY_BNKA.

DATA: IT_BNKA TYPE STANDARD TABLE OF TY_BNKA,
      WA_BNKA TYPE TY_BNKA.

TYPES: BEGIN OF TY_NEFT_DETAILS,
        REC_TYP(1) TYPE N,            " Value is 1
        VALUE_DATE(8),                " Value date - Used for grouping
        SEND_ACC_NUM(35),             " Sender's Account Number - Used for grouping
        VALUE_DATE_FMT(10),           " Value date with "/" included
        I2_TRANS_REF_NUM(20),         " IDEAL Transaction Ref No
        BEN_BNK_IFSC_CODE(11),        " Beneficiary Bank IFSC Code
        BEN_ACC_NO(35),               " Beneficiary account no.
        BEN_NAME(50),                 " Beneficiary Name
        BEN_ADDRS1(35),               " Beneficiary Address line 1
        BEN_ADDRS2(35),               " Beneficiary Address line 2
        BEN_ADDRS3(35),               " Beneficiary Address line 3
        BEN_ADDRS4(35),               " Beneficiary Address line 4
        RES_STATUS(1),                " Resident status
        CITIZEN_STATUS(1),            " Citizen status
        BEN_CUST_ACC_TYP(2),          " Beneficiary customer a/c type
        T_CODE(2),                    " Transaction Code
        AMOUNT TYPE P DECIMALS 2,     " Amount
        CHAR_AMT(19) TYPE C,
        AMOUNT_SUM TYPE RWBTR,        " Amount Sum
        BNK_CHRGS(3),                 " Bank Charges
        TRNC_REF(16),                 " Transaction Reference
        PYMT_DTLS(210),               " Payment Details
        BOARD_RATE(10),               " Board Rate
        FX_CTRCT_REF(10),             " FX Contract Reference
        DELVRY_MODE(1),               " Delivery Mode
        EMAIL(60),                    " Email
        PH_NUM(24),                   " Phone number
        ADVICE_NAME(50),              " Advice Name
        ADRS_LN1(35),                 " Address line 1
        ADRS_LN2(35),                 " Address line 2
        ADRS_LN3(35),                 " Address line 3
        ADRS_LN4(35),                 " Address line 4
        ADVICE_DTLS(1500),            " Advice Details
      END OF TY_NEFT_DETAILS.

DATA: IT_NEFT_DETAILS TYPE STANDARD TABLE OF TY_NEFT_DETAILS,
      WA_NEFT_DETAILS TYPE TY_NEFT_DETAILS.

TYPES:BEGIN OF TY_DETAILS,
        TDFORMAT TYPE TLINE-TDFORMAT,
        TDLINE TYPE TLINE-TDLINE,
       END OF TY_DETAILS.

DATA:   IT_NEFTDTLS TYPE STANDARD TABLE OF TY_DETAILS,
        WA_NEFTDTLS TYPE TY_DETAILS.

*  Internal table for NEFT File Header record
TYPES: BEGIN OF TY_NEFT_HDR,
      REC_TYP(1) TYPE N,                   " Value is 1
      VALUE_DATE(10),                      " Value date with "/" included
      SEND_ACC_NUM(35),                    " Sender's Account Number - Used for grouping
      SNAME(35),                           " Sender name
      BATCH_NUM(5),                        " Batch number
      SEND_ID(8),                          " Sender ID
      FILLER(2220),
     END OF TY_NEFT_HDR.

DATA:IT_NEFT_HDR TYPE STANDARD TABLE OF TY_NEFT_HDR,
      WA_NEFT_HDR TYPE TY_NEFT_HDR.

DATA: MTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA: BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA: IT_FILETABLE TYPE FILETABLE,
      IT_FILETABLE1 TYPE FILETABLE,
      WA_FILETABLE TYPE FILE_TABLE,
      WA_FILETABLE1 TYPE FILE_TABLE.

*  variables to store payment method
DATA:  DD_PM(5)    TYPE C,
       EFT_PM(5) TYPE C,
       CHW_PM(5)   TYPE C,
       RTGS_PM(5)  TYPE C,
       NEFT_PM(5)  TYPE C.

*  Declare variables
DATA:  GV_PEFILE(128),
       GV_DFILENUM(10) VALUE '1',
       GV_PFILENUM(10) VALUE '1',
       GV_RFNAME(116),
       GV_DFNAME(116),
       GV_PFNAME(116),
       GV_XVORL(1),
       GV_EXIST(1),
       GV_RC(1),
       GV_FILLER1(16) VALUE SPACE,
       GV_IDENTITY TYPE REGUH-LAUFI,
       GV_FILLER2(81) VALUE SPACE,
       GV_TEMP(1400) TYPE C,
       GV_ID TYPE LAUFI,
       GV_AND(5) VALUE ' And ',
       GV_DOT(1) VALUE '.',
       GV_LEN TYPE N,
       GV_TOTAMT(15) TYPE P,
       GV_LENNEW TYPE N,
       GV_TEMP2(1400) TYPE C,
       GV_NEFT_CREDIT TYPE P DECIMALS 2,
       GV_NEFT_DEBIT TYPE P DECIMALS 2,
       GV_TEMP1(1400) TYPE C,
       GV_CHAR2(19) TYPE C,
       GV_TEMP3(1400) TYPE C,
*       gv_ename(60) TYPE c VALUE 'abc@indofil.com',"'abc@xchanging.com',
       GV_ENAME TYPE ZDBSPN-ZDBSMAIL,
       GV_P1(60),
       GV_P2(20),
       GV_LENTH TYPE I,
       GV_BUKRS TYPE BUKRS,
        L_UNAME TYPE UNAME,
         L_USNAME TYPE SY-UNAME,
       GV_FILENUM(5) TYPE N,
       GVV_AT(1) TYPE C VALUE '@',
       GVV_DOT(1) TYPE C VALUE '.',
       GV_POS TYPE I,
       GV_NO_FILES TYPE I,
       GV_NO_FILES1 TYPE I,
       GV_USER_ACTION TYPE I,
       GV_COUNT TYPE N,
       GV_USER_ACTION1 TYPE I,
       GV_CHAR TYPE C,
       GV_CEMS_PATH TYPE STRING,
       GV_BATCH_PATH TYPE STRING,
       GV_KEYSTR TYPE STRING,
       TOTAL_PAYM LIKE REGUH-RWBTR,
       TOTAL_TRAN(8) TYPE N VALUE 0,
       SUMMARY_FLAG(1) VALUE SPACE,
       MAXEFLAG(1) VALUE SPACE,
       MAXCFLAG(1) VALUE SPACE,
       BUFFER_COUNT TYPE I,
       T_RTGSFILE(116),
       T_NEFTFILE(116),
       GV_RTGSFILE(128),
       V_CODE(15),
       V_CODE1(15),
       V_CODE2(6),
       V_FIRST(1),
       V_MM(2),
       V_MMM(3),
       DATETIME(20) TYPE C,
       V_EFT TYPE C,
       W_WORKFILE(300),
       W_UPLOADFILE(300),
       W_FILE(300),
       FILENO TYPE I,
       W_TITLE(100),
       FILE_OPEN_FLAG(1),
       W_LINE TYPE I,
       UP_CRLF(2) TYPE X VALUE '0D0A'.

DATA : TEMP_AMT TYPE P DECIMALS 2,
       TEMP_AMT2 TYPE P DECIMALS 2,
       RC(1),
       G_VARIANT  LIKE DISVARIANT,
       GX_VARIANT LIKE DISVARIANT,
       G_SAVE(1) TYPE C,
       G_EXIT(1)  TYPE C.
*----------------------------------------------*
* Special Character Restriction
*----------------------------------------------*
DATA:  GV_TILT(1) VALUE '~',
       GV_TOPKEY(1) VALUE '`',
       GV_EXCLAMTRY(1) VALUE '!',
       GV_ATRATE(1) VALUE '@',
       GV_HASH(1) VALUE '#',
       GV_DOLLAR(1) VALUE '$',
       GV_PERCENT(1) VALUE '%',
       GV_CARRET(1) VALUE '^',
       GV_AMPERSAND(1) VALUE '&',
       GV_STAR(1) VALUE '*',
       GV_UNDERSCORE(1) VALUE '_',
       GV_EQUALS(1) VALUE '=',
       GV_LESS(1) VALUE '<',
       GV_MORE(1) VALUE '>',
       GV_SEMICOLON(1) VALUE ';',
       GV_DBLE(1) VALUE '"',
       GV_LFTBRCKT(1) VALUE '{',
       GV_RGTBRCKT(1) VALUE '}',
       GV_LFSQBRCKT(1) VALUE '[',
       GV_RTSQBRCKT(1) VALUE ']',
       GV_PIPE(1) VALUE '|',
       GV_SLASH(1) VALUE '\',
* Above To be replaced by these
       GV_HIFEN(1) VALUE '-',
       GV_AT(4) VALUE ' At ',
       GV_COLON(1) VALUE ':',
       GV_FWDSLASH(1) VALUE '/',
       GV_LSMALBRCKT(1) VALUE '(',
       GV_COMMA(1) VALUE ',',
       GV_RSMALBRCKT(1) VALUE ')',
       GV_LEN1(3) TYPE C,
       GV_LEN2(3) TYPE C,
       GV_ADMINID TYPE UNAME,
       GV_BATCH_NUM(8) TYPE N,
       C_BTCH_SUMMY_REC(3) VALUE '099',
       GV_IBAN TYPE TIBAN-IBAN,
       GV_ADMINID2 TYPE UNAME.

CONSTANTS: C_CARET VALUE '^',
           C_P TYPE C VALUE '\',
           C_TWENTY(2) VALUE '20',
           C_ONE(1) VALUE '1',
           C_NINE(1) VALUE '9',
            C_BEN(3) VALUE 'BEN',
           C_OUR(3) VALUE 'OUR',
           C_F VALUE 'F',
           C_DBSBANK_ID(4) VALUE '7171',
           C_REM_FILETYP(4) VALUE 'REM',
           C_FILE_DESC(20) VALUE 'DBS Pymt Extraction',
           C_EFT_EXTN(4) VALUE '.EFT',
           C_BTCH_HDR(3) VALUE '000',
           C_PYMT_REC(3) VALUE '010',
            C_FT(3) VALUE 'FT',
           C_ZERO(1) VALUE '0'.



*DATA: gv_fmname TYPE  rs38l_fnam.
*DATA gv_subrc LIKE sy-subrc.
*DATA: it_regup  LIKE regup OCCURS 0 WITH HEADER LINE,
*      it_regup1 LIKE regup OCCURS 0 WITH HEADER LINE,
*      it_reguh  LIKE reguh OCCURS 0 WITH HEADER LINE.
