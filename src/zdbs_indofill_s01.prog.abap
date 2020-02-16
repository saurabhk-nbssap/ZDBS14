*&---------------------------------------------------------------------*
*&  Include           ZDBS_INDOFILL_S01
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
*  Selection screen - program selection
*-----------------------------------------------------------------------

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: PA_LAUFD LIKE ZDBSPR-LAUFD MODIF ID DEF," OBLIGATORY ,    " Run ID
            PA_LAUFI LIKE ZDBSPR-LAUFI MODIF ID DEF. "OBLIGATORY ,    " Run Date
PARAMETERS: PA_TEST  AS CHECKBOX DEFAULT 'X' MODIF ID DEF.             " Test Run checkbox
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS:
            PA_VALDT LIKE SY-DATUM DEFAULT SY-DATUM OBLIGATORY,       " Value Date
*            pa_pfile LIKE rlgrap-filename OBLIGATORY
*                         DEFAULT 'C:\CEMS\infolder',                  " Path where the cheque file gets saved
*            pa_dfile LIKE rlgrap-filename OBLIGATORY
*                         DEFAULT 'C:\CEMS\infolder',                  " Path where the DD file gets saved
            PA_RTGS LIKE RLGRAP-FILENAME OBLIGATORY
                        DEFAULT 'C:\CEMS\infolder',                   " Path where the rtgs file gets saved
            PA_NEFT LIKE RLGRAP-FILENAME OBLIGATORY
                        DEFAULT 'C:\CEMS\infolder',                   " Path where the neft file gets saved
            PA_EFILE LIKE RLGRAP-FILENAME OBLIGATORY
                       DEFAULT 'C:\CEMS\infolder',                   " Path where the EFT file gets saved
            PA_ERROR LIKE RLGRAP-FILENAME OBLIGATORY
                      DEFAULT 'C:\dbs\errorlog'.                      " Path where the error file gets saved
SELECTION-SCREEN END OF BLOCK B2.


PARAMETERS P_CHEQ AS CHECKBOX USER-COMMAND DEF.

SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-007.

PARAMETERS: PA_PFILE LIKE RLGRAP-FILENAME MODIF ID MD2 OBLIGATORY
            DEFAULT 'C:\CEMS\infolder'.

SELECTION-SCREEN END OF BLOCK B5.

PARAMETERS P_DD AS CHECKBOX USER-COMMAND GHI.

SELECTION-SCREEN BEGIN OF BLOCK B6 WITH FRAME TITLE TEXT-013.

PARAMETERS: PA_DFILE LIKE RLGRAP-FILENAME MODIF ID MD3 OBLIGATORY
            DEFAULT 'C:\CEMS\infolder'.

SELECTION-SCREEN END OF BLOCK B6.


PARAMETERS: P_CEMS  AS CHECKBOX USER-COMMAND ABC.                     " Check Box for selecting Encryption option

SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-004.
PARAMETERS: PA_CEMS TYPE CHAR1024 MODIF ID MD1 DEFAULT 'C:\CEMS',     " Path of the CEMS software - except the bin
            PA_KEYST TYPE CHAR1024 MODIF ID MD1
            DEFAULT 'C:\CEMS\keystore\INNIPL01.jceks',                " Key file location
            PA_PIN TYPE CHAR1024 MODIF ID MD1,                        " PIN used for encryption
            PA_CRTID TYPE CHAR1024 MODIF ID MD1,                      " Certificate ID
            PA_PATH TYPE CHAR1024 MODIF ID MD1
            DEFAULT 'C:\CEMS\bin\CEMS.bat'.                           " Batch File location
SELECTION-SCREEN END OF BLOCK B4.


*----
*At selection-screen on value-request
*----
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_LAUFI.
  PERFORM GET_LAUFI_LAUFD_HELP USING 'PA_LAUFI'
                                      'I'
                                      'PA_LAUFD'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_LAUFD.
  PERFORM GET_LAUFI_LAUFD_HELP USING 'PA_LAUFD'
                                      'D'
                                      'PA_LAUFI'.


*&---------------------------------------------------------------------*
*&      Form  get_laufi_laufd_help
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LV_FNAME   text
*      -->LV_F1TYP   text
*      -->LV_F2NME   text
*----------------------------------------------------------------------*
FORM GET_LAUFI_LAUFD_HELP USING  LV_FNAME LIKE DYNPREAD-FIELDNAME
                          LV_F1TYP
                          LV_F2NME.


*
  DATA:    BEGIN OF TLAUFK OCCURS 1.
          INCLUDE STRUCTURE ILAUFK.
  DATA:    END OF TLAUFK.

  DATA : LV_LAUFI LIKE REGUH-LAUFI,
         LV_LAUFD LIKE REGUH-LAUFD.

  DATA: LT_DYNPREAD LIKE TABLE OF DYNPREAD,
        LW_DYNPREAD LIKE LINE OF LT_DYNPREAD.

  TLAUFK-LAUFK = SPACE.
  TLAUFK-SIGN  = 'I'.
  APPEND TLAUFK.

  CALL FUNCTION 'F4_ZAHLLAUF'
    EXPORTING
      F1TYP = LV_F1TYP
      F2NME = LV_F2NME
    IMPORTING
      LAUFD = LV_LAUFD
      LAUFI = LV_LAUFI
    TABLES
      LAUFK = TLAUFK.

  LW_DYNPREAD-FIELDNAME  = LV_FNAME.
  IF LV_FNAME CS 'LAUFI'.
    WRITE LV_LAUFI TO LW_DYNPREAD-FIELDVALUE.
  ELSE.
    WRITE LV_LAUFD TO LW_DYNPREAD-FIELDVALUE.
  ENDIF.
  APPEND LW_DYNPREAD TO LT_DYNPREAD.

  LW_DYNPREAD-FIELDNAME  = LV_F2NME.
  IF LV_F2NME CS 'LAUFI'.
    WRITE LV_LAUFI TO LW_DYNPREAD-FIELDVALUE.
  ELSE.
    WRITE LV_LAUFD TO LW_DYNPREAD-FIELDVALUE.
  ENDIF.
  APPEND LW_DYNPREAD TO LT_DYNPREAD.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      DYNAME     = SY-CPROG
      DYNUMB     = '1000'
    TABLES
      DYNPFIELDS = LT_DYNPREAD.


ENDFORM.                    " GET_laufi_laufd_help
