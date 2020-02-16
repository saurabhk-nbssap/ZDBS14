*&---------------------------------------------------------------------*
*&  Include           ZDBS_INDOFILL_F01
*&---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       FORM init_filenames                                           *
*---------------------------------------------------------------------*
*       Initialize physical filenames
*---------------------------------------------------------------------*
FORM INIT_FILENAMES CHANGING RCODE .
  RCODE = 0.
  if p_cheq is not initial.
  CONCATENATE W_WORKFILE PA_PFILE INTO GV_PFNAME.
  endif.
  if p_dd is not initial.
  CONCATENATE W_WORKFILE PA_DFILE INTO GV_DFNAME.
  endif.
  MOVE PA_RTGS TO T_RTGSFILE.
  MOVE PA_NEFT TO T_NEFTFILE.

ENDFORM.                    "init_filenames
*---------------------------------------------------------------------*
*       FORM check_files_exist                                        *
*---------------------------------------------------------------------*
*  Check if input filename already exists                             *
*---------------------------------------------------------------------*
FORM CHECK_FILES_EXIST CHANGING RCODE TYPE ANY.
  DATA: W_TEST(128).
  DATA: L_ITAB_FILES TYPE STANDARD TABLE OF FILE_INFO,
        L_WA_FILES TYPE FILE_INFO.

  DATA: LV_SOURCE TYPE STRING,
        LV_DEST TYPE STRING,
        LV_RC TYPE I,
        LV_COUNT TYPE I,
        LV_DIR TYPE STRING,
        LV_ARCHIVE_DIR TYPE STRING,
        LV_ARCHIVE_DIR_CCH TYPE STRING,
        LV_ARCHIVE_DIR_NEFT TYPE STRING,
        LV_ARCHIVE_DIR_RTGS TYPE STRING,
        LV_ARCHIVE_DIR_DD TYPE STRING,
        LV_RESULT.

  LV_DIR = PA_NEFT.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
    EXPORTING
      DIRECTORY                   = LV_DIR
      FILTER                      = '*.*'
      FILES_ONLY                  = 'X'
    CHANGING
      FILE_TABLE                  = L_ITAB_FILES
      COUNT                       = LV_COUNT
    EXCEPTIONS
      CNTL_ERROR                  = 1
      DIRECTORY_LIST_FILES_FAILED = 2
      WRONG_PARAMETER             = 3
      ERROR_NO_GUI                = 4
      NOT_SUPPORTED_BY_GUI        = 5
      OTHERS                      = 6.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
*-----------------------------------------------------*
*    Archive folder Creation
*-----------------------------------------------------*
    CONCATENATE LV_DIR '\Archive' INTO LV_ARCHIVE_DIR.

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
      EXPORTING
        DIRECTORY            = LV_ARCHIVE_DIR
      RECEIVING
        RESULT               = LV_RESULT
      EXCEPTIONS
        CNTL_ERROR           = 1
        ERROR_NO_GUI         = 2
        WRONG_PARAMETER      = 3
        NOT_SUPPORTED_BY_GUI = 4
        OTHERS               = 5.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

    ELSE.

      IF LV_RESULT IS INITIAL.

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE
          EXPORTING
            DIRECTORY                = LV_ARCHIVE_DIR
          CHANGING
            RC                       = LV_RC
          EXCEPTIONS
            DIRECTORY_CREATE_FAILED  = 1
            CNTL_ERROR               = 2
            ERROR_NO_GUI             = 3
            DIRECTORY_ACCESS_DENIED  = 4
            DIRECTORY_ALREADY_EXISTS = 5
            PATH_NOT_FOUND           = 6
            UNKNOWN_ERROR            = 7
            NOT_SUPPORTED_BY_GUI     = 8
            WRONG_PARAMETER          = 9
            OTHERS                   = 10.
        IF SY-SUBRC <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
* WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
*-----------------------------------------------------*
*   Archive folder for NEFT
*-----------------------------------------------------*
      CONCATENATE LV_DIR '\Archive\NEFT' INTO LV_ARCHIVE_DIR_NEFT.

      CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
        EXPORTING
          DIRECTORY            = LV_ARCHIVE_DIR_NEFT
        RECEIVING
          RESULT               = LV_RESULT
        EXCEPTIONS
          CNTL_ERROR           = 1
          ERROR_NO_GUI         = 2
          WRONG_PARAMETER      = 3
          NOT_SUPPORTED_BY_GUI = 4
          OTHERS               = 5.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

      ELSE.

        IF LV_RESULT IS INITIAL.

          CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE
            EXPORTING
              DIRECTORY                = LV_ARCHIVE_DIR_NEFT
            CHANGING
              RC                       = LV_RC
            EXCEPTIONS
              DIRECTORY_CREATE_FAILED  = 1
              CNTL_ERROR               = 2
              ERROR_NO_GUI             = 3
              DIRECTORY_ACCESS_DENIED  = 4
              DIRECTORY_ALREADY_EXISTS = 5
              PATH_NOT_FOUND           = 6
              UNKNOWN_ERROR            = 7
              NOT_SUPPORTED_BY_GUI     = 8
              WRONG_PARAMETER          = 9
              OTHERS                   = 10.
          IF SY-SUBRC <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
* WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

        ENDIF.
*-----------------------------------------------------*
*  Archive folder Creation for Check
*-----------------------------------------------------*
        CONCATENATE LV_DIR '\Archive\CCH' INTO LV_ARCHIVE_DIR_CCH.

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
          EXPORTING
            DIRECTORY            = LV_ARCHIVE_DIR_CCH
          RECEIVING
            RESULT               = LV_RESULT
          EXCEPTIONS
            CNTL_ERROR           = 1
            ERROR_NO_GUI         = 2
            WRONG_PARAMETER      = 3
            NOT_SUPPORTED_BY_GUI = 4
            OTHERS               = 5.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

        ELSE.

          IF LV_RESULT IS INITIAL.

            CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE
              EXPORTING
                DIRECTORY                = LV_ARCHIVE_DIR_CCH
              CHANGING
                RC                       = LV_RC
              EXCEPTIONS
                DIRECTORY_CREATE_FAILED  = 1
                CNTL_ERROR               = 2
                ERROR_NO_GUI             = 3
                DIRECTORY_ACCESS_DENIED  = 4
                DIRECTORY_ALREADY_EXISTS = 5
                PATH_NOT_FOUND           = 6
                UNKNOWN_ERROR            = 7
                NOT_SUPPORTED_BY_GUI     = 8
                WRONG_PARAMETER          = 9
                OTHERS                   = 10.
            IF SY-SUBRC <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
* WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

          ENDIF.
*-----------------------------------------------------*
*  Archive Folder Creation for RTGS
*-----------------------------------------------------*
          CONCATENATE LV_DIR '\Archive\RTGS' INTO LV_ARCHIVE_DIR_RTGS.

          CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
            EXPORTING
              DIRECTORY            = LV_ARCHIVE_DIR_RTGS
            RECEIVING
              RESULT               = LV_RESULT
            EXCEPTIONS
              CNTL_ERROR           = 1
              ERROR_NO_GUI         = 2
              WRONG_PARAMETER      = 3
              NOT_SUPPORTED_BY_GUI = 4
              OTHERS               = 5.
          IF SY-SUBRC <> 0.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

          ELSE.
            IF LV_RESULT IS INITIAL.

              CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE
                EXPORTING
                  DIRECTORY                = LV_ARCHIVE_DIR_RTGS
                CHANGING
                  RC                       = LV_RC
                EXCEPTIONS
                  DIRECTORY_CREATE_FAILED  = 1
                  CNTL_ERROR               = 2
                  ERROR_NO_GUI             = 3
                  DIRECTORY_ACCESS_DENIED  = 4
                  DIRECTORY_ALREADY_EXISTS = 5
                  PATH_NOT_FOUND           = 6
                  UNKNOWN_ERROR            = 7
                  NOT_SUPPORTED_BY_GUI     = 8
                  WRONG_PARAMETER          = 9
                  OTHERS                   = 10.
              IF SY-SUBRC <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
* WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              ENDIF.
            ENDIF.
*-----------------------------------------------------*
*  Archive Folder Creation for DD
*-----------------------------------------------------*
            CONCATENATE LV_DIR '\Archive\DD' INTO LV_ARCHIVE_DIR_DD.

            CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
              EXPORTING
                DIRECTORY            = LV_ARCHIVE_DIR_DD
              RECEIVING
                RESULT               = LV_RESULT
              EXCEPTIONS
                CNTL_ERROR           = 1
                ERROR_NO_GUI         = 2
                WRONG_PARAMETER      = 3
                NOT_SUPPORTED_BY_GUI = 4
                OTHERS               = 5.
            IF SY-SUBRC <> 0.
              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

            ELSE.
              IF LV_RESULT IS INITIAL.

                CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE
                  EXPORTING
                    DIRECTORY                = LV_ARCHIVE_DIR_DD
                  CHANGING
                    RC                       = LV_RC
                  EXCEPTIONS
                    DIRECTORY_CREATE_FAILED  = 1
                    CNTL_ERROR               = 2
                    ERROR_NO_GUI             = 3
                    DIRECTORY_ACCESS_DENIED  = 4
                    DIRECTORY_ALREADY_EXISTS = 5
                    PATH_NOT_FOUND           = 6
                    UNKNOWN_ERROR            = 7
                    NOT_SUPPORTED_BY_GUI     = 8
                    WRONG_PARAMETER          = 9
                    OTHERS                   = 10.
                IF SY-SUBRC <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
* WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                ENDIF.
              ENDIF.
*---------------------------------------------------------------*
              LOOP AT L_ITAB_FILES INTO L_WA_FILES.

                IF L_WA_FILES-FILENAME CS 'NEFT'.
                  CLEAR: LV_SOURCE,LV_DEST.

                  CONCATENATE LV_DIR '\' L_WA_FILES-FILENAME  INTO LV_SOURCE.
                  CONCATENATE LV_ARCHIVE_DIR_NEFT '\' L_WA_FILES-FILENAME  INTO LV_DEST.

                  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_COPY
                    EXPORTING
                      SOURCE               = LV_SOURCE
                      DESTINATION          = LV_DEST
                      OVERWRITE            = 'X'
                    EXCEPTIONS
                      CNTL_ERROR           = 1
                      ERROR_NO_GUI         = 2
                      WRONG_PARAMETER      = 3
                      DISK_FULL            = 4
                      ACCESS_DENIED        = 5
                      FILE_NOT_FOUND       = 6
                      DESTINATION_EXISTS   = 7
                      UNKNOWN_ERROR        = 8
                      PATH_NOT_FOUND       = 9
                      DISK_WRITE_PROTECT   = 10
                      DRIVE_NOT_READY      = 11
                      NOT_SUPPORTED_BY_GUI = 12
                      OTHERS               = 13.
                  IF SY-SUBRC <> 0.
                    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                  ELSE.
                    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
                      EXPORTING
                        FILENAME             = LV_SOURCE
                      CHANGING
                        RC                   = LV_RC
                      EXCEPTIONS
                        FILE_DELETE_FAILED   = 1
                        CNTL_ERROR           = 2
                        ERROR_NO_GUI         = 3
                        FILE_NOT_FOUND       = 4
                        ACCESS_DENIED        = 5
                        UNKNOWN_ERROR        = 6
                        NOT_SUPPORTED_BY_GUI = 7
                        WRONG_PARAMETER      = 8
                        OTHERS               = 9.
                    IF SY-SUBRC <> 0.
                      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                    ENDIF.
                  ENDIF.
*----------------------------------------------------------*
                ELSEIF L_WA_FILES-FILENAME CS 'CHW'.

                  CLEAR: LV_SOURCE,LV_DEST.

                  CONCATENATE LV_DIR '\' L_WA_FILES-FILENAME INTO LV_SOURCE.
                  CONCATENATE LV_ARCHIVE_DIR_CCH '\' L_WA_FILES-FILENAME INTO LV_DEST.

                  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_COPY
                    EXPORTING
                      SOURCE               = LV_SOURCE
                      DESTINATION          = LV_DEST
                      OVERWRITE            = 'X'
                    EXCEPTIONS
                      CNTL_ERROR           = 1
                      ERROR_NO_GUI         = 2
                      WRONG_PARAMETER      = 3
                      DISK_FULL            = 4
                      ACCESS_DENIED        = 5
                      FILE_NOT_FOUND       = 6
                      DESTINATION_EXISTS   = 7
                      UNKNOWN_ERROR        = 8
                      PATH_NOT_FOUND       = 9
                      DISK_WRITE_PROTECT   = 10
                      DRIVE_NOT_READY      = 11
                      NOT_SUPPORTED_BY_GUI = 12
                      OTHERS               = 13.
                  IF SY-SUBRC <> 0.
                    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                  ELSE.
                    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
                      EXPORTING
                        FILENAME             = LV_SOURCE
                      CHANGING
                        RC                   = LV_RC
                      EXCEPTIONS
                        FILE_DELETE_FAILED   = 1
                        CNTL_ERROR           = 2
                        ERROR_NO_GUI         = 3
                        FILE_NOT_FOUND       = 4
                        ACCESS_DENIED        = 5
                        UNKNOWN_ERROR        = 6
                        NOT_SUPPORTED_BY_GUI = 7
                        WRONG_PARAMETER      = 8
                        OTHERS               = 9.
                    IF SY-SUBRC <> 0.
                      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                    ENDIF.
                  ENDIF.
*----------------------------------------------------------------------*
                ELSEIF L_WA_FILES-FILENAME CS 'RTGS'.

                  CLEAR: LV_SOURCE,LV_DEST.

                  CONCATENATE LV_DIR '\' L_WA_FILES-FILENAME INTO LV_SOURCE.
                  CONCATENATE LV_ARCHIVE_DIR_RTGS '\' L_WA_FILES-FILENAME  INTO LV_DEST.

                  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_COPY
                    EXPORTING
                      SOURCE               = LV_SOURCE
                      DESTINATION          = LV_DEST
                      OVERWRITE            = 'X'
                    EXCEPTIONS
                      CNTL_ERROR           = 1
                      ERROR_NO_GUI         = 2
                      WRONG_PARAMETER      = 3
                      DISK_FULL            = 4
                      ACCESS_DENIED        = 5
                      FILE_NOT_FOUND       = 6
                      DESTINATION_EXISTS   = 7
                      UNKNOWN_ERROR        = 8
                      PATH_NOT_FOUND       = 9
                      DISK_WRITE_PROTECT   = 10
                      DRIVE_NOT_READY      = 11
                      NOT_SUPPORTED_BY_GUI = 12
                      OTHERS               = 13.
                  IF SY-SUBRC <> 0.
                    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                  ELSE.
                    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
                      EXPORTING
                        FILENAME             = LV_SOURCE
                      CHANGING
                        RC                   = LV_RC
                      EXCEPTIONS
                        FILE_DELETE_FAILED   = 1
                        CNTL_ERROR           = 2
                        ERROR_NO_GUI         = 3
                        FILE_NOT_FOUND       = 4
                        ACCESS_DENIED        = 5
                        UNKNOWN_ERROR        = 6
                        NOT_SUPPORTED_BY_GUI = 7
                        WRONG_PARAMETER      = 8
                        OTHERS               = 9.
                    IF SY-SUBRC <> 0.
                      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                    ENDIF.
                  ENDIF.
*----------------------------------------------------------------------*
                ELSEIF L_WA_FILES-FILENAME CS 'DD'.

                  CLEAR: LV_SOURCE,LV_DEST.

                  CONCATENATE LV_DIR '\' L_WA_FILES-FILENAME INTO LV_SOURCE.
                  CONCATENATE LV_ARCHIVE_DIR_DD '\' L_WA_FILES-FILENAME  INTO LV_DEST.

                  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_COPY
                    EXPORTING
                      SOURCE               = LV_SOURCE
                      DESTINATION          = LV_DEST
                      OVERWRITE            = 'X'
                    EXCEPTIONS
                      CNTL_ERROR           = 1
                      ERROR_NO_GUI         = 2
                      WRONG_PARAMETER      = 3
                      DISK_FULL            = 4
                      ACCESS_DENIED        = 5
                      FILE_NOT_FOUND       = 6
                      DESTINATION_EXISTS   = 7
                      UNKNOWN_ERROR        = 8
                      PATH_NOT_FOUND       = 9
                      DISK_WRITE_PROTECT   = 10
                      DRIVE_NOT_READY      = 11
                      NOT_SUPPORTED_BY_GUI = 12
                      OTHERS               = 13.
                  IF SY-SUBRC <> 0.
                    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                  ELSE.
                    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
                      EXPORTING
                        FILENAME             = LV_SOURCE
                      CHANGING
                        RC                   = LV_RC
                      EXCEPTIONS
                        FILE_DELETE_FAILED   = 1
                        CNTL_ERROR           = 2
                        ERROR_NO_GUI         = 3
                        FILE_NOT_FOUND       = 4
                        ACCESS_DENIED        = 5
                        UNKNOWN_ERROR        = 6
                        NOT_SUPPORTED_BY_GUI = 7
                        WRONG_PARAMETER      = 8
                        OTHERS               = 9.
                    IF SY-SUBRC <> 0.
                      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                    ENDIF.
                  ENDIF.
*----------------------------------------------------------------------*
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  RCODE = 0.
ENDFORM.                    "check_files_exist

*---------------------------------------------------------------------*
*       FORM check_payment_run                                        *
*---------------------------------------------------------------------*
*  Check if payment has been run for specific date
*----------------------------------------------------------------------
FORM CHECK_PAYMENT_RUN CHANGING P_RC PA_TEST.

  DATA: L_BN(8) TYPE N,
        L_GN(5) TYPE N,
        LV_MSG(100) .

  P_RC = 0.
*------------------------------------------------*
* Test run for proposal only
*------------------------------------------------*
  IF PA_TEST IS NOT INITIAL .

    SELECT * INTO TABLE IREGUH
    FROM REGUH WHERE LAUFD = PA_LAUFD
                 AND LAUFI = PA_LAUFI
                 AND VBLNR <> ''
                 AND AVISG <> 'A'
                 AND AVISG <> 'V'.

    READ TABLE IREGUH WITH KEY XVORL = 'X'.
    IF SY-SUBRC <> 0.
      IT_ERR-VBLNR = 'General'.
      IT_ERR-ERROR = 'Payment Proposal has not been scheduled'.
      APPEND IT_ERR.
      P_RC = 1.
      EXIT.
    ELSE.
      DELETE IREGUH WHERE XVORL = ' '.
      GV_BUKRS = IREGUH-ZBUKR.
    ENDIF.

  ELSE.
*------------------------------------------------*
* Actual run for payment run
*------------------------------------------------*
    SELECT * INTO TABLE IREGUH
    FROM REGUH WHERE LAUFD = PA_LAUFD
                 AND LAUFI = PA_LAUFI
                 AND VBLNR <> ''
                 AND AVISG <> 'A'
                 AND AVISG <> 'V'.

    READ TABLE IREGUH WITH KEY XVORL = ' '.
    IF SY-SUBRC <> 0.
      IT_ERR-VBLNR = 'General'.
      IT_ERR-ERROR = 'Payment Proposal has not been scheduled'.
      APPEND IT_ERR.
      P_RC = 1.
      EXIT.
    ELSE.
      DELETE IREGUH WHERE XVORL = 'X'.
      GV_BUKRS = IREGUH-ZBUKR.
    ENDIF.
  ENDIF.

  SORT IREGUH BY VBLNR.
  DELETE ADJACENT DUPLICATES FROM IREGUH COMPARING VBLNR.
*---------------------------------------------------------------*
  SELECT *
  FROM ZDBSPR
  INTO TABLE IT_ZDBSPR.

  SORT IT_ZDBSPR BY LAUFD LAUFI.

  READ TABLE IT_ZDBSPR INTO WA_ZDBSPR
  WITH KEY LAUFD = PA_LAUFD
           LAUFI = PA_LAUFI
  BINARY SEARCH.

  IF SY-SUBRC = 0.
    SELECT SINGLE UNAME
    FROM  ZDELLUSR
    INTO  L_UNAME
    WHERE UNAME EQ L_USNAME.

    IF SY-SUBRC EQ 0.
      GV_EXIST = '1'.
      CONCATENATE 'WARNNING! Payment Run Date: '
                    PA_LAUFD 'ID:'
                   PA_LAUFI 'has already been processed.'
                   ' You are reprocessing .'
                  INTO IT_ERR-ERROR SEPARATED BY SPACE.

      MESSAGE IT_ERR-ERROR TYPE 'I'.
      CLEAR IT_ERR-ERROR.
    ELSE.
      CONCATENATE 'No Authorization to user:'(005) SY-UNAME
                    'for reprocessing.'(006)
                    INTO IT_ERR-ERROR SEPARATED BY SPACE.
      MESSAGE IT_ERR-ERROR TYPE 'I'.
      CLEAR IT_ERR-ERROR.
      LEAVE LIST-PROCESSING.

    ENDIF.
  ELSE.
    GV_EXIST ='0'.
  ENDIF.
  CLEAR L_UNAME.
  SORT IT_ZDBSPR BY ZDBSGN.
  READ TABLE IT_ZDBSPR INTO WA_ZDBSPR WITH KEY VALUT = PA_VALDT.
  IF SY-SUBRC EQ 0.
    GV_FILENUM = WA_ZDBSPR-ZDBSGN + 1.
  ELSE.
    GV_FILENUM = 1.
  ENDIF.
*--------------------------------------------------------------------*
* Prepare settlement data items table
*--------------------------------------------------------------------*
  IF NOT IREGUH IS INITIAL.

    SELECT BANKS BANKL BANKA BNKLZ STRAS ORT01 SWIFT
    FROM BNKA
    INTO TABLE IT_BNKA
    FOR ALL ENTRIES IN IREGUH
    WHERE BANKS = IREGUH-UBNKS
      AND BANKL = IREGUH-ZBNKY.

    SELECT BUKRS BUTXT
    FROM T001
    INTO TABLE IT_T001
    FOR ALL ENTRIES IN IREGUH
    WHERE BUKRS = IREGUH-ZBUKR.

    SELECT * FROM T012K
    INTO TABLE IT_T012K
    FOR ALL ENTRIES IN IREGUH
    WHERE BUKRS = IREGUH-ZBUKR
    AND   HBKID = IREGUH-HBKID
    AND   HKTID = IREGUH-HKTID.

* Prepare settlement data items table
    SELECT * FROM REGUP
    INTO TABLE IREGUP
    FOR ALL ENTRIES IN IREGUH
    WHERE LAUFD = PA_LAUFD
      AND LAUFI = PA_LAUFI
      AND XVORL = IREGUH-XVORL
      AND VBLNR <> ''
      AND ZBUKR = IREGUH-ZBUKR.

    SORT IREGUP BY VBLNR BUZEI .

    SELECT ADDRNUMBER SMTP_ADDR FROM ADR6
    INTO TABLE IT_ADR6
    FOR ALL ENTRIES IN IREGUH
    WHERE ADDRNUMBER = IREGUH-ZADNR.

    SELECT ADDRNUMBER FAX_NUMBER FROM ADRC
    INTO TABLE IT_ADRC
    FOR ALL ENTRIES IN IREGUH
    WHERE ADDRNUMBER = IREGUH-ZADNR.

    IF NOT IREGUP[] IS INITIAL.

      SELECT * FROM BSEG
      INTO TABLE IBSEG
      FOR ALL ENTRIES IN IREGUP
      WHERE BUKRS = IREGUP-BUKRS
        AND BELNR = IREGUP-BELNR
        AND GJAHR = IREGUP-GJAHR
        AND BUZEI = IREGUP-BUZEI
        AND ZLSCH = IREGUP-ZLSCH.

    ENDIF.

    SELECT  * FROM ZDBSPN
    INTO TABLE IT_ZDBSPN
    FOR ALL ENTRIES IN IREGUH
    WHERE ZLSCH = IREGUH-RZAWE
      AND LAND1 = IREGUH-LAND1
      AND WAERS = IREGUH-WAERS.

    SELECT * FROM ZDBSCP
    INTO TABLE IT_ZDBSCP
    WHERE BUKRS = IREGUH-ZBUKR.

    LOOP AT IREGUH.
      CLEAR WA_TEMP_REGUH.
      MOVE-CORRESPONDING IREGUH TO WA_TEMP_REGUH.
      WA_TEMP_REGUH-BANK_NO = IREGUH-ZBNKN.
      APPEND WA_TEMP_REGUH TO ITAB_TEMP_REGUH.
    ENDLOOP.

    SELECT BANKS BANKL BANKN IBAN FROM TIBAN
       APPENDING TABLE ITAB_TIBAN
       FOR ALL ENTRIES IN ITAB_TEMP_REGUH
       WHERE BANKS = ITAB_TEMP_REGUH-ZBNKS
       AND BANKN = ITAB_TEMP_REGUH-BANK_NO
       AND BANKL = ITAB_TEMP_REGUH-ZBNKY.
  ENDIF.
*-----------------------------------------------------*
* Retrieve one-char payment method from zdbspn
*------------------------------------------------------*
  LOOP AT IT_ZDBSPN INTO WA_ZDBSPN.
*    CASE WA_ZDBSPN-ZDBSPT.
*      WHEN 'CCH'.
*        IF WA_ZDBSPN-ZLSCH NA CHW_PM.
*          CONCATENATE CHW_PM WA_ZDBSPN-ZLSCH INTO CHW_PM.
*        ENDIF.
*      WHEN 'RTGS'.
*        IF WA_ZDBSPN-ZLSCH NA RTGS_PM.
*          CONCATENATE RTGS_PM WA_ZDBSPN-ZLSCH INTO RTGS_PM.
*        ENDIF.
*      WHEN 'NEFT'.
*        IF WA_ZDBSPN-ZLSCH NA NEFT_PM.
*          CONCATENATE NEFT_PM WA_ZDBSPN-ZLSCH INTO NEFT_PM.
*        ENDIF.
*      WHEN 'BCH'.
*        IF WA_ZDBSPN-ZLSCH NA DD_PM.
*          CONCATENATE DD_PM WA_ZDBSPN-ZLSCH INTO DD_PM.
*        ENDIF.
*    ENDCASE.

    IF WA_ZDBSPN-ZDBSPT EQ 'RTGS' OR WA_ZDBSPN-ZDBSPT EQ 'NEFT'.
      IF WA_ZDBSPN-ZLSCH NA RTGS_PM OR WA_ZDBSPN-ZLSCH NA NEFT_PM.
        CONCATENATE RTGS_PM WA_ZDBSPN-ZLSCH INTO RTGS_PM.
        CONCATENATE NEFT_PM WA_ZDBSPN-ZLSCH INTO NEFT_PM.
      ENDIF.
    ENDIF.


  ENDLOOP.

ENDFORM.                    "check_payment_run

*---------------------------------------------------------------------
*       FORM get_data                                                 *
*---------------------------------------------------------------------*
FORM PROCESS_DATA CHANGING GV_ID.

  DATA: LV_SL_NO TYPE I,
        W_MACRO(140),
        T_STATUS(1),
        T_TEMP(13),
        FIRST_XREF3_FLAG(1),
        PREV_XREF3_FLAG(1),
        PREV_XREF3 LIKE REGUP-XREF3,
        SLEN TYPE I.

  DATA: BEGIN OF IMACRO OCCURS 1.
          INCLUDE STRUCTURE ZPDETL0.
  DATA: END OF IMACRO.

  DATA: BEGIN OF IMICRO OCCURS 1.
          INCLUDE STRUCTURE ZPDETL.
  DATA: END OF IMICRO.

  DATA: BEGIN OF UTAB OCCURS 1,
          WORD(20),
        END OF UTAB.

  LOOP AT IREGUH.

    ADD 1 TO LV_SL_NO.

    GV_ID = IREGUH-LAUFI.

    PERFORM VALIDATE_DATA CHANGING GV_RC.
    IF GV_RC = '1'.
      CONTINUE.
    ENDIF.

    WA_REGUH_PM-SL_NO = LV_SL_NO.

    MOVE-CORRESPONDING IREGUH TO WA_REGUH_PM.

    APPEND WA_REGUH_PM TO IT_REGUH_PM.

  ENDLOOP.

ENDFORM.                    "process_data
*---------------------------------------------------------------------*
*       FORM save_files                                               *
*---------------------------------------------------------------------*
*       Save files
*---------------------------------------------------------------------*
FORM SAVE_FILES USING P_GV_ID.

  DATA W_ROWS_LEFT TYPE I.

  CONCATENATE GV_BUKRS '_' PA_VALDT SY-UZEIT INTO DATETIME.
  PERFORM SAVE_RTGS.
  PERFORM SAVE_NEFT USING P_GV_ID.
  PERFORM SAVE_DD CHANGING W_ROWS_LEFT.
  PERFORM SAVE_CHW CHANGING W_ROWS_LEFT.
  PERFORM PROCESS_EFT.

ENDFORM.                    "save_files
*---------------------------------------------------------------------*
*       FORM display_log                                              *
*---------------------------------------------------------------------*
*       Display data log, error log, and file creation log
*---------------------------------------------------------------------*
FORM DISPLAY_LOG.
  DATA: W_NAME(70), W_WAERS LIKE REGUH-WAERS.
  DATA : LS_STRING TYPE STRING.

  DATA: BEGIN OF GRANTAB OCCURS 0,
          RWBTR LIKE REGUH-RWBTR,
          WAERS LIKE REGUH-WAERS,
        END OF GRANTAB.

  DATA: BEGIN OF CURTAB OCCURS 0,
          LIFNR LIKE REGUH-LIFNR,
          RWBTR LIKE REGUH-RWBTR,
          WAERS LIKE REGUH-WAERS,
        END OF CURTAB.
  DATA : V_SNO(5) TYPE N.

  IF IT_LIST[] IS INITIAL AND IT_ERR[] IS INITIAL AND IT_FILIST[] IS INITIAL .

    WRITE: 'No payment documents exists to extract for the given Payment Date - ',
            PA_LAUFD,' and Payment Run ID - ',PA_LAUFI.
    EXIT.
  ENDIF.


  SORT IT_LIST BY RZAWE DESCENDING RWBTR .

  LOOP AT IT_LIST.

    W_NAME = IT_LIST-TEXT1.
    W_WAERS = IT_LIST-WAERS.
    V_SNO = V_SNO + 1.
    AT FIRST.
      FORMAT COLOR COL_KEY ON.
      WRITE:  /1 'Payment Method',  80 '',
             /1  'S.No', 11 'Employee',
             70 'Post Date', 82 'Value Date', 94 'Doc No',
            106  'Payment Amount', 129 ''.
      ULINE.
      FORMAT COLOR COL_KEY OFF.
    ENDAT.
    AT NEW RZAWE.
      FORMAT COLOR COL_GROUP ON.
      WRITE: /1 IT_LIST-RZAWE, 2 '-', 3(18) W_NAME.
      FORMAT COLOR COL_GROUP OFF.
    ENDAT.

    CLEAR CURTAB.
    CLEAR GRANTAB.
    MOVE-CORRESPONDING IT_LIST TO CURTAB.
    MOVE-CORRESPONDING IT_LIST TO GRANTAB.
    COLLECT CURTAB.
    COLLECT GRANTAB.
    WRITE:
         /1  V_SNO NO-ZERO LEFT-JUSTIFIED, 11 IT_LIST-NAME,
          70 IT_LIST-ZALDT ,
          82 PA_VALDT,
          94 IT_LIST-VBLNR,
          106  IT_LIST-RWBTR CURRENCY IT_LIST-WAERS, 125 IT_LIST-WAERS.

    AT LAST.
      WRITE: /1 ''.
      ULINE.
      WRITE: /1 ''.
      SORT GRANTAB BY WAERS.
      LOOP AT GRANTAB.
        AT FIRST.
          FORMAT COLOR COL_TOTAL ON.
          WRITE: 45 'Grand Total:'.
          FORMAT COLOR COL_TOTAL OFF.
        ENDAT.
        AT NEW WAERS.
          FORMAT COLOR COL_TOTAL ON.
          WRITE: 106 GRANTAB-RWBTR CURRENCY GRANTAB-WAERS ,
                 125(3) GRANTAB-WAERS.
          WRITE: /1 ''.
          FORMAT COLOR COL_TOTAL OFF.
        ENDAT.
      ENDLOOP.
      ULINE.
    ENDAT.

    WRITE: /1 ''.
  ENDLOOP.

  SKIP 3.
  WRITE: /25 '*** END OF PAYMENT REPORT ***'.

  NEW-PAGE.

  FORMAT COLOR COL_GROUP ON.
  WRITE:  / 'Error Report'.
  FORMAT COLOR COL_GROUP OFF.
  SKIP.
  LOOP AT IT_ERR.
    WRITE: /1 IT_ERR-VBLNR, 15(65) IT_ERR-ERROR,
           /15(35) IT_ERR-ERROR+65(35).
  ENDLOOP.

  IF NOT IT_ERR[] IS INITIAL.
    IT_ERR-VBLNR = 'Doc.No'.
    IT_ERR-ERROR = 'Error message'.
    INSERT IT_ERR INDEX 1.
    CONCATENATE PA_ERROR '_' 'DBS' SY-DATUM SY-UZEIT '.txt'
    INTO PA_ERROR.
    LS_STRING = PA_ERROR.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        FILENAME                = LS_STRING
        FILETYPE                = 'ASC'
      TABLES
        DATA_TAB                = IT_ERR
      EXCEPTIONS
        FILE_WRITE_ERROR        = 1
        NO_BATCH                = 2
        GUI_REFUSE_FILETRANSFER = 3
        INVALID_TYPE            = 4
        NO_AUTHORITY            = 5
        UNKNOWN_ERROR           = 6
        HEADER_NOT_ALLOWED      = 7
        SEPARATOR_NOT_ALLOWED   = 8
        FILESIZE_NOT_ALLOWED    = 9
        HEADER_TOO_LONG         = 10
        DP_ERROR_CREATE         = 11
        DP_ERROR_SEND           = 12
        DP_ERROR_WRITE          = 13
        UNKNOWN_DP_ERROR        = 14
        ACCESS_DENIED           = 15
        DP_OUT_OF_MEMORY        = 16
        DISK_FULL               = 17
        DP_TIMEOUT              = 18
        FILE_NOT_FOUND          = 19
        DATAPROVIDER_EXCEPTION  = 20
        CONTROL_FLUSH_ERROR     = 21
        OTHERS                  = 22.

    IF SY-SUBRC EQ 0.
      WRITE : /'Error file downloaded to ',
              / PA_ERROR .
    ELSE.
      WRITE : 'Error ! Unable to Write error file. Err type :'.
      PERFORM WRITE_ERROR USING SY-SUBRC .

    ENDIF.
    REFRESH IT_ERR.

  ENDIF.
*** Z01 ends.
  SKIP 3.
  WRITE: /25 '*** END OF ERROR REPORT ***'.

  NEW-PAGE.

  FORMAT COLOR COL_GROUP ON.
  WRITE: /1 'Files created:'.
  FORMAT COLOR COL_GROUP OFF.
  SKIP.
  LOOP AT IT_FILIST.
    WRITE: /1 IT_FILIST-OUTNAME.
  ENDLOOP.

  SKIP 3.
  WRITE: /25 '*** END OF FILE REPORT ***'.

ENDFORM.                    " display_log


*---------------------------------------------------------------------*
*       FORM update_database                                          *
*---------------------------------------------------------------------*
*       Update ZDBSPR with new Batch Number or Message Sequence Number
*       Update BSEG with assignment for GIRO
*---------------------------------------------------------------------*
FORM UPDATE_DATABASE.
  IF PA_TEST IS INITIAL.
    CLEAR ZDBSPR.
*   Update batch no/msn in ZDBSPR table
    ZDBSPR-LAUFD = IREGUH-LAUFD.
    ZDBSPR-LAUFI = IREGUH-LAUFI.
    ZDBSPR-VALUT = PA_VALDT.
    IF GV_EXIST = '1'.
      UPDATE ZDBSPR.
    ELSE.
      INSERT ZDBSPR.
    ENDIF.
  ENDIF.

ENDFORM.                    "update_database
*--------------------------------------------------------------------*
*       FORM append_rec                                         *
*---------------------------------------------------------------------*
FORM APPEND_REC USING DATEN LIKE OUTTAB.

  MOVE DATEN TO OUTTAB_PRM.
  IF V_EFT = 'Y'.
    DO .
      REPLACE '~' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '|' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '`' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '@' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.

    DO .
      REPLACE '#' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '$' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '%' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '¬' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '&' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '_' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '*' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '=' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '!' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '¢' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '¦' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '\' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE ';' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '}' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '{' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE ':' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '>' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '<' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '-' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.
    DO .
      REPLACE '"' WITH SPACE INTO OUTTAB_PRM-LINE_DTLS.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDDO.

  ENDIF.
  OUTTAB_PRM+1499 = '1'.
  APPEND OUTTAB_PRM.
  CLEAR OUTTAB_PRM.
ENDFORM.                    "append_rec

*---------------------------------------------------------------------*
*       FORM open_eft_file                                            *
*---------------------------------------------------------------------*
*       Open ET/CHW file
*---------------------------------------------------------------------*
*  -->  T_CHAR                                                        *
*---------------------------------------------------------------------*
FORM OPEN_FILE USING T_CHAR TYPE ANY.

  DATA: COUNT TYPE I,
        SCOUNT(10),
        W_OLDNUM(10).

  CASE T_CHAR.
    WHEN 'p'.
      SHIFT GV_PFILENUM LEFT DELETING LEADING SPACE.
      MOVE GV_PFNAME TO GV_PEFILE.

*     using default file name
      IF MAXCFLAG = 'X' AND GV_PEFILE = ''.
        CONCATENATE GV_PEFILE '\CHW_' DATETIME
        GV_PFILENUM '.CHW' INTO GV_PEFILE.
        MOVE GV_PFILENUM TO W_OLDNUM.
        GV_PFILENUM = GV_PFILENUM + 1.
        MAXCFLAG = ''.

*     USER-SPECIFIED FILENAME+1
      ELSEIF MAXCFLAG = 'X' AND GV_PEFILE <> ''.
        CONCATENATE GV_PFNAME '\CHW_' DATETIME
        GV_PFILENUM '.CHW' INTO GV_PEFILE.
        MOVE GV_PFILENUM TO W_OLDNUM.
        GV_PFILENUM = GV_PFILENUM + 1.
        MAXCFLAG = ''.
      ENDIF.

    WHEN 'd'.
      SHIFT GV_DFILENUM LEFT DELETING LEADING SPACE.
      MOVE GV_DFNAME TO GV_PEFILE.

*     using default file name
      IF MAXCFLAG = 'X' AND GV_PEFILE = ''.
        CONCATENATE GV_PEFILE '\DD_' DATETIME
        GV_DFILENUM '.DD' INTO GV_PEFILE.
        MOVE GV_DFILENUM TO W_OLDNUM.
        GV_DFILENUM = GV_DFILENUM + 1.
        MAXCFLAG = ''.

*     USER-SPECIFIED FILENAME+1
      ELSEIF MAXCFLAG = 'X' AND GV_PEFILE <> ''.
        CONCATENATE GV_DFNAME '\DD_' DATETIME
        GV_DFILENUM '.DD' INTO GV_PEFILE.
        MOVE GV_DFILENUM TO W_OLDNUM.
        GV_DFILENUM = GV_DFILENUM + 1.
        MAXCFLAG = ''.
      ENDIF.
  ENDCASE.

* Long File Names : replace spaces with underscores
  TRANSLATE GV_PEFILE USING ' _'.
  SHIFT GV_PEFILE RIGHT DELETING TRAILING '_'.
  SHIFT GV_PEFILE LEFT DELETING LEADING SPACE.

*  OPEN DATASET gv_pefile FOR OUTPUT IN BINARY MODE.
  IT_FILIST-FULLNAME = GV_PEFILE.
  IT_FILIST-FILENAME = GV_PEFILE.
  IT_FILIST-OUTNAME = GV_PEFILE.
  IT_FILIST-OUTPUTFOLDER = PA_EFILE.
  APPEND IT_FILIST.

ENDFORM.                    "open_eft_file
*---------------------------------------------------------------------*
*       FORM close_eft_file                                          *
*---------------------------------------------------------------------*
*       Close DD file
*---------------------------------------------------------------------*
FORM CLOSE_DD_FILE.
  DATA : LS_STRING TYPE STRING .

  LS_STRING = GV_PEFILE.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = LS_STRING
      FILETYPE                = 'ASC'
      TRUNC_TRAILING_BLANKS   = 'X'
    TABLES
      DATA_TAB                = OUTTAB_PRM
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      OTHERS                  = 22.

  IF SY-SUBRC <> 0.
    WRITE : 'Downloading file ' , GV_PEFILE , '. Error type: '.
    PERFORM WRITE_ERROR USING SY-SUBRC.
  ENDIF.
  REFRESH OUTTAB_PRM.

ENDFORM.                    "close_eft_file
*---------------------------------------------------------------------*
*       FORM write_payment_details                                    *
*---------------------------------------------------------------------*
*       Write 010 and 020 payment details to file
*---------------------------------------------------------------------*
FORM WRITE_PAYMENT_DETAILS.

  LOOP AT BUFFERTAB.
    CLEAR OUTTAB.
    OUTTAB-LINE_DTLS = BUFFERTAB-LINE_DTLS.

    PERFORM APPEND_REC USING : OUTTAB.
  ENDLOOP.

ENDFORM.                    "write_payment_details
*---------------------------------------------------------------------*
*       FORM write_batch_summary (PRIME)                   *
*---------------------------------------------------------------------*
*       Write batch summary record to file(PRIME)
*---------------------------------------------------------------------*
FORM WRITE_BATCH_SUMMARY_P.
  DATA: T_TEMP(18).

  IF SUMMARY_FLAG = ' ' AND TOTAL_TRAN <> 0.
    CLEAR P_BSRTAB.
    P_BSRTAB-REC_TYPE = '099'.
    P_BSRTAB-TOTAL_TRAN = TOTAL_TRAN.
    CLEAR T_TEMP.

    WRITE TOTAL_PAYM TO T_TEMP NO-SIGN NO-GROUPING
              DECIMALS 4 RIGHT-JUSTIFIED.
    OVERLAY T_TEMP WITH '000000000000.0000'.
    TRANSLATE T_TEMP USING ',.'.
    P_BSRTAB-TOTAL_PAYM = T_TEMP.

*   Output batch summary record
    CLEAR OUTTAB.
    OUTTAB-LINE_DTLS    = P_BSRTAB-REC_TYPE.
    OUTTAB-LINE_DTLS+3  = P_BSRTAB-TOTAL_TRAN.
    OUTTAB-LINE_DTLS+11 = P_BSRTAB-TOTAL_PAYM.
    OUTTAB-LINE_DTLS+29 = P_BSRTAB-BSR_FILLER2.

    PERFORM APPEND_REC USING : OUTTAB.

    CLEAR TOTAL_TRAN.
    CLEAR TOTAL_PAYM.
    SUMMARY_FLAG = 'X'.
  ENDIF.

ENDFORM.                               "  write_batch_summary_p.

*---------------------------------------------------------------------*
*       FORM save_Prime                                               *
*---------------------------------------------------------------------*
*       Save CHECK&GIRO  transactions (PRIME)
*---------------------------------------------------------------------*
*  -->  p_rows_left                                                   *
*---------------------------------------------------------------------*
FORM SAVE_CHW CHANGING P_ROWS_LEFT TYPE I.

  DATA: W_TEMP(20), TEMP_PAYM LIKE REGUH-RWBTR.
  DATA: W_ROWSLEFT TYPE I.
  DATA: TABLINES TYPE I.
  DATA: T_TEMP(17) TYPE C.
  DATA: SBRNCH(3).
  IF ZDBSTL-ZDBSEL = 0.   "This need to be changed for Prime
    EXIT.
  ENDIF.

  DESCRIBE TABLE PTAB LINES TABLINES.
  IF TABLINES = 0.
    EXIT.
  ENDIF.

  FILE_OPEN_FLAG = ' '.
  SUMMARY_FLAG = ' '.

* Initialize number of rows left to populate as Maximum Primeimit
  W_ROWSLEFT = ZDBSTL-ZDBSEL.

  SORT PTAB BY PAYTYP CUSREF CURRCY.

  LOOP AT PTAB.

    AT FIRST.
      MAXEFLAG = 'X'.
      MAXCFLAG = 'X'.

      PERFORM OPEN_FILE USING 'p'.
      PERFORM WRITE_PFILE_HEADER.
      FILE_OPEN_FLAG = 'X'.
      SUMMARY_FLAG = 'X'.
    ENDAT.
*---------------------------------------------*
*   Populate 010 record
*---------------------------------------------*
    CLEAR BUFFERTAB.
    BUFFERTAB-LINE_DTLS    = PTAB-RECTYP.
    BUFFERTAB-LINE_DTLS+3  = PTAB-PAYTYP.
    BUFFERTAB-LINE_DTLS+6  = PTAB-CHEC_NO. " ptab-cusref.
    BUFFERTAB-LINE_DTLS+22 = PTAB-CURRCY.
    BUFFERTAB-LINE_DTLS+25(17) = PTAB-AMOUNT.
    BUFFERTAB-LINE_DTLS+42 = PTAB-VALUT.
    BUFFERTAB-LINE_DTLS+50 = PTAB-RECVD.
    BUFFERTAB-LINE_DTLS+120 = PTAB-TEXT1.
    BUFFERTAB-LINE_DTLS+160 = PTAB-TEXT2.
    BUFFERTAB-LINE_DTLS+200 = PTAB-TEXT3.
    BUFFERTAB-LINE_DTLS+240 = PTAB-TEXT4.
    BUFFERTAB-LINE_DTLS+280 = PTAB-SCURR.
    BUFFERTAB-LINE_DTLS+283 = PTAB-DRACCT.
    BUFFERTAB-LINE_DTLS+317 = PTAB-RCACCT.
    BUFFERTAB-LINE_DTLS+359 = PTAB-BANKC.
    BUFFERTAB-LINE_DTLS+362 = PTAB-DELIND.
    BUFFERTAB-LINE_DTLS+363 = PTAB-MAILTO.
    BUFFERTAB-LINE_DTLS+503 = PTAB-POSTAL.
    BUFFERTAB-LINE_DTLS+511 = PTAB-PAYDTL.
    BUFFERTAB-LINE_DTLS+863 = PTAB-PRATLOC.
    BUFFERTAB-LINE_DTLS+879 = PTAB-DDAREF.
    BUFFERTAB-LINE_DTLS+928 = PTAB-ORIGCQ.

    IF PTAB-ORIGAMT IS INITIAL.
      T_TEMP = '000000000000.0000'.
    ELSE.
      CLEAR T_TEMP.
      WRITE PTAB-ORIGAMT TO T_TEMP
            NO-GROUPING NO-SIGN DECIMALS 4 RIGHT-JUSTIFIED.
      OVERLAY T_TEMP WITH '000000000000.0000'.

    ENDIF.

    BUFFERTAB-LINE_DTLS+938 = T_TEMP.
    BUFFERTAB-LINE_DTLS+975 = PTAB-TCODE.
    BUFFERTAB-LINE_DTLS+977 = PTAB-PARTI.
    BUFFERTAB-LINE_DTLS+989 = PTAB-ADVISE.
    BUFFERTAB-LINE_DTLS+990 = PTAB-PYBLAT.
    BUFFERTAB-LINE_DTLS+1006 = PTAB-FILLER.
    APPEND BUFFERTAB.
    BUFFER_COUNT = BUFFER_COUNT + 1.
    TEMP_PAYM = TEMP_PAYM + PTAB-AMOUNT.
*---------------------------------------------*
*   Populate 020 record
*---------------------------------------------*
    LOOP AT IT_INV1.
      IF IT_INV1-CUST_VBLNR = PTAB-CUSREF.
        CLEAR BUFFERTAB.
        BUFFERTAB-LINE_DTLS     = '020'.
        BUFFERTAB-LINE_DTLS+3   = GV_FILLER1.
        BUFFERTAB-LINE_DTLS+19  = IT_INV1-PAYM_DTLS.
        BUFFERTAB-LINE_DTLS+1420  = GV_FILLER2.
        APPEND BUFFERTAB.
*       Accumulate 020 data for batch summary record
        BUFFER_COUNT = BUFFER_COUNT + 1.
      ENDIF.
    ENDLOOP.
*---------------------------------------------------*
    IF BUFFER_COUNT > W_ROWSLEFT.
*     Wite batch summary and close current file
      PERFORM WRITE_BATCH_SUMMARY.
      PERFORM CLOSE_CHW_FILE.
      CLEAR OUTTAB. REFRESH OUTTAB.
      FILE_OPEN_FLAG = ' '.
      MAXEFLAG = 'X'.

*     Open a new CHW file and write file header
      PERFORM OPEN_FILE USING 'p'.
      FILE_OPEN_FLAG = 'X'.
      W_ROWSLEFT = ZDBSTL-ZDBSEL.
      PERFORM WRITE_PFILE_HEADER.
      IF SUMMARY_FLAG = 'X'.
        DESCRIBE TABLE BUFFERTAB2 LINES W_LINE.
        SUMMARY_FLAG = ' '.
      ENDIF.
*---------------------------------------------*
*     Write 010 and 020 records to file
*---------------------------------------------*
      PERFORM WRITE_PAYMENT_DETAILS.

      W_ROWSLEFT = W_ROWSLEFT - BUFFER_COUNT.
      TOTAL_TRAN = TOTAL_TRAN + BUFFER_COUNT.
      TOTAL_PAYM = TOTAL_PAYM + TEMP_PAYM.

      CLEAR BUFFERTAB. REFRESH BUFFERTAB.
      CLEAR BUFFERTAB2. REFRESH BUFFERTAB2.

    ELSEIF BUFFER_COUNT < W_ROWSLEFT.
      IF FILE_OPEN_FLAG = ' '.
        MAXEFLAG = 'X'.
        PERFORM OPEN_FILE USING 'p'.
        FILE_OPEN_FLAG = 'X'.
        PERFORM WRITE_PFILE_HEADER.
      ENDIF.

      IF SUMMARY_FLAG = 'X'.
        DESCRIBE TABLE BUFFERTAB2 LINES W_LINE.
        SUMMARY_FLAG = ' '.
      ENDIF.
*---------------------------------------------*
*     Write 010 and 020 records to file
*---------------------------------------------*
      PERFORM WRITE_PAYMENT_DETAILS.

      W_ROWSLEFT = W_ROWSLEFT - BUFFER_COUNT.
      TOTAL_TRAN = TOTAL_TRAN + BUFFER_COUNT.
      TOTAL_PAYM = TOTAL_PAYM + TEMP_PAYM.

      CLEAR BUFFERTAB. REFRESH BUFFERTAB.
      CLEAR BUFFERTAB2. REFRESH BUFFERTAB2.

    ELSEIF BUFFER_COUNT = W_ROWSLEFT.
      IF FILE_OPEN_FLAG = ''.
        MAXEFLAG = 'X'.

        PERFORM OPEN_FILE USING 'p'.
        FILE_OPEN_FLAG = 'X'.
        PERFORM WRITE_PFILE_HEADER.
      ENDIF.

      IF SUMMARY_FLAG = 'X'.
        DESCRIBE TABLE BUFFERTAB2 LINES W_LINE.
        SUMMARY_FLAG = ' '.
      ENDIF.
*---------------------------------------------*
*     Write 010 and 020 records to file
*---------------------------------------------*
      PERFORM WRITE_PAYMENT_DETAILS.

      TOTAL_TRAN = TOTAL_TRAN + BUFFER_COUNT.
      TOTAL_PAYM = TOTAL_PAYM + TEMP_PAYM.

      PERFORM WRITE_BATCH_SUMMARY.
      PERFORM CLOSE_CHW_FILE.
      FILE_OPEN_FLAG = ' '.

      CLEAR BUFFERTAB. REFRESH BUFFERTAB.
      W_ROWSLEFT = ZDBSTL-ZDBSEL.

      CLEAR OUTTAB. REFRESH OUTTAB.
      CLEAR BUFFERTAB2. REFRESH BUFFERTAB2.

    ENDIF.

    CLEAR BUFFER_COUNT.
    CLEAR TEMP_PAYM.

    AT LAST.
      PERFORM WRITE_BATCH_SUMMARY_P.
      PERFORM CLOSE_CHW_FILE.
    ENDAT.

  ENDLOOP.

  P_ROWS_LEFT = W_ROWSLEFT.
ENDFORM.                                         "SAVE_PRIME
*---------------------------------------------------------------------*
*       FORM save_rtgs                                                *
*---------------------------------------------------------------------*
FORM SAVE_RTGS.

  IF NOT IT_RTGS[] IS INITIAL.

    PERFORM OPEN_RTGS_FILE.

    FIELD-SYMBOLS: <LFS_ANY> TYPE ANY.
    DATA: LC_NO_OF_FIELDS TYPE I VALUE 27.

    LOOP AT IT_RTGS_NEW ASSIGNING <FS_RTGS_NEW>.

      DO LC_NO_OF_FIELDS TIMES.

        IF SY-INDEX NE 4.    "Valut field in not required in output.

          ASSIGN COMPONENT SY-INDEX OF STRUCTURE <FS_RTGS_NEW> TO <LFS_ANY>.

          REPLACE ALL OCCURRENCES OF '#' IN <LFS_ANY> WITH SPACE.

          IF SY-INDEX EQ 1.

            OUTTAB_RTGS-LINE_DTLS = <LFS_ANY>.

          ELSE.

            CONCATENATE OUTTAB_RTGS-LINE_DTLS <LFS_ANY>
            INTO OUTTAB_RTGS-LINE_DTLS
            SEPARATED BY C_CARET.

          ENDIF.

        ENDIF.

      ENDDO.

      APPEND OUTTAB_RTGS.
      CLEAR OUTTAB_RTGS.

    ENDLOOP.

    UNASSIGN: <FS_RTGS_NEW>,
              <LFS_ANY>.

    PERFORM CLOSE_RTGS_FILE.

  ENDIF.
ENDFORM.                    "SAVE_RTGS

*&---------------------------------------------------------------------*
*&      Form  save_neft
*&---------------------------------------------------------------------*
FORM SAVE_NEFT USING P_GV_ID.
  SORT IT_NEFT_DETAILS BY VALUE_DATE SEND_ACC_NUM.
  DATA: LV_BTCH_NO(5) TYPE N VALUE '50000',
       LV_CNT_DEBIT(8) TYPE N VALUE 0,
       LV_NEFT_DETAILS TYPE TY_NEFT_DETAILS,
       LV_TTL_CREDIT(22),
       LV_TTL_DEBIT(22),
       LV_AMT(19).

  LOOP AT IT_NEFT_DETAILS INTO WA_NEFT_DETAILS.

    LV_NEFT_DETAILS = WA_NEFT_DETAILS.

    ADD 1 TO LV_CNT_DEBIT.

    AT NEW SEND_ACC_NUM.
*----------------------------------------------------------------*
*** Prepare the Header Record
*----------------------------------------------------------------*
      ADD 1 TO LV_BTCH_NO.

      WA_NEFT_FINAL-RECORD+0(1) =  LV_NEFT_DETAILS-REC_TYP.
      WA_NEFT_FINAL-RECORD+1(10) = LV_NEFT_DETAILS-VALUE_DATE_FMT.
      WA_NEFT_FINAL-RECORD+11(35) = LV_NEFT_DETAILS-SEND_ACC_NUM.

      READ TABLE IT_NEFT_HDR INTO WA_NEFT_HDR
        WITH KEY VALUE_DATE = LV_NEFT_DETAILS-VALUE_DATE
                 SEND_ACC_NUM = LV_NEFT_DETAILS-SEND_ACC_NUM.

      IF SY-SUBRC EQ 0.

        WA_NEFT_FINAL-RECORD+46(35) = WA_NEFT_HDR-SNAME.
*        WA_NEFT_FINAL-RECORD+81(5) = LV_BTCH_NO.
        WA_NEFT_FINAL-RECORD+86(8) = WA_NEFT_HDR-SEND_ID.
        WA_NEFT_FINAL-RECORD+94(2220) = SPACE.

      ELSE.

        WA_NEFT_FINAL-RECORD+46(35) = SPACE.
*        WA_NEFT_FINAL-RECORD+81(5) = LV_BTCH_NO.
        WA_NEFT_FINAL-RECORD+86(8) = SPACE.
        WA_NEFT_FINAL-RECORD+94(2220) = SPACE.

      ENDIF.

      APPEND WA_NEFT_FINAL TO IT_NEFT_FINAL.
      CLEAR: WA_NEFT_FINAL,
             LV_NEFT_DETAILS.

    ENDAT.
*----------------------------------------------------------------*
*** Prepare the Detail Record
*----------------------------------------------------------------*
    WA_NEFT_FINAL-RECORD+0(1) =  C_ONE.
    WA_NEFT_FINAL-RECORD+1(20) = WA_NEFT_DETAILS-I2_TRANS_REF_NUM.
    WA_NEFT_FINAL-RECORD+29(11) = WA_NEFT_DETAILS-BEN_BNK_IFSC_CODE.
    WA_NEFT_FINAL-RECORD+40(35) = WA_NEFT_DETAILS-BEN_ACC_NO.
    WA_NEFT_FINAL-RECORD+75(50) = WA_NEFT_DETAILS-BEN_NAME.
    WA_NEFT_FINAL-RECORD+125(35) = WA_NEFT_DETAILS-BEN_ADDRS1.
    WA_NEFT_FINAL-RECORD+160(35) = WA_NEFT_DETAILS-BEN_ADDRS2.
    WA_NEFT_FINAL-RECORD+195(35) = WA_NEFT_DETAILS-BEN_ADDRS3.
    WA_NEFT_FINAL-RECORD+230(35) = WA_NEFT_DETAILS-BEN_ADDRS4.
    WA_NEFT_FINAL-RECORD+265(1) = WA_NEFT_DETAILS-RES_STATUS.
    WA_NEFT_FINAL-RECORD+266(1) = WA_NEFT_DETAILS-CITIZEN_STATUS.
    WA_NEFT_FINAL-RECORD+267(2) = WA_NEFT_DETAILS-BEN_CUST_ACC_TYP.
    WA_NEFT_FINAL-RECORD+269(2) = WA_NEFT_DETAILS-T_CODE.

    WRITE WA_NEFT_DETAILS-CHAR_AMT TO WA_NEFT_DETAILS-CHAR_AMT RIGHT-JUSTIFIED.
    WA_NEFT_FINAL-RECORD+271(19) = WA_NEFT_DETAILS-CHAR_AMT.

    OVERLAY WA_NEFT_FINAL-RECORD+271(19) WITH '0000000000000000000'.

    WA_NEFT_FINAL-RECORD+290(3) = WA_NEFT_DETAILS-BNK_CHRGS.
    WA_NEFT_FINAL-RECORD+293(16) = WA_NEFT_DETAILS-TRNC_REF.
    WA_NEFT_FINAL-RECORD+309(210) = WA_NEFT_DETAILS-PYMT_DTLS.
    WA_NEFT_FINAL-RECORD+519(10) = WA_NEFT_DETAILS-BOARD_RATE.
    WA_NEFT_FINAL-RECORD+529(10) = WA_NEFT_DETAILS-FX_CTRCT_REF.
    WA_NEFT_FINAL-RECORD+539(1) = WA_NEFT_DETAILS-DELVRY_MODE.
    WA_NEFT_FINAL-RECORD+540(60) = WA_NEFT_DETAILS-EMAIL.
    WA_NEFT_FINAL-RECORD+600(24) = WA_NEFT_DETAILS-PH_NUM.
    WA_NEFT_FINAL-RECORD+624(50) = WA_NEFT_DETAILS-ADVICE_NAME.
    WA_NEFT_FINAL-RECORD+674(35) = WA_NEFT_DETAILS-ADRS_LN1.
    WA_NEFT_FINAL-RECORD+709(35) = WA_NEFT_DETAILS-ADRS_LN2.
    WA_NEFT_FINAL-RECORD+744(35) = WA_NEFT_DETAILS-ADRS_LN3.
    WA_NEFT_FINAL-RECORD+779(35) = WA_NEFT_DETAILS-ADRS_LN4.


    WA_NEFT_FINAL+814(1500) = WA_NEFT_DETAILS-ADVICE_DTLS.

    APPEND WA_NEFT_FINAL TO IT_NEFT_FINAL.
    CLEAR WA_NEFT_FINAL.
*------------------------------------------------------------*
***   BATCH SUMMARY RECORD
*------------------------------------------------------------*
    AT END OF SEND_ACC_NUM.
*  Summation of the amount and the no:of transactions
      SUM.

      WA_NEFT_FINAL-RECORD+0(1) = C_NINE.
      WA_NEFT_FINAL-RECORD+1(8) = LV_CNT_DEBIT.

      MOVE WA_NEFT_DETAILS-AMOUNT_SUM TO GV_NEFT_CREDIT.

      WRITE GV_NEFT_CREDIT TO LV_TTL_CREDIT NO-SIGN NO-GROUPING DECIMALS 2 LEFT-JUSTIFIED.
      REPLACE '.' with ' ' into LV_TTL_CREDIT.
      CONDENSE LV_TTL_CREDIT NO-GAPS.

      WRITE LV_TTL_CREDIT TO LV_TTL_CREDIT RIGHT-JUSTIFIED.
      WA_NEFT_FINAL-RECORD+9(22) = LV_TTL_CREDIT.
      CLEAR LV_TTL_CREDIT.

      OVERLAY WA_NEFT_FINAL-RECORD+1(30) WITH '000000000000000000000000000000'.
      OVERLAY WA_NEFT_FINAL-RECORD+31(8) WITH '00000000'.
      OVERLAY WA_NEFT_FINAL-RECORD+39(22) WITH '0000000000000000000000'.

      WA_NEFT_FINAL-RECORD+61(2253) = SPACE.

      APPEND WA_NEFT_FINAL TO IT_NEFT_FINAL.

      CLEAR: LV_CNT_DEBIT,
             WA_NEFT_FINAL.

    ENDAT.
    CLEAR: TEMP_AMT,TEMP_AMT2,GV_NEFT_CREDIT,GV_NEFT_DEBIT.
  ENDLOOP.

  IF NOT IT_NEFT_FINAL IS INITIAL AND IT_ERR[] IS INITIAL.

    DATA: LV_FILENAME TYPE STRING.

    CONCATENATE PA_NEFT '\NEFT_' GV_BUKRS P_GV_ID SY-DATUM SY-UZEIT '.NEFT' INTO LV_FILENAME.

    IT_FILIST-FULLNAME = LV_FILENAME.

    PERFORM FRNTEND_DOWNLOAD TABLES IT_NEFT_FINAL USING LV_FILENAME PA_NEFT.
    REFRESH IT_NEFT_FINAL.
  ENDIF.
ENDFORM.                    " save_neft
*---------------------------------------------------------------------*
*       FORM OPEN_RTGS_FILE                                           *
*---------------------------------------------------------------------*
FORM OPEN_RTGS_FILE.
  CONCATENATE T_RTGSFILE '\RTGS_' DATETIME  '.RTGS' INTO GV_RTGSFILE.
*  CONDENSE GV_RTGSFILE.
*  TRANSLATE GV_RTGSFILE USING ' _'.
  SHIFT GV_RTGSFILE RIGHT DELETING TRAILING '_'.
  SHIFT GV_RTGSFILE LEFT DELETING LEADING SPACE.
  IT_FILIST-FULLNAME = GV_RTGSFILE.
  IT_FILIST-FILENAME = GV_RTGSFILE.
  IT_FILIST-OUTNAME  = GV_RTGSFILE.
  IT_FILIST-OUTPUTFOLDER = PA_RTGS.
  APPEND IT_FILIST.
ENDFORM.                    "OPEN_RTGS_FILE
*---------------------------------------------------------------------*
*       FORM CLOSE_RTGS_FILE                                           *
*---------------------------------------------------------------------*
FORM CLOSE_RTGS_FILE.
  DATA : LS_STRING TYPE STRING .

  LS_STRING = GV_RTGSFILE.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = LS_STRING
      FILETYPE                = 'ASC'
    TABLES
      DATA_TAB                = OUTTAB_RTGS
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      OTHERS                  = 22.

  IF SY-SUBRC <> 0.
    WRITE : 'Downloading file ' , GV_RTGSFILE , '. Error type: '.
    PERFORM WRITE_ERROR USING SY-SUBRC.
  ENDIF.
  REFRESH OUTTAB_RTGS.

ENDFORM.                    "CLOSE_RTGS_FILE
*---------------------------------------------------------------------*
*       FORM write_free_format                                        *
*---------------------------------------------------------------------*
*       Use results returned from function module
*       Z_DBS_GET_PAYMENT_DETAILS.
*       Format macro payment details in 010 record.
*       Format micro payment details in 020 records.
*---------------------------------------------------------------------*
*  -->  Preguh                                                      *
*  -->  P_iregup                                                      *
*  -->  P_IBSEG                                                       *
*  -->  P_IMICRO                                                      *
*  -->  P_IMACRO                                                      *
*---------------------------------------------------------------------*
FORM WRITE_FREE_FORMAT TABLES   P_WA_REGUH_PM STRUCTURE IREGUH
                                P_IREGUP STRUCTURE IREGUP
                                P_IBSEG  STRUCTURE IBSEG
                                P_IMICRO STRUCTURE ZPDETL
                                P_IMACRO STRUCTURE ZPDETL0.
  DATA:LV_LINES TYPE I.
  LOOP AT P_IMICRO.
    CLEAR IT_INV1.
    IT_INV1-REC_TYPE   = '020'.             " RECORD TYPE
    PERFORM FORMAT_DATE USING SY-DATUM
                        CHANGING IT_INV1-CREA_DATE.
    IT_INV1-CREA_TIME = SY-UZEIT.           " CREATION TIME
    IT_INV1-CUST_VBLNR = P_WA_REGUH_PM-VBLNR.    " CUST REF.
    LV_LINES = STRLEN( P_IMICRO ).
    IT_INV1-PAYM_DTLS = P_IMICRO+3(1400).
    APPEND IT_INV1.
  ENDLOOP.

ENDFORM.                    "write_free_format
*---------------------------------------------------------------------*
*       FORM get_address_printform                                    *
*---------------------------------------------------------------------*
*       Use standard SAP address routine to format address into 4 lines
*---------------------------------------------------------------------*
*  -->  P_REGUH                                                       *
*  -->  P_ADRS                                                        *
*---------------------------------------------------------------------*
FORM GET_ADDRESS_PRINTFORM TABLES P_ADRS STRUCTURE ADRS
                           USING  P_REGUH TYPE TY_REGUH_PM.


  DATA : V_ADRS(140) TYPE C,
         LV_ALT_PAYE1 TYPE LFA1-LNRZA,
         LV_ALT_PAYE2 TYPE LFB1-LNRZB.

  IF P_REGUH-LIFNR IS NOT INITIAL.

    SELECT SINGLE LNRZA
    FROM LFA1 INTO LV_ALT_PAYE1
    WHERE LIFNR = P_REGUH-LIFNR.

    SELECT SINGLE LNRZB
    FROM LFB1 INTO LV_ALT_PAYE2
    WHERE LIFNR = P_REGUH-LIFNR.

    IF LV_ALT_PAYE1 IS NOT INITIAL OR LV_ALT_PAYE2 IS NOT INITIAL.

      SELECT SINGLE * FROM ADRC
      WHERE ADDRNUMBER = P_REGUH-ZADNR.

      V_ADRS+0(35) = ADRC-NAME1.
      V_ADRS+35(35) = ADRC-STREET.
      V_ADRS+70(35) = ADRC-STR_SUPPL3.
      CONCATENATE ADRC-CITY1 ADRC-POST_CODE1 INTO V_ADRS+105(35) SEPARATED BY SPACE.
    ELSE.
      SELECT SINGLE * FROM ADRC
     WHERE ADDRNUMBER = P_REGUH-ADRNR.

      V_ADRS+0(35) = ADRC-NAME1.
      V_ADRS+35(35) = ADRC-STREET.
      V_ADRS+70(35) = ADRC-STR_SUPPL3.
      CONCATENATE ADRC-CITY1 ADRC-POST_CODE1 INTO V_ADRS+105(35) SEPARATED BY SPACE.
    ENDIF.

  ELSE.

    SELECT * FROM PAYR INTO TABLE IT_PAYR WHERE ZBUKR = P_REGUH-ZBUKR
                                           AND HBKID = P_REGUH-HBKID
                                           AND HKTID = P_REGUH-HKTID
                                           AND RZAWE = P_REGUH-RZAWE.

    READ TABLE IT_PAYR WITH KEY  VBLNR =  P_REGUH-VBLNR.
    V_ADRS+0(35) = IT_PAYR-ZNME1.
    V_ADRS+35(35) = IT_PAYR-ZNME2.
    V_ADRS+70(35) = IT_PAYR-ZSTRA.
    CONCATENATE IT_PAYR-ZORT1 IT_PAYR-ZPSTL INTO V_ADRS+105(35) SEPARATED BY SPACE.
  ENDIF.

  IF V_ADRS+105(35) NE SPACE.
    IF V_ADRS+70(35) EQ SPACE.
      V_ADRS+70(35) = V_ADRS+105(35).
      CLEAR V_ADRS+105(35).
    ENDIF.
  ENDIF.

  IF V_ADRS+70(35) NE SPACE.
    IF V_ADRS+35(35) EQ SPACE.
      V_ADRS+35(35) = V_ADRS+70(35).
      CLEAR V_ADRS+70(35).
    ENDIF.
  ENDIF.

  IF V_ADRS+35(35) NE SPACE.
    IF V_ADRS+0(35) EQ SPACE.
      V_ADRS+0(35) = V_ADRS+35(35).
    ENDIF.
  ENDIF.

  P_ADRS-LINE0 = V_ADRS+0(35).
  P_ADRS-LINE1 = V_ADRS+35(35).
  P_ADRS-LINE2 = V_ADRS+70(35).
  P_ADRS-LINE3 = V_ADRS+105(35).

ENDFORM.                    "get_address_printform
*&---------------------------------------------------------------------*
*&      Form  check_valid_batch_number
*&---------------------------------------------------------------------*
*       Roll over batch number and message sequence number to zero
*       when they hit maximum of 99999999 and 99999 respectively
*----------------------------------------------------------------------*
FORM CHECK_VALID_BATCH_NUMBER USING MODE TYPE ANY
                              CHANGING BN LIKE ZDBSPR-ZDBSBN
                                       GN LIKE ZDBSPR-ZDBSGN.

  IF MODE CA 'EB'.
    IF BN = 99999999.
      BN = 0.
    ENDIF.
  ENDIF.

  IF MODE CA 'GB'.
    IF GN = 99999.
      GN = 0.
    ENDIF.
  ENDIF.
*** Z01 begins.
  IF MODE = 'A'.
    IF BN = 79999.
      BN = 0.
    ENDIF.
  ENDIF.
*** Z01 ends.
ENDFORM.                    " check_valid_batch_number
***********************************************************************
* Batch Input Screen Modules
***********************************************************************
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM TYPE ANY
                      DYNPRO TYPE ANY.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.                    "bdc_dynpro

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM TYPE ANY
                     FVAL TYPE ANY.
  IF FVAL <> ' '.
    CLEAR BDCDATA.
    BDCDATA-FNAM = FNAM.
    BDCDATA-FVAL = FVAL.
    APPEND BDCDATA.
  ENDIF.
ENDFORM.                    "bdc_field
*---------------------------------------------------------------------*
*       FORM BDC_TRANSACTION                                          *
*---------------------------------------------------------------------*
*  -->  TCODE                                                         *
*---------------------------------------------------------------------*
FORM BDC_TRANSACTION USING TCODE TYPE ANY.

  CALL TRANSACTION TCODE USING BDCDATA MODE   'N'
                               MESSAGES INTO MTAB.

ENDFORM.                    "bdc_transaction
*&---------------------------------------------------------------------*
*&      Form  FORMAT_DATE
*&---------------------------------------------------------------------*
FORM FORMAT_DATE  USING   P_DATUM LIKE SY-DATUM
                CHANGING    P_CRDATE TYPE ANY.
  P_CRDATE+4(4) = P_DATUM+0(4)  .
  P_CRDATE+2(2) = P_DATUM+4(2).
  P_CRDATE+0(2) = P_DATUM+6(2).

ENDFORM.                    " FORMAT_DATE
*&---------------------------------------------------------------------*
*&      Form  validate_data
*&---------------------------------------------------------------------*
FORM VALIDATE_DATA CHANGING P_GV_RC TYPE ANY .

  CLEAR P_GV_RC .

  READ TABLE IT_ZDBSPN INTO WA_ZDBSPN
  WITH KEY ZLSCH = IREGUH-RZAWE
           LAND1 = IREGUH-LAND1.
  IF SY-SUBRC <> 0.
    READ TABLE IT_ZDBSPN INTO WA_ZDBSPN
    WITH KEY ZLSCH = IREGUH-RZAWE
             LAND1 = ' '.
    IF SY-SUBRC <> 0.
      IT_ERR-VBLNR = IREGUH-VBLNR.
      CONCATENATE 'Paymt meth' IREGUH-RZAWE 'currency'
                  IREGUH-WAERS 'to ctry' IREGUH-LAND1
                  'not maintained in zdbspn.'
                  INTO IT_ERR-ERROR SEPARATED BY SPACE.
      APPEND IT_ERR.
      P_GV_RC = '1'.
      EXIT.
    ENDIF.
  ENDIF.

  SELECT SINGLE ZDBSCI INTO ZDBSCP-ZDBSCI FROM ZDBSCP
                            WHERE BUKRS = IREGUH-ZBUKR.
  IF SY-SUBRC <> 0.
    IT_ERR-VBLNR = 'GENERAL'.
    CONCATENATE 'Company ID cannot be found for CC'
                IREGUH-ZBUKR
                INTO IT_ERR-ERROR SEPARATED BY SPACE.
    APPEND IT_ERR.
    P_GV_RC = '1'.

    EXIT.
  ENDIF.

ENDFORM.                    " validate_data
*&---------------------------------------------------------------------*
*&      Form  CHECK_FILE_LIMIT
*&---------------------------------------------------------------------*
FORM CHECK_FILE_LIMIT  CHANGING P_GV_RC TYPE ANY.
  SELECT SINGLE * FROM ZDBSTL.
  IF ZDBSTL-ZDBSGL = 0.
    IT_ERR-VBLNR = 'GENERAL'.
    IT_ERR-ERROR = 'Maximum limit on GIRO is not set in ZDBSTL.'.
    APPEND IT_ERR.
    P_GV_RC = 1.
  ENDIF.
ENDFORM.                    " CHECK_FILE_LIMIT
*&---------------------------------------------------------------------*
*&      Form  write_error
*&---------------------------------------------------------------------*
FORM WRITE_ERROR USING P_SUBRC LIKE SY-SUBRC.
  DATA LV_ERR(30).
  CLEAR LV_ERR.
  IF P_SUBRC = '1'.
    LV_ERR ='FILE_WRITE_ERROR'.
  ELSEIF P_SUBRC = '2'.
    LV_ERR ='NO_BATCH'.
  ELSEIF P_SUBRC = '3'.
    LV_ERR ='GUI_REFUSE_FILETRANSFER'.
  ELSEIF P_SUBRC = '4'.
    LV_ERR ='INVALID_TYPE'.
  ELSEIF P_SUBRC = '5'.
    LV_ERR ='NO_AUTHORITY'.
  ELSEIF P_SUBRC = '6'.
    LV_ERR ='UNKNOWN_ERROR'.
  ELSEIF P_SUBRC = '7'.
    LV_ERR ='HEADER_NOT_ALLOWED'.
  ELSEIF P_SUBRC = '8'.
    LV_ERR ='SEPARATOR_NOT_ALLOWED'.
  ELSEIF P_SUBRC = '9'.
    LV_ERR ='FILESIZE_NOT_ALLOWED'.
  ELSEIF P_SUBRC = '10'.
    LV_ERR ='HEADER_TOO_LONG '.
  ELSEIF P_SUBRC = '11'.
    LV_ERR ='DP_ERROR_CREATE'.
  ELSEIF P_SUBRC = '12'.
    LV_ERR ='DP_ERROR_SEND '.
  ELSEIF P_SUBRC = '13'.
    LV_ERR ='DP_ERROR_WRITE'.
  ELSEIF P_SUBRC = '14'.
    LV_ERR ='UNKNOWN_DP_ERROR'.
  ELSEIF P_SUBRC = '15'.
    LV_ERR ='ACCESS_DENIED'.
  ELSEIF P_SUBRC = '16'.
    LV_ERR ='DP_OUT_OF_MEMORY'.
  ELSEIF P_SUBRC = '17'.
    LV_ERR ='DISK_FULL'.
  ELSEIF P_SUBRC = '18'.
    LV_ERR ='DP_TIMEOUT'.
  ELSEIF P_SUBRC = '19'.
    LV_ERR ='FILE_NOT_FOUND'.
  ELSEIF P_SUBRC = '20'.
    LV_ERR ='DATAPROVIDER_EXCEPTION'.
  ELSEIF P_SUBRC = '21'.
    LV_ERR ='CONTROL_FLUSH_ERROR'.
  ELSEIF P_SUBRC = '22'.
    LV_ERR ='OTHERS '.
  ENDIF.

  WRITE : LV_ERR .

ENDFORM.                    " write_error
*&---------------------------------------------------------------------*
*&      Form  fill_PRIME
*&---------------------------------------------------------------------*
FORM FILL_CHW USING P_WA_REGUH_PM TYPE TY_REGUH_PM.

  DATA: W_MACRO(140),
        T_STATUS(1),
        T_TEMP(13),
        GV_LENGTH(2).

  DATA: BEGIN OF IMACRO OCCURS 1.
          INCLUDE STRUCTURE ZPDETL0.
  DATA: END OF IMACRO.

  DATA: BEGIN OF IMICRO OCCURS 1.
          INCLUDE STRUCTURE ZPDETL.
  DATA: END OF IMICRO.

  CLEAR: PTAB , GV_ENAME.
  READ TABLE IT_ZDBSPN INTO WA_ZDBSPN
  WITH KEY ZLSCH = P_WA_REGUH_PM-RZAWE
           LAND1 = P_WA_REGUH_PM-LAND1
           WAERS = P_WA_REGUH_PM-WAERS.
  GV_ENAME = WA_ZDBSPN-ZDBSMAIL.

****  Mandatory
*-----------------------------------------------*
*  1-Record Type
*-----------------------------------------------*
  PTAB-RECTYP     = '010'.
*-----------------------------------------------*
*  2-Payment Type
*-----------------------------------------------*
*  READ TABLE IT_ZDBSPN INTO WA_ZDBSPN
*  WITH KEY ZLSCH = P_WA_REGUH_PM-RZAWE
*           LAND1 = P_WA_REGUH_PM-LAND1
*           WAERS = P_WA_REGUH_PM-WAERS.
*
*  PTAB-PAYTYP = WA_ZDBSPN-ZDBSPT.

  PTAB-PAYTYP = 'CCH'.
*-----------------------------------------------*
* 3-Customer Reference
*-----------------------------------------------*
  ptab-cusref  = p_wa_reguh_pm-vblnr.
*-----------------------------------------------*
* 4-Payment Currency (CHECK-M)
*-----------------------------------------------*
  PTAB-CURRCY     = P_WA_REGUH_PM-WAERS.
  SELECT SINGLE * FROM   TCURC
            WHERE ISOCD = P_WA_REGUH_PM-WAERS.
  IF SY-SUBRC <> 0.
    IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
    CONCATENATE 'Payment Currency '
                P_WA_REGUH_PM-WAERS
                'is not maintained in ISO currency Codes.'
                INTO IT_ERR-ERROR SEPARATED BY SPACE.
    APPEND IT_ERR.
    EXIT.
  ENDIF.
*-----------------------------------------------*
*   5-Payment amount (CHECK-M)
*-----------------------------------------------*
  IF P_WA_REGUH_PM-RWBTR = 0.
  ELSE.
*    IF wa_zdbspn-zdbspt = 'BCH' OR wa_zdbspn-zdbspt = 'CCH'.
    WRITE P_WA_REGUH_PM-RWBTR TO PTAB-AMOUNT NO-SIGN NO-GROUPING
    CURRENCY P_WA_REGUH_PM-WAERS DECIMALS 4 RIGHT-JUSTIFIED.
    OVERLAY PTAB-AMOUNT WITH '000000000000.0000'.
    TRANSLATE PTAB-AMOUNT USING ',.'.
    IF PTAB-AMOUNT(2) <> '00' OR
       PTAB-AMOUNT+15(2) <> '00' .
      IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
      CONCATENATE 'Payment amount'
                   PTAB-AMOUNT
                  'is invalid. '
                  INTO IT_ERR-ERROR SEPARATED BY SPACE.
      APPEND IT_ERR.
      EXIT.
    ENDIF.
*    ENDIF.
  ENDIF.
*-----------------------------------------------*
*   6-Value Date(CHECK-M,GIRO-M)
*-----------------------------------------------*
  PTAB-VALUT    = PA_VALDT.
*-----------------------------------------------*
*   7-Receiving Party Name(CHECK-M)
*-----------------------------------------------*
  DATA:LV_PAYENAME(70).
  IF P_WA_REGUH_PM-LIFNR IS NOT INITIAL.
    IF P_WA_REGUH_PM-ZNME1 IS NOT INITIAL.
      CONCATENATE P_WA_REGUH_PM-ZNME1 P_WA_REGUH_PM-ZNME2 INTO PTAB-RECVD
                               SEPARATED BY SPACE.
    ELSE.
      CONCATENATE P_WA_REGUH_PM-NAME1 P_WA_REGUH_PM-NAME2 INTO PTAB-RECVD
                                SEPARATED BY SPACE.
    ENDIF.
  ELSE.
    SELECT * FROM PAYR INTO TABLE IT_PAYR WHERE ZBUKR = P_WA_REGUH_PM-ZBUKR
                                           AND HBKID = P_WA_REGUH_PM-HBKID
                                           AND HKTID = P_WA_REGUH_PM-HKTID
                                           AND RZAWE = P_WA_REGUH_PM-RZAWE.

    READ TABLE IT_PAYR WITH KEY  VBLNR =  P_WA_REGUH_PM-VBLNR.
    CONCATENATE IT_PAYR-ZNME1 IT_PAYR-ZNME2 INTO LV_PAYENAME SEPARATED BY SPACE.
    PTAB-RECVD = LV_PAYENAME.
  ENDIF.
  CLEAR LV_PAYENAME.

*-----------------------------------------------*
*    8-Client Text 1 - Postal Code
*-----------------------------------------------*
  IF P_WA_REGUH_PM-ZPSTL IS NOT INITIAL.
    PTAB-TEXT1 = P_WA_REGUH_PM-ZPSTL.
  ELSE.
    SELECT SINGLE * FROM ADRC
    WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
    IF SY-SUBRC EQ 0.
      PTAB-TEXT1 = ADRC-POST_CODE1.
    ENDIF.
  ENDIF.
  IF PTAB-TEXT1 IS INITIAL.
    PTAB-TEXT1 = 'XXXXXX'.
    CONDENSE PTAB-TEXT1.
  ENDIF.
*-----------------------------------------------*
*   9-Client Text 2 - Telephone or Fax
*-----------------------------------------------*
  IF P_WA_REGUH_PM-KUNNR <> ''.
    SELECT SINGLE TELF1 INTO PTAB-TEXT2 FROM KNA1
    WHERE KUNNR = P_WA_REGUH_PM-KUNNR.

  ELSEIF P_WA_REGUH_PM-LIFNR  <> ''.
    SELECT SINGLE TELF1 INTO PTAB-TEXT2 FROM LFA1
    WHERE LIFNR = P_WA_REGUH_PM-LIFNR.
  ENDIF.

  IF PTAB-TEXT2 IS INITIAL.
    IF P_WA_REGUH_PM-ZTLFX IS NOT INITIAL.
      PTAB-TEXT2 = P_WA_REGUH_PM-ZTLFX.
    ELSE.
      SELECT SINGLE * FROM ADRC
      WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
      IF SY-SUBRC EQ 0.
        PTAB-TEXT2 = ADRC-FAX_NUMBER.
      ENDIF.
    ENDIF.
  ENDIF.
*-----------------------------------------------*
*   10-Client Text 3 - Vendor Code
*-----------------------------------------------*
  IF P_WA_REGUH_PM-KUNNR <> ''.
    PTAB-TEXT3 = P_WA_REGUH_PM-KUNNR.
  ELSEIF P_WA_REGUH_PM-LIFNR  <> ''.
    PTAB-TEXT3 = P_WA_REGUH_PM-LIFNR.
  ENDIF.
*-----------------------------------------------*
*    11-Client Text 4
*-----------------------------------------------*
  PTAB-TEXT4 = PTAB-CUSREF.
*-----------------------------------------------*
*   12-Settlement Currency (CHECK-M)
*-----------------------------------------------*
  READ TABLE IT_T012K WITH KEY BUKRS = P_WA_REGUH_PM-ZBUKR
                               HBKID = P_WA_REGUH_PM-HBKID
                               HKTID = P_WA_REGUH_PM-HKTID.
  PTAB-SCURR   = IT_T012K-WAERS.

  SELECT SINGLE * FROM   TCURC
          WHERE ISOCD = IT_T012K-WAERS.
  IF SY-SUBRC <> 0.
    IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
    CONCATENATE 'Settlement Currency '
                 IT_T012K-WAERS
                'is not maintained in ISO currency Codes.'
                 INTO IT_ERR-ERROR SEPARATED BY SPACE.
    APPEND IT_ERR.
    EXIT.
  ENDIF.
*-----------------------------------------------*
*  13-Debit Account(CHECK-M)
*-----------------------------------------------*
  READ TABLE IT_T012K WITH KEY BUKRS = P_WA_REGUH_PM-ZBUKR
                             HBKID = P_WA_REGUH_PM-HBKID
                             HKTID = P_WA_REGUH_PM-HKTID.
  IF SY-SUBRC = 0.

    CLEAR UTAB. REFRESH UTAB.
    SPLIT P_WA_REGUH_PM-UBKNT AT '-' INTO TABLE UTAB.
    LOOP AT UTAB.
      CONCATENATE PTAB-DRACCT UTAB-WORD INTO PTAB-DRACCT.
    ENDLOOP.

    IF  PTAB-DRACCT = ''.
      IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
      CONCATENATE 'Settlement A/C Number for'
                  P_WA_REGUH_PM-NAME1
                  'is not maintained in settlement data.'
                  INTO IT_ERR-ERROR SEPARATED BY SPACE.
      APPEND IT_ERR.
      EXIT.
    ENDIF.
    IF PTAB-DRACCT CS 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
      IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
      CONCATENATE 'Dbit Account'
                  PTAB-DRACCT
                  'contains non-numeric values.'
                  INTO IT_ERR-ERROR SEPARATED BY SPACE.
      APPEND IT_ERR.
      EXIT.
    ENDIF.
  ENDIF.
*-----------------------------------------------*
*  14-Recieving Party Account (CHECK-NA)
*-----------------------------------------------*
*-----------------------------------------------*
*  15-Receiving Bank Number(CHECK-NA)
*-----------------------------------------------*
*-----------------------------------------------*
*  16-Receiving Branch(CHECK-NA)
*-----------------------------------------------*
*-----------------------------------------------*
*    17-Bank Charges.(CHECK-O)
*-----------------------------------------------*
  PTAB-BANKC = 'OUR'.
*-----------------------------------------------*
*    18-Delivery Indicator
*-----------------------------------------------*
  READ TABLE IT_ZDBSPN INTO WA_ZDBSPN
  WITH KEY ZLSCH = P_WA_REGUH_PM-RZAWE
           LAND1 = P_WA_REGUH_PM-ZLAND
           WAERS = P_WA_REGUH_PM-WAERS.

  IF SY-SUBRC EQ 0.
    PTAB-DELIND  = WA_ZDBSPN-ZDBSDM.
  ENDIF.
*  ptab-delind = 'M'.
*-----------------------------------------------*
*    19-'Mail to' Party name and address (CHECK-O)
*-----------------------------------------------*
  PERFORM GET_ADDRESS_PRINTFORM TABLES ADRS USING P_WA_REGUH_PM.
  GV_LEN1 = '30'.
  GV_LEN2 = '32'.
  IF PTAB-MAILTO IS INITIAL.

    PTAB-MAILTO = ADRS-LINE0.
    IF PTAB-MAILTO+0(35) IS INITIAL.
      PTAB-MAILTO+0(35) = 'INDOFIL'.
    ENDIF.
    PERFORM REPLACE_CHARS USING PTAB-MAILTO+0(35) GV_LEN1 GV_LEN2 CHANGING
    PTAB-MAILTO+0(35).

    PTAB-MAILTO+35 = ADRS-LINE1.
    IF PTAB-MAILTO+35 IS INITIAL.
      PTAB-MAILTO+35 = 'XXXXX'.
    ENDIF.
    PERFORM REPLACE_CHARS USING PTAB-MAILTO+35(35) GV_LEN1 GV_LEN2 CHANGING
PTAB-MAILTO+35(35).

    PTAB-MAILTO+70 = ADRS-LINE2.
    IF  PTAB-MAILTO+70 IS INITIAL.
      PTAB-MAILTO+70 = 'XXXXXX'.
    ENDIF.
    PERFORM REPLACE_CHARS USING PTAB-MAILTO+70(35) GV_LEN1 GV_LEN2 CHANGING
PTAB-MAILTO+70(35).

    PTAB-MAILTO+105 = ADRS-LINE3.
    IF  PTAB-MAILTO+105 IS INITIAL.
      PTAB-MAILTO+105 = 'XXXXXX'.
    ENDIF.
    PERFORM REPLACE_CHARS USING PTAB-MAILTO+105(35) GV_LEN1 GV_LEN2 CHANGING
PTAB-MAILTO+105(35).
    CLEAR:GV_LEN1,GV_LEN2.

  ENDIF.
*-----------------------------------------------*
*    20-Local Postal Code (CHECK-O)
*-----------------------------------------------*
  IF IREGUH-ZPSTL IS NOT INITIAL.
    PTAB-POSTAL = IREGUH-ZPSTL.
  ELSE.
    SELECT SINGLE * FROM ADRC
    WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
    IF SY-SUBRC EQ 0.
      PTAB-POSTAL = ADRC-POST_CODE1.
    ENDIF.
  ENDIF.
  CONDENSE PTAB-POSTAL.
  IF PTAB-POSTAL IS INITIAL.
    PTAB-POSTAL = 'XXXXXX'.
    CONDENSE PTAB-POSTAL.
  ENDIF.
*-----------------------------------------------*
*   21-Payment Details (CHECK-O,GIRO-O)
*-----------------------------------------------*
  REFRESH IREGUP2.   "sri
  MOVE-CORRESPONDING P_WA_REGUH_PM TO IREGUH2.
  APPEND IREGUH2.
  LOOP AT IREGUP WHERE LAUFD = P_WA_REGUH_PM-LAUFD
                   AND LAUFI = P_WA_REGUH_PM-LAUFI
                   AND XVORL = P_WA_REGUH_PM-XVORL
                   AND VBLNR = P_WA_REGUH_PM-VBLNR.
    MOVE-CORRESPONDING IREGUP TO IREGUP2.
    APPEND IREGUP2.
    READ TABLE IBSEG WITH KEY BELNR = IREGUP-BELNR
                              GJAHR = IREGUP-GJAHR
                              BUZEI = IREGUP-BUZEI.
    MOVE-CORRESPONDING IBSEG TO IBSEG2.
    APPEND IBSEG2.
  ENDLOOP.

  CLEAR: IMICRO, IMACRO.
  REFRESH: IMICRO, IMACRO.

  CALL FUNCTION 'Z_DBS_GET_PAYMENT_DETAILS_INDO'
    EXPORTING
      I_REGUH   = IREGUH2
      I_DD_PM   = DD_PM
      I_CHW_PM  = CHW_PM
      I_NEFT_PM = NEFT_PM
      I_RTGS_PM = RTGS_PM
    TABLES
      I_REGUP   = IREGUP2
      E_MICRO   = IMICRO
      I_BSEG    = IBSEG                                     "sriibseg2
      E_MACRO   = IMACRO.
*-----------------------------------------------*
*     Payment details + SGTXT are concatenated into micro
*-----------------------------------------------*
  PERFORM WRITE_FREE_FORMAT TABLES IREGUH2 IREGUP2
                                   IBSEG2 IMICRO IMACRO.

  CLEAR IT_LIST.
  MOVE-CORRESPONDING IREGUH2 TO IT_LIST.
  WRITE IREGUH2-RWBTR TO T_TEMP NO-SIGN NO-GROUPING
      CURRENCY IREGUH-WAERS DECIMALS 0  RIGHT-JUSTIFIED.
  OVERLAY T_TEMP WITH '000000000000000000'.
  TRANSLATE T_TEMP USING ',.'.

  IT_LIST-RWBTR = IREGUH2-RWBTR.

  CONCATENATE IREGUH2-ZNME1 IREGUH2-ZNME2 INTO IT_LIST-NAME
     SEPARATED BY SPACE.
  SELECT SINGLE LAND1 INTO T001-LAND1 FROM T001
      WHERE BUKRS = IREGUH2-ZBUKR.
  SELECT SINGLE TEXT1 INTO IT_LIST-TEXT1
                  FROM T042Z WHERE LAND1 = T001-LAND1
                               AND ZLSCH = IREGUH2-RZAWE.
  IT_LIST-RZAWE = IREGUH2-RZAWE.
  MOVE-CORRESPONDING IREGUP2 TO IT_LIST.
  APPEND IT_LIST.
*-----------------------------------------------*
*   24-Print at location
*-----------------------------------------------*
  PTAB-PRATLOC = '0811'.
*-----------------------------------------------*
*   25  DDA Reference  (CHECK-NA)
*-----------------------------------------------*
*-----------------------------------------------*
*   26 Original Settlement CCY (*,GIRO-NA)
*-----------------------------------------------*
* 27 Original Debit Account No.(*,GIRO-NA)
* 28 Original Cheque No.(*,GIRO-NA)
  PTAB-ORIGCQ = '0000000000'.
* OVERLAY ptab-origcq WITH '0000000000'.
* 29 Original Payment Amount.(*,GIRO-NA)
*-----------------------------------------------*
* 30 Originating Name  (CHECK-NA)
*-----------------------------------------------*
*-----------------------------------------------*
*  31 GIRO Transaction Code (CHECK-@,GIRO-M)
*-----------------------------------------------*
  PTAB-TCODE = '00'.
*-----------------------------------------------*
*  33-Send Beneficiary Advise By (CHECK-NA)
*-----------------------------------------------*
*-----------------------------------------------*
* 34 Payable at location
*-----------------------------------------------*
  IF WA_ZDBSPN-ZDBSPT = 'CCH' .
    PTAB-PYBLAT = 'DBSMUM' .
  ELSEIF WA_ZDBSPN-ZDBSPT = 'BCH' .
    IF P_WA_REGUH_PM-ZORT1 = 'Bangalore'
    OR P_WA_REGUH_PM-ZORT1 = 'Calcutta'
    OR P_WA_REGUH_PM-ZORT1 = 'Chennai'
    OR P_WA_REGUH_PM-ZORT1 = 'Delhi'
    OR P_WA_REGUH_PM-ZORT1 = 'Moradabad'
    OR P_WA_REGUH_PM-ZORT1 = 'Mumbai'
    OR P_WA_REGUH_PM-ZORT1 = 'Nasik'
    OR P_WA_REGUH_PM-ZORT1 = 'Pune'
    OR P_WA_REGUH_PM-ZORT1 = 'Salem'
    OR P_WA_REGUH_PM-ZORT1 = 'Surat'.
      PTAB-PYBLAT = 'DBSMUM' .
    ELSE.
      PTAB-PYBLAT = 'HDFMUM'.
    ENDIF.
  ENDIF.
*------------------------------------------------*
  REFRESH IT_PAYR.
  SELECT * FROM PAYR INTO TABLE IT_PAYR WHERE ZBUKR = P_WA_REGUH_PM-ZBUKR
                                          AND HBKID = P_WA_REGUH_PM-HBKID
                                          AND HKTID = P_WA_REGUH_PM-HKTID
                                          AND RZAWE = P_WA_REGUH_PM-RZAWE.

  READ TABLE IT_PAYR WITH KEY  VBLNR =  P_WA_REGUH_PM-VBLNR.
  PTAB-CHEC_NO = IT_PAYR-CHECT.

  IF IT_PAYR-VOIDR NE 0.

    WRITE '0' TO PTAB-AMOUNT NO-SIGN NO-GROUPING CURRENCY
         P_WA_REGUH_PM-WAERS DECIMALS 4 RIGHT-JUSTIFIED.
    OVERLAY PTAB-AMOUNT WITH '000000000001.0000'.
    TRANSLATE PTAB-AMOUNT USING ',.'.

    WRITE TEXT-017 TO PTAB-RECVD.
    WRITE TEXT-017 TO PTAB-MAILTO.
  ENDIF.
*-----------------------------------------------*
* 21 23 Fax no (GIRO - PRIME)
*-----------------------------------------------*
  IF P_WA_REGUH_PM-ZTLFX IS NOT INITIAL.
    PTAB-PAYDTL = P_WA_REGUH_PM-ZTLFX .
  ELSE.
    SELECT SINGLE * FROM ADRC
  WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
    IF SY-SUBRC EQ 0.
      PTAB-PAYDTL = ADRC-FAX_NUMBER.
    ENDIF.
  ENDIF.
  IF PTAB-PAYDTL IS INITIAL.
    PTAB-PAYDTL = 'XXXXXX'.
    CONDENSE PTAB-PAYDTL.
  ENDIF.
*-----------------------------------------------*
* 22 Email address
*-----------------------------------------------*
  IF NOT P_WA_REGUH_PM-ZADNR IS INITIAL AND P_WA_REGUH_PM-ADRNR IS NOT INITIAL.
    SELECT SINGLE SMTP_ADDR
    INTO PTAB-BEMAIL
    FROM ADR6
    WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
    CONDENSE PTAB-BEMAIL NO-GAPS.

  ELSEIF NOT P_WA_REGUH_PM-ZADNR IS INITIAL AND P_WA_REGUH_PM-ADRNR IS INITIAL.
    SELECT SINGLE SMTP_ADDR
    INTO PTAB-BEMAIL
    FROM ADR6
    WHERE ADDRNUMBER = P_WA_REGUH_PM-ZADNR.
    CONDENSE PTAB-BEMAIL NO-GAPS.

  ELSEIF P_WA_REGUH_PM-ZADNR IS INITIAL AND P_WA_REGUH_PM-ADRNR IS NOT INITIAL.
    SELECT SINGLE SMTP_ADDR
   INTO PTAB-BEMAIL
   FROM ADR6
   WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
    CONDENSE PTAB-BEMAIL NO-GAPS.

  ELSEIF P_WA_REGUH_PM-ZADNR IS INITIAL AND P_WA_REGUH_PM-ADRNR IS INITIAL.

    SELECT SINGLE TLFXS
    INTO PTAB-BEMAIL FROM LFB1
    WHERE LIFNR = P_WA_REGUH_PM-LIFNR
    AND   BUKRS = P_WA_REGUH_PM-ZBUKR.
    CONDENSE PTAB-BEMAIL NO-GAPS.

  ENDIF.

  SEARCH PTAB-BEMAIL FOR GVV_AT.
  IF SY-SUBRC EQ 0.
    SPLIT PTAB-BEMAIL AT '@' INTO GV_P1 GV_P2.
    CONDENSE GV_P2.
    IF GV_P2 CA GVV_DOT.
      GV_LENTH = STRLEN( GV_P2 ).
      GV_POS = GV_LENTH - 1.
      IF GV_P2+GV_POS(1) EQ '.'.
        PTAB-BEMAIL = GV_ENAME.
        CONDENSE PTAB-BEMAIL NO-GAPS.
      ENDIF.
    ELSE.
      PTAB-BEMAIL = GV_ENAME.
      CONDENSE PTAB-BEMAIL NO-GAPS.
    ENDIF.
*       do nothing
  ELSE.
    PTAB-BEMAIL = GV_ENAME.
    CONDENSE PTAB-BEMAIL NO-GAPS.
  ENDIF.

  IF PTAB-BEMAIL IS INITIAL.
    MOVE GV_ENAME TO PTAB-BEMAIL.
    CONDENSE PTAB-BEMAIL NO-GAPS.
  ENDIF.

  CLEAR:GV_POS,GV_LENTH,GV_P1,GV_P2.
*-----------------------------------------------*
*-----------------------------------------------*
* 20 Local Postal Code (CHECK-O,GIRO-O)
*-----------------------------------------------*
  IF P_WA_REGUH_PM-ZPSTL IS NOT INITIAL.
    PTAB-POSTAL = P_WA_REGUH_PM-ZPSTL.
  ELSE.
    SELECT SINGLE * FROM ADRC
    WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
    IF SY-SUBRC EQ 0.
      PTAB-POSTAL = ADRC-POST_CODE1.
    ENDIF.
  ENDIF.

  CONDENSE PTAB-POSTAL.

  IF PTAB-POSTAL IS INITIAL.
    PTAB-POSTAL = 'XXXXXX'.
    CONDENSE PTAB-POSTAL.
  ENDIF.

  APPEND PTAB.

ENDFORM.                    " fill_PRIME
*&---------------------------------------------------------------------*
*&      Form  fill_neft
*&---------------------------------------------------------------------*
FORM FILL_NEFT USING P_WA_REGUH_PM TYPE TY_REGUH_PM.

  DATA: BEGIN OF IMACRO OCCURS 1.
          INCLUDE STRUCTURE ZPDETL0.
  DATA: END OF IMACRO.

  DATA: BEGIN OF IMICRO OCCURS 1.
          INCLUDE STRUCTURE ZPDETL.
  DATA: END OF IMICRO.

  DATA:T_TEMP(13),LV_TEMP TYPE STRING.

  CLEAR: WA_NEFT_DETAILS , GV_ENAME.

  READ TABLE IT_ZDBSPN INTO WA_ZDBSPN
  WITH KEY ZLSCH = P_WA_REGUH_PM-RZAWE
           LAND1 = P_WA_REGUH_PM-LAND1
           WAERS = P_WA_REGUH_PM-WAERS.
  GV_ENAME = WA_ZDBSPN-ZDBSMAIL.

*----------------------------------------------------------------------*
*** Record Type
*----------------------------------------------------------------------*
  WA_NEFT_DETAILS-REC_TYP = C_ZERO.
*----------------------------------------------------------------------*
*** Value Date
*----------------------------------------------------------------------*
  WA_NEFT_DETAILS-VALUE_DATE = PA_VALDT.

  CONCATENATE PA_VALDT+0(4) PA_VALDT+4(2) PA_VALDT+6(2)
              INTO WA_NEFT_DETAILS-VALUE_DATE_FMT.
*----------------------------------------------------------------------*
*** Sender Account Number
*----------------------------------------------------------------------*
  CLEAR UTAB. REFRESH UTAB.
  SPLIT P_WA_REGUH_PM-UBKNT AT '-' INTO TABLE UTAB.
  LOOP AT UTAB.
    CONCATENATE WA_NEFT_DETAILS-SEND_ACC_NUM UTAB-WORD
           INTO WA_NEFT_DETAILS-SEND_ACC_NUM.
  ENDLOOP.

  PERFORM CHK_N_UPDATE_ERROR_LOG
  USING  WA_NEFT_DETAILS-SEND_ACC_NUM
         P_WA_REGUH_PM-VBLNR
         'Sender Account number cannot be blank'.
*----------------------------------------------------------------------*
*** Header Details
*----------------------------------------------------------------------*
  READ TABLE IT_NEFT_HDR INTO WA_NEFT_HDR
    WITH KEY VALUE_DATE = WA_NEFT_DETAILS-VALUE_DATE
             SEND_ACC_NUM = WA_NEFT_DETAILS-SEND_ACC_NUM.

  IF SY-SUBRC NE 0.

    CLEAR WA_NEFT_HDR.
    WA_NEFT_HDR-VALUE_DATE = WA_NEFT_DETAILS-VALUE_DATE.
    WA_NEFT_HDR-SEND_ACC_NUM = WA_NEFT_DETAILS-SEND_ACC_NUM.
*----------------------------------------------------------------------*
*** Sender ID
*----------------------------------------------------------------------*
    PERFORM  GET_COMPANY_ID USING P_WA_REGUH_PM-ZBUKR
                            CHANGING WA_NEFT_HDR-SEND_ID.

    READ TABLE IT_ZDBSCP ASSIGNING <FS_ZDBSCP>
       WITH KEY BUKRS = P_WA_REGUH_PM-ZBUKR.

    IF SY-SUBRC EQ 0.
      WA_NEFT_HDR-SEND_ID = <FS_ZDBSCP>-ZDBSCI.
    ENDIF.
*----------------------------------------------------------------------*
*** Sender Name
*----------------------------------------------------------------------*
    READ TABLE IT_T001 INTO WA_T001
    WITH KEY BUKRS = P_WA_REGUH_PM-ZBUKR.

    IF  SY-SUBRC EQ 0.
      WA_NEFT_HDR-SNAME = WA_T001-BUTXT.
    ENDIF.

    GV_LEN1 = '30'.
    GV_LEN2 = '32'.
    PERFORM REPLACE_CHARS USING WA_NEFT_HDR-SNAME GV_LEN1 GV_LEN2 CHANGING
    WA_NEFT_HDR-SNAME.
    CLEAR:GV_LEN1,GV_LEN2.

    APPEND WA_NEFT_HDR TO IT_NEFT_HDR.
    CLEAR WA_NEFT_HDR.

  ENDIF.
*----------------------------------------------------------------------*
*** End of Header Details
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*** IDEAL Transaction Reference No.
*----------------------------------------------------------------------*
*  wa_neft_details-i2_trans_ref_num = p_wa_reguh_pm-vblnr.
*----------------------------------------------------------------------*
*** IFSC Code
*----------------------------------------------------------------------*
*  wa_neft_details-ben_bnk_ifsc_code = p_wa_reguh_pm-zswif.

  WA_NEFT_DETAILS-BEN_BNK_IFSC_CODE = P_WA_REGUH_PM-ZBNKY+0(11).
  REPLACE 'O' WITH '0' INTO WA_NEFT_DETAILS-BEN_BNK_IFSC_CODE.

  CONDENSE WA_NEFT_DETAILS-BEN_BNK_IFSC_CODE NO-GAPS.
*----------------------------------------------------------------------*
*** Beneficiary Account Number
*----------------------------------------------------------------------*
  WA_NEFT_DETAILS-BEN_ACC_NO = P_WA_REGUH_PM-ZBNKN.

  PERFORM CHK_N_UPDATE_ERROR_LOG
  USING  WA_NEFT_DETAILS-BEN_ACC_NO
         P_WA_REGUH_PM-VBLNR
         'Receiving Account number cannot be blank'.
*----------------------------------------------------------------------*
*** Beneficiary Name
*----------------------------------------------------------------------*
  IF P_WA_REGUH_PM-ZNME1 IS NOT INITIAL.
    CONCATENATE P_WA_REGUH_PM-ZNME1 P_WA_REGUH_PM-ZNME2 INTO WA_NEFT_DETAILS-BEN_NAME+0(50)
    SEPARATED BY SPACE.
  ELSE.
    CONCATENATE P_WA_REGUH_PM-NAME1 P_WA_REGUH_PM-NAME2 INTO WA_NEFT_DETAILS-BEN_NAME+0(50)
   SEPARATED BY SPACE.
  ENDIF.

  GV_LEN1 = '45'.
  GV_LEN2 = '47'.
  PERFORM REPLACE_CHARS USING WA_NEFT_DETAILS-BEN_NAME GV_LEN1 GV_LEN2 CHANGING
WA_NEFT_DETAILS-BEN_NAME.

  PERFORM CHK_N_UPDATE_ERROR_LOG
   USING  WA_NEFT_DETAILS-BEN_NAME+0(50)
          P_WA_REGUH_PM-VBLNR
          'Receiver Name cannot be blank'.
  CLEAR:GV_LEN1,GV_LEN2.
*----------------------------------------------------------------------*
*** Beneficiary Address
*----------------------------------------------------------------------*
  PERFORM GET_ADDRESS_PRINTFORM TABLES ADRS USING P_WA_REGUH_PM.
  WA_NEFT_DETAILS-BEN_ADDRS1 = ADRS-LINE0.
  WA_NEFT_DETAILS-BEN_ADDRS2 = ADRS-LINE1.
  WA_NEFT_DETAILS-BEN_ADDRS3 = ADRS-LINE2.
  WA_NEFT_DETAILS-BEN_ADDRS4 = ADRS-LINE3.

  GV_LEN1 = '30'.
  GV_LEN2 = '32'.

  PERFORM REPLACE_CHARS USING WA_NEFT_DETAILS-BEN_ADDRS1 GV_LEN1 GV_LEN2
  CHANGING WA_NEFT_DETAILS-BEN_ADDRS1.

  PERFORM REPLACE_CHARS USING WA_NEFT_DETAILS-BEN_ADDRS2 GV_LEN1 GV_LEN2
  CHANGING WA_NEFT_DETAILS-BEN_ADDRS2.

  PERFORM REPLACE_CHARS USING WA_NEFT_DETAILS-BEN_ADDRS3 GV_LEN1 GV_LEN2
  CHANGING WA_NEFT_DETAILS-BEN_ADDRS3.

  PERFORM CHK_N_UPDATE_ERROR_LOG
    USING  WA_NEFT_DETAILS-BEN_ADDRS1
           P_WA_REGUH_PM-VBLNR
           'Benificiary Bank Address line1 cannot be blank'.
  CLEAR:GV_LEN1,GV_LEN2.
*----------------------------------------------------------------------*
*** Resident & Citizen Status (NA-NEFT)
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*** Transaction Code
*----------------------------------------------------------------------*
  WA_NEFT_DETAILS-T_CODE = C_TWENTY.
*----------------------------------------------------------------------*
*** Payment amount
*----------------------------------------------------------------------*
  MOVE P_WA_REGUH_PM-RWBTR TO WA_NEFT_DETAILS-AMOUNT.

  WRITE WA_NEFT_DETAILS-AMOUNT TO GV_CHAR2 NO-SIGN NO-GROUPING DECIMALS 2 LEFT-JUSTIFIED.
  REPLACE '.' WITH '' INTO GV_CHAR2.
  CONDENSE GV_CHAR2 NO-GAPS.

  WA_NEFT_DETAILS-CHAR_AMT = GV_CHAR2 .
  CLEAR GV_CHAR2.

  WA_NEFT_DETAILS-AMOUNT_SUM = P_WA_REGUH_PM-RWBTR.

  PERFORM CHK_N_UPDATE_ERROR_LOG
   USING WA_NEFT_DETAILS-AMOUNT
         P_WA_REGUH_PM-VBLNR
         'Payment Amount cannot be blank'.
*----------------------------------------------------------------------*
*** Bank Charges
*----------------------------------------------------------------------*
*  wa_neft_details-bnk_chrgs = 'OUR'.
* Common for all the Payment Methods
*----------------------------------------------------------------------*
*** Transaction Reference
*----------------------------------------------------------------------*
  WA_NEFT_DETAILS-TRNC_REF = P_WA_REGUH_PM-VBLNR.
*----------------------------------------------------------------------*
*** Payment Details
*----------------------------------------------------------------------*
  WA_NEFT_DETAILS-PYMT_DTLS = P_WA_REGUH_PM-VBLNR.
  CONDENSE WA_NEFT_DETAILS-PYMT_DTLS.

  GV_LEN1 = '205'.
  GV_LEN2 = '207'.

  PERFORM REPLACE_CHARS USING WA_NEFT_DETAILS-PYMT_DTLS GV_LEN1 GV_LEN2
  CHANGING WA_NEFT_DETAILS-PYMT_DTLS.

  CLEAR: LV_TEMP,GV_LEN1,GV_LEN2,WA_NEFTDTLS.
*----------------------------------------------------------------------*
*Board Rate - Not Applicable
*FX Contract Reference - Not Applicable
*----------------------------------------------------------------------*
*Need to identify the logic to fill in the below details
*Delivery Mode
*----------------------------------------------------------------------*
  READ TABLE IT_ZDBSPN INTO WA_ZDBSPN
  WITH KEY ZLSCH = P_WA_REGUH_PM-RZAWE
           LAND1 = P_WA_REGUH_PM-ZLAND
           WAERS = P_WA_REGUH_PM-WAERS.

  IF SY-SUBRC EQ 0.
    WA_NEFT_DETAILS-DELVRY_MODE = WA_ZDBSPN-ZDBSDM.
  ENDIF.
*----------------------------------------------------------------------*
*Email and Phone Number
*----------------------------------------------------------------------*
  CASE WA_NEFT_DETAILS-DELVRY_MODE.

    WHEN 'E'.
      IF NOT P_WA_REGUH_PM-ZADNR IS INITIAL AND P_WA_REGUH_PM-ADRNR IS NOT INITIAL.
        SELECT SINGLE SMTP_ADDR
        INTO WA_NEFT_DETAILS-EMAIL
        FROM ADR6
        WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
        CONDENSE WA_NEFT_DETAILS-EMAIL NO-GAPS.

      ELSEIF NOT P_WA_REGUH_PM-ZADNR IS INITIAL AND P_WA_REGUH_PM-ADRNR IS INITIAL.

        SELECT SINGLE SMTP_ADDR
        INTO WA_NEFT_DETAILS-EMAIL
        FROM ADR6
        WHERE ADDRNUMBER = P_WA_REGUH_PM-ZADNR.
        CONDENSE WA_NEFT_DETAILS-EMAIL NO-GAPS.

      ELSEIF P_WA_REGUH_PM-ZADNR IS INITIAL AND P_WA_REGUH_PM-ADRNR IS NOT INITIAL.

        SELECT SINGLE SMTP_ADDR
        INTO WA_NEFT_DETAILS-EMAIL
        FROM ADR6
        WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
        CONDENSE WA_NEFT_DETAILS-EMAIL NO-GAPS.

      ELSEIF P_WA_REGUH_PM-ZADNR IS INITIAL AND P_WA_REGUH_PM-ADRNR IS INITIAL.

        SELECT SINGLE TLFXS
        INTO WA_NEFT_DETAILS-EMAIL FROM LFB1
        WHERE LIFNR = P_WA_REGUH_PM-LIFNR
        AND   BUKRS = P_WA_REGUH_PM-ZBUKR.
        CONDENSE WA_NEFT_DETAILS-EMAIL NO-GAPS.

      ENDIF.

      SEARCH WA_NEFT_DETAILS-EMAIL FOR GVV_AT.
      IF SY-SUBRC EQ 0.
        SPLIT WA_NEFT_DETAILS-EMAIL AT '@' INTO GV_P1 GV_P2.
        CONDENSE GV_P2.
        IF GV_P2 CA GVV_DOT.
          GV_LENTH = STRLEN( GV_P2 ).
          GV_POS = GV_LENTH - 1.
          IF GV_P2+GV_POS(1) EQ '.'.
            WA_NEFT_DETAILS-EMAIL = GV_ENAME.
            CONDENSE WA_NEFT_DETAILS-EMAIL NO-GAPS.
          ENDIF.
        ELSE.
          WA_NEFT_DETAILS-EMAIL = GV_ENAME.
          CONDENSE WA_NEFT_DETAILS-EMAIL NO-GAPS.
        ENDIF.
*       do nothing
      ELSE.
        WA_NEFT_DETAILS-EMAIL = GV_ENAME.
        CONDENSE WA_NEFT_DETAILS-EMAIL NO-GAPS.
      ENDIF.

      IF WA_NEFT_DETAILS-EMAIL IS INITIAL.
        MOVE GV_ENAME TO WA_NEFT_DETAILS-EMAIL.
        CONDENSE WA_NEFT_DETAILS-EMAIL NO-GAPS.
      ENDIF.
      CLEAR:GV_POS,GV_LENTH,GV_P2,GV_P1.

    WHEN 'F'.
      IF P_WA_REGUH_PM-ZTLFX IS NOT INITIAL.
        MOVE P_WA_REGUH_PM-ZTLFX TO WA_NEFT_DETAILS-PH_NUM.
      ELSE.
        SELECT SINGLE * FROM ADRC
        WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
        IF SY-SUBRC EQ 0.
          WA_NEFT_DETAILS-PH_NUM = ADRC-FAX_NUMBER.
        ENDIF.
      ENDIF.
      IF WA_NEFT_DETAILS-PH_NUM IS INITIAL.
        IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
        IT_ERR-ERROR = 'Fax Number cannot be blank for Delivery mode F.'.
        APPEND IT_ERR.
        EXIT .
      ENDIF.

    WHEN 'M' OR 'R' OR 'C'.
      PERFORM GET_ADDRESS_PRINTFORM TABLES ADRS USING P_WA_REGUH_PM.
      GV_LEN1 = '45'.
      GV_LEN2 = '47'.

      WA_NEFT_DETAILS-BEN_ADDRS1 = ADRS-LINE0.
      PERFORM REPLACE_CHARS USING WA_NEFT_DETAILS-BEN_ADDRS1 GV_LEN1 GV_LEN2
      CHANGING WA_NEFT_DETAILS-BEN_ADDRS1.

      WA_NEFT_DETAILS-BEN_ADDRS2 = ADRS-LINE1.
      PERFORM REPLACE_CHARS USING WA_NEFT_DETAILS-BEN_ADDRS2 GV_LEN1 GV_LEN2
      CHANGING
      WA_NEFT_DETAILS-BEN_ADDRS2.

      WA_NEFT_DETAILS-BEN_ADDRS3 = ADRS-LINE2.
      PERFORM REPLACE_CHARS USING WA_NEFT_DETAILS-BEN_ADDRS3 GV_LEN1 GV_LEN2
      CHANGING
      WA_NEFT_DETAILS-BEN_ADDRS3.

      MOVE P_WA_REGUH_PM-ZPSTL TO WA_NEFT_DETAILS-BEN_ADDRS4.
      PERFORM REPLACE_CHARS USING WA_NEFT_DETAILS-BEN_ADDRS4 GV_LEN1 GV_LEN2
      CHANGING
      WA_NEFT_DETAILS-BEN_ADDRS4.

      IF WA_NEFT_DETAILS-BEN_ADDRS4 IS INITIAL.
        MOVE LFA1-PSTLZ TO WA_NEFT_DETAILS-BEN_ADDRS4.
        PERFORM REPLACE_CHARS USING WA_NEFT_DETAILS-BEN_ADDRS4 GV_LEN1 GV_LEN2
        CHANGING
        WA_NEFT_DETAILS-BEN_ADDRS4.
      ENDIF.

      IF WA_NEFT_DETAILS-BEN_ADDRS1 IS INITIAL.
        IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
        IT_ERR-ERROR = 'Mail to party Address line1 cannot be blank for Delivery modes M, R & C. '
                          .
        APPEND IT_ERR.
        EXIT .
      ENDIF.
  ENDCASE.
*----------------------------------------------------------------------*
*Advice Name
*----------------------------------------------------------------------*
  IF  WA_NEFT_DETAILS-DELVRY_MODE IS NOT INITIAL.

    WA_NEFT_DETAILS-ADVICE_NAME = WA_NEFT_DETAILS-BEN_NAME.
    GV_LEN1 = '45'.
    GV_LEN2 = '47'.
    PERFORM REPLACE_CHARS USING WA_NEFT_DETAILS-ADVICE_NAME GV_LEN1 GV_LEN2
    CHANGING WA_NEFT_DETAILS-ADVICE_NAME.

    IF WA_NEFT_DETAILS-ADVICE_NAME IS INITIAL.
      IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
      IT_ERR-ERROR = 'Mail to party name cannot be blank'.
      APPEND IT_ERR.
      EXIT .
    ENDIF.
  ENDIF.
*  endif.
*----------------------------------------------------------------------*
*Advice Details
*----------------------------------------------------------------------*
  DATA:NO_LINES TYPE I.
  REFRESH IREGUP2.
  MOVE-CORRESPONDING P_WA_REGUH_PM TO IREGUH2.
  APPEND IREGUH2.
  LOOP AT IREGUP WHERE LAUFD = P_WA_REGUH_PM-LAUFD
                   AND LAUFI = P_WA_REGUH_PM-LAUFI
                   AND XVORL = P_WA_REGUH_PM-XVORL
                   AND VBLNR = P_WA_REGUH_PM-VBLNR.
    MOVE-CORRESPONDING IREGUP TO IREGUP2.
    APPEND IREGUP2.
    READ TABLE IBSEG WITH KEY BELNR = IREGUP-BELNR
                              GJAHR = IREGUP-GJAHR
                              BUZEI = IREGUP-BUZEI.
    MOVE-CORRESPONDING IBSEG TO IBSEG2.
    APPEND IBSEG2.
  ENDLOOP.

  CLEAR: IMICRO, IMACRO.
  REFRESH: IMICRO, IMACRO.

  CALL FUNCTION 'Z_DBS_GET_PAYMENT_DETAILS_INDO'
    EXPORTING
      I_REGUH   = IREGUH2
      I_DD_PM   = DD_PM
      I_CHW_PM  = CHW_PM
      I_NEFT_PM = NEFT_PM
      I_RTGS_PM = RTGS_PM
    TABLES
      I_REGUP   = IREGUP2
      E_MICRO   = IMICRO
      I_BSEG    = IBSEG
      E_MACRO   = IMACRO.
*-----------------------------------------------*
*     Payment details + SGTXT are concatenated into micro
*-----------------------------------------------*
  LOOP AT IMICRO.
    DESCRIBE TABLE IMICRO LINES NO_LINES.

    IF IMICRO-BLOCK1 IS NOT INITIAL.
      GV_TEMP+0(69) = IMICRO-BLOCK1.
      IF IMICRO-BLOCK2 IS NOT INITIAL.
        GV_TEMP+69(1) = ','.
      ELSE.
        GV_TEMP+69(1) = ' '.
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK2 IS NOT INITIAL.
      GV_TEMP+70(69) = IMICRO-BLOCK2.
      IF IMICRO-BLOCK3 IS NOT INITIAL.
        GV_TEMP+139(1) = ',' .
      ELSE.
        GV_TEMP+139(1) = ' '.
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK3 IS NOT INITIAL.
      GV_TEMP+140(69) = IMICRO-BLOCK3.
      IF IMICRO-BLOCK4 IS NOT INITIAL.
        GV_TEMP+209(1) = ',' .
      ELSE.
        GV_TEMP+209(1) = ' '.
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK4 IS NOT INITIAL.
      GV_TEMP+210(69) = IMICRO-BLOCK4.
      IF IMICRO-BLOCK5 IS NOT INITIAL.
        GV_TEMP+279(1) = ',' .
      ELSE.
        GV_TEMP+279(1) = ' '.
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK5 IS NOT INITIAL.
      GV_TEMP+280(69) = IMICRO-BLOCK5.
      IF IMICRO-BLOCK6 IS NOT INITIAL.
        GV_TEMP+349(1) = ',' .
      ELSE.
        GV_TEMP+349(1) = ' '.
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK6 IS NOT INITIAL.
      GV_TEMP+350(69) = IMICRO-BLOCK6.
      IF IMICRO-BLOCK7 IS NOT INITIAL.
        GV_TEMP+419(1) = ',' .
      ELSE.
        GV_TEMP+419(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK7 IS NOT INITIAL.
      GV_TEMP+420(69) = IMICRO-BLOCK7.
      IF IMICRO-BLOCK8 IS NOT INITIAL.
        GV_TEMP+489(1) = ','.
      ELSE.
        GV_TEMP+489(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK8 IS NOT INITIAL.
      GV_TEMP+490(69) = IMICRO-BLOCK8.
      IF IMICRO-BLOCK9 IS NOT INITIAL.
        GV_TEMP+559(1) = ',' .
      ELSE.
        GV_TEMP+559(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK9 IS NOT INITIAL.
      GV_TEMP+560(69) = IMICRO-BLOCK9.
      IF IMICRO-BLOCK10 IS NOT INITIAL.
        GV_TEMP+629(1) = ','.
      ELSE.
        GV_TEMP+629(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK10 IS NOT INITIAL.
      GV_TEMP+630(69) = IMICRO-BLOCK10.
      IF IMICRO-BLOCK11 IS NOT INITIAL.
        GV_TEMP+699(1) = ',' .
      ELSE.
        GV_TEMP+699(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK11 IS NOT INITIAL.
      GV_TEMP+700(69) = IMICRO-BLOCK11.
      IF IMICRO-BLOCK12 IS NOT INITIAL.
        GV_TEMP+769(1) = ','.
      ELSE.
        GV_TEMP+769(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK12 IS NOT INITIAL.
      GV_TEMP+770(69) = IMICRO-BLOCK12.
      IF IMICRO-BLOCK13 IS NOT INITIAL.
        GV_TEMP+839(1) = ',' .
      ELSE.
        GV_TEMP+839(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK13 IS NOT INITIAL.
      GV_TEMP+840(69) = IMICRO-BLOCK13.
      IF IMICRO-BLOCK14 IS NOT INITIAL.
        GV_TEMP+909(1) = ',' .
      ELSE.
        GV_TEMP+909(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK14 IS NOT INITIAL.
      GV_TEMP+910(69) = IMICRO-BLOCK14.
      IF IMICRO-BLOCK15 IS NOT INITIAL.
        GV_TEMP+979(1) = ',' .
      ELSE.
        GV_TEMP+979(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK15 IS NOT INITIAL.
      GV_TEMP+980(69) = IMICRO-BLOCK15.
      IF IMICRO-BLOCK16 IS NOT INITIAL.
        GV_TEMP+1049(1) = ',' .
      ELSE.
        GV_TEMP+1049(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK16 IS NOT INITIAL.
      GV_TEMP+1050(69) = IMICRO-BLOCK16.
      IF IMICRO-BLOCK17 IS NOT INITIAL.
        GV_TEMP+1119(1) = ',' .
      ELSE.
        GV_TEMP+1119(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK17 IS NOT INITIAL.
      GV_TEMP+1120(69) = IMICRO-BLOCK17.
      IF IMICRO-BLOCK18 IS NOT INITIAL.
        GV_TEMP+1189(1) = ',' .
      ELSE.
        GV_TEMP+1189(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK18 IS NOT INITIAL.
      GV_TEMP+1190(69) = IMICRO-BLOCK18.
      IF IMICRO-BLOCK19 IS NOT INITIAL.
        GV_TEMP+1259(1) = ','.
      ELSE.
        GV_TEMP+1259(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK19 IS NOT INITIAL.
      GV_TEMP+1260(69) = IMICRO-BLOCK19.
      IF IMICRO-BLOCK20 IS NOT INITIAL.
        GV_TEMP+1329(1) = ','.
      ELSE.
        GV_TEMP+1329(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK20 IS NOT INITIAL.
      GV_TEMP+1330(69) = IMICRO-BLOCK20.
      DESCRIBE TABLE IMICRO LINES NO_LINES.
      IF NO_LINES GT 1.
        GV_TEMP+1399(1) = ','.
      ELSE.
        GV_TEMP+1399(1) = ' '.
      ENDIF.
    ENDIF.

    IF SY-TABIX EQ 1.
      MOVE GV_TEMP TO GV_TEMP1.
    ELSEIF SY-TABIX EQ 2.
      MOVE GV_TEMP TO GV_TEMP2.
    ELSE.
      MOVE GV_TEMP TO GV_TEMP3.
    ENDIF.
    CLEAR GV_TEMP.
  ENDLOOP.
  CONCATENATE GV_TEMP1 GV_TEMP2 INTO WA_NEFT_DETAILS-ADVICE_DTLS.

  CLEAR: GV_TEMP1,GV_TEMP2,NO_LINES.

  APPEND WA_NEFT_DETAILS TO IT_NEFT_DETAILS.

  PERFORM APPEND_LOG_DISPLAY USING P_WA_REGUH_PM.

ENDFORM.                    " fill_neft
*&---------------------------------------------------------------------*
*&      Form  fill_rtgs
*&---------------------------------------------------------------------*
FORM FILL_RTGS USING P_WA_REGUH_PM TYPE TY_REGUH_PM.

  DATA: BEGIN OF IMACRO OCCURS 1.
          INCLUDE STRUCTURE ZPDETL0.
  DATA: END OF IMACRO.

  DATA: BEGIN OF IMICRO OCCURS 1.
          INCLUDE STRUCTURE ZPDETL.
  DATA: END OF IMICRO.
  DATA: T_TEMP(13).

  CLEAR: IT_RTGS , GV_ENAME.

  READ TABLE IT_ZDBSPN INTO WA_ZDBSPN
  WITH KEY ZLSCH = P_WA_REGUH_PM-RZAWE
           LAND1 = P_WA_REGUH_PM-LAND1
           WAERS = P_WA_REGUH_PM-WAERS.
  GV_ENAME = WA_ZDBSPN-ZDBSMAIL.

*---------------------------------------------------------------------*
***1 Product name
*---------------------------------------------------------------------*
  IT_RTGS-PNAME = 'R'.
**---------------------------------------------------------------------*
****2 Organization Id
**---------------------------------------------------------------------*
*  it_rtgs-orgid = 'INDOFILL'.
*---------------------------------------------------------------------*
***3 Bank Reference Number
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*** 4 Customer Reference no.
*---------------------------------------------------------------------*
  GV_LEN1 = '11'.
  GV_LEN2 = '13'.
  MOVE P_WA_REGUH_PM-VBLNR TO IT_RTGS-VBLNR.
  PERFORM REPLACE_CHARS USING IT_RTGS-VBLNR GV_LEN1 GV_LEN2 CHANGING
IT_RTGS-VBLNR.
  CLEAR:GV_LEN1,GV_LEN2.
*---------------------------------------------------------------------*
*** 3 Debit A/C no.
*---------------------------------------------------------------------*
  CLEAR UTAB. REFRESH UTAB.
  SPLIT P_WA_REGUH_PM-UBKNT AT '-' INTO TABLE UTAB.
  LOOP AT UTAB.
    CONCATENATE IT_RTGS-UBKNT UTAB-WORD INTO IT_RTGS-UBKNT.
  ENDLOOP.
  IF IT_RTGS-UBKNT IS INITIAL.
    PERFORM CHK_N_UPDATE_ERROR_LOG
    USING  IT_RTGS-UBKNT
           P_WA_REGUH_PM-VBLNR
           'Debit Account number cannot be blank'.
  ENDIF.
*---------------------------------------------------------------------*
*** 4 Value Date
*---------------------------------------------------------------------*
  CONCATENATE PA_VALDT+6(2) PA_VALDT+4(2) PA_VALDT+0(4)
              INTO IT_RTGS-VALUTS . "separated by '-'.
*** Value date in String for sorting
**---------------------------------------------------------------------*
***5 Payment amount
*---------------------------------------------------------------------*
  WRITE P_WA_REGUH_PM-RWBTR TO IT_RTGS-RWBTR NO-SIGN NO-GROUPING
                        CURRENCY P_WA_REGUH_PM-WAERS.
  TRANSLATE IT_RTGS-RWBTR USING ',.' .
  CONDENSE IT_RTGS-RWBTR.
*    it_rtgs-rwbtr_sum = it_rtgs-rwbtr_sum + it_rtgs-rwbtr.

*  MOVE p_wa_reguh_pm-rwbtr TO it_rtgs-rwbtr.

*  WRITE it_rtgs-rwbtr TO gv_char2 NO-SIGN NO-GROUPING DECIMALS 0 LEFT-JUSTIFIED.
*  CONDENSE gv_char2 NO-GAPS.

*  wa_neft_details-char_amt = gv_char2 .
*  CLEAR gv_char2.

*  it_rtgs-rwbtr_sum = p_wa_reguh_pm-rwbtr.

  IF IT_RTGS-RWBTR IS INITIAL.
    IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
    IT_ERR-ERROR = 'Payment Amount cannot be blank'.
    APPEND IT_ERR.
    EXIT .
  ENDIF.
*---------------------------------------------------------------------*
*** 7 Bene Name
*---------------------------------------------------------------------*
  IF P_WA_REGUH_PM-ZNME1 IS NOT INITIAL.
    CONCATENATE P_WA_REGUH_PM-ZNME1 P_WA_REGUH_PM-ZNME2 INTO IT_RTGS-NAME
                               SEPARATED BY SPACE.
  ELSE.
    CONCATENATE P_WA_REGUH_PM-NAME1 P_WA_REGUH_PM-NAME2 INTO IT_RTGS-NAME
              SEPARATED BY SPACE.
  ENDIF.

  GV_LEN1 = '45'.
  GV_LEN2 = '47'.
  PERFORM REPLACE_CHARS USING IT_RTGS-NAME GV_LEN1 GV_LEN2 CHANGING
IT_RTGS-NAME.

  IF IT_RTGS-NAME IS INITIAL.
    IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
    IT_ERR-ERROR = 'Benificiary Name cannot be blank'.
    APPEND IT_ERR.
    EXIT .
  ENDIF.
  CLEAR: GV_LEN1,GV_LEN2.
*---------------------------------------------------------------------*
***8-10  Beneficiary Address
*---------------------------------------------------------------------*
  PERFORM GET_ADDRESS_PRINTFORM TABLES ADRS USING P_WA_REGUH_PM.
  MOVE ADRS-LINE0 TO IT_RTGS-BEN_ADD1.
  MOVE ADRS-LINE1 TO IT_RTGS-BEN_ADD2.
  MOVE ADRS-LINE2 TO IT_RTGS-BEN_ADD3.

  GV_LEN1 = '30'.
  GV_LEN2 = '32'.

  PERFORM REPLACE_CHARS USING IT_RTGS-BEN_ADD1 GV_LEN1 GV_LEN2 CHANGING
IT_RTGS-BEN_ADD1.
  CLEAR:GV_LEN1,GV_LEN2.

  IF IT_RTGS-BEN_ADD1 IS INITIAL.
    IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
    IT_ERR-ERROR = 'Benificiary Address line1 cannot be blank'.
    APPEND IT_ERR.
    EXIT .
  ENDIF.
*---------------------------------------------------------------------*
***11 Bene Account Number
*---------------------------------------------------------------------*
  IF NOT P_WA_REGUH_PM-ZBNKN IS INITIAL.
    IT_RTGS-ZBNKN = P_WA_REGUH_PM-ZBNKN.
  ELSE.

    IF P_WA_REGUH_PM-LIFNR <> ''.
      IF NOT P_WA_REGUH_PM-ZBNKY IS INITIAL.
        SELECT SINGLE * FROM LFBK WHERE LIFNR = P_WA_REGUH_PM-LIFNR
                                    AND BANKL = P_WA_REGUH_PM-ZBNKY
                                    AND BANKS = P_WA_REGUH_PM-ZBNKS.
      ELSE.
        SELECT SINGLE * FROM LFBK WHERE LIFNR = P_WA_REGUH_PM-LIFNR
                                    AND BANKL = P_WA_REGUH_PM-ZBNKL
                                    AND BANKS = P_WA_REGUH_PM-ZBNKS.
      ENDIF.
      IT_RTGS-ZBNKN = LFBK-BKREF.
      IF IT_RTGS-ZBNKN = ''.
        IT_RTGS-ZBNKN = LFBK-BANKN.
      ENDIF.
    ELSE.
      IF NOT P_WA_REGUH_PM-ZBNKY IS INITIAL.
        SELECT SINGLE * FROM KNBK WHERE KUNNR = P_WA_REGUH_PM-KUNNR
                                    AND BANKL = P_WA_REGUH_PM-ZBNKY
                                    AND BANKS = P_WA_REGUH_PM-ZBNKS.
      ELSE.
        SELECT SINGLE * FROM KNBK WHERE KUNNR = P_WA_REGUH_PM-KUNNR
                                    AND BANKL = P_WA_REGUH_PM-ZBNKL
                                    AND BANKS = P_WA_REGUH_PM-ZBNKS.
      ENDIF.
      IT_RTGS-ZBNKN = KNBK-BKREF.
      IF IT_RTGS-ZBNKN = ''.
        IT_RTGS-ZBNKN = KNBK-BANKN.
      ENDIF.
    ENDIF.

  ENDIF.

  IF IT_RTGS-ZBNKN IS INITIAL.
    IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
    IT_ERR-ERROR = 'Benificiary Account Number cannot be blank'.
    APPEND IT_ERR.
    EXIT .
  ENDIF.
*---------------------------------------------------------------------*
*** 14-17 Bene bank name & ADDRESS.
*---------------------------------------------------------------------*
  SELECT SINGLE * FROM BNKA WHERE BANKS = P_WA_REGUH_PM-ZBNKS
                              AND BANKL = P_WA_REGUH_PM-ZBNKY.
  GV_LEN1 = '30'.
  GV_LEN2 = '32'.
  IF SY-SUBRC = 0.
    MOVE BNKA-BANKA TO IT_RTGS-BANKA.
    PERFORM REPLACE_CHARS USING IT_RTGS-BANKA GV_LEN1 GV_LEN2 CHANGING
IT_RTGS-BANKA.
  ENDIF.
  CLEAR:GV_LEN1, GV_LEN2.
  IF IT_RTGS-BANKA IS INITIAL.
    IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
    IT_ERR-ERROR = 'Benificiary Bank Name cannot be blank'.
    APPEND IT_ERR.
    EXIT .
  ENDIF.
*---------------------------------------------------------------------*
*** 16 IFSC Code - Bank code - 4 digits '0' in 5th digit branch -6 digits
*---------------------------------------------------------------------*
*  it_rtgs-ifsc = p_wa_reguh_pm-zswif.
  IT_RTGS-IFSC = P_WA_REGUH_PM-ZBNKY+0(11).
  REPLACE 'O' WITH '0' INTO IT_RTGS-IFSC.
  CONDENSE  IT_RTGS-IFSC NO-GAPS.


*---------------------------------------------------------------------*
****18  Payment Details
*---------------------------------------------------------------------*
  MOVE P_WA_REGUH_PM-VBLNR TO IT_RTGS-PAY_DTL.
*---------------------------------------------------------------------*
  GV_LEN1 = '135'.
  GV_LEN2 = '137'.

  PERFORM REPLACE_CHARS USING IT_RTGS-PAY_DTL GV_LEN1 GV_LEN2 CHANGING
IT_RTGS-PAY_DTL.
  CLEAR:GV_LEN1,GV_LEN2.
*---------------------------------------------------------------------*
* Delivery Mode
*---------------------------------------------------------------------*
  READ TABLE IT_ZDBSPN INTO WA_ZDBSPN
    WITH KEY ZLSCH = P_WA_REGUH_PM-RZAWE
             LAND1 = P_WA_REGUH_PM-ZLAND
             WAERS = P_WA_REGUH_PM-WAERS.

  IF SY-SUBRC EQ 0.
    IT_RTGS-DELI_MODE = WA_ZDBSPN-ZDBSDM.
  ENDIF.
*---------------------------------------------------------------------*
**25 E_mail - if delivery mode is E-mail
*---------------------------------------------------------------------*
  IF IT_RTGS-DELI_MODE = 'E'.

    IF NOT P_WA_REGUH_PM-ZADNR IS INITIAL AND P_WA_REGUH_PM-ADRNR IS NOT INITIAL.
      SELECT SINGLE SMTP_ADDR
      INTO IT_RTGS-EMAIL
      FROM ADR6
      WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
      CONDENSE IT_RTGS-EMAIL NO-GAPS.

    ELSEIF NOT P_WA_REGUH_PM-ZADNR IS INITIAL AND P_WA_REGUH_PM-ADRNR IS INITIAL.

      SELECT SINGLE SMTP_ADDR
      INTO IT_RTGS-EMAIL
      FROM ADR6
      WHERE ADDRNUMBER = P_WA_REGUH_PM-ZADNR.
      CONDENSE IT_RTGS-EMAIL NO-GAPS.

    ELSEIF P_WA_REGUH_PM-ZADNR IS INITIAL AND P_WA_REGUH_PM-ADRNR IS NOT INITIAL.

      SELECT SINGLE SMTP_ADDR
      INTO IT_RTGS-EMAIL
      FROM ADR6
      WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
      CONDENSE IT_RTGS-EMAIL NO-GAPS.

    ELSEIF P_WA_REGUH_PM-ZADNR IS INITIAL AND P_WA_REGUH_PM-ADRNR IS INITIAL.
      SELECT SINGLE TLFXS
     INTO IT_RTGS-EMAIL FROM LFB1
     WHERE LIFNR = P_WA_REGUH_PM-LIFNR
     AND   BUKRS = P_WA_REGUH_PM-ZBUKR.
      CONDENSE IT_RTGS-EMAIL NO-GAPS.
    ENDIF.

    SEARCH IT_RTGS-EMAIL FOR GVV_AT.
    IF SY-SUBRC EQ 0.
      SPLIT IT_RTGS-EMAIL AT '@' INTO GV_P1 GV_P2.
      CONDENSE GV_P2.
      IF GV_P2 CA GVV_DOT.
        GV_LENTH = STRLEN( GV_P2 ).
        GV_POS = GV_LENTH - 1.
        IF GV_P2+GV_POS(1) EQ '.'.
          IT_RTGS-EMAIL = GV_ENAME.
          CONDENSE IT_RTGS-EMAIL NO-GAPS.
        ENDIF.
      ELSE.
        IT_RTGS-EMAIL = GV_ENAME.
        CONDENSE IT_RTGS-EMAIL NO-GAPS.
      ENDIF.
*       do nothing
    ELSE.
      IT_RTGS-EMAIL = GV_ENAME.
      CONDENSE IT_RTGS-EMAIL NO-GAPS.
    ENDIF.

    IF IT_RTGS-EMAIL IS INITIAL.
      MOVE GV_ENAME TO IT_RTGS-EMAIL.
      CONDENSE IT_RTGS-EMAIL NO-GAPS.
    ENDIF.

    CLEAR:GV_POS,GV_LENTH,GV_P1,GV_P2.
  ENDIF.
*---------------------------------------------------------------------*
*** 6 Payment currency
*---------------------------------------------------------------------*
  MOVE P_WA_REGUH_PM-WAERS TO IT_RTGS-WAERS.
  IF IT_RTGS-WAERS IS INITIAL.
    IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
    IT_ERR-ERROR = 'Payment currency cannot be blank'.
    APPEND IT_ERR.
    EXIT .
  ENDIF.
*---------------------------------------------------------------------*
***12 13 Bene Bank & Branch code
*---------------------------------------------------------------------*
*  IF NOT p_wa_reguh_pm-zbnky IS INITIAL.
*    MOVE p_wa_reguh_pm-zbnky TO it_rtgs-zbnkl.
*  ELSE.
*    MOVE p_wa_reguh_pm-zbnkl TO it_rtgs-zbnkl.
*  ENDIF.
*
*  IF NOT p_wa_reguh_pm-zbnky IS INITIAL.
*    SPLIT p_wa_reguh_pm-zbnky AT '-' INTO it_rtgs-zbnkl it_rtgs-brnch.
*    IF it_rtgs-brnch IS INITIAL.
*      it_rtgs-brnch = p_wa_reguh_pm-zbnky+4(3).
*    ENDIF.
*  ELSE.
*    SPLIT p_wa_reguh_pm-zbnkl AT '-' INTO it_rtgs-zbnkl it_rtgs-brnch.
*    IF it_rtgs-brnch IS INITIAL.
*      it_rtgs-brnch = p_wa_reguh_pm-zbnkl+4(3).
*    ENDIF.
*  ENDIF.
*
*  IF it_rtgs-zbnkl IS INITIAL.
*    it_err-vblnr = p_wa_reguh_pm-vblnr.
*    it_err-error = 'Benificiary Bank Code cannot be blank'.
*    APPEND it_err.
*    EXIT .
*  ENDIF.
*---------------------------------------------------------------------*
*** Optional
*** 20-24 Advice Name
*---------------------------------------------------------------------*
  IF NOT IT_RTGS-DELI_MODE IS INITIAL.
    IT_RTGS-ADV_NAME  = IT_RTGS-NAME.
    GV_LEN1 = '95'.
    GV_LEN2 = '97'.

    PERFORM REPLACE_CHARS USING IT_RTGS-ADV_NAME GV_LEN1 GV_LEN2 CHANGING
 IT_RTGS-ADV_NAME.
    CLEAR:GV_LEN1,GV_LEN2.

    IF IT_RTGS-ADV_NAME IS INITIAL.
      IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
      CONCATENATE 'Mail to party name cannot be blank'
                   'for Delivery mode' IT_RTGS-DELI_MODE
                   INTO IT_ERR-ERROR SEPARATED BY SPACE.
      APPEND IT_ERR.
      EXIT .
    ENDIF.

  ENDIF.
*---------------------------------------------------------------------*
  IF IT_RTGS-DELI_MODE CA 'MRC'.
    IT_RTGS-MAIL_ADD1 = ADRS-LINE0.

    GV_LEN1 = '45'.
    GV_LEN2 = '47'.

    PERFORM REPLACE_CHARS USING IT_RTGS-MAIL_ADD1 GV_LEN1 GV_LEN2
CHANGING
IT_RTGS-MAIL_ADD1.

    IF IT_RTGS-MAIL_ADD1 IS INITIAL.
      IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
      IT_ERR-ERROR = 'Mail to party Address line1 cannot be blank for Delivery modes M R & C.'.
      APPEND IT_ERR.
      EXIT .
    ENDIF.
  ENDIF.
*---------------------------------------------------------------------*
*** 26 fax number - if delivery mode is Fax
*---------------------------------------------------------------------*
  IF IT_RTGS-DELI_MODE = 'F'.
    IF P_WA_REGUH_PM-ZTLFX IS NOT INITIAL.
      MOVE P_WA_REGUH_PM-ZTLFX TO IT_RTGS-FAX.
    ELSE.
      SELECT SINGLE * FROM ADRC
      WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
      IF SY-SUBRC EQ 0.
        IT_RTGS-FAX = ADRC-FAX_NUMBER.
      ENDIF.
    ENDIF.

    IF IT_RTGS-FAX IS INITIAL.
      IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
      IT_ERR-ERROR = 'Fax Number cannot be blank for Delivery mode F.'.
      APPEND IT_ERR.
      EXIT .
    ENDIF.
  ENDIF.
*---------------------------------------------------------------------*
***27 Advice (Payment details).
*---------------------------------------------------------------------*
  DATA:NO_LINES TYPE I.
  MOVE-CORRESPONDING P_WA_REGUH_PM TO IREGUH2.
  REFRESH : IREGUP2 ,IREGUH2.
  APPEND IREGUH2.
  LOOP AT IREGUP WHERE LAUFD = P_WA_REGUH_PM-LAUFD
                   AND LAUFI = P_WA_REGUH_PM-LAUFI
                   AND XVORL = P_WA_REGUH_PM-XVORL
                   AND VBLNR = P_WA_REGUH_PM-VBLNR.
    MOVE-CORRESPONDING IREGUP TO IREGUP2.
    APPEND IREGUP2.
    READ TABLE IBSEG WITH KEY BELNR = IREGUP-BELNR
                              GJAHR = IREGUP-GJAHR
                              BUZEI = IREGUP-BUZEI.
    MOVE-CORRESPONDING IBSEG TO IBSEG2.
    APPEND IBSEG2.
  ENDLOOP.

  CLEAR: IMICRO, IMACRO.
  REFRESH: IMICRO, IMACRO.

  CALL FUNCTION 'Z_DBS_GET_PAYMENT_DETAILS_INDO'
    EXPORTING
      I_REGUH   = IREGUH2
      I_DD_PM   = DD_PM
      I_CHW_PM  = CHW_PM
      I_RTGS_PM = RTGS_PM
      I_NEFT_PM = NEFT_PM
    TABLES
      I_REGUP   = IREGUP2
      E_MICRO   = IMICRO
      I_BSEG    = IBSEG2
      E_MACRO   = IMACRO.

  LOOP AT IMICRO.
    IF IMICRO-BLOCK1 IS NOT INITIAL.
      GV_TEMP+0(69) = IMICRO-BLOCK1.
      IF IMICRO-BLOCK2 IS NOT INITIAL.
        GV_TEMP+69(1) = ','.
      ELSE.
        GV_TEMP+69(1) = ' '.
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK2 IS NOT INITIAL.
      GV_TEMP+70(69) = IMICRO-BLOCK2.
      IF IMICRO-BLOCK3 IS NOT INITIAL.
        GV_TEMP+139(1) = ',' .
      ELSE.
        GV_TEMP+139(1) = ' '.
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK3 IS NOT INITIAL.
      GV_TEMP+140(69) = IMICRO-BLOCK3.
      IF IMICRO-BLOCK4 IS NOT INITIAL.
        GV_TEMP+209(1) = ',' .
      ELSE.
        GV_TEMP+209(1) = ' '.
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK4 IS NOT INITIAL.
      GV_TEMP+210(69) = IMICRO-BLOCK4.
      IF IMICRO-BLOCK5 IS NOT INITIAL.
        GV_TEMP+279(1) = ',' .
      ELSE.
        GV_TEMP+279(1) = ' '.
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK5 IS NOT INITIAL.
      GV_TEMP+280(69) = IMICRO-BLOCK5.
      IF IMICRO-BLOCK6 IS NOT INITIAL.
        GV_TEMP+349(1) = ',' .
      ELSE.
        GV_TEMP+349(1) = ' '.
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK6 IS NOT INITIAL.
      GV_TEMP+350(69) = IMICRO-BLOCK6.
      IF IMICRO-BLOCK7 IS NOT INITIAL.
        GV_TEMP+419(1) = ',' .
      ELSE.
        GV_TEMP+419(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK7 IS NOT INITIAL.
      GV_TEMP+420(69) = IMICRO-BLOCK7.
      IF IMICRO-BLOCK8 IS NOT INITIAL.
        GV_TEMP+489(1) = ','.
      ELSE.
        GV_TEMP+489(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK8 IS NOT INITIAL.
      GV_TEMP+490(69) = IMICRO-BLOCK8.
      IF IMICRO-BLOCK9 IS NOT INITIAL.
        GV_TEMP+559(1) = ',' .
      ELSE.
        GV_TEMP+559(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK9 IS NOT INITIAL.
      GV_TEMP+560(69) = IMICRO-BLOCK9.
      IF IMICRO-BLOCK10 IS NOT INITIAL.
        GV_TEMP+629(1) = ','.
      ELSE.
        GV_TEMP+629(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK10 IS NOT INITIAL.
      GV_TEMP+630(69) = IMICRO-BLOCK10.
      IF IMICRO-BLOCK11 IS NOT INITIAL.
        GV_TEMP+699(1) = ',' .
      ELSE.
        GV_TEMP+699(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK11 IS NOT INITIAL.
      GV_TEMP+700(69) = IMICRO-BLOCK11.
      IF IMICRO-BLOCK12 IS NOT INITIAL.
        GV_TEMP+769(1) = ','.
      ELSE.
        GV_TEMP+769(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK12 IS NOT INITIAL.
      GV_TEMP+770(69) = IMICRO-BLOCK12.
      IF IMICRO-BLOCK13 IS NOT INITIAL.
        GV_TEMP+839(1) = ',' .
      ELSE.
        GV_TEMP+839(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK13 IS NOT INITIAL.
      GV_TEMP+840(69) = IMICRO-BLOCK13.
      IF IMICRO-BLOCK14 IS NOT INITIAL.
        GV_TEMP+909(1) = ',' .
      ELSE.
        GV_TEMP+909(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK14 IS NOT INITIAL.
      GV_TEMP+910(69) = IMICRO-BLOCK14.
      IF IMICRO-BLOCK15 IS NOT INITIAL.
        GV_TEMP+979(1) = ',' .
      ELSE.
        GV_TEMP+979(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK15 IS NOT INITIAL.
      GV_TEMP+980(69) = IMICRO-BLOCK15.
      IF IMICRO-BLOCK16 IS NOT INITIAL.
        GV_TEMP+1049(1) = ',' .
      ELSE.
        GV_TEMP+1049(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK16 IS NOT INITIAL.
      GV_TEMP+1050(69) = IMICRO-BLOCK16.
      IF IMICRO-BLOCK17 IS NOT INITIAL.
        GV_TEMP+1119(1) = ',' .
      ELSE.
        GV_TEMP+1119(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK17 IS NOT INITIAL.
      GV_TEMP+1120(69) = IMICRO-BLOCK17.
      IF IMICRO-BLOCK18 IS NOT INITIAL.
        GV_TEMP+1189(1) = ',' .
      ELSE.
        GV_TEMP+1189(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK18 IS NOT INITIAL.
      GV_TEMP+1190(69) = IMICRO-BLOCK18.
      IF IMICRO-BLOCK19 IS NOT INITIAL.
        GV_TEMP+1259(1) = ','.
      ELSE.
        GV_TEMP+1259(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK19 IS NOT INITIAL.
      GV_TEMP+1260(69) = IMICRO-BLOCK19.
      IF IMICRO-BLOCK20 IS NOT INITIAL.
        GV_TEMP+1329(1) = ','.
      ELSE.
        GV_TEMP+1329(1) = ' ' .
      ENDIF.
    ENDIF.

    IF IMICRO-BLOCK20 IS NOT INITIAL.
      GV_TEMP+1330(69) = IMICRO-BLOCK20.
      DESCRIBE TABLE IMICRO LINES NO_LINES.
      IF NO_LINES GT 1.
        GV_TEMP+1399(1) = ','.
      ELSE.
        GV_TEMP+1399(1) = ' '.
      ENDIF.
    ENDIF.

    IF SY-TABIX EQ 1.
      MOVE GV_TEMP TO GV_TEMP1.
    ELSEIF SY-TABIX EQ 2.
      MOVE GV_TEMP TO GV_TEMP2.
    ELSE.
      MOVE GV_TEMP TO GV_TEMP3.
    ENDIF.
    CLEAR GV_TEMP.
  ENDLOOP.

  CONCATENATE GV_TEMP1 GV_TEMP2 INTO IT_RTGS-ADVICE.
  CLEAR: GV_TEMP1,GV_TEMP2,NO_LINES.
  APPEND IT_RTGS.
  SORT IT_RTGS BY UBKNT.
  WA_RTGS_NEW-PNAME = IT_RTGS-PNAME.  " Product name
  WA_RTGS_NEW-VBLNR = IT_RTGS-VBLNR. " customer Ref no
  WA_RTGS_NEW-UBKNT = IT_RTGS-UBKNT. " Debit Account Number

  CLEAR: V_MM, V_MMM.
  MOVE IT_RTGS-VALUTS+2(2) TO V_MM.
  CASE V_MM.
    WHEN '01'.
      V_MMM = 'Jan'.
    WHEN '02'.
      V_MMM = 'Feb'.
    WHEN '03'.
      V_MMM = 'Mar'.
    WHEN '04'.
      V_MMM = 'Apr'.
    WHEN '05'.
      V_MMM = 'May'.
    WHEN '06'.
      V_MMM = 'Jun'.
    WHEN '07'.
      V_MMM = 'Jul'.
    WHEN '08'.
      V_MMM = 'Aug'.
    WHEN '09'.
      V_MMM = 'Sep'.
    WHEN '10'.
      V_MMM = 'Oct'.
    WHEN '11'.
      V_MMM = 'Nov'.
    WHEN '12'.
      V_MMM = 'Dec'.
  ENDCASE.

  CONCATENATE IT_RTGS-VALUTS+0(2) '-' V_MMM '-' IT_RTGS-VALUTS+4(4) INTO WA_RTGS_NEW-VALUTS.
  WA_RTGS_NEW-RWBTR = IT_RTGS-RWBTR.          " Payment Amount
  WA_RTGS_NEW-WAERS = IT_RTGS-WAERS.         " Payment currency
  WA_RTGS_NEW-NAME = IT_RTGS-NAME.       " Bene Name
  WA_RTGS_NEW-BEN_ADD1 = IT_RTGS-BEN_ADD1.   " Bene address1
  WA_RTGS_NEW-BEN_ADD2 = IT_RTGS-BEN_ADD2.   " Bene address2
  WA_RTGS_NEW-BEN_ADD3 = IT_RTGS-BEN_ADD3.   " Bene address3
  WA_RTGS_NEW-ZBNKN = IT_RTGS-ZBNKN.          " Bene Account Number
  WA_RTGS_NEW-BANKA = IT_RTGS-BANKA.          " Bene Bank name
  WA_RTGS_NEW-BK_ADD1 = IT_RTGS-BK_ADD1.      " Bene Bank address1
  WA_RTGS_NEW-BK_ADD2 = IT_RTGS-BK_ADD2.      " Bene Bank address2
  WA_RTGS_NEW-BK_ADD3 = IT_RTGS-BK_ADD3.      " Bene Bank address3
  WA_RTGS_NEW-IFSC = IT_RTGS-IFSC.            " IFSC Code
  WA_RTGS_NEW-PAY_DTL = IT_RTGS-PAY_DTL.      " Payment detail
  WA_RTGS_NEW-DELI_MODE = IT_RTGS-DELI_MODE.  " Delivery mode
  WA_RTGS_NEW-ADV_NAME = IT_RTGS-ADV_NAME.    " Mail to party name
  WA_RTGS_NEW-MAIL_ADD1 = IT_RTGS-MAIL_ADD1.  " Mail 2 party adrs1
  WA_RTGS_NEW-MAIL_ADD2 = IT_RTGS-MAIL_ADD2.  " Mail 2 party adrs1
  WA_RTGS_NEW-MAIL_ADD3 = IT_RTGS-MAIL_ADD3.  " Mail 2 party adrs1
  WA_RTGS_NEW-MAIL_ADD4 = IT_RTGS-MAIL_ADD4.  " Mail 2 party adrs1
  WA_RTGS_NEW-EMAIL = IT_RTGS-EMAIL.          " E-mail address
  WA_RTGS_NEW-FAX = IT_RTGS-FAX.              " Fax number
  WA_RTGS_NEW-ADVICE = IT_RTGS-ADVICE.        " Advice Details

  IF NOT WA_RTGS_NEW IS INITIAL.
    APPEND WA_RTGS_NEW TO IT_RTGS_NEW.
    CLEAR: WA_RTGS_NEW.
  ENDIF.

  SORT IT_RTGS_NEW BY VALUT UBKNT.

  CLEAR IT_LIST.
  MOVE-CORRESPONDING IREGUH2 TO IT_LIST.
  WRITE IREGUH2-RWBTR TO T_TEMP NO-GROUPING
        CURRENCY P_WA_REGUH_PM-WAERS.
  TRANSLATE T_TEMP USING ',.' .
  IT_LIST-RWBTR = T_TEMP.
  CONCATENATE IREGUH2-ZNME1 IREGUH2-ZNME2 INTO IT_LIST-NAME
                                      SEPARATED BY SPACE.
  SELECT SINGLE LAND1 INTO T001-LAND1 FROM T001
          WHERE BUKRS = IREGUH2-ZBUKR.

  SELECT SINGLE TEXT1 INTO IT_LIST-TEXT1
                  FROM T042Z WHERE LAND1 = T001-LAND1
                               AND ZLSCH = IREGUH2-RZAWE.
  IT_LIST-RZAWE = IREGUH2-RZAWE.
  MOVE-CORRESPONDING IREGUP2 TO IT_LIST.
  APPEND IT_LIST.

ENDFORM.                    " fill_rtgs
*&---------------------------------------------------------------------*
*&      Form  replace_chars
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REPLACE_CHARS USING P_FIELD1 TYPE ANY
                         P_GV_LEN1 TYPE ANY
                         P_GV_LEN2 TYPE ANY
                   CHANGING P_FIELD2 TYPE ANY.


  REPLACE ALL OCCURRENCES OF GV_TILT IN P_FIELD1 WITH
GV_HIFEN.

  IF P_FIELD1 CA GV_TOPKEY.
    REPLACE GV_TOPKEY WITH SPACE INTO P_FIELD1.
    CONDENSE P_FIELD1.
  ENDIF.

  REPLACE ALL OCCURRENCES OF GV_EXCLAMTRY IN P_FIELD1 WITH
GV_DOT.

  IF STRLEN( P_FIELD1 ) LE P_GV_LEN2.
    REPLACE ALL OCCURRENCES OF GV_ATRATE IN P_FIELD1 WITH
  GV_AT.
  ELSEIF P_FIELD1 CA GV_ATRATE.
    REPLACE GV_ATRATE WITH SPACE INTO P_FIELD1.
    CONDENSE P_FIELD1.
  ENDIF.

  REPLACE ALL OCCURRENCES OF GV_HASH IN P_FIELD1 WITH
GV_COLON.
  REPLACE ALL OCCURRENCES OF GV_DOLLAR IN P_FIELD1 WITH
GV_FWDSLASH.
  REPLACE ALL OCCURRENCES OF GV_PERCENT IN P_FIELD1 WITH
GV_FWDSLASH.

  REPLACE ALL OCCURRENCES OF GV_CARRET IN P_FIELD1 WITH
GV_DOT.

  IF STRLEN( P_FIELD1 ) LE P_GV_LEN1.
    REPLACE ALL OCCURRENCES OF GV_AMPERSAND IN P_FIELD1 WITH
  GV_AND.
  ELSE.
    REPLACE ALL OCCURRENCES OF GV_AMPERSAND IN P_FIELD1 WITH 'N'.
  ENDIF.

  REPLACE ALL OCCURRENCES OF GV_STAR IN P_FIELD1 WITH GV_HIFEN.
  REPLACE ALL OCCURRENCES OF GV_UNDERSCORE IN P_FIELD1 WITH
GV_HIFEN.
  REPLACE ALL OCCURRENCES OF GV_EQUALS IN P_FIELD1 WITH
GV_HIFEN.
  REPLACE ALL OCCURRENCES OF GV_LESS IN P_FIELD1 WITH
GV_LSMALBRCKT.
  REPLACE ALL OCCURRENCES OF GV_MORE IN P_FIELD1 WITH
GV_RSMALBRCKT.
  REPLACE ALL OCCURRENCES OF GV_SEMICOLON IN P_FIELD1 WITH
GV_COMMA.

  IF P_FIELD1 CA GV_DBLE.
    REPLACE GV_DBLE WITH SPACE INTO P_FIELD1.
    CONDENSE P_FIELD1.
  ENDIF.

  REPLACE ALL OCCURRENCES OF GV_LFTBRCKT IN P_FIELD1 WITH
GV_LSMALBRCKT.
  REPLACE ALL OCCURRENCES OF GV_RGTBRCKT IN P_FIELD1 WITH
GV_RSMALBRCKT.
  REPLACE ALL OCCURRENCES OF GV_LFSQBRCKT IN P_FIELD1 WITH
GV_LSMALBRCKT.
  REPLACE ALL OCCURRENCES OF GV_RTSQBRCKT IN P_FIELD1 WITH
GV_RSMALBRCKT.
  REPLACE ALL OCCURRENCES OF GV_PIPE IN P_FIELD1 WITH
GV_COLON.
  REPLACE ALL OCCURRENCES OF GV_SLASH IN P_FIELD1 WITH
GV_FWDSLASH.
  CONDENSE P_FIELD1 .
ENDFORM.                    " replace_chars
*&---------------------------------------------------------------------*
*&      Form  encrypt_files
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ENCRYPT_FILES .
  DATA: LV_PARM TYPE STRING,
          LV_CEMS_BIN TYPE STRING,
          LV_ENCRYP_OUTPUT TYPE CHAR1024,
          LV_WINDOWS_DIR TYPE STRING,
          LV_CEMS_BAT TYPE STRING,
          LV_INPUT_FILE TYPE STRING.

  CONCATENATE '"' PA_CEMS '\bin' '"' INTO LV_CEMS_BIN.

  CONCATENATE '"' IT_FILIST-FULLNAME '"' INTO IT_FILIST-FULLNAME.

  CONCATENATE '"' PA_KEYST '"' INTO GV_KEYSTR.

  LOOP AT IT_FILIST. "it_filist.

    CLEAR LV_INPUT_FILE.
    CONCATENATE '"' IT_FILIST-OUTNAME '"' INTO LV_INPUT_FILE.

    CLEAR LV_ENCRYP_OUTPUT.
    CONCATENATE '"' IT_FILIST-OUTPUTFOLDER '"' INTO LV_ENCRYP_OUTPUT.

    CLEAR LV_PARM.
    CONCATENATE 'encrypt' GV_KEYSTR PA_PIN PA_CRTID
                '-input' LV_INPUT_FILE
                '-output' LV_ENCRYP_OUTPUT
                INTO LV_PARM
                SEPARATED BY SPACE.

    LV_CEMS_BAT = PA_PATH.

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>EXECUTE
      EXPORTING
        APPLICATION            = LV_CEMS_BAT
        PARAMETER              = LV_PARM
        SYNCHRONOUS            = 'X'
      EXCEPTIONS
        CNTL_ERROR             = 1
        ERROR_NO_GUI           = 2
        BAD_PARAMETER          = 3
        FILE_NOT_FOUND         = 4
        PATH_NOT_FOUND         = 5
        FILE_EXTENSION_UNKNOWN = 6
        ERROR_EXECUTE_FAILED   = 7
        SYNCHRONOUS_FAILED     = 8
        NOT_SUPPORTED_BY_GUI   = 9
        OTHERS                 = 10.
    IF SY-SUBRC <> 0.
      IT_ERR-VBLNR = 'Windows'.
      IT_ERR-ERROR = 'Unable to execute the CEMS software'.

      APPEND IT_ERR.
    ENDIF.

  ENDLOOP.

  IF NOT P_CEMS IS INITIAL.
    PERFORM CHECK_FILES_EXIST_JAR CHANGING RC.
  ENDIF.
ENDFORM.                    " encrypt_files
*&---------------------------------------------------------------------*
*&      Form  check_files_exist_jar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_RC  text
*----------------------------------------------------------------------*
FORM CHECK_FILES_EXIST_JAR  CHANGING RCODE.
  DATA: L_ITAB_FILES1 TYPE STANDARD TABLE OF FILE_INFO,
          L_WA_FILES1 TYPE FILE_INFO.

  DATA: LV_SOURCE1 TYPE STRING,
        LV_DEST1 TYPE STRING,
        LV_STR1(013) TYPE C VALUE '\infolder',
        LV_STR2(014) TYPE C VALUE '\outfolder',
        LV_STR3(08) TYPE C VALUE '\Archive',
        LV_RC1 TYPE I,
        LV_COUNT1 TYPE I,
        LV_COUNT2 TYPE I,
        LV_DIR1 TYPE STRING,
        LV_ARCHIVE_DIR1 TYPE STRING,
        LV_RESULT1.

  LV_DIR1 = PA_NEFT.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
    EXPORTING
      DIRECTORY                   = LV_DIR1
      FILTER                      = '*.*'
      FILES_ONLY                  = 'X'
    CHANGING
      FILE_TABLE                  = L_ITAB_FILES1
      COUNT                       = LV_COUNT1
    EXCEPTIONS
      CNTL_ERROR                  = 1
      DIRECTORY_LIST_FILES_FAILED = 2
      WRONG_PARAMETER             = 3
      ERROR_NO_GUI                = 4
      NOT_SUPPORTED_BY_GUI        = 5
      OTHERS                      = 6.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.

    REPLACE LV_STR1 IN LV_DIR1 WITH LV_STR2.
*    CONDENSE LV_DIR1.
SHIFT LV_DIR1 RIGHT DELETING TRAILING space.
  SHIFT LV_DIR1 LEFT DELETING LEADING space.

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
      EXPORTING
        DIRECTORY            = LV_DIR1
      RECEIVING
        RESULT               = LV_RESULT1
      EXCEPTIONS
        CNTL_ERROR           = 1
        ERROR_NO_GUI         = 2
        WRONG_PARAMETER      = 3
        NOT_SUPPORTED_BY_GUI = 4
        OTHERS               = 5.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

    ELSE.

      IF LV_RESULT1 IS INITIAL.

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE
          EXPORTING
            DIRECTORY                = LV_DIR1
          CHANGING
            RC                       = LV_RC1
          EXCEPTIONS
            DIRECTORY_CREATE_FAILED  = 1
            CNTL_ERROR               = 2
            ERROR_NO_GUI             = 3
            DIRECTORY_ACCESS_DENIED  = 4
            DIRECTORY_ALREADY_EXISTS = 5
            PATH_NOT_FOUND           = 6
            UNKNOWN_ERROR            = 7
            NOT_SUPPORTED_BY_GUI     = 8
            WRONG_PARAMETER          = 9
            OTHERS                   = 10.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDIF.

      LOOP AT L_ITAB_FILES1 INTO L_WA_FILES1.

        IF L_WA_FILES1-FILENAME CA 'jar'.

          CLEAR: LV_SOURCE1,
                 LV_DEST1.

          CONCATENATE PA_NEFT '\' L_WA_FILES1-FILENAME  INTO LV_SOURCE1
          .
          CONCATENATE LV_DIR1 '\' L_WA_FILES1-FILENAME  INTO LV_DEST1.

          CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_COPY
            EXPORTING
              SOURCE               = LV_SOURCE1
              DESTINATION          = LV_DEST1
              OVERWRITE            = 'X'
            EXCEPTIONS
              CNTL_ERROR           = 1
              ERROR_NO_GUI         = 2
              WRONG_PARAMETER      = 3
              DISK_FULL            = 4
              ACCESS_DENIED        = 5
              FILE_NOT_FOUND       = 6
              DESTINATION_EXISTS   = 7
              UNKNOWN_ERROR        = 8
              PATH_NOT_FOUND       = 9
              DISK_WRITE_PROTECT   = 10
              DRIVE_NOT_READY      = 11
              NOT_SUPPORTED_BY_GUI = 12
              OTHERS               = 13.
          IF SY-SUBRC <> 0.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ELSE.
            CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
              EXPORTING
                FILENAME             = LV_SOURCE1
              CHANGING
                RC                   = LV_RC1
              EXCEPTIONS
                FILE_DELETE_FAILED   = 1
                CNTL_ERROR           = 2
                ERROR_NO_GUI         = 3
                FILE_NOT_FOUND       = 4
                ACCESS_DENIED        = 5
                UNKNOWN_ERROR        = 6
                NOT_SUPPORTED_BY_GUI = 7
                WRONG_PARAMETER      = 8
                OTHERS               = 9.
            IF SY-SUBRC <> 0.
              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
  RCODE = 0.
ENDFORM.                    " check_files_exist_jar
*&---------------------------------------------------------------------*
*&      Form  check_files_exist_outfolder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_RC  text
*----------------------------------------------------------------------*
FORM CHECK_FILES_EXIST_OUTFOLDER  CHANGING RCODE.
  DATA: W_TEST2(128).
  DATA: L_ITAB_FILES2 TYPE STANDARD TABLE OF FILE_INFO,
        L_WA_FILES2 TYPE FILE_INFO.

  DATA: LV_SOURCE2 TYPE STRING,
        LV_DEST2 TYPE STRING,
        LV_RC2 TYPE I,
        LV_STR1(9) TYPE C VALUE '\infolder',
        LV_STR2(11) TYPE C VALUE '\outfolder',
        LV_ARC1(8) TYPE C VALUE '\archive',
        LV_ARC_CCH(13) TYPE C VALUE '\archive\CCH',
        LV_ARC_DD(12) TYPE C VALUE '\archive\DD',
        LV_ARC_RTGS(14) TYPE C VALUE '\archive\RTGS',
        LV_ARC_NEFT(14) TYPE C VALUE '\archive\NEFT',
        LV_COUNT2 TYPE I,
        LV_DIR2 TYPE STRING,
        LV_ARCHIVE_DIR2 TYPE STRING,
        LV_ARCHIVE_DIR2_CCH TYPE STRING,
        LV_ARCHIVE_DIR2_DD TYPE STRING,
        LV_ARCHIVE_DIR2_NEFT TYPE STRING,
        LV_ARCHIVE_DIR2_RTGS TYPE STRING,
        LV_RESULT2.

  LV_DIR2 = PA_NEFT.

  REPLACE LV_STR1 WITH lv_str2 INTO LV_DIR2. "SPACE
 SHIFT LV_DIR2 RIGHT DELETING TRAILING space.
  SHIFT lv_dir2 LEFT DELETING LEADING SPACE.
*  CONCATENATE LV_DIR2 LV_STR2 INTO LV_DIR2.
*  CONDENSE LV_DIR2 NO-GAPS.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
    EXPORTING
      DIRECTORY                   = LV_DIR2
      FILTER                      = '*.*'
      FILES_ONLY                  = 'X'
    CHANGING
      FILE_TABLE                  = L_ITAB_FILES2
      COUNT                       = LV_COUNT2
    EXCEPTIONS
      CNTL_ERROR                  = 1
      DIRECTORY_LIST_FILES_FAILED = 2
      WRONG_PARAMETER             = 3
      ERROR_NO_GUI                = 4
      NOT_SUPPORTED_BY_GUI        = 5
      OTHERS                      = 6.
  IF SY-SUBRC <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
* WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*----------------------------------------------------------------*
  ELSE.
*----------------------------------------------------------------*
* Archive Directory
*----------------------------------------------------------------*
    CONCATENATE LV_DIR2 LV_ARC1 INTO LV_ARCHIVE_DIR2.
    SHIFT LV_archive_DIR2 RIGHT DELETING TRAILING space.
  SHIFT lv_archive_dir2 LEFT DELETING LEADING SPACE.
*    CONDENSE LV_ARCHIVE_DIR2  NO-GAPS.

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
      EXPORTING
        DIRECTORY            = LV_ARCHIVE_DIR2
      RECEIVING
        RESULT               = LV_RESULT2
      EXCEPTIONS
        CNTL_ERROR           = 1
        ERROR_NO_GUI         = 2
        WRONG_PARAMETER      = 3
        NOT_SUPPORTED_BY_GUI = 4
        OTHERS               = 5.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.

      IF LV_RESULT2 IS INITIAL.

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE
          EXPORTING
            DIRECTORY                = LV_ARCHIVE_DIR2
          CHANGING
            RC                       = LV_RC2
          EXCEPTIONS
            DIRECTORY_CREATE_FAILED  = 1
            CNTL_ERROR               = 2
            ERROR_NO_GUI             = 3
            DIRECTORY_ACCESS_DENIED  = 4
            DIRECTORY_ALREADY_EXISTS = 5
            PATH_NOT_FOUND           = 6
            UNKNOWN_ERROR            = 7
            NOT_SUPPORTED_BY_GUI     = 8
            WRONG_PARAMETER          = 9
            OTHERS                   = 10.
        IF SY-SUBRC <> 0.
        ENDIF.
      ENDIF.
*----------------------------------------------------------------*
*      CCH Folder
*----------------------------------------------------------------*
      CONCATENATE LV_DIR2 LV_ARC_CCH INTO LV_ARCHIVE_DIR2_CCH.
        SHIFT LV_archive_DIR2_cch RIGHT DELETING TRAILING space.
  SHIFT lv_archive_dir2_cch LEFT DELETING LEADING SPACE.
*      CONDENSE LV_ARCHIVE_DIR2_CCH  NO-GAPS.

      CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
        EXPORTING
          DIRECTORY            = LV_ARCHIVE_DIR2_CCH
        RECEIVING
          RESULT               = LV_RESULT2
        EXCEPTIONS
          CNTL_ERROR           = 1
          ERROR_NO_GUI         = 2
          WRONG_PARAMETER      = 3
          NOT_SUPPORTED_BY_GUI = 4
          OTHERS               = 5.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

      ELSE.
        IF LV_RESULT2 IS INITIAL.

          CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE
            EXPORTING
              DIRECTORY                = LV_ARCHIVE_DIR2_CCH
            CHANGING
              RC                       = LV_RC2
            EXCEPTIONS
              DIRECTORY_CREATE_FAILED  = 1
              CNTL_ERROR               = 2
              ERROR_NO_GUI             = 3
              DIRECTORY_ACCESS_DENIED  = 4
              DIRECTORY_ALREADY_EXISTS = 5
              PATH_NOT_FOUND           = 6
              UNKNOWN_ERROR            = 7
              NOT_SUPPORTED_BY_GUI     = 8
              WRONG_PARAMETER          = 9
              OTHERS                   = 10.
          IF SY-SUBRC <> 0.
          ENDIF.
        ENDIF.
*----------------------------------------------------------------*
*     DD Folder Creation
*----------------------------------------------------------------*
        CONCATENATE LV_DIR2 LV_ARC_DD INTO LV_ARCHIVE_DIR2_DD.
*        CONDENSE LV_ARCHIVE_DIR2_DD  NO-GAPS.
          SHIFT LV_archive_DIR2_dd RIGHT DELETING TRAILING space.
  SHIFT lv_archive_dir2_dd LEFT DELETING LEADING SPACE.

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
          EXPORTING
            DIRECTORY            = LV_ARCHIVE_DIR2_DD
          RECEIVING
            RESULT               = LV_RESULT2
          EXCEPTIONS
            CNTL_ERROR           = 1
            ERROR_NO_GUI         = 2
            WRONG_PARAMETER      = 3
            NOT_SUPPORTED_BY_GUI = 4
            OTHERS               = 5.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

        ELSE.

          IF LV_RESULT2 IS INITIAL.

            CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE
              EXPORTING
                DIRECTORY                = LV_ARCHIVE_DIR2_DD
              CHANGING
                RC                       = LV_RC2
              EXCEPTIONS
                DIRECTORY_CREATE_FAILED  = 1
                CNTL_ERROR               = 2
                ERROR_NO_GUI             = 3
                DIRECTORY_ACCESS_DENIED  = 4
                DIRECTORY_ALREADY_EXISTS = 5
                PATH_NOT_FOUND           = 6
                UNKNOWN_ERROR            = 7
                NOT_SUPPORTED_BY_GUI     = 8
                WRONG_PARAMETER          = 9
                OTHERS                   = 10.
            IF SY-SUBRC <> 0.
            ENDIF.
          ENDIF.
*----------------------------------------------------------------*
*     RTGS Folder Creation
*----------------------------------------------------------------*
          CONCATENATE LV_DIR2 LV_ARC_RTGS INTO LV_ARCHIVE_DIR2_RTGS.
*          CONDENSE LV_ARCHIVE_DIR2_RTGS  NO-GAPS.
            SHIFT LV_archive_DIR2_rtgs RIGHT DELETING TRAILING space.
  SHIFT lv_archive_dir2_rtgs LEFT DELETING LEADING SPACE.

          CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
            EXPORTING
              DIRECTORY            = LV_ARCHIVE_DIR2_RTGS
            RECEIVING
              RESULT               = LV_RESULT2
            EXCEPTIONS
              CNTL_ERROR           = 1
              ERROR_NO_GUI         = 2
              WRONG_PARAMETER      = 3
              NOT_SUPPORTED_BY_GUI = 4
              OTHERS               = 5.
          IF SY-SUBRC <> 0.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

          ELSE.

            IF LV_RESULT2 IS INITIAL.

              CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE
                EXPORTING
                  DIRECTORY                = LV_ARCHIVE_DIR2_RTGS
                CHANGING
                  RC                       = LV_RC2
                EXCEPTIONS
                  DIRECTORY_CREATE_FAILED  = 1
                  CNTL_ERROR               = 2
                  ERROR_NO_GUI             = 3
                  DIRECTORY_ACCESS_DENIED  = 4
                  DIRECTORY_ALREADY_EXISTS = 5
                  PATH_NOT_FOUND           = 6
                  UNKNOWN_ERROR            = 7
                  NOT_SUPPORTED_BY_GUI     = 8
                  WRONG_PARAMETER          = 9
                  OTHERS                   = 10.
              IF SY-SUBRC <> 0.
              ENDIF.
            ENDIF.
*----------------------------------------------------------------*
*    NEFT Folder Creation
*----------------------------------------------------------------*
            CONCATENATE LV_DIR2 LV_ARC_NEFT INTO LV_ARCHIVE_DIR2_NEFT.
*            CONDENSE LV_ARCHIVE_DIR2_NEFT  NO-GAPS.
              SHIFT LV_archive_DIR2_neft RIGHT DELETING TRAILING space.
  SHIFT lv_archive_dir2_neft LEFT DELETING LEADING SPACE.

            CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
              EXPORTING
                DIRECTORY            = LV_ARCHIVE_DIR2_NEFT
              RECEIVING
                RESULT               = LV_RESULT2
              EXCEPTIONS
                CNTL_ERROR           = 1
                ERROR_NO_GUI         = 2
                WRONG_PARAMETER      = 3
                NOT_SUPPORTED_BY_GUI = 4
                OTHERS               = 5.
            IF SY-SUBRC <> 0.
              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

            ELSE.

              IF LV_RESULT2 IS INITIAL.

                CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE
                  EXPORTING
                    DIRECTORY                = LV_ARCHIVE_DIR2_NEFT
                  CHANGING
                    RC                       = LV_RC2
                  EXCEPTIONS
                    DIRECTORY_CREATE_FAILED  = 1
                    CNTL_ERROR               = 2
                    ERROR_NO_GUI             = 3
                    DIRECTORY_ACCESS_DENIED  = 4
                    DIRECTORY_ALREADY_EXISTS = 5
                    PATH_NOT_FOUND           = 6
                    UNKNOWN_ERROR            = 7
                    NOT_SUPPORTED_BY_GUI     = 8
                    WRONG_PARAMETER          = 9
                    OTHERS                   = 10.
                IF SY-SUBRC <> 0.
                ENDIF.
              ENDIF.
*----------------------------------------------------------------*
              LOOP AT L_ITAB_FILES2 INTO L_WA_FILES2.

                IF L_WA_FILES2-FILENAME CS 'RTGS'.
                  CLEAR: LV_SOURCE2,
                         LV_DEST2.

                  CONCATENATE LV_DIR2 '\' L_WA_FILES2-FILENAME INTO LV_SOURCE2.
                  CONCATENATE LV_ARCHIVE_DIR2_RTGS '\' L_WA_FILES2-FILENAME  INTO LV_DEST2.

                  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_COPY
                    EXPORTING
                      SOURCE               = LV_SOURCE2
                      DESTINATION          = LV_DEST2
                      OVERWRITE            = 'X'
                    EXCEPTIONS
                      CNTL_ERROR           = 1
                      ERROR_NO_GUI         = 2
                      WRONG_PARAMETER      = 3
                      DISK_FULL            = 4
                      ACCESS_DENIED        = 5
                      FILE_NOT_FOUND       = 6
                      DESTINATION_EXISTS   = 7
                      UNKNOWN_ERROR        = 8
                      PATH_NOT_FOUND       = 9
                      DISK_WRITE_PROTECT   = 10
                      DRIVE_NOT_READY      = 11
                      NOT_SUPPORTED_BY_GUI = 12
                      OTHERS               = 13.
                  IF SY-SUBRC <> 0.
                    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                  ELSE.
                    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
                      EXPORTING
                        FILENAME             = LV_SOURCE2
                      CHANGING
                        RC                   = LV_RC2
                      EXCEPTIONS
                        FILE_DELETE_FAILED   = 1
                        CNTL_ERROR           = 2
                        ERROR_NO_GUI         = 3
                        FILE_NOT_FOUND       = 4
                        ACCESS_DENIED        = 5
                        UNKNOWN_ERROR        = 6
                        NOT_SUPPORTED_BY_GUI = 7
                        WRONG_PARAMETER      = 8
                        OTHERS               = 9.
                    IF SY-SUBRC <> 0.
                      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                    ENDIF.
                  ENDIF.
*----------------------------------------------------------------*
                ELSEIF L_WA_FILES2-FILENAME CS 'CHW'.
                  CLEAR: LV_SOURCE2,
                         LV_DEST2.

                  CONCATENATE LV_DIR2 '\' L_WA_FILES2-FILENAME  INTO LV_SOURCE2.
                  CONCATENATE LV_ARCHIVE_DIR2_CCH '\' L_WA_FILES2-FILENAME  INTO LV_DEST2.

                  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_COPY
                    EXPORTING
                      SOURCE               = LV_SOURCE2
                      DESTINATION          = LV_DEST2
                      OVERWRITE            = 'X'
                    EXCEPTIONS
                      CNTL_ERROR           = 1
                      ERROR_NO_GUI         = 2
                      WRONG_PARAMETER      = 3
                      DISK_FULL            = 4
                      ACCESS_DENIED        = 5
                      FILE_NOT_FOUND       = 6
                      DESTINATION_EXISTS   = 7
                      UNKNOWN_ERROR        = 8
                      PATH_NOT_FOUND       = 9
                      DISK_WRITE_PROTECT   = 10
                      DRIVE_NOT_READY      = 11
                      NOT_SUPPORTED_BY_GUI = 12
                      OTHERS               = 13.
                  IF SY-SUBRC <> 0.
                    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                  ELSE.
                    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
                      EXPORTING
                        FILENAME             = LV_SOURCE2
                      CHANGING
                        RC                   = LV_RC2
                      EXCEPTIONS
                        FILE_DELETE_FAILED   = 1
                        CNTL_ERROR           = 2
                        ERROR_NO_GUI         = 3
                        FILE_NOT_FOUND       = 4
                        ACCESS_DENIED        = 5
                        UNKNOWN_ERROR        = 6
                        NOT_SUPPORTED_BY_GUI = 7
                        WRONG_PARAMETER      = 8
                        OTHERS               = 9.
                    IF SY-SUBRC <> 0.
                      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                    ENDIF.
                  ENDIF.
*----------------------------------------------------------------*
                ELSEIF L_WA_FILES2-FILENAME CS 'NEFT'.
                  CLEAR: LV_SOURCE2,
                         LV_DEST2.

                  CONCATENATE LV_DIR2 '\' L_WA_FILES2-FILENAME  INTO LV_SOURCE2.
                  CONCATENATE LV_ARCHIVE_DIR2_NEFT '\' L_WA_FILES2-FILENAME  INTO LV_DEST2.

                  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_COPY
                    EXPORTING
                      SOURCE               = LV_SOURCE2
                      DESTINATION          = LV_DEST2
                      OVERWRITE            = 'X'
                    EXCEPTIONS
                      CNTL_ERROR           = 1
                      ERROR_NO_GUI         = 2
                      WRONG_PARAMETER      = 3
                      DISK_FULL            = 4
                      ACCESS_DENIED        = 5
                      FILE_NOT_FOUND       = 6
                      DESTINATION_EXISTS   = 7
                      UNKNOWN_ERROR        = 8
                      PATH_NOT_FOUND       = 9
                      DISK_WRITE_PROTECT   = 10
                      DRIVE_NOT_READY      = 11
                      NOT_SUPPORTED_BY_GUI = 12
                      OTHERS               = 13.
                  IF SY-SUBRC <> 0.
                    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                  ELSE.
                    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
                      EXPORTING
                        FILENAME             = LV_SOURCE2
                      CHANGING
                        RC                   = LV_RC2
                      EXCEPTIONS
                        FILE_DELETE_FAILED   = 1
                        CNTL_ERROR           = 2
                        ERROR_NO_GUI         = 3
                        FILE_NOT_FOUND       = 4
                        ACCESS_DENIED        = 5
                        UNKNOWN_ERROR        = 6
                        NOT_SUPPORTED_BY_GUI = 7
                        WRONG_PARAMETER      = 8
                        OTHERS               = 9.
                    IF SY-SUBRC <> 0.
                      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                    ENDIF.
                  ENDIF.
*----------------------------------------------------------------*
                ELSEIF L_WA_FILES2-FILENAME CS 'DD'.
                  CLEAR: LV_SOURCE2,
                         LV_DEST2.

                  CONCATENATE LV_DIR2 '\' L_WA_FILES2-FILENAME  INTO LV_SOURCE2.
                  CONCATENATE LV_ARCHIVE_DIR2_DD '\' L_WA_FILES2-FILENAME  INTO LV_DEST2.

                  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_COPY
                    EXPORTING
                      SOURCE               = LV_SOURCE2
                      DESTINATION          = LV_DEST2
                      OVERWRITE            = 'X'
                    EXCEPTIONS
                      CNTL_ERROR           = 1
                      ERROR_NO_GUI         = 2
                      WRONG_PARAMETER      = 3
                      DISK_FULL            = 4
                      ACCESS_DENIED        = 5
                      FILE_NOT_FOUND       = 6
                      DESTINATION_EXISTS   = 7
                      UNKNOWN_ERROR        = 8
                      PATH_NOT_FOUND       = 9
                      DISK_WRITE_PROTECT   = 10
                      DRIVE_NOT_READY      = 11
                      NOT_SUPPORTED_BY_GUI = 12
                      OTHERS               = 13.
                  IF SY-SUBRC <> 0.
                    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                  ELSE.
                    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
                      EXPORTING
                        FILENAME             = LV_SOURCE2
                      CHANGING
                        RC                   = LV_RC2
                      EXCEPTIONS
                        FILE_DELETE_FAILED   = 1
                        CNTL_ERROR           = 2
                        ERROR_NO_GUI         = 3
                        FILE_NOT_FOUND       = 4
                        ACCESS_DENIED        = 5
                        UNKNOWN_ERROR        = 6
                        NOT_SUPPORTED_BY_GUI = 7
                        WRONG_PARAMETER      = 8
                        OTHERS               = 9.
                    IF SY-SUBRC <> 0.
                      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                    ENDIF.
                  ENDIF.
*----------------------------------------------------------------*
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  RCODE = 0.

ENDFORM.                    " check_files_exist_outfolder
*&---------------------------------------------------------------------*
*&      Form  gui_directory_browse
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0041   text
*      <--P_PA_CEMS  text
*----------------------------------------------------------------------*
FORM GUI_DIRECTORY_BROWSE  USING P_TITLE TYPE STRING
                           CHANGING P_DIR_PATH.
  DATA: LV_DIR_PATH TYPE STRING.

  LV_DIR_PATH = P_DIR_PATH.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE
    EXPORTING
      WINDOW_TITLE         = P_TITLE
      INITIAL_FOLDER       = LV_DIR_PATH
    CHANGING
      SELECTED_FOLDER      = LV_DIR_PATH
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  P_DIR_PATH = LV_DIR_PATH.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT = G_VARIANT
      I_SAVE     = G_SAVE
    IMPORTING
      E_EXIT     = G_EXIT
      ES_VARIANT = GX_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.
  IF SY-SUBRC = 2.
    MESSAGE ID SY-MSGID TYPE 'S'      NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF G_EXIT = SPACE.
      P_DIR_PATH = GX_VARIANT-VARIANT.
    ENDIF.
  ENDIF.

ENDFORM.                    " gui_directory_browse
*&---------------------------------------------------------------------*
*&      Form  modify_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN .

  GV_ADMINID = SY-UNAME."'10106'."'DEVINDIA'.
  GV_ADMINID2 = 'XXXX'.

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'ABC'.
      SCREEN-ACTIVE = 0.
      MODIFY SCREEN.
      CLEAR SCREEN.
    ENDIF.
  ENDLOOP.

  LOOP AT SCREEN.

    IF P_CHEQ = '  '.
      IF SCREEN-GROUP1 = 'MD2' .
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF P_DD = '  '.
      IF SCREEN-GROUP1 = 'MD3' .
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.


    IF P_CEMS = '  '.
      IF SCREEN-GROUP1 = 'MD1' .
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSE.

      IF SY-UNAME EQ GV_ADMINID OR SY-UNAME EQ GV_ADMINID2.
        IF SCREEN-GROUP1 = 'MD1' .
          SCREEN-ACTIVE = '1'.
          MODIFY SCREEN.
        ENDIF.
        IF SCREEN-NAME = 'PA_PIN'.
          SCREEN-INVISIBLE = '1'.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF SCREEN-NAME = 'PA_PIN' OR SCREEN-NAME = 'PA_CRTID'.
          SCREEN-INVISIBLE = '1'.
          MODIFY SCREEN.
        ENDIF.

        IF SCREEN-NAME = 'PA_PIN' OR SCREEN-NAME = 'PA_CRTID'.
          SCREEN-INPUT = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " modify_screen
*&---------------------------------------------------------------------*
*&      Form  SET_PAYMTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_PAYMTS .

  DATA: LV_BELNR TYPE BELNR_D,
        LV_VBLNR TYPE VBLNR.

  SORT IT_REGUH_PM BY RZAWE.

  DATA LV_AMTLIM TYPE RWBTR.

  LOOP AT IT_REGUH_PM INTO WA_REGUH_PM.

    READ TABLE IREGUP WITH KEY LAUFD = WA_REGUH_PM-LAUFD
                              LAUFI = WA_REGUH_PM-LAUFI
                              XVORL = WA_REGUH_PM-XVORL
                              ZBUKR = WA_REGUH_PM-ZBUKR
                              LIFNR = WA_REGUH_PM-LIFNR
                              KUNNR = WA_REGUH_PM-KUNNR
                              EMPFG = WA_REGUH_PM-EMPFG
                              VBLNR = WA_REGUH_PM-VBLNR.
    IF SY-SUBRC EQ 0.
      LV_BELNR = IREGUP-BELNR.
      LV_VBLNR = IREGUP-VBLNR.
    ENDIF.

*   Prepare 'Mail to' party address tables
    SELECT SINGLE XCPDK PSTLZ FROM LFA1
    INTO (LFA1-XCPDK, LFA1-PSTLZ)
    WHERE LIFNR = WA_REGUH_PM-LIFNR.

*    IF WA_REGUH_PM-WAERS EQ 'INR'.
*      IF  WA_REGUH_PM-RZAWE CA CHW_PM.
*        PERFORM FILL_CHW USING WA_REGUH_PM .
*
*      ELSEIF WA_REGUH_PM-RZAWE CA DD_PM.
*        PERFORM FILL_DD USING WA_REGUH_PM.
*
*      ELSEIF WA_REGUH_PM-RZAWE CA RTGS_PM."neft_pm.
*
*        READ TABLE IT_ZDBSPN
*        INTO WA_ZDBSPN
*        WITH KEY ZLSCH = WA_REGUH_PM-RZAWE
*                 LAND1 = WA_REGUH_PM-LAND1
*                 WAERS = WA_REGUH_PM-WAERS.
*
*        LV_AMTLIM = WA_ZDBSPN-ZDBSAMT.
*
*        IF WA_REGUH_PM-BKREF NE 'NEFT'.
*          DATA LV_RWBTR TYPE RWBTR.
*          LV_RWBTR = ABS( WA_REGUH_PM-RWBTR ).
*          IF LV_RWBTR LT LV_AMTLIM.
*            PERFORM FILL_NEFT USING WA_REGUH_PM.
*          ELSE.
*            PERFORM FILL_RTGS USING WA_REGUH_PM.
*          ENDIF.
*        ELSEIF WA_REGUH_PM-BKREF EQ 'NEFT'.
*
*          PERFORM FILL_NEFT USING WA_REGUH_PM.
*
*        ENDIF.
**      ELSEIF wa_reguh_pm-rzawe CA rtgs_pm.
**        DATA lv_rwbtr TYPE rwbtr.
**        lv_rwbtr = ABS( wa_reguh_pm-rwbtr ).
**        IF lv_rwbtr LT 10000000.
**          PERFORM fill_neft USING wa_reguh_pm.
**        ELSE.
**          PERFORM fill_rtgs USING wa_reguh_pm.
**        ENDIF.
*      ENDIF.
*    ELSE.
*      PERFORM FILL_EFT USING WA_REGUH_PM.
*    ENDIF.

*-----Indian Currency---------------------------------------*
    IF WA_REGUH_PM-WAERS EQ 'INR'.
*-----------------------------------------------------------*
      IF P_CHEQ IS NOT INITIAL AND P_DD IS INITIAL.
        PERFORM FILL_CHW USING WA_REGUH_PM .

      ELSEIF P_CHEQ IS INITIAL AND P_DD IS NOT INITIAL.
        PERFORM FILL_DD USING WA_REGUH_PM.

      ELSEIF P_CHEQ IS NOT INITIAL AND P_DD IS NOT INITIAL.
        PERFORM FILL_CHW USING WA_REGUH_PM .
        PERFORM FILL_DD USING WA_REGUH_PM.

      ELSEIF P_CHEQ IS INITIAL AND P_DD IS INITIAL.
        IF WA_REGUH_PM-RZAWE CA RTGS_PM OR WA_REGUH_PM-RZAWE CA NEFT_PM .
          READ TABLE IT_ZDBSPN
          INTO WA_ZDBSPN
          WITH KEY ZLSCH = WA_REGUH_PM-RZAWE
                  LAND1 = WA_REGUH_PM-LAND1
                  WAERS = WA_REGUH_PM-WAERS.

          LV_AMTLIM = WA_ZDBSPN-ZDBSAMT.

          IF WA_REGUH_PM-BKREF NE 'NEFT'.
            DATA LV_RWBTR TYPE RWBTR.
            LV_RWBTR = ABS( WA_REGUH_PM-RWBTR ).
            IF LV_RWBTR LT LV_AMTLIM.
              PERFORM FILL_NEFT USING WA_REGUH_PM.
            ELSE.
              PERFORM FILL_RTGS USING WA_REGUH_PM.
            ENDIF.
          ELSEIF WA_REGUH_PM-BKREF EQ 'NEFT'.
            PERFORM FILL_NEFT USING WA_REGUH_PM.
          ENDIF.
        ENDIF.
      ENDIF.
*--Foreign Currencies------------------------------------------------*
    ELSE.
      PERFORM FILL_EFT USING WA_REGUH_PM.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " SET_PAYMTS
*&---------------------------------------------------------------------*
*&      Form  APPEND_LOG_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_p_wa_reguh_pm  text
*----------------------------------------------------------------------*
FORM APPEND_LOG_DISPLAY  USING P_P_WA_REGUH_PM TYPE TY_REGUH_PM.
  CLEAR IT_LIST.
  MOVE-CORRESPONDING P_P_WA_REGUH_PM TO IT_LIST.

  IT_LIST-RWBTR = P_P_WA_REGUH_PM-RWBTR.

  CONCATENATE P_P_WA_REGUH_PM-ZNME1 P_P_WA_REGUH_PM-ZNME2 INTO IT_LIST-NAME
                                      SEPARATED BY SPACE.

  SELECT SINGLE LAND1 INTO T001-LAND1 FROM T001
          WHERE BUKRS = P_P_WA_REGUH_PM-ZBUKR.

  SELECT SINGLE TEXT1 INTO IT_LIST-TEXT1
                  FROM T042Z WHERE LAND1 = T001-LAND1
                               AND ZLSCH = P_P_WA_REGUH_PM-RZAWE.

  APPEND IT_LIST.
ENDFORM.                    " APPEND_LOG_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  FRNTEND_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_NEFT_FINAL  text
*      -->P_LV_FILENAME  text
*      -->P_PA_NEFT  text
*----------------------------------------------------------------------*
FORM FRNTEND_DOWNLOAD  TABLES   PITAB_TABLE TYPE STANDARD TABLE
                       USING    P_FILENAME
                                P_OUTPUTFOLDER.
  DATA: LV_FILENAME TYPE STRING.

  LV_FILENAME = P_FILENAME.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = LV_FILENAME
      FILETYPE                = 'ASC'
    TABLES
      DATA_TAB                = PITAB_TABLE
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      OTHERS                  = 22.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IT_FILIST-FULLNAME = LV_FILENAME.
    IT_FILIST-OUTNAME = LV_FILENAME.
    IT_FILIST-OUTPUTFOLDER = P_OUTPUTFOLDER.
    APPEND  IT_FILIST.
    CLEAR IT_FILIST.
  ENDIF.
ENDFORM.                    " FRNTEND_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  CHK_N_UPDATE_ERROR_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_NEFT_DETAILS_SEND_ACC_NUM  text
*      -->P_p_wa_reguh_pm_VBLNR  text
*      -->P_5147   text
*----------------------------------------------------------------------*
FORM CHK_N_UPDATE_ERROR_LOG  USING  P_CHECK_VALUE
                                    P_LV_VBLNR
                                    ERR_MSG.
  IF P_CHECK_VALUE IS INITIAL.
    IT_ERR-VBLNR = P_LV_VBLNR.
    IT_ERR-ERROR = ERR_MSG.
    APPEND IT_ERR.
  ENDIF.
ENDFORM.                    " CHK_N_UPDATE_ERROR_LOG
*&---------------------------------------------------------------------*
*&      Form  GET_COMPANY_ID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_p_wa_reguh_pm_ZBUKR  text
*      <--P_WA_NEFT_HDR_SEND_ID  text
*----------------------------------------------------------------------*
FORM GET_COMPANY_ID  USING    P_WA_REGUH_PM_ZBUKR
                     CHANGING P_WA_NEFT_HDR_SEND_ID.
  READ TABLE IT_ZDBSCP ASSIGNING <FS_ZDBSCP>
                   WITH KEY BUKRS = P_WA_REGUH_PM_ZBUKR.

  IF SY-SUBRC EQ 0.
    P_WA_NEFT_HDR_SEND_ID = <FS_ZDBSCP>-ZDBSCI.
  ENDIF.

  UNASSIGN <FS_ZDBSCP>.

ENDFORM.                    " GET_COMPANY_ID
*---------------------------------------------------------------------*
*       FORM write_batch_summary                                      *
*---------------------------------------------------------------------*
*       Write batch summary record to file
*---------------------------------------------------------------------*
FORM WRITE_BATCH_SUMMARY.
  DATA: T_TEMP(18).

  IF SUMMARY_FLAG = ' ' AND TOTAL_TRAN <> 0.
    CLEAR BSRTAB.
    BSRTAB-REC_TYPE = '099'.
    PERFORM FORMAT_DATE USING SY-DATUM
                      CHANGING BSRTAB-CREA_DATE.
    BSRTAB-CREA_TIME = SY-UZEIT.
    BSRTAB-TOTAL_TRAN = TOTAL_TRAN.
    CLEAR T_TEMP.

*   Output batch summary record
    CLEAR OUTTAB.
    OUTTAB-LINE_DTLS = BSRTAB-REC_TYPE.
    OUTTAB-LINE_DTLS+3 = BSRTAB-BSR_FILLER1.
    OUTTAB-LINE_DTLS+6 = BSRTAB-CREA_DATE.
    OUTTAB-LINE_DTLS+14 = BSRTAB-CREA_TIME.
    OUTTAB-LINE_DTLS+20 = BSRTAB-TOTAL_TRAN.
    OUTTAB-LINE_DTLS+28 = BSRTAB-TOTAL_PAYM.
    OUTTAB-LINE_DTLS+46 = BSRTAB-BSR_FILLER2.

    PERFORM APPEND_REC USING : OUTTAB.

    CLEAR TOTAL_TRAN.
    CLEAR TOTAL_PAYM.
    SUMMARY_FLAG = 'X'.
  ENDIF.

ENDFORM.                    "write_batch_summary
*----------------------------------------------------------------------*
*       FORM write_file_header(PRIME)                                  *
*----------------------------------------------------------------------*
*       Write File Header Record to file(PRIME
*-----------------------------------------------------------------------
FORM WRITE_DFILE_HEADER.

  CLEAR DTABH.
  DTABH-RECTYP = '000'.
  DTABH-CRDATE = SY-DATUM.
  DTABH-CRTIME = SY-UZEIT.
  DTABH-REFER = 'INDOFILL' .
  DTABH-BUKRS = ZDBSCP-ZDBSCI.

  DTABH-DESC = DTABH-REFER.

  ZDBSPR-ZDBSGN = ZDBSPR-ZDBSGN + 1.

  PERFORM CHECK_VALID_BATCH_NUMBER USING 'G'
                         CHANGING ZDBSPR-ZDBSBN ZDBSPR-ZDBSGN.
  DTABH-ORGAN =  '2009'.
  DTABH-BANK = '7171'.

* Append File Header Record into OUTTAB
  CLEAR OUTTAB.
  OUTTAB-LINE_DTLS       = DTABH-RECTYP.
  OUTTAB-LINE_DTLS+3     = DTABH-CRDATE.
  OUTTAB-LINE_DTLS+11    = DTABH-CRTIME.
  OUTTAB-LINE_DTLS+17    = DTABH-REFER.
  OUTTAB-LINE_DTLS+33    = ZDBSCP-ZDBSCI.
  OUTTAB-LINE_DTLS+45    = DTABH-ENTIT.
  OUTTAB-LINE_DTLS+115   = DTABH-DESC.
  OUTTAB-LINE_DTLS+135   = 'I'.
  OUTTAB-LINE_DTLS+757   = DTABH-ORGAN.
  OUTTAB-LINE_DTLS+761   = DTABH-BANK.
  OUTTAB-LINE_DTLS+761   = '    '.
  OUTTAB-LINE_DTLS+765 = DTABH-FILL3.

  PERFORM APPEND_REC USING : OUTTAB.

ENDFORM.                                        " write_pfile_header.
*&---------------------------------------------------------------------*
*&      Form  SAVE_DD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_W_ROWS_LEFT  text
*----------------------------------------------------------------------*
FORM SAVE_DD  CHANGING P_W_ROWS_LEFT.

  DATA: W_TEMP(20), TEMP_PAYM LIKE REGUH-RWBTR.
  DATA: W_ROWSLEFT TYPE I.
  DATA: TABLINES TYPE I.
  DATA: T_TEMP(17) TYPE C.
  DATA: SBRNCH(3).

  IF ZDBSTL-ZDBSEL = 0.   "This need to be changed for Prime
    EXIT.
  ENDIF.

  DESCRIBE TABLE DTAB LINES TABLINES.
  IF TABLINES = 0.
    EXIT.
  ENDIF.

  FILE_OPEN_FLAG = ' '.
  SUMMARY_FLAG = ' '.

* Initialize number of rows left to populate as Maximum Primelimit
  W_ROWSLEFT = ZDBSTL-ZDBSEL.

  SORT DTAB BY PAYTYP CUSREF CURRCY.

  LOOP AT DTAB.

    AT FIRST.
      MAXEFLAG = 'X'.
      MAXCFLAG = 'X'.

      PERFORM OPEN_FILE USING 'd'.
      PERFORM WRITE_DFILE_HEADER.
      FILE_OPEN_FLAG = 'X'.
      SUMMARY_FLAG = 'X'.
    ENDAT.
*---------------------------------------------*
*   Populate 010 record
*---------------------------------------------*
    CLEAR BUFFERTAB.
    BUFFERTAB-LINE_DTLS    = DTAB-RECTYP.
    BUFFERTAB-LINE_DTLS+3  = DTAB-PAYTYP.
    BUFFERTAB-LINE_DTLS+6  = DTAB-CHEC_NO. " dtab-cusref.
    BUFFERTAB-LINE_DTLS+22 = DTAB-CURRCY.
    BUFFERTAB-LINE_DTLS+25(17) = DTAB-AMOUNT.
    BUFFERTAB-LINE_DTLS+42 = DTAB-VALUT.
    BUFFERTAB-LINE_DTLS+50 = DTAB-RECVD.
    BUFFERTAB-LINE_DTLS+120 = DTAB-TEXT1.
    BUFFERTAB-LINE_DTLS+160 = DTAB-TEXT2.
    BUFFERTAB-LINE_DTLS+200 = DTAB-TEXT3.
    BUFFERTAB-LINE_DTLS+240 = DTAB-TEXT4.
    BUFFERTAB-LINE_DTLS+280 = DTAB-SCURR.
    BUFFERTAB-LINE_DTLS+283 = DTAB-DRACCT.
    BUFFERTAB-LINE_DTLS+317 = DTAB-RCACCT.
    BUFFERTAB-LINE_DTLS+359 = DTAB-BANKC.
    BUFFERTAB-LINE_DTLS+362 = DTAB-DELIND.
    BUFFERTAB-LINE_DTLS+363 = DTAB-MAILTO.
    BUFFERTAB-LINE_DTLS+503 = DTAB-POSTAL.
    BUFFERTAB-LINE_DTLS+511 = DTAB-PAYDTL.
    BUFFERTAB-LINE_DTLS+863 = DTAB-PRATLOC.
    BUFFERTAB-LINE_DTLS+879 = DTAB-DDAREF.
    BUFFERTAB-LINE_DTLS+928 = DTAB-ORIGCQ.

    IF DTAB-ORIGAMT IS INITIAL.
      T_TEMP = '000000000000.0000'.
    ELSE.
      CLEAR T_TEMP.
      WRITE DTAB-ORIGAMT TO T_TEMP
            NO-GROUPING NO-SIGN DECIMALS 4 RIGHT-JUSTIFIED.
      OVERLAY T_TEMP WITH '000000000000.0000'.

    ENDIF.

    BUFFERTAB-LINE_DTLS+938 = T_TEMP.
    BUFFERTAB-LINE_DTLS+975 = DTAB-TCODE.
    BUFFERTAB-LINE_DTLS+977 = DTAB-PARTI.
    BUFFERTAB-LINE_DTLS+989 = DTAB-ADVISE.
    BUFFERTAB-LINE_DTLS+990 = DTAB-PYBLAT.
    BUFFERTAB-LINE_DTLS+1006 = DTAB-FILLER.
    APPEND BUFFERTAB.
    BUFFER_COUNT = BUFFER_COUNT + 1.
    TEMP_PAYM = TEMP_PAYM + DTAB-AMOUNT.
*---------------------------------------------*
*   Populate 020 record
*---------------------------------------------*
    LOOP AT IT_INV1.
      IF IT_INV1-CUST_VBLNR = DTAB-CUSREF.
        CLEAR BUFFERTAB.
        BUFFERTAB-LINE_DTLS     = '020'.
        BUFFERTAB-LINE_DTLS+3   = GV_FILLER1.
        BUFFERTAB-LINE_DTLS+19  = IT_INV1-PAYM_DTLS.
        BUFFERTAB-LINE_DTLS+1420  = GV_FILLER2.
        APPEND BUFFERTAB.
*       Accumulate 020 data for batch summary record
        BUFFER_COUNT = BUFFER_COUNT + 1.
      ENDIF.
    ENDLOOP.
*---------------------------------------------------*
*    Buffer Count more than the row limit
*---------------------------------------------------*
    IF BUFFER_COUNT > W_ROWSLEFT.
*     Wite batch summary and close current file
      PERFORM WRITE_BATCH_SUMMARY.
      PERFORM CLOSE_DD_FILE.
      CLEAR OUTTAB. REFRESH OUTTAB.
      FILE_OPEN_FLAG = ' '.
      MAXEFLAG = 'X'.

*     Open a new EFT file and write file header
      PERFORM OPEN_FILE USING 'd'.
      FILE_OPEN_FLAG = 'X'.
      W_ROWSLEFT = ZDBSTL-ZDBSEL.
      PERFORM WRITE_DFILE_HEADER.
      IF SUMMARY_FLAG = 'X'.
        DESCRIBE TABLE BUFFERTAB2 LINES W_LINE.

        SUMMARY_FLAG = ' '.
      ENDIF.
*---------------------------------------------*
*     Write 010 and 020 records to file
*---------------------------------------------*
      PERFORM WRITE_PAYMENT_DETAILS.

      W_ROWSLEFT = W_ROWSLEFT - BUFFER_COUNT.
      TOTAL_TRAN = TOTAL_TRAN + BUFFER_COUNT.
      TOTAL_PAYM = TOTAL_PAYM + TEMP_PAYM.

      CLEAR BUFFERTAB. REFRESH BUFFERTAB.
      CLEAR BUFFERTAB2. REFRESH BUFFERTAB2.

    ELSEIF BUFFER_COUNT < W_ROWSLEFT.
      IF FILE_OPEN_FLAG = ' '.
        MAXEFLAG = 'X'.
        PERFORM OPEN_FILE USING 'd'.
        FILE_OPEN_FLAG = 'X'.
        PERFORM WRITE_DFILE_HEADER.
      ENDIF.

      IF SUMMARY_FLAG = 'X'.
        DESCRIBE TABLE BUFFERTAB2 LINES W_LINE.
        SUMMARY_FLAG = ' '.
      ENDIF.
*---------------------------------------------*
*     Write 010 and 020 records to file
*---------------------------------------------*
      PERFORM WRITE_PAYMENT_DETAILS.

      W_ROWSLEFT = W_ROWSLEFT - BUFFER_COUNT.
      TOTAL_TRAN = TOTAL_TRAN + BUFFER_COUNT.
      TOTAL_PAYM = TOTAL_PAYM + TEMP_PAYM.

      CLEAR BUFFERTAB. REFRESH BUFFERTAB.
      CLEAR BUFFERTAB2. REFRESH BUFFERTAB2.

    ELSEIF BUFFER_COUNT = W_ROWSLEFT.
      IF FILE_OPEN_FLAG = ''.
        MAXEFLAG = 'X'.

        PERFORM OPEN_FILE USING 'd'.
        FILE_OPEN_FLAG = 'X'.
        PERFORM WRITE_DFILE_HEADER.
      ENDIF.

      IF SUMMARY_FLAG = 'X'.
        DESCRIBE TABLE BUFFERTAB2 LINES W_LINE.
        SUMMARY_FLAG = ' '.
      ENDIF.
*---------------------------------------------*
*     Write 010 and 020 records to file
*---------------------------------------------*
      PERFORM WRITE_PAYMENT_DETAILS.

      TOTAL_TRAN = TOTAL_TRAN + BUFFER_COUNT.
      TOTAL_PAYM = TOTAL_PAYM + TEMP_PAYM.

      PERFORM WRITE_BATCH_SUMMARY.
      PERFORM CLOSE_DD_FILE.
      FILE_OPEN_FLAG = ' '.

      CLEAR BUFFERTAB. REFRESH BUFFERTAB.
      W_ROWSLEFT = ZDBSTL-ZDBSEL.

      CLEAR OUTTAB. REFRESH OUTTAB.
      CLEAR BUFFERTAB2. REFRESH BUFFERTAB2.

    ENDIF.

    CLEAR BUFFER_COUNT.
    CLEAR TEMP_PAYM.

    AT LAST.
      PERFORM WRITE_BATCH_SUMMARY_D.
      PERFORM CLOSE_DD_FILE.
    ENDAT.

  ENDLOOP.

  P_W_ROWS_LEFT = W_ROWSLEFT.
ENDFORM.                    " SAVE_DD
*&---------------------------------------------------------------------*
*&      Form  FILL_DD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_REGUH_PM  text
*----------------------------------------------------------------------*
FORM FILL_DD USING P_WA_REGUH_PM TYPE TY_REGUH_PM.
  DATA: W_MACRO(140),
        T_STATUS(1),
        T_TEMP(13),
        GV_LENGTH(2).

  DATA: BEGIN OF IMACRO OCCURS 1.
          INCLUDE STRUCTURE ZPDETL0.
  DATA: END OF IMACRO.

  DATA: BEGIN OF IMICRO OCCURS 1.
          INCLUDE STRUCTURE ZPDETL.
  DATA: END OF IMICRO.

  CLEAR: DTAB , GV_ENAME.
  READ TABLE IT_ZDBSPN INTO WA_ZDBSPN
  WITH KEY ZLSCH = P_WA_REGUH_PM-RZAWE
           LAND1 = P_WA_REGUH_PM-LAND1
           WAERS = P_WA_REGUH_PM-WAERS.
  GV_ENAME = WA_ZDBSPN-ZDBSMAIL.


****  Mandatory
*-----------------------------------------------*
*  1-Record Type
*-----------------------------------------------*
  DTAB-RECTYP     = '010'.
*-----------------------------------------------*
*  2-Payment Type
*-----------------------------------------------*
*  READ TABLE IT_ZDBSPN
*  INTO WA_ZDBSPN
*  WITH KEY ZLSCH = P_WA_REGUH_PM-RZAWE
*           LAND1 = P_WA_REGUH_PM-LAND1
*           WAERS = P_WA_REGUH_PM-WAERS.
*
*  DTAB-PAYTYP = WA_ZDBSPN-ZDBSPT.


   DTAB-PAYTYP = 'BCH'.

*-----------------------------------------------*
* 3-Customer Reference
*-----------------------------------------------*
  DTAB-CUSREF  = P_WA_REGUH_PM-VBLNR.
*-----------------------------------------------*
* 4-Payment Currency (DD-M)
*-----------------------------------------------*
  DTAB-CURRCY     = P_WA_REGUH_PM-WAERS.
  SELECT SINGLE * FROM   TCURC
            WHERE ISOCD = P_WA_REGUH_PM-WAERS.
  IF SY-SUBRC <> 0.
    IT_ERR-VBLNR = IREGUH-VBLNR.
    CONCATENATE 'Payment Currency '
                P_WA_REGUH_PM-WAERS
                'is not maintained in ISO currency Codes.'
                INTO IT_ERR-ERROR SEPARATED BY SPACE.
    APPEND IT_ERR.
    EXIT.
  ENDIF.
*-----------------------------------------------*
*   5-Payment amount (DD-M)
*-----------------------------------------------*
  IF P_WA_REGUH_PM-RWBTR = 0.
  ELSE.
    WRITE P_WA_REGUH_PM-RWBTR TO DTAB-AMOUNT NO-SIGN NO-GROUPING
     CURRENCY P_WA_REGUH_PM-WAERS DECIMALS 4 RIGHT-JUSTIFIED.
    OVERLAY DTAB-AMOUNT WITH '000000000000.0000'.
    TRANSLATE DTAB-AMOUNT USING ',.'.
    IF DTAB-AMOUNT(2) <> '00' OR
       DTAB-AMOUNT+15(2) <> '00' .
      IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
      CONCATENATE 'Payment amount'
                   DTAB-AMOUNT
                  'is invalid. '
                  INTO IT_ERR-ERROR SEPARATED BY SPACE.
      APPEND IT_ERR.
      EXIT.
    ENDIF.
  ENDIF.
*-----------------------------------------------*
*   6-Value Date(DD-M,GIRO-M)
*-----------------------------------------------*
  DTAB-VALUT    = PA_VALDT.
*-----------------------------------------------*
*   7-Receiving Party Name(DD-M)
*-----------------------------------------------*
  DATA:LV_PAYENAME(70).
  IF P_WA_REGUH_PM-LIFNR IS NOT INITIAL.
    IF P_WA_REGUH_PM-ZNME1 IS NOT INITIAL.
      CONCATENATE P_WA_REGUH_PM-ZNME1 P_WA_REGUH_PM-ZNME2 INTO DTAB-RECVD
                               SEPARATED BY SPACE.
    ELSE.
      CONCATENATE P_WA_REGUH_PM-NAME1 P_WA_REGUH_PM-NAME2 INTO DTAB-RECVD
                                SEPARATED BY SPACE.
    ENDIF.
  ELSE.
    SELECT * FROM PAYR INTO TABLE IT_PAYR WHERE ZBUKR = P_WA_REGUH_PM-ZBUKR
                                           AND HBKID = P_WA_REGUH_PM-HBKID
                                           AND HKTID = P_WA_REGUH_PM-HKTID
                                           AND RZAWE = P_WA_REGUH_PM-RZAWE.

    READ TABLE IT_PAYR WITH KEY  VBLNR =  P_WA_REGUH_PM-VBLNR.
    CONCATENATE IT_PAYR-ZNME1 IT_PAYR-ZNME2 INTO LV_PAYENAME SEPARATED BY SPACE.
    DTAB-RECVD = LV_PAYENAME.
  ENDIF.
  CLEAR LV_PAYENAME.
*-----------------------------------------------*
*    8-Client Text 1 - Postal Code
*-----------------------------------------------*
  IF P_WA_REGUH_PM-ZPSTL IS NOT INITIAL.
    DTAB-TEXT1 = P_WA_REGUH_PM-ZPSTL.
  ELSE.
    SELECT SINGLE * FROM ADRC
    WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
    IF SY-SUBRC EQ 0.
      DTAB-TEXT1 = ADRC-POST_CODE1.
    ENDIF.
  ENDIF.
  IF DTAB-TEXT1 IS INITIAL.
    DTAB-TEXT1 = 'XXXXXX'.
    CONDENSE DTAB-TEXT1.
  ENDIF.
*-----------------------------------------------*
*   9-Client Text 2 - Telephone or Fax
*-----------------------------------------------*
  IF P_WA_REGUH_PM-KUNNR <> ''.
    SELECT SINGLE TELF1 INTO DTAB-TEXT2 FROM KNA1
    WHERE KUNNR = P_WA_REGUH_PM-KUNNR.

  ELSEIF P_WA_REGUH_PM-LIFNR  <> ''.
    SELECT SINGLE TELF1 INTO DTAB-TEXT2 FROM LFA1
    WHERE LIFNR = P_WA_REGUH_PM-LIFNR.
  ENDIF.

  IF DTAB-TEXT2 IS INITIAL.
    IF P_WA_REGUH_PM-ZTLFX IS NOT INITIAL.
      DTAB-TEXT2 = P_WA_REGUH_PM-ZTLFX.
    ELSE.
      SELECT SINGLE * FROM ADRC
      WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
      IF SY-SUBRC EQ 0.
        DTAB-TEXT2 = ADRC-FAX_NUMBER.
      ENDIF.
    ENDIF.
  ENDIF.
*-----------------------------------------------*
*   10-Client Text 3 - Vendor Code
*-----------------------------------------------*
  IF P_WA_REGUH_PM-KUNNR <> ''.
    DTAB-TEXT3 = P_WA_REGUH_PM-KUNNR.
  ELSEIF P_WA_REGUH_PM-LIFNR  <> ''.
    DTAB-TEXT3 = P_WA_REGUH_PM-LIFNR.
  ENDIF.
*-----------------------------------------------*
*    11-Client Text 4
*-----------------------------------------------*
  DTAB-TEXT4 = DTAB-CUSREF.
*-----------------------------------------------*
*   12-Settlement Currency (DD-M)
*-----------------------------------------------*
  READ TABLE IT_T012K WITH KEY BUKRS = P_WA_REGUH_PM-ZBUKR
                               HBKID = P_WA_REGUH_PM-HBKID
                               HKTID = P_WA_REGUH_PM-HKTID.
  DTAB-SCURR   = IT_T012K-WAERS.

  SELECT SINGLE * FROM   TCURC
          WHERE ISOCD = IT_T012K-WAERS.
  IF SY-SUBRC <> 0.
    IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
    CONCATENATE 'Settlement Currency '
                 IT_T012K-WAERS
                'is not maintained in ISO currency Codes.'
                 INTO IT_ERR-ERROR SEPARATED BY SPACE.
    APPEND IT_ERR.
    EXIT.
  ENDIF.
*-----------------------------------------------*
*  13-Debit Account(DD-M)
*-----------------------------------------------*
  READ TABLE IT_T012K WITH KEY BUKRS = P_WA_REGUH_PM-ZBUKR
                             HBKID = P_WA_REGUH_PM-HBKID
                             HKTID = P_WA_REGUH_PM-HKTID.
  IF SY-SUBRC = 0.

    CLEAR UTAB. REFRESH UTAB.
    SPLIT P_WA_REGUH_PM-UBKNT AT '-' INTO TABLE UTAB.
    LOOP AT UTAB.
      CONCATENATE DTAB-DRACCT UTAB-WORD INTO DTAB-DRACCT.
    ENDLOOP.

    IF  DTAB-DRACCT = ''.
      IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
      CONCATENATE 'Settlement A/C Number for'
                  P_WA_REGUH_PM-NAME1
                  'is not maintained in settlement data.'
                  INTO IT_ERR-ERROR SEPARATED BY SPACE.
      APPEND IT_ERR.
      EXIT.
    ENDIF.
    IF DTAB-DRACCT CS 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
      IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
      CONCATENATE 'Dbit Account'
                  DTAB-DRACCT
                  'contains non-numeric values.'
                  INTO IT_ERR-ERROR SEPARATED BY SPACE.
      APPEND IT_ERR.
      EXIT.
    ENDIF.
  ENDIF.
*-----------------------------------------------*
*  14-Recieving Party Account (DD-NA)
*-----------------------------------------------*
*-----------------------------------------------*
*  15-Receiving Bank Number(DD-NA)
*-----------------------------------------------*
*-----------------------------------------------*
*  16-Receiving Branch(DD-NA)
*-----------------------------------------------*
*-----------------------------------------------*
*    17-Bank Charges.(DD-O)
*-----------------------------------------------*
  DTAB-BANKC = 'OUR'.
*-----------------------------------------------*
*    18-Delivery Indicator
*-----------------------------------------------*
*  dtab-delind = 'M'.
  READ TABLE IT_ZDBSPN INTO WA_ZDBSPN
  WITH KEY ZLSCH = P_WA_REGUH_PM-RZAWE
         LAND1 = P_WA_REGUH_PM-ZLAND
         WAERS = P_WA_REGUH_PM-WAERS.

  IF SY-SUBRC EQ 0.
    DTAB-DELIND  = WA_ZDBSPN-ZDBSDM.
  ENDIF.
*-----------------------------------------------*
*    19-'Mail to' Party name and address (DD-O)
*-----------------------------------------------*
  PERFORM GET_ADDRESS_PRINTFORM TABLES ADRS USING P_WA_REGUH_PM .
  GV_LEN1 = '30'.
  GV_LEN2 = '32'.
  IF DTAB-MAILTO IS INITIAL.

    DTAB-MAILTO = ADRS-LINE0.
    IF DTAB-MAILTO+0(35) IS INITIAL.
      DTAB-MAILTO+0(35) = 'INDOFILL GLOBAL PVT LTD'.
    ENDIF.
    PERFORM REPLACE_CHARS USING DTAB-MAILTO+0(35) GV_LEN1 GV_LEN2 CHANGING
    DTAB-MAILTO+0(35).

    DTAB-MAILTO+35 = ADRS-LINE1.
    IF DTAB-MAILTO+35 IS INITIAL.
      DTAB-MAILTO+35 = 'XXXXX'.
    ENDIF.
    PERFORM REPLACE_CHARS USING DTAB-MAILTO+35(35) GV_LEN1 GV_LEN2 CHANGING
DTAB-MAILTO+35(35).

    DTAB-MAILTO+70 = ADRS-LINE2.
    IF  DTAB-MAILTO+70 IS INITIAL.
      DTAB-MAILTO+70 = 'XXXXXX'.
    ENDIF.
    PERFORM REPLACE_CHARS USING DTAB-MAILTO+70(35) GV_LEN1 GV_LEN2 CHANGING
DTAB-MAILTO+70(35).

    DTAB-MAILTO+105 = ADRS-LINE3.
    IF  DTAB-MAILTO+105 IS INITIAL.
      DTAB-MAILTO+105 = 'XXXXXX'.
    ENDIF.
    PERFORM REPLACE_CHARS USING DTAB-MAILTO+105(35) GV_LEN1 GV_LEN2 CHANGING
DTAB-MAILTO+105(35).
    CLEAR:GV_LEN1,GV_LEN2.

  ENDIF.
*-----------------------------------------------*
*    20-Local Postal Code (DD-O)
*-----------------------------------------------*
  IF P_WA_REGUH_PM-ZPSTL IS NOT INITIAL.
    DTAB-POSTAL = P_WA_REGUH_PM-ZPSTL.
  ELSE.
    SELECT SINGLE * FROM ADRC
    WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
    IF SY-SUBRC EQ 0.
      DTAB-POSTAL = ADRC-POST_CODE1.
    ENDIF.
  ENDIF.
  CONDENSE DTAB-POSTAL.
  IF DTAB-POSTAL IS INITIAL.
    DTAB-POSTAL = 'XXXXXX'.
    CONDENSE DTAB-POSTAL.
  ENDIF.
*-----------------------------------------------*
*   21-Payment Details (DD-O,GIRO-O)
*-----------------------------------------------*
  REFRESH IREGUP2.   "sri
  MOVE-CORRESPONDING P_WA_REGUH_PM TO IREGUH2.
  APPEND IREGUH2.
  LOOP AT IREGUP WHERE LAUFD = P_WA_REGUH_PM-LAUFD
                   AND LAUFI = P_WA_REGUH_PM-LAUFI
                   AND XVORL = P_WA_REGUH_PM-XVORL
                   AND VBLNR = P_WA_REGUH_PM-VBLNR.
    MOVE-CORRESPONDING IREGUP TO IREGUP2.
    APPEND IREGUP2.
    READ TABLE IBSEG WITH KEY BELNR = IREGUP-BELNR
                              GJAHR = IREGUP-GJAHR
                              BUZEI = IREGUP-BUZEI.
    MOVE-CORRESPONDING IBSEG TO IBSEG2.
    APPEND IBSEG2.
  ENDLOOP.

  CLEAR: IMICRO, IMACRO.
  REFRESH: IMICRO, IMACRO.

  CALL FUNCTION 'Z_DBS_GET_PAYMENT_DETAILS_INDO'
    EXPORTING
      I_REGUH   = IREGUH2
      I_DD_PM   = DD_PM
      I_CHW_PM  = CHW_PM
      I_NEFT_PM = NEFT_PM
      I_RTGS_PM = RTGS_PM
    TABLES
      I_REGUP   = IREGUP2
      E_MICRO   = IMICRO
      I_BSEG    = IBSEG
      E_MACRO   = IMACRO.
*-----------------------------------------------*
*     Payment details + SGTXT are concatenated into micro
*-----------------------------------------------*
  PERFORM WRITE_FREE_FORMAT TABLES IREGUH2 IREGUP2
                                   IBSEG2 IMICRO IMACRO.

  CLEAR IT_LIST.
  MOVE-CORRESPONDING IREGUH2 TO IT_LIST.
  WRITE IREGUH2-RWBTR TO T_TEMP NO-SIGN NO-GROUPING
      CURRENCY P_WA_REGUH_PM-WAERS DECIMALS 0  RIGHT-JUSTIFIED.
  OVERLAY T_TEMP WITH '000000000000000000'.
  TRANSLATE T_TEMP USING ',.'.

  IT_LIST-RWBTR = IREGUH2-RWBTR.

  CONCATENATE IREGUH2-ZNME1 IREGUH2-ZNME2 INTO IT_LIST-NAME
     SEPARATED BY SPACE.
  SELECT SINGLE LAND1 INTO T001-LAND1 FROM T001
      WHERE BUKRS = IREGUH2-ZBUKR.
  SELECT SINGLE TEXT1 INTO IT_LIST-TEXT1
                  FROM T042Z WHERE LAND1 = T001-LAND1
                               AND ZLSCH = IREGUH2-RZAWE.
  IT_LIST-RZAWE = IREGUH2-RZAWE.
  MOVE-CORRESPONDING IREGUP2 TO IT_LIST.
  APPEND IT_LIST.
*-----------------------------------------------*
*   24-Print at location
*-----------------------------------------------*
  DTAB-PRATLOC = '0811'.
*-----------------------------------------------*
*   25  DDA Reference  (CHECK-NA)
*-----------------------------------------------*
*-----------------------------------------------*
*   26 Original Settlement CCY (*,GIRO-NA)
*-----------------------------------------------*
* 27 Original Debit Account No.(*,GIRO-NA)
* 28 Original Cheque No.(*,GIRO-NA)
  DTAB-ORIGCQ = '0000000000'.
* 29 Original Payment Amount.(*,GIRO-NA)
*-----------------------------------------------*
* 30 Originating Name  (CHECK-NA)
*-----------------------------------------------*
*-----------------------------------------------*
*  31 GIRO Transaction Code (CHECK-@,GIRO-M)
*-----------------------------------------------*
  DTAB-TCODE = '00'.
*-----------------------------------------------*
*  33-Send Beneficiary Advise By (CHECK-NA)
*-----------------------------------------------*
*-----------------------------------------------*
* 34 Payable at location
*-----------------------------------------------*
  IF WA_ZDBSPN-ZDBSPT = 'CCH' .
    DTAB-PYBLAT = 'DBSMUM' .
  ELSEIF WA_ZDBSPN-ZDBSPT =  'BCH'."'DD' .
*{   REPLACE        SBXK900030                                        1
*\    IF P_WA_REGUH_PM-ZORT1 = 'Bangalore' OR P_WA_REGUH_PM-ZORT1 = 'Calcutta' OR P_WA_REGUH_PM-ZORT1 = 'Chennai'
*\     OR P_WA_REGUH_PM-ZORT1 = 'Delhi' OR P_WA_REGUH_PM-ZORT1 = 'Moradabad'  OR P_WA_REGUH_PM-ZORT1 = 'Mumbai'
*\     OR P_WA_REGUH_PM-ZORT1 = 'Nasik' OR P_WA_REGUH_PM-ZORT1 = 'Pune' OR P_WA_REGUH_PM-ZORT1 = 'Salem'OR P_WA_REGUH_PM-ZORT1 = 'Surat'.
*\      DTAB-PYBLAT = 'DBSMUM' .
*\    ELSE.
*\      DTAB-PYBLAT = 'HDFMUM'.
*\    ENDIF.
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Monday, October 15, 2018 20:23:00
* Changed By - ABAP01 - Bhushan Mehta
* Purpose    - Simplification list - must be a space or equivalent character
* Solution   - Add a space or equivalent character
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*--------------------------------------------------------------------*

    IF p_wa_reguh_pm-zort1 EQ 'Bangalore' OR p_wa_reguh_pm-zort1 EQ 'Calcutta' OR
       p_wa_reguh_pm-zort1 EQ 'Chennai'   OR p_wa_reguh_pm-zort1 EQ 'Delhi'    OR
       p_wa_reguh_pm-zort1 EQ 'Moradabad' OR p_wa_reguh_pm-zort1 EQ 'Mumbai'   OR
       p_wa_reguh_pm-zort1 EQ 'Nasik'     OR p_wa_reguh_pm-zort1 EQ 'Pune'     OR
       p_wa_reguh_pm-zort1 EQ 'Salem'     OR p_wa_reguh_pm-zort1 EQ 'Surat'.
      dtab-pyblat = 'DBSMUM'.
    ELSE.
      dtab-pyblat = 'HDFMUM'.
    ENDIF.
*}   REPLACE
    ENDIF.
*------------------------------------------------*
  REFRESH IT_PAYR.
  SELECT * FROM PAYR INTO TABLE IT_PAYR WHERE ZBUKR = P_WA_REGUH_PM-ZBUKR
                                          AND HBKID = P_WA_REGUH_PM-HBKID
                                          AND HKTID = P_WA_REGUH_PM-HKTID
                                          AND RZAWE = P_WA_REGUH_PM-RZAWE.

  READ TABLE IT_PAYR WITH KEY  VBLNR =  P_WA_REGUH_PM-VBLNR.
  DTAB-CHEC_NO = IT_PAYR-CHECT.

  IF IT_PAYR-VOIDR NE 0.

    WRITE '0' TO DTAB-AMOUNT NO-SIGN NO-GROUPING CURRENCY
         P_WA_REGUH_PM-WAERS DECIMALS 4 RIGHT-JUSTIFIED.
    OVERLAY DTAB-AMOUNT WITH '000000000001.0000'.
    TRANSLATE DTAB-AMOUNT USING ',.'.

    WRITE TEXT-017 TO DTAB-RECVD.
    WRITE TEXT-017 TO DTAB-MAILTO.
  ENDIF.
*-----------------------------------------------*
* 21 23 Fax no (GIRO - PRIME)
*-----------------------------------------------*
  IF P_WA_REGUH_PM-ZTLFX IS NOT INITIAL.
    DTAB-PAYDTL = P_WA_REGUH_PM-ZTLFX .
  ELSE.
    SELECT SINGLE * FROM ADRC
  WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
    IF SY-SUBRC EQ 0.
      DTAB-PAYDTL = ADRC-FAX_NUMBER.
    ENDIF.
  ENDIF.
  IF DTAB-PAYDTL IS INITIAL.
    DTAB-PAYDTL = 'XXXXXX'.
    CONDENSE DTAB-PAYDTL.
  ENDIF.
*-----------------------------------------------*
* 22 Email address
*-----------------------------------------------*
  IF NOT P_WA_REGUH_PM-ZADNR IS INITIAL AND P_WA_REGUH_PM-ADRNR IS NOT INITIAL.
    SELECT SINGLE SMTP_ADDR
    INTO DTAB-BEMAIL
    FROM ADR6
    WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
    CONDENSE DTAB-BEMAIL NO-GAPS.

  ELSEIF NOT P_WA_REGUH_PM-ZADNR IS INITIAL AND P_WA_REGUH_PM-ADRNR IS INITIAL.
    SELECT SINGLE SMTP_ADDR
    INTO DTAB-BEMAIL
    FROM ADR6
    WHERE ADDRNUMBER = P_WA_REGUH_PM-ZADNR.
    CONDENSE DTAB-BEMAIL NO-GAPS.

  ELSEIF P_WA_REGUH_PM-ZADNR IS INITIAL AND P_WA_REGUH_PM-ADRNR IS NOT INITIAL.
    SELECT SINGLE SMTP_ADDR
   INTO DTAB-BEMAIL
   FROM ADR6
   WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
    CONDENSE DTAB-BEMAIL NO-GAPS.

  ELSEIF P_WA_REGUH_PM-ZADNR IS INITIAL AND P_WA_REGUH_PM-ADRNR IS INITIAL.

    SELECT SINGLE TLFXS
    INTO DTAB-BEMAIL FROM LFB1
    WHERE LIFNR = P_WA_REGUH_PM-LIFNR
    AND   BUKRS = P_WA_REGUH_PM-ZBUKR.
    CONDENSE DTAB-BEMAIL NO-GAPS.

  ENDIF.

  SEARCH DTAB-BEMAIL FOR GVV_AT.
  IF SY-SUBRC EQ 0.
    SPLIT DTAB-BEMAIL AT '@' INTO GV_P1 GV_P2.
    CONDENSE GV_P2.
    IF GV_P2 CA GVV_DOT.
      GV_LENTH = STRLEN( GV_P2 ).
      GV_POS = GV_LENTH - 1.
      IF GV_P2+GV_POS(1) EQ '.'.
        DTAB-BEMAIL = GV_ENAME.
        CONDENSE DTAB-BEMAIL NO-GAPS.
      ENDIF.
    ELSE.
      DTAB-BEMAIL = GV_ENAME.
      CONDENSE DTAB-BEMAIL NO-GAPS.
    ENDIF.
*       do nothing
  ELSE.
    DTAB-BEMAIL = GV_ENAME.
    CONDENSE DTAB-BEMAIL NO-GAPS.
  ENDIF.

  IF DTAB-BEMAIL IS INITIAL.
    MOVE GV_ENAME TO DTAB-BEMAIL.
    CONDENSE DTAB-BEMAIL NO-GAPS.
  ENDIF.

  CLEAR:GV_POS,GV_LENTH,GV_P1,GV_P2.
*-----------------------------------------------*
*-----------------------------------------------*
* 20 Local Postal Code (CHECK-O,GIRO-O)
*-----------------------------------------------*
  IF P_WA_REGUH_PM-ZPSTL IS NOT INITIAL.
    DTAB-POSTAL = P_WA_REGUH_PM-ZPSTL.
  ELSE.
    SELECT SINGLE * FROM ADRC
    WHERE ADDRNUMBER = P_WA_REGUH_PM-ADRNR.
    IF SY-SUBRC EQ 0.
      DTAB-POSTAL = ADRC-POST_CODE1.
    ENDIF.
  ENDIF.

  CONDENSE PTAB-POSTAL.

  IF DTAB-POSTAL IS INITIAL.
    DTAB-POSTAL = 'XXXXXX'.
    CONDENSE DTAB-POSTAL.
  ENDIF.

  APPEND DTAB.
ENDFORM.                    " FILL_DD
*&---------------------------------------------------------------------*
*&      Form  WRITE_PFILE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_PFILE_HEADER .

  CLEAR PTABH.
  PTABH-RECTYP = '000'.
  PTABH-CRDATE = SY-DATUM.
  PTABH-CRTIME = SY-UZEIT.
  PTABH-REFER = 'INDOFILL' .
  PTABH-BUKRS = ZDBSCP-ZDBSCI.

  PTABH-DESC = PTABH-REFER.

  ZDBSPR-ZDBSGN = ZDBSPR-ZDBSGN + 1.

  PERFORM CHECK_VALID_BATCH_NUMBER USING 'G'
                         CHANGING ZDBSPR-ZDBSBN ZDBSPR-ZDBSGN.
  PTABH-ORGAN =  '2009'.
  PTABH-BANK = '7171'.

* Append File Header Record into OUTTAB
  CLEAR OUTTAB.
  OUTTAB-LINE_DTLS       = PTABH-RECTYP.
  OUTTAB-LINE_DTLS+3     = PTABH-CRDATE.
  OUTTAB-LINE_DTLS+11    = PTABH-CRTIME.
  OUTTAB-LINE_DTLS+17    = PTABH-REFER.
  OUTTAB-LINE_DTLS+33    = ZDBSCP-ZDBSCI.
  OUTTAB-LINE_DTLS+45    = PTABH-ENTIT.
  OUTTAB-LINE_DTLS+115   = PTABH-DESC.
  OUTTAB-LINE_DTLS+135   = 'I'.
  OUTTAB-LINE_DTLS+757   = PTABH-ORGAN.
  OUTTAB-LINE_DTLS+761   = PTABH-BANK.
  OUTTAB-LINE_DTLS+761   = '    '.
  OUTTAB-LINE_DTLS+765 = PTABH-FILL3.

  PERFORM APPEND_REC USING : OUTTAB.
ENDFORM.                    " WRITE_PFILE_HEADER
*&---------------------------------------------------------------------*
*&      Form  CLOSE_CHW_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLOSE_CHW_FILE .
  DATA : LS_STRING TYPE STRING .

  LS_STRING = GV_PEFILE.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = LS_STRING
      FILETYPE                = 'ASC'
      TRUNC_TRAILING_BLANKS   = 'X'
    TABLES
      DATA_TAB                = OUTTAB_PRM
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      OTHERS                  = 22.

  IF SY-SUBRC <> 0.
    WRITE : 'Downloading file ' , GV_PEFILE , '. Error type: '.
    PERFORM WRITE_ERROR USING SY-SUBRC.
  ENDIF.
  REFRESH OUTTAB_PRM.
ENDFORM.                    " CLOSE_CHW_FILE
*&---------------------------------------------------------------------*
*&      Form  WRITE_BATCH_SUMMARY_D
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_BATCH_SUMMARY_D .
  DATA: T_TEMP(18).

  IF SUMMARY_FLAG = ' ' AND TOTAL_TRAN <> 0.
    CLEAR D_BSRTAB.
    D_BSRTAB-REC_TYPE = '099'.
    D_BSRTAB-TOTAL_TRAN = TOTAL_TRAN.
    CLEAR T_TEMP.

    WRITE TOTAL_PAYM TO T_TEMP NO-SIGN NO-GROUPING
              DECIMALS 4 RIGHT-JUSTIFIED.
    OVERLAY T_TEMP WITH '000000000000.0000'.
    TRANSLATE T_TEMP USING ',.'.
    D_BSRTAB-TOTAL_PAYM = T_TEMP.

*   Output batch summary record
    CLEAR OUTTAB.
    OUTTAB-LINE_DTLS    = D_BSRTAB-REC_TYPE.
    OUTTAB-LINE_DTLS+3  = D_BSRTAB-TOTAL_TRAN.
    OUTTAB-LINE_DTLS+11 = D_BSRTAB-TOTAL_PAYM.
    OUTTAB-LINE_DTLS+29 = D_BSRTAB-BSR_FILLER2.

    PERFORM APPEND_REC USING : OUTTAB.

    CLEAR TOTAL_TRAN.
    CLEAR TOTAL_PAYM.
    SUMMARY_FLAG = 'X'.
  ENDIF.
ENDFORM.                    " WRITE_BATCH_SUMMARY_D
*&---------------------------------------------------------------------*
*&      Form  CHECK_USER_AUTHORIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_USER_AUTHORIZATION .
  DATA: LV_MSG TYPE STRING,
         LV_BNAME TYPE SY-UNAME,
         LV_BUKRS TYPE BUKRS.

  SELECT SINGLE BNAME FROM ZDBSUSER INTO LV_BNAME
    WHERE BNAME = SY-UNAME." AND
*          bukrs = gv_bukrs.

  IF SY-SUBRC NE 0.

    CONCATENATE SY-UNAME
                'is not authorized to extract payment details from'
                GV_BUKRS 'Company Code' INTO LV_MSG
                SEPARATED BY SPACE.

    MESSAGE LV_MSG TYPE 'A'.

  ENDIF.
ENDFORM.                    " CHECK_USER_AUTHORIZATION
*&---------------------------------------------------------------------*
*&      Form  FILL_EFT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_REGUH_PM  text
*----------------------------------------------------------------------*
FORM FILL_EFT  USING P_WA_REGUH_PM TYPE TY_REGUH_PM .

  CLEAR: WA_EFT.
*------------------------------------------------------*
*Value Date or Process Date
*------------------------------------------------------*
* If ACT then Processing Date and
* If TT then Value Date = PD + x (Determined from the table)
* Considering the current date (p_valudt) as processing date
*  IF p_wa_reguh_pm-pymt_mtd EQ c_tt.
*
*    CASE p_wa_reguh_pm-waers.
*
*      WHEN 'IDR' OR 'ZAR' OR 'THB'.
*
*        wa_eft-val_procs_date = p_valudt + 2.
*
*      WHEN 'SGD' OR 'USD' OR 'GBP' OR 'EUR'.
*
*        wa_eft-val_procs_date = p_valudt.
*
*      WHEN OTHERS.
*
*        wa_eft-val_procs_date = p_valudt + 1.
*
*    ENDCASE.
*
*  ELSEIF p_wa_reguh_pm-pymt_mtd EQ c_act OR p_wa_reguh_pm-pymt_mtd EQ c_meps.
*
*    wa_eft-val_procs_date = p_valudt.
*
*  ENDIF.
*------------------------------------------------------*
* Payment Currency
*------------------------------------------------------*
  WA_EFT-CURRENCY = P_WA_REGUH_PM-WAERS.
*------------------------------------------------------*
*  Payment Type
*------------------------------------------------------*
  WA_EFT-PYMT_TYP = P_WA_REGUH_PM-PYMT_MTD+0(3).
*------------------------------------------------------*
*  Company Code
*------------------------------------------------------*
  WA_EFT-COMPY_CODE = P_WA_REGUH_PM-ZBUKR.
*------------------------------------------------------*
* Customer Reference Number
*------------------------------------------------------*
  WA_EFT-CUST_REF_NO = P_WA_REGUH_PM-VBLNR.
*------------------------------------------------------*
*  Payment Amount
*------------------------------------------------------*
  DATA LV_TEMP_AMT TYPE P LENGTH 8 DECIMALS 3.
  DATA: LV_STR TYPE STRING,
        TEMP_AMT(15) TYPE C,
         TEMP_AMT1 TYPE REGUH-RWBTR,
        LV_TEMP(5) TYPE C.
  DATA: LV_LEN TYPE I.
  DATA: LV_OFFSET TYPE I.

  IF P_WA_REGUH_PM-WAERS EQ 'IDR' OR P_WA_REGUH_PM-WAERS EQ 'JPY'.

    WRITE P_WA_REGUH_PM-RWBTR TO WA_EFT-AMOUNT
    CURRENCY P_WA_REGUH_PM-WAERS
    NO-SIGN NO-GROUPING
    LEFT-JUSTIFIED.

    SPLIT WA_EFT-AMOUNT AT '.' INTO WA_EFT-AMOUNT LV_TEMP.
    GV_TOTAMT = GV_TOTAMT + WA_EFT-AMOUNT.

  ELSEIF P_WA_REGUH_PM-WAERS EQ 'BHD'.

    WRITE P_WA_REGUH_PM-RWBTR TO WA_EFT-AMOUNT
    CURRENCY P_WA_REGUH_PM-WAERS NO-SIGN NO-GROUPING
    LEFT-JUSTIFIED.

    SPLIT WA_EFT-AMOUNT AT '.' INTO WA_EFT-AMOUNT LV_TEMP.
    LV_STR = LV_TEMP.
    LV_LEN = STRLEN( LV_STR ).

    IF LV_LEN EQ 0.
      CONCATENATE WA_EFT-AMOUNT '.' '000' INTO TEMP_AMT.
      GV_TOTAMT = GV_TOTAMT + TEMP_AMT.
      CONCATENATE WA_EFT-AMOUNT '000' INTO WA_EFT-AMOUNT.

    ELSEIF LV_LEN EQ 1.
      CONCATENATE WA_EFT-AMOUNT '.' LV_TEMP+0(1) '00' INTO TEMP_AMT.
      GV_TOTAMT = GV_TOTAMT + TEMP_AMT.
      CONCATENATE WA_EFT-AMOUNT LV_TEMP+0(1) '00' INTO WA_EFT-AMOUNT.

    ELSEIF LV_LEN EQ 2.
      CONCATENATE WA_EFT-AMOUNT '.' LV_TEMP+0(2) '0' INTO TEMP_AMT.
      GV_TOTAMT = GV_TOTAMT + TEMP_AMT.
      CONCATENATE WA_EFT-AMOUNT LV_TEMP+0(2) '0' INTO WA_EFT-AMOUNT.

    ELSEIF LV_LEN GE 3.
      CONCATENATE WA_EFT-AMOUNT '.' LV_TEMP+0(3) INTO TEMP_AMT.
      GV_TOTAMT = GV_TOTAMT + TEMP_AMT.
      CONCATENATE WA_EFT-AMOUNT LV_TEMP+0(3) INTO WA_EFT-AMOUNT.

    ENDIF.

  ELSE.

    WRITE P_WA_REGUH_PM-RWBTR TO WA_EFT-AMOUNT
    CURRENCY P_WA_REGUH_PM-WAERS  NO-SIGN NO-GROUPING
    LEFT-JUSTIFIED.

    SPLIT WA_EFT-AMOUNT AT '.' INTO WA_EFT-AMOUNT LV_TEMP.
    LV_STR = LV_TEMP.
    LV_LEN = STRLEN( LV_STR ).

    IF LV_LEN EQ 0.
      CONCATENATE WA_EFT-AMOUNT '.' '00' INTO TEMP_AMT.
      GV_TOTAMT = GV_TOTAMT + TEMP_AMT.
      CONCATENATE WA_EFT-AMOUNT '00' INTO WA_EFT-AMOUNT.

    ELSEIF LV_LEN EQ 1.
      CONCATENATE WA_EFT-AMOUNT '.' LV_TEMP+0(1) '0' INTO TEMP_AMT.
      GV_TOTAMT = GV_TOTAMT + TEMP_AMT.
      CONCATENATE WA_EFT-AMOUNT LV_TEMP+0(1) '0' INTO WA_EFT-AMOUNT.

    ELSEIF LV_LEN GE 2.
      CONCATENATE WA_EFT-AMOUNT '.' LV_TEMP+0(2) INTO TEMP_AMT.
      GV_TOTAMT = GV_TOTAMT + TEMP_AMT.
      CONCATENATE WA_EFT-AMOUNT LV_TEMP+0(2) INTO WA_EFT-AMOUNT.

    ENDIF.

  ENDIF.
  CLEAR:TEMP_AMT,TEMP_AMT1.
*------------------------------------------------------*
*  Payment Details
*------------------------------------------------------*
  LOOP AT IREGUP WHERE LAUFD = P_WA_REGUH_PM-LAUFD
                   AND LAUFI = P_WA_REGUH_PM-LAUFI
                   AND XVORL = P_WA_REGUH_PM-XVORL
                   AND VBLNR = P_WA_REGUH_PM-VBLNR.

    IF SY-TABIX = 1.
      WA_EFT-PYMT_DTLS = IREGUP-XBLNR.
    ELSE.
      IF WA_EFT-PYMT_DTLS IS INITIAL.
        IF IREGUP-XBLNR IS NOT INITIAL.
          MOVE IREGUP-XBLNR TO WA_EFT-PYMT_DTLS.
        ENDIF.
      ELSE.
        IF IREGUP-XBLNR IS NOT INITIAL.
          CONCATENATE WA_EFT-PYMT_DTLS ',' IREGUP-XBLNR INTO WA_EFT-PYMT_DTLS .
        ENDIF.
      ENDIF.
    ENDIF.

* PERFORM replace_chars USING wa_eft-pymt_dtls CHANGING wa_eft-pymt_dtls.

  ENDLOOP.
*------------------------------------------------------*
*   Beneficiary Name
*------------------------------------------------------*
  WA_EFT-BEN_NAME = P_WA_REGUH_PM-ZNME1.

  IF WA_EFT-BEN_NAME IS INITIAL.
    IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
    IT_ERR-ERROR = 'Beneficiary Name can not be blank'.
    APPEND IT_ERR.
    EXIT .
  ENDIF.
*------------------------------------------------------*
*   Beneficiary Address
*   If IBAN field has value then append to the last line of Ben Address.
*------------------------------------------------------*
  WA_EFT-BEN_ADDR+0(35) = P_WA_REGUH_PM-STRAS.
  WA_EFT-BEN_ADDR+35(35) =  P_WA_REGUH_PM-ORT01.
  WA_EFT-BEN_ADDR+70(35) = P_WA_REGUH_PM-PSTLZ.

  IF  WA_EFT-BEN_ADDR+35(35) NE SPACE.
    IF  WA_EFT-BEN_ADDR+0(35) EQ SPACE.
      WA_EFT-BEN_ADDR+0(35) = WA_EFT-BEN_ADDR+35(35).
      CLEAR WA_EFT-BEN_ADDR+35(35).
    ENDIF.
  ENDIF.

  IF WA_EFT-BEN_ADDR+70(35) NE SPACE.
    IF  WA_EFT-BEN_ADDR+35(35) EQ SPACE.
      WA_EFT-BEN_ADDR+35(35) = WA_EFT-BEN_ADDR+70(35).
      CLEAR WA_EFT-BEN_ADDR+70(35).
    ENDIF.
  ENDIF.
  GV_LEN1 = '30'.
  GV_LEN2 = '32'.
  PERFORM REPLACE_CHARS USING WA_EFT-BEN_ADDR+0(35) GV_LEN1 GV_LEN2 CHANGING
   WA_EFT-BEN_ADDR+0(35).
*  endif.
  PERFORM REPLACE_CHARS USING  WA_EFT-BEN_ADDR+35(35) GV_LEN1 GV_LEN2 CHANGING
    WA_EFT-BEN_ADDR+35(35).
  PERFORM REPLACE_CHARS USING  WA_EFT-BEN_ADDR+70(35) GV_LEN1 GV_LEN2 CHANGING
    WA_EFT-BEN_ADDR+70(35).
*------------------------------------------------------*
*   Bank Code
*------------------------------------------------------*
*  WRITE p_wa_reguh_pm-zbnkl+0(4) TO wa_eft-bank_code RIGHT-JUSTIFIED.
*  OVERLAY wa_eft-bank_code WITH '0000'.

*  IF wa_eft-bank_code IS INITIAL.
*    it_err-vblnr = p_wa_reguh_pm-vblnr.
*    it_err-error = 'Bank Code can not be blank'.
*    APPEND it_err.
*    EXIT .
*  ENDIF.
*------------------------------------------------------*
*   Country Payable
*------------------------------------------------------*
  WA_EFT-CNTRY_PAYBL = P_WA_REGUH_PM-ZBNKS+0(2).

  IF WA_EFT-CNTRY_PAYBL IS INITIAL.
    IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
    IT_ERR-ERROR = 'Country Payable can not be blank'.
    APPEND IT_ERR.
    EXIT .
  ENDIF.
*------------------------------------------------------*
*   Beneficiary Account Number
*------------------------------------------------------*
  WA_EFT-BEN_ACC_NO = P_WA_REGUH_PM-ZBNKN.

  IF WA_EFT-BEN_ACC_NO IS INITIAL.
    IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
    IT_ERR-ERROR = 'Beneficiary Account Number can not be blank'.
    APPEND IT_ERR.
    EXIT .
  ENDIF.
*------------------------------------------------------*
*   Beneficiary Bank Name and Address
*------------------------------------------------------*
  READ TABLE IT_BNKA INTO WA_BNKA
       WITH KEY BANKS = P_WA_REGUH_PM-ZBNKS
                BANKL = P_WA_REGUH_PM-ZBNKY.

  IF SY-SUBRC EQ 0.

    READ TABLE ITAB_TIBAN INTO WA_TIBAN
     WITH KEY BANKS = P_WA_REGUH_PM-ZBNKS
              BANKL = P_WA_REGUH_PM-ZBNKY
              BANKN = P_WA_REGUH_PM-BANK_NO.

    IF SY-SUBRC EQ 0.
      GV_IBAN = WA_TIBAN-IBAN.
    ENDIF.

    PERFORM BANK_ADDRESS USING GV_IBAN
                         CHANGING WA_EFT-BEN_BNK_NAME_ADDR+0(35)
                                  WA_EFT-BEN_BNK_NAME_ADDR+35(35)
                                  WA_EFT-BEN_BNK_NAME_ADDR+70(35)
                                  WA_EFT-BEN_BNK_NAME_ADDR+105(35).

    IF WA_EFT-BEN_BNK_NAME_ADDR+0(35) IS INITIAL.
      IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
      IT_ERR-ERROR = 'Beneficiary Bank Name can not be blank'.
      APPEND IT_ERR.
      EXIT .
    ENDIF.
    GV_LEN1 = '30'.
    GV_LEN2 = '32'.
    PERFORM REPLACE_CHARS USING  WA_EFT-BEN_BNK_NAME_ADDR+35(35) GV_LEN1 GV_LEN2 CHANGING
    WA_EFT-BEN_BNK_NAME_ADDR+35(35).

    PERFORM REPLACE_CHARS USING   WA_EFT-BEN_BNK_NAME_ADDR+70(35) GV_LEN1 GV_LEN2 CHANGING
    WA_EFT-BEN_BNK_NAME_ADDR+70(35).

    PERFORM REPLACE_CHARS USING   WA_EFT-BEN_BNK_NAME_ADDR+105(35) GV_LEN1 GV_LEN2 CHANGING
     WA_EFT-BEN_BNK_NAME_ADDR+105(35).

    CLEAR GV_IBAN.
*------------------------------------------------------*
*    Beneficiary Bank SWIFT Code
*------------------------------------------------------*
    WA_EFT-BEN_BNK_SWIFT_BIC = WA_BNKA-SWIFT.
  ENDIF.
*------------------------------------------------------*
*DBS Bank's Charges
*------------------------------------------------------*
  WA_EFT-DBS_CHRGS = C_OUR.
*------------------------------------------------------*
*Other Bank's Charges
*------------------------------------------------------*
  WA_EFT-AGENT_CHRGS = C_BEN.
*------------------------------------------------------*
*   Settlement Account Number
*------------------------------------------------------*
  WA_EFT-STTL_ACC_NO = P_WA_REGUH_PM-UBKNT.

  CLEAR WA_UTAB. REFRESH ITAB_UTAB.
  SPLIT WA_EFT-STTL_ACC_NO AT '-' INTO TABLE ITAB_UTAB.
  CLEAR WA_EFT-STTL_ACC_NO.
  LOOP AT ITAB_UTAB INTO WA_UTAB.
    CONCATENATE WA_EFT-STTL_ACC_NO WA_UTAB-WORD
      INTO WA_EFT-STTL_ACC_NO.
  ENDLOOP.


  IF WA_EFT-STTL_ACC_NO IS INITIAL.
    IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
    IT_ERR-ERROR = 'Settlement Account Number can not be blank'.
    APPEND IT_ERR.
    EXIT .
  ENDIF.
*------------------------------------------------------*
*    Settlement Currency
*------------------------------------------------------*
  CLEAR IT_T012K.

  READ TABLE IT_T012K WITH KEY BUKRS = P_WA_REGUH_PM-ZBUKR
                                             HBKID = P_WA_REGUH_PM-HBKID
                                             HKTID = P_WA_REGUH_PM-HKTID.
  IF SY-SUBRC = 0.

    WA_EFT-STTL_CURRY   = IT_T012K-WAERS.

    IF WA_EFT-STTL_CURRY IS INITIAL.
      IT_ERR-VBLNR = P_WA_REGUH_PM-VBLNR.
      IT_ERR-ERROR = 'Settlement Currency is not maintained'.
      APPEND IT_ERR.
      EXIT .
    ENDIF.
  ENDIF.

  IF NOT WA_EFT IS INITIAL.

    APPEND WA_EFT TO ITAB_EFT.
    CLEAR WA_EFT.

  ENDIF.

  PERFORM APPEND_LOG_DISPLAY USING P_WA_REGUH_PM.

ENDFORM.                    " FILL_EFT
*&---------------------------------------------------------------------*
*&      Form  BANK_ADDRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_IBAN  text
*      <--P_WA_EFT_BEN_BNK_NAME_ADDR+0(35)  text
*      <--P_WA_EFT_BEN_BNK_NAME_ADDR+35(35  text
*      <--P_WA_EFT_BEN_BNK_NAME_ADDR+70(35  text
*      <--P_WA_EFT_BEN_BNK_NAME_ADDR+105(3  text
*----------------------------------------------------------------------*
FORM BANK_ADDRESS  USING    P_REGUH_PM-ZIBAN TYPE ANY
                  CHANGING P_ADDR1 TYPE ANY
                            P_ADDR2 TYPE ANY
                            P_ADDR3 TYPE ANY
                            P_ADDR4 TYPE ANY.
  DATA:LV_STRING TYPE STRING,LV_TEMP TYPE STRING,LV_TEMP1 TYPE SY-MSGV1,
   LV_TEMP2 TYPE SY-MSGV1,LV_TEMP3 TYPE SY-MSGV1,LV_TEMP4 TYPE SY-MSGV1,
   LV_LINE3 TYPE SY-MSGV1,LV_LINE2 TYPE SY-MSGV1 ,
   LV_LINE1 TYPE SY-MSGV1,LV_LINE4 TYPE SY-MSGV1,LINE3(35),LV_DUMMY(35),
   LINE4(35),LINE1(35),LINE2(35),LV_LEN TYPE I,LV_LEN1 TYPE I,LV_LEN2 TYPE I,
   LV_LEN3 TYPE I, LV_LEN4 TYPE I,VAR_LEN TYPE I,VAR_LEN2 TYPE I,
   VAR_LEN1 TYPE I,VAR_LEN3 TYPE I.
*--------------------------------------------------------------------*
* Check for line1
*--------------------------------------------------------------------*
  LV_LEN = STRLEN( WA_BNKA-BANKA ).
  IF LV_LEN > 35.

    LV_STRING = WA_BNKA-BANKA.
    CALL FUNCTION 'HR_ES_SPLIT_STRING_TO_4_CHAR50'
      EXPORTING
        IN_STRING = LV_STRING
        STR_LEN   = 35
      IMPORTING
        CHAR1     = LV_LINE1
        CHAR2     = LV_LINE2.
    CLEAR:LV_LEN,LV_STRING.
    LINE1 = LV_LINE1.
    LINE2 = LV_LINE2.
    CLEAR: LV_LINE1,LV_LINE2.

    CONCATENATE LINE2 WA_BNKA-STRAS WA_BNKA-ORT01 WA_BNKA-BRNCH
    INTO LV_TEMP SEPARATED BY SPACE.

    LV_LEN1 = STRLEN( LV_TEMP ).

    IF LV_LEN1 > 35.

      CALL FUNCTION 'HR_ES_SPLIT_STRING_TO_4_CHAR50'
        EXPORTING
          IN_STRING = LV_TEMP
          STR_LEN   = 35
        IMPORTING
          CHAR1     = LV_LINE1
          CHAR2     = LV_LINE2
          CHAR3     = LV_LINE3
          CHAR4     = LV_LINE4.

      LINE2 = LV_LINE1.
      IF LV_LINE2 IS NOT INITIAL.
        LINE3 = LV_LINE2.
        IF NOT P_REGUH_PM-ZIBAN IS INITIAL.
          CONCATENATE 'IBAN or Sort' P_REGUH_PM-ZIBAN
          INTO LINE4 SEPARATED BY SPACE .
        ELSE.
          IF WA_BNKA-BNKLZ IS NOT INITIAL.
            CONCATENATE 'Bank No' WA_BNKA-BNKLZ
            INTO LINE4 SEPARATED BY SPACE .
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.
      LINE3 = LV_TEMP.
      IF NOT P_REGUH_PM-ZIBAN IS INITIAL.
        CONCATENATE 'IBAN or Sort' P_REGUH_PM-ZIBAN
        INTO LINE4 SEPARATED BY SPACE.
      ELSE.
        IF WA_BNKA-BNKLZ IS NOT INITIAL.
          CONCATENATE 'Bank No' WA_BNKA-BNKLZ
         INTO LINE4 SEPARATED BY SPACE .
        ENDIF.
      ENDIF.
    ENDIF.
*------------------------------------------------------------------*
  ELSE.
    LINE1 = WA_BNKA-BANKA.
    CLEAR: LV_LEN,LV_STRING.

    CONCATENATE WA_BNKA-STRAS WA_BNKA-ORT01 WA_BNKA-BRNCH
    INTO LV_TEMP SEPARATED BY SPACE.

    LV_LEN = STRLEN( LV_TEMP ).
    LV_STRING = LV_TEMP.

    IF LV_LEN > 35.
      CALL FUNCTION 'HR_ES_SPLIT_STRING_TO_4_CHAR50'
        EXPORTING
          IN_STRING = LV_STRING
          STR_LEN   = 35
        IMPORTING
          CHAR1     = LV_TEMP1
          CHAR2     = LV_TEMP2
          CHAR3     = LV_TEMP3
          CHAR4     = LV_TEMP4.
      CLEAR LV_STRING.

      LINE2 = LV_TEMP1.
      LINE3 = LV_TEMP2.
      IF NOT P_REGUH_PM-ZIBAN IS INITIAL.
        CONCATENATE 'IBAN or Sort' P_REGUH_PM-ZIBAN
        INTO LINE4 SEPARATED BY SPACE .
      ELSE.
        IF WA_BNKA-BNKLZ IS NOT INITIAL.
          CONCATENATE 'Bank No' WA_BNKA-BNKLZ
        INTO LINE4 SEPARATED BY SPACE .
        ENDIF.
      ENDIF.

    ELSE.
      LINE2 = LV_TEMP.
      IF NOT P_REGUH_PM-ZIBAN IS INITIAL.
        CONCATENATE 'IBAN or Sort' P_REGUH_PM-ZIBAN
        INTO LINE4 SEPARATED BY SPACE.
      ELSE .
        IF WA_BNKA-BNKLZ IS NOT INITIAL.
          CONCATENATE 'Bank No' WA_BNKA-BNKLZ
        INTO LINE4 SEPARATED BY SPACE .
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*-----------------------------------------------------------*
  P_ADDR1 = LINE1.
  P_ADDR2 = LINE2.
  P_ADDR3 = LINE3.
  P_ADDR4 = LINE4.

  IF P_ADDR2 EQ SPACE.
    MOVE 'NA' TO P_ADDR2.
  ENDIF.

  CLEAR: LV_STRING,LV_TEMP,LV_TEMP1 ,LV_TEMP2 ,
       LV_TEMP3 ,LV_TEMP4 ,LV_LINE1,LV_LINE2 ,
       LINE3,LINE4,LINE1,LINE2,
       LV_LEN ,LV_LEN1 ,LV_LEN2 ,LV_LEN3 ,LV_LEN4,
       VAR_LEN ,VAR_LEN2 ,VAR_LEN1 ,VAR_LEN3 .
ENDFORM.                    " BANK_ADDRESS
*&---------------------------------------------------------------------*
*&      Form  PROCESS_EFT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_EFT .
  DATA: LV_DATE TYPE SY-DATUM,
          LV_LINES TYPE C,
          LV_TIME TYPE SY-UZEIT,
          LV_EFT TYPE TYP_EFT,
          LV_BTCH_NO(8) TYPE N VALUE '50000000',       " Batch Number
          LV_CNT_TRNC(8) TYPE N VALUE 0,               " Count Transaction
          LV_CUSTID_ERR(500),
          LV_FL_NUM(2) TYPE N.

  DATA: LV_FILENAME TYPE STRING.
  CONCATENATE 'DBS_' SY-DATUM '_' SY-UZEIT 'EFT' '_' LV_FL_NUM C_EFT_EXTN INTO LV_FILENAME.

  LV_BTCH_NO = GV_BATCH_NUM.

  SORT ITAB_EFT BY VAL_PROCS_DATE CURRENCY COMPY_CODE.

  LV_DATE = SY-DATUM.
  CONCATENATE  LV_DATE+6(2) LV_DATE+4(2) LV_DATE+0(4) INTO LV_DATE.

  LV_TIME = SY-UZEIT.

  DESCRIBE TABLE ITAB_EFT LINES LV_LINES.

  LOOP AT ITAB_EFT INTO WA_EFT.

    LV_EFT = WA_EFT.
    LV_EFT-AMOUNT = WA_EFT-AMOUNT.
    WA_EFT_FINAL-END_OF_LINE = SPACE.
*------------------------------------------------------------*
*** FILE HEADER RECORD
*------------------------------------------------------------*
    AT FIRST.
*     Record Type
      WA_EFT_FINAL-RECORD+0(3) = C_F.
*     Filler
*     Creation Date
      WA_EFT_FINAL-RECORD+6(8) = LV_DATE.
*     Creation Time
      WA_EFT_FINAL-RECORD+14(6) = LV_TIME.
*     File Name
      WA_EFT_FINAL-RECORD+20(16) = LV_FILENAME.
*     Company Id
      PERFORM  GET_COMPANY_ID USING LV_EFT-COMPY_CODE
                              CHANGING WA_EFT_FINAL+36(12).

      CLEAR LV_CUSTID_ERR.
      CONCATENATE 'Maintain Company Code ' LV_EFT-COMPY_CODE ' mapping in ZDBSCP table'
                   INTO LV_CUSTID_ERR.

      PERFORM CHK_N_UPDATE_ERROR_LOG USING WA_EFT_FINAL+36(12)
                                           LV_EFT-CUST_REF_NO
                                           LV_CUSTID_ERR.

*     Filler
*     Bank ID
      WA_EFT_FINAL-RECORD+56(12) = C_DBSBANK_ID.

*     Filler
*     File Type
      WA_EFT_FINAL-RECORD+78(4) = C_REM_FILETYP.

*     File Description
      WA_EFT_FINAL-RECORD+82(20) = C_FILE_DESC.

*     DBS Field
*     Filler

      IF NOT WA_EFT_FINAL IS INITIAL.

        APPEND WA_EFT_FINAL TO ITAB_EFT_FINAL.
        CLEAR: WA_EFT_FINAL.

      ENDIF.
    ENDAT.
*------------------------------------------------------------*
***   BATCH HEADER RECORD
*------------------------------------------------------------*
    ON CHANGE OF WA_EFT-COMPY_CODE.

*      sort itab_zdbspr by laufd laufi.
*      READ TABLE itab_zdbspr INTO wa_zdbspr INDEX 1.
*      lv_btch_no = wa_zdbspr-zdbsbn + 1.
      ADD 1 TO LV_BTCH_NO.

*     Record Type
      WA_EFT_FINAL-RECORD+0(3) = C_BTCH_HDR.
*     Filler
*     Creation Date
      WA_EFT_FINAL-RECORD+6(8) = LV_DATE.
*     Creation Time
      WA_EFT_FINAL-RECORD+14(6) = LV_TIME.
*     File Name
*      wa_eft_final-record+20(16) = lv_filename.

*     Company Id

      PERFORM  GET_COMPANY_ID USING LV_EFT-COMPY_CODE
                              CHANGING WA_EFT_FINAL+36(12).

*     Filler
*     Bank ID
      WA_EFT_FINAL-RECORD+56(12) = C_DBSBANK_ID.

*     Filler
*     Batch Number

      WA_EFT_FINAL-RECORD+78(8) = LV_BTCH_NO.

*     Company Info
*     DBS Field
*     Filler

      IF NOT WA_EFT_FINAL IS INITIAL.

        APPEND WA_EFT_FINAL TO ITAB_EFT_FINAL.
        CLEAR: WA_EFT_FINAL.

      ENDIF.
    ENDON.
*------------------------------------------------------------*
***     PAYMENT RECORD
*------------------------------------------------------------*
    ADD 1 TO LV_CNT_TRNC.

*     Record Type
    WA_EFT_FINAL-RECORD+0(3) = C_PYMT_REC.
*     Filler
*     Creation Date
    WA_EFT_FINAL-RECORD+6(8) = LV_DATE.
*     Creation Time
    WA_EFT_FINAL-RECORD+14(6) = LV_TIME.

*     Message Category
    WA_EFT_FINAL-RECORD+20(3) = C_FT.

*     Payment Type
    WA_EFT_FINAL-RECORD+23(3) = WA_EFT-PYMT_TYP.

*     Customer Reference
    WA_EFT_FINAL-RECORD+26(16) = WA_EFT-CUST_REF_NO.

*     Filler
*     Payment Currency
    WA_EFT_FINAL-RECORD+44(3) = LV_EFT-CURRENCY.

*      Payment Amount
    WRITE LV_EFT-AMOUNT TO WA_EFT_FINAL-RECORD+47(11)
          NO-SIGN NO-GROUPING DECIMALS 0
          RIGHT-JUSTIFIED.
    OVERLAY WA_EFT_FINAL-RECORD+47(11) WITH '00000000000'.

    CONCATENATE PA_VALDT+6(2)
                PA_VALDT+4(2)
                PA_VALDT+0(4) INTO
                WA_EFT_FINAL-RECORD+58(8).

*      Payment Details
    WA_EFT_FINAL-RECORD+66(140) = LV_EFT-PYMT_DTLS.

*      Beneficiary Name - ACT / TT / MEPS
    WA_EFT_FINAL-RECORD+206(35) = LV_EFT-BEN_NAME.

*      Beneficiary Address - ACT / TT / MEPS
    WA_EFT_FINAL-RECORD+241(105) = LV_EFT-BEN_ADDR.

*      Filler
*      Bank Code
    WA_EFT_FINAL-RECORD+428(4) = LV_EFT-BANK_CODE.

*      Filler
*      Country Payable
    WA_EFT_FINAL-RECORD+473(2) = LV_EFT-CNTRY_PAYBL.

*      Filler
*      Beneficiary Account Number
    WA_EFT_FINAL-RECORD+479(34) = LV_EFT-BEN_ACC_NO.

*      Filler
*      Beneficiary Bank Name & Address
    WA_EFT_FINAL-RECORD+532(140) = LV_EFT-BEN_BNK_NAME_ADDR.

*      Filler
*      Intermediatry Bank Name & Address - N/A
*      Beneficiary Bank Swift BIC
    WA_EFT_FINAL-RECORD+818(11) = LV_EFT-BEN_BNK_SWIFT_BIC.

*     Intermediatry Bank SWIFT BIC - N/A
*     Filler
*     DBS and Agent Bank's Charges
    WA_EFT_FINAL-RECORD+852(3) = LV_EFT-DBS_CHRGS.
    WA_EFT_FINAL-RECORD+855(3) = LV_EFT-AGENT_CHRGS.

*     Settlement Account Number
    WA_EFT_FINAL-RECORD+858(34) = LV_EFT-STTL_ACC_NO.

*     Filler
*     Settlement_currency
    WA_EFT_FINAL-RECORD+897(3) = LV_EFT-STTL_CURRY.

*     Filler
*     FX contract ref
*     Filler
*     Delivery Method
    WA_EFT_FINAL-RECORD+956(2) = LV_EFT-DELVRY_MODE.

*     Pickup Location
*     Instruction to Ordering Bank
*     Filler

    IF NOT WA_EFT_FINAL IS INITIAL.

      APPEND WA_EFT_FINAL TO ITAB_EFT_FINAL.
      CLEAR: WA_EFT_FINAL.

    ENDIF.
*------------------------------------------------------------*
***   BATCH SUMMARY RECORD
*------------------------------------------------------------*
    AT LAST.
      SUM.
*     Record Type
      WA_EFT_FINAL-RECORD+0(3) = C_BTCH_SUMMY_REC.

*     Filler
*     Creation Date
      WA_EFT_FINAL-RECORD+6(8) = LV_DATE.
*     Creation Time
      WA_EFT_FINAL-RECORD+14(6) = LV_TIME.

*     Total Number of Transactions
      WA_EFT_FINAL-RECORD+20(8) = LV_CNT_TRNC.

*     Total Payment Amount
      WRITE GV_TOTAMT TO WA_EFT_FINAL-RECORD+28(18)
            NO-SIGN NO-GROUPING DECIMALS 4 LEFT-JUSTIFIED.

      REPLACE ',' WITH '.' INTO WA_EFT_FINAL-RECORD+28(18).
*
      CONDENSE WA_EFT_FINAL-RECORD+28(18) NO-GAPS.
      WRITE WA_EFT_FINAL-RECORD+28(18) TO WA_EFT_FINAL-RECORD+28(18)
            RIGHT-JUSTIFIED.

      OVERLAY WA_EFT_FINAL-RECORD+28(18) WITH '0000000000000.0000'.

*     Filler

      IF NOT WA_EFT_FINAL IS INITIAL.

        APPEND WA_EFT_FINAL TO ITAB_EFT_FINAL.
        CLEAR: WA_EFT_FINAL.

      ENDIF.

      CLEAR LV_CNT_TRNC.

    ENDAT.

    CLEAR: LV_EFT,
           WA_EFT.

  ENDLOOP.
* Download the data - File header, Batch Header, Payment Records and Trailer to the file

  IF NOT ITAB_EFT_FINAL IS INITIAL AND IT_ERR[] IS INITIAL.

    LV_FL_NUM = LV_FL_NUM + 1.

    CONCATENATE 'DBS_' SY-DATUM '_' SY-UZEIT 'EFT' '_' LV_FL_NUM C_EFT_EXTN INTO LV_FILENAME.

    CONCATENATE PA_EFILE '\' LV_FILENAME INTO LV_FILENAME.

    PERFORM FRNTEND_DOWNLOAD TABLES ITAB_EFT_FINAL USING LV_FILENAME PA_EFILE.

    GV_BATCH_NUM = LV_BTCH_NO.
    REFRESH ITAB_EFT_FINAL.

  ENDIF.
ENDFORM.                    " PROCESS_EFT
*&---------------------------------------------------------------------*
*&      Module  PA_LAUFD_VAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE PA_LAUFD_VAL INPUT.
*
*REFRESH TLAUFK.
*  TLAUFK-LAUFK = SPACE.
*  TLAUFK-SIGN  = 'I'.
*  APPEND TLAUFK.
*  CALL FUNCTION 'F4_ZAHLLAUF'
*    EXPORTING
*      F1TYP = 'D'
*      F2NME = 'ZDBSPR-LAUFD' "zdbspr
*    IMPORTING
*      LAUFD = ZDBSPR-LAUFD
*      LAUFI = ZDBSPR-LAUFI
*      NOTHING_SELECTED = xf4_c1
*    TABLES
*      LAUFK = TLAUFK.
*  IF xf4_c1 IS INITIAL.
*    LEAVE TO SCREEN 1000.
*  ENDIF.
*
*  WRITE ZDBSPR-LAUFD TO Pa_LAUFD.
*
*
*ENDMODULE.                 " PA_LAUFD_VAL  INPUT
**&---------------------------------------------------------------------*
**&      Module  PA_LAUFI_VAL  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE PA_LAUFI_VAL INPUT.
*REFRESH TLAUFK.
*  TLAUFK-LAUFK = SPACE.
*  TLAUFK-SIGN  = 'I'.
*  APPEND TLAUFK.
*  CALL FUNCTION 'F4_ZAHLLAUF'
*    EXPORTING
*      F1TYP = 'I'
*      F2NME = 'ZDBSPR-LAUFI'
*    IMPORTING
*      LAUFD = ZDBSPR-LAUFD
*      LAUFI = ZDBSPR-LAUFI
*      NOTHING_SELECTED = xf4_c1
*    TABLES
*      LAUFK = TLAUFK.
*  IF xf4_c1 IS INITIAL.
*    LEAVE TO SCREEN 1000.
*  ENDIF.
*
*  WRITE ZDBSPR-LAUFI TO PA_LAUFI.
*ENDMODULE.                 " PA_LAUFI_VAL  INPUT
