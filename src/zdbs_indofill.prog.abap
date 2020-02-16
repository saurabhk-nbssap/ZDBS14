*&---------------------------------------------------------------------*
*  Program        : ZDBS_INDOFILL PAYMENT FILE EXTRACTION FOR NEW IDEAL
*  Process        : To be run after a Paymnet Run.
*                   Validates data for output IDEAL file formats.
*  Creation Date  : 17/4/2012
*  Author         : Poornima.N Dell Services Pvt Ltd.
*&---------------------------------------------------------------------*
*& Report  ZDBS_INDOFILL
*&---------------------------------------------------------------------*
 REPORT   ZDBS_INDOFILL.

 INCLUDE ZDBS_INDOFILL_TOP.  " global Data
 INCLUDE ZDBS_INDOFILL_S01.   " Selection Screen
 INCLUDE ZDBS_INDOFILL_F01.  " FORM-Routines

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  l_usname = sy-uname.
  PERFORM check_user_authorization.
  gv_rc = 0.
  PERFORM check_file_limit CHANGING gv_rc.
*----------------------------------------------------------------------*
* Make the variant as default variant
*----------------------------------------------------------------------*
  DATA: h_repid   LIKE rsvar-report,
        h_variant LIKE rsvar-variant.

  h_repid = sy-repid.
  h_variant = sy-uname.

  CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
    EXPORTING
      report               = h_repid
      variant              = h_variant
    EXCEPTIONS
      variant_not_existent = 01
      variant_obsolete     = 02.
  IF sy-subrc NE 0.
    CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
      EXPORTING
        report               = h_repid
        variant              = 'ZDBS'
      EXCEPTIONS
        variant_not_existent = 01
        variant_obsolete     = 02.
  ENDIF.
*-----------------------------------------------------------------------
* At-Selection-Screen-Output
*-----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.
  DATA:rc LIKE sy-subrc.
*-----------------------------------------------------------------------
* At-Selection-Screen
*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_cems.
PERFORM gui_directory_browse USING 'CEMS Folder Location'
                               CHANGING pa_cems.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_pfile.
  PERFORM gui_directory_browse USING 'PRIME Folder Location'
                               CHANGING pa_pfile.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_neft.
  PERFORM gui_directory_browse USING 'NEFT Folder Location'
                               CHANGING pa_neft.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_rtgs .
  PERFORM gui_directory_browse USING 'RTGS Folder Location'
                               CHANGING pa_rtgs.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_error.
  PERFORM gui_directory_browse USING 'Error Folder Location'
                               CHANGING pa_error.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_path.

  gv_no_files1 = 1.

  gv_user_action1 = 9.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Batch File location'
      initial_directory       = gv_batch_path
    CHANGING
      file_table              = it_filetable1
      rc                      = gv_no_files1
      user_action             = gv_user_action1
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  READ TABLE it_filetable1 INTO wa_filetable1 INDEX 1.

  IF sy-subrc EQ 0.

    pa_path = wa_filetable1-filename.

  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = g_variant
      i_save     = g_save
    IMPORTING
      e_exit     = g_exit
      es_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S'      NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF g_exit = space.
      pa_path = gx_variant-variant.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_keyst.

  gv_no_files = 1.

  gv_user_action = 9.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Keystore file location'
      initial_directory       = gv_cems_path
    CHANGING
      file_table              = it_filetable
      rc                      = gv_no_files
      user_action             = gv_user_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE it_filetable INTO wa_filetable INDEX 1.

  IF sy-subrc EQ 0.

    pa_keyst = wa_filetable-filename.

  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = g_variant
      i_save     = g_save
    IMPORTING
      e_exit     = g_exit
      es_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S'      NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF g_exit = space.
      pa_keyst = gx_variant-variant.
    ENDIF.
  ENDIF.
*-----------------------------------------------------------------------
*  TOP OF PAGE
*-----------------------------------------------------------------------
TOP-OF-PAGE.

  IF pa_test = 'X'.
    w_title = 'Payment File Extraction - Test Run with Payment Proposal Only'.
  ELSE.
    w_title = 'Payment File Extraction - Actual Run'.
  ENDIF.

  FORMAT COLOR COL_HEADING ON.
  WRITE:  /1 sy-repid, 24 w_title, 120 sy-datum, 80 '/',
            85 sy-uzeit USING EDIT MASK '__:__:__',
          /1 sy-uname, 18 'Payment Run Date:', ireguh-laufd,
             'ID:', ireguh-laufi, 110 'Page' , sy-pagno.
  ULINE.

  FORMAT COLOR COL_HEADING OFF.
*---------------------------------------------------------------------*
* Start of selection
*--------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM init_filenames CHANGING gv_rc.

  IF gv_rc = 0.
      PERFORM check_files_exist CHANGING gv_rc.
      PERFORM check_files_exist_outfolder CHANGING gv_rc.
  ENDIF.

  IF gv_rc = 0.
    PERFORM check_payment_run CHANGING gv_rc pa_test.
  ENDIF.

  IF gv_rc = 0.
    PERFORM process_data CHANGING gv_id.
    gv_identity = gv_id.
    PERFORM set_paymts.
  ENDIF.

END-OF-SELECTION.
*---------------------------------------------------------------------*
* End of selection
*---------------------------------------------------------------------*
if  gv_rc = 0 AND it_err[] IS INITIAL and pa_test is initial.
  PERFORM save_files USING gv_identity.
  IF NOT p_cems IS INITIAL.
    PERFORM encrypt_files.
  ENDIF.
endif.

  PERFORM display_log.

  IF ( pa_test IS INITIAL ) AND gv_rc = 0.
    PERFORM update_database.
  ENDIF.
