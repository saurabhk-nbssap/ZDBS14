*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZDELLUSR
*   generation date: 04.05.2012 at 15:45:04 by user 10106
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZDELLUSR           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
