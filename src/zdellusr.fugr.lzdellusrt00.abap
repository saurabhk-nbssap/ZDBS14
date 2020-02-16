*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 04.05.2012 at 15:45:04 by user 10106
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZDELLUSR........................................*
DATA:  BEGIN OF STATUS_ZDELLUSR                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDELLUSR                      .
CONTROLS: TCTRL_ZDELLUSR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDELLUSR                      .
TABLES: ZDELLUSR                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
