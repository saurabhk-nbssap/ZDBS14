*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 04.05.2012 at 15:53:12 by user 10106
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZDBSUSER........................................*
DATA:  BEGIN OF STATUS_ZDBSUSER                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDBSUSER                      .
CONTROLS: TCTRL_ZDBSUSER
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDBSUSER                      .
TABLES: ZDBSUSER                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
