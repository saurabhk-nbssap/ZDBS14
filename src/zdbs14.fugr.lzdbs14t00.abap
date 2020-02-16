*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 14.05.2012 at 16:30:48 by user 10106
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZDBSCP..........................................*
DATA:  BEGIN OF STATUS_ZDBSCP                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDBSCP                        .
CONTROLS: TCTRL_ZDBSCP
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZDBSPN..........................................*
DATA:  BEGIN OF STATUS_ZDBSPN                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDBSPN                        .
CONTROLS: TCTRL_ZDBSPN
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZDBSPR..........................................*
DATA:  BEGIN OF STATUS_ZDBSPR                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDBSPR                        .
CONTROLS: TCTRL_ZDBSPR
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZDBSTL..........................................*
DATA:  BEGIN OF STATUS_ZDBSTL                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDBSTL                        .
CONTROLS: TCTRL_ZDBSTL
            TYPE TABLEVIEW USING SCREEN '0004'.
*.........table declarations:.................................*
TABLES: *ZDBSCP                        .
TABLES: *ZDBSPN                        .
TABLES: *ZDBSPR                        .
TABLES: *ZDBSTL                        .
TABLES: ZDBSCP                         .
TABLES: ZDBSPN                         .
TABLES: ZDBSPR                         .
TABLES: ZDBSTL                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
