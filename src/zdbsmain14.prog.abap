*&---------------------------------------------------------------------*
*& Report  ZDBSMAIN14
*&
*&---------------------------------------------------------------------*

REPORT  zdbsmain14.

DATA: ok-code(4), fcode(4).

*CALL SCREEN 1000.
*&---------------------------------------------------------------------*
*&      Module  1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1000 INPUT.

  fcode = ok-code.
  CASE fcode.
    WHEN 'MTMT'. CALL TRANSACTION 'SM30'.
    WHEN 'ECPR'. CALL TRANSACTION 'F110'.
    WHEN 'GNPF'. CALL TRANSACTION 'ZDBS'."ZF14
    WHEN 'BACK' OR 'EXIT' OR 'CANC'. SET SCREEN 0.
  ENDCASE.

ENDMODULE.                 " 1000  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1000 OUTPUT.
  SET PF-STATUS '1000'.
  SET TITLEBAR '1000'.

ENDMODULE.                 " STATUS_1000  OUTPUT
