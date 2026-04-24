      BLOCK DATA INIT
      INTEGER BOARD(120)
      COMMON /PLRDAT/ BOARD
      INTEGER EMPTY, OFFBRD
      INTEGER WPAWN, WKNGHT, WBISH, WROOK, WQUEN, WKING
      INTEGER BPAWN, BKNGHT, BBISH, BROOK, BQUEN, BKING
      
      PARAMETER (EMPTY=0, OFFBRD=99)
      PARAMETER (WPAWN=1, WKNGHT=2, WBISH=3, WROOK=4, WQUEN=5, WKING=6)
      PARAMETER (BPAWN=-1, BKNGHT=-2, BBISH=-3, BROOK=-4, BQUEN=-5, BKI
     +NG=-6)
      INTEGER MVSTCK(3,2000), CURRSP
      COMMON /STACK/ CURRSP,MVSTCK 
      DATA BOARD /120*0/ 
      DATA MVSTCK /6000*0/
      DATA CURRSP /0/
      END

      PROGRAM MAIN
      CHARACTER*80 CMD
      LOGICAL ENGSID, SIDMOV
      CALL INIBRD
      ENGSID = .FALSE. 
      SIDMOV = .TRUE. 
   10 CONTINUE
        READ(*,'(A)') CMD

        IF (CMD(1:8) .EQ. 'xboard') THEN
           CONTINUE
        ELSE IF (CMD(1:4) .EQ. 'new') THEN
           CALL INIBRD
           SIDMOV = .TRUE.
        ELSE IF (CMD(1:5) .EQ. 'force') THEN
           ENGSID= .FALSE. 
        ELSE IF (CMD(1:2) .EQ. 'go') THEN
           ENGSID = SIDMOV
           CALL DOMOVE(SIDMOV)
        ELSE IF (CMD(1:4) .EQ. 'quit') THEN
           STOP
        ELSE IF (CMD(1:5).EQ.'print') THEN 
            CALL PRTBRD
        ELSE IF (CMD(1:6).EQ.'pieces') THEN 
            CALL PCAMNT
        ELSE IF (CMD(1:4).EQ.'list') THEN 
            CALL LSTMVS
        ELSE IF (CMD(1:4).EQ.'info') THEN 
            WRITE(*,*) SIDMOV, ENGSID 
        ELSE
           CALL PRSNMV(CMD)
           SIDMOV = .NOT. SIDMOV
           IF (ENGSID .EQV. SIDMOV) CALL DOMOVE(SIDMOV)
        END IF
      GOTO 10
  900 FORMAT(1I2)
      END

      SUBROUTINE INIBRD
      INTEGER EMPTY, OFFBRD
      INTEGER WPAWN, WKNGHT, WBISH, WROOK, WQUEN, WKING
      INTEGER BPAWN, BKNGHT, BBISH, BROOK, BQUEN, BKING
      PARAMETER (EMPTY=0, OFFBRD=99)
      PARAMETER (WPAWN=1, WKNGHT=2, WBISH=3, WROOK=4, WQUEN=5, WKING=6)
      PARAMETER (BPAWN=-1, BKNGHT=-2, BBISH=-3, BROOK=-4, BQUEN=-5, BKI
     +NG=-6)

      INTEGER GETIDX
      INTEGER BOARD(120)
      COMMON /PLRDAT/ BOARD
      DO 10 I = 1, 120
         BOARD(I) = OFFBRD
   10 CONTINUE

      DO 30 IRANK = 1, 8
         DO 20 IFILE = 1, 8
            I = GETIDX(IRANK,IFILE)
            BOARD(I) = EMPTY
   20    CONTINUE
   30 CONTINUE

      DO 40 IFILE = 1,8
        I = GETIDX(2,IFILE)
        BOARD(I) = WPAWN 
        I = GETIDX(7,IFILE)
        BOARD(I) = BPAWN
   40 CONTINUE
      BOARD(GETIDX(1,1)) = WROOK
      BOARD(GETIDX(1,8)) = WROOK
      BOARD(GETIDX(1,2)) = WKNGHT
      BOARD(GETIDX(1,7)) = WKNGHT
      BOARD(GETIDX(1,3)) = WBISH
      BOARD(GETIDX(1,4)) = WQUEN
      BOARD(GETIDX(1,5)) = WKING
      BOARD(GETIDX(1,6)) = WBISH

      BOARD(GETIDX(8,1)) = BROOK
      BOARD(GETIDX(8,8)) = BROOK
      BOARD(GETIDX(8,2)) = BKNGHT
      BOARD(GETIDX(8,7)) = BKNGHT
      BOARD(GETIDX(8,3)) = BBISH
      BOARD(GETIDX(8,4)) = BQUEN
      BOARD(GETIDX(8,5)) = BKING
      BOARD(GETIDX(8,6)) = BBISH
      END


      FUNCTION GETIDX(RANK,FILE)
        INTEGER RANK,FILE, GETIDX
        GETIDX = (RANK+1) * 10 + (FILE+1)
      END 

      SUBROUTINE MAKMOV(FROM,TOWARD)
        INTEGER EMPTY 
        PARAMETER(EMPTY=0)
        INTEGER FROM, TOWARD 
        INTEGER BOARD(120) 
        COMMON /PLRDAT/ BOARD 
        BOARD(TOWARD) = BOARD(FROM)
        BOARD(FROM) = EMPTY
      END 
      SUBROUTINE PRSNMV(CMD)
      INTEGER GETIDX
      CHARACTER*(*) CMD
      INTEGER I1, I2, F1, R1, F2, R2
    
      F1 = ICHAR(CMD(1:1)) - ICHAR('a') + 1
      R1 = ICHAR(CMD(2:2)) - ICHAR('0')
      F2 = ICHAR(CMD(3:3)) - ICHAR('a') + 1
      R2 = ICHAR(CMD(4:4)) - ICHAR('0')

      I1 = GETIDX(R1,F1)
      I2 = GETIDX(R2,F2)

      CALL MAKMOV(I1, I2)
      END

      SUBROUTINE DOMOVE(SIDMOV)
      LOGICAL SIDMOV
      INTEGER BOARD(120)
      COMMON /PLRDAT/ BOARD 
      INTEGER EMPTY, OFFBRD
      INTEGER WPAWN, WKNGHT, WBISH, WROOK, WQUEN, WKING
      INTEGER BPAWN, BKNGHT, BBISH, BROOK, BQUEN, BKING
      PARAMETER (EMPTY=0, OFFBRD=99)
      PARAMETER (WPAWN=1, WKNGHT=2, WBISH=3, WROOK=4, WQUEN=5, WKING=6)
      PARAMETER (BPAWN=-1, BKNGHT=-2, BBISH=-3, BROOK=-4, BQUEN=-5, BKI
     +NG=-6)
      INTEGER OFSMUL 
      INTEGER RKOFST(4), BPOFST(4), QNOFST(8)
      DATA RKOFST /10,-10,+1,-1/ 
      DATA BPOFST /11,+9,-9,-11/
      DATA QNOFST /10,-10,+1,-1,11,+9,-9,-11/
      CALL RSTSTK
      DO 10 I=21,100
        OFSMUL = 1
        IF(BOARD(I) .EQ. EMPTY .OR. BOARD(I) .EQ. OFFBRD) GOTO 10 
        IF(SIDMOV.EQV..TRUE..AND.BOARD(I).GT.EMPTY) THEN
            IF (BOARD(I) .EQ. WROOK) THEN
                DO 20 IX=1,4
                OFSMUL = 1
   30 CONTINUE
            ITARGT = I + RKOFST(IX) * OFSMUL
            IF (ITARGT .LT. 1 .OR. ITARGT .GT. 120) GOTO 20
                IF (BOARD(ITARGT) .EQ. EMPTY) THEN
                    CALL PSHSTK(I,ITARGT,BOARD(ITARGT))
                    OFSMUL = OFSMUL + 1
                ELSE IF (BOARD(ITARGT) .LT. EMPTY) THEN
                    CALL PSHSTK(I,ITARGT,BOARD(ITARGT))
                    GOTO 20
                ELSE IF (BOARD(ITARGT) .GT. EMPTY) THEN
                    GOTO 20 
                ELSE IF (BOARD(ITARGT) .EQ. OFFBRD) THEN
                    GOTO 20
                END IF
                GOTO 30
   20           CONTINUE 
            ELSE IF (BOARD(I) .EQ. WBISH) THEN 
            DO 60 IX=1,4
                OFSMUL = 1
   70 CONTINUE
            ITARGT = I + BPOFST(IX) * OFSMUL
            IF (ITARGT .LT. 1 .OR. ITARGT .GT. 120) GOTO 60
                IF (BOARD(ITARGT) .EQ. EMPTY) THEN
                    CALL PSHSTK(I,ITARGT,BOARD(ITARGT))
                    OFSMUL = OFSMUL + 1
                ELSE IF (BOARD(ITARGT) .LT. EMPTY) THEN
                    CALL PSHSTK(I,ITARGT,BOARD(ITARGT))
                    GOTO 60
                ELSE IF (BOARD(ITARGT) .GT. EMPTY) THEN
                    GOTO 60 
                ELSE IF (BOARD(ITARGT) .EQ. OFFBRD) THEN
                    GOTO 60
                END IF
                GOTO 70
   60           CONTINUE
      ELSE IF (BOARD(I) .EQ. WQUEN) THEN 
            DO 100 IX=1,8
                OFSMUL = 1
  110 CONTINUE
            ITARGT = I + QNOFST(IX) * OFSMUL
            IF (ITARGT .LT. 1 .OR. ITARGT .GT. 120) GOTO 100
                IF (BOARD(ITARGT) .EQ. EMPTY) THEN
                    CALL PSHSTK(I,ITARGT,BOARD(ITARGT))
                    OFSMUL = OFSMUL + 1
                ELSE IF (BOARD(ITARGT) .LT. EMPTY) THEN
                    CALL PSHSTK(I,ITARGT,BOARD(ITARGT))
                    GOTO 100
                ELSE IF (BOARD(ITARGT) .GT. EMPTY) THEN
                    GOTO 100 
                ELSE IF (BOARD(ITARGT) .EQ. OFFBRD) THEN
                    GOTO 100
                END IF
                GOTO 110
  100 CONTINUE
            END IF

        ELSE IF (SIDMOV.EQV..FALSE..AND.BOARD(I).LT.EMPTY) THEN 
        IF (BOARD(I) .EQ. BROOK) THEN
                DO 40 IX=1,4
                OFSMUL = 1
   50 CONTINUE
            ITARGT = I + RKOFST(IX) * OFSMUL
            IF (ITARGT .LT. 1 .OR. ITARGT .GT. 120) GOTO 40
                IF (BOARD(ITARGT) .EQ. EMPTY) THEN
                    CALL PSHSTK(I,ITARGT,BOARD(ITARGT))
                    OFSMUL = OFSMUL + 1
                ELSE IF (BOARD(ITARGT) .GT. EMPTY) THEN
                    CALL PSHSTK(I,ITARGT,BOARD(ITARGT))
                    GOTO 40
                ELSE IF (BOARD(ITARGT) .LT. EMPTY) THEN
                    GOTO 40 
                ELSE IF (BOARD(ITARGT) .EQ. OFFBRD) THEN
                    GOTO 40
                END IF
                GOTO 50
   40           CONTINUE 
      ELSE IF (BOARD(I) .EQ. BBISH) THEN 
              DO 80 IX=1,4
                  OFSMUL = 1
   90             CONTINUE
                  ITARGT = I + BPOFST(IX) * OFSMUL
                  IF (ITARGT .LT. 1 .OR. ITARGT .GT. 120) GOTO 80

                  IF (BOARD(ITARGT) .EQ. EMPTY) THEN
                      CALL PSHSTK(I,ITARGT,BOARD(ITARGT))
                      OFSMUL = OFSMUL + 1
                      GOTO 90
                  ELSE IF (BOARD(ITARGT) .GT. EMPTY) THEN
                      CALL PSHSTK(I,ITARGT,BOARD(ITARGT))
                      GOTO 80                  
                  END IF
                  IF (BOARD(ITARGT).LT.EMPTY) GOTO 80
   80         CONTINUE
      ELSE IF (BOARD(I) .EQ. BQUEN) THEN 
            DO 120 IX=1,8
                OFSMUL = 1
  130 CONTINUE
            ITARGT = I + QNOFST(IX) * OFSMUL
            IF (ITARGT .LT. 1 .OR. ITARGT .GT. 120) GOTO 120
                IF (BOARD(ITARGT) .EQ. EMPTY) THEN
                    CALL PSHSTK(I,ITARGT,BOARD(ITARGT))
                    OFSMUL = OFSMUL + 1
                ELSE IF (BOARD(ITARGT) .GT. EMPTY) THEN
                    CALL PSHSTK(I,ITARGT,BOARD(ITARGT))
                    GOTO 120
                ELSE IF (BOARD(ITARGT) .LT. EMPTY) THEN
                    GOTO 120 
                ELSE IF (BOARD(ITARGT) .EQ. OFFBRD) THEN
                    GOTO 120
                END IF
                GOTO 130
  120 CONTINUE
          END IF
      END IF 
   10 CONTINUE
      
      END

      SUBROUTINE PSHSTK(FROM,TOWARD,LSTPC)
        INTEGER CURRSP, MVSTCK(3,2000)
        COMMON /STACK/ CURRSP,MVSTCK
        INTEGER FROM,TOWARD,LSTPC
        CURRSP = CURRSP + 1
        MVSTCK(1,CURRSP) = FROM
        MVSTCK(2,CURRSP) = TOWARD 
        MVSTCK(3,CURRSP) = LSTPC 
      END
      
      SUBROUTINE PRTBRD
      INTEGER GETIDX
      INTEGER INBRD(120)
      COMMON /PLRDAT/ INBRD 
      DO10IX=8,1,-1
      WRITE(*,900)INBRD(GETIDX(IX,1)),INBRD(GETIDX(IX,2)),INBRD(GETIDX(I
     1X,3)),INBRD(GETIDX(IX,4)),INBRD(GETIDX(IX,5)),INBRD(GETIDX(IX,6)),
     2INBRD(GETIDX(IX,7)),INBRD(GETIDX(IX,8))
   10 CONTINUE  
  900 FORMAT(8I4)
  910 FORMAT(A)
      END 
      
      SUBROUTINE LSTMVS
      CHARACTER*4 GTPIEC
      INTEGER CURRSP, MVSTCK(3,2000)
      INTEGER BOARD(120)
      COMMON/STACK/ CURRSP,MVSTCK 
      COMMON/PLRDAT/BOARD
      IF(CURRSP.EQ.0) RETURN
      DO 10IX=1,CURRSP 
      WRITE(*,900)GTPIEC(BOARD(MVSTCK(1,IX))),MVSTCK(1,IX),MVSTCK(2,IX)
   10 CONTINUE  
  900 FORMAT('PIECE ',1A4,' FROM ',1I3,' TO ',1I3)
      END

      FUNCTION GTPIEC(SELEPC) 
      CHARACTER*4 GTPIEC, PCRAY(6)
      INTEGER SELEPC,BOARD(120)
      COMMON/PLRDAT/BOARD
    
      DATA PCRAY/'PAWN','KNIGT','BSHP','ROOK','QUEN','KING'/
      
      GTPIEC = PCRAY(ABS(SELEPC))
  910 FORMAT(1I2) 
      END

      SUBROUTINE PCAMNT 
      INTEGER GETIDX
      CHARACTER*4 GTPIEC
      INTEGER BOARD(120), PCLIST(-6:6)
      COMMON/PLRDAT/BOARD 
      
      DO 10 IRANK=1,8
       DO 20 IFILE=1,8
        ITARGT = GETIDX(IRANK,IFILE)
        PCLIST(BOARD(ITARGT)) = PCLIST(BOARD(ITARGT))+1
   20  CONTINUE 
   10 CONTINUE  
      DO 30 IX=-6,6
        IF(IX.EQ.0) THEN 
          WRITE(*,910) 'BLACK END'
          WRITE(*,910) '----------------'
          WRITE(*,910) 'WHITE START'
        ELSE
          WRITE(*,900) GTPIEC(IX),PCLIST(IX)
        END IF
  900 FORMAT(A,' AMOUNT: ',I3)     
  910 FORMAT(A)
   30 CONTINUE  
      END

      SUBROUTINE RSTSTK
      INTEGER CURRSP,MVSTCK(3,2000)
      COMMON /STACK/ CURRSP,MVSTCK 
      CURRSP=0
      END
