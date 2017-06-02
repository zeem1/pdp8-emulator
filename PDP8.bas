      INSTALL @dir$+"/Number.bbc"
      REM INSTALL @lib$+"/multiwin.bbc"
      INSTALL @dir$+"status.bbc"
      INSTALL @dir$+"IOT.bbc"
      INSTALL @dir$+"RK8E.bbc"

      VDU 23,22,800;524;10,21,2,8:REM Window size
      VDU 19,1,2,0,0,0:REM Set foreground colour to green
      COLOUR128:COLOUR1:CLS

      REM PROC_multiwin(1):REM Multiple window support, 1 window
      REM HWND%=FN_createwin(1,"Output",100,100,640,512,0,0,0)
      REM OSCLI "FONT """ + @lib$ + "DejaVuSansMono"", 10"
      OSCLI "FONT """ + @dir$ + "Glass_TTY_VT220.ttf"", 15"
      test%=OPENOUT(@dir$+"/trace.log")
      :
      REM ON ERROR PROC_closewin(1):END
      ON ERROR CLOSE#0:END

      REM A%=accumulator, Q%=MQ register, C%=fetched memory contents, P%=program counter, M%=address of memory block, I%=instruction field, D%=data field, L%=link register
      REM S%=single-step enabled, U%=Status display enabled, K%=keyboard flag, T%=teleprinter flag
      :
      REM Init machine

      REM PC for FOCAL69 or memory test (0200), should be &FEE for RIM load. SR is 3777 for BIN load from HST:
      DIM M% 131071:P%=128:A%=0:L%=0:Q%=0:sr%=&7FF:int%=FALSE:int_inhib%=FALSE
      REM Memory control
      I%=0:D%=0:insbuffer%=0:intbuffer%=0:icontrol%=TRUE
      REM int%=Interrupts on/off, int_inhib%=interrupt inhibit (e.g. ION instruction), icontrol%=memory extension interrupt inhibit
      REM TTY/TAPE flags/buffers
      kint%=TRUE:kbdbuf$="":ttybuf$="":K%=FALSE:T%=TRUE:hstflag%=FALSE:hstbuffer%=0
      REM RK8E, testing
      rk_ca%=0:rk_com%=0:rk_da%=0:rk_st%=0:REM Curr addr, command, disk addr, status registers

      t%=TIME:S%=FALSE:U%=FALSE:REM PROCopen_status:REM temp - to allow single-step and status enable at beginning

      REM Set up the RIM and BIN loaders in memory
      FOR c%=&F97 TO &FFF:READ d%:PROCdeposit(I%+c%,d%):NEXT
      PROCdeposit(I%+4095,2753):REM JMP 7701 at location 7777
      REM BIN Loader (first byte at 7627, enter at 7701, 7777 if not overwritten by RIM loader)
      DATA 1674,2224,704,4072,2719,1162,3616,2711,650,4000,2712,652,188,737,3912,1174,4072,2966,652,174,687,1675,2712,56,3201,0,0,3097,2738,3102,1676,652,2992,3081,2744,3086
      DATA 2741,192,2275,3617,653,3842,3098,3084,3212,687,1675,3972,4032,747,746,1713,2198,2763,1677,651,1758,652,1790,2224,1773,2198,2749,2275,3856,2782,1678,766,749,653,2765,0,1934,1166,3968,2778,0,766,3654,3590,3590,749,3043,2738,6,0,0
      REM RIM Loader (begins at 7756)
      DATA &C0C,&C09,&AEF,&C0E,&E46,&E06,&F48,&AEF,&E06,&C09,&AF7,&C0E,&F10,&7FE,&6FE,&AEF,0,0

      REM The RK8E boot loader:
      FOR c%=19 TO 26:READ d%:PROCdeposit(I%+c%,d%):NEXT
      DATA3079,3556,538,3558,3555,538,2585,0

      REM Open RK05 image, test:
      rk_file%=OPENIN(@dir$+"/multos8.rk05")

      PRINT "PDP-8/e Emulator"
      PRINT "================"
      PRINT
      INPUT"Core image load (Y/N)",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASE c$ OF
        WHEN "Y":
          OSCLI"DIR "+@dir$:OSCLI". *.CORE"
          INPUT"IMAGE FILE NAME",F$
          file%=OPENIN(@dir$+"/"+F$)
          address%=I%
          REPEAT
            byte1%=BGET#file%:IFbyte1%=&0ATHENbyte1%=BGET#file%
            byte2%=BGET#file%
            PROCdeposit(address%,((byte1%-33)<<6) + (byte2%-33))
            address%+=1
          UNTIL EOF#file%:REM CLOSE#file%:PRINT
          PRINT"LOADED "+F$+" IMAGE"
        OTHERWISE:
          PRINT "RIM loader is at 7756, BIN loader at 7777, RK8E boot at 23"
      ENDCASE
      C%=FNexamine(I%+P%):PROCcommand:REM need to get start PC from user


      REM ** Main loop **
      REPEAT
        IF TIME>t%+10 THEN PROCkbd:t%=TIME
        IF int_inhib%<0 THEN int_inhib%+=1:REM IF NOT int_inhib% THEN int%=TRUE
        IF int% THEN IF FNirqline AND icontrol% AND NOT int_inhib% THEN int%=FALSE:PROCdeposit(FALSE,P%):intbuffer%=(I%>>9)+(D%>>12):I%=0:D%=0:P%=1
        startpc%=P%:PROCexecute:REM for status
        IFINKEY(-114)THENPROCcommand
        IF U% THEN d$=FNstatus(startpc%):PRINT#test%,d$:REM PRINTd$
        IF S% THEN PROCpause
      UNTIL FALSE
      :
      DEFFNaddr(eff_M%)
      LOCAL temp%
      IF (eff_M%AND&100) = 0 THEN
        =I%+(((eff_M% AND &80) >>7)*(P% AND &F80) + (eff_M% AND &7F)):REM direct
      ELSE
        temp%=((eff_M% AND &80) >>7)*(P% AND &F80) + (eff_M% AND &7F)
        IF temp%>7 AND temp%<16 THEN PROCdeposit(I%+temp%,(FNexamine(I%+temp%)+1)AND&FFF)
        =D%+FNexamine(I%+temp%)
      ENDIF

      DEFFNaddr_jump(eff_M%)
      LOCAL temp%
      IF (eff_M%AND&100) = 0 THEN
        =(((eff_M% AND &80) >>7)*(P% AND &F80) + (eff_M% AND &7F)):REM direct
      ELSE
        temp%=((eff_M% AND &80) >>7)*(P% AND &F80) + (eff_M% AND &7F)
        IF temp%>7 AND temp%<16 THEN PROCdeposit(I%+temp%,(FNexamine(I%+temp%)+1)AND&FFF)
        =FNexamine(I%+temp%)
      ENDIF
      :
      DEFPROCdeposit(address%,word%)
      M%!(address%<<2)=word%
      IF U% THEN PRINT #test%,"Depositing addr "+FNo0(address%,5)+" with value "+FNo0(word%,4)
      ENDPROC
      :
      DEFFNexamine(address%)
      IF U% THEN PRINT #test%,"Examining  addr "+FNo0(address%,5)+", result    "+FNo0(M%!(address%<<2),4)
      =M%!(address%<<2)
      :
      DEFPROCexecute
      LOCAL addr%,temp%
      C%=FNexamine(I%+P%):CASE (C% AND &E00) OF
        WHEN 0:     REM AND - and operand with AC
          A%=A% AND FNexamine(FNaddr(C%)):P%=(P%+1)AND&FFF
        WHEN &200:  REM TAD - add operand to (a 13 bit value)
          A%=A%+FNexamine(FNaddr(C%))
          IF A%>4095 THEN A%=A%-4096:L%=(NOT L%)AND1
          P%=(P%+1)AND&FFF
        WHEN &400:  REM ISZ - increment operand and skip if result is zero
          addr%=FNaddr(C%):temp%=(FNexamine(addr%)+1)AND&FFF:PROCdeposit(addr%,temp%):IFtemp%=FALSE THENP%=(P%+1)AND&FFF
          P%=(P%+1)AND&FFF
        WHEN &600:  REM DCA - deposit AC in memory and clear AC
          PROCdeposit(FNaddr(C%),A%):A%=0:P%=(P%+1)AND&FFF
        WHEN &800:  REM JMS - jump to subroutine
          REM re-enable interrupts via separate memory management control and transfer instruction field buffer to instruction field register
          REM icontrol%=TRUE:I%=insbuffer%:temp%=FNaddr_jump(C%):PROCdeposit(I%+temp%,P%+1):P%=temp%+1
          icontrol%=TRUE:temp%=FNaddr_jump(C%):I%=insbuffer%:PROCdeposit(I%+temp%,P%+1):P%=temp%+1
        WHEN &A00:  REM JMP - jump
          REM icontrol%=TRUE:I%=insbuffer%:P%=FNaddr_jump(C%)
          icontrol%=TRUE:P%=FNaddr_jump(C%):I%=insbuffer%
        WHEN &C00:  REM IOT - input/output transfer
          PROCiot
        WHEN &E00:  REM OPR - microcoded operations
          CASE (C% AND &F00) OF
            WHEN &E00:REM Group 1 (%1110xxxxxxxx)
              IF (C%AND&80)=&80 THEN A%=0:REM CLA
              IF (C%AND&40)=&40 THEN L%=FALSE  :REM CLL
              IF (C%AND&20)=&20 THEN A%=(NOT A%)AND&FFF:REM CMA
              IF (C%AND&10)=&10 THEN L%=(NOT L%)AND1  :REM CML
              IF (C%AND1)  =  1 THEN A%+=1:IF A%=4096 THEN A%=0:L%=(NOT L%)AND1: REM IAC - increment <L,AC>
              IF (C%AND4)  =  4 THEN A%=A%<<1:A%=A%+L%:L%=(A% AND &1000)>>12:A%=A% AND &FFF: REM RAL rotate <L,AC> left
              IF (C%AND8)  =  8 THEN A%=A%+L%*4096:L%=A% AND 1:A%=A%>>1: REM RAR rotate <L,AC> right
              IF (C%AND6)  =  6 THEN A%=A%<<1:A%=A%+L%:L%=(A% AND &1000)>>12:A%=A% AND &FFF: REM RTL rotate <L,AC> left twice
              IF (C%AND&A) = &A THEN A%=A%+L%*4096:L%=A% AND 1:A%=A%>>1: REM RTR rotate <L,AC> right twice
              IF (C%AND14) =  2 THEN temp%=A%AND&FC0:A%=((A%AND&3F)<<6)+(temp%>>6):REM BSW (8e and up)
            WHEN &F00: REM Groups 2 and 3 (%1111xxxxxxxx)
              IF (C%AND1)=0 THEN
                REM Group 2 (%1111xxxxxxx0), (OR|AND) group
                cond%=FALSE
                IF (C%AND&40)=&40THENIF A%>2047 cond%=TRUE: REM SMA - Skip on AC < 0 (or group)  | SPA – Skip on AC ≥ 0 (and group)
                IF (C%AND&20)=&20THENIF A%=0 cond%=TRUE: REM SZA - Skip on AC = 0 (or group)  | SNA – Skip on AC ≠ 0 (and group)
                IF (C%AND&10)=&10THENIF L%=1 cond%=TRUE: REM SNL - Skip on L != 0 (or group)  |  SZL – Skip on L = 0 (and group)
                IF (C%AND&80)=&80THEN A%=0: REM CLA
                IF (C%AND2)  =  2THEN PRINT'"CPU HALT"':PROCbell(150):PROCcommand
                IF (C%AND4)  =  4THEN A%=A% OR sr%:REM OSR - logically 'or' front-panel switches with AC
                IF (C%AND8)=0 THEN
                  IF cond%=TRUE THEN P%=(P%+1)AND&FFF:REM Bit 8 not set (OR), skip if any conditions true
                ELSE
                  IF cond%=FALSE THEN P%=(P%+1)AND&FFF:REM Bit 8 set (AND), skip if all conditions true
                ENDIF
              ELSE
                REM Group 3 (%1111xxxxxxx1); MQ instructions
                IF (C%AND128)=128THENA%=0:REM Bit 5 set, CLA
                IF (C%AND80)=64THENA%=A%ORQ%:REM Bit 6 set, MQA
                IF (C%AND80)=16THENQ%=A%:A%=0:REM Bit 8 set, MQL
                IF (C%AND80)=80THENtemp%=Q%:Q%=A%:A%=temp%:REM Bits 6 and 8 set, SWP
              ENDIF
          ENDCASE
          P%=(P%+1)AND&FFF
      ENDCASE
      ENDPROC
      :
      DEFPROCkbd
      LOCAL kbdtemp$:kbdtemp$=INKEY$(0):IFkbdtemp$<>""THEN
        IFASCkbdtemp$>96THENkbdtemp$=CHR$(ASC(kbdtemp$)AND223)
        kbdbuf$=kbdbuf$+kbdtemp$:K%=TRUE
      ENDIF
      ENDPROC
      :
      DEFPROCtprinter
      LOCALtemp%,pos%
      IFLENttybuf$>0THEN
        temp%=(ASCttybuf$)AND&7F
        CASE temp% OF
          WHEN12: temp%=0:REM Ignore form-feed (clear screen), this isn't a teleprinter
          WHEN 9: pos%=((POS+8)DIV8*8):PRINTSPC(pos%-POS);:REM PRINT"***";(pos%);:REM Expand tabs to 8 chars
          WHEN 7: PROCbell(200)
          OTHERWISE: VDUtemp%
        ENDCASE
        ttybuf$="":T%=TRUE
      ENDIF
      ENDPROC
      :
      DEFPROCtape
      REM HS Tape
      IF (NOT EOF#file%) THEN
        IF hstflag%= FALSE THEN hstbuffer%=BGET#file%:hstflag%=TRUE
      ELSE hstflag%=FALSE
      ENDIF
      ENDPROC
      :
      DEFPROCpause
      REPEATUNTILGET=32
      ENDPROC
      :
      DEFPROCcommand
      LOCALc$,p%:PRINTFNstatus(P%):REM S%=TRUE:REM *********** TEST *****************
      PRINT "Stopped. ";
      REPEAT
        OSCLI"FX15,1":INPUT"COMMAND:"'"(E)xamine/(D)eposit/(C)ont/(P)C/(T)ape/(S)ingle-step/Save core (I)mage/Stat(u)s Display",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
        CASE c$ OF
          WHEN "S":
            S%=NOT S%
          WHEN "U":
            U%=NOT U%
            REM IF U%=TRUE THEN
            REM PROCopen_status
            REM ELSE
            REM PROC_closewin(1)
            REM ENDIF
          WHEN "E":
            INPUT"EXAMINE ADDRESS";p%:PRINT"ADDR ";FNo0(I%+FNo2d(p%),5);" = ";FNo0(M%!((I%+FNo2d(p%))<<2),4)
          WHEN "P":
            INPUT"PC";p%:P%=FNo2d(p%)AND&FFF
          WHEN "D":
            PROCmanual_deposit
          WHEN "T":
            PROCchangetape
          WHEN "I":
            PROCsavecore
        ENDCASE
      UNTILc$="C"
      ENDPROC
      :
      DEFPROCchangetape
      LOCALF$
      CLOSE #0: REM CLOSE#file%
      OSCLI"DIR "+@dir$:OSCLI". *.BIN"
      INPUT"TAPE FILE NAME",F$
      file%=OPENIN(@dir$+"/"+F$):hstbuffer%=BGET#file%:hstflag%=TRUE:PRINT"LOADED "+F$+" TAPE"
      ENDPROC

      DEFPROCsavecore
      LOCAL F$,count%,n%,l%,file%,word%
      OSCLI"DIR "+@dir$:OSCLI". *.CORE"
      INPUT"IMAGE FILE NAME TO SAVE",F$
      INPUT"NUMBER OF WORDS",count%
      file%=OPENOUT(@dir$+"/"+F$)
      FORn%=0TOcount%-1
        word%=FNexamine(n%)
        BPUT#file%,((word%AND&FC0)>>6)+33:BPUT#file%,(word%AND&3F)+33
        IF n%MOD64=FALSE THEN BPUT#file%,10:REM Split into 64-character (32-word) lines
      NEXT:CLOSE#file%
      PRINT"SAVED "+F$+" IMAGE"
      ENDPROC

      DEFPROCmanual_deposit
      LOCALc$,c%
      INPUT "(A)C, (M)EMORY, (S)WITCH REG, (D)ATA FIELD, (I)NSTR FIELD",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASE c$ OF
        WHEN "A":
        WHEN "M":
        WHEN "S":
          REPEAT
            PRINT "Current switch register is ";FNo0(sr%,4):INPUT "Enter New Octal Value (0-7777):"c%
          UNTIL VAL(LEFT$(STR$c%,1))>=0 AND VAL(LEFT$(STR$c%,1))<=7 AND VAL(MID$(STR$c%,2,1))>=0 AND VAL(MID$(STR$c%,2,1))<=7 AND VAL(MID$(STR$c%,3,1))>=0 AND VAL(MID$(STR$c%,3,1))<=7 AND VAL(RIGHT$(STR$c%,1))>=0 AND VAL(RIGHT$(STR$c%,1))<=7
          sr%=FNo2d(c%)
        WHEN "D":
          REPEAT
            PRINT "Current data field is ";D%>>12:INPUT "Enter New Field (0-7):"c%
          UNTIL c%>=0 AND c%<=7
          D%=c%<<12
        WHEN "I":
          REPEAT
            PRINT "Current instruction field is ";I%>>12:INPUT "Enter New Field (0-7):"c%
          UNTIL c%>=0 AND c%<=7
          I%=c%<<12:insbuffer%=I%:REM second one is a test for problem when manually depositing IF
      ENDCASE
      ENDPROC

      DEFPROCbell(pitch%)
      LOCAL N%
      N%=-15:FORN%=-15TO0
        SOUND1,N%,pitch%,2:SOUND2,N%,pitch%/1.125,2
      NEXT
      ENDPROC

      DEFFNirqline
      REM PRINT "FNinterrupt, K%=";K%;" T%=";T%;" kint%=";kint%
      =(K% OR T%) AND kint%
