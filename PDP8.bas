      INSTALL @dir$+"/Number.bbc"
      INSTALL @lib$+"/multiwin.bbc"
      INSTALL @dir$+"status.bbc"
      INSTALL @dir$+"IOT.bbc"
      REM INSTALL @dir$+"EAE.bbc"
      INSTALL @dir$+"RK8E.bbc"

      VDU 23,22,800;524;10,21,2,8:REM Window and font sizes

      OSCLI"ESC OFF":REM ASCII 27 needed in emulator
      COLOUR&80:COLOUR1:CLS

      PROC_multiwin(1):REM Multiple window support, 1 window
      HWND%=FN_createwin(1,"Debug window",100,100,640,512,0,0,0)
      PROC_selectwin(1):VDU 23,22,640;256;8,16,2,8:REM Window size
      REM OSCLI "FONT """ + @lib$ + "DejaVuSansMono"", 12"
      PROC_selectwin(0):VDU 19,1,2,0,0,0:REM Set foreground colour to green
      OSCLI "FONT """ + @dir$ + "Glass_TTY_VT220.ttf"", 15"

      CLOSE#0:trace%=OPENOUT(@dir$+"/trace.log"):screen%=OPENOUT(@dir$+"/screen.txt")
      file%=FALSE:rk_file0%=FALSE:rk_file1%=FALSE:rk_file2%=FALSE:rk_file3%=FALSE:REM Prevents failure when no tape or disk image opened
      :
      ON ERROR PROC_closewin(1):REPORT:CLOSE#0:END
      ON CLOSE PROC_closewin(1):CLOSE#0:QUIT

      DIM M% 131071
      OSCLI"TIMER 20":ON TIME PROCkbd:RETURN

      PROCinit

      PRINT "PDP-8/e Emulator"
      PRINT "================"
      PRINT '"RIM loader is at 7756, BIN loader at 7777, RK8E boot at 23"'
      C%=FNexamine(I%+P%):PROCcommand:REM User needs to set the machine up
      :
      REM ** Main loop **
      REPEAT
        IFINKEY(-114)THENPROCcommand
        IF int% THEN IF NOT int_inhib% THEN IF FNirqline THEN IF icontrol% THEN int%=FALSE:PROCdeposit(FALSE,P%):intbuffer%=(I%>>&9)+(D%>>&C):I%=FALSE:insbuffer%=FALSE:D%=FALSE:P%=&1
        IF int_inhib%<FALSE THEN int_inhib%-=TRUE
        startpc%=P%:PROCexecute
        IF TF% OR TS% THEN
          d$=FNstatus(startpc%)
          IF TF% THEN PROCtrace_file(d$)
          IF TS% THEN PROC_selectwin(&1):PRINTd$:PROC_selectwin(0)
        ENDIF
        IF S% THEN PROCpause
      UNTIL FALSE
      :
      DEFFNaddr(eff_M%)
      IF (eff_M%AND&100)=FALSE THEN
        =I%+(((eff_M% AND &80) >>&7)*(P% AND &F80) + (eff_M% AND &7F))
      ELSE
        temp%=((eff_M% AND &80) >>&7)*(P% AND &F80) + (eff_M% AND &7F)
        IF temp%>&7 AND temp%<&10 THEN PROCdeposit(I%+temp%,(FNexamine(I%+temp%)-TRUE)AND&FFF)
        =D%+FNexamine(I%+temp%)
      ENDIF
      DEFPROCdeposit(address%,word%) M%!(address%<<&2)=word% ENDPROC
      DEFFNexamine(address%) =M%!(address%<<&2)
      DEFFNirqline =(K% OR T%) AND kint%
      DEFPROCexecute
      C%=FNexamine(I%+P%):CASE C% AND &E00 OF
        WHEN FALSE: A%=A% AND FNexamine(FNaddr(C%)):P%=(P%-TRUE)AND&FFF:REM AND - and operand with AC
        WHEN &200:A%=A%+FNexamine(FNaddr(C%)):P%=(P%-TRUE)AND&FFF:IF A%>&FFF THEN A%=A%-&1000:L%=(NOT L%)AND&1:REM TAD - add operand to (a 13 bit value)
        WHEN &400:addr%=FNaddr(C%):temp%=(FNexamine(addr%)-TRUE)AND&FFF:PROCdeposit(addr%,temp%):P%=(P%-TRUE)AND&FFF:IFtemp%=FALSE THENP%=(P%-TRUE)AND&FFF:REM ISZ - increment operand and skip if result is zero
        WHEN &600:PROCdeposit(FNaddr(C%),A%):A%=FALSE:P%=(P%-TRUE)AND&FFF:REM DCA - deposit AC in memory and clear AC
        WHEN &800:icontrol%=TRUE:temp%=FNaddr(C%)AND&FFF:I%=insbuffer%:PROCdeposit(I%+temp%,P%-TRUE):P%=temp%-TRUE:REM JMS - jump to subroutine; re-enable interrupts via separate memory management control and transfer instruction field buffer to instruction field register
        WHEN &A00:icontrol%=TRUE:P%=FNaddr(C%)AND&FFF:I%=insbuffer%:REM JMP - jump
        WHEN &C00:PROCiot:REM IOT - input/output transfer
        WHEN &E00:  REM OPR - microcoded operations
          CASE (C% AND &F00) OF
            WHEN &E00:REM Group 1 (%1110xxxxxxxx)
              IF (C%AND&80)=&80 THEN A%=FALSE:REM CLA
              IF (C%AND&40)=&40 THEN L%=FALSE  :REM CLL
              IF (C%AND&20)=&20 THEN A%=(NOT A%)AND&FFF:REM CMA
              IF (C%AND&10)=&10 THEN L%=(NOT L%)AND&1  :REM CML
              IF (C%AND&1) = &1 THEN A%-=TRUE:IF A%=&1000 THEN A%=FALSE:L%=(NOT L%)AND&1: REM IAC - increment <L,AC>
              IF (C%AND&4) = &4 THEN A%=A%<<&1:A%=A%+L%:L%=(A% AND &1000)>>&C:A%=A% AND &FFF: REM RAL rotate <L,AC> left
              IF (C%AND&8) = &8 THEN A%=A%+L%*&1000:L%=A% AND &1:A%=A%>>&1: REM RAR rotate <L,AC> right
              IF (C%AND&6) = &6 THEN A%=A%<<&1:A%=A%+L%:L%=(A% AND &1000)>>&C:A%=A% AND &FFF: REM RTL rotate <L,AC> left twice
              IF (C%AND&A) = &A THEN A%=A%+L%*&1000:L%=A% AND &1:A%=A%>>&1: REM RTR rotate <L,AC> right twice
              IF (C%AND&E) = &2 THEN temp%=A%AND&FC0:A%=((A%AND&3F)<<&6)+(temp%>>&6):REM BSW (8e and up)
            WHEN &F00: REM Groups 2 and 3 (%1111xxxxxxxx)
              IF (C%AND&1)=FALSE THEN
                REM Group 2 (%1111xxxxxxx0), (OR|AND) group
                cond%=FALSE
                IF (C%AND&40)=&40THENIF A%>&7FF cond%=TRUE: REM SMA - Skip on AC < 0 (or group)  | SPA – Skip on AC ≥ 0 (and group)
                IF (C%AND&20)=&20THENIF A%=FALSE cond%=TRUE: REM SZA - Skip on AC = 0 (or group)  | SNA – Skip on AC ≠ 0 (and group)
                IF (C%AND&10)=&10THENIF L%=&1 cond%=TRUE: REM SNL - Skip on L != 0 (or group)  |  SZL – Skip on L = 0 (and group)
                IF (C%AND&80)=&80THEN A%=FALSE: REM CLA
                IF (C%AND&2) = &2THEN PRINT'"CPU HALT"':PROCbell(150):PROCcommand:REM Bell crashes on Toshiba
                IF (C%AND&4) = &4THEN A%=A% OR sr%:REM OSR - logically 'or' front-panel switches with AC
                IF (C%AND&8)=FALSE THEN
                  IF cond%=TRUE THEN P%=(P%-TRUE)AND&FFF:REM Bit 8 not set (OR), skip if any conditions true
                ELSE
                  IF cond%=FALSE THEN P%=(P%-TRUE)AND&FFF:REM Bit 8 set (AND), skip if all conditions true
                ENDIF
              ELSE
                REM Group 3 (%1111xxxxxxx1); MQ/EAE instructions
                REM Standard MQ instructions
                IF (C%AND&D0)=&80THENA%=FALSE:REM Bit 4 set and bit 7 clear, CLA
                IF (C%AND&D0)=&40THENA%=A%ORQ%:REM Bit 5 set, MQA
                IF (C%AND&D0)=&C0THENA%=Q%:REM Bits 4 and 5 set, CLA+MQA
                IF (C%AND&D0)=&10THENQ%=A%:A%=FALSE:REM Bit 7 set, MQL
                IF (C%AND&D0)=&50THENtemp%=Q%:Q%=A%:A%=temp%:REM Bits 5 and 7 set (MQA+MQL), SWP
                IF (C%AND&D0)=&90THENA%=FALSE:Q%=FALSE:REM Bits 4 and 7 set (CLA+MQL), CAM
                PROCeae:REM Put here in case I add an option to disable the EAE
              ENDIF
          ENDCASE
          P%=(P%-TRUE)AND&FFF
      ENDCASE
      ENDPROC
      :
      DEFPROCkbd
      LOCAL kbdtemp$
      kbdtemp$=INKEY$(0):IFkbdtemp$<>""THEN
        IFASCkbdtemp$>96THENkbdtemp$=CHR$(ASC(kbdtemp$)AND&DF)
        kbdbuf$=kbdtemp$:K%=TRUE
      ENDIF
      ENDPROC
      :
      DEFPROCtprinter
      LOCALtemp%,pos%
      IFLENttybuf$>0THEN
        temp%=(ASCttybuf$)AND&7F
        CASE temp% OF
          WHEN &C: temp%=FALSE:REM Ignore form-feed (clear screen), this isn't a teleprinter
          WHEN &9: pos%=((POS+&8)DIV&8*&8):PRINTSPC(pos%-POS);:PRINT#screen%,SPC(pos%-POS);:REM Expand tabs to 8 chars
          WHEN &7: PROCbell(200)
          WHEN FALSE: REM suppress output of ASCII 0 to text output file
          OTHERWISE: VDUtemp%:BPUT#screen%,temp%
        ENDCASE
        ttybuf$="":T%=TRUE:PTR#screen%=PTR#screen%:REM flush buffer
      ENDIF
      ENDPROC
      :
      DEFPROCtape
      REM HS Tape
      IF file%<>0 THEN
        IF (NOT EOF#file%) THEN
          IF hstflag%= FALSE THEN hstbuffer%=BGET#file%:hstflag%=TRUE
        ELSE hstflag%=FALSE
        ENDIF
      ENDIF
      ENDPROC
      :
      DEFPROCpause
      REPEATUNTILGET=32
      ENDPROC
      :
      DEFPROCcommand
      LOCALc$,p%:PRINTFNstatus(P%)
      PRINT "Stopped. ";
      REPEAT
        OSCLI"FX15,1":INPUT"COMMAND:"'"(C)ont/(E)xamine/(D)eposit/(F)ile/De(b)ug/(Q)uit",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
        CASE c$ OF
          WHEN "E":
            INPUT"EXAMINE ADDRESS";p%:PRINT"ADDR ";FNo0(FNo2d(p%),5);" = ";FNo0(M%!((FNo2d(p%))<<2),4)
          WHEN "D":
            PROCmanual_deposit
          WHEN "F":
            PROCfile
          WHEN "B":
            PROCdebug
          WHEN "Q":
            PROCquit
        ENDCASE
      UNTILc$="C"
      ENDPROC
      :
      DEFPROCfile
      LOCALF$,c$
      INPUT"(T)ape/(D)isk/(C)ore image",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASE c$ OF
        WHEN "T":
          OSCLI"DIR "+@dir$:OSCLI". *.BIN"
          INPUT"TAPE FILE NAME",F$
          IF file%<>0 THEN CLOSE#file%
          file%=OPENIN(@dir$+"/"+F$):hstbuffer%=BGET#file%:hstflag%=TRUE
          IF file%<>0 THEN PRINT"LOADED "+F$+" TAPE" ELSE PRINT "COULD NOT LOAD "+F$
        WHEN"D":
          PROCdiskmenu
        WHEN"C":
          PROCcore_image
      ENDCASE
      ENDPROC

      DEFPROCdiskmenu
      LOCALF$,c$
      INPUT"(L)oad, (U)nload, Write (P)rotect, Write (E)nable",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASE c$ OF
        WHEN "L":
          OSCLI"DIR "+@dir$:OSCLI". *.RK05"
          INPUT"RK05 FILE NAME",F$
          INPUT"LOAD TO DISK UNIT (0-3)",d%
          CASE d% OF
            WHEN 0:
              IF rk_file0%<>0 THEN CLOSE#rk_file0%
              rk_file0%=OPENUP(@dir$+"/"+F$)
              IF rk_file0%<>0 THEN PRINT"LOADED "+F$+" DISK IMAGE TO UNIT ";d% ELSE PRINT "COULD NOT LOAD "+F$
            WHEN 1:
              IF rk_file1%<>0 THEN CLOSE#rk_file1%
              rk_file1%=OPENUP(@dir$+"/"+F$)
              IF rk_file1%<>0 THEN PRINT"LOADED "+F$+" DISK IMAGE TO UNIT ";d% ELSE PRINT "COULD NOT LOAD "+F$
            WHEN 2:
              IF rk_file2%<>0 THEN CLOSE#rk_file2%
              rk_file2%=OPENUP(@dir$+"/"+F$)
              IF rk_file2%<>0 THEN PRINT"LOADED "+F$+" DISK IMAGE TO UNIT ";d% ELSE PRINT "COULD NOT LOAD "+F$
            WHEN 3:
              IF rk_file3%<>0 THEN CLOSE#rk_file3%
              rk_file3%=OPENUP(@dir$+"/"+F$)
              IF rk_file3%<>0 THEN PRINT"LOADED "+F$+" DISK IMAGE TO UNIT ";d% ELSE PRINT "COULD NOT LOAD "+F$
          ENDCASE
        WHEN "U":
          INPUT"UNLOAD FROM DISK UNIT (0-3)",d%
          CASE d% OF
            WHEN 0:
              IF rk_file0%<>0 THEN CLOSE#rk_file0%:PRINT"UNLOADED RK05 UNIT ";d% ELSE PRINT "COULD NOT UNLOAD UNIT ";d%
            WHEN 1:
              IF rk_file1%<>0 THEN CLOSE#rk_file1%:PRINT"UNLOADED RK05 UNIT ";d% ELSE PRINT "COULD NOT UNLOAD UNIT ";d%
            WHEN 2:
              IF rk_file2%<>0 THEN CLOSE#rk_file2%:PRINT"UNLOADED RK05 UNIT ";d% ELSE PRINT "COULD NOT UNLOAD UNIT ";d%
            WHEN 3:
              IF rk_file3%<>0 THEN CLOSE#rk_file3%:PRINT"UNLOADED RK05 UNIT ";d% ELSE PRINT "COULD NOT UNLOAD UNIT ";d%
          ENDCASE
        WHEN "P":
          INPUT"WRITE PROTECT DISK UNIT (0-3)",d%
          CASE d% OF
            WHEN 0:
              rkro0%=TRUE
              PRINT"WRITE PROTECTED RK05 UNIT 0"
            WHEN 1:
              rkro1%=TRUE
              PRINT"WRITE PROTECTED RK05 UNIT 1"
            WHEN 2:
              rkro2%=TRUE
              PRINT"WRITE PROTECTED RK05 UNIT 2"
            WHEN 3:
              rkro3%=TRUE
              PRINT"WRITE PROTECTED RK05 UNIT 3"
          ENDCASE
        WHEN "E":
          INPUT"WRITE ENABLE DISK UNIT (0-3)",d%
          CASE d% OF
            WHEN 0:
              rkro0%=FALSE
              PRINT"WRITE ENABLED RK05 UNIT 0"
            WHEN 1:
              rkro1%=FALSE
              PRINT"WRITE ENABLED RK05 UNIT 1"
            WHEN 2:
              rkro2%=FALSE
              PRINT"WRITE ENABLED RK05 UNIT 2"
            WHEN 3:
              rkro3%=FALSE
              PRINT"WRITE ENABLED RK05 UNIT 3"
          ENDCASE
      ENDCASE
      ENDPROC

      DEFPROCcore_image
      LOCAL F$,count%,n%,l%,file%,word%
      INPUT"(L)oad/(S)ave",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASE c$ OF
        WHEN "S":
          OSCLI"DIR "+@dir$:OSCLI". *.CORE"
          INPUT"IMAGE FILE NAME TO SAVE",F$
          INPUT"NUMBER OF WORDS",count%
          file%=OPENOUT(@dir$+"/"+F$)
          IF file%<>0 THEN
            FORn%=FALSE TOcount%-1
              word%=FNexamine(n%)
              BPUT#file%,((word%AND&FC0)>>6)+33:BPUT#file%,(word%AND63)+33
              IF (n%MOD&40)=FALSE THEN BPUT#file%,10:REM Split into 64-character (32-word) lines
            NEXT:CLOSE#file%
            PRINT"SAVED "+F$+" IMAGE"
          ELSE
            PRINT"COULD NOT SAVE "+F$
          ENDIF
        WHEN "L":
          OSCLI"DIR "+@dir$:OSCLI". *.CORE"
          INPUT"IMAGE FILE NAME",F$
          file%=OPENIN(@dir$+"/"+F$)
          IF file%<>0 THEN
            address%=I%
            REPEAT
              byte1%=BGET#file%:IFbyte1%=10THENbyte1%=BGET#file%
              byte2%=BGET#file%
              PROCdeposit(address%,((byte1%-33)<<6) + (byte2%-33))
              address%+=1
            UNTIL EOF#file%
            PRINT"LOADED "+F$+" IMAGE"
          ELSE
            PRINT"COULD NOT LOAD "+F$
          ENDIF
      ENDCASE
      ENDPROC

      DEFPROCmanual_deposit
      LOCALc$,c%,p%
      INPUT "(P)C, (A)C, (M)EMORY, (S)WITCH REG, (D)F, (I)F, I(B), M(Q)",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASE c$ OF
        WHEN "P":
          INPUT"PC";p%:P%=FNo2d(p%)AND&FFF
        WHEN "A":
          INPUT"AC";p%:A%=FNo2d(p%)AND&FFF
        WHEN "Q":
          INPUT"MQ";p%:Q%=FNo2d(p%)AND&FFF
        WHEN "M":
          INPUT"LOCATION";p%:p%=FNo2d(p%)AND&7FFF
          REPEAT
            PRINT "CURRENT CONTENTS "FNo0(p%,5)" IS ";FNo0(M!(p%<<2),4):INPUT "ENTER NEW OCTAL VALUE (0-7777):"c%
          UNTIL VAL(LEFT$(STR$c%,1))>=FALSE AND VAL(LEFT$(STR$c%,1))<=7 AND VAL(MID$(STR$c%,2,1))>=FALSE AND VAL(MID$(STR$c%,2,1))<=7 AND VAL(MID$(STR$c%,3,1))>=FALSE AND VAL(MID$(STR$c%,3,1))<=7 AND VAL(RIGHT$(STR$c%,1))>=FALSE AND VAL(RIGHT$(STR$c%,1))<=7
          M!(p%<<2)=c%
        WHEN "S":
          REPEAT
            PRINT "CURRENT SR IS ";FNo0(sr%,4):INPUT "ENTER NEW OCTAL VALUE (0-7777):"c%
          UNTIL VAL(LEFT$(STR$c%,1))>=FALSE AND VAL(LEFT$(STR$c%,1))<=7 AND VAL(MID$(STR$c%,2,1))>=FALSE AND VAL(MID$(STR$c%,2,1))<=7 AND VAL(MID$(STR$c%,3,1))>=FALSE AND VAL(MID$(STR$c%,3,1))<=7 AND VAL(RIGHT$(STR$c%,1))>=FALSE AND VAL(RIGHT$(STR$c%,1))<=7
          sr%=FNo2d(c%)
        WHEN "D":
          REPEAT
            PRINT "CURRENT DF IS ";D%>>12:INPUT "ENTER NEW DF (0-7):"c%
          UNTIL c%>=FALSE AND c%<=7
          D%=c%<<12
        WHEN "I":
          REPEAT
            PRINT "CURRENT IF IS ";I%>>12:INPUT "ENTER NEW IF (0-7):"c%
          UNTIL c%>=FALSE AND c%<=7
          I%=c%<<12
        WHEN "B":
          REPEAT
            PRINT "CURRENT IB IS ";insbuffer%>>12:INPUT "ENTER NEW IB (0-7):"c%
          UNTIL c%>=FALSE AND c%<=7
          insbuffer%=c%<<12

      ENDCASE
      ENDPROC

      DEFPROCdebug
      LOCALc$
      INPUT"Trace to (F)ile/(T)race to screen/(S)ingle-step",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASE c$ OF
        WHEN "F":
          TF%=NOTTF%
          PRINT "TRACE TO FILE TURNED ";:IF TF% THEN PRINT "ON" ELSE PRINT "OFF"
        WHEN "T":
          TS%=NOTTS%
          PRINT "TRACE TO SCREEN TURNED ";:IF TS% THEN PRINT "ON" ELSE PRINT "OFF"
        WHEN "S":
          S%=NOTS%
          IF S% THEN TS%=TRUE:REM Enable on-screen debug when single-step turned on
          PRINT "SINGLE-STEP TURNED ";:IF S% THEN PRINT "ON" ELSE PRINT "OFF"
      ENDCASE
      ENDPROC

      DEFPROCbell(pitch%)
      LOCAL N%
      N%=-15:FORN%=-15TO0
        SOUND1,N%,pitch%,2:SOUND2,N%,pitch%/1.125,2
      NEXT
      ENDPROC

      DEFPROCinit
      REM PC for FOCAL69 or memory test (0200), should be &FEE for RIM load. SR is 3777 for BIN load from HST
      REM A%=accumulator, Q%=MQ register, C%=fetched memory contents, P%=program counter, M%=address of memory block, I%=instruction field, D%=data field, L%=link register
      REM K%=keyboard flag, T%=teleprinter flag
      REM S%=single-step enabled, TF%=Status display enabled, TS%=Trace to screen enabled
      REM int%=Interrupts on/off, int_inhib%=interrupt inhibit (e.g. ION instruction), icontrol%=memory extension interrupt inhibit
      P%=&80:A%=FALSE:L%=FALSE:Q%=FALSE:sr%=&7FF:int%=FALSE:int_inhib%=FALSE
      REM Memory control
      I%=FALSE:D%=FALSE:insbuffer%=FALSE:intbuffer%=FALSE:icontrol%=TRUE
      REM TTY/TAPE flags/buffers
      kint%=TRUE:kbdbuf$="":ttybuf$="":K%=FALSE:T%=TRUE:hstflag%=FALSE:hstbuffer%=FALSE
      REM RK8E
      rk_ca%=FALSE:rk_com%=FALSE:rk_da%=FALSE:rk_st%=FALSE:REM Curr addr, command, disk addr, status registers
      rkro0%=FALSE:rkro1%=FALSE:rkro2%=FALSE:rkro3%=FALSE:REM Read-only status for each drive
      REM KE8-E Extended Arithmetic Element
      eae_sc%=FALSE:eae_instr%=FALSE:eae_mode%=FALSE:eae_gtf%=FALSE:REM Step counter, instruction register, mode (A=FALSE, B=TRUE), greater than flag

      REM Debugging options
      S%=FALSE:TF%=FALSE:TS%=FALSE:REM S%=single-step, TF%=trace to file, TS%=trace to screen

      REM Set up the RIM and BIN loaders in memory
      RESTORE
      FOR c%=&F97 TO &FFF:READ d%:PROCdeposit(c%,d%):NEXT
      PROCdeposit(&FFF,2753):REM JMP 7701 at location 7777
      REM BIN Loader (first byte at 7627, enter at 7701, 7777 if not overwritten by RIM loader)
      DATA 1674,2224,704,4072,2719,1162,3616,2711,650,4000,2712,652,188,737,3912,1174,4072,2966,652,174,687,1675,2712,56,3201,0,0,3097,2738,3102,1676,652,2992,3081,2744,3086
      DATA 2741,192,2275,3617,653,3842,3098,3084,3212,687,1675,3972,4032,747,746,1713,2198,2763,1677,651,1758,652,1790,2224,1773,2198,2749,2275,3856,2782,1678,766,749,653,2765,0,1934,1166,&F80,2778,0,766,3654,3590,3590,749,3043,2738,6,0,0
      REM RIM Loader (begins at 7756)
      DATA &C0C,&C09,&AEF,&C0E,&E46,&E06,&F48,&AEF,&E06,&C09,&AF7,&C0E,&F10,&7FE,&6FE,&AEF,0,0

      REM The RK8E boot loader:
      FOR c%=19 TO 26:READ d%:PROCdeposit(c%,d%):NEXT
      DATA3079,3556,538,3558,3555,538,2585,0

      ENDPROC

      DEFPROCtrace_file(t$):PRINT#trace%,t$:BPUT#trace%,10:ENDPROC

      DEFPROCquit
      LOCALc$,c%
      INPUT"(R)estart or (Q)uit",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASE c$ OF
        WHEN "R":
          FORc%=FALSE TO131072:M%!c%=FALSE:NEXT
          PROCinit
        WHEN "Q":
          QUIT
      ENDCASE
      ENDPROC

      DEFPROCeae
      LOCALtemp%,temp2%
      REM Mode swap instructions, come first otherwise SCA/MQL bits conflict
      IF (C%AND&3E)=&18THENeae_mode%=TRUE:ENDPROC:REM SWAB; MQL already executed by main Group 3 routine
      IF (C%AND&3E)=&26THENeae_mode%=FALSE:eae_gtf%=FALSE:ENDPROC:REM SWBA
      REM EAE instructions
      IF eae_mode%=FALSE THEN
        REM Mode A
        IF (C%AND&20)=&20THENA%=A%OReae_sc%:REM bit 6 set, SCA
        CASE (C%AND&E) OF
          WHEN 2: P%=(P%-TRUE)AND&FFF:eae_sc%=NOT(FNexamine(I%+P%))AND&1F:REM SCL
          WHEN 4: P%=(P%-TRUE)AND&FFF:temp%=(Q%*FNexamine(I%+P%))+A%:A%=(temp%AND&FFF000)>>12:Q%=temp%AND&FFF:L%=FALSE:REM MUY
          WHEN 6: P%=(P%-TRUE)AND&FFF:temp%=(A%<<12)+Q%:temp2%=FNexamine(I%+P%):Q%=temp%DIVtemp2%:L%=-(Q%=FALSE):A%=temp%MODtemp2%:REM DVI
          WHEN 8: REM NMI
            eae_sc%=FALSE:temp%=(A%<<12)+Q%:IF(temp%AND&3FFFFF)=0THENENDPROC:REM Already normalised
            REPEAT:temp%=(temp%<<1)AND&1FFFFFF:eae_sc%=(eae_sc%+1)AND&1F:PROCpause
            UNTIL((temp%AND&200000)<<1)<>temp%AND&400000
            L%=(temp%AND&1000000)>>24:A%=(temp%AND&FFF000)>>12:Q%=temp%AND&FFF
          WHEN 10:
            P%=(P%-TRUE)AND&FFF:temp%=(A%<<12)+Q%:eae_sc%=(NOT FNexamine(I%+P%))AND&1F:REM SHL
            REPEAT:temp%=(temp%<<1)AND&1FFFFFF:eae_sc%=(eae_sc%+1)AND&1F:UNTILeae_sc%=0
            L%=(temp%AND&1000000)>>24:A%=(temp%AND&FFF000)>>12:Q%=temp%AND&FFF
          WHEN 12: P%=(P%-TRUE)AND&FFF:temp%=(A%<<12)+Q%:eae_sc%=(NOT FNexamine(I%+P%))AND&1F:REM ASR
            REPEAT:temp2%=temp%AND&800000:temp%=(temp%>>1)AND&FFFFFF:temp%=temp%+temp2%:L%=temp2%>>23:eae_sc%=(eae_sc%+1)AND&1F:UNTILeae_sc%=0
            A%=((temp%AND&FFF000)>>12):Q%=temp%AND&FFF
          WHEN 14: P%=(P%-TRUE)AND&FFF:temp%=(A%<<12)+Q%:eae_sc%=(NOT FNexamine(I%+P%))AND&1F:REM LSR
            REPEAT:temp%=(temp%>>1)AND&FFFFFF:eae_sc%=(eae_sc%+1)AND&1F:UNTILeae_sc%=0
            L%=0:A%=(temp%AND&FFF000)>>12:Q%=temp%AND&FFF
        ENDCASE
        eae_gtf%=FALSE:REM GTF is cleared by all Mode A instructions
      ELSE
        REM Mode B
        IF (C%AND&20)=&20THEN
          CASE C%AND&E OF
            WHEN 0: A%=A%OReae_sc%:REM SCA
            WHEN 2: REM DAD
            WHEN 4: REM DST
            WHEN 6: P%=(P%-TRUE)AND&FFF:REM SWBA
            WHEN 8: IFA%+Q%=FALSE THENP%=(P%-TRUE)AND&FFF:REM DPSZ
            WHEN 10: REM DPIC
            WHEN 12: REM DCM
            WHEN 14: IFA%<=Q%THENL%=1ELSEL%=0:REM SAM
              A%=Q%-A%
              REM GTF FLAG? MANUAL PAGE 5-9
          ENDCASE
        ELSE
          CASE C%AND&E OF
            WHEN 2: eae_sc%=A%AND&1F:A%=FALSE:REM ACS
            WHEN 4: P%=(P%-TRUE)AND&FFF:temp%=(Q%*FNexamine(D%+FNexamine(I%+P%)))+A%:A%=(temp%AND&FFF000)>>12:Q%=temp%AND&FFF:REM MUY
            WHEN 6: P%=(P%-TRUE)AND&FFF:temp%=(A%<<12)+Q%:temp2%=FNexamine(D%+FNexamine(I%+P%)):Q%=temp%DIVtemp2%:L%=-(Q%=FALSE):A%=temp%MODtemp2%:REM DVI
            WHEN 8: REM NMI
              eae_sc%=FALSE:temp%=(A%<<12)+Q%:IFtemp%AND&3FFFFF=0THENENDPROC:REM Already normalised
              REPEAT:temp%=(temp%<<1)AND&1FFFFFF:eae_sc%=(eae_sc%+1)AND&1F:UNTIL((temp%AND&200000)<<1)<>temp%AND&400000
              L%=(temp%AND&1000000)>>24:A%=(temp%AND&FFF000)>>12:Q%=temp%AND&FFF
            WHEN 10: P%=(P%-TRUE)AND&FFF:temp%=(A%<<12)+Q%:eae_sc%=FNexamine(I%+P%)AND&1F):REM SHL
              WHILEeae_sc%>0:temp%=(temp%<<1)AND&1FFFFFF:eae_sc%-=1
                L%=(temp%AND&1000000)>>24:A%=(temp%AND&FFF000)>>12:Q%=temp%AND&FFF:eae_gtf%=FALSE:REM GTF is a test ******
              WHEN 12:  P%=(P%-TRUE)AND&FFF:temp%=(A%<<12)+Q%:eae_sc%=FNexamine(I%+P%)AND&1F):REM ASR - not finished (GTF flag?)
                WHILEeae_sc%>0:temp2%=temp%AND&800000:temp%=(temp%>>1)AND&FFFFFF:eae_sc%-=1:ENDWHILE
                L%=temp2%>>23:A%=((temp%AND&FFF000)>>12)+temp2%:Q%=temp%AND&FFF
              WHEN 14: P%=(P%-TRUE)AND&FFF:temp%=(A%<<12)+Q%:eae_sc%=FNexamine(I%+P%)AND&1F):REM LSR - not finished (GTF flag?)
                WHILEeae_sc%>0:temp%=(temp%>>1)AND&FFFFFF:eae_sc%-=1:ENDWHILE
                L%=0:A%=(temp%AND&FFF000)>>12:Q%=temp%AND&FFF
            ENDCASE
          ENDIF
        ENDIF
        ENDPROC

