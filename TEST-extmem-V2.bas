      INSTALL @dir$+"/Number.bbc"
      INSTALL @lib$+"/multiwin.bbc"
      PROC_multiwin(1):REM Multiple window support, 1 window
      PROC_selectwin(0)

      REM ON ERROR PROC_closewin(1):END

      MODE 3
      singlestep%=FALSE
      :
      REM Init machine
      DIM mem% 131071
      ac%=0:link%=0:sr%=2047:int%=FALSE:ion%=FALSE:idefer%=TRUE:REM PC for FOCAL69 test (0200), &FEE for RIM load
      REM TTY/TAPE flags/buffers
      kbdbuf$="":ttybuf$="":kbdflag%=FALSE:ttyflag%=TRUE

      REM Memory control
      ifield%=0:dfield%=0:insbuffer%=0:intbuffer%=0:icontrol%=TRUE
      :
      REM Initial program e.g. RIM loader
      REM FOR c%=&FEE TO &FFF:READ d%:PROCdeposit(c%,d%):NEXT:REM ***** BODGE TEST TO MEMORY FIELD 1 GOES HERE*****
      REM (test &80 to &86 : DATA &E80,&285,&E04,&A81,0,1,4095)
      REM HST RIM loader (7756 to 7777, &fee to &fff):
      REM DATA &C0C,&C09,&AEF,&C0E,&E46,&E06,&F48,&AEF,&E06,&C09,&AF7,&C0E,&F10,&7FE,&6FE,&AEF,0,0
      REM ** read in a RIM file **
      REM file%=OPENIN(@dir$+"/focal.rim")
      REM file%=OPENIN(@dir$+"/dec-08-lbaa-pm_5-10-67.bin")
      hstflag%=FALSE:hstbuffer%=0
      :
      INPUT"RIM/BIN load or core image load (R/C)",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASE c$ OF
        WHEN "C":
          OSCLI"DIR "+@dir$:OSCLI". *.CORE"
          INPUT"IMAGE FILE NAME",F$
          file%=OPENIN(@dir$+"/"+F$)
          address%=0
          REPEAT
            byte1%=BGET#file%:byte2%=BGET#file%
            PROCdeposit(address%,((byte1%-33)<<6) + (byte2%-33)):REM PRINT FNo0(address%,4);" ";FNo0(word%,4);" ";
            address%+=1
          UNTIL EOF#file%:REM CLOSE#file%:PRINT
          PRINT"LOADED "+F$+" IMAGE"
          PROCcommand:REM need to get start PC from user
        WHEN "R":
          file%=OPENIN(@dir$+"/dec-08-lbaa-pm_5-10-67.bin")
          FOR c%=&FEE TO &FFF:READ d%:PROCdeposit(c%,d%):NEXT
          DATA &C0C,&C09,&AEF,&C0E,&E46,&E06,&F48,&AEF,&E06,&C09,&AF7,&C0E,&F10,&7FE,&6FE,&AEF,0,0
          pc%=&FEE:REM Start the RIM loader, load the BIN loader
      ENDCASE

      REM ** Main loop **
      REPEAT
        startpc%=pc%:REM for status
        IF (NOT idefer%) AND ion% THEN int%=TRUE:ion%=FALSE
        IF idefer% THEN idefer%+=1
        IF(kbdflag% OR ttyflag% )AND (int% AND icontrol%) THEN int%=FALSE:PROCdeposit(FALSE,pc%):intbuffer%=(ifield%>>9)+(dfield%>>12):pc%=1
        PROCexecute
        PROCkbd
        IFINKEY(-114)THENPROCcommand
        IF singlestep%=TRUE THEN xt%=POS:yt%=VPOS:PROC_selectwin(1):PRINTFNstatus(startpc%):PROC_selectwin(0):PRINTTAB(xt%,yt%);:
        REM PROCpause
      UNTIL FALSE
      :
      DEFFNaddr(eff_mem%)
      LOCAL temp%
      IF (eff_mem%AND&100) = 0 THEN
        =ifield%+(((eff_mem% AND &80) >>7)*(pc% AND &F80) + (eff_mem% AND &7F)):REM direct
      ELSE
        temp%=dfield%+(((eff_mem% AND &80) >>7)*(pc% AND &F80) + (eff_mem% AND &7F)):REM IF singlestep% THEN PRINT" indirect ";
        IF temp%>7 AND temp%<16 THEN PROCdeposit(temp%,(FNexamine(temp%)+1)AND&FFF):REM PRINT "Indirect ref through ";FNo0(temp%,4);", incrementing to ";FNo0(mem%(temp%),4)
        =dfield%+FNexamine(temp%)
      ENDIF
      :
      DEFPROCdeposit(address%,word%)
      mem%!(address%<<2)=word%
      REM IF singlestep% THEN PROC_selectwin(1):PRINT "Depositing ";FNo0(word%,4);" into addr ";FNo0(address%,5): PROC_selectwin(0)
      ENDPROC
      :
      DEFFNexamine(address%)
      REM IF singlestep% THEN PROC_selectwin(1):PRINT "Examining address ";FNo0(address%,5);", result ";FNo0(mem%!(address%<<1),4): PROC_selectwin(0)
      =mem%!(address%<<2)

      :
      DEFPROCexecute
      REM LOCAL contents%
      LOCAL addr%,temp%
      contents%=FNexamine(ifield%+pc%)
      CASE (contents% AND &E00) OF
        WHEN 0:     REM AND - and operand with AC
          ac%=ac% AND FNexamine(FNaddr(contents%))
          pc%=(pc%+1)AND&FFF
        WHEN &200:  REM TAD - add operand to (a 13 bit value)
          ac%=ac%+FNexamine(FNaddr(contents%))
          IF ac%>4095 THEN
            ac%=ac%-4096
            link%=(NOT link%)AND1
          ENDIF
          pc%=(pc%+1)AND&FFF
        WHEN &400:  REM ISZ - increment operand and skip if result is zero
          addr%=FNaddr(contents%)
          PROCdeposit(addr%,(FNexamine(addr%)+1)AND&FFF)
          IFFNexamine(addr%)=FALSE THENpc%=(pc%+1)AND&FFF
          REM temp%=(FNexamine(FNaddr(contents%))+1)AND&FFF:PROCdeposit(FNaddr(contents%),temp%)
          REM IF NOT temp% THENpc%=(pc%+1)AND&FFF
          pc%=(pc%+1)AND&FFF
        WHEN &600:  REM DCA - deposit AC in memory and clear AC
          PROCdeposit(FNaddr(contents%),ac%)
          ac%=0
          pc%=(pc%+1)AND&FFF
        WHEN &800:  REM JMS - jump to subroutine
          icontrol%=TRUE:REM re-enable interrupts via separate memory management control
          ifield%=insbuffer%:REM memory control: transfer instruction field buffer to instruction field register
          PROCdeposit(FNaddr(contents%) ,pc%+1)
          pc%=(FNaddr(contents%))+1
        WHEN &A00:  REM JMP - jump
          icontrol%=TRUE:REM re-enable interrupts via separate memory management control
          ifield%=insbuffer%:REM memory control: transfer instruction field buffer to instruction field register
          pc%=FNaddr(contents%)
        WHEN &C00:  REM IOT - input/output transfer
          CASE (contents% AND &1F8) OF
            WHEN 0:  REM Program Interrupt and flag (internal IOT)
              CASE (contents% AND 7) OF
                WHEN 0:  REM SKON
                  IF int%=TRUE THEN pc%=(pc%+1)AND&FFF
                WHEN 1: REM ION
                  ion%=TRUE:idefer%=TRUE
                WHEN 2: REM IOF
                  int%=FALSE
                WHEN 4: REM GTF
                WHEN 5: REM RTF
                WHEN 6: REM SGT
                WHEN 7: REM CAF
              ENDCASE
            WHEN 8: REM HS tape input
              PROCtape
              CASE (contents% AND 7) OF
                WHEN 1: REM RSF; Reader skip if flag
                  IF hstflag%=TRUE THEN pc%+=1
                WHEN 2: REM RRB; Read Reader Buffer
                  ac%=ac% OR hstbuffer%
                WHEN 4: REM RFC; Reader Flag Clear
                  hstflag%=FALSE
                WHEN 6: REM RRB RFC; read buffer and continue
                  ac%=ac% OR hstbuffer%
                  hstflag%=FALSE
              ENDCASE
            WHEN 9: REM HS punch output
              CASE (contents% AND 7) OF
                WHEN 1: REM PSF; Punch skip if flag
                WHEN 2: REM PCF; Punch Clear Flag
                WHEN 4: REM PPC; Punch Put Character
                WHEN 6: REM PLS; Punch Load Sequence
              ENDCASE
            WHEN 24: REM Teletype keyboard/reader
              CASE (contents% AND 7) OF
                WHEN 1: REM KSF:
                  IFkbdflag%THENpc%=(pc%+1)AND&FFF
                WHEN 2: REM KCC
                  kbdflag%=FALSE:ac%=0:REM pc%=(pc%+1)AND&FFF:REM *** start read from tape goes here
                WHEN 4: REM KRS
                  ac%=ac% OR ASC(LEFT$(kbdbuf$,1)):kbdbuf$="":REM RIGHT$(kbdbuf$,LENkbdbuf$-1):REM ** Pull from kbd buffer and OR with ac
                WHEN 6: REM KRB
                  ac%=ASC(LEFT$(kbdbuf$,1)):kbdflag%=FALSE:kbdbuf$="":REM RIGHT$(kbdbuf$,LENkbdbuf$-1):REM ** Pull from kbd buffer and put in ac, clear flag
              ENDCASE
            WHEN 32:REM Teletype teleprinter/punch
              CASE (contents% AND 7) OF
                WHEN 1:  REM TSF:
                  IFttyflag%THENpc%=(pc%+1)AND&FFF
                WHEN 2: REM TCF
                  ttyflag%=FALSE
                WHEN 4: REM TPC
                  ttybuf$=ttybuf$+CHR$(ac% AND &7F)
                  REM PRINT "[TPC ";ac%;"]";:REM PROCpause:REM **************
                WHEN 6: REM TLS
                  ttyflag%=FALSE:ttybuf$=ttybuf$+CHR$(ac% AND &7F)
                  REM PRINT "[TLS ";ac%;"]";:REM PROCpause:REM **************
              ENDCASE
              PROCtprinter
            WHEN 128,136,144,152,160,168,176,184:REM 62XX - Memory management
              CASE (contents% AND 7) OF
                WHEN 1: REM 62X1 CDF; Change Data Field
                  REM dfield%=(contents% AND &38)>>3
                  dfield%=(contents% AND &38)<<9
                  REM PRINT "[CDF "; (contents% AND &38)>>3;"]";
                WHEN 2: REM 62X2 CIF; Change Instruction Field
                  REM insbuffer%=(contents% AND &38)>>3:REM Buffered until next JMP or JMS instruction
                  insbuffer%=(contents% AND &38)<<9:REM Buffered until next JMP or JMS instruction
                  icontrol%=FALSE:REM Disable interrupts with separate flip-flop, until next branch
                  REM PRINT "[CIF "; (contents% AND &38)>>3;"]" ;
                WHEN 3: REM 62X3 CDI; Change Data and Instruction Fields
                  REM dfield%=(mem%(pc%) AND &38)>>3
                  dfield%=(mem%(pc%) AND &38)>>9
                  insbuffer%=dfield%:REM Buffered until next JMP or JMS instruction
                  icontrol%=FALSE:REM Disable interrupts with separate flip-flop, until next branch
                  REM PRINT "[CDI "; (contents% AND &38)>>3;"]";
                WHEN 4: REM Other instructions
                  CASE (contents% AND &38) OF
                    WHEN 8: REM 6214 RDF;  Read Data Field
                      ac%=ac% OR (dfield%>>9):REM was <<3
                    WHEN 16: REM 6224 RIF; Read Instruction Field
                      ac%=ac% OR (ifield%>>9):REM was <<3
                    WHEN 24: REM 6234 RIB; Read Interrupt Buffer
                      ac%=ac% OR (intbuffer% AND &3F)
                    WHEN 32: REM 6244 RMF; Restore Memory Field
                      dfield%=(intbuffer% AND 7)<<12
                      insbuffer%=(intbuffer% AND &38)<<9
                  ENDCASE
              ENDCASE

          ENDCASE
        ENDIF
        pc%=(pc%+1)AND&FFF
      WHEN &E00:  REM OPR - microcoded operations
        CASE (contents% AND &F00) OF
          WHEN &E00:REM Group 1 (%1110xxxxxxxx)
            IF (contents%AND&80)=&80 THEN ac%=0:REM CLA
            IF (contents%AND&40)=&40 THEN link%=FALSE  :REM CLL
            IF (contents%AND&20)=&20 THEN ac%=(NOT ac%)AND&FFF:REM CMA
            IF (contents%AND&10)=&10 THEN link%=(NOT link%)AND1  :REM CML
            IF (contents%AND1)  =  1 THEN ac%=ac%+1:IF ac%=4096 THEN ac%=0:link%=1: REM IAC - increment <L,AC>
            IF (contents%AND4)  =  4 THEN ac%=ac%<<1:ac%=ac%+link%:link%=(ac% AND &1000)>>12:ac%=ac% AND &FFF: REM RAL rotate <L,AC> left
            IF (contents%AND8)  =  8 THEN ac%=ac%+link%*4096:link%=ac% AND 1:ac%=ac%>>1: REM RAR rotate <L,AC> right
            IF (contents%AND6)  =  6 THEN ac%=ac%<<1:ac%=ac%+link%:link%=(ac% AND &1000)>>12:ac%=ac% AND &FFF: REM RTL rotate <L,AC> left twice
            IF (contents%AND&A) = &A THEN ac%=ac%+link%*4096:link%=ac% AND 1:ac%=ac%>>1: REM RTR rotate <L,AC> right twice
            IF (contents%AND2)  =  2 THEN VDU7: REM BSW (8e and up)
          WHEN &F00:REM Group 2 (%1111xxxxxxxx), (OR|AND) group
            cond%=FALSE
            IF (contents%AND&40)=&40THENIF ac%>2047 cond%=TRUE: REM SMA - Skip on AC < 0 (or group)  | SPA – Skip on AC ≥ 0 (and group)
            IF (contents%AND&20)=&20THENIF ac%=0 cond%=TRUE: REM SZA - Skip on AC = 0 (or group)  | SNA – Skip on AC ≠ 0 (and group)
            IF (contents%AND&10)=&10THENIF link%=1 cond%=TRUE: REM SNL - Skip on L != 0 (or group)  |  SZL – Skip on L = 0 (and group)
            IF (contents%AND&80)=&80THEN ac%=0: REM CLA
            IF (contents%AND2)  =  2THEN PRINT"CPU Halt":PROCcommand
            IF (contents%AND4)  =  4THEN ac%=ac% OR sr%:REM OSR - logically 'or' front-panel switches with AC
            IF (contents%AND8)=0 THEN
              IF cond%=TRUE THEN pc%=(pc%+1)AND&FFF:REM Bit 8 not set (OR), skip if any conditions true
            ELSE
              IF cond%=FALSE THEN pc%=(pc%+1)AND&FFF:REM Bit 8 set (AND), skip if all conditions true
            ENDIF
        ENDCASE
        pc%=(pc%+1)AND&FFF
      ENDCASE
      ENDPROC
      :
      DEFPROCkbd
      LOCAL kbdtemp$
      REM Keyboard
      kbdtemp$=INKEY$(0)
      IFkbdtemp$<>""THEN
      IFASCkbdtemp$>96THENkbdtemp$=CHR$(ASC(kbdtemp$)AND223)
      kbdbuf$=kbdtemp$
      ENDIF
      kbdflag%=FALSE:IFLENkbdbuf$>0THENkbdflag%=TRUE
      :
      DEFPROCtprinter
      LOCAL ttemp$
      IFLENttybuf$>0THEN
      ttemp$=LEFT$(ttybuf$,1):IFASC(ttemp$)<127THENVDUASC(ttemp$)
      ttybuf$=RIGHT$(ttybuf$,LENttybuf$-1)
      IFLENttybuf$=0 THEN ttyflag%=TRUE
      ENDIF
      ENDPROC
      :
      DEFPROCtape
      REM HS Tape
      IF (NOT EOF#file%) THEN
      IF hstflag%= FALSE THEN hstbuffer%=BGET#file%:hstflag%=TRUE
      ELSE hstflag%=FALSE:REM PTR#file%=0
      ENDIF
      ENDPROC
      :
      DEFFNstatus(pc%)
      LOCAL singletemp%,contents%,contentsd%,dis$
      singletemp%=singlestep%:singlestep%=FALSE:REM suppress diagnostic messages during status output
      contents%=FNexamine(ifield%+pc%)
      CASE (contents% AND &E00) OF
      WHEN 0,&200,&400,&600,&800,&A00:
        CASE (contents% AND &E00) OF
          WHEN 0:
            dis$="AND "
          WHEN &200:
            dis$="TAD "
          WHEN &400:
            dis$="ISZ "
          WHEN &600:
            dis$="DCA "
          WHEN &800:
            dis$="JMS "
          WHEN &A00:
            dis$="JMP "
        ENDCASE
        IF (contents%AND &100)=&100 THEN
          contentsd%=FNexamine(dfield%+pc%)
          dis$=dis$+"I "+FNo0(((contents% AND &80) >>7)*(pc% AND &F80) + (contents% AND &7F),4)+" ("+FNo0(FNexamine(((contentsd% AND &80) >>7)*(pc% AND &F80) + (contentsd% AND &7F)),4) + ")"
        ELSE
          dis$=dis$+FNo0(((contents% AND &80) >>7)*(pc% AND &F80) + (contents% AND &7F),4)
        ENDIF
      WHEN &C00:
        CASE (contents%AND &1FF) OF
          WHEN 0: dis$="SKON":REM Program Interrupt and flag (internal IOT)
          WHEN 1: dis$="ION"
          WHEN 2: dis$="IOF"
          WHEN 3: dis$="SRQ"
          WHEN 4: dis$="GTF"
          WHEN 5: dis$="RTF"
          WHEN 6: dis$="SGT"
          WHEN 7: dis$="CAF"
          WHEN 8: dis$="RPE":REM HS tape input
          WHEN 9: dis$="RSF"
          WHEN 10: dis$="RRB"
          WHEN 12: dis$="RFC"
          WHEN 14: dis$="RRB RFC"
          WHEN 16:dis$="PCE":REM HS punch output
          WHEN 17:dis$="PSF"
          WHEN 18:dis$="PCF"
          WHEN 20:dis$="PPC"
          WHEN 22:dis$="PLS"
          WHEN 25: dis$="KSF":REM Teletype keyboard/reader
          WHEN 26: dis$="KCC"
          WHEN 28: dis$="KRS"
          WHEN 30: dis$="KRB"
          WHEN 33: dis$="TSF":REM Teletype teleprinter/punch
          WHEN 34: dis$="TCF"
          WHEN 36: dis$="TPC"
          WHEN 38: dis$="TLS ["+FNo0(ASC(LEFT$(ttybuf$,1)),3)+"]"
          WHEN 129,137,145,153,161,169,177,185: dis$="CDF "+((contents% AND &38) >>3) :REM Memory management
          WHEN 130,138,146,154,162,170,178,186: dis$="CIF "+((contents% AND &38) >>3)
          WHEN 140: dis$="RDF"
          WHEN 148: dis$="RIF"
          WHEN 156: dis$="RIB"
          WHEN 164: dis$="RMF"
          WHEN 497: dis$="DTRA":REM DECtape
          WHEN 498: dis$="DTCA"
          WHEN 500: dis$="DTXA"
          WHEN 505: dis$="DTSF"
          WHEN 508: dis$="DTRB"
          WHEN 508: dis$="DTLB"
          OTHERWISE:dis$="IOT "+FNo0(contents%AND&1FF,3)
        ENDCASE
      WHEN &E00:
        CASE (contents%AND &F00) OF
          WHEN &E00:REM Group 1 (%1110xxxxxxxx)
            IF (contents%AND&80)=&80 THEN dis$=dis$+"CLA "
            IF (contents%AND&40)=&40 THEN dis$=dis$+"CLL "
            IF (contents%AND&20)=&20 THEN dis$=dis$+"CMA "
            IF (contents%AND&10)=&10 THEN dis$=dis$+"CML "
            IF (contents%AND1)  =  1 THEN dis$=dis$+"IAC "
            IF (contents%AND4)  =  4 THEN dis$=dis$+"RAL "
            IF (contents%AND8)  =  8 THEN dis$=dis$+"RAR "
            IF (contents%AND6)  =  6 THEN dis$=LEFT$(dis$,LEN(dis$)-4):dis$=dis$+"RTL "
            IF (contents%AND&A) = &A THEN dis$=LEFT$(dis$,LEN(dis$)-4):dis$=dis$+"RTR "
            IF (contents%AND2)  =  2 THEN : REM BSW (8e and up)
          WHEN &F00:REM Group 2 (%1111xxxxxxxx), AND/OR group
            IF (contents%AND8)=8 THEN
              IF (contents%AND&40)=&40 THEN dis$=dis$+"SPA "
              IF (contents%AND&20)=&20 THEN dis$=dis$+"SNA "
              IF (contents%AND&10)=&10 THEN dis$=dis$+"SZL "
              IF (contents%AND&80)=&80 THEN dis$=dis$+"CLA "
              IF (contents%AND2)  =  2 THEN dis$=dis$+"HLT "
              IF (contents%AND4)  =  4 THEN dis$=dis$+"OSR "
            ELSE
              IF (contents%AND&40)=&40 THEN dis$=dis$+"SMA "
              IF (contents%AND&20)=&20 THEN dis$=dis$+"SZA "
              IF (contents%AND&10)=&10 THEN dis$=dis$+"SNL "
              IF (contents%AND&80)=&80 THEN dis$=dis$+"CLA "
            ENDIF
        ENDCASE
      ENDCASE
      singlestep%=singletemp%:REM re-enable diagnostic messages
      ="IF:"+STR$(ifield%>>12)+" DF:"+STR$(dfield%>>12)+" PC:"+FNo0(pc%,4)+" LINK:"+STR$link%+" AC:"+FNo0(ac%,4)+" INT:"+STR$int%+" INSTR:"+FNo0(contents%,4)+" ("+dis$+")"
      :
      DEFPROCpause
      REPEATUNTILGET=32
      ENDPROC
      :
      DEFPROCcommand
      LOCALc$,p%:PRINTFNstatus(pc%):REM singlestep%=TRUE:REM *********** TEST *****************
      PRINT "Stopped. ";
      REPEAT
      OSCLI"FX15,1":INPUT"COMMAND:"'"(E)xamine/(D)eposit/(C)ont/(P)C/(T)ape/(S)ingle-step toggle/Save core (I)mage",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASE c$ OF
        WHEN "S":
          singlestep%=NOT singlestep%
          IF singlestep%=TRUE THEN
            st_handle%=FN_createwin(1,"Diagnostic output",100,100,640,512,0,0,0)
            PROC_selectwin(1)
            OSCLI "FONT """ + @lib$ + "DejaVuSansMono"", 10"
            PROC_selectwin(0)
          ELSE
            PROC_closewin(1)
          ENDIF
        WHEN "E":
          INPUT"EXAMINE ADDRESS";p%:PRINTmem%?FNo2d(p%)
        WHEN "P":
          INPUT"PC";p%:pc%=FNo2d(p%)
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
      DEFFNo2d(o%):LOCALo$:o$=RIGHT$("0000"+STR$o%,4):=VAL(LEFT$(o$,1))*512+VAL(MID$(o$,2,1))*64+VAL(MID$(o$,3,1))*8+VAL(RIGHT$(o$,1))
      :
      DEFPROCchangetape
      LOCALF$
      CLOSE#file%
      OSCLI"DIR "+@dir$:OSCLI". *.BIN"
      INPUT"TAPE FILE NAME",F$
      file%=OPENIN(@dir$+"/"+F$):hstbuffer%=BGET#file%:hstflag%=TRUE:PRINT"LOADED "+F$+" TAPE"
      ENDPROC

      DEFPROCsavecore
      LOCAL F$,count%,n%,l%,file%
      OSCLI"DIR "+@dir$:OSCLI". *.CORE"
      INPUT"IMAGE FILE NAME TO SAVE",F$
      INPUT"NUMBER OF WORDS",count%
      file%=OPENOUT(@dir$+"/"+F$)
      FORn%=0TOcount%-1
      word%=((mem%?(n%+32768))<<4)+mem%?n%
      byte1%=((word%AND&FC0)>>6)+33:byte2%=(word%AND&3F)+33
      BPUT#file%,byte1%:BPUT#file%,byte2%
      IF count%DIV64=FALSE THEN BPUT#file%,10:REM Split into 64-character (32-word) lines
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
          PRINT "Current data field is ";dfield%>>12:INPUT "Enter New Field (0-7):"c%
        UNTIL c%>=0 AND c%<=7
        dfield%=c%<<12
      WHEN "I":
        REPEAT
          PRINT "Current instruction field is ";ifield%>>12:INPUT "Enter New Field (0-7):"c%
        UNTIL c%>=0 AND c%<=7
        ifield%=c%<<12

      ENDCASE
      ENDPROC

