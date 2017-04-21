      INSTALL @dir$+"/Number.bbc"
      MODE 3
      statustemp%=FALSE
      REM Init machine
      DIM mem%(32767)
      pc%=&FEE:ac%=0:link%=0:int%=FALSE:ion%=FALSE:idefer%=TRUE:REM PC for FOCAL69 test (0200), &FEE for RIM load
      REM TTY/TAPE flags/buffers
      kbdbuf$="":ttybuf$="":kbdflag%=FALSE:ttyflag%=TRUE

      REM Memory control
      ifield%=0:dfield%=0:insbuffer%=0:intbuffer%=0:icontrol%=TRUE
      TIME=0
      :
      REM Initial program e.g. RIM loader
      FOR c%=&FEE TO &FFF:READ d%:mem%(c%)=d%:NEXT
      REM (test &80 to &86 : DATA &E80,&285,&E04,&A81,0,1,4095)
      REM HST RIM loader (7756 to 7777, &fee to &fff):
      DATA &C0C,&C09,&AEF,&C0E,&E46,&E06,&F48,&AEF,&E06,&C09,&AF7,&C0E,&F10,&7FE,&6FE,&AEF,0,0
      REM ** read in a RIM file **
      REM file%=OPENIN(@dir$+"/focal.rim")
      file%=OPENIN(@dir$+"/dec-08-lbaa-pm_5-10-67.bin")
      hstflag%=TRUE:hstbuffer%=0

      REM REPEAT
      REM addr1%=BGET#file%:addr2%=BGET#file%:byte1%=BGET#file%:byte2%=BGET#file%
      REM address%=((addr1%AND&3F)<<6) + (addr2%AND&3F)
      REM word%=((byte1%AND&3F)<<6) + (byte2%AND&3F)
      REM mem%(address%)=word%:REM PRINT FNo0(address%,4);" ";FNo0(word%,4);" ";
      REM UNTIL EOF#file%:CLOSE#file%:PRINT
      :
      REM ** Main loop **
      REPEAT
        startpc%=pc%:REM for status
        IF (NOT idefer%) AND ion% THEN int%=TRUE:ion%=FALSE
        IF idefer% THEN idefer%+=1
        IF(kbdflag% OR ttyflag% )AND (int%) THEN int%=FALSE:mem%(FALSE)=pc%:intbuffer%=(ifield%<<3)+dfield%:pc%=1:REM AND (int% AND icontrol%)
        PROCexecute
        PROCio
        IF INKEY(-114) THEN PROCcommand
        IF statustemp%=TRUE THEN PROCstatus(startpc%):PROCpause
        REM x%=POS:y%=VPOS:PRINTTAB(0,0);:PROCstatus(startpc%):PRINTTAB(x%,y%);
        REM FORN%=0TO10000000:NEXT
      UNTIL FALSE
      :
      DEFFNaddr(eff_mem%)
      LOCAL temp%
      IF (eff_mem%AND&100) = 0 THEN
        =((eff_mem% AND &80) >>7)*(pc% AND &F80) + (eff_mem% AND &7F):REM direct
      ELSE
        temp%=((eff_mem% AND &80) >>7)*(pc% AND &F80) + (eff_mem% AND &7F):REM indirect
        IF temp%>7 AND temp%<16 THEN mem%(temp%)=(mem%(temp%)+1)AND&FFF:REM PRINT "Indirect ref through ";FNo0(temp%,4);", incrementing to ";FNo0(mem%(temp%),4)
        =mem%(temp%)
      ENDIF
      :
      DEFPROCexecute
      LOCALcontents%
      CASE (mem%(pc%)AND &E00) OF
        WHEN 0:     REM AND - and operand with AC
          ac%=ac% AND mem%(FNaddr(mem%(pc%)))
          pc%=(pc%+1)AND&FFF
        WHEN &200:  REM TAD - add operand to (a 13 bit value)
          ac%=ac%+mem%(FNaddr(mem%(pc%)))
          IF ac%>4095 THEN
            ac%=ac%-4096
            link%=(NOT link%)AND1
          ENDIF
          pc%=(pc%+1)AND&FFF
        WHEN &400:  REM ISZ - increment operand and skip if result is zero
          addr%=FNaddr(mem%(pc%))
          mem%(addr%)=(mem%(addr%)+1)AND&FFF
          IFmem%(addr%)=FALSE THENpc%=(pc%+1)AND&FFF
          pc%=(pc%+1)AND&FFF
        WHEN &600:  REM DCA - deposit AC in memory and clear AC
          mem%(FNaddr(mem%(pc%)))=ac%
          ac%=0
          pc%=(pc%+1)AND&FFF
        WHEN &800:  REM JMS - jump to subroutine
          icontrol%=TRUE:REM re-enable interrupts via separate memory management control
          ifield%=insbuffer%:REM memory control: transfer instruction field buffer to instruction field register
          mem%(FNaddr(mem%(pc%)))=pc%+1
          pc%=(FNaddr(mem%(pc%)))+1
        WHEN &A00:  REM JMP - jump
          icontrol%=TRUE:REM re-enable interrupts via separate memory management control
          ifield%=insbuffer%:REM memory control: transfer instruction field buffer to instruction field register
          pc%=FNaddr(mem%(pc%))
        WHEN &C00:  REM IOT - input/output transfer
          CASE (mem%(pc%) AND &1F8) OF
            WHEN 0:  REM Program Interrupt and flag (internal IOT)
              CASE (mem%(pc%) AND 7) OF
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
              CASE (mem%(pc%) AND 7) OF
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
              CASE (mem%(pc%) AND 7) OF
                WHEN 1: REM PSF; Punch skip if flag
                WHEN 2: REM PCF; Punch Clear Flag
                WHEN 4: REM PPC; Punch Put Character
                WHEN 6: REM PLS; Punch Load Sequence
              ENDCASE
            WHEN 24: REM Teletype keyboard/reader
              CASE (mem%(pc%) AND 7) OF
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
              CASE (mem%(pc%) AND 7) OF
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
            WHEN 128,136,144,152,160,168,176,184:REM 62XX - Memory management
              CASE (mem%(pc%) AND 7) OF
                WHEN 1: REM 62X1 CDF; Change Data Field
                  dfield%=(mem%(pc%) AND &38)>>3
                  REM PRINT "[CDF "; (contents% AND &38)>>3;"]";
                WHEN 2: REM 62X2 CIF; Change Instruction Field
                  insbuffer%=(mem%(pc%) AND &38)>>3:REM Buffered until next JMP or JMS instruction
                  icontrol%=FALSE:REM Disable interrupts with separate flip-flop, until next branch
                  REM PRINT "[CIF "; (contents% AND &38)>>3;"]" ;
                WHEN 3: REM 62X3 CDI; Change Data and Instruction Fields
                  dfield%=(mem%(pc%) AND &38)>>3
                  insbuffer%=dfield%:REM Buffered until next JMP or JMS instruction
                  icontrol%=FALSE:REM Disable interrupts with separate flip-flop, until next branch
                  REM PRINT "[CDI "; (contents% AND &38)>>3;"]";
                WHEN 4: REM Other instructions
                  CASE (mem%(pc%) AND &38) OF
                    WHEN 8: REM 6214 RDF;  Read Data Field
                      ac%=ac% OR (dfield%<<3)
                    WHEN 16: REM 6224 RIF; Read Instruction Field
                      ac%=ac% OR (ifield%<<3)
                    WHEN 24: REM 6234 RIB; Read Interrupt Buffer
                      ac%=ac% OR (intbuffer% AND &3F)
                    WHEN 32: REM 6244 RMF; Restore Memory Field
                      dfield%=intbuffer% AND 7
                      insbuffer%=(intbuffer% AND &38)>>3
                  ENDCASE
              ENDCASE

          ENDCASE
        ENDIF
        pc%=(pc%+1)AND&FFF
      WHEN &E00:  REM OPR - microcoded operations
        contents%=mem%(pc%)
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
            IF (contents%AND4)  =  4THEN ac%=ac% OR 2047:REM OSR - logically 'or' front-panel switches with AC (*** TEST WITH 3777 ***)
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
      DEFPROCio
      LOCAL kbdtemp$,ttemp$
      REM Keyboard
      kbdtemp$=INKEY$(0)
      IFkbdtemp$<>""THENkbdbuf$=kbdtemp$
      kbdflag%=FALSE:IFLENkbdbuf$>0THENkbdflag%=TRUE
      REM Printer
      IFLENttybuf$>0THEN
      ttemp$=LEFT$(ttybuf$,1):IFASC(ttemp$)<127THENVDUASC(ttemp$)
      ttybuf$=RIGHT$(ttybuf$,LENttybuf$-1)
      IFLENttybuf$=0 THEN ttyflag%=TRUE
      ENDIF
      ENDPROC

      DEFPROCtape
      REM HS Tape
      IF (NOT EOF#file%) THEN
      IF hstflag%= FALSE THEN hstbuffer%=BGET#file%:hstflag%=TRUE
      ELSE hstflag%=FALSE:REM PTR#file%=0
      ENDIF
      ENDPROC
      :
      DEFPROCstatus(pc%)
      CASE (mem%(pc%)AND &E00) OF
      WHEN 0:
        dis$="AND ":IF (mem%(pc%)AND &100)=&100 THEN dis$=dis$+"I "
        dis$=dis$+FNo0(((mem%(pc%) AND &80) >>7)*(pc% AND &F80) + (mem%(pc%) AND &7F),4)
        IF (mem%(pc%)AND &100)=&100 THEN dis$=dis$+" ("+FNo0(mem%(((mem%(pc%) AND &80) >>7)*(pc% AND &F80) + (mem%(pc%) AND &7F)),4)+")"
      WHEN &200:
        dis$="TAD ":IF (mem%(pc%)AND &100)=&100 THEN dis$=dis$+"I "
        dis$=dis$+FNo0(((mem%(pc%) AND &80) >>7)*(pc% AND &F80) + (mem%(pc%) AND &7F),4)
        IF (mem%(pc%)AND &100)=&100 THEN dis$=dis$+" ("+FNo0(mem%(((mem%(pc%) AND &80) >>7)*(pc% AND &F80) + (mem%(pc%) AND &7F)),4)+")"
      WHEN &400:
        dis$="ISZ ":IF (mem%(pc%)AND &100)=&100 THEN dis$=dis$+"I "
        dis$=dis$+FNo0(((mem%(pc%) AND &80) >>7)*(pc% AND &F80) + (mem%(pc%) AND &7F),4)
        IF (mem%(pc%)AND &100)=&100 THEN dis$=dis$+" ("+FNo0(mem%(((mem%(pc%) AND &80) >>7)*(pc% AND &F80) + (mem%(pc%) AND &7F)),4)+")"
      WHEN &600:
        dis$="DCA ":IF (mem%(pc%)AND &100)=&100 THEN dis$=dis$+"I "
        dis$=dis$+FNo0(((mem%(pc%) AND &80) >>7)*(pc% AND &F80) + (mem%(pc%) AND &7F),4)
        IF (mem%(pc%)AND &100)=&100 THEN dis$=dis$+" ("+FNo0(mem%(((mem%(pc%) AND &80) >>7)*(pc% AND &F80) + (mem%(pc%) AND &7F)),4)+")"
      WHEN &800:
        dis$="JMS ":IF (mem%(pc%)AND &100)=&100 THEN dis$=dis$+"I "
        dis$=dis$+FNo0(((mem%(pc%) AND &80) >>7)*(pc% AND &F80) + (mem%(pc%) AND &7F),4)
        IF (mem%(pc%)AND &100)=&100 THEN dis$=dis$+" ("+FNo0(mem%(((mem%(pc%) AND &80) >>7)*(pc% AND &F80) + (mem%(pc%) AND &7F)),4)+")"
      WHEN &A00:
        dis$="JMP ":IF (mem%(pc%)AND &100)=&100 THEN dis$=dis$+"I "
        dis$=dis$+FNo0(((mem%(pc%) AND &80) >>7)*(pc% AND &F80) + (mem%(pc%) AND &7F),4)
        IF (mem%(pc%)AND &100)=&100 THEN dis$=dis$+" ("+FNo0(mem%(((mem%(pc%) AND &80) >>7)*(pc% AND &F80) + (mem%(pc%) AND &7F)),4)+")"
      WHEN &C00:
        CASE (mem%(pc%)AND &1FF) OF
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
          WHEN 129,137,145,153,161,169,177,185: dis$="CDF "+STR$((mem%(pc%) AND &38) >>3) :REM Memory management
          WHEN 130,138,146,154,162,170,178,186: dis$="CIF "+STR$((mem%(pc%) AND &38) >>3)
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
          OTHERWISE:dis$="IOT "+FNo0(mem%(pc%)AND&1FF,3)
        ENDCASE
      WHEN &E00:
        dis$="OPR ("
        CASE (mem%(pc%)AND &F00) OF
          WHEN &E00:REM Group 1 (%1110xxxxxxxx)
            IF (mem%(pc%)AND&80)=&80 THEN dis$=dis$+" CLA"
            IF (mem%(pc%)AND&40)=&40 THEN dis$=dis$+" CLL"
            IF (mem%(pc%)AND&20)=&20 THEN dis$=dis$+" CMA"
            IF (mem%(pc%)AND&10)=&10 THEN dis$=dis$+" CML"
            IF (mem%(pc%)AND1)  =  1 THEN dis$=dis$+" IAC"
            IF (mem%(pc%)AND4)  =  4 THEN dis$=dis$+" RAL"
            IF (mem%(pc%)AND8)  =  8 THEN dis$=dis$+" RAR"
            IF (mem%(pc%)AND6)  =  6 THEN dis$=dis$+" RTL"
            IF (mem%(pc%)AND&A) = &A THEN dis$=dis$+" RTR"
            IF (mem%(pc%)AND2)  =  2 THEN : REM BSW (8e and up)
          WHEN &F00:REM Group 2 (%1111xxxxxxxx), AND/OR group
            IF (mem%(pc%)AND8)=8 THEN
              IF (mem%(pc%)AND&40)=&40 THEN dis$=dis$+" SPA"
              IF (mem%(pc%)AND&20)=&20 THEN dis$=dis$+" SNA"
              IF (mem%(pc%)AND&10)=&10 THEN dis$=dis$+" SZL"
              IF (mem%(pc%)AND&80)=&80 THEN dis$=dis$+" CLA"
              IF (mem%(pc%)AND2)  =  2 THEN dis$=dis$+" HLT"
              IF (mem%(pc%)AND4)  =  4 THEN dis$=dis$+" OSR"
            ELSE
              IF (mem%(pc%)AND&40)=&40 THEN dis$=dis$+" SMA"
              IF (mem%(pc%)AND&20)=&20 THEN dis$=dis$+" SZA"
              IF (mem%(pc%)AND&10)=&10 THEN dis$=dis$+" SNL"
              IF (mem%(pc%)AND&80)=&80 THEN dis$=dis$+" CLA"
            ENDIF
        ENDCASE
        dis$=dis$+")"
      ENDCASE
      PRINT "IF:";ifield%;" DF:";dfield%;" PC:";FNo0(pc%,4);" LINK:";link% ;" AC:";FNo0(ac%,4);" INT:";int%;" INSTR:";FNo0(mem%(pc%),4);" (";dis$;")          "
      REM PRINT "Contents of 7767,7777: ";FNo0(mem%(&FFE),4); " ";FNo0(mem%(&FFF),4); "   "
      ENDPROC
      :
      DEFPROCpause
      REPEATUNTILGET=32
      ENDPROC
      :
      DEFPROCcommand
      LOCALc$,p%:PROCstatus(pc%):REM statustemp%=TRUE:REM *********** TEST *****************
      PRINT "Stopped. ";
      REPEAT
      OSCLI"FX15,1":INPUT"COMMAND E(xamine)/C(ont)/P(C)/T(ape)/S(ingle-step toggle)",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASE c$ OF
        WHEN "S":
          statustemp%=NOT statustemp%
        WHEN "E":
          INPUT"EXAMINE ADDRESS";p%:PRINTmem%(FNo2d(p%))
        WHEN "P":
          INPUT"PC";p%:pc%=FNo2d(p%)
        WHEN "T":
          CLOSE#file%:file%=OPENIN(@dir$+"/dec-08-ajae-pb.bin"):hstbuffer%=BGET#file%:hstflag%=TRUE:PRINT"LOADED FOCAL BIN TAPE"
      ENDCASE
      UNTILc$="C"
      ENDPROC
      :
      DEFFNo2d(o%):LOCALo$:o$=RIGHT$("0000"+STR$o%,4):=VAL(LEFT$(o$,1))*512+VAL(MID$(o$,2,1))*64+VAL(MID$(o$,3,1))*8+VAL(RIGHT$(o$,1))
