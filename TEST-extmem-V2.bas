      INSTALL @dir$+"/Number.bbc"
      INSTALL @lib$+"/multiwin.bbc"
      COLOUR128:COLOUR7:CLS

      REM PROC_multiwin(1):REM Multiple window support, 1 window
      REM HWND%=FN_createwin(1,"Output",100,100,640,512,0,0,0)
      REM ::COLOUR128:COLOUR7:CLS
      OSCLI "FONT """ + @lib$ + "DejaVuSansMono"", 10"
      test%=OPENOUT(@dir$+"/trace.log")
      :

      REM ON ERROR PROC_closewin(1):END

      REM A%=accumulator, Q%=MQ register, C%=fetched memory contents, P%=program counter, M%=address of memory block, I%=instruction field, D%=data field, L%=link register
      REM S%=single-step enabled, U%=Status display enabled, K%=keyboard flag, T%=teleprinter flag
      :
      REM Init machine

      DIM M% 131071:P%=128:A%=0:L%=0:Q%=0:sr%=0:int%=FALSE:int_inhib%=FALSE:REM PC for FOCAL69 or memory test (0200), &FEE for RIM load
      REM Memory control
      I%=0:D%=0:insbuffer%=0:intbuffer%=0:icontrol%=TRUE
      REM int%=Interrupts on/off, int_inhib%=interrupt inhibit (e.g. ION instruction), icontrol%=memory extension interrupt inhibit
      REM TTY/TAPE flags/buffers
      kint%=TRUE:kbdbuf$="":ttybuf$="":K%=FALSE:T%=TRUE:hstflag%=FALSE:hstbuffer%=0
      t%=TIME

      S%=FALSE:U%=FALSE:REM PROCopen_status:REM temp - to allow single-step and status enable at beginning

      INPUT"BIN load or core image load (B/C)",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASE c$ OF
        WHEN "C":
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
          PROCcommand:REM need to get start PC from user
        WHEN "B":
          file%=OPENIN(@dir$+"/dec-08-lbaa-pm_5-10-67.bin")
          FOR c%=&FEE TO &FFF:READ d%:PROCdeposit(I%+c%,d%):NEXT
          DATA &C0C,&C09,&AEF,&C0E,&E46,&E06,&F48,&AEF,&E06,&C09,&AF7,&C0E,&F10,&7FE,&6FE,&AEF,0,0
          P%=&FEE:REM Start the RIM loader, load the BIN loader
      ENDCASE

      :

      REM ** Main loop **
      REPEAT

        IF int_inhib%<0 THEN int_inhib%+=1:IF NOT int_inhib% THEN int%=TRUE
        IF FNirqline AND int% AND icontrol% AND NOT int_inhib% THEN int%=FALSE:PROCdeposit(FALSE,P%):intbuffer%=(I%>>9)+(D%>>12):I%=0:D%=0:P%=1
        startpc%=P%:REM for status
        REM IF TIME>t%+10 THEN
        PROCkbd:REM t%=TIME
        IFINKEY(-114)THENPROCcommand
        PROCexecute
        IF U% THEN
          REM xt%=POS:yt%=VPOS
          d$=FNstatus(startpc%):PRINTd$:PRINT#test%,d$
          REM PRINTTAB(xt%,yt%);
        ENDIF
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
      REM IF S% THEN ::PRINT "Depositing ";FNo0(word%,4);" into addr ";FNo0(address%,5): :
      IF U% THEN PRINT #test%,"Depositing "+FNo0(word%,4)+" into addr "+FNo0(address%,5)
      ENDPROC
      :
      DEFFNexamine(address%)
      REM IF S% THEN ::PRINT "Examining address ";FNo0(address%,5);", result ";FNo0(M%!(address%<<2),4): :
      IF U% THEN PRINT #test%,"Examining address "+FNo0(address%,5)+", result "+FNo0(M%!(address%<<2),4)
      =M%!(address%<<2)
      :
      DEFPROCexecute
      LOCAL addr%,temp%
      C%=FNexamine(I%+P%)
      CASE (C% AND &E00) OF
        WHEN 0:     REM AND - and operand with AC
          A%=A% AND FNexamine(FNaddr(C%))
          P%=(P%+1)AND&FFF
        WHEN &200:  REM TAD - add operand to (a 13 bit value)
          A%=A%+FNexamine(FNaddr(C%))
          IF A%>4095 THEN
            A%=A%-4096
            L%=(NOT L%)AND1
          ENDIF
          P%=(P%+1)AND&FFF
        WHEN &400:  REM ISZ - increment operand and skip if result is zero
          addr%=FNaddr(C%):temp%=(FNexamine(addr%)+1)AND&FFF
          PROCdeposit(addr%,temp%)
          IFtemp%=FALSE THENP%=(P%+1)AND&FFF
          P%=(P%+1)AND&FFF
        WHEN &600:  REM DCA - deposit AC in memory and clear AC
          PROCdeposit(FNaddr(C%),A%)
          A%=0
          P%=(P%+1)AND&FFF
        WHEN &800:  REM JMS - jump to subroutine
          icontrol%=TRUE:REM re-enable interrupts via separate memory management control
          I%=insbuffer%:REM memory control: transfer instruction field buffer to instruction field register
          temp%=FNaddr_jump(C%)
          PROCdeposit(I%+temp%,P%+1)
          P%=temp%+1
        WHEN &A00:  REM JMP - jump
          icontrol%=TRUE:REM re-enable interrupts via separate memory management control
          I%=insbuffer%:REM memory control: transfer instruction field buffer to instruction field register
          P%=FNaddr_jump(C%)
        WHEN &C00:  REM IOT - input/output transfer
          CASE (C% AND &1F8) OF
            WHEN 0:  REM Program Interrupt and flag (internal IOT)
              CASE (C% AND 7) OF
                WHEN 0:  REM SKON
                  IF int%=TRUE THEN P%=(P%+1)AND&FFF:int%=FALSE:int_inhib%=FALSE
                WHEN 1: REM ION
                  int_inhib%=-2:int%=TRUE
                WHEN 2: REM IOF
                  int%=FALSE:int_inhib%=FALSE
                WHEN 3: REM SRQ
                  IFFNirqline THEN P%=(P%+1)AND&FFF
                WHEN 4: REM GTF
                  A%=L%*2048:REM Link into bit 0
                  REM Greater than flag into bit 1; not implemented
                  A%=A%+(FNirqline AND 512):REM IRQ line into bit 2
                  A%=A%+(int_inhib% AND 256):REM Interrupt inhibit into bit 3
                  A%=A%+(int% AND 128):REM Interrupt enable into bit 4
                  REM User flag into bit 5; not implemented
                  A%=A%+intbuffer%:REM save field register into bits 6-11
                WHEN 5: REM RTF
                  L%=-((A% AND 2048)=2048):REM Link from bit 0
                  REM Greater than flag from bit 1; not implemented
                  REM User flag from bit 5; not implemented
                  intbuffer%=A%AND63:REM save field register from bits 6-11
                  int%=TRUE:int_inhib%=-2
                WHEN 6: REM SGT
                  REM Not implemented (requires EAE)
                WHEN 7: REM CAF
                  REM Clear AC and link, clear TTY flags, enable keyboard interrupt, disable interrupts globally
                  A%=0:L%=0:K%=FALSE:T%=FALSE:kint=TRUE:int%=FALSE:int_inhib%=FALSE
              ENDCASE
            WHEN 8: REM HS tape input
              PROCtape
              CASE (C% AND 7) OF
                WHEN 1: REM RSF; Reader skip if flag
                  IF hstflag%=TRUE THEN P%=(P%+1)AND&FFF
                WHEN 2: REM RRB; Read Reader Buffer
                  A%=A% OR hstbuffer%
                WHEN 4: REM RFC; Reader Flag Clear
                  hstflag%=FALSE
                WHEN 6: REM RRB RFC; read buffer and continue
                  A%=A% OR hstbuffer%
                  hstflag%=FALSE
              ENDCASE
            WHEN 9: REM HS punch output
              CASE (C% AND 7) OF
                WHEN 1: REM PSF; Punch skip if flag
                WHEN 2: REM PCF; Punch Clear Flag
                WHEN 4: REM PPC; Punch Put Character
                WHEN 6: REM PLS; Punch Load Sequence
              ENDCASE
            WHEN 24: REM Teletype keyboard/reader
              CASE (C% AND 7) OF
                WHEN 0: REM KCF
                  REM K%=FALSE
                WHEN 1: REM KSF:
                  IFK%THENP%=(P%+1)AND&FFF
                WHEN 2: REM KCC
                  K%=FALSE:A%=0:REM *** start read from tape goes here
                WHEN 4: REM KRS
                  IFLENkbdbuf$>0 THEN A%=A%ORASCkbdbuf$:kbdbuf$=RIGHT$(kbdbuf$,LENkbdbuf$-1) ELSE A%=0:REM ** Pull from kbd buffer and OR with ac
                WHEN 5: REM KIE
                  kint%=-(A% AND 1)
                WHEN 6: REM KRB
                  REM PRINT#test%,"KRB - asc kbdbuf$ = "+STR$ ASCkbdbuf$+" K%="+STR$K%+" kbdbuf$="+kbdbuf$
                  IFLENkbdbuf$>0 THEN A%=ASCkbdbuf$:K%=FALSE:kbdbuf$=RIGHT$(kbdbuf$,LENkbdbuf$-1) ELSE A%=0:K%=FALSE:REM ** Pull from kbd buffer and put in ac, clear flag
              ENDCASE
            WHEN 32:REM Teletype teleprinter/punch
              CASE (C% AND 7) OF
                WHEN0: REM SPF: ****************TEST*****************
                  T%=TRUE
                WHEN 1:  REM TSF:
                  IFT%THENP%=(P%+1)AND&FFF
                WHEN 2: REM TCF
                  T%=FALSE
                WHEN 4: REM TPC
                  ttybuf$=CHR$(A% AND &7F)
                  REM PRINT "[TPC ";A%;"]";:REM PROCpause:REM **************
                WHEN 6: REM TLS
                  T%=FALSE:ttybuf$=CHR$(A% AND &7F)
                  REM PRINT "[TLS ";A%;"]";:REM PROCpause:REM **************
              ENDCASE
              PROCtprinter
            WHEN 128,136,144,152,160,168,176,184:REM 62XX - Memory management
              CASE (C% AND 7) OF
                WHEN 1: REM 62X1 CDF; Change Data Field
                  REM D%=(C% AND &38)>>3
                  D%=(C% AND &38)<<9
                  REM PRINT "[CDF "; (C% AND &38)>>3;"]";
                WHEN 2: REM 62X2 CIF; Change Instruction Field
                  REM insbuffer%=(C% AND &38)>>3:REM Buffered until next JMP or JMS instruction
                  insbuffer%=(C% AND &38)<<9:REM Buffered until next JMP or JMS instruction
                  icontrol%=FALSE:REM Disable interrupts with separate flip-flop, until next branch
                  REM PRINT "[CIF "; (C% AND &38)>>3;"]" ;
                WHEN 3: REM 62X3 CDI; Change Data and Instruction Fields
                  D%=(C% AND &38)<<9
                  insbuffer%=D%:REM Buffered until next JMP or JMS instruction
                  icontrol%=FALSE:REM Disable interrupts with separate flip-flop, until next branch
                  REM PRINT "[CDI "; (C% AND &38)>>3;"]";
                WHEN 4: REM Other instructions
                  CASE (C% AND &38) OF
                    WHEN 8: REM 6214 RDF;  Read Data Field
                      A%=A% OR (D%>>9):REM was <<3
                    WHEN 16: REM 6224 RIF; Read Instruction Field
                      A%=A% OR (I%>>9):REM was <<3
                    WHEN 24: REM 6234 RIB; Read Interrupt Buffer
                      A%=A% OR (intbuffer% AND &3F)
                    WHEN 32: REM 6244 RMF; Restore Memory Field
                      D%=(intbuffer% AND 7)<<12
                      insbuffer%=(intbuffer% AND &38)<<9
                  ENDCASE
              ENDCASE
          ENDCASE
        ENDIF
        P%=(P%+1)AND&FFF
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
              IF (C%AND2)  =  2THEN PRINT'"CPU HALT"'"KINT=";kint%;" T%=";T%;" K%=";K%:PROCbell(150):PROCcommand
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
      LOCAL kbdtemp$
      REM Keyboard
      kbdtemp$=INKEY$(0)
      IFkbdtemp$<>""THEN
      IFASCkbdtemp$>96THENkbdtemp$=CHR$(ASC(kbdtemp$)AND223)
      kbdbuf$=kbdbuf$+kbdtemp$
      ENDIF
      IF(LENkbdbuf$>0)AND kint% THEN K%=TRUE ELSEK%=FALSE
      ENDPROC
      :
      DEFPROCtprinter
      LOCALtemp%
      IFLENttybuf$>0THEN
      temp%=(ASCttybuf$)AND&7F:VDUtemp%:IFtemp%=7THENPROCbell(200)
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
      DEFFNstatus(P%)
      LOCAL statustemp%,dis$
      statustemp%=U%:U%=FALSE:REM suppress diagnostic messages during status output
      CASE (C% AND &E00) OF
      WHEN 0,&200,&400,&600,&800,&A00:
        CASE (C% AND &E00) OF
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
        IF (C%AND &100)=&100 THEN dis$=dis$+"I "
        dis$=dis$+FNo0(((C% AND &80) >>7)*(P% AND &F80) + (C% AND &7F),4)
        IF (C%AND &100)=&100 THEN
          IF (C% AND &E00)<=&600 THEN dis$=dis$+" ("+FNo0(D%+FNexamine(I%+((C% AND &80) >>7)*(P% AND &F80) + (C% AND &7F)),5)+")" ELSE dis$=dis$+" ("+FNo0(FNexamine(I%+((C% AND &80) >>7)*(P% AND &F80) + (C% AND &7F)),4)+")"
        ENDIF
      WHEN &C00:
        CASE (C%AND &1FF) OF
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
          WHEN 24: dis$="KCF"
          WHEN 25: dis$="KSF":REM Teletype keyboard/reader
          WHEN 26: dis$="KCC"
          WHEN 28: dis$="KRS"
          WHEN 29: dis$="KIE"
          WHEN 30: dis$="KRB"
          WHEN 32: dis$="SPF"
          WHEN 33: dis$="TSF":REM Teletype teleprinter/punch
          WHEN 34: dis$="TCF"
          WHEN 36: dis$="TPC"
          WHEN 38: dis$="TLS ["+FNo0(ASC(LEFT$(ttybuf$,1)),3)+"]"
          WHEN 129,137,145,153,161,169,177,185: dis$="CDF "+STR$((C% AND &38) >>3) :REM Memory management
          WHEN 130,138,146,154,162,170,178,186: dis$="CIF "+STR$((C% AND &38) >>3)
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
          OTHERWISE:dis$="IOT "+FNo0(C%AND&1FF,3)
        ENDCASE
      WHEN &E00:
        CASE (C%AND &F00) OF
          WHEN &E00:REM Group 1 (%1110xxxxxxxx)
            IF (C%AND&80)=&80 THEN dis$=dis$+"CLA "
            IF (C%AND&40)=&40 THEN dis$=dis$+"CLL "
            IF (C%AND&20)=&20 THEN dis$=dis$+"CMA "
            IF (C%AND&10)=&10 THEN dis$=dis$+"CML "
            IF (C%AND1)  =  1 THEN dis$=dis$+"IAC "
            IF (C%AND14) =  4 THEN dis$=dis$+"RAL "
            IF (C%AND14) =  8 THEN dis$=dis$+"RAR "
            IF (C%AND14) =  6 THEN dis$=dis$+"RTL "
            IF (C%AND14) = 10 THEN dis$=dis$+"RTR "
            IF (C%AND14) =  2 THEN dis$=dis$+"BSW "
            IF (C% AND &FF)= 0 THEN dis$=dis$+"NOP "
          WHEN &F00:REM Group 2 (%1111xxxxxxx0), AND/OR group
            IF (C%AND8)=8 THEN
              IF (C%AND&40)=&40 THEN dis$=dis$+"SPA "
              IF (C%AND&20)=&20 THEN dis$=dis$+"SNA "
              IF (C%AND&10)=&10 THEN dis$=dis$+"SZL "
              IF (C%AND&80)=&80 THEN dis$=dis$+"CLA "
              IF (C%AND2)  =  2 THEN dis$=dis$+"HLT "
              IF (C%AND4)  =  4 THEN dis$=dis$+"OSR "
            ELSE
              IF (C%AND&40)=&40 THEN dis$=dis$+"SMA "
              IF (C%AND&20)=&20 THEN dis$=dis$+"SZA "
              IF (C%AND&10)=&10 THEN dis$=dis$+"SNL "
              IF (C%AND&80)=&80 THEN dis$=dis$+"CLA "
              IF (C%AND2)  =  2 THEN dis$=dis$+"HLT "
            ENDIF
            IF (C% AND &FF)= 8 THEN dis$=dis$+"SKP "
            IF (C% AND &FF)= 0 THEN dis$=dis$+"NOP "
          WHEN &F01: REM Group 3 (%1111xxxxxxx1); MQ instructions
            IF (C%AND208)=128THENdis$=dis$+"CLA "
            IF (C%AND208)=64THENdis$=dis$+"MQA "
            IF (C%AND208)=16THENdis$=dis$+"MQL "
            IF (C%AND208)=80THENdis$=dis$+"SWP "
            IF (C%AND208)=144THENdis$=dis$+"CAM ":
        ENDCASE
      ENDCASE
      U%=statustemp%:REM re-enable diagnostic messages
      ="IF:"+STR$(I%>>12)+" DF:"+STR$(D%>>12)+" PC:"+FNo0(P%,4)+" L:"+STR$L%+" AC:"+FNo0(A%,4)+" MQ:"+FNo0(Q%,4)+" INT:"+STR$(int%)+" INH:"+STR$(int_inhib%)+" KINT:"+STR$kint%+" INST:"+FNo0(C%,4)+" ("+dis$+")"
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
          INPUT"PC";p%:P%=FNo2d(p%)
        WHEN "D":
          PROCmanual_deposit
        WHEN "T":
          PROCchangetape
        WHEN "I":
          PROCsavecore
      ENDCASE
      UNTILc$="C"
      :
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
      N%=-15:REM FORN%=-15TO0
      REM SOUND1,N%,pitch%,10:REM SOUND2,N%,pitch%/1.125,5
      REM NEXT
      ENDPROC

      DEFFNirqline
      REM PRINT "FNinterrupt, K%=";K%;" T%=";T%;" kint%=";kint%
      =((K% OR T%) AND kint%)
