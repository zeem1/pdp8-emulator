      INSTALL @dir$+"/Number.bbc"
      INSTALL @lib$+"/multiwin.bbc"
      INSTALL @dir$+"status.bbc"
      INSTALL @dir$+"IOT.bbc"
      INSTALL @dir$+"EAE.bbc"
      INSTALL @dir$+"RK8E.bbc"

      VDU 23,22,800;524;10,21,2,8:REM Window and font sizes

      OSCLI"ESC OFF":REM ASCII 27 needed in emulator
      COLOUR&80:COLOUR1:CLS

      CLOSE#0:trace%=OPENOUT(@dir$+"/trace.log"):screen%=OPENOUT(@dir$+"/screen.txt")
      file%=FALSE:rk_file0%=FALSE:rk_file1%=FALSE:rk_file2%=FALSE:rk_file3%=FALSE:REM Prevents failure when no tape or disk image opened
      :
      REM ON ERROR PROC_closewin(1):REPORT:CLOSE#0:END
      ON CLOSE PROC_closewin(1):CLOSE#0:QUIT

      DIM M% 131071
      OSCLI"TIMER 20":ON TIME PROCkbd:RETURN

      PROCinit

      PRINT "PDP-8/e Emulator"
      PRINT "================"
      PRINT '"RIM loader is at 7756, BIN loader at 7777, RK8E boot at 23"'
      C%=FNE(I%+P%):PROCcommand:REM User needs to set the machine up
      :
      REM ** Main loop **
      REPEAT
        IFF%THENF%=FALSE:PROCcommand
        IFU%THENIFNOTV%THENIF((K%ORT%)ANDkint%)THENIFW%THENU%=FALSE:PROCD(FALSE,P%):B%=(I%>>&9)+(D%>>&C):I%=FALSE:S%=FALSE:D%=FALSE:P%=&1
        IFV%<FALSE THENV%-=TRUE
        G%=P%:PROCe:IFTF%ORTS%THEN
          d$=FNstatus(G%)
          IFTF% THEN PROCtrace_file(d$)
          IFTS% THEN PROC_selectwin(&1):PRINTd$:PROC_selectwin(0)
        ENDIF
        IFH% THEN PROCpause
      UNTIL FALSE
      DEFFNA(G%)
      IF(G%AND&100)=FALSE THEN
        =I%+(((G%AND&80)>>&7)*(P%AND&F80)+(G%AND&7F))
      ELSE
        T1%=((G%AND&80)>>&7)*(P%AND&F80)+(G%AND&7F)
        IFT1%>&7 ANDT1%<&10THENPROCD(I%+T1%,(FNE(I%+T1%)-TRUE)AND&FFF)
        =D%+FNE(I%+T1%)
      ENDIF
      DEFPROCD(X%,Y%):M%!(X%<<&2)=Y%:ENDPROC
      DEFFNE(Z%):=M%!(Z%<<&2)
      DEFPROCe
      C%=FNE(I%+P%):CASEC%AND&E00OF
        WHENFALSE:A%=A%ANDFNE(FNA(C%)):P%=(P%-TRUE)AND&FFF
        WHEN&200:A%=A%+FNE(FNA(C%)):P%=(P%-TRUE)AND&FFF:IFA%>&FFFTHENA%=A%-&1000:L%=(NOTL%)AND&1
        WHEN&400:T2%=FNA(C%):T1%=(FNE(T2%)-TRUE)AND&FFF:PROCD(T2%,T1%):P%=(P%-TRUE)AND&FFF:IFT1%=FALSE THENP%=(P%-TRUE)AND&FFF
        WHEN&600:PROCD(FNA(C%),A%):A%=FALSE:P%=(P%-TRUE)AND&FFF
        WHEN&800:W%=TRUE:T1%=FNA(C%)AND&FFF:I%=S%:PROCD(I%+T1%,P%-TRUE):P%=(T1%-TRUE)AND&FFF
        WHEN&A00:W%=TRUE:P%=FNA(C%)AND&FFF:I%=S%
        WHEN&C00:PROCiot
        WHEN&E00:CASEC%AND&F00OF
            WHEN&E00:
              IF(C%AND&80)=&80THENA%=FALSE
              IF(C%AND&40)=&40THENL%=FALSE
              IF(C%AND&20)=&20THENA%=(NOTA%)AND&FFF
              IF(C%AND&10)=&10THENL%=(NOTL%)AND&1
              IF(C%AND&1)=&1THENA%-=TRUE:IFA%=&1000THENA%=FALSE:L%=(NOT L%)AND&1
              IF(C%AND&4)=&4THENA%=A%<<&1:A%=A%+L%:L%=(A%AND&1000)>>&C:A%=A%AND&FFF
              IF(C%AND&8)=&8THENA%=A%+L%*&1000:L%=A%AND&1:A%=A%>>&1
              IF(C%AND&6)=&6THENA%=A%<<&1:A%=A%+L%:L%=(A%AND&1000)>>&C:A%=A% AND &FFF
              IF(C%AND&A)=&ATHENA%=A%+L%*&1000:L%=A%AND&1:A%=A%>>&1
              IF(C%AND&E)=&2THENT1%=A%AND&FC0:A%=((A%AND&3F)<<&6)+(T1%>>&6)
            WHEN&F00:
              IF(C%AND&1)=FALSE THEN
                cond%=FALSE
                IF(C%AND&40)=&40THENIFA%>&7FF cond%=TRUE
                IF(C%AND&20)=&20THENIFA%=FALSE cond%=TRUE
                IF(C%AND&10)=&10THENIFL%=&1 cond%=TRUE
                IF(C%AND&80)=&80THENA%=FALSE
                IF(C%AND&2)=&2THENPRINT'"CPU HALT AT ";TIME$:PROCbell(150):F%=TRUE
                IF(C%AND&4)=&4THENA%=A%ORsr%
                IF(C%AND&8)=FALSE THEN
                  IFcond%=TRUE THENP%=(P%-TRUE)AND&FFF
                ELSE
                  IFcond%=FALSE THENP%=(P%-TRUE)AND&FFF
                ENDIF
              ELSE
                IF(C%AND&80)=&80THENA%=FALSE
                IF(C%AND&50)=&50THEN
                  T1%=Q%:Q%=A%:A%=T1%
                ELSE
                  IF(C%AND&40)=&40THENA%=A%ORQ%
                  IF(C%AND&10)=&10THENQ%=A%:A%=FALSE
                ENDIF
                PROCeae
              ENDIF
          ENDCASE
          P%=(P%-TRUE)AND&FFF
      ENDCASE
      ENDPROC
      :
      DEFPROCkbd
      LOCAL kbdtemp$
      IFINKEY(-114)THENF%=TRUE
      kbdtemp$=INKEY$(0):IFkbdtemp$<>""THEN
        IFASCkbdtemp$>96THENkbdtemp$=CHR$(ASC(kbdtemp$)AND&DF)
        kbdbuf$=kbdtemp$:K%=TRUE
      ENDIF
      ENDPROC

      DEFPROCtprinter
      LOCALT1%,pos%,temp$
      IFLENttybuf$>0THEN
        T1%=(ASCttybuf$)AND&7F
        CASET1% OF
          WHEN&C: T1%=FALSE:REM Ignore form-feed (clear screen), this isn't a teleprinter
          WHEN&9: pos%=((POS+8)DIV8*8)
            temp$="":FORN=1TOpos%-POS:temp$=temp$+" ":NEXT:PRINTtemp$;
            PRINT#screen%,temp$:REM Expand tabs to 8 chars
          WHEN&7: PRINT'"TTY BELL AT ";TIME$':PROCbell(200)
          WHENFALSE: REM suppress output of ASCII 0 to text output file
          OTHERWISE: VDUT1%:BPUT#screen%,T1%
        ENDCASE
        ttybuf$="":T%=TRUE:PTR#screen%=PTR#screen%:REM flush buffer
      ENDIF
      ENDPROC
      :
      DEFPROCtape
      REM HS Tape
      IFfile%<>0 THEN
        IF(NOT EOF#file%) THEN
          IFhstflag%= FALSE THEN hstbuffer%=BGET#file%:hstflag%=TRUE
        ELSE hstflag%=FALSE
        ENDIF
      ENDIF
      ENDPROC
      :
      DEFPROCpause
      PRINT"PAUSED":REPEATUNTILGET=32
      ENDPROC
      :
      DEFPROCcommand
      LOCALc$,p%:PROC_selectwin(0):PRINTFNstatus(P%)
      PRINT "Stopped. ";
      REPEAT
        OSCLI"FX15,1":INPUT"COMMAND:"'"(C)ont/(E)xamine/(D)eposit/(F)ile/De(b)ug/(Q)uit",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
        CASEc$ OF
          WHEN"E":
            PROCexamine
          WHEN"D":
            PROCmanual_deposit
          WHEN"F":
            PROCfile
          WHEN"B":
            PROCdebug
          WHEN"Q":
            PROCquit
        ENDCASE
      UNTILc$="C":PRINT'"EXECUTION STARTED AT ";TIME$'
      ENDPROC

      DEFPROCexamine
      LOCALc$
      INPUT"(M)emory or (R)egisters",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASEc$ OF
        WHEN"M":
          INPUT"EXAMINE ADDRESS";p%:PRINT"ADDR ";FNo0(FNo2d(p%),5);" = ";FNo0(M%!((FNo2d(p%))<<2),4)
        WHEN"R":
          PRINT "PC:";FNo0(P%,4);" AC:";FNo0(A%,4);" MQ:";FNo0(Q%,4);" L:";L%;" DF:";D%>>12;" IF:";I%>>12
          PRINT "EAE SC:";FNo0(eae_sc%,2);" GTF:";ABSeae_gtf%;" MODE:";:IFeae_mode%PRINT"B"ELSEPRINT"A"
      ENDCASE
      ENDPROC
      :
      DEFPROCfile
      LOCALF$,c$
      INPUT"(T)ape/(D)isk/(C)ore image",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASEc$ OF
        WHEN"T":
          OSCLI"DIR "+@dir$:OSCLI". *.BIN"
          INPUT"TAPE FILE NAME",F$
          IFfile%<>0 THEN CLOSE#file%
          file%=OPENIN(@dir$+"/"+F$):hstbuffer%=BGET#file%:hstflag%=TRUE
          IFfile%<>0 THEN PRINT"LOADED "+F$+" TAPE" ELSE PRINT "COULD NOT LOAD "+F$
        WHEN"D":
          PROCdiskmenu
        WHEN"C":
          PROCcore_image
      ENDCASE
      ENDPROC

      DEFPROCdiskmenu
      LOCALF$,c$
      INPUT"(L)oad, (U)nload, Write (P)rotect, Write (E)nable",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASEc$ OF
        WHEN"L":
          OSCLI"DIR "+@dir$:OSCLI". *.RK05"
          INPUT"RK05 FILE NAME",F$
          INPUT"LOAD TO DISK UNIT (0-3)",d%
          CASEd% OF
            WHEN0:
              IFrk_file0%<>0 THEN CLOSE#rk_file0%
              rk_file0%=OPENUP(@dir$+"/"+F$)
              IFrk_file0%<>0 THEN PRINT"LOADED "+F$+" DISK IMAGE TO UNIT ";d% ELSE PRINT "COULD NOT LOAD "+F$
            WHEN1:
              IFrk_file1%<>0 THEN CLOSE#rk_file1%
              rk_file1%=OPENUP(@dir$+"/"+F$)
              IFrk_file1%<>0 THEN PRINT"LOADED "+F$+" DISK IMAGE TO UNIT ";d% ELSE PRINT "COULD NOT LOAD "+F$
            WHEN2:
              IFrk_file2%<>0 THEN CLOSE#rk_file2%
              rk_file2%=OPENUP(@dir$+"/"+F$)
              IFrk_file2%<>0 THEN PRINT"LOADED "+F$+" DISK IMAGE TO UNIT ";d% ELSE PRINT "COULD NOT LOAD "+F$
            WHEN3:
              IFrk_file3%<>0 THEN CLOSE#rk_file3%
              rk_file3%=OPENUP(@dir$+"/"+F$)
              IFrk_file3%<>0 THEN PRINT"LOADED "+F$+" DISK IMAGE TO UNIT ";d% ELSE PRINT "COULD NOT LOAD "+F$
          ENDCASE
        WHEN"U":
          INPUT"UNLOAD FROM DISK UNIT (0-3)",d%
          CASEd% OF
            WHEN0:
              IFrk_file0%<>0 THEN CLOSE#rk_file0%:PRINT"UNLOADED RK05 UNIT ";d% ELSE PRINT "COULD NOT UNLOAD UNIT ";d%
            WHEN1:
              IFrk_file1%<>0 THEN CLOSE#rk_file1%:PRINT"UNLOADED RK05 UNIT ";d% ELSE PRINT "COULD NOT UNLOAD UNIT ";d%
            WHEN2:
              IFrk_file2%<>0 THEN CLOSE#rk_file2%:PRINT"UNLOADED RK05 UNIT ";d% ELSE PRINT "COULD NOT UNLOAD UNIT ";d%
            WHEN3:
              IFrk_file3%<>0 THEN CLOSE#rk_file3%:PRINT"UNLOADED RK05 UNIT ";d% ELSE PRINT "COULD NOT UNLOAD UNIT ";d%
          ENDCASE
        WHEN"P":
          INPUT"WRITE PROTECT DISK UNIT (0-3)",d%
          CASEd% OF
            WHEN0:
              rkro0%=TRUE
              PRINT"WRITE PROTECTED RK05 UNIT 0"
            WHEN1:
              rkro1%=TRUE
              PRINT"WRITE PROTECTED RK05 UNIT 1"
            WHEN2:
              rkro2%=TRUE
              PRINT"WRITE PROTECTED RK05 UNIT 2"
            WHEN3:
              rkro3%=TRUE
              PRINT"WRITE PROTECTED RK05 UNIT 3"
          ENDCASE
        WHEN"E":
          INPUT"WRITE ENABLE DISK UNIT (0-3)",d%
          CASEd% OF
            WHEN0:
              rkro0%=FALSE
              PRINT"WRITE ENABLED RK05 UNIT 0"
            WHEN1:
              rkro1%=FALSE
              PRINT"WRITE ENABLED RK05 UNIT 1"
            WHEN2:
              rkro2%=FALSE
              PRINT"WRITE ENABLED RK05 UNIT 2"
            WHEN3:
              rkro3%=FALSE
              PRINT"WRITE ENABLED RK05 UNIT 3"
          ENDCASE
      ENDCASE
      ENDPROC

      DEFPROCcore_image
      LOCAL F$,start%,count%,n%,l%,file%,word%
      INPUT"(L)oad/(S)ave",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASEc$ OF
        WHEN"S":
          OSCLI"DIR "+@dir$:OSCLI". *.CORE"
          INPUT"IMAGE FILE NAME TO SAVE",F$
          INPUT"START ADDRESS (OCTAL)",start%
          INPUT"NUMBER OF WORDS (OCTAL)",count%
          file%=OPENOUT(@dir$+"/"+F$)
          IFfile%<>0 THEN
            FORn%=FNo2d(start%) TO FNo2d(count%)-1
              word%=FNE(n%)
              BPUT#file%,((word%AND&FC0)>>6)+33:BPUT#file%,(word%AND63)+33
              IF(n%MOD&40)=FALSE THEN BPUT#file%,10:REM Split into 64-character (32-word) lines
            NEXT:CLOSE#file%
            PRINT"SAVED "+F$+" IMAGE"
          ELSE
            PRINT"COULD NOT SAVE "+F$
          ENDIF
        WHEN"L":
          OSCLI"DIR "+@dir$:OSCLI". *.CORE"
          INPUT"IMAGE FILE NAME",F$
          INPUT"START ADDRESS (OCTAL)",start%
          file%=OPENIN(@dir$+"/"+F$)
          IFfile%<>0 THEN
            IFTF%THENPROCtrace_file("Loading core image file")
            address%=start%
            REPEAT
              byte1%=BGET#file%:IFbyte1%=10THENbyte1%=BGET#file%
              byte2%=BGET#file%
              PROCD(address%,((byte1%-33)<<6) + (byte2%-33)):IFTF%THENPROCtrace_file("Load " + FNo0(address%,4) + " with " + FNo0(((byte1%-33)<<6) + (byte2%-33),4) )
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
      CASEc$ OF
        WHEN"P":
          INPUT"PC";p%:P%=FNo2d(p%)AND&FFF
        WHEN"A":
          INPUT"AC";p%:A%=FNo2d(p%)AND&FFF
        WHEN"Q":
          INPUT"MQ";p%:Q%=FNo2d(p%)AND&FFF
        WHEN"M":
          INPUT"START LOCATION";p%:p%=FNo2d(p%)AND&7FFF
          REPEAT
            PRINT "CURRENT CONTENTS "FNo0(p%,5)" IS ";FNo0(M%!(p%<<2),4):INPUT "ENTER NEW OCTAL VALUE (0-7777) (-1 TO QUIT):"c%
            IFFNo2d(c%)>=0ANDFNo2d(c%)<=4095THENM%!(p%<<2)=FNo2d(c%)
            p%=(p%+1)AND&FFF
          UNTILc%<0ORc%>7777
          REM UNTIL NOT(VAL(LEFT$(STR$c%,1))>=FALSE AND VAL(LEFT$(STR$c%,1))<=7 AND VAL(MID$(STR$c%,2,1))>=FALSE AND
          REM VAL(MID$(STR$c%,2,1))<=7 AND VAL(MID$(STR$c%,3,1))>=FALSE AND VAL(MID$(STR$c%,3,1))<=7 AND VAL(RIGHT$(STR$c%,1))>=FALSE AND VAL(RIGHT$(STR$c%,1))<=7)
        WHEN"S":
          REPEAT
            PRINT "CURRENT SR IS ";FNo0(sr%,4):INPUT "ENTER NEW OCTAL VALUE (0-7777):"c%
          UNTIL VAL(LEFT$(STR$c%,1))>=FALSE AND VAL(LEFT$(STR$c%,1))<=7 AND VAL(MID$(STR$c%,2,1))>=FALSE AND VAL(MID$(STR$c%,2,1))<=7 AND VAL(MID$(STR$c%,3,1))>=FALSE AND VAL(MID$(STR$c%,3,1))<=7 AND VAL(RIGHT$(STR$c%,1))>=FALSE AND VAL(RIGHT$(STR$c%,1))<=7
          sr%=FNo2d(c%)
        WHEN"D":
          REPEAT
            PRINT "CURRENT DF IS ";D%>>12:INPUT "ENTER NEW DF (0-7):"c%
          UNTIL c%>=FALSE AND c%<=7
          D%=c%<<12
        WHEN"I":
          REPEAT
            PRINT "CURRENT IFIS ";I%>>12:INPUT "ENTER NEW IF(0-7):"c%
          UNTIL c%>=FALSE AND c%<=7
          I%=c%<<12
        WHEN"B":
          REPEAT
            PRINT "CURRENT IB IS ";S%>>12:INPUT "ENTER NEW IB (0-7):"c%
          UNTIL c%>=FALSE AND c%<=7
          S%=c%<<12

      ENDCASE
      ENDPROC

      DEFPROCdebug
      LOCALc$
      INPUT"Trace to (F)ile/(T)race to screen/(S)ingle-step",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASEc$ OF
        WHEN"F":
          TF%=NOTTF%
          PRINT "TRACE TO FILE TURNED ";:IFTF% THEN PRINT "ON" ELSE PRINT "OFF"
        WHEN"T":
          TS%=NOTTS%
          PRINT "TRACE TO SCREEN TURNED ";:IFTS% THEN PRINT "ON" ELSE PRINT "OFF"
        WHEN"S":
          H%=NOTH%
          IFH% THEN TS%=TRUE:REM Enable on-screen debug when single-step turned on
          PRINT "SINGLE-STEP TURNED ";:IFH% THEN PRINT "ON" ELSE PRINT "OFF"
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
      REM U%=Interrupts on/off, V%=interrupt inhibit (e.g. ION instruction), W%=memory extension interrupt inhibit
      P%=&80:A%=FALSE:L%=FALSE:Q%=FALSE:sr%=&7FF:U%=FALSE:V%=FALSE
      REM Memory control
      I%=FALSE:D%=FALSE:S%=FALSE:B%=FALSE:W%=TRUE
      REM TTY/TAPE flags/buffers
      kint%=TRUE:kbdbuf$=CHR$(0):ttybuf$=CHR$(0):K%=FALSE:T%=TRUE:hstflag%=FALSE:hstbuffer%=FALSE
      REM RK8E
      rk_ca%=FALSE:rk_com%=FALSE:rk_da%=FALSE:rk_st%=FALSE:REM Curr addr, command, disk addr, status registers
      rkro0%=FALSE:rkro1%=FALSE:rkro2%=FALSE:rkro3%=FALSE:REM Read-only status for each drive
      REM KE8-E Extended Arithmetic Element
      eae_sc%=FALSE:eae_mode%=FALSE:eae_gtf%=FALSE:REM Step counter, mode (A=FALSE, B=TRUE), greater than flag

      REM Debugging options
      S%=FALSE:TF%=FALSE:TS%=FALSE:F%=FALSE:REM S%=single-step, TF%=trace to file, TS%=trace to screen, HLT flag

      REM Set up the RIM and BIN loaders in memory
      RESTORE
      FOR c%=&F97 TO &FFF:READ d%:PROCD(c%,d%):NEXT
      PROCD(&FFF,2753):REM JMP 7701 at location 7777
      REM BIN Loader (first byte at 7627, enter at 7701, 7777 if not overwritten by RIM loader)
      DATA 1674,2224,704,4072,2719,1162,3616,2711,650,4000,2712,652,188,737,3912,1174,4072,2966,652,174,687,1675,2712,56,3201,0,0,3097,2738,3102,1676,652,2992,3081,2744,3086
      DATA 2741,192,2275,3617,653,3842,3098,3084,3212,687,1675,3972,4032,747,746,1713,2198,2763,1677,651,1758,652,1790,2224,1773,2198,2749,2275,3856,2782,1678,766,749,653,2765,0,1934,1166,&F80,2778,0,766,3654,3590,3590,749,3043,2738,6,0,0
      REM RIM Loader (begins at 7756)
      DATA &C0C,&C09,&AEF,&C0E,&E46,&E06,&F48,&AEF,&E06,&C09,&AF7,&C0E,&F10,&7FE,&6FE,&AEF,0,0

      REM The RK8E boot loader:
      FOR c%=19 TO 26:READ d%:PROCD(c%,d%):NEXT
      DATA3079,3556,538,3558,3555,538,2585,0

      ENDPROC

      DEFPROCtrace_file(t$):PRINT#trace%,t$:BPUT#trace%,10:PTR#trace%=PTR#trace%:ENDPROC

      DEFPROCquit
      LOCALc$,c%
      INPUT"(R)estart or (Q)uit",c$:c$=LEFT$(CHR$(ASCc$AND223),1)
      CASEc$ OF
        WHEN"R":
          FORc%=FALSE TO131072:M%!c%=FALSE:NEXT
          PROCinit
        WHEN"Q":
          QUIT
      ENDCASE
      ENDPROC




