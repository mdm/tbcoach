DECLARE SUB GText (X%, y%, text$, c%)
DECLARE SUB SaveUser ()
DECLARE FUNCTION Parse$ (parsestr$)
DECLARE FUNCTION CheckVokabel% (antwort$, eingabe$)
DECLARE SUB FillByBListe ()
DECLARE SUB FillByWListe ()
DECLARE SUB ShowResult (antwort$, result%)
DECLARE FUNCTION CheckFach1% ()
DECLARE FUNCTION CheckFach2% ()
DECLARE FUNCTION CheckFach3% ()
DECLARE FUNCTION CheckFach4% ()
DECLARE FUNCTION CheckFach5% ()
DECLARE FUNCTION GetFachNr% ()
DECLARE SUB LoadFiles ()
DECLARE SUB Warte ()
DECLARE SUB RestoreScreen (backupnr AS INTEGER)
DECLARE SUB SaveScreen (backupnr AS INTEGER)
DECLARE SUB LoadUser ()
DECLARE SUB SaveFiles ()
DECLARE FUNCTION GetDayNr% ()
DECLARE FUNCTION StartMenu% ()
DECLARE FUNCTION SelectFromList$ (liste$(), title$)
DECLARE SUB BubbleSort (liste$())
' 2006: Yes, there was a time when I actually believed BubbleSort was fast. ^^
'DECLARE SUB MergeSort (liste$())
'DECLARE SUB QuickSort (liste$())
DECLARE SUB Vokabeln ()
DECLARE FUNCTION ReadButtons% (abutton AS INTEGER, name1$, name2$, row AS INTEGER, col AS INTEGER)
DECLARE FUNCTION ReadLaut% (col AS INTEGER, row AS INTEGER, l AS INTEGER, s$)
DECLARE SUB BenutzerErfassen ()
DECLARE FUNCTION ReadPwd% (col AS INTEGER, row AS INTEGER, l AS INTEGER, s$)
DECLARE FUNCTION Trim$ (l AS INTEGER, s$)
DECLARE SUB StrOutput (col AS INTEGER, row AS INTEGER, StrPos AS INTEGER, mode AS INTEGER, s$)
DECLARE SUB Erlaubt (ch AS INTEGER, StrPos AS INTEGER, s$)
DECLARE SUB EditKey (ch AS INTEGER, StrPos AS INTEGER, l AS INTEGER, s$)
DECLARE FUNCTION ReadStr% (col AS INTEGER, row AS INTEGER, l AS INTEGER, s$, v$)
DECLARE SUB BenutzerLaden ()
DECLARE SUB Statistik ()
DECLARE SUB Spiel ()
DECLARE SUB Optionen ()
DECLARE SUB VPaketInstallieren ()
DECLARE SUB BenutzerAufloesen ()
DECLARE SUB BenutzerErstellen ()
DECLARE SUB BDatenAendern ()
DECLARE SUB VokabelnEingeben ()
DECLARE SUB VokabelnAbfragen ()
DECLARE SUB Vokabelliste ()
DECLARE SUB Benutzerverwaltung ()
DECLARE SUB OkMsg (msg$)
DECLARE FUNCTION GetKey% ()
DECLARE FUNCTION SelectItem% (items AS INTEGER, menuitem() AS STRING, itemstate() AS INTEGER, aktivitem AS INTEGER, menutitle AS STRING)
DECLARE SUB CreateMenu (menutitle AS STRING, items AS INTEGER, menuitem() AS STRING, itemstate() AS INTEGER, itemdesc() AS STRING, aktivitem AS INTEGER)
DECLARE SUB DrawWindow (X%, y%, w%, h%, fc%, bc%, ra%, title$)
DECLARE SUB Center (znr%, text$)
DECLARE SUB MainMenu ()
' 2006: QB.BI contains various advanced type defs, etc.
' $INCLUDE: 'C:\QB45\INC\qb.bi'

TYPE vokabelrec
  frage AS STRING * 40
  antwort AS STRING * 40
END TYPE
TYPE userrec
  username AS STRING * 40
  usernr AS STRING * 4
  idnr AS INTEGER
  lessons AS INTEGER
  lastlesson AS INTEGER
END TYPE

CONST KEYCTRLRETURN% = 10
CONST KEYRETURN% = 13
CONST KEYESC% = 27
CONST KEYRIGHT% = 77 + 255
CONST KEYLEFT% = 75 + 255
CONST KEYHOME% = 71 + 255
CONST KEYEND% = 79 + 255
CONST KEYDEL% = 83 + 255
CONST KEYBACK% = 8
CONST KEYTAB% = 9
CONST KEYUP% = 72 + 255
CONST KEYDOWN% = 80 + 255
CONST KEYPGUP% = 73 + 255
CONST KEYPGDN% = 81 + 255
CONST MAUSEVENT% = 500
CONST fachbreite% = 30

' $DYNAMIC
DIM i  AS INTEGER
DIM SHARED inreg AS RegTypeX, outreg AS RegTypeX
DIM SHARED mausx AS INTEGER, mausy AS INTEGER
DIM SHARED mausflag AS INTEGER, mauslast AS INTEGER
DIM SHARED user AS INTEGER, auser AS INTEGER
DIM SHARED benutzer(1 TO 100) AS userrec
DIM SHARED karten(1 TO 7) AS INTEGER, maxkarten(1 TO 6) AS INTEGER
maxkarten(1) = 1 * fachbreite%
maxkarten(2) = 2 * fachbreite%
maxkarten(3) = 5 * fachbreite%
maxkarten(4) = 8 * fachbreite%
maxkarten(5) = 14 * fachbreite%
maxkarten(6) = fachbreite% - 4
DIM SHARED fach1(1 TO 1 * fachbreite%)  AS vokabelrec
DIM SHARED fach2(1 TO 2 * fachbreite%)  AS vokabelrec
DIM SHARED fach3(1 TO 5 * fachbreite%)  AS vokabelrec
DIM SHARED fach4(1 TO 8 * fachbreite%)  AS vokabelrec
DIM SHARED fach5(1 TO 14 * fachbreite%)  AS vokabelrec
DIM SHARED wliste AS vokabelrec
DIM SHARED backlist(1 TO fachbreite% - 4) AS vokabelrec
DIM SHARED backup(1 TO 5, 3839) AS INTEGER
DIM SHARED errormsg AS STRING, helpcontext AS STRING
' $STATIC

ON ERROR GOTO ErrorHandler

LOCATE , , 0
inreg.ax = &H1001
inreg.bx = &H300
CALL INTERRUPTX(&H10, inreg, outreg)
DEF SEG = &HB800
FOR i = 0 TO 3999 STEP 2
  POKE i, 176
  POKE i + 1, 120: '112 = dunkler
NEXT i
' 2006: I lost backlogo.dat. It was used to "emboss" a logo on the background
' It contains the numbers of the screen positions to be altered ((y+16)*80+x)
'OPEN "backlogo.dat" FOR INPUT AS 1
'  DO UNTIL EOF(1)
'    INPUT #1, i
'    POKE i + 16 * 80, 32
'    POKE 1 + i + 16 * 80, 120: '112 = dunkler
'  LOOP
'CLOSE 1
DEF SEG
COLOR 15, 3
Center 1, STRING$(80, " ")
Center 25, STRING$(80, " ")
Center 1, "Toxic Brain Coach 1.7·"
LoadUser
inreg.ax = 0
CALL INTERRUPTX(&H33, inreg, outreg)
IF StartMenu% THEN MainMenu
Ende:
COLOR 7, 0
CLS
OkMsg "Das Programm ist;;NOCH;;nicht fertig;"
LOCATE 1, 1, 1
SYSTEM

ErrorHandler:
SaveScreen 4
SELECT CASE ERR
  CASE 100
    OkMsg errormsg
    RestoreScreen 4
    RESUME NEXT
  CASE 53
    OkMsg "Die Datei " + errormsg + " wurde nicht gefunden.;"
    RestoreScreen 4
    RESUME Ende
  CASE ELSE
    'RESUME Ende
    ERROR ERR
END SELECT

SUB BDatenAendern
  '$DYNAMIC
  DIM endflag AS INTEGER, aktiv AS INTEGER, abbrkey AS INTEGER, abutton AS INTEGER
  DIM i AS INTEGER, temp1 AS STRING, temp2 AS STRING, temp3 AS STRING
  '$STATIC
  DrawWindow 41 - INT(48 / 2), 13 - INT(14 / 2), 48, 14, 15, 1, 1, "Benutzerdaten Ñndern"
  COLOR 15, 1
  LOCATE 8, 21
  PRINT "Benutzername:";
  LOCATE 9, 21
  COLOR 15, 0
  PRINT benutzer(auser).username; SPACE$(40 - LEN(benutzer(auser).username));
  temp1 = benutzer(auser).username
  LOCATE 11, 21
  COLOR 15, 1
  PRINT "Passwort:";
  LOCATE 12, 21
  COLOR 15, 0
  PRINT SPACE$(40);
  LOCATE 14, 21
  COLOR 15, 1
  PRINT "Passwort (BestÑtigung):";
  LOCATE 15, 21
  COLOR 15, 0
  PRINT SPACE$(40);
  LOCATE 17, 29
  COLOR 15, 1
  PRINT "    Ok     ";
  COLOR 0, 1
  PRINT "  Abbruch   ";
  aktiv = 1
  abutton = 1
  DO
    SELECT CASE aktiv
      CASE 1
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, "Benutzernamen eingeben (max. 40 Zeichen). [ENTER]/[TAB] = weiter"
	abbrkey = ReadStr%(21, 9, 40, temp1, temp1)
	IF abbrkey = KEYESC% THEN
	  endflag = 1
	ELSE
	  aktiv = 2
	  errormsg = "USER.DAT"
	  FOR i = 1 TO user
	    IF benutzer(i).username = temp1 AND NOT benutzer(i).usernr = benutzer(auser).usernr THEN
	      errormsg = "Einen Benutzer mit diesem Namen gibt es schon.;"
	      ERROR 100
	      aktiv = 1
	    END IF
	  NEXT i
	  IF RTRIM$(temp1) = "" THEN
	    errormsg = "Der Name des Benutzers mu· min. 1 Zeichen lang sein.;"
	    ERROR 100
	    aktiv = 1
	  END IF
	END IF
      CASE 2
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, "Passwort eingeben (max. 40 Zeichen). [ENTER]/[TAB] = weiter"
	abbrkey = ReadPwd%(21, 12, 40, temp2)
	IF abbrkey = KEYESC% THEN
	  endflag = 1
	ELSE
	  IF LEN(temp2) < 6 THEN
	    errormsg = "Das Passwort mu· mindestens 6 Zeichen lang sein.;"
	    ERROR 100
	    temp2 = ""
	    aktiv = 2
	  ELSE
	    aktiv = 3
	  END IF
	END IF
      CASE 3
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, "Passwort wiederholen (max. 40 Zeichen). [ENTER]/[TAB] = weiter"
	abbrkey = ReadPwd%(21, 15, 40, temp3)
	IF abbrkey = KEYESC% THEN
	  endflag = 1
	ELSE
	  IF NOT temp2 = temp3 THEN
	    errormsg = "Dei beiden eingegebenen Passwîrter mÅssen gleich sein.;"
	    ERROR 100
	    temp2 = ""
	    temp3 = ""
	    aktiv = 2
	  ELSE
	    aktiv = 4
	  END IF
	END IF
      CASE 4
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, CHR$(27) + " " + CHR$(26) + " = SchaltflÑche aktivieren, [ENTER] = bestÑtigen, [TAB] = weiter"
	abbrkey = ReadButtons%(abutton, "Ok", "Abbruch", 17, 29)
	SELECT CASE abbrkey
	  CASE KEYTAB%
	    aktiv = 1
	  CASE KEYRETURN%
	    endflag = 1
	  CASE KEYESC%
	    endflag = 1
	  CASE ELSE
	END SELECT
      CASE ELSE
    END SELECT
  LOOP UNTIL endflag = 1
  IF NOT (abbrkey = KEYESC% OR abutton = 2) THEN
    benutzer(auser).username = temp1
    ' 2006: idnr not saved
    idnr = 0
    FOR i = 1 TO LEN(temp2)
      idnr = idnr + ASC(MID$(temp2, i, 1))
    NEXT i
    errormsg = "USER.DAT"
    OPEN "user.dat" FOR RANDOM AS 1 LEN = LEN(benutzer)
      PUT #1, auser, benutzer(auser)
    CLOSE 1
    COLOR 15, 3
    Center 1, STRING$(80, " ")
    Center 1, "Toxic Brain Coach 1.7· - " + LTRIM$(RTRIM$(benutzer(auser).username))
  END IF
END SUB

SUB BenutzerAufloesen
END SUB

SUB BenutzerErfassen
  '$DYNAMIC
  DIM endflag AS INTEGER, aktiv AS INTEGER, abbrkey AS INTEGER, abutton AS INTEGER
  DIM i AS INTEGER, j AS INTEGER, temp1 AS STRING, temp2 AS STRING
  '$STATIC
  IF user = UBOUND(benutzer) THEN
    errormsg = "Die maximale Anzahl von Benutzern (100) wurde erreicht."
    ERROR 100
    EXIT SUB
  ELSE
    user = user + 1
  END IF
  DrawWindow 41 - INT(48 / 2), 13 - INT(14 / 2), 48, 14, 15, 1, 1, "Neuen Benutzer erfassen"
  COLOR 15, 1
  LOCATE 8, 21
  PRINT "Benutzername:";
  LOCATE 9, 21
  COLOR 15, 0
  PRINT "Neuer Benutzer"; SPACE$(26);
  benutzer(user).username = "Neuer Benutzer"
  LOCATE 11, 21
  COLOR 15, 1
  PRINT "Passwort:";
  LOCATE 12, 21
  COLOR 15, 0
  PRINT SPACE$(40);
  LOCATE 14, 21
  COLOR 15, 1
  PRINT "Passwort (BestÑtigung):";
  LOCATE 15, 21
  COLOR 15, 0
  PRINT SPACE$(40);
  LOCATE 17, 29
  COLOR 15, 1
  PRINT "    Ok     ";
  COLOR 0, 1
  PRINT "  Abbruch   ";
  aktiv = 1
  abutton = 1
  DO
    SELECT CASE aktiv
      CASE 1
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, "Benutzernamen eingeben (max. 40 Zeichen). [ENTER]/[TAB] = weiter"
	abbrkey = ReadStr%(21, 9, 40, benutzer(user).username, benutzer(user).username)
	IF abbrkey = KEYESC% THEN
	  endflag = 1
	ELSE
	  aktiv = 2
	  FOR i = 1 TO user - 1
	    IF benutzer(i).username = benutzer(user).username THEN
	      errormsg = "Einen Benutzer mit diesem Namen gibt es schon.;"
	      ERROR 100
	      aktiv = 1
	    END IF
	  NEXT i
	  IF RTRIM$(benutzer(user).username) = "" THEN
	    errormsg = "Der Name des Benutzers mu· min. 1 Zeichen lang sein.;"
	    ERROR 100
	    aktiv = 1
	  END IF
	END IF
      CASE 2
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, "Passwort eingeben (max. 40 Zeichen). [ENTER]/[TAB] = weiter"
	abbrkey = ReadPwd%(21, 12, 40, temp1)
	IF abbrkey = KEYESC% THEN
	  endflag = 1
	ELSE
	  IF LEN(temp1) < 6 THEN
	    errormsg = "Das Passwort mu· mindestens 6 Zeichen lang sein.;"
	    ERROR 100
	    temp1 = ""
	    aktiv = 2
	  ELSE
	    aktiv = 3
	  END IF
	END IF
      CASE 3
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, "Passwort wiederholen (max. 40 Zeichen). [ENTER]/[TAB] = weiter"
	abbrkey = ReadPwd%(21, 15, 40, temp2)
	IF abbrkey = KEYESC% THEN
	  endflag = 1
	ELSE
	  IF NOT temp1 = temp2 THEN
	    errormsg = "Dei beiden eingegebenen Passwîrter mÅssen gleich sein.;"
	    ERROR 100
	    temp1 = ""
	    temp2 = ""
	    aktiv = 2
	  ELSE
	    aktiv = 4
	  END IF
	END IF
      CASE 4
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, CHR$(27) + " " + CHR$(26) + " = SchaltflÑche aktivieren, [ENTER] = bestÑtigen, [TAB] = weiter"
	abbrkey = ReadButtons%(abutton, "Ok", "Abbruch", 17, 29)
	SELECT CASE abbrkey
	  CASE KEYTAB%
	    aktiv = 1
	  CASE KEYRETURN%
	    endflag = 1
	  CASE KEYESC%
	    endflag = 1
	  CASE ELSE
	END SELECT
      CASE ELSE
    END SELECT
  LOOP UNTIL endflag = 1
  IF NOT (abbrkey = KEYESC% OR abutton = 2) THEN
    j = 0
    DO
      j = j + 1
      FOR i = 1 TO user - 1
	IF VAL(benutzer(i).usernr) = j THEN
	  benutzer(user).usernr = "USED"
	ELSE
	  benutzer(user).usernr = ""
	END IF
      NEXT i
    LOOP WHILE benutzer(user).usernr = "USED"
    temp1 = LTRIM$(RTRIM$(STR$(j)))
    benutzer(user).usernr = STRING$(4 - LEN(temp1), "0") + temp1
    benutzer(user).idnr = 0
    FOR i = 1 TO LEN(temp2)
      benutzer(user).idnr = benutzer(user).idnr + ASC(MID$(temp2, i, 1))
    NEXT i
    benutzer(user).lessons = 0
    benutzer(user).lastlesson = GetDayNr%
    SaveUser
    auser = user
    COLOR 15, 3
    Center 1, STRING$(80, " ")
    Center 1, "Toxic Brain Coach 1.7· - " + LTRIM$(RTRIM$(benutzer(user).username))
  ELSE
    user = user - 1
  END IF
END SUB

SUB BenutzerLaden
  '$DYNAMIC
  DIM abbrkey AS INTEGER
  DIM i AS INTEGER, j AS INTEGER
  DIM tusername AS STRING * 40, tidnr AS INTEGER
  DIM pwd AS STRING, temp AS STRING
  DIM userliste$(1 TO user)
  '$STATIC
  FOR i = 1 TO user
    userliste$(i) = benutzer(i).username
  NEXT i
  COLOR 15, 3
  Center 25, STRING$(80, " ")
  Center 25, CHR$(25) + CHR$(24) + ", [BILD" + CHR$(25) + "]/[BILD" + CHR$(24) + "], [ENDE]/[POS1] = Benutzer auswÑhlen. [ENTER]/[TAB] = weiter"
  tusername = SelectFromList$(userliste$(), "Benutzer laden - Auswahl")
  IF NOT tusername = SPACE$(40) THEN
    i = 0
    DO
      i = i + 1
    LOOP UNTIL tusername = benutzer(i).username
    SaveScreen 3
    COLOR 15, 3
    Center 25, STRING$(80, " ")
    Center 25, "Bitte das Passwort fÅr den Benutzer eingeben. [ENTER] = bestÑtigen"
    DrawWindow 41 - INT(48 / 2), 12 - INT(7 / 2), 48, 7, 0, 3, 2, "Passwortabfrage"
    LOCATE 11, 21
    COLOR 0, 3
    PRINT "Passwort:";
    abbrkey = ReadPwd%(21, 12, 40, pwd)
    IF NOT abbrkey = KEYESC% THEN
      tidnr = 0
      FOR j = 1 TO LEN(pwd)
	tidnr = tidnr + ASC(MID$(pwd, j, 1))
      NEXT j
      IF NOT tidnr = benutzer(i).idnr THEN
	errormsg = "Das Passwort ist falsch.;Der Benutzer wird nicht geladen.;"
	ERROR 100
	abbrkey = KEYESC%
      END IF
    END IF
    IF NOT abbrkey = KEYESC% THEN
      auser = i
      LoadFiles
      IF GetDayNr% >= benutzer(auser).lastlesson THEN
	i = GetDayNr% - benutzer(auser).lastlesson
      ELSE
	i = 365
	IF VAL(RIGHT$(DATE$, 4)) MOD 4 = 0 THEN i = i + 1
	' 2006: exception needed for years, where year % 1000 = 0
	i = i - benutzer(auser).lastlesson + GetDayNr%
      END IF
      IF i > 5 THEN OkMsg "Sie haben seit" + STR$(i) + " Tage nicht gelernt.; Das ist viel zu selten.;Sie sollten min. alle 5 Tage Åben;"
      COLOR 15, 3
      Center 1, STRING$(80, " ")
      Center 1, "Toxic Brain Coach 1.7· - " + LTRIM$(RTRIM$(benutzer(auser).username))
    END IF
    RestoreScreen 3
  END IF
END SUB

SUB Benutzerverwaltung
  '$DYNAMIC
  DIM menutitle AS STRING, items AS INTEGER
  menutitle = "Benutzerverwaltung"
  items = 5
  DIM menuitem(1 TO items) AS STRING, itemstate(1 TO items) AS INTEGER
  DIM itemdesc(1 TO items) AS STRING, aktivitem AS INTEGER
  DIM endflag AS INTEGER
  menuitem(1) = "~N~euen Benutzer erfassen"
  itemdesc(1) = "Stammdaten fÅr neuen Benutzer aufnehmen"
  itemstate(1) = 1
  menuitem(2) = "~B~enutzer laden"
  itemdesc(2) = "Einen bereits vorhandenen Benutzer laden"
  itemstate(2) = 1
  menuitem(3) = "Benutzer~d~aten Ñndern"
  itemdesc(3) = "Benutzername oder Passwort Ñndern"
  itemstate(3) = 1
  menuitem(4) = "Benutzer au~f~lîsen"
  itemdesc(4) = "Nicht mehr benîtigte Benutzer als aufgelîst markieren"
  itemstate(4) = 1
  menuitem(5) = "~Z~urÅck"
  itemdesc(5) = "Zum HauptmenÅ zurÅckkehren"
  itemstate(5) = 1
  aktivitem = 1
  DO
    CreateMenu (menutitle), items, menuitem(), itemstate(), itemdesc(), aktivitem
    SELECT CASE SelectItem%(items, menuitem(), itemstate(), aktivitem, menutitle)
      CASE 1
	SaveScreen 2
	BenutzerErfassen
	RestoreScreen 2
      CASE 2
	SaveScreen 2
	BenutzerLaden
	RestoreScreen 2
      CASE 3
	SaveScreen 2
	BDatenAendern
	RestoreScreen 2
      CASE 4
	SaveScreen 2
	BenutzerAufloesen
	RestoreScreen 2
      CASE 5
	endflag = 1
      CASE ELSE
    END SELECT
  LOOP UNTIL endflag = 1
END SUB

SUB BubbleSort (liste$())
  '$DYNAMIC
  DIM e AS INTEGER, i AS INTEGER, sorting AS INTEGER
  e = UBOUND(liste$)
  DO
    sorting = 0
    FOR i = 1 TO (e - 1)
      IF liste$(i) > liste$(i + 1) THEN
	SWAP liste$(i), liste$(i + 1)
	sorting = i
      END IF
    NEXT i
    e = sorting
  LOOP WHILE sorting
END SUB

SUB Center (znr%, text$)
 LOCATE znr%, 41 - INT(LEN(text$) / 2)
 PRINT text$;
END SUB

FUNCTION CheckFach1%
  DIM endflag AS INTEGER, aktiv AS INTEGER, abbrkey AS INTEGER
  DIM i AS INTEGER, j AS INTEGER, abutton AS INTEGER
  DIM temp AS STRING * 40
  DrawWindow 41 - INT(48 / 2), 13 - INT(12 / 2), 48, 12, 15, 1, 1, "Vokabeln abfragen"
  COLOR 15, 1
  LOCATE 9, 21
  PRINT "deutsches Wort:";
  LOCATE 10, 21
  COLOR 15, 1
  PRINT fach1(1).frage;
  LOCATE 12, 21
  COLOR 15, 1
  PRINT "fremdsprachiges Wort:";
  LOCATE 13, 21
  COLOR 15, 0
  PRINT "fremdsprachiges Wort"; SPACE$(20);
  temp = "fremdsprachiges Wort"
  LOCATE 16, 29
  COLOR 15, 1
  PRINT "  Weiter    ";
  COLOR 0, 1
  PRINT "   Ende     ";
  aktiv = 1
  abutton = 1
  DO
    SELECT CASE aktiv
      CASE 1
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, "Fremdsprachiges Wort eingeben (max. 40 Zeichen). [ENTER]/[TAB] = weiter"
	abbrkey = ReadStr%(21, 13, 40, temp, temp)
	IF abbrkey = KEYESC% THEN
	  endflag = 1
	ELSE
	  j = 0
	  FOR i = 1 TO 40
	    IF MID$(wliste.antwort, i, 1) = "(" THEN j = j + 1
	    IF MID$(wliste.antwort, i, 1) = ")" THEN j = j - 1
	  NEXT i
	  IF j = 0 THEN
	    aktiv = 2
	  ELSE
	    errormsg = "Die Zahl der îffnenden Klammern mu· der der schlie·enden entsprechen"
	    ERROR 100
	  END IF
	END IF
      CASE 2
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, CHR$(27) + " " + CHR$(26) + " = SchaltflÑche aktivieren, [ENTER] = bestÑtigen, [TAB] = weiter"
	abbrkey = ReadButtons%(abutton, "Weiter", "Ende", 17, 29)
	SELECT CASE abbrkey
	  CASE KEYTAB%
	    aktiv = 1
	  CASE KEYRETURN%
	    endflag = 1
	  CASE KEYESC%
	    endflag = 1
	  CASE ELSE
	END SELECT
      CASE ELSE
    END SELECT
  LOOP UNTIL endflag = 1
  IF NOT (abbrkey = KEYESC% OR abutton = 2) THEN
    SELECT CASE CheckVokabel%((fach1(1).antwort), (temp))
      CASE 1
	karten(2) = karten(2) + 1
	fach2(karten(2)).frage = fach1(1).frage
	fach2(karten(2)).antwort = fach1(1).antwort
	SaveScreen 3
	ShowResult (fach1(1).antwort), 1
	RestoreScreen 3
      CASE 0
	karten(6) = karten(6) + 1
	backlist(karten(6)).frage = fach1(1).frage
	backlist(karten(6)).antwort = fach1(1).antwort
	SaveScreen 3
	ShowResult (fach1(1).antwort), 0
	RestoreScreen 3
      CASE 2
	OkMsg "Rechtschreibfehler!?;Bitte korrigieren."
	endflag = 0
      CASE ELSE
    END SELECT
    karten(1) = karten(1) - 1
    FOR i = 1 TO karten(1)
      fach1(i).frage = fach1(i + 1).frage
      fach1(i).antwort = fach1(i + 1).antwort
    NEXT i
  END IF
  CheckFach1% = abbrkey
END FUNCTION

FUNCTION CheckFach2%
  DIM endflag AS INTEGER, aktiv AS INTEGER, abbrkey AS INTEGER
  DIM i AS INTEGER, j AS INTEGER, abutton AS INTEGER
  DIM temp AS STRING * 40
  DrawWindow 41 - INT(48 / 2), 13 - INT(12 / 2), 48, 12, 15, 1, 1, "Vokabeln abfragen"
  COLOR 15, 1
  LOCATE 9, 21
  PRINT "deutsches Wort:";
  LOCATE 10, 21
  COLOR 15, 1
  PRINT fach2(1).frage;
  LOCATE 12, 21
  COLOR 15, 1
  PRINT "fremdsprachiges Wort:";
  LOCATE 13, 21
  COLOR 15, 0
  PRINT "fremdsprachiges Wort"; SPACE$(20);
  temp = "fremdsprachiges Wort"
  LOCATE 16, 29
  COLOR 15, 1
  PRINT "  Weiter    ";
  COLOR 0, 1
  PRINT "   Ende     ";
  aktiv = 1
  abutton = 1
  DO
    SELECT CASE aktiv
      CASE 1
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, "Fremdsprachiges Wort eingeben (max. 40 Zeichen). [ENTER]/[TAB] = weiter"
	abbrkey = ReadStr%(21, 13, 40, temp, temp)
	IF abbrkey = KEYESC% THEN
	  endflag = 1
	ELSE
	  j = 0
	  FOR i = 1 TO 40
	    IF MID$(wliste.antwort, i, 1) = "(" THEN j = j + 1
	    IF MID$(wliste.antwort, i, 1) = ")" THEN j = j - 1
	  NEXT i
	  IF j = 0 THEN
	    aktiv = 2
	  ELSE
	    errormsg = "Die Zahl der îffnenden Klammern mu· der der schlie·enden entsprechen"
	    ERROR 100
	  END IF
	END IF
      CASE 2
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, CHR$(27) + " " + CHR$(26) + " = SchaltflÑche aktivieren, [ENTER] = bestÑtigen, [TAB] = weiter"
	abbrkey = ReadButtons%(abutton, "Weiter", "Ende", 17, 29)
	SELECT CASE abbrkey
	  CASE KEYTAB%
	    aktiv = 1
	  CASE KEYRETURN%
	    endflag = 1
	  CASE KEYESC%
	    endflag = 1
	  CASE ELSE
	END SELECT
      CASE ELSE
    END SELECT
  LOOP UNTIL endflag = 1
  IF NOT (abbrkey = KEYESC% OR abutton = 2) THEN
    SELECT CASE CheckVokabel%((fach2(1).antwort), (temp))
      CASE 1
	karten(3) = karten(3) + 1
	fach3(karten(3)).frage = fach2(1).frage
	fach3(karten(3)).antwort = fach2(1).antwort
	SaveScreen 3
	ShowResult (fach2(1).antwort), 1
	RestoreScreen 3
      CASE 0
	karten(6) = karten(6) + 1
	backlist(karten(6)).frage = fach2(1).frage
	backlist(karten(6)).antwort = fach2(1).antwort
	SaveScreen 3
	ShowResult (fach2(1).antwort), 0
	RestoreScreen 3
      CASE 2
	OkMsg "Rechtschreibfehler!?;Bitte korrigieren."
	endflag = 0
      CASE ELSE
    END SELECT
    karten(2) = karten(2) - 1
    FOR i = 1 TO karten(2)
      fach2(i).frage = fach2(i + 1).frage
      fach2(i).antwort = fach2(i + 1).antwort
    NEXT i
  END IF
  CheckFach2% = abbrkey
END FUNCTION

FUNCTION CheckFach3%
  DIM endflag AS INTEGER, aktiv AS INTEGER, abbrkey AS INTEGER
  DIM i AS INTEGER, j AS INTEGER, abutton AS INTEGER
  DIM temp AS STRING * 40
  DrawWindow 41 - INT(48 / 2), 13 - INT(12 / 2), 48, 12, 15, 1, 1, "Vokabeln abfragen"
  COLOR 15, 1
  LOCATE 9, 21
  PRINT "deutsches Wort:";
  LOCATE 10, 21
  COLOR 15, 1
  PRINT fach3(1).frage;
  LOCATE 12, 21
  COLOR 15, 1
  PRINT "fremdsprachiges Wort:";
  LOCATE 13, 21
  COLOR 15, 0
  PRINT "fremdsprachiges Wort"; SPACE$(20);
  temp = "fremdsprachiges Wort"
  LOCATE 16, 29
  COLOR 15, 1
  PRINT "  Weiter    ";
  COLOR 0, 1
  PRINT "   Ende     ";
  aktiv = 1
  abutton = 1
  DO
    SELECT CASE aktiv
      CASE 1
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, "Fremdsprachiges Wort eingeben (max. 40 Zeichen). [ENTER]/[TAB] = weiter"
	abbrkey = ReadStr%(21, 13, 40, temp, temp)
	IF abbrkey = KEYESC% THEN
	  endflag = 1
	ELSE
	  j = 0
	  FOR i = 1 TO 40
	    IF MID$(wliste.antwort, i, 1) = "(" THEN j = j + 1
	    IF MID$(wliste.antwort, i, 1) = ")" THEN j = j - 1
	  NEXT i
	  IF j = 0 THEN
	    aktiv = 2
	  ELSE
	    errormsg = "Die Zahl der îffnenden Klammern mu· der der schlie·enden entsprechen"
	    ERROR 100
	  END IF
	END IF
      CASE 2
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, CHR$(27) + " " + CHR$(26) + " = SchaltflÑche aktivieren, [ENTER] = bestÑtigen, [TAB] = weiter"
	abbrkey = ReadButtons%(abutton, "Weiter", "Ende", 17, 29)
	SELECT CASE abbrkey
	  CASE KEYTAB%
	    aktiv = 1
	  CASE KEYRETURN%
	    endflag = 1
	  CASE KEYESC%
	    endflag = 1
	  CASE ELSE
	END SELECT
      CASE ELSE
    END SELECT
  LOOP UNTIL endflag = 1
  IF NOT (abbrkey = KEYESC% OR abutton = 2) THEN
    SELECT CASE CheckVokabel%((fach3(1).antwort), (temp))
      CASE 1
	karten(4) = karten(4) + 1
	fach4(karten(4)).frage = fach3(1).frage
	fach4(karten(4)).antwort = fach3(1).antwort
	SaveScreen 3
	ShowResult (fach3(1).antwort), 1
	RestoreScreen 3
      CASE 0
	karten(6) = karten(6) + 1
	backlist(karten(6)).frage = fach3(1).frage
	backlist(karten(6)).antwort = fach3(1).antwort
	SaveScreen 3
	ShowResult (fach3(1).antwort), 0
	RestoreScreen 3
      CASE 2
	OkMsg "Rechtschreibfehler!?;Bitte korrigieren."
	endflag = 0
      CASE ELSE
    END SELECT
    karten(3) = karten(3) - 1
    FOR i = 1 TO karten(3)
      fach3(i).frage = fach3(i + 1).frage
      fach3(i).antwort = fach3(i + 1).antwort
    NEXT i
  END IF
  CheckFach3% = abbrkey
END FUNCTION

FUNCTION CheckFach4%
  DIM endflag AS INTEGER, aktiv AS INTEGER, abbrkey AS INTEGER
  DIM i AS INTEGER, j AS INTEGER, abutton AS INTEGER
  DIM temp AS STRING * 40
  DrawWindow 41 - INT(48 / 2), 13 - INT(12 / 2), 48, 12, 15, 1, 1, "Vokabeln abfragen"
  COLOR 15, 1
  LOCATE 9, 21
  PRINT "deutsches Wort:";
  LOCATE 10, 21
  COLOR 15, 1
  PRINT fach4(1).frage;
  LOCATE 12, 21
  COLOR 15, 1
  PRINT "fremdsprachiges Wort:";
  LOCATE 13, 21
  COLOR 15, 0
  PRINT "fremdsprachiges Wort"; SPACE$(20);
  temp = "fremdsprachiges Wort"
  LOCATE 16, 29
  COLOR 15, 1
  PRINT "  Weiter    ";
  COLOR 0, 1
  PRINT "   Ende     ";
  aktiv = 1
  abutton = 1
  DO
    SELECT CASE aktiv
      CASE 1
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, "Fremdsprachiges Wort eingeben (max. 40 Zeichen). [ENTER]/[TAB] = weiter"
	abbrkey = ReadStr%(21, 13, 40, temp, temp)
	IF abbrkey = KEYESC% THEN
	  endflag = 1
	ELSE
	  j = 0
	  FOR i = 1 TO 40
	    IF MID$(wliste.antwort, i, 1) = "(" THEN j = j + 1
	    IF MID$(wliste.antwort, i, 1) = ")" THEN j = j - 1
	  NEXT i
	  IF j = 0 THEN
	    aktiv = 2
	  ELSE
	    errormsg = "Die Zahl der îffnenden Klammern mu· der der schlie·enden entsprechen"
	    ERROR 100
	  END IF
	END IF
      CASE 2
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, CHR$(27) + " " + CHR$(26) + " = SchaltflÑche aktivieren, [ENTER] = bestÑtigen, [TAB] = weiter"
	abbrkey = ReadButtons%(abutton, "Weiter", "Ende", 17, 29)
	SELECT CASE abbrkey
	  CASE KEYTAB%
	    aktiv = 1
	  CASE KEYRETURN%
	    endflag = 1
	  CASE KEYESC%
	    endflag = 1
	  CASE ELSE
	END SELECT
      CASE ELSE
    END SELECT
  LOOP UNTIL endflag = 1
  IF NOT (abbrkey = KEYESC% OR abutton = 2) THEN
    SELECT CASE CheckVokabel%((fach4(1).antwort), (temp))
      CASE 1
	karten(5) = karten(5) + 1
	fach5(karten(5)).frage = fach4(1).frage
	fach5(karten(5)).antwort = fach4(1).antwort
	SaveScreen 3
	ShowResult (fach4(1).antwort), 1
	RestoreScreen 3
      CASE 0
	karten(6) = karten(6) + 1
	backlist(karten(6)).frage = fach4(1).frage
	backlist(karten(6)).antwort = fach4(1).antwort
	SaveScreen 3
	ShowResult (fach4(1).antwort), 0
	RestoreScreen 3
      CASE 2
	OkMsg "Rechtschreibfehler!?;Bitte korrigieren."
	endflag = 0
      CASE ELSE
    END SELECT
    karten(4) = karten(4) - 1
    FOR i = 1 TO karten(4)
      fach4(i).frage = fach4(i + 1).frage
      fach4(i).antwort = fach4(i + 1).antwort
    NEXT i
  END IF
  CheckFach4% = abbrkey
END FUNCTION

FUNCTION CheckFach5%
  DIM endflag AS INTEGER, aktiv AS INTEGER, abbrkey AS INTEGER
  DIM i AS INTEGER, j AS INTEGER, abutton AS INTEGER
  DIM temp AS STRING * 40
  DrawWindow 41 - INT(48 / 2), 13 - INT(12 / 2), 48, 12, 15, 1, 1, "Vokabeln abfragen"
  COLOR 15, 1
  LOCATE 9, 21
  PRINT "deutsches Wort:";
  LOCATE 10, 21
  COLOR 15, 1
  PRINT fach5(1).frage;
  LOCATE 12, 21
  COLOR 15, 1
  PRINT "fremdsprachiges Wort:";
  LOCATE 13, 21
  COLOR 15, 0
  PRINT "fremdsprachiges Wort"; SPACE$(20);
  temp = "fremdsprachiges Wort"
  LOCATE 16, 29
  COLOR 15, 1
  PRINT "  Weiter    ";
  COLOR 0, 1
  PRINT "   Ende     ";
  aktiv = 1
  abutton = 1
  DO
    SELECT CASE aktiv
      CASE 1
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, "Fremdsprachiges Wort eingeben (max. 40 Zeichen). [ENTER]/[TAB] = weiter"
	abbrkey = ReadStr%(21, 13, 40, temp, temp)
	IF abbrkey = KEYESC% THEN
	  endflag = 1
	ELSE
	  j = 0
	  FOR i = 1 TO 40
	    IF MID$(wliste.antwort, i, 1) = "(" THEN j = j + 1
	    IF MID$(wliste.antwort, i, 1) = ")" THEN j = j - 1
	  NEXT i
	  IF j = 0 THEN
	    aktiv = 2
	  ELSE
	    errormsg = "Die Zahl der îffnenden Klammern mu· der der schlie·enden entsprechen"
	    ERROR 100
	  END IF
	END IF
      CASE 2
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, CHR$(27) + " " + CHR$(26) + " = SchaltflÑche aktivieren, [ENTER] = bestÑtigen, [TAB] = weiter"
	abbrkey = ReadButtons%(abutton, "Weiter", "Ende", 17, 29)
	SELECT CASE abbrkey
	  CASE KEYTAB%
	    aktiv = 1
	  CASE KEYRETURN%
	    endflag = 1
	  CASE KEYESC%
	    endflag = 1
	  CASE ELSE
	END SELECT
      CASE ELSE
    END SELECT
  LOOP UNTIL endflag = 1
  IF NOT (abbrkey = KEYESC% OR abutton = 2) THEN
    SELECT CASE CheckVokabel%((fach5(1).antwort), (temp))
      CASE 1
	' 2006: these were saved somewhere in later versions
	SaveScreen 3
	ShowResult (fach5(1).antwort), 1
	RestoreScreen 3
      CASE 0
	karten(6) = karten(6) + 1
	backlist(karten(6)).frage = fach5(1).frage
	backlist(karten(6)).antwort = fach5(1).antwort
	SaveScreen 3
	ShowResult (fach5(1).antwort), 0
	RestoreScreen 3
      CASE 2
	OkMsg "Rechtschreibfehler!?;Bitte korrigieren."
	endflag = 0
      CASE ELSE
    END SELECT
    karten(5) = karten(5) - 1
    FOR i = 1 TO karten(5)
      fach5(i).frage = fach5(i + 1).frage
      fach5(i).antwort = fach5(i + 1).antwort
    NEXT i
  END IF
  CheckFach5% = abbrkey
END FUNCTION

FUNCTION CheckVokabel% (anwort$, eingabe$)
  antwort$ = Parse$((antwort$))
  eingabe$ = Parse$((eingabe$))
  IF antwort$ = eingabe$ THEN
    CheckVokabel% = 1
  ELSE
    IF UCASE$(antwort$) = UCASE$(eingabe$) THEN
      CheckVokabel% = 2
    END IF
  END IF
END FUNCTION

SUB CreateMenu (menutitle AS STRING, items AS INTEGER, menuitem() AS STRING, itemstate() AS INTEGER, itemdesc() AS STRING, aktivitem AS INTEGER)
  '$DYNAMIC
  DIM maxlen AS INTEGER, i AS INTEGER, j AS INTEGER
  DIM fc AS INTEGER, bc AS INTEGER
  FOR i = 1 TO items
    IF LEN(menuitem(i)) - 2 > maxlen THEN maxlen = LEN(menuitem(i)) - 2
  NEXT i
  maxlen = maxlen + 8
  IF LEN(menutitle) + 2 > maxlen THEN maxlen = LEN(menutitle) + 2
  IF maxlen < 23 THEN maxlen = 23
  DrawWindow 41 - INT(maxlen / 2), 13 - INT((items * 2 + 3) / 2), maxlen, items * 2 + 3, 15, 1, 1, menutitle
  FOR i = 1 TO items
    LOCATE 13 - INT((items * 2 + 3) / 2) + 2 * i, 41 - INT(maxlen / 2) + 3
    IF i = aktivitem THEN bc = 3 ELSE bc = 1
    IF itemstate(i) = 1 THEN fc = 15 ELSE fc = 9
    j = INSTR(menuitem(i), "~")
    COLOR fc, bc
    PRINT " "; LEFT$(menuitem(i), j - 1);
    IF itemstate(i) = 1 THEN COLOR 4, bc
    PRINT MID$(menuitem(i), j + 1, 1);
    COLOR fc, bc
    PRINT MID$(menuitem(i), j + 3); " ";
    PRINT SPACE$(maxlen - 6 - LEN(menuitem(i)));
  NEXT i
  COLOR 15, 1
  LOCATE 13 - INT((items * 2 + 3) / 2) + items * 2 + 2, 41 - INT(maxlen / 2) + 3
  PRINT "Bitte wÑhlen Sie "; CHR$(25); CHR$(24);
  COLOR 15, 3
  Center 25, STRING$(80, " ")
  Center 25, itemdesc(aktivitem)
END SUB

SUB DrawWindow (X%, y%, w%, h%, fc%, bc%, ra%, title$)
  '$DYNAMIC
  DIM sx, sy, ex, ey, i, j, shadow AS INTEGER

  title$ = " " + title$ + " "
  COLOR fc%, bc%
  sx = X%
  sy = y%
  ex = X% + w% - 1
  ey = y% + h% - 1
  DEF SEG = &HB800
  shadow = 8: '7 = heller
  FOR i = sy TO ey
    FOR j = sx + 2 TO ex + 2
      POKE 2 * (i * 80 + j) - 1, shadow
    NEXT j
  NEXT i
  LOCATE sy, sx
  SELECT CASE ra%
    CASE 1
      PRINT " ⁄"; STRING$(ex - sx - 3, "ƒ"); "ø ";
      FOR i = sy + 1 TO ey - 1
	LOCATE i, sx
	PRINT " ≥"; STRING$(ex - sx - 3, " "); "≥ ";
      NEXT i
      LOCATE i, sx
      PRINT " ¿"; STRING$(ex - sx - 3, "ƒ"); "Ÿ ";
    CASE 2
      PRINT " …"; STRING$(ex - sx - 3, "Õ"); "ª ";
      FOR i = sy + 1 TO ey - 1
	LOCATE i, sx
	PRINT " ∫"; STRING$(ex - sx - 3, " "); "∫ ";
      NEXT i
      LOCATE i, sx
      PRINT " »"; STRING$(ex - sx - 3, "Õ"); "º ";
    CASE ELSE
      FOR i = sy TO ey
	LOCATE i, sx
	PRINT STRING$(ex - sx + 1, " ");
      NEXT i
  END SELECT
  LOCATE y%, X% + INT(w% / 2) - INT(LEN(title$) / 2)
  PRINT title$
END SUB

SUB EditKey (ch AS INTEGER, StrPos AS INTEGER, l AS INTEGER, s$)
  SELECT CASE ch
    CASE KEYRIGHT%
      IF StrPos < l THEN StrPos = StrPos + 1
    CASE KEYLEFT%
      IF StrPos > 1 THEN StrPos = StrPos - 1
    CASE KEYHOME%
      StrPos = 1
    CASE KEYEND%
      StrPos = l
      DO WHILE MID$(s$, StrPos - 1, 1) = " "
	StrPos = StrPos - 1
      LOOP
      IF StrPos > l THEN StrPos = l
    CASE KEYDEL%
      s$ = LEFT$(s$, StrPos - 1) + MID$(s$, StrPos + 1)
    CASE KEYBACK%
      IF StrPos > 1 THEN
	s$ = LEFT$(s$, StrPos - 2) + MID$(s$, StrPos)
	StrPos = StrPos + 1
      END IF
    CASE ELSE
  END SELECT
END SUB

SUB Erlaubt (ch AS INTEGER, StrPos AS INTEGER, s$)
  DIM buchst AS STRING * 1
  buchst = CHR$(ch)
  IF RIGHT$(s$, 1) = " " THEN
    s$ = LEFT$(s$, StrPos - 1) + buchst + MID$(s$, StrPos)
    StrPos = StrPos + 1
  ELSE
    BEEP
  END IF
END SUB

SUB FillByBListe
  DIM i AS INTEGER
  DO UNTIL karten(1) = maxkarten(1) OR karten(6) = 0
    karten(1) = karten(1) + 1
    fach1(karten(1)).frage = backlist(1).frage
    fach1(karten(1)).antwort = backlist(1).antwort
    karten(6) = karten(6) - 1
    FOR i = 1 TO karten(6)
      backlist(i).frage = backlist(i + 1).frage
      backlist(i).antwort = backlist(i + 1).antwort
    NEXT i
  LOOP
END SUB

SUB FillByWListe
  DIM i AS INTEGER
  OPEN "users\user" + benutzer(auser).usernr + ".lst" FOR RANDOM AS 1 LEN = LEN(wliste)
  OPEN "users\user" + benutzer(auser).usernr + ".tmp" FOR RANDOM AS 2 LEN = LEN(wliste)
    DO UNTIL karten(1) = maxkarten(1) OR karten(7) = 0
      karten(1) = karten(1) + 1
      GET #1, 1, fach1(karten(1))
      karten(7) = karten(7) - 1
      FOR i = 1 TO karten(7)
	GET #1, i + 1, wliste
	PUT #1, i, wliste
	PUT #2, i, wliste
      NEXT i
      msg$ = "Folgende Vokabel wurde in den Lernkreislauf aufgenommen:;;     deutsches Wort: " + fach1(karten(1)).frage + ";;fremdsprachiges Wort: " + fach1(karten(1)).antwort + ";"
      OkMsg msg$
    LOOP
  CLOSE 1
  KILL "users\user" + benutzer(auser).usernr + ".lst"
  OPEN "users\user" + benutzer(auser).usernr + ".lst" FOR RANDOM AS 1 LEN = LEN(wliste)
    FOR i = 1 TO karten(7)
      GET #2, i, wliste
      PUT #1, i, wliste
    NEXT i
  CLOSE 1, 2
  KILL "users\user" + benutzer(auser).usernr + ".tmp"
END SUB

FUNCTION GetDayNr%
  '$DYNAMIC
  DIM i AS INTEGER, days(1 TO 12) AS INTEGER, day AS INTEGER, month AS INTEGER
  days(1) = 31
  days(2) = 28
  days(3) = 31
  days(4) = 30
  days(5) = 31
  days(6) = 30
  days(7) = 31
  days(8) = 31
  days(9) = 30
  days(10) = 31
  days(11) = 30
  days(12) = 31
  month = VAL(LEFT$(DATE$, 2))
  day = VAL(MID$(DATE$, 4, 2))
  IF month = 1 THEN
    GetDayNr% = day
  ELSE
    ' 2006: typo below
    'FOR i = 1 TO monat - 1
    FOR i = 1 TO month - 1
      day = day + days(i)
    NEXT i
    GetDayNr% = day
  END IF
END FUNCTION

FUNCTION GetFachNr%
  GetFachNr% = 1
  IF karten(2) = maxkarten(2) THEN
    GetFachNr% = 2
    IF karten(3) = maxkarten(3) THEN
      GetFachNr% = 3
      IF karten(4) = maxkarten(4) THEN
	GetFachNr% = 4
	IF karten(5) = maxkarten(5) THEN
	  GetFachNr% = 5
	END IF
      END IF
    END IF
  END IF
END FUNCTION

FUNCTION GetKey%
  inreg.ax = 1
  CALL INTERRUPTX(&H33, inreg, outreg)
  DO
    ch$ = INKEY$
    inreg.ax = 3
    CALL INTERRUPTX(&H33, inreg, outreg)
    IF outreg.bx + mausflag = 1 THEN ch$ = "MAUS"
    inreg.ax = &H1680
    CALL INTERRUPTX(&H2F, inreg, outreg)
  LOOP WHILE ch$ = ""

  SELECT CASE LEN(ch$)
    CASE 1
      GetKey% = ASC(ch$)
    CASE 2
      GetKey% = ASC(RIGHT$(ch$, 1)) + 255
    CASE 4
      mausx = outreg.cx / 8 + 1
      mausy = outreg.dx / 8 + 1
      GetKey% = MAUSEVENT%
    CASE ELSE
  END SELECT
  inreg.ax = 2
  CALL INTERRUPTX(&H33, inreg, outreg)
END FUNCTION

SUB GText (X%, y%, text$, c%)
  DIM i AS INTEGER, j AS INTEGER, cnr AS INTEGER, cl AS INTEGER
  COLOR c%
  FOR cl = 1 TO LEN(text$)
    cnr = ASC(MID$(text$, cl, 1))
    FOR i = 0 TO 7
      FOR j = 0 TO 7
	DEF SEG = &HF000
	IF PEEK(&HFA6E + cnr * 8 + i) AND 2 ^ (7 - j) THEN PSET (X% + (cl - 1) * 8 + j, y% + i)
	DEF SEG
      NEXT j
    NEXT i
  NEXT cl
END SUB

SUB LoadFiles
  DIM i AS INTEGER
  OPEN "users\user" + benutzer(auser).usernr + ".vf1" FOR RANDOM AS 1 LEN = LEN(fach1(1))
    karten(1) = LOF(1) / LEN(fach1(1))
    FOR i = 1 TO karten(1)
      GET #1, i, fach1(i)
    NEXT i
  CLOSE 1
  OPEN "users\user" + benutzer(auser).usernr + ".vf2" FOR RANDOM AS 1 LEN = LEN(fach2(1))
    karten(2) = LOF(1) / LEN(fach2(1))
    FOR i = 1 TO karten(2)
      GET #1, i, fach2(i)
    NEXT i
  CLOSE 1
  OPEN "users\user" + benutzer(auser).usernr + ".vf3" FOR RANDOM AS 1 LEN = LEN(fach3(1))
    karten(3) = LOF(1) / LEN(fach3(1))
    FOR i = 1 TO karten(3)
      GET #1, i, fach3(i)
    NEXT i
  CLOSE 1
  OPEN "users\user" + benutzer(auser).usernr + ".vf4" FOR RANDOM AS 1 LEN = LEN(fach4(1))
    karten(4) = LOF(1) / LEN(fach4(1))
    FOR i = 1 TO karten(4)
      GET #1, i, fach4(i)
    NEXT i
  CLOSE 1
  OPEN "users\user" + benutzer(auser).usernr + ".vf5" FOR RANDOM AS 1 LEN = LEN(fach5(1))
    karten(5) = LOF(1) / LEN(fach5(1))
    FOR i = 1 TO karten(5)
      GET #1, i, fach5(i)
    NEXT i
  CLOSE 1
  OPEN "users\user" + benutzer(auser).usernr + ".bck" FOR RANDOM AS 1 LEN = LEN(backlist(1))
    karten(6) = LOF(1) / LEN(backlist(1))
    FOR i = 1 TO karten(6)
      GET #1, i, backlist(i)
    NEXT i
  CLOSE 1
  OPEN "users\user" + benutzer(auser).usernr + ".lst" FOR RANDOM AS 1 LEN = LEN(wliste)
    karten(7) = LOF(1) / LEN(wliste)
  CLOSE 1
END SUB

SUB LoadUser
  DIM i AS INTEGER
  errormsg = "USER.DAT"
  OPEN "user.dat" FOR RANDOM AS 1 LEN = LEN(benutzer(1))
    user = LOF(1) / LEN(benutzer(1))
    FOR i = 1 TO user
      GET #1, i, benutzer(i)
    NEXT i
  CLOSE 1
END SUB

SUB MainMenu
  '$DYNAMIC
  DIM menutitle AS STRING, items AS INTEGER
  menutitle = "HauptmenÅ"
  items = 6
  DIM menuitem(1 TO items) AS STRING, itemstate(1 TO items) AS INTEGER
  DIM itemdesc(1 TO items) AS STRING, aktivitem AS INTEGER
  DIM endflag AS INTEGER
  DIM i AS INTEGER, gkarten AS INTEGER
  menuitem(1) = "~B~enutzererwaltung..."
  itemdesc(1) = "Neue Benutzer erfassen, vorhandene laden und auflîsen"
  itemstate(1) = 1
  menuitem(2) = "~V~okabeln..."
  itemdesc(2) = "Vokabeln eingeben, anzeigen/Ñndern und abfragen"
  itemstate(2) = 1
  menuitem(3) = "~S~tatistik"
  itemdesc(3) = "Leistungsdiagramm und Ñhnliches"
  itemstate(3) = 1
  menuitem(4) = "S~p~iel"
  itemdesc(4) = CHR$(34) + "GalgenmÑnnchen" + CHR$(34) + " spielen"
  itemstate(4) = 1
  menuitem(5) = "~O~ptionen..."
  itemdesc(5) = "Sound- und Farbeinstellungen Ñndern"
  itemstate(5) = 1
  menuitem(6) = "Programm~e~nde"
  itemdesc(6) = "Toxic Brain Coach 1.7· beenden"
  itemstate(6) = 1
  aktivitem = 1
  DO
    IF lessons = 0 THEN itemstate(3) = 1 ELSE itemstate(3) = 1
    FOR i = 1 TO 7
      gkarten = gkarten + karten(i)
    NEXT i
    IF gkarten = 0 THEN itemstate(4) = 0 ELSE itemstate(4) = 1
    CreateMenu (menutitle), items, menuitem(), itemstate(), itemdesc(), aktivitem
    SELECT CASE SelectItem%(items, menuitem(), itemstate(), aktivitem, menutitle)
      CASE 1
	SaveScreen 1
	Benutzerverwaltung
	RestoreScreen 1
      CASE 2
	SaveScreen 1
	Vokabeln
	RestoreScreen 1
      CASE 3
	SaveScreen 1
	Statistik
	RestoreScreen 1
	COLOR 15, 3
	Center 1, STRING$(80, " ")
	Center 1, "Toxic Brain Coach 1.7· - " + LTRIM$(RTRIM$(benutzer(auser).username))
      CASE 4
	SaveScreen 1
	Spiel
	RestoreScreen 1
      CASE 5
	SaveScreen 1
	Optionen
	RestoreScreen 1
      CASE 6
	SaveScreen 1
	endflag = 1
	RestoreScreen 1
      CASE ELSE
    END SELECT
  LOOP UNTIL endflag = 1
END SUB

SUB OkMsg (msg$)
  DIM zeilen AS INTEGER, i AS INTEGER
  DIM bsl AS INTEGER, maxlen AS INTEGER, z AS INTEGER
  FOR i = 1 TO LEN(msg$)
    z = z + 1
    IF MID$(msg$, i, 1) = ";" THEN
      zeilen = zeilen + 1
      z = z - 1
      IF maxlen < z THEN maxlen = z
      z = 0
    END IF
  NEXT i
  maxlen = maxlen + 8
  IF maxlen < 18 THEN maxlen = 18
  bsl = 13 - INT((6 + zeilen) / 2)
  DrawWindow 41 - INT(maxlen / 2), bsl, maxlen, 6 + zeilen, 0, 3, 2, "Hinweis"
  FOR i = 1 TO zeilen
    Center bsl + 1 + i, LEFT$(msg$, INSTR(msg$, ";") - 1)
    msg$ = MID$(msg$, INSTR(msg$, ";") + 1)
  NEXT i
  COLOR 15, 0
  Center bsl + zeilen + 3, "    OK    "
  COLOR 8, 3
  PRINT "‹";
  Center bsl + zeilen + 4, "  " + STRING$(10, "ﬂ")
  Warte
END SUB

SUB Optionen
END SUB

FUNCTION Parse$ (parsestr$)
  DIM ks AS INTEGER, ke AS INTEGER, i AS INTEGER, l AS INTEGER
  DO WHILE INSTR(parsestr$, "(")
    DO
      i = i + 1
      IF MID$(parsestr$, i, 1) = "(" THEN ks = i
      IF MID$(parsestr$, i, 1) = ")" THEN ks = i
    LOOP UNTIL ks > 0 AND ks > 0
    parsestr$ = LEFT$(parsestr$, ks - 1) + MID$(parsestr$, ke + 1)
    i = 0
    ks = 0
    ke = 0
  LOOP
  DO
    l = LEN(parsestr$)
    DO
      i = i + 1
    LOOP UNTIL (MID$(parsestr$, i, 1) = " " AND MID$(parsestr$, i + 1, 1) = " ") OR i = l
    IF NOT i = l THEN parsestr$ = LEFT$(parsestr$, i - 1) + MID$(parsestr$, i + 1)

    i = 0
  LOOP UNTIL l = LEN(parsestr$)
  Parse$ = LTRIM$(RTRIM$(parsestr$))
END FUNCTION

FUNCTION ReadButtons% (abutton AS INTEGER, name1$, name2$, row AS INTEGER, col AS INTEGER)
  DO
    SELECT CASE abutton
      CASE 1
	LOCATE row, col
	COLOR 15, 3
	PRINT SPACE$(5 - INT(LEN(name1$) / 2)); name1$;
	PRINT SPACE$(5 + INT(LEN(name1$) / 2) - LEN(name1$));
	COLOR 0, 1
	PRINT "‹";
	PRINT SPACE$(6 - INT(LEN(name2$) / 2)); name2$;
	PRINT SPACE$(6 + INT(LEN(name2$) / 2) - LEN(name2$));
	LOCATE row + 1, col + 1
	PRINT STRING$(10, "ﬂ"); STRING$(12, " ");
      CASE 2
	LOCATE row, col
	COLOR 0, 1
	PRINT SPACE$(5 - INT(LEN(name1$) / 2)); name1$;
	PRINT SPACE$(7 + INT(LEN(name1$) / 2) - LEN(name1$));
	COLOR 15, 3
	PRINT SPACE$(5 - INT(LEN(name2$) / 2)); name2$;
	PRINT SPACE$(5 + INT(LEN(name2$) / 2) - LEN(name2$));
	COLOR 0, 1
	PRINT "‹";
	LOCATE row + 1, col + 1
	PRINT STRING$(12, " "); STRING$(10, "ﬂ");
      CASE ELSE
    END SELECT
    SELECT CASE GetKey%
      CASE KEYLEFT%
	IF abutton = 1 THEN abutton = 2 ELSE abutton = 1
      CASE KEYRIGHT%
	IF abutton = 1 THEN abutton = 2 ELSE abutton = 1
      CASE KEYTAB%
	ReadButtons% = KEYTAB%
	endflag = 1
      CASE KEYRETURN%
	ReadButtons% = KEYRETURN%
	endflag = 1
      CASE KEYESC%
	ReadButtons% = KEYESC%
	endflag = 1
      CASE ELSE
    END SELECT
  LOOP UNTIL endflag = 1
  LOCATE row, col
  IF abutton = 1 THEN COLOR 15, 1 ELSE COLOR 0, 1
  PRINT SPACE$(5 - INT(LEN(name1$) / 2)); name1$;
  PRINT SPACE$(7 + INT(LEN(name1$) / 2) - LEN(name1$));
  IF abutton = 2 THEN COLOR 15, 1 ELSE COLOR 0, 1
  PRINT SPACE$(5 - INT(LEN(name2$) / 2)); name2$;
  PRINT SPACE$(6 + INT(LEN(name2$) / 2) - LEN(name2$));
  LOCATE row + 1, col + 1
  PRINT STRING$(22, " ");
END FUNCTION

FUNCTION ReadPwd% (col AS INTEGER, row AS INTEGER, l AS INTEGER, s$)
  DIM ch AS INTEGER, StrPos AS INTEGER
  StrPos = 1
  DO
    s$ = Trim$(l, (s$))
    CALL StrOutput((col), (row), (StrPos), 2, (s$))
    ch = GetKey%
    IF ch > 31 AND ch < 256 THEN
      CALL Erlaubt((ch), StrPos, s$)
    ELSE
      CALL EditKey((ch), StrPos, l, s$)
    END IF
  LOOP UNTIL ch = KEYRETURN% OR ch = KEYESC% OR ch = KEYTAB%
  COLOR 15, 0
  CALL StrOutput((col), (row), (StrPos), 2, (s$))
  LOCATE , , 0
  s$ = RTRIM$(s$)
  ReadPwd% = ch
END FUNCTION

FUNCTION ReadStr% (col AS INTEGER, row AS INTEGER, l AS INTEGER, s$, v$)
  DIM ch AS INTEGER, StrPos AS INTEGER
  DIM edited AS INTEGER
  edited = 0
  StrPos = 1
  s$ = v$
  DO
    s$ = Trim$(l, (s$))
    IF edited = 0 THEN
      COLOR 15, 3
    ELSE
      COLOR 15, 0
    END IF
    CALL StrOutput((col), (row), (StrPos), 1, (s$))
    ch = GetKey%
    IF ch >= 32 AND ch <= 255 THEN
      IF edited = 0 THEN s$ = SPACE$(l)
      CALL Erlaubt((ch), StrPos, s$)
    ELSE
      IF edited = 0 AND (ch = KEYBACK% OR ch = KEYDEL%) THEN s$ = SPACE$(l)
      CALL EditKey((ch), StrPos, l, s$)
    END IF
    edited = 1
  LOOP UNTIL ch = KEYRETURN% OR ch = KEYESC% OR ch = KEYTAB%
  COLOR 15, 0
  CALL StrOutput((col), (row), (StrPos), 1, (s$))
  LOCATE , , 0
  s$ = RTRIM$(s$)
  ReadStr% = ch
END FUNCTION

SUB RestoreScreen (backupnr AS INTEGER)
  DIM i AS INTEGER
  DEF SEG = &HB800
  FOR i = 0 TO 3839
    POKE i + 159, backup(backupnr, i)
  NEXT i
  DEF SEG
END SUB

SUB SaveFiles
  DIM i AS INTEGER
  KILL "users\user" + benutzer(auser).usernr + ".vf1"
  OPEN "users\user" + benutzer(auser).usernr + ".vf1" FOR RANDOM AS 1 LEN = LEN(fach1(1))
    FOR i = 1 TO karten(1)
      PUT #1, i, fach1(i)
    NEXT i
  CLOSE 1
  KILL "users\user" + benutzer(auser).usernr + ".vf2"
  OPEN "users\user" + benutzer(auser).usernr + ".vf2" FOR RANDOM AS 1 LEN = LEN(fach2(1))
    FOR i = 1 TO karten(2)
      PUT #1, i, fach1(i)
    NEXT i
  CLOSE 1
  KILL "users\user" + benutzer(auser).usernr + ".vf3"
  OPEN "users\user" + benutzer(auser).usernr + ".vf3" FOR RANDOM AS 1 LEN = LEN(fach3(1))
    FOR i = 1 TO karten(3)
      PUT #1, i, fach1(i)
    NEXT i
  CLOSE 1
  KILL "users\user" + benutzer(auser).usernr + ".vf4"
  OPEN "users\user" + benutzer(auser).usernr + ".vf4" FOR RANDOM AS 1 LEN = LEN(fach4(1))
    FOR i = 1 TO karten(4)
      PUT #1, i, fach1(i)
    NEXT i
  CLOSE 1
  KILL "users\user" + benutzer(auser).usernr + ".vf5"
  OPEN "users\user" + benutzer(auser).usernr + ".vf5" FOR RANDOM AS 1 LEN = LEN(fach5(1))
    FOR i = 1 TO karten(5)
      PUT #1, i, fach1(i)
    NEXT i
  CLOSE 1
  KILL "users\user" + benutzer(auser).usernr + ".bck"
  OPEN "users\user" + benutzer(auser).usernr + ".bck" FOR RANDOM AS 1 LEN = LEN(backlist(1))
    FOR i = 1 TO karten(6)
      PUT #1, i, fach1(i)
    NEXT i
  CLOSE 1
END SUB

SUB SaveScreen (backupnr AS INTEGER)
  DIM i AS INTEGER
  DEF SEG = &HB800
  FOR i = 0 TO 3839
    backup(backupnr, i) = PEEK(i + 159)
  NEXT i
  DEF SEG
END SUB

SUB SaveUser
  DIM i AS INTEGER
  errormsg = "USER.DAT"
  KILL "user.dat"
  OPEN "user.dat" FOR RANDOM AS 1 LEN = LEN(benutzer(1))
    FOR i = 1 TO user
      PUT #1, i, benutzer(i)
    NEXT i
  CLOSE 1
END SUB

FUNCTION SelectFromList$ (liste$(), title$)
  DIM endflag AS INTEGER, aktiv AS INTEGER, abbrkey AS INTEGER
  DIM i AS INTEGER, abutton AS INTEGER, j AS INTEGER
  DIM vstart AS INTEGER, vende AS INTEGER, aitem AS INTEGER
  DIM filter AS STRING
  BubbleSort liste$()
  DrawWindow 41 - INT(52 / 2), 13 - INT(14 / 2), 52, 14, 15, 1, 1, titles$
  LOCATE 17, 29
  COLOR 15, 1
  PRINT "    Ok    ";
  COLOR 0, 1
  PRINT "  Abbruch  ";
  aktiv = 1
  abutton = 1
  aitem = 1
  vstart = 1
  vende = 6
  DO
    SELECT CASE aktiv
      CASE 1
	COLOR 15, 1
	LOCATE 8, 19
	PRINT "+"; STRING$(42, "-"); "+";
	LOCATE 8, 21
	PRINT filter;
	FOR i = 9 TO 14
	  LOCATE i, 19
	  PRINT "|"; SPACE$(42); "|";
	NEXT i
	FOR i = vstart TO vende
	  LOCATE 9 + i - vstart, 20
	  IF i = aitem THEN COLOR 15, 3
	  IF NOT i > UBOUND(liste$) THEN PRINT " "; liste$(i); " ";
	  COLOR 15, 1
	NEXT i
	LOCATE 15, 19
	PRINT "+"; SPACE$(42); "+";
	COLOR 15, 1
	LOCATE 15, 20
	PRINT CHR$(24); STRING$(40, "%"); CHR$(25);
	LOCATE 15, 20 + INT(aitem * 100 / UBOUND(liste$) * .4 + .5)
	PRINT "!";
	i = GetKey%
	SELECT CASE i
	  CASE KEYDOWN%
	    IF aitem = UBOUND(liste$) THEN
	      BEEP
	    ELSE
	      aitem = aitem + 1
	      IF aitem > vende THEN
		IF vende = UBOUND(liste$) THEN
		  BEEP
		  aitem = aitem - 1
		ELSE
		  vstart = vstart + 1
		  vende = vende + 1
		END IF
	      END IF
	    END IF
	  CASE KEYUP%
	    IF aitem = 1 THEN
	      BEEP
	    ELSE
	      aitem = aitem - 1
	      IF aitem < vstart THEN
		IF vstart = 1 THEN
		  BEEP
		  aitem = aitem + 1
		ELSE
		  vstart = vstart - 1
		  vende = vende - 1
		END IF
	      END IF
	    END IF
	  CASE KEYPGDN%
	    IF vende = UBOUND(liste$) THEN
	      BEEP
	    ELSE
	      aitem = aitem + vende - vstart + 1
	      i = vstart
	      vstart = vende + 1
	      vende = 2 * vende - i + 1
	      IF aitem > UBOUND(liste$) THEN aitem = UBOUND(liste$)
	      DO WHILE vende > UBOUND(liste$)
		vstart = vstart - 1
		vende = vende - 1
	      LOOP
	    END IF
	  CASE KEYPGUP%
	    IF vstart = 1 THEN
	      BEEP
	    ELSE
	      aitem = aitem - vende + vstart - 1
	      i = vstart
	      vstart = 2 * vstart - vende - 1
	      vende = i - 1
	      IF aitem < 1 THEN aitem = 1
	      DO WHILE vstart < 1
		vstart = vstart + 1
		vende = vende + 1
	      LOOP
	    END IF
	  CASE KEYEND%
	    IF aitem = UBOUND(liste$) THEN
	      BEEP
	    ELSE
	      aitem = UBOUND(liste$)
	      vstart = UBOUND(liste$) - vende + vstart
	      vende = UBOUND(liste$)
	    END IF
	  CASE KEYHOME%
	    IF aitem = 1 THEN
	      BEEP
	    ELSE
	      aitem = 1
	      vende = vende - vstart + 1
	      vstart = 1
	    END IF
	  CASE KEYTAB%
	    aktiv = 2
	  CASE KEYTURN%
	    aktiv = 2
	  CASE KEYESC%
	    abbrkey = KEYESC%
	    endflag = 1
	  CASE ELSE
	    IF i = KEYBACK% THEN
	      IF filter = "" THEN
		BEEP
	      ELSE
		filter = LEFT$(filter, LEN(filter) - 1)
	      END IF
	      j = 0
	      i = 0
	      DO
		i = i + 1
		IF UCASE$(filter) = UCASE$(LEFT$(liste$(i), LEN(filter))) THEN j = 1
	      LOOP UNTIL i = UBOUND(liste$) OR j > 0
	      IF j = 0 THEN
		BEEP
		filter = LEFT$(filter, LEN(filter) - 1)
	      ELSE
		aitem = j
		i = vstart
		vstart = j
		vende = vstart + vende - i
		DO WHILE vende > UBOUND(liste$)
		  vstart = vstart - 1
		  vende = vende - 1
		LOOP
	      END IF
	    ELSEIF i > 31 AND i < 256 THEN
	      filter = filter + CHR$(i)
	      j = 0
	      i = 0
	      DO
		i = i + 1
		IF UCASE$(filter) = UCASE$(LEFT$(liste$(i), LEN(filter))) THEN j = 1
	      LOOP UNTIL i = UBOUND(liste$) OR j > 0
	      IF j = 0 THEN
		BEEP
		filter = LEFT$(filter, LEN(filter) - 1)
	      ELSE
		aitem = j
		i = vstart
		vstart = j
		vende = vstart + vende - i
		DO WHILE vende > UBOUND(liste$)
		  vstart = vstart - 1
		  vende = vende - 1
		LOOP
	      END IF
	    ELSE
	      BEEP
	    END IF
	END SELECT
      CASE 2
	COLOR 15, 3
	Center 25, STRING$(80, " ")
	Center 25, CHR$(27) + " " + CHR$(26) + " = SchaltflÑche aktivieren, [ENTER] = bestÑtigen, [TAB] = weiter"
	abbrkey = ReadButtons%(abutton, "Ok", "Abbruch", 17, 29)
	SELECT CASE abbrkey
	  CASE KEYTAB%
	    aktiv = 1
	  CASE KEYRETURN%
	    endflag = 1
	  CASE KEYESC%
	    endflag = 1
	  CASE ELSE
	END SELECT
      CASE ELSE
    END SELECT
  LOOP UNTIL endflag = 1
  IF abbrkey = KEYESC% OR abutton = 2 THEN
    SelectFromList$ = ""
  ELSE
    SelectFromList$ = liste$(aitem)
  END IF
END FUNCTION

FUNCTION SelectItem% (items AS INTEGER, menuitem() AS STRING, itemstate() AS INTEGER, aktivitem AS INTEGER, menutitle AS STRING)
  DIM i AS INTEGER, j AS INTEGER, endflag AS INTEGER
  DO
    i = GetKey%
    SELECT CASE i
      CASE MAUSEVENT%
	FOR i = 1 TO items
	  IF LEN(menuitem(i)) - 2 > maxlen THEN maxlen = LEN(menuitem(i)) - 2
	NEXT i
	maxlen = maxlen + 8
	IF LEN(menutitel) + 2 > maxlen THEN maxlen = LEN(menutitle) + 2
	IF maxlen < 23 THEN maxlen = 23
	FOR i = 1 TO items
	  IF mausy = 13 - INT((items * 2 + 3) / 2) + 2 * i THEN
	    IF mausy > (41 - INT(maxlen / 2) + 2) AND mausx < (41 - INT(maxlen / 2) + 3 + maxlen) THEN j = i
	  END IF
	NEXT i
	IF j > 0 AND j <= items THEN
	  IF mausflag = 1 THEN
	    IF itemstate(aktivitem) = 1 AND j = mauslast THEN
	      SelectItem% = j
	      endflag = 1
	    END IF
	  ELSE
	    mauslast = j
	    aktivitem = mauslast
	    SelectItem% = 0
	    endflag = 1
	  END IF
	  mausflag = 1 - mausflag
	END IF
      CASE KEYESC%
	SelectItem% = items
	endflag = 1
      CASE KEYRETURN%
	IF itemstate(aktivitem) = 1 THEN
	  SelectItem% = aktivitem
	  endflag = 1
	END IF
      CASE KEYUP%
	aktivitem = aktivitem - 1
	IF aktivitem < 1 THEN aktivitem = items
	SelectItem% = 0
	endflag = 1
      CASE KEYDOWN%
	aktivitem = aktivitem + 1
	IF aktivitem > items THEN aktivitem = 1
	SelecItem% = 0
	endflag = 1
      CASE ELSE
	IF i < 255 THEN
	  FOR j = 1 TO items
	    IF UCASE$(CHR$(i)) = UCASE$(MID$(menuitem(j), INSTR(menuitem(j), "~") + 1, 1)) THEN
	      aktivitem = j
	      SelectItem% = aktivitem
	      endflag = 1
	    END IF
	  NEXT j
	END IF
    END SELECT
  LOOP UNTIL endflag = 1
END FUNCTION

SUB ShowResult (antwort$, result%)
  DIM bc AS INTEGER
  DIM title AS STRING
  DIM maxlen AS INTEGER
  SELECT CASE result%
    CASE 0
      bc = 4
      title = "Falsch"
    CASE 1
      bc = 2
      title = "Richtig"
    CASE ELSE
  END SELECT
  antwort$ = LTRIM$(RTRIM$(antwort$))
  maxlen = LEN(antwort4) + 8
  IF maxlen < 18 THEN maxlen = 18
  DrawWindow 41 - INT(maxlen / 2), 13 - INT(7 / 2), maxlen, 7, 15, bc, 2, title
  Center 12, antwort$
  COLOR 15, 0
  Center 14, "    OK    "
  COLOR 8, bc
  PRINT "!";
  Center 15, "  " + STRING$(10, "!")
  Warte
END SUB

SUB Spiel
  DrawWindow 41 - INT(72 / 2), 13 - INT(20 / 2), 72, 20, 15, 1, 1, CHR$(34) + "GalgenmÑnnchen" + CHR$(34)
  Warte
END SUB

FUNCTION StartMenu%
  DIM menutitle AS STRING, items AS INTEGER
  menutitle = "Startmenu"
  items = 3
  DIM menuitem(1 TO items) AS STRING, itemstate(1 TO items) AS INTEGER
  DIM itemdesc(1 TO items) AS STRING, aktivitem AS INTEGER
  DIM endflag AS INTEGER
  menuitem(1) = "~N~euen Benutzer erfassen"
  itemdesc(1) = "Stammdaten fÅr neuen Benutzer aufnehmen"
  itemstate(1) = 1
  menuitem(2) = "~B~enutzer laden"
  itemdesc(2) = "Einen bereits vorhandenen Benutzer laden"
  IF user > 0 THEN itemstate(2) = 1
  menuitem(3) = "Programm~e~nde"
  itemdesc(3) = "Toxic Brain Coach 1.7 beenden"
  itemstate(3) = 1
  aktivitem = 1
  SaveScreen 1
  DO
    CreateMenu (menutitle), items, menuitem(), itemstate(), itemdesc(), aktivitem
    SELECT CASE SelectItem%(items, menuitem(), itemstate(), aktivitem, menutitle)
      CASE 1
	SaveScreen 2
	BenutzerErfassen
	RestoreScreen 2
      CASE 2
	SaveScreen 2
	BenutzerLaden
	RestoreScreen 2
      CASE 3
	endflag = 1
      CASE ELSE
    END SELECT
  LOOP WHILE auser = 0 AND endflag = 0
  RestoreScreen 1
  IF auser = 0 THEN
    StartMenu% = 0
  ELSE
    StartMenu% = 1
  END IF
END FUNCTION

SUB Statistik
  SCREEN 12
  inreg.ax = &H1001
  inreg.bx = &H300
  CALL INTERRUPTX(&H10, inreg, outreg)
  LINE (0, 0)-(639, 15), 3, BF
  GText 320 - INT((LEN(LTRIM$(RTRIM$(benutzer(auser).username))) + 24) * 8 / 2), 4, "Toxic Brain Coach 1.7 - " + LTRIM$(RTRIM$(benutzer(auser).username)), 15
  DIM anzahl AS INTEGER, sw AS INTEGER, i AS INTEGER, msg AS STRING
  DIM j AS INTEGER
  '**
  anzahl = 70
  sw = INT(600 / anzahl)
  '**
  COLOR 15
  LINE (19, 20)-(19, 320), 15
  FOR i = 20 TO 320 STEP 30
    LINE (17, i)-(21, i), 15
    FOR j = 19 TO (19 + anzahl * sw) STEP sw
      PSET (j, i)
    NEXT j
    IF NOT i = 320 THEN GText 20 + anzahl * sw, i, STR$(100 - (i - 20) / 3), 15
  NEXT i
  msg = "Richtige Antworten in %"
  FOR i = 1 TO LEN(msg)
    GText 4, 59 + i * 8, MID$(msg, i, 1), 15
  NEXT i
  LINE (19, 320)-(19 + anzahl * sw, 320), 15
  FOR i = 19 TO (19 + anzahl * sw) STEP sw
    LINE (i, 318)-(i, 322), 15
  NEXT i
  msg = "Nummer der Abfrage innerhalb der letzen 100"
  GText 148, 328, msg, 15
  Warte
  SCREEN 0
END SUB

SUB StrOutput (col AS INTEGER, row AS INTEGER, StrPos AS INTEGER, mode AS INTEGER, s$)
  LOCATE row, col, 0
  SELECT CASE mode
    CASE 1
      PRINT RTRIM$(s$);
      COLOR 15, 0
      PRINT SPACE$(LEN(s$) - LEN(RTRIM$(s$)))
      LOCATE row, col + StrPos - 1, 1
    CASE 2
      PRINT STRING$(LEN(RTRIM$(s$)), "*");
      COLOR 15, 0
      PRINT SPACE$(LEN(s$) - LEN(RTRIM$(s$)))
      LOCATE row, col + StrPos - 1, 1
    CASE 3
      PRINT "["; RTRIM$(s$); "]";
      COLOR 15, 0
      PRINT SPACE$(LEN(s$) - LEN(RTRIM$(s$)))
      LOCATE row, col + StrPos, 1
    CASE ELSE
  END SELECT
END SUB

FUNCTION Trim$ (l AS INTEGER, s$)
  IF LEN(s$) < l THEN
    Trim$ = s$ + SPACE$(l - LEN(s$))
  ELSEIF LEN(s$) >= l THEN
    Trim$ = LEFT$(s$, l)
  END IF
END FUNCTION

SUB Vokabelliste
  DIM endflag AS INTEGER, aktiv AS INTEGER, abbrkey AS INTEGER
  DIM i AS INTEGER, abutton AS INTEGER, j AS INTEGER
  DIM temp AS STRING
  DIM userliste$(1 TO user)
  OPEN "d:\tb\coach\main\user.dat" FOR INPUT AS 1
    FOR i = 1 TO user
      LINE INPUT #1, userliste$(i)
      userliste$(i) = MID$(userliste$(i), 2, 40)
    NEXT i
  CLOSE 1
  COLOR 15, 3
  Center 25, STRING$(80, " ")
  Center 25, CHR$(25) + CHR$(24) + ", [BILD" + CHR$(25) + "]/[BILD" + CHR$(24) + "], [ENDE]/[POS1] = Vokabel auswÑhlen. [ENTER]/[TAB] = weiter"
  temp = SelectFromList$(userliste$(), "Vokabeln anzeigen/Ñndern - Auswahl")
END SUB

SUB Vokabeln
  DIM menutitle AS STRING, items AS INTEGER
  menutitle = "Vokabelverwaltung"
  items = 5
  DIM menuitem(1 TO items) AS STRING, itemstate(1 TO items) AS INTEGER
  DIM itemdesc(1 TO items) AS STRING, aktivitem AS INTEGER
  DIM endflag AS INTEGER
  menuitem(1) = "~N~eue Vokabeln eingeben"
  itemdesc(1) = "Neue Vokabeln fÅr diesen Benutzer eingeben"
  itemstate(1) = 1
  menuitem(2) = "~V~okabel abfragen"
  itemdesc(2) = "Neue Vokabeln aus der Warteliste Åbernehmen und Abfrage starten"
  itemstate(2) = 1
  menuitem(3) = "Vokabeln ~a~nzeigen/Ñndern"
  itemdesc(3) = "Alphabetische Vokabelliste anzeigen - ggf. Vokabeln daraus Ñndern"
  itemstate(3) = 1
  menuitem(4) = "Vokabelpaket ~i~nstallieren"
  itemdesc(4) = "Installieren eines Vokabelpakets von Toxic Brain (siehe BESTELL.TXT)"
  itemstate(4) = 1
  menuitem(5) = "~Z~urÅck"
  itemdesc(5) = "Zum HauptmenÅ zurÅckkehren"
  itemstate(5) = 1
  aktivitem = 1
  DO
    CreateMenu (menutitle), items, menuitem(), itemstate(), itemdesc(), aktivitem
    SELECT CASE SelectItem%(items, menuitem(), itemstate(), aktivitem, menutitle)
      CASE 1
	SaveScreen 2
	VokabelnEingeben
	RestoreScreen 2
      CASE 2
	SaveScreen 2
	VokabelnAbfragen
	RestoreScreen 2
      CASE 3
	SaveScreen 2
	Vokabelliste
	RestoreScreen 2
      CASE 4
	SaveScreen 2
	VPaketInstallieren
	RestoreScreen 2
      CASE 5
	endflag = 1
      CASE ELSE
    END SELECT
  LOOP UNTIL endflag = 1
END SUB

SUB VokabelnAbfragen
  DIM fachnr AS INTEGER, abbrkey AS INTEGER, learned AS INTEGER
  DIM i AS INTEGER, counter AS INTEGER
  FillByBListe
  SaveScreen 3
  FillByWListe
  RestoreScreen 3
  fachnr = GetFachNr%
  DO WHILE karten(fachnr) > maxkarten(fachnr) - 25 AND NOT abbrkey = KEYESC%
    i = karten(6)
    SELECT CASE fachnr
      CASE 1
	abbrkey = CheckFach1%
      CASE 2
	abbrkey = CheckFach2%
      CASE 3
	abbrkey = CheckFach3%
      CASE 4
	abbrkey = CheckFach4%
      CASE 5
	abbrkey = CheckFach5%
      CASE ELSE
    END SELECT
    IF NOT avvrkey = KEYESC% THEN
      learned = learned + 1
      IF karten(6) = i THEN counter = counter + 1
    END IF
    IF fachnr < 5 AND karten(fachnr + 1) = maxkarten(fachnr + 1) THEN fachnr = GetFachNr%
    FillByBListe
  LOOP
  SaveFiles
  IF learned > 0 THEN
    benutzer(auser).lessons = benutzer(auser).lessons + 1
    benutzer(auser).lastlesson = GetDayNr%
    OPEN "d:\tb\coach\main\user\user" + benutzer(auser).usernr + ".stk" FOR RANDOM AS 1 LEN = 2
      IF LOF(1) / 2 = 100 THEN
	FOR i = 1 TO 99
	  GET #1, i + 1, abbrkey
	  PUT #1, i, abbrkey
	NEXT i
      END IF
      i = INT(counter / learned * 100 + .5)
      PUT #1, LOF(1) / 2 + 1, i
    CLOSE 1
  END IF
END SUB

SUB VokabelnEingeben
  DIM endflag AS INTEGER, aktiv AS INTEGER, abbrkey AS INTEGER
  DIM i AS INTEGER, j AS INTEGER, abutton AS INTEGER
  DO
    DrawWindow 41 - INT(48 / 2), 13 - INT(12 / 2), 48, 12, 15, 1, 1, "Neue Vokabeln eingeben"
    COLOR 15, 1
    LOCATE 9, 2
    PRINT "fremdsprachiges Wort:";
    LOCATE 10, 21
    COLOR 15, 0
    PRINT "fremdsprachiges Wort"; SPACE$(20)
    wliste.antwort = "fremdsprachiges Wort"
    LOCATE 12, 21
    COLOR 15, 1
    PRINT "deutsches Wort:";
    LOCATE 13, 21
    COLOR 15, 0
    PRINT "deutsches Wort"; SPACE$(26);
    wliste.frage = "deutsches Wort"
    LOCATE 16, 29
    COLOR 15, 1
    PRINT "  Weiter    ";
    COLOR 0, 1
    PRINT " ENDE     ";
    aktiv = 1
    abutton = 1
    DO
      SELECT CASE aktiv
	CASE 1
	  COLOR 15, 3
	  Center 25, STRING$(80, " ")
	  Center 25, "Fremdsprachiges Wort eingeben (max. 40 Zeichen). [ENTER]/[TAB] = weiter"
	  abbrkey = ReadStr%(21, 10, 40, wliste.antwort, wliste.antwort)
	  IF abbrkey = KEYESC% THEN
	    endflag = 1
	  ELSE
	    j = 0
	    FOR i = 1 TO 40
	      IF MID$(wliste.antwort, i, 1) = "(" THEN j = j + 1
	      IF MID$(wliste.antwort, i, 1) = ")" THEN j = j - 1
	    NEXT i
	    IF j = 0 THEN
	      aktiv = 2
	    ELSE
	      errormsg = "Die Zahl der îffnenden Klammern mu· der Zahl;der schlie·enden Klammern entsprechen.;"
	      ERROR 100
	    END IF
	  END IF
	CASE 2
	  COLOR 15, 3
	  Center 25, STRING$(80, " ")
	  Center 25, "Deutsches Wort eingeben (max. 40 Zeichen). [ENTER]/[TAB] =weiter"
	  abbrkey = ReadStr%(21, 13, 40, wliste.frage, wliste.frage)
	  IF abbrkey = KEYESC% THEN endflag = 1 ELSE aktiv = 3
	CASE 3
	  COLOR 15, 3
	  Center 25, STRING$(80, " ")
	  Center 25, CHR$(27) + " " + CHR$(26) + " = SchaltflÑche aktivieren, [ENTER] =	bestÑtigen, [TAB] = weiter"
	  abbrkey = ReadButtons%(abutton, "Weiter", "Ende", 16, 29)
	  SELECT CASE abbrkey
	    CASE KEYTAB%
	      aktiv = 1
	    CASE KEYRETURN%
	      endflag = 1
	    CASE KEYESC%
	      endflag = 1
	    CASE ELSE
	  END SELECT
	CASE ELSE
      END SELECT
    LOOP UNTIL endflag = 1
    endflag = 0
    IF abbrkey = KEYESC% OR abutton = 2 THEN
      endflag = 1
    ELSE
      OPEN "d:\tb\coach\main\user\user" + benutzer(auser).usernr + ".lst" FOR RANDOM AS 1 LEN = LEN(wliste)
	PUT #1, LOF(1) / LEN(wliste) + 1, wliste
      CLOSE 1
      karten(7) = karten(7) + 1
    END IF
  LOOP UNTIL endflag = 1
END SUB

SUB VPaketInstallieren
END SUB

SUB Warte
  DO
    inreg.ax = &H1680
    CALL INTERRUPTX(&H2F, inreg, outreg)
  LOOP UNTIL LEN(INKEY$)
END SUB

