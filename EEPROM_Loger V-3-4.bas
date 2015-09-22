' ------------------------------------------------------------------------------
$PROG &HFF,&HE2,&HDF,&HFF' generated. Take care that the chip supports all fuse bytes.
'
'        HW:   tr080-R01
'
'
'        http://www.mikrocontroller.net/topic/357240?goto=3996411#3996400
'  Port_io.0 = > Funktions-LED
'  Port_io.1 = > Power-LED
'  Port_io.2 = > LDO enable
'  Port_io.3 = > Taster
'
'  2x EEPROM 24C02
'  ADS112 inaktiv!
'
'  User-Parameter:
'     - Wartezeit zwischen Messungen
'     - run over oder Memory fill
'     - BMA Bereich
'
'  Programm-Parameter
'     - EEPROM Größe
'
'  Historie
'  V2:   läuft
'  V3:   mit seriellem Protokoll zum Programmieren und Daten abrufen
'        EEPROM Größe 2*2k
'  V3.1: EEPROM mit 1024k


' ToDo:
' - Range aus EEPROM lesen und in BMA eintragen
' ------------------------------------------------------------------------------


$Regfile="attiny85.dat"
$Crystal=8000000
$hwstack=50'40
$swstack=32' 16
$framesize=32
$version 3,4,5


' ------------------ Konstanten

   ' I2C Devices
   const adrPCA9536 = 130
   const adrBMA020 = 112
   const adrADS1112 = 144
   const adrEEPROM1 = 160
   const adrEEPROM2 = 162
   Const Timer0_preload = 131' Timer1 tickt im 1ms Takt (siehe Const Timer1_preload Wert)

   const debugmode = 0

   const EEPROM_Groesse = &h7fff
   const EEPROM_Anzahl  = 2   ' zwei EEPROM sind bestückt (logisch aufgeteilt in 2



   const Datenbytes =6 ' Anzahl der zu speichernden Daten in Byte

   const Ausschalten = 65000        ' Ausschaltzeit in ms

' ------------------ Variablen

   dim i as byte,i_word as word,i_integer as  Integer  , j as byte
   dim Messung_Nr as word
   dim HByte as byte, LByte as byte ' Bytes für alles
   dim OnCounter as word
   dim Wartezeit as  Byte, Wartecounter as word,Wartecounter2 as word

   ' BMA020
   dim BMA020_Byte(6) as byte
   dim BMA020_integer(3) as integer  at BMA020_Byte(1) overlay    'Index 1,3,5=> LB,   2.4.6 => HB

   dim Taste_gedrueckt as word   ' Zähler für gedrückte Taste
   dim Port_IO as byte 'lesewert von GPIO

   dim Flagbyte as byte
     Flag_Messung alias flagbyte.0
     Flag_Ausgabe alias flagbyte.1
     Flag_LED_ctrl alias flagbyte.2
     flag_overrun alias flagbyte.3
     Flag_DelayMeasure alias flagbyte.4
     Flag_inWarteschleife alias flagbyte.5


   dim EEPROM_RW_Adresse_Byte(2) as byte
   dim EEPROM_RW_Adresse as  Word at EEPROM_RW_Adresse_Byte(1) overlay
   dim EEPROM_RW_Adresse_ERAM as eram word ' lokales EEPROM aktuelle Adr
   dim EEPROM_RW_AdrStart_ERAM as eram word ' lokales EEPROM letzte MEssung Start

   dim EEPROM_BMA020_Range as eram byte  'speichere BMA-Range, mit der Messung vorgenommen wurde
   dim EEPROM_Wartezeit as eram word      ' Wartezeit

   dim Messpause as word

   ' ####### Testzwecke
   dim testInteger as integer , testByte as byte

' ------------------ Subroutinen deklarieren
   declare sub RWGPIO(byval Commandbyte as byte, Databyte as byte)
   declare sub BMA020_Range(byval Range as byte)
   declare sub BMA020_ACC()
   declare sub RW_EEPROM(Datenwort as integer,byval Aktion as byte)
   declare function BMA020_GetRange() as byte

' ------------------ Deklarationen
     'SDA und SCL definieren
      Config Sda = Portb.0
      Config Scl = Portb.2
      'Config I2cdelay = 250                                    ' Busgeschwindigkeit drosseln, um Signale sicher zu übertragen ~ 1.5kHz
      'Config I2cdelay = 20


   ' Timer
      Config Timer0 = Timer , Prescale = 64
      On Timer0 Timer0_isr

    ' SW UART
      Open "COMB.1:19200,8,N,1" For Output As #1
      Open "COMB.3:19200,8,N,1" For Input As #2


' ------------------ Hauptprogramm

   ' Initialisiere PCA9536
      i = 0:  call RWGPIO(3,i)
      i = 13: call RWGPIO(1,i) ' LDO ein, Taster Pull up,  LED1 ein, LED2 aus

   ' Initialisiere BMA020
      'call BMA020_Range(0)    ' 0 = ±2g    1 = ±4g   2 = ±8g
      i = EEPROM_BMA020_Range
      if i>2 then i=0 ' auf Plausibilität prüfen
      call BMA020_Range(i)

   ' Initialisiere ADS1112


   ' Interrupts einschalten
         enable interrupts
         enable timer0


  do
         ' Bestimme Zeit zwischen zwei Messugen
         if Flag_DelayMeasure = 1 and Flag_inWarteschleife = 0 then 'Messen nur,wenn nicht in Warteschleife
            Flag_DelayMeasure = 0
            Messpause = onCounter         ' OnCounter wurde zurückgesetzt
            'Messpause_ERAM = Messpause
         end if

         ' lese Taste ein
        call rwgpio(0,port_io)
        if port_io.3=0 then  ' Taste gedrückt?
           Taste_gedrueckt = 0 ' ähler zurücksetzen
           OnCounter=0 'Ausschaltzähler zurücksetzten
           while port_io.3=0
               call rwgpio(0,port_io)
               ' warte bis Taste losgelassen wird
           wend

        ' wie lange war Taste gedrückt?
           if Taste_gedrueckt >1000 then
            ' Ausgabe starten
             '  Flag_Ausgabe = 1
           elseif Taste_gedrueckt >50 then
               ' Messung starten/stoppen

               if Flag_Messung = 0 then
                 ' Messungen starten
                 ' Lese AVR EEPROM
                  EEPROM_RW_Adresse = EEPROM_RW_Adresse_ERAM ' hole aus AVR-EEPROM
                  EEPROM_RW_AdrStart_ERAM = EEPROM_RW_Adresse ' sicher Startadresse
                  Flag_LED_ctrl = 1 ' LED modifizieren
                  Messung_Nr=0

                  ' get BMA020 Range and store in µC EEPROM
                  i=  BMA020_GetRange()
                  EEPROM_BMA020_Range = i

                  OnCounter=0:Flag_DelayMeasure=1 ' MEsse Zeit zwischen den ersten beiden Messungen
               else
                 ' MEssungen stoppen
                 EEPROM_RW_Adresse_ERAM = EEPROM_RW_Adresse ' schreibe in AVR-EEPROM
                 Flag_inWarteschleife = 0 ' zurücksetzen

                 ' Messpause in µC-EEPROM speicher
                 EEPROM_Wartezeit=Messpause
               end if

               Flag_Messung = not Flag_Messung
               port_io.0 = not PORT_io.0
              port_io.3=1 ' Schalter enablen
               call rwgpio(1,port_io)
           else

           end if
         end if


     ' Anforderung von serieller Schnittstelle?
     ' keine HW Unterstützung, daher muss solange penetriert werden, bis das Zeichen registriert wird

         if Flag_Messung = 0 then ' nur während Messpause
              i=inkey(#2)
              if i<>0 then
               i = 12: call RWGPIO(1,i) ' LDO ein, Taster Pull up,  LED1 ein, LED2 aus
               print #1,"Seriellmode"
               print #1,"OK"
                OnCounter=0
               while OnCounter<10000
                  i=inkey(#2)
                  if i<>0 then
                     print #1, "gelesen ";chr(i) ;"   i";i
                     OnCounter=0
                     select case i
                        case 82 ' "R"
                           Flag_Ausgabe  =1
                        case 86 '"V"
                           print #1, version(1)
                           print #1, version(2)
                           print #1, version(3)
                        case 97 'a   Beschleunigung ±2g
                           print #1,"ACC=+/-2"
                           print #1,"OK"
                           call BMA020_Range(0)
                        case 98 'b   Beschleunigung ±4g
                           print #1,"ACC=+/-4"
                           print #1,"OK"
                           call BMA020_Range(1)
                        case 99 'c   Beschleunigung ±8g
                           print #1,"ACC=+/-8"
                           print #1,"OK"
                           call BMA020_Range(2)

                        case 100 'd
                           ' read current Range setting
                           j = BMA020_GetRange()
                           print #1, "   Range: "; j
                        case 101 'e    'EEPROM rücksetzen
                           EEPROM_RW_AdrStart_ERAM=0
                           EEPROM_RW_Adresse_ERAM=0
                        case 114 'r wie Wartezeit
                           print #1,"wait=0"
                           print #1,"OK"
                           Wartezeit = 0
                        case 115 's
                           print #1,"wait=0.5ms"
                           print #1,"OK"
                           Wartezeit = 1
                        case 116 't
                           print #1,"wait=1s"
                           print #1,"OK"
                           Wartezeit = 1
                        case 117 'u
                           print #1,"wait=2s"
                           print #1,"OK"
                           Wartezeit = 2
                        case 118 'v
                           print #1,"wait=5s"
                           print #1,"OK"
                           Wartezeit = 5
                        case 119 'w
                           print #1,"wait=10s"
                           print #1,"OK"
                           Wartezeit = 10


                     end  SELECT
                     exit WHILE
                  end if
               wend
              i = 13: call RWGPIO(1,i) ' LDO ein, Taster Pull up,  LED1 ein, LED2 aus
              else
               '
              end if
         end if

     ' Messung
      if Flag_Messung=1 and Flag_inWarteschleife = 0 then
            if flag_overrun = 0 then
               ' Overrun durch Zählen der Einträge
               i_word=EEPROM_Groesse*EEPROM_Anzahl
               i_word=i_word/ Datenbytes
               incr Messung_Nr
               #if debugmode=1
                  print #1,"Messung "; Messung_nr;" von ";i_word
               #endif
               if Messung_nr =i_word then
                  'MEssung ausschalten
                  Flag_Messung=0
                  EEPROM_RW_Adresse_ERAM = EEPROM_RW_Adresse ' schreibe in AVR-EEPROM
                  i = 13: call RWGPIO(1,i) ' LDO ein, Taster Pull up,  LED1 ein, LED2 aus
               end if
           end if

           ' MEssung BMA020
            call BMA020_ACC()
           #if debugmode=1
               print #1,"  x";bma020_integer(1);
               print #1,"  y";bma020_integer(2);
               print #1,"  z";bma020_integer(3);
               print #1,"  Adr:  #### ";EEPROM_RW_Adresse
           #endif

         ' speichern in EEPROM

'            ' doppelt den ersten Eintrag speichern da hier ein Problem liegt => Workaraound
'               call RW_EEPROM(bma020_integer(i),0)
'
           testinteger=999
            call RW_EEPROM(testinteger,0)
            EEPROM_RW_Adresse = EEPROM_RW_Adresse - 2

            for i=1 to 3 'step -1
               'incr testInteger
               'testinteger=1001
                'call RW_EEPROM(testinteger,0)
               call RW_EEPROM(bma020_integer(i),0)
            next


      end if


      ' ---- LED modifizieren ----
         if Flag_LED_ctrl = 1 then
                call rwgpio(0,port_io) 'lese GPIO
                port_io.3=1 'Schalter auf einlesen
                if Flag_Messung =1 then
                        ' LED blinken lassen für Dauer der Messung

                        port_io.0= not port_io.0
                        call rwgpio(1,port_io) 'schreibe GPIO
                else
                        'print #1, "LED still ";port_io
                        port_io.0 = 1

                        'port_io=13              ' LDO ein, Taster Pull up,  LED1 ein, LED2 aus
                        call RWGPIO(1,port_io)
                        Flag_LED_ctrl  = 0
                end if
         end if



     ' ----  Ausgabe
        if Flag_Ausgabe = 1 then
            EEPROM_RW_Adresse = EEPROM_RW_AdrStart_ERAM  ' Lese Startadresse
            i_word = EEPROM_RW_Adresse_ERAM              ' Lese Endadresse

            i = EEPROM_BMA020_Range
            print #1,"/Range: ";i

            Messpause  = EEPROM_Wartezeit
            print #1,"/Messpause: ";Messpause
            print #1,"/Start: ";EEPROM_RW_Adresse
            print #1,"/Ende:  ";i_word



            while i_word <> EEPROM_RW_Adresse
               call rw_eeprom(i_integer,1)
               print #1,i_integer

            wend
            print #1,"/Fertig"
            Flag_Ausgabe = 0

        end if


  ' ----  Wartezeitzeit abarbeiten
      if Flag_Messung = 1 then     ' Wartezeit nur bei aktiver Messung
         if Flag_inWarteschleife = 0 then
            Wartecounter  =0        ' Counter zurücksetzten, wenn Einstieg in Warteschleife
         end if


         if wartezeit=0 then         ' Wartezeit bestimmen und Flag setzen
            ' nicht warten
            Wartecounter2  = 0
         elseif wartezeit = 1 then
            Wartecounter2  =500
         else
            Wartecounter2 = wartezeit * 1000
         end if

         if Wartecounter < Wartecounter2 then
            Flag_inWarteschleife  =1
         else
            Flag_inWarteschleife  =0
         end if



      end if


  loop




end




' ------------------ Subroutinen
   sub RW_EEPROM(Datenwort as integer,byval Aktion as byte)
   ' schreibe 2Byte (Word) in's externe EEPROM
      local adrEEPROM_local as byte
      local j_word as word
      local Datenbyte as  Byte



      incr EEPROM_RW_Adresse              ' Speicherstelle hochzählen und begrenzen
      j_word = EEPROM_Anzahl*EEPROM_Groesse
      if EEPROM_RW_Adresse > j_word then
         EEPROM_RW_Adresse = 0
      end if

      if high(EEPROM_RW_Adresse)>0 then  ' prüfe welches EEPROM
         adrEEPROM_local = adrEEPROM1
      else
         adrEEPROM_local=  adrEEPROM2
      end if

      if Aktion  = 0 then ' schreiben
         'waitms 15
         i2cstart
         i2cwbyte adrEEPROM_local            ' EEPROM adresse
         i2cwbyte EEPROM_RW_Adresse_Byte(2)  'Speicherstelle  HB
         i2cwbyte EEPROM_RW_Adresse_Byte(1)  'Speicherstelle  LB
         'waitms 5
         Datenbyte = high(Datenwort)
         i2cwbyte Datenbyte             ' Daten HighByte
         'waitms 5
         Datenbyte = low(Datenwort)
         i2cwbyte Datenbyte            ' Daten LowByte
         'waitms 5
         i2cstop
         waitms 10   ' warten - ist das nötig ????????????????????????????????

      else     'lesen
         i2cstart
         i2cwbyte adrEEPROM_local            ' EEPROM adresse
         i2cwbyte EEPROM_RW_Adresse_Byte(2)  'Speicherstelle  HB
         i2cwbyte EEPROM_RW_Adresse_Byte(1)  'Speicherstelle  LB

         adrEEPROM_local = adrEEPROM_local +1
         i2cstart
         i2cwbyte adrEEPROM_local            ' EEPROM adresse  READ
   'waitms 5
         i2crbyte Datenbyte,ack
         j_word=Datenbyte
         shift j_word,left,8
         i2crbyte Datenbyte,nack
         j_word  =j_word + Datenbyte
         i2cstop

         Datenwort = j_word


      end if

      ' Adresse nachziehen
      incr EEPROM_RW_Adresse              ' Speicherstelle hochzählen und begrenzen
      j_word = EEPROM_Anzahl*EEPROM_Groesse
      if EEPROM_RW_Adresse > j_word then
         EEPROM_RW_Adresse = 0
      end if


   end sub

   sub RWGPIO(byval Commandbyte as byte,  Databyte as byte)
      ' control GPIO from PCA9536

      local adrPCA9536_R as byte
      adrPCA9536_R = adrPCA9536+1
      if Commandbyte>0 then ' nicht lesen
         'I2CInit
         i2CStart
         I2cwbyte adrPCA9536
         I2Cwbyte Commandbyte
         I2Cwbyte Databyte
         I2CStop
         'print #1, "schreibe GPIO ;";Databyte
      else
         I2CStart
         I2Cwbyte adrPCA9536
         I2Cwbyte Commandbyte

         I2CStart
         I2Cwbyte adrPCA9536_R
         i2crbyte Databyte,ack
         I2CStop

        Databyte= Databyte and &hf ' die die vier LSB sind relevant
      end if

   end sub


   sub BMA020_Range(byval Range as byte)
      ' define new Range in BMA020
      ' first read the &h14 as Bit 7,6,5 must not be changed!!
      local adrBMA020_r as byte,myRange as byte,i_sub as byte
      adrBMA020_r=adrBMA020+1

       I2CStart
       I2cwbyte adrBMA020
       I2cwbyte &h14 'Datenregister
       I2CStop

       I2CStart
       I2cwbyte adrBMA020_r
       i2crbyte myRange,nack
       I2CStop

       ' now modify and rewrite
      myRange = myRange and &hE7 ' Bit 3/4 ausmaskieren
      i_sub = Range
      shift i_sub,left,3
      myrange = myrange or i_sub


      I2CStart
      I2cwbyte adrBMA020
      I2cwbyte &h14
      i2cwbyte myrange
      I2CStop

   end sub

   function BMA020_GetRange() as byte
      local adrBMA020_r as byte,i_sub as byte
      adrBMA020_r = adrBMA020 + 1

       I2CStart
       I2cwbyte adrBMA020
       I2cwbyte &h14 'Datenregister
       I2CStop

       I2CStart
       I2cwbyte adrBMA020_r
       i2crbyte i_sub,nack
       I2CStop

       i_sub = i_sub AND &h18
       shift i_sub,right,3

       BMA020_GetRange = i_sub

   end function


   sub BMA020_ACC()
      local adrBMA020_r as byte,i_sub as byte, j_sub as byte

      adrBMA020_r=adrBMA020+1

       I2CStart
       I2cwbyte adrBMA020
       I2cwbyte 2 'Datenregister
       I2CStop

       I2CStart
       I2cwbyte adrBMA020_r
       For i_sub = 1 To 6
            i2crbyte BMA020_Byte(i_sub),ack
       Next
       I2CStop

       for i_sub=1 to 3 ' richtig verschieben
         shift bma020_integer(i_sub),right,6
       next



   end sub


Timer0_isr:
   ' Timer1 wieder neu vorladen
   Timer0 = Timer0_preload
   ' Wartecounter
   incr Wartecounter
   ' automatisch abschalten
   incr OnCounter
   if OnCounter > Ausschalten then
      if Flag_Messung = 0 then ' nur wenn keine Messung läuft
         ' Spannung aus
         i=8: call RWGPIO(1,i) ' LDO ein, Taster Pull up,  LED1 ein, LED2 aus
      end if
   end if

   ' Taste gedrückt -> Counter
      if port_io.3=0 then
         incr Taste_gedrueckt
      end if


   return