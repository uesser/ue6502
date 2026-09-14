# Spezifikation: Bidirektionales Parallel-Interface (65C02 SBC -> ESP32/FabGL)

## 1. Systemübersicht & Design-Philosophie
Das Ziel ist die Anbindung des **Olimex ESP32-SBC-FabGL** als intelligente Grafikkarte und Tastatur-Interface an einen **65C02 Einplatinencomputer (SBC)**. 
Um Ressourcen für zukünftige Erweiterungen (z. B. SPI, Sound oder Massenspeicher) zu schonen, bleibt **Port B des VIA 65C22 zu 100 % unberührt**. Das gesamte Interface wird über **einen einzigen 8-Bit-Port (Port A)** und die beiden systemeigenen Handshake-Pins **CA1 und CA2** abgewickelt.

### Die Master-Slave-Vorfahrtsregel
* Der Bus befindet sich zu 99 % der Zeit im **Grafik-Ausgabemodus** (VIA-Port A ist Ausgang, ESP32-Pins sind Eingang/hochohmig).
* Der ESP32 darf **niemals unaufgefordert** Daten auf den Bus legen. Er meldet Tastaturevents asynchron über einen Interrupt.
* Der VIA schaltet seine Datenleitungen nur für die Dauer der Interrupt-Service-Routine (ISR) auf Eingang um, holt sich den Scancode und schaltet sofort wieder auf Ausgang zurück. Bus-Kollisionen sind dadurch physikalisch ausgeschlossen.

---

## 2. Hardware-Architektur & Verkabelung

### Elektrische Pegelwarnung (5V vs. 3,3V)
Der 65C02 und der VIA 65C22 arbeiten mit **5V-Logik**, das Olimex-Board (ESP32) strikt mit **3,3V-Logik**. 
* **Datenbus & CA2:** Ein direkte Anschluss würde den ESP32 zerstören bzw. der CMOS-VIA (`W65C22N`) erkennt die 3,3V des ESP32 nicht zuverlässig als High. Es wird zwingend ein **8-Kanal bidirektionaler Pegelwandler (TXS0108E)** dazwischengeschaltet.
* **CA1 (Tastatur-IRQ):** Läuft ebenfalls über den Pegelwandler.
* **VCC-Trennung:** Die Stromschienen (5V des SBC und 3,3V des Olimex) dürfen **nicht** miteinander verbunden werden. Nur die Masse (**GND**) muss gekoppelt werden.

### Pin-Belegungsmatrix

| Signal | Richtung | VIA 65C22 Pin | Pegelwandler (TXS0108E) | Olimex ESP32-SBC-FabGL Pin |
| :--- | :---: | :---: | :---: | :--- |
| **D0** | Bidirektional | **PA0** | Kanal 1 (B -> A) | `LCD_CON1` Pin 3 (**GPIO 5**) |
| **D1** | Bidirektional | **PA1** | Kanal 2 (B -> A) | `LCD_CON1` Pin 4 (**GPIO 22**) |
| **D2** | Bidirektional | **PA2** | Kanal 3 (B -> A) | `LCD_CON1` Pin 5 (**GPIO 18**) |
| **D3** | Bidirektional | **PA3** | Kanal 4 (B -> A) | `LCD_CON1` Pin 6 (**GPIO 23**) |
| **D4** | Bidirektional | **PA4** | Kanal 5 (B -> A) | `LCD_CON1` Pin 7 (**GPIO 19**) |
| **D5** | Bidirektional | **PA5** | Kanal 6 (B -> A) | `LCD_CON1` Pin 8 (**GPIO 21**) |
| **D6** | Bidirektional | **PA6** | Kanal 7 (B -> A) | `LCD_CON1` Pin 9 (**GPIO 15**) |
| **D7** | Bidirektional | **PA7** | Kanal 8 (B -> A) | `LCD_CON1` Pin 10 (**GPIO 4**) |
| **Strobe / Ack** | VIA ➔ ESP32 | **CA2** | *Separater Kanal / Modul* | `Access_Bus1` Pin 5 (**GPIO 2**) |
| **Tastatur-IRQ** | ESP32 ➔ VIA | **CA1** | *Separater Kanal / Modul* | `Access_Bus1` Pin 4 (**GPIO 33**) |
| **Masse** | Gemeinsam | **GND** | **GND** (Zentraler Sternpunkt) | `LCD_CON1` Pin 2 & Pin 11 (**GND**) |

*Hinweis zum Wandler-Modul:* Der `OE` (Output Enable) Pin des TXS0108E wird fest auf die 3,3V-Seite (`VCCA`) gelegt, damit der Wandler dauerhaft aktiv ist.

---

## 3. Das Handshake-Protokoll (CA1 & CA2)

Wir nutzen die nativen, historisch exakt so vorgesehenen Fähigkeiten des VIA 65C22 voll aus.

### CA2 – Der universelle Ausgabe-Strobe (Pulse Mode)
Der VIA wird im Peripheral Control Register (`VIA_PCR`) so konfiguriert, dass **CA2 im automatischen Puls-Modus** läuft. 
* **Der Effekt:** Jedes Mal, wenn die 6502-CPU die Registeradresse `VIA_PORTA` anspricht (egal ob mit `STA` zum Schreiben oder `LDA` zum Lesen), zieht der VIA den Pin **CA2 im nächsten Taktzyklus vollautomatisch für eine Periode auf Low** und danach wieder auf High.
* Dies dient dem ESP32 sowohl als **Grafik-Strobe** (Daten bereit) als auch als **Tastatur-Acknowledge** (Daten abgeholt).

### CA1 – Der asynchrone Eingabe-Interrupt (Tastatur)
* **Der Effekt:** Reagiert im VIA standardmäßig auf die fallende Flanke (High-to-Low). Wenn der ESP32 eine Taste meldet, zieht er CA1 auf Low. Der VIA löst sofort den CPU-Interrupt aus.

---

## 4. Software-Spezifikation (65C02 Assembler)

### Kernel-Funktion: `_send_graficdata`
Da der ESP32 (240 MHz) Befehle rasend schnell in einen internen RAM-Puffer (FIFO) einliest und im Hintergrund verarbeitet, ist das Risiko eines "Busy" beim reinen Datentransfer gegen Null reduziert. Die Funktion benötigt keine Warteschleife und ist durch `SEI`/`CLI` **atomar (ununterbrechbar)** gekapselt, um Race-Conditions mit dem Tastatur-Interrupt auszuschließen.

```assembly
; =============================================================================
; KERNEL FUNKTION: _send_graficdata
; Eingang:        A = Das zu sendende Daten-Byte (Grafikbefehl/Koordinate)
; Vorbedingung:   VIA_DDRA steht standardmäßig auf Ausgang (\$FF)
; =============================================================================
_send_graficdata:
    SEI                 ; --- START ATOMARE PHASE ---
                        ; Verhindert, dass die ISR den Bus mitten im Senden auf Eingang schaltet

    STA VIA_PORTA       ; Daten auf den Bus legen.
                        ; TRIGGERT AUTOMATISCH DEN CA2-PULS ALS GRAFIK-STROBE!

    CLI                 ; --- ENDE ATOMARE PHASE ---
                        ; Interrupts wieder freigeben.

    RTS
```

### Interrupt-Service-Routine: `tastatur_handler`
Das Tastatur-Format wurde auf ein **hocheffizientes 7-Bit + High-Bit-Format** optimiert. Es gibt maximal 114 Tasten. 
* Bit 7 = `0`: Taste gedrückt (Make) -> Scancode-Bereich `0x00`–`0x7F`
* Bit 7 = `1`: Taste losgelassen (Break) -> Scancode-Bereich `0x80`–`0xFF`
Dadurch entfallen komplexe Multi-Byte-Auswertungen (wie das PS/2-typische `0xF0`) auf dem 6502 komplett.

```assembly
; =============================================================================
; INTERRUPT SERVICE ROUTINE: tastatur_handler
; =============================================================================
tastatur_handler:
    PHA                 ; Arbeitsregister auf dem Stack sichern
    PHX                 ; (Das Status/Flag-Register P sichert die 
    PHY                 ;  65C02-Hardware bereits vollautomatisch!)

    LDA VIA_IFR         ; Interrupt Flag Register prüfen
    AND #\$02            ; Bit 1 = CA1 (Tastatur-IRQ)
    BEQ .not_keyboard   ; Wenn 0, war es eine andere IRQ-Quelle

    ; 1. Bus-Richtung auf EINGANG umschalten
    LDA #\$00
    STA VIA_DDRA       

    ; 2. Daten direkt einlesen
    ; TRIGGERT AUTOMATISCH DEN CA2-PULS ALS LESEQUITTUNG (ACK) AN DEN ESP32!
    ; Durch das Lesen von PORTA löscht der VIA zudem das CA1-Flag im IFR selbstständig.
    LDA VIA_PORTA       ; A enthält nun den Scancode (Bit 7 = Up/Down)

    ; 3. Daten in den System-RAM-Puffer schieben
    JSR _push_to_key_buffer

    ; 4. Bus-Richtung sofort wieder als AUSGANG für Grafik sperren
    LDA #\$FF
    STA VIA_DDRA       

.not_keyboard:
    PLY                 ; Register wiederherstellen
    PLX                 
    PLA                 
    RTI                 ; Rücksprung (stellt auch das Flag-Register P wieder her)
```

---

## 5. Software-Logik auf der ESP32-Seite (C++)

Der ESP32 nutzt seine Dual-Core-Architektur. Während Core 0 die Grafikgenerierung und den USB-Host-Dongle verwaltet, lauscht Core 1 auf dem Parallelbus.

### Der asynchrone Verriegelungs-Trick
Um zu verhindern, dass der ESP32 die Leitungen zu früh freigibt (während der 6502 noch auf dem Weg in die ISR ist), **zwingen wir den ESP32 zum Warten**:
1. Bei Tastendruck schaltet der ESP32 seine Daten-Pins auf `OUTPUT`, legt den Scancode an und zieht **CA1 auf Low**.
2. Er blockiert in einer C++ Schleife so lange, bis er über seinen Hardware-Interrupt sieht, dass **CA2 auf Low gezuckt ist** (Beweis, dass der 6502 `LDA VIA_PORTA` ausgeführt hat).
3. Erst nach dieser Quittung gibt der ESP32 den Bus wieder frei (Pins auf `INPUT`) und setzt **CA1 zurück auf High**.
4. Ein integrierter Software-Timeout (z. B. 5 ms) im ESP32 schützt das System vor einem Hard-Lock, falls am 6502 die Interrupts permanent gesperrt sein sollten.
