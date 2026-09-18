       IDENTIFICATION DIVISION.
       PROGRAM-ID. DSIM_TP05_EX02.
       AUTHOR. ADRIANO JUNIOR (CB3030644),
               ARTHUR LANZILOTTI (CB3031306),
               KAIK PERSIKE (CB3029689),
               LUIZ GUSTAVO (CB3030326).

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-ENTRADA ASSIGN TO "COTAHIST_D15092026.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-ENTRADA.
           SELECT ARQ-ON ASSIGN TO "HIST_ON_15092026.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARQ-PN ASSIGN TO "HIST_PN_15092026.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARQ-BDR ASSIGN TO "HIST_BDR_15092026.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-ENTRADA.
       01 REG-ENTRADA.
           05 CH-TIPREG       PIC X(02).
           05 FILLER          PIC X(10).
           05 CH-CODNEG       PIC X(12).
           05 FILLER          PIC X(03).
           05 CH-NOMRES       PIC X(12).
           05 CH-ESPECI       PIC X(10).
           05 FILLER          PIC X(07).
           05 CH-PREABE       PIC X(13).
           05 CH-PREMAX       PIC X(13).
           05 CH-PREMIN       PIC X(13).
           05 FILLER          PIC X(13).
           05 CH-PREULT       PIC X(13).
           05 FILLER          PIC X(134).

       FD ARQ-ON.
       01 REG-ON              PIC X(79).

       FD ARQ-PN.
       01 REG-PN              PIC X(79).

       FD ARQ-BDR.
       01 REG-BDR             PIC X(79).

       WORKING-STORAGE SECTION.
       01 WS-FS-ENTRADA       PIC XX.
       01 WS-EOF              PIC X VALUE 'N'.

       01 WS-COUNTERS.
           05 WS-CONT-ON      PIC 9(09) VALUE ZEROS.
           05 WS-CONT-PN      PIC 9(09) VALUE ZEROS.
           05 WS-CONT-BDR     PIC 9(09) VALUE ZEROS.

       01 WS-SYS-DATE-TIME.
           05 WS-DATE         PIC X(08).
           05 WS-TIME         PIC X(08).

       01 WS-HEADER.
           05 WS-H-TIPO       PIC X(03) VALUE "001".
           05 WS-H-ARQ-NUM    PIC X(02).
           05 WS-H-DATA       PIC X(08).
           05 WS-H-HORA       PIC X(08).
           05 WS-H-NOME       PIC X(30)
           VALUE
           "ARTHUR LANZILOTTI, ADRIANO JUNIOR, KAIK PERSIKE".
           05 FILLER          PIC X(28) VALUE SPACES.

       01 WS-DETAIL.
           05 WS-D-TIPO       PIC X(03) VALUE "101".
           05 WS-D-CODNEG     PIC X(12).
           05 WS-D-NOMRES     PIC X(12).
           05 WS-D-PREABE     PIC X(13).
           05 WS-D-PREMAX     PIC X(13).
           05 WS-D-PREMIN     PIC X(13).
           05 WS-D-PREULT     PIC X(13).

       01 WS-TRAILER.
           05 WS-T-TIPO       PIC X(03) VALUE "901".
           05 WS-T-ARQ-NUM    PIC X(02).
           05 WS-T-DATA       PIC X(08).
           05 WS-T-HORA       PIC X(08).
           05 WS-T-TOTAL      PIC 9(09).
           05 FILLER          PIC X(49) VALUE SPACES.

       PROCEDURE DIVISION.
       INICIO.
           ACCEPT WS-DATE FROM DATE YYYYMMDD.
           ACCEPT WS-TIME FROM TIME.
           OPEN INPUT ARQ-ENTRADA.
           OPEN OUTPUT ARQ-ON ARQ-PN ARQ-BDR.
           PERFORM 100-GRAVA-HEADERS.
           READ ARQ-ENTRADA
               AT END MOVE 'Y' TO WS-EOF
           END-READ.
           PERFORM 200-PROCESSA-REGISTROS UNTIL WS-EOF = 'Y'.
           PERFORM 300-GRAVA-TRAILERS.
           CLOSE ARQ-ENTRADA ARQ-ON ARQ-PN ARQ-BDR.
           STOP RUN.

       100-GRAVA-HEADERS.
           MOVE WS-DATE TO WS-H-DATA.
           MOVE WS-TIME TO WS-H-HORA.
           MOVE "11" TO WS-H-ARQ-NUM.
           WRITE REG-ON FROM WS-HEADER.
           MOVE "12" TO WS-H-ARQ-NUM.
           WRITE REG-PN FROM WS-HEADER.
           MOVE "13" TO WS-H-ARQ-NUM.
           WRITE REG-BDR FROM WS-HEADER.

       200-PROCESSA-REGISTROS.
           IF CH-TIPREG = "01"
               MOVE CH-CODNEG TO WS-D-CODNEG
               MOVE CH-NOMRES TO WS-D-NOMRES
               MOVE CH-PREABE TO WS-D-PREABE
               MOVE CH-PREMAX TO WS-D-PREMAX
               MOVE CH-PREMIN TO WS-D-PREMIN
               MOVE CH-PREULT TO WS-D-PREULT
               EVALUATE TRUE
                   WHEN CH-ESPECI(1:2) = "ON"
                       WRITE REG-ON FROM WS-DETAIL
                       ADD 1 TO WS-CONT-ON
                   WHEN CH-ESPECI(1:2) = "PN"
                       WRITE REG-PN FROM WS-DETAIL
                       ADD 1 TO WS-CONT-PN
                   WHEN CH-ESPECI(1:3) = "BDR" OR CH-ESPECI(1:2) = "DR"
                       WRITE REG-BDR FROM WS-DETAIL
                       ADD 1 TO WS-CONT-BDR
               END-EVALUATE
           END-IF.
           READ ARQ-ENTRADA
               AT END MOVE 'Y' TO WS-EOF
           END-READ.

       300-GRAVA-TRAILERS.
           ACCEPT WS-DATE FROM DATE YYYYMMDD.
           ACCEPT WS-TIME FROM TIME.
           MOVE WS-DATE TO WS-T-DATA.
           MOVE WS-TIME TO WS-T-HORA.
           MOVE "91" TO WS-T-ARQ-NUM.
           MOVE WS-CONT-ON TO WS-T-TOTAL.
           WRITE REG-ON FROM WS-TRAILER.
           MOVE "92" TO WS-T-ARQ-NUM.
           MOVE WS-CONT-PN TO WS-T-TOTAL.
           WRITE REG-PN FROM WS-TRAILER.
           MOVE "93" TO WS-T-ARQ-NUM.
           MOVE WS-CONT-BDR TO WS-T-TOTAL.
           WRITE REG-BDR FROM WS-TRAILER.
