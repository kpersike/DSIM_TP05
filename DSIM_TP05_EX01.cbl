       IDENTIFICATION DIVISION.
       PROGRAM-ID. DSIM_TP05_EX01.
       AUTHOR. ADRIANO JUNIOR (CB3030644),
               ARTHUR LANZILOTTI (CB3031306),
               KAIK PERSIKE (CB3029689),
               LUIZ GUSTAVO (CB3030326).

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-FUNC ASSIGN TO "FUNCIONARIOS.DAT"
               ORGANIZATION IS RECORD SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-FUNC
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 62 CHARACTERS
           DATA RECORD IS REG-FUNC.
       01 REG-FUNC.
           05 FL-CODIGO       PIC 9(05).
           05 FL-NOME         PIC X(30).
           05 FL-DEPTO        PIC X(20).
           05 FL-SALARIO      PIC 9(05)V99.

       WORKING-STORAGE SECTION.
       77 WS-STATUS           PIC X(02) VALUE "00".
       77 WS-QTD              PIC 9(03) VALUE 0.
       77 WS-CONT             PIC 9(03) VALUE 0.
       77 WS-EOF              PIC X     VALUE "N".

      *> Acumuladores e totalizadores solicitados
       77 WS-TOTAL-FUNC       PIC 9(03) VALUE 0.
       77 WS-SOMA-SALARIOS    PIC 9(08)V99 VALUE 0.
       77 WS-MEDIA-SALARIO    PIC 9(05)V99 VALUE 0.
       77 WS-MAIOR-SALARIO    PIC 9(05)V99 VALUE 0.

      *> Campos de apoio para a tela de cadastro
       01 REG-ENTRADA.
           05 W-CODIGO        PIC 9(05) VALUE ZEROS.
           05 W-NOME          PIC X(30) VALUE SPACES.
           05 W-DEPTO         PIC X(20) VALUE SPACES.
           05 W-SALARIO       PIC 9(05)V99 VALUE ZEROS.

       SCREEN SECTION.
       01 TELA-LIMPA.
           05 BLANK SCREEN.

       01 TELA-QTD.
           05 LINE 05 COL 10 VALUE "=== CADASTRO DE FUNCIONARIOS ===".
           05 LINE 08 COL 10 VALUE "Quantidade de funcionarios: ".
           05 LINE 08 COL 38 PIC 9(03) USING WS-QTD.

       01 TELA-CADASTRO.
           05 LINE 02 COL 10 VALUE "=== DIGITACAO DE DADOS ===".
           05 LINE 05 COL 10 VALUE "Codigo (5 digitos)..: ".
           05 LINE 05 COL 32 PIC 9(05) USING W-CODIGO.
           05 LINE 07 COL 10 VALUE "Nome (ate 30 chars).: ".
           05 LINE 07 COL 32 PIC X(30) USING W-NOME.
           05 LINE 09 COL 10 VALUE "Departamento........: ".
           05 LINE 09 COL 32 PIC X(20) USING W-DEPTO.
           05 LINE 11 COL 10 VALUE "Salario (99999,99)...: ".
           05 LINE 11 COL 32 PIC 9(05)V99 USING W-SALARIO.

       01 TELA-EXIBE-REG.
           05 LINE 03 COL 10 VALUE "=== CONSULTA DE FUNCIONARIOS ===".
           05 LINE 06 COL 10 VALUE "Codigo........: ".
           05 LINE 06 COL 26 PIC 9(05) FROM FL-CODIGO.
           05 LINE 08 COL 10 VALUE "Nome..........: ".
           05 LINE 08 COL 26 PIC X(30) FROM FL-NOME.
           05 LINE 10 COL 10 VALUE "Departamento..: ".
           05 LINE 10 COL 26 PIC X(20) FROM FL-DEPTO.
           05 LINE 12 COL 10 VALUE "Salario.......: ".
           05 LINE 12 COL 26 PIC 9(05)V99 FROM FL-SALARIO.
           05 LINE 15 COL 10
           VALUE "Pressione [ENTER] para o proximo...".

       01 TELA-RESUMO.
           05 LINE 03 COL 10
           VALUE "=== RESUMO GERAL DOS FUNCIONARIOS ===".
           05 LINE 06 COL 10
           VALUE "Total de funcionarios cadastrados.: ".
           05 LINE 06 COL 48 PIC 9(03) FROM WS-TOTAL-FUNC.
           05 LINE 08 COL 10
           VALUE "Soma total dos salarios...........: R$ ".
           05 LINE 08 COL 48 PIC Z(07)9,99 FROM WS-SOMA-SALARIOS.
           05 LINE 10 COL 10
           VALUE "Salario medio.....................: R$ ".
           05 LINE 10 COL 48 PIC Z(05)9,99 FROM WS-MEDIA-SALARIO.
           05 LINE 12 COL 10
           VALUE "Maior salario encontrado..........: R$ ".
           05 LINE 12 COL 48 PIC Z(05)9,99 FROM WS-MAIOR-SALARIO.
           05 LINE 16 COL 10
           VALUE "Fim do programa. Pressione ENTER para sair.".

       PROCEDURE DIVISION.
       INICIO-PROGRAMA.
           PERFORM ETAPA-GRAVACAO.
           PERFORM ETAPA-LEITURA.
           STOP RUN.

       ETAPA-GRAVACAO.
           DISPLAY TELA-LIMPA.
           DISPLAY TELA-QTD.
           ACCEPT TELA-QTD.

           OPEN OUTPUT ARQ-FUNC.
           IF WS-STATUS NOT EQUAL "00"
               DISPLAY "Erro ao criar o arquivo. Status: " WS-STATUS
               STOP RUN
           END-IF.

           PERFORM VARYING WS-CONT FROM 1 BY 1 UNTIL WS-CONT > WS-QTD
               DISPLAY TELA-LIMPA
               DISPLAY TELA-CADASTRO
               ACCEPT TELA-CADASTRO

               *> Move os dados digitados na tela para o registro do arquivo
               MOVE W-CODIGO  TO FL-CODIGO
               MOVE W-NOME    TO FL-NOME
               MOVE W-DEPTO   TO FL-DEPTO
               MOVE W-SALARIO TO FL-SALARIO

               WRITE REG-FUNC
               IF WS-STATUS NOT EQUAL "00"
                   DISPLAY "Erro ao gravar registro. Status: " WS-STATUS
               END-IF
           END-PERFORM.

           CLOSE ARQ-FUNC.

       ETAPA-LEITURA.
           OPEN INPUT ARQ-FUNC.
           IF WS-STATUS NOT EQUAL "00"
               DISPLAY "Erro ao abrir o arquivo para leitura. Status: " WS-STATUS
               STOP RUN
           END-IF.

           MOVE "N" TO WS-EOF.

           PERFORM UNTIL WS-EOF EQUAL "S"
               READ ARQ-FUNC
                   AT END
                       MOVE "S" TO WS-EOF
                   NOT AT END
      *> Cálculos realizados durante a leitura (conforme exigência)
                       ADD 1 TO WS-TOTAL-FUNC
                       ADD FL-SALARIO TO WS-SOMA-SALARIOS

                       IF FL-SALARIO > WS-MAIOR-SALARIO
                           MOVE FL-SALARIO TO WS-MAIOR-SALARIO
                       END-IF

                       *> Exibição do registro na tela de saída
                       DISPLAY TELA-LIMPA
                       DISPLAY TELA-EXIBE-REG
                       ACCEPT W-CODIGO  *> Apenas para pausar a tela aguardando ENTER
               END-READ
           END-PERFORM.

           CLOSE ARQ-FUNC.

      *> Cálculo da média salarial após ler todos os registros
           IF WS-TOTAL-FUNC > 0
               COMPUTE
               WS-MEDIA-SALARIO = WS-SOMA-SALARIOS / WS-TOTAL-FUNC
           END-IF.

           *> Apresentação da tela de resumo final
           DISPLAY TELA-LIMPA
           DISPLAY TELA-RESUMO
           ACCEPT W-CODIGO.
