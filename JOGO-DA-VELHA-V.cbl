      ******************************************************************
      * Author: Erika Tavares Fernandes
      * Date: 02/12/2018
      * Purpose: Jogo da velha para estudo.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JOGO-DA-VELHA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT BLOQUEIO ASSIGN TO 'bloqueio.txt'
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT ATAQUE ASSIGN TO 'ataque.txt'
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD BLOQUEIO.
       01 BLOQUEIO-ARQUIVO.
           05 LINHA    PIC 9(1).
           05 CELULA-1 PIC 9(1).
           05 CELULA-2 PIC 9(1).
           05 CELULA-3 PIC 9(1).
           05 I        PIC 9(1).

       FD ATAQUE.
       01 ATAQUE-ARQUIVO.
           05 LINHA-A    PIC 9(1).
           05 CELULA-1-A PIC 9(1).
           05 CELULA-2-A PIC 9(1).
           05 CELULA-3-A PIC 9(1).
           05 I-A        PIC 9(1).


       WORKING-STORAGE SECTION.
    *********** JOGO -----------------------------------------------
       01 JOGO-DA-VELHA-TABELA.
           05 CELULA00 PIC X(1).
           05 CELULA01 PIC X(1).
           05 CELULA02 PIC X(1).
           05 CELULA10 PIC X(1).
           05 CELULA11 PIC X(1).
           05 CELULA12 PIC X(1).
           05 CELULA20 PIC X(1).
           05 CELULA21 PIC X(1).
           05 CELULA22 PIC X(1).

       01 USER-PLAYER        PIC X(1).
       01 COBOL-PLAYER       PIC X(1).
       01 V-COBOL-JOGOU      PIC X(1) VALUE "F".
       01 V-USER-JOGOU       PIC X(1) VALUE "F".
       01 PLAYER-AUX         PIC X(1).

       01 JOGADOR-ESCOLHIDO  PIC X(1) VALUE "F".

       01 COORDENADA         PIC 9(2).

    ***     -- QUANDO COBOL JOGAR.
       01 EXISTE_ATAQUE      PIC X(1) VALUE "V".
       01 EXISTE_BLOQUEIO    PIC X(1) VALUE "V".

    ***     -- PARA IMITAR MAIS JOGADAS.
       01 AUX-COORD-M-JOGADAS   PIC 9(3).
       01 CONTA-COORD-M-JOGADAS PIC 9 OCCURS 9 TIMES INDEXED BY I-CJ.
       01 AUX-I-CJ              PIC 9(1).
       01 ULTIMA-I-CJ           PIC 9(1).
       01 CONTA-VOLTA           PIC 9(1) VALUE 0.

       01 CELULAS-OCUPADAS PIC X(1) OCCURS 9 TIMES INDEXED BY IND-CO.
       01 AUX-CEL-OP       PIC 9(1).

       01 QUANT-JOGO                 PIC 9(1) VALUE 0.
       01 PERGUNTA-FINAL             PIC 9(1).
       01 RESULTADO-RANDOM-QM-INICIA PIC 99.

    ***     -- PARA VERIFICAR FIM DE JOGO
       01 VENCEU      PIC X(1) VALUE "F".
       01 FIM-DE-JOGO PIC 9(1).

       01 PLACAR-USUARIO PIC 9(2).
       01 PLACAR-COBOL   PIC 9(2).

    ************* BLOQUEIO --------------------------------------------
       01 WS-BLOQUEIO OCCURS 3 TIMES INDEXED BY WS-NUM-LINHA.
           05 WS-LINHA    PIC 9(1).
           05 WS-CELULA-1 PIC 9(1).
           05 WS-CELULA-2 PIC 9(1).
           05 WS-CELULA-3 PIC 9(1).
           05 WS-I        PIC 9(1).
       01 WS-EOF          PIC A(1).
       01 WS-LINHA-AUX    PIC S9(1) VALUE 0.
       01 CONTADOR-BLOQUEIO-LINHA-1    PIC 9(1) VALUE 0.
       01 CONTADOR-BLOQUEIO-LINHA-2    PIC 9(1) VALUE 0.
       01 CONTADOR-BLOQUEIO-LINHA-3    PIC 9(1) VALUE 0.
       01 CONTADOR-BLOQUEIO-COLUNA-1   PIC 9(1) VALUE 0.
       01 CONTADOR-BLOQUEIO-COLUNA-2   PIC 9(1) VALUE 0.
       01 CONTADOR-BLOQUEIO-COLUNA-3   PIC 9(1) VALUE 0.
       01 CONTADOR-BLOQUEIO-DIAGONAL-1 PIC 9(1) VALUE 0.
       01 CONTADOR-BLOQUEIO-DIAGONAL-2 PIC 9(1) VALUE 0.

       01 MARCADO-LINHA1    PIC X(1) VALUE "F".
       01 MARCADO-LINHA2    PIC X(1) VALUE "F".
       01 MARCADO-LINHA3    PIC X(1) VALUE "F".
       01 MARCADO-COLUNA1   PIC X(1) VALUE "F".
       01 MARCADO-COLUNA2   PIC X(1) VALUE "F".
       01 MARCADO-COLUNA3   PIC X(1) VALUE "F".
       01 MARCADO-DIAGONAL1 PIC X(1) VALUE "F".
       01 MARCADO-DIAGONAL2 PIC X(1) VALUE "F".

       01 SINAL-LINHA-1    PIC S9(2).
       01 SINAL-LINHA-2    PIC S9(2).
       01 SINAL-LINHA-3    PIC S9(2).

       01 SINAL-COLUNA-1   PIC S9(2).
       01 SINAL-COLUNA-2   PIC S9(2).
       01 SINAL-COLUNA-3   PIC S9(2).

       01 SINAL-DIAGONAL-1 PIC S9(2).
       01 SINAL-DIAGONAL-2 PIC S9(2).

       01 CONTA-I PIC 9(1) VALUE 1.

       01 BLOQUEIO-LISTA OCCURS 20000 TIMES INDEXED BY NUM-LINHA-LISTA.
           05 LINHA-LISTA    PIC 9(1).
           05 CELULA-1-LISTA PIC 9(1).
           05 CELULA-2-LISTA PIC 9(1).
           05 CELULA-3-LISTA PIC 9(1).
           05 I-LISTA        PIC 9(1).
       01 BLOQUEIO-LISTA-AUX PIC 9(5) VALUE 0.


 *********** ATAQUE --------------------------------------------

       01 WS-ATAQUE OCCURS 3 TIMES INDEXED BY WS-NUM-LINHA-A.
           05 WS-LINHA-A    PIC 9(1).
           05 WS-CELULA-1-A PIC 9(1).
           05 WS-CELULA-2-A PIC 9(1).
           05 WS-CELULA-3-A PIC 9(1).
           05 WS-I-A        PIC 9(1).
       01 WS-EOF-A          PIC A(1).
       01 WS-LINHA-AUX-A    PIC S9(1) VALUE 0.

       01 CONTADOR-ATAQUE-LINHA-1    PIC 9(1) VALUE 0.
       01 CONTADOR-ATAQUE-LINHA-2    PIC 9(1) VALUE 0.
       01 CONTADOR-ATAQUE-LINHA-3    PIC 9(1) VALUE 0.
       01 CONTADOR-ATAQUE-COLUNA-1   PIC 9(1) VALUE 0.
       01 CONTADOR-ATAQUE-COLUNA-2   PIC 9(1) VALUE 0.
       01 CONTADOR-ATAQUE-COLUNA-3   PIC 9(1) VALUE 0.
       01 CONTADOR-ATAQUE-DIAGONAL-1 PIC 9(1) VALUE 0.
       01 CONTADOR-ATAQUE-DIAGONAL-2 PIC 9(1) VALUE 0.

       01 MARCADO-LINHA1-A    PIC X(1) VALUE "F".
       01 MARCADO-LINHA2-A    PIC X(1) VALUE "F".
       01 MARCADO-LINHA3-A    PIC X(1) VALUE "F".
       01 MARCADO-COLUNA1-A   PIC X(1) VALUE "F".
       01 MARCADO-COLUNA2-A   PIC X(1) VALUE "F".
       01 MARCADO-COLUNA3-A   PIC X(1) VALUE "F".
       01 MARCADO-DIAGONAL1-A PIC X(1) VALUE "F".
       01 MARCADO-DIAGONAL2-A PIC X(1) VALUE "F".

       01 SINAL-LINHA-1-A    PIC S9(2).
       01 SINAL-LINHA-2-A    PIC S9(2).
       01 SINAL-LINHA-3-A    PIC S9(2).

       01 SINAL-COLUNA-1-A   PIC S9(2).
       01 SINAL-COLUNA-2-A   PIC S9(2).
       01 SINAL-COLUNA-3-A   PIC S9(2).

       01 SINAL-DIAGONAL-1-A PIC S9(2).
       01 SINAL-DIAGONAL-2-A PIC S9(2).

       01 CONTA-I-A PIC 9(1) VALUE 1.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY " ".

           IF (QUANT-JOGO) = 0 THEN
               DISPLAY "  Bem Vindo ao Jogo Da Velha do Cobol "
           ELSE
               PERFORM ZERA-VARIAVEIS
           END-IF.
           PERFORM MOSTRA-JOGO.
           ADD 1 TO QUANT-JOGO.



     **       ---- LER ARQUIVOS DE ANALISE ----------------------
            PERFORM INICIA-CELULAS-OCUPADAS WITH TEST AFTER
            VARYING AUX-CEL-OP
            FROM 1 BY 1  UNTIL AUX-CEL-OP  = 9.
            PERFORM LE-ARQUIVO-BLOQUEIO.
            PERFORM INICIA-BLOQUEIO.
            PERFORM POPULA-MONITORA WITH TEST AFTER
            VARYING BLOQUEIO-LISTA-AUX
            FROM 1 BY 1 UNTIL BLOQUEIO-LISTA-AUX = NUM-LINHA-LISTA.

            PERFORM INICIA-ATAQUE.

    ***        ---- JOGO---------------------------------------
            PERFORM ESCOLHER-JOGADOR WITH TEST AFTER
            UNTIL JOGADOR-ESCOLHIDO = "V".
            PERFORM QM-INICIA.
            PERFORM JOGO UNTIL FIM-DE-JOGO = 9.
            PERFORM PERGUNTA-REINICIA-JOGO.

            STOP RUN.
     ************* JOGO ------------------------------------------------
       ESCOLHER-JOGADOR.
            DISPLAY " ".
            DISPLAY "  Escolha X ou O para jogar : ".
            ACCEPT USER-PLAYER.

           EVALUATE TRUE
               WHEN USER-PLAYER = "X" OR USER-PLAYER = "x"
                  MOVE "O" TO COBOL-PLAYER
                  MOVE "V" TO JOGADOR-ESCOLHIDO
               WHEN USER-PLAYER = "O" OR USER-PLAYER = "o"
                   MOVE "X" TO COBOL-PLAYER
                   MOVE "V" TO JOGADOR-ESCOLHIDO
                WHEN OTHER
                 DISPLAY "ESCOLHA INCORRETA"
                 MOVE "F" TO JOGADOR-ESCOLHIDO
            END-EVALUATE.

       QM-INICIA.
           COMPUTE RESULTADO-RANDOM-QM-INICIA = FUNCTION RANDOM * (2).

       PERGUNTA-REINICIA-JOGO.
               DISPLAY " ".
               DISPLAY "Deseja iniciar um novo jogo?".
                DISPLAY "1 - SIM".
                DISPLAY "2 - NAO".
                ACCEPT PERGUNTA-FINAL.

                IF(PERGUNTA-FINAL = 1) THEN
                       PERFORM MAIN-PROCEDURE
                ELSE
                    DISPLAY " ############# FIM ############## "
                    STOP RUN
                END-IF.

       ZERA-VARIAVEIS.
           MOVE 0 TO FIM-DE-JOGO.
           MOVE "F" TO VENCEU.

           MOVE SPACE TO CELULA00.
           MOVE SPACE TO CELULA01.
           MOVE SPACE TO CELULA02.
           MOVE SPACE TO CELULA10.
           MOVE SPACE TO CELULA11.
           MOVE SPACE TO CELULA12.
           MOVE SPACE TO CELULA20.
           MOVE SPACE TO CELULA21.
           MOVE SPACE TO CELULA22.

           MOVE "F" TO MARCADO-LINHA1.
           MOVE "F" TO MARCADO-LINHA2.
           MOVE "F" TO MARCADO-LINHA3.
           MOVE "F" TO MARCADO-COLUNA1.
           MOVE "F" TO MARCADO-COLUNA2.
           MOVE "F" TO MARCADO-COLUNA3.
           MOVE "F" TO MARCADO-DIAGONAL1.
           MOVE "F" TO MARCADO-DIAGONAL2.

           MOVE "F" TO MARCADO-LINHA1-A.
           MOVE "F" TO MARCADO-LINHA2-A.
           MOVE "F" TO MARCADO-LINHA3-A.
           MOVE "F" TO MARCADO-COLUNA1-A.
           MOVE "F" TO MARCADO-COLUNA2-A.
           MOVE "F" TO MARCADO-COLUNA3-A.
           MOVE "F" TO MARCADO-DIAGONAL1-A.
           MOVE "F" TO MARCADO-DIAGONAL2-A.


       MOSTRA-JOGO.
            DISPLAY " "
            DISPLAY " ----------- ".
            DISPLAY " " CELULA00 " | " CELULA01 " | " CELULA02 " ".
            DISPLAY " " CELULA10 " | " CELULA11 " | " CELULA12 " ".
            DISPLAY " " CELULA20 " | " CELULA21 " | " CELULA22 " ".
            DISPLAY " ----------- ".

       MOSTRA-PLACAR.
           DISPLAY " ".
            DISPLAY "    PLACAR   "
            DISPLAY "|______________|".
            DISPLAY "| VOCE | COBOL |".
            DISPLAY "|   " PLACAR-USUARIO " | " PLACAR-COBOL "    |"
            DISPLAY " ".

       JOGADA-USUARIO.
           DISPLAY "Sua vez ..."
           DISPLAY " "
           DISPLAY "Entre com a coordenada desejada : ".
            DISPLAY "  ------------ ".
            DISPLAY " 00 | 01 | 02 ".
            DISPLAY " 10 | 11 | 12 ".
            DISPLAY " 20 | 21 | 22 ".
            ACCEPT COORDENADA.

           EVALUATE TRUE
               WHEN ((COORDENADA = 0) AND (CELULA00 = SPACE
               OR CELULA00 = LOW-VALUE))
                   MOVE USER-PLAYER TO CELULA00
                   MOVE 0 TO WS-LINHA(1)
                   MOVE 1 TO WS-CELULA-1(1)
                   MOVE "V" TO CELULAS-OCUPADAS(1)
                   MOVE "V" TO V-USER-JOGOU
               WHEN ((COORDENADA = 1 AND (CELULA01 = SPACE
               OR CELULA01 = LOW-VALUE)))
                   MOVE USER-PLAYER TO CELULA01
                   MOVE 0 TO WS-LINHA(1)
                   MOVE 1 TO WS-CELULA-2(1)
                   MOVE "V" TO CELULAS-OCUPADAS(2)
                   MOVE "V" TO V-USER-JOGOU
               WHEN ((COORDENADA = 2 AND (CELULA02 = SPACE
               OR CELULA02 = LOW-VALUE)))
                   MOVE USER-PLAYER TO CELULA02
                   MOVE 0 TO WS-LINHA(1)
                   MOVE 1 TO WS-CELULA-3(1)
                   MOVE "V" TO CELULAS-OCUPADAS(3)
                   MOVE "V" TO V-USER-JOGOU
               WHEN ((COORDENADA = 10 AND (CELULA10 = SPACE
               OR CELULA10 = LOW-VALUE)))
                   MOVE USER-PLAYER TO CELULA10
                   MOVE 1 TO WS-LINHA(2)
                   MOVE 1 TO WS-CELULA-1(2)
                   MOVE "V" TO CELULAS-OCUPADAS(4)
                   MOVE "V" TO V-USER-JOGOU
               WHEN ((COORDENADA = 11 AND (CELULA11 = SPACE
               OR CELULA11 = LOW-VALUE)))
                   MOVE USER-PLAYER TO CELULA11
                   MOVE 1 TO WS-LINHA(2)
                   MOVE 1 TO WS-CELULA-2(2)
                   MOVE "V" TO CELULAS-OCUPADAS(5)
                   MOVE "V" TO V-USER-JOGOU
               WHEN ((COORDENADA = 12 AND (CELULA12 = SPACE
               OR CELULA12 = LOW-VALUE)))
                   MOVE USER-PLAYER TO CELULA12
                   MOVE 1 TO WS-LINHA(2)
                   MOVE 1 TO WS-CELULA-3(2)
                   MOVE "V" TO CELULAS-OCUPADAS(6)
                   MOVE "V" TO V-USER-JOGOU
               WHEN ((COORDENADA = 20 AND (CELULA20 = SPACE
               OR CELULA20 = LOW-VALUE)))
                   MOVE USER-PLAYER TO CELULA20
                   MOVE 2 TO WS-LINHA(3)
                   MOVE 1 TO WS-CELULA-1(3)
                   MOVE "V" TO CELULAS-OCUPADAS(7)
                   MOVE "V" TO V-USER-JOGOU
               WHEN ((COORDENADA = 21 AND (CELULA21 = SPACE
               OR CELULA21 = LOW-VALUE)))
                   MOVE USER-PLAYER TO CELULA21
                   MOVE 2 TO WS-LINHA(3)
                   MOVE 1 TO WS-CELULA-2(3)
                   MOVE "V" TO CELULAS-OCUPADAS(8)
                   MOVE "V" TO V-USER-JOGOU
               WHEN ((COORDENADA = 22 AND (CELULA22 = SPACE
               OR CELULA22 = LOW-VALUE)))
                   MOVE USER-PLAYER TO CELULA22
                   MOVE 2 TO WS-LINHA(3)
                   MOVE 1 TO WS-CELULA-3(3)
                   MOVE "V" TO CELULAS-OCUPADAS(9)
                   MOVE "V" TO V-USER-JOGOU
                WHEN OTHER
                 DISPLAY "CELULA INVALIDA"
            END-EVALUATE.

           IF V-USER-JOGOU = "V" THEN
               ADD 1 TO CONTA-I
               MOVE CONTA-I TO WS-I(1)
               MOVE CONTA-I TO WS-I(2)
               MOVE CONTA-I TO WS-I(3)

               PERFORM ESCREVE-BLOQUEIO-JOGADA
               PERFORM LE-ARQUIVO-BLOQUEIO
           END-IF.

       JOGADA-COBOL.
           DISPLAY "Vez do COBOL..."
           DISPLAY " "

           PERFORM ANALISA-ATAQUE.

           IF(V-COBOL-JOGOU = "F") THEN
              PERFORM ANALISA-BLOQUEIO

                   IF(V-COBOL-JOGOU = "F") THEN
     **              PEGA MAIS JOGADA
                            PERFORM ANALISA-MAIS-JOGADAS VARYING
                            AUX-I-CJ
                            FROM 1 BY 1 UNTIL AUX-I-CJ = 9
                            PERFORM QUAL-CEL-MAIS-JOGADA
                           MOVE 0 TO CONTA-VOLTA
                       END-IF
           END-IF.

           MOVE "F" TO V-COBOL-JOGOU.

           MOVE "V" TO EXISTE_ATAQUE
           MOVE "V" TO EXISTE_BLOQUEIO.


       ANALISA-ATAQUE.
     **     ---- VERIFICA SE DA PARA FECHAR UM JOGO (ATAQUE)
            PERFORM VERIFICA-ONDE-FAZER-ATAQUE VARYING WS-LINHA-AUX-A
            FROM 1 BY 1 UNTIL WS-LINHA-AUX-A = 4.

            IF (CONTADOR-ATAQUE-LINHA-1 IS GREATER THAN 1)
                AND (CELULAS-OCUPADAS(1) = "N"
                   OR CELULAS-OCUPADAS(2) = "N"
                   OR CELULAS-OCUPADAS(3) = "N")
                   THEN
               MOVE SINAL-LINHA-1-A TO COORDENADA
               MOVE "V" TO MARCADO-LINHA1-A
               PERFORM FAZ-JOGADA-COBOL
           ELSE
               IF (CONTADOR-ATAQUE-LINHA-2 IS GREATER THAN 1)
                   AND (CELULAS-OCUPADAS(4) = "N"
                   OR CELULAS-OCUPADAS(5) = "N"
                   OR CELULAS-OCUPADAS(6) = "N")
                   THEN
                   MOVE SINAL-LINHA-2-A TO COORDENADA
                   MOVE "V" TO MARCADO-LINHA2-A
                   PERFORM FAZ-JOGADA-COBOL
               ELSE
                   IF (CONTADOR-ATAQUE-LINHA-3 IS GREATER THAN 1)
                   AND (CELULAS-OCUPADAS(7) = "N"
                   OR CELULAS-OCUPADAS(8) = "N"
                   OR CELULAS-OCUPADAS(9) = "N")
                   THEN
                       MOVE SINAL-LINHA-3-A TO COORDENADA
                       MOVE "V" TO MARCADO-LINHA3-A
                       PERFORM FAZ-JOGADA-COBOL
                   ELSE
                       IF (CONTADOR-ATAQUE-COLUNA-1 IS GREATER THAN 1)
                           AND (CELULAS-OCUPADAS(1) = "N"
                           OR CELULAS-OCUPADAS(4) = "N"
                           OR CELULAS-OCUPADAS(7) = "N")
                           THEN
                           MOVE SINAL-COLUNA-1-A TO COORDENADA
                           MOVE "V" TO MARCADO-COLUNA1-A
                           PERFORM FAZ-JOGADA-COBOL
                       ELSE
                         IF (CONTADOR-ATAQUE-COLUNA-2 IS GREATER THAN 1)
                               AND (CELULAS-OCUPADAS(2) = "N"
                               OR CELULAS-OCUPADAS(5) = "N"
                               OR CELULAS-OCUPADAS(8) = "N")
                               THEN
                              MOVE SINAL-COLUNA-2-A TO COORDENADA
                              MOVE "V" TO MARCADO-COLUNA2-A
                              PERFORM FAZ-JOGADA-COBOL
                           ELSE
                         IF (CONTADOR-ATAQUE-COLUNA-3 IS GREATER THAN 1)
                                   AND (CELULAS-OCUPADAS(3) = "N"
                                   OR CELULAS-OCUPADAS(6) = "N"
                                   OR CELULAS-OCUPADAS(9) = "N")
                                   THEN
                                  MOVE SINAL-COLUNA-3-A TO COORDENADA
                                  MOVE "V" TO MARCADO-COLUNA3-A
                                  PERFORM FAZ-JOGADA-COBOL
                               ELSE
                                   IF (CONTADOR-ATAQUE-DIAGONAL-1
                                       IS GREATER THAN 1)
                                       AND (CELULAS-OCUPADAS(1) = "N"
                                       OR CELULAS-OCUPADAS(5) = "N"
                                       OR CELULAS-OCUPADAS(9) = "N")
                                       THEN
                                       MOVE SINAL-DIAGONAL-1-A
                                       TO COORDENADA
                                       MOVE "V" TO MARCADO-DIAGONAL1-A
                                       PERFORM FAZ-JOGADA-COBOL
                                   ELSE
                                       IF (CONTADOR-ATAQUE-DIAGONAL-2
                                           IS GREATER THAN 1)
                                         AND (CELULAS-OCUPADAS(3) = "N"
                                         OR CELULAS-OCUPADAS(5) = "N"
                                         OR CELULAS-OCUPADAS(7) = "N")
                                         THEN
                                       MOVE SINAL-DIAGONAL-2-A
                                       TO COORDENADA
                                       MOVE "V" TO MARCADO-DIAGONAL2-A
                                       PERFORM FAZ-JOGADA-COBOL
                                       ELSE
     **     ---- NAO HA ATAQUE, ANALISE BLOQUEIO
                                        MOVE "F" TO EXISTE_ATAQUE
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.
           MOVE 0 TO CONTADOR-ATAQUE-LINHA-1.
           MOVE 0 TO CONTADOR-ATAQUE-LINHA-2.
           MOVE 0 TO CONTADOR-ATAQUE-LINHA-3.
           MOVE 0 TO CONTADOR-ATAQUE-COLUNA-1.
           MOVE 0 TO CONTADOR-ATAQUE-COLUNA-2.
           MOVE 0 TO CONTADOR-ATAQUE-COLUNA-3.
           MOVE 0 TO CONTADOR-ATAQUE-DIAGONAL-1.
           MOVE 0 TO CONTADOR-ATAQUE-DIAGONAL-2.

           MOVE -1 TO SINAL-LINHA-1-A.
           MOVE -1 TO SINAL-LINHA-2-A.
           MOVE -1 TO SINAL-LINHA-3-A.

           MOVE -1 TO SINAL-COLUNA-1-A.
           MOVE -1 TO SINAL-COLUNA-2-A.
           MOVE -1 TO SINAL-COLUNA-3-A.

           MOVE -1 TO SINAL-DIAGONAL-1-A.
           MOVE -1 TO SINAL-DIAGONAL-2-A.

       ANALISA-BLOQUEIO.
     **     ----  VERIFICA SE HA BLOQUEIO A FAZER
           PERFORM VERIFICA-ONDE-FAZER-BLOQUEIO VARYING WS-LINHA-AUX
               FROM 1 BY 1 UNTIL WS-LINHA-AUX = 4.

           IF (CONTADOR-BLOQUEIO-LINHA-1 = 2)
               AND (CELULAS-OCUPADAS(1) = "N"
               OR CELULAS-OCUPADAS(2) = "N"
               OR CELULAS-OCUPADAS(3) = "N")
               THEN
               MOVE SINAL-LINHA-1 TO COORDENADA
               MOVE "V" TO MARCADO-LINHA1
               PERFORM FAZ-JOGADA-COBOL
           ELSE
               IF (CONTADOR-BLOQUEIO-LINHA-2 = 2)
                   AND (CELULAS-OCUPADAS(4) = "N"
                   OR CELULAS-OCUPADAS(5) = "N"
                   OR CELULAS-OCUPADAS(6) = "N")
                   THEN
                   MOVE SINAL-LINHA-2 TO COORDENADA
                   MOVE "V" TO MARCADO-LINHA2
                   PERFORM FAZ-JOGADA-COBOL
               ELSE
                   IF (CONTADOR-BLOQUEIO-LINHA-3 = 2)
                       AND (CELULAS-OCUPADAS(7) = "N"
                       OR CELULAS-OCUPADAS(8) = "N"
                       OR CELULAS-OCUPADAS(9) = "N")
                       THEN
                       MOVE SINAL-LINHA-3 TO COORDENADA
                       MOVE "V" TO MARCADO-LINHA3
                       PERFORM FAZ-JOGADA-COBOL
                   ELSE
                       IF (CONTADOR-BLOQUEIO-COLUNA-1 = 2)
                           AND (CELULAS-OCUPADAS(1) = "N"
                           OR CELULAS-OCUPADAS(4) = "N"
                           OR CELULAS-OCUPADAS(7) = "N")
                           THEN
                           MOVE SINAL-COLUNA-1 TO COORDENADA
                           MOVE "V" TO MARCADO-COLUNA1
                           PERFORM FAZ-JOGADA-COBOL
                       ELSE
                           IF (CONTADOR-BLOQUEIO-COLUNA-2 = 2)
                               AND (CELULAS-OCUPADAS(2) = "N"
                               OR CELULAS-OCUPADAS(5) = "N"
                               OR CELULAS-OCUPADAS(8) = "N")
                              THEN
                              MOVE SINAL-COLUNA-2 TO COORDENADA
                              MOVE "V" TO MARCADO-COLUNA2
                              PERFORM FAZ-JOGADA-COBOL
                           ELSE
                               IF (CONTADOR-BLOQUEIO-COLUNA-3 = 2)
                                   AND (CELULAS-OCUPADAS(3) = "N"
                                   OR CELULAS-OCUPADAS(6) = "N"
                                   OR CELULAS-OCUPADAS(9) = "N")
                                   THEN
                                  MOVE SINAL-COLUNA-3 TO COORDENADA
                                  MOVE "V" TO MARCADO-COLUNA3
                                  PERFORM FAZ-JOGADA-COBOL
                               ELSE
                                   IF (CONTADOR-BLOQUEIO-DIAGONAL-1 = 2)
                                       AND (CELULAS-OCUPADAS(1) = "N"
                                       OR CELULAS-OCUPADAS(5) = "N"
                                       OR CELULAS-OCUPADAS(9) = "N")
                                       THEN
                                       MOVE SINAL-DIAGONAL-1
                                       TO COORDENADA
                                       MOVE "V" TO MARCADO-DIAGONAL1
                                       PERFORM FAZ-JOGADA-COBOL
                                   ELSE
                                       IF CONTADOR-BLOQUEIO-DIAGONAL-2
                                        = 2
                                        AND (CELULAS-OCUPADAS(3) = "N"
                                        OR CELULAS-OCUPADAS(5) = "N"
                                        OR CELULAS-OCUPADAS(7) = "N")
                                           THEN
                                       MOVE SINAL-DIAGONAL-2
                                       TO COORDENADA
                                       MOVE "V" TO MARCADO-DIAGONAL2
                                       PERFORM FAZ-JOGADA-COBOL
                                       ELSE

     **     ---- NAO HA BLOQUEIO, ANALISE
                                       MOVE "F" TO EXISTE_BLOQUEIO
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.
           MOVE 0 TO CONTADOR-BLOQUEIO-LINHA-1.
           MOVE 0 TO CONTADOR-BLOQUEIO-LINHA-2.
           MOVE 0 TO CONTADOR-BLOQUEIO-LINHA-3.
           MOVE 0 TO CONTADOR-BLOQUEIO-COLUNA-1.
           MOVE 0 TO CONTADOR-BLOQUEIO-COLUNA-2.
           MOVE 0 TO CONTADOR-BLOQUEIO-COLUNA-3.
           MOVE 0 TO CONTADOR-BLOQUEIO-DIAGONAL-1.
           MOVE 0 TO CONTADOR-BLOQUEIO-DIAGONAL-2.

           MOVE -1 TO SINAL-LINHA-1.
           MOVE -1 TO SINAL-LINHA-2.
           MOVE -1 TO SINAL-LINHA-3.

           MOVE -1 TO SINAL-COLUNA-1.
           MOVE -1 TO SINAL-COLUNA-2.
           MOVE -1 TO SINAL-COLUNA-3.

           MOVE -1 TO SINAL-DIAGONAL-1.
           MOVE -1 TO SINAL-DIAGONAL-2.


       ANALISA-MAIS-JOGADAS.

           IF CELULAS-OCUPADAS(AUX-I-CJ) = "N" THEN
               IF(CONTA-VOLTA) = 0 THEN
                   MOVE CONTA-COORD-M-JOGADAS(AUX-I-CJ)
                   TO AUX-COORD-M-JOGADAS
                   MOVE AUX-I-CJ TO ULTIMA-I-CJ
                   ADD 1 TO CONTA-VOLTA
               ELSE
                   IF(CONTA-COORD-M-JOGADAS(AUX-I-CJ) IS GREATER THAN
                       AUX-COORD-M-JOGADAS) THEN
                       MOVE CONTA-COORD-M-JOGADAS(AUX-I-CJ) TO
                       AUX-COORD-M-JOGADAS
                       MOVE AUX-I-CJ TO ULTIMA-I-CJ
                   END-IF
               END-IF
           END-IF.


       QUAL-CEL-MAIS-JOGADA.
           EVALUATE TRUE
               WHEN ULTIMA-I-CJ = 1
                   MOVE 0 TO COORDENADA
               WHEN ULTIMA-I-CJ = 2
                   MOVE 1 TO COORDENADA
               WHEN ULTIMA-I-CJ = 3
                   MOVE 2 TO COORDENADA
               WHEN ULTIMA-I-CJ = 4
                   MOVE 10 TO COORDENADA
               WHEN ULTIMA-I-CJ = 5
                   MOVE 11 TO COORDENADA
               WHEN ULTIMA-I-CJ = 6
                   MOVE 12 TO COORDENADA
               WHEN ULTIMA-I-CJ = 7
                   MOVE 20 TO COORDENADA
               WHEN ULTIMA-I-CJ = 8
                   MOVE 21 TO COORDENADA
               WHEN ULTIMA-I-CJ = 9
                   MOVE 22 TO COORDENADA
           END-EVALUATE.
           PERFORM FAZ-JOGADA-COBOL.

       FAZ-JOGADA-COBOL.
               EVALUATE TRUE
               WHEN ((COORDENADA = 0) AND (CELULA00 = SPACE
               OR CELULA00 = LOW-VALUE))
                   MOVE COBOL-PLAYER TO CELULA00
                   MOVE 0 TO WS-LINHA-A(1)
                   MOVE 1 TO WS-CELULA-1-A(1)
                   MOVE "V" TO V-COBOL-JOGOU
                   MOVE "V" TO CELULAS-OCUPADAS(1)
               WHEN ((COORDENADA = 1 AND (CELULA01 = SPACE
               OR CELULA01 = LOW-VALUE)))
                   MOVE COBOL-PLAYER TO CELULA01
                   MOVE 0 TO WS-LINHA-A(1)
                   MOVE 1 TO WS-CELULA-2-A(1)
                   MOVE "V" TO V-COBOL-JOGOU
                   MOVE "V" TO CELULAS-OCUPADAS(2)
               WHEN ((COORDENADA = 2 AND (CELULA02 = SPACE
               OR CELULA02 = LOW-VALUE)))
                   MOVE COBOL-PLAYER TO CELULA02
                   MOVE 0 TO WS-LINHA-A(1)
                   MOVE 1 TO WS-CELULA-3-A(1)
                   MOVE "V" TO V-COBOL-JOGOU
                   MOVE "V" TO CELULAS-OCUPADAS(3)
               WHEN ((COORDENADA = 10 AND (CELULA10 = SPACE
               OR CELULA10 = LOW-VALUE)))
                   MOVE COBOL-PLAYER TO CELULA10
                   MOVE 1 TO WS-LINHA-A(2)
                   MOVE 1 TO WS-CELULA-1-A(2)
                   MOVE "V" TO V-COBOL-JOGOU
                   MOVE "V" TO CELULAS-OCUPADAS(4)
               WHEN ((COORDENADA = 11 AND (CELULA11 = SPACE
               OR CELULA11 = LOW-VALUE)))
                   MOVE COBOL-PLAYER TO CELULA11
                   MOVE 1 TO WS-LINHA-A(2)
                   MOVE 1 TO WS-CELULA-2-A(2)
                   MOVE "V" TO V-COBOL-JOGOU
                   MOVE "V" TO CELULAS-OCUPADAS(5)
               WHEN ((COORDENADA = 12 AND (CELULA12 = SPACE
               OR CELULA12 = LOW-VALUE)))
                   MOVE COBOL-PLAYER TO CELULA12
                   MOVE 1 TO WS-LINHA-A(2)
                   MOVE 1 TO WS-CELULA-3-A(2)
                   MOVE "V" TO V-COBOL-JOGOU
                   MOVE "V" TO CELULAS-OCUPADAS(6)
               WHEN ((COORDENADA = 20 AND (CELULA20 = SPACE
               OR CELULA20 = LOW-VALUE)))
                   MOVE COBOL-PLAYER TO CELULA20
                   MOVE 2 TO WS-LINHA-A(3)
                   MOVE 1 TO WS-CELULA-1-A(3)
                   MOVE "V" TO V-COBOL-JOGOU
                   MOVE "V" TO CELULAS-OCUPADAS(7)
               WHEN ((COORDENADA = 21 AND (CELULA21 = SPACE
               OR CELULA21 = LOW-VALUE)))
                   MOVE COBOL-PLAYER TO CELULA21
                   MOVE 2 TO WS-LINHA-A(3)
                   MOVE 1 TO WS-CELULA-2-A(3)
                   MOVE "V" TO V-COBOL-JOGOU
                   MOVE "V" TO CELULAS-OCUPADAS(8)
               WHEN ((COORDENADA = 22 AND (CELULA22 = SPACE
               OR CELULA22 = LOW-VALUE)))
                   MOVE COBOL-PLAYER TO CELULA22
                   MOVE 2 TO WS-LINHA-A(3)
                   MOVE 1 TO WS-CELULA-3-A(3)
                   MOVE "V" TO V-COBOL-JOGOU
                   MOVE "V" TO CELULAS-OCUPADAS(9)
                WHEN OTHER
                 MOVE "F" TO V-COBOL-JOGOU
            END-EVALUATE.
               DISPLAY COORDENADA.

           ADD 1 TO CONTA-I-A.
           MOVE CONTA-I-A TO WS-I-A(1).
           MOVE CONTA-I-A TO WS-I-A(2).
           MOVE CONTA-I-A TO WS-I-A(3).

           PERFORM ESCREVE-ATAQUE-JOGADA.



       VERIFICA-ONDE-FAZER-BLOQUEIO.

           IF WS-CELULA-1(WS-LINHA-AUX) = 1 THEN
               IF ((WS-LINHA-AUX = 1) AND (MARCADO-LINHA1 = "F")) THEN
                   ADD 1 TO CONTADOR-BLOQUEIO-LINHA-1
               END-IF
               IF ((WS-LINHA-AUX = 2) AND (MARCADO-LINHA2 = "F"))THEN
                   ADD 1 TO CONTADOR-BLOQUEIO-LINHA-2
               END-IF
               IF ((WS-LINHA-AUX = 3) AND (MARCADO-LINHA3 = "F")) THEN
                   ADD 1 TO CONTADOR-BLOQUEIO-LINHA-3
               END-IF

               IF(MARCADO-COLUNA1 = "F")
                   THEN
                   ADD 1 TO CONTADOR-BLOQUEIO-COLUNA-1
               END-IF

               IF ((WS-LINHA-AUX = 1)
                   AND (MARCADO-DIAGONAL1 = "F")) THEN
                   ADD 1 TO CONTADOR-BLOQUEIO-DIAGONAL-1
               END-IF
               IF ((WS-LINHA-AUX = 3)
                   AND (MARCADO-DIAGONAL2 = "F"))THEN
                   ADD 1 TO CONTADOR-BLOQUEIO-DIAGONAL-2
               END-IF

           ELSE
             IF WS-CELULA-1-A(WS-LINHA-AUX) = 0
               IF WS-LINHA-AUX = 1 THEN
                   MOVE 0 TO SINAL-LINHA-1
                   MOVE 0 TO SINAL-COLUNA-1
                   MOVE 0 TO SINAL-DIAGONAL-1
               END-IF
               IF WS-LINHA-AUX = 2 THEN
                   MOVE 10 TO SINAL-LINHA-2
                   MOVE 10 TO SINAL-COLUNA-1
               END-IF
               IF WS-LINHA-AUX = 3 THEN
                   MOVE 20 TO SINAL-LINHA-3
                   MOVE 20 TO SINAL-COLUNA-1
                   MOVE 20 TO SINAL-DIAGONAL-2
               END-IF
             END-IF
           END-IF.
           IF WS-CELULA-2(WS-LINHA-AUX) = 1 THEN
              IF ((WS-LINHA-AUX = 1) AND (MARCADO-LINHA1 = "F")) THEN
                   ADD 1 TO CONTADOR-BLOQUEIO-LINHA-1
               END-IF
               IF ((WS-LINHA-AUX = 2) AND (MARCADO-LINHA2 = "F"))THEN
                   ADD 1 TO CONTADOR-BLOQUEIO-LINHA-2
               END-IF
               IF ((WS-LINHA-AUX = 3) AND (MARCADO-LINHA3 = "F")) THEN
                   ADD 1 TO CONTADOR-BLOQUEIO-LINHA-3
               END-IF


               IF(MARCADO-COLUNA2 = "F")
                   THEN
                    ADD 1 TO CONTADOR-BLOQUEIO-COLUNA-2
               END-IF

               IF (WS-LINHA-AUX = 2) THEN
                   IF(MARCADO-DIAGONAL1 = "F") THEN
                       ADD 1 TO CONTADOR-BLOQUEIO-DIAGONAL-1
                   END-IF
                   IF(MARCADO-DIAGONAL2 = "F") THEN
                       ADD 1 TO CONTADOR-BLOQUEIO-DIAGONAL-2
                   END-IF
               END-IF

           ELSE
             IF WS-CELULA-2-A(WS-LINHA-AUX) = 0
               IF WS-LINHA-AUX = 1 THEN
                   MOVE 1 TO SINAL-LINHA-1
                   MOVE 1 TO SINAL-COLUNA-2
               END-IF
               IF WS-LINHA-AUX = 2 THEN
                   MOVE 11 TO SINAL-LINHA-2
                   MOVE 11 TO SINAL-COLUNA-2
                   MOVE 11 TO SINAL-DIAGONAL-1
                   MOVE 11 TO SINAL-DIAGONAL-2
               END-IF
               IF WS-LINHA-AUX = 3 THEN
                   MOVE 21 TO SINAL-LINHA-3
                   MOVE 21 TO SINAL-COLUNA-2
               END-IF
             END-IF
           END-IF.
           IF WS-CELULA-3(WS-LINHA-AUX) = 1 THEN
               IF ((WS-LINHA-AUX = 1) AND (MARCADO-LINHA1 = "F")) THEN
                   ADD 1 TO CONTADOR-BLOQUEIO-LINHA-1
               END-IF
               IF ((WS-LINHA-AUX = 2) AND (MARCADO-LINHA2 = "F")) THEN
                   ADD 1 TO CONTADOR-BLOQUEIO-LINHA-2
               END-IF
               IF ((WS-LINHA-AUX = 3) AND (MARCADO-LINHA3 = "F")) THEN
                   ADD 1 TO CONTADOR-BLOQUEIO-LINHA-3
               END-IF

               IF(MARCADO-COLUNA3 = "F")
                   THEN
                   ADD 1 TO CONTADOR-BLOQUEIO-COLUNA-3
               END-IF

               IF ((WS-LINHA-AUX = 3)
                   AND (MARCADO-DIAGONAL1 = "F")) THEN
                   ADD 1 TO CONTADOR-BLOQUEIO-DIAGONAL-1
               END-IF
               IF ((WS-LINHA-AUX = 1)
                   AND (MARCADO-DIAGONAL2 = "F"))THEN
                   ADD 1 TO CONTADOR-BLOQUEIO-DIAGONAL-2
               END-IF

           ELSE
            IF WS-CELULA-3-A(WS-LINHA-AUX) = 0
               IF WS-LINHA-AUX = 1 THEN
                   MOVE 2 TO SINAL-LINHA-1
                   MOVE 2 TO SINAL-COLUNA-3
                   MOVE 2 TO SINAL-DIAGONAL-2
               END-IF
               IF WS-LINHA-AUX = 2 THEN
                   MOVE 12 TO SINAL-LINHA-2
                   MOVE 12 TO SINAL-COLUNA-3
               END-IF
               IF WS-LINHA-AUX = 3 THEN
                   MOVE 22 TO SINAL-LINHA-3
                   MOVE 22 TO SINAL-COLUNA-3
                   MOVE 22 TO SINAL-DIAGONAL-1
               END-IF
             END-IF
           END-IF.


       VERIFICA-ONDE-FAZER-ATAQUE.
           IF WS-CELULA-1-A(WS-LINHA-AUX-A) = 1 THEN
               IF ((WS-LINHA-AUX-A = 1)
                   AND (MARCADO-LINHA1-A = "F")) THEN
                   ADD 1 TO CONTADOR-ATAQUE-LINHA-1
               END-IF
               IF ((WS-LINHA-AUX-A = 2)
                   AND (MARCADO-LINHA2-A = "F")) THEN
                   ADD 1 TO CONTADOR-ATAQUE-LINHA-2
               END-IF
               IF ((WS-LINHA-AUX-A = 3)
                   AND (MARCADO-LINHA3-A = "F"))THEN
                   ADD 1 TO CONTADOR-ATAQUE-LINHA-3
               END-IF

               IF(MARCADO-COLUNA1-A = "F")
                   THEN
                   ADD 1 TO CONTADOR-ATAQUE-COLUNA-1
               END-IF

               IF ((WS-LINHA-AUX-A = 1)
                   AND (MARCADO-DIAGONAL1-A = "F")) THEN
                   ADD 1 TO CONTADOR-ATAQUE-DIAGONAL-1
               END-IF
               IF ((WS-LINHA-AUX-A = 3)
                   AND (MARCADO-DIAGONAL2-A = "F")) THEN
                   ADD 1 TO CONTADOR-ATAQUE-DIAGONAL-2
               END-IF
           ELSE
             IF WS-CELULA-1(WS-LINHA-AUX-A) = 0
               IF WS-LINHA-AUX-A = 1 THEN
                   MOVE 0 TO SINAL-LINHA-1-A
                   MOVE 0 TO SINAL-COLUNA-1-A
                   MOVE 0 TO SINAL-DIAGONAL-1-A
               END-IF
               IF WS-LINHA-AUX-A = 2 THEN
                   MOVE 10 TO SINAL-LINHA-2-A
                   MOVE 10 TO SINAL-COLUNA-1-A
               END-IF
               IF WS-LINHA-AUX-A = 3 THEN
                   MOVE 20 TO SINAL-LINHA-3-A
                   MOVE 20 TO SINAL-COLUNA-1-A
                   MOVE 20 TO SINAL-DIAGONAL-2-A
               END-IF
             END-IF
           END-IF.
           IF WS-CELULA-2-A(WS-LINHA-AUX-A) = 1 THEN
              IF ((WS-LINHA-AUX-A = 1)
                  AND (MARCADO-LINHA1-A = "F"))THEN
                   ADD 1 TO CONTADOR-ATAQUE-LINHA-1
               END-IF
               IF ((WS-LINHA-AUX-A = 2)
                   AND (MARCADO-LINHA2-A = "F")) THEN
                   ADD 1 TO CONTADOR-ATAQUE-LINHA-2
               END-IF
               IF ((WS-LINHA-AUX-A = 3)
                   AND (MARCADO-LINHA3-A = "F")) THEN
                   ADD 1 TO CONTADOR-ATAQUE-LINHA-3
               END-IF

               IF(MARCADO-COLUNA2-A = "F")
                   THEN
                   ADD 1 TO CONTADOR-ATAQUE-COLUNA-2
              END-IF

               IF (WS-LINHA-AUX-A = 2) THEN
                   IF(MARCADO-DIAGONAL1-A = "F") THEN
                       ADD 1 TO CONTADOR-ATAQUE-DIAGONAL-1
                   END-IF
                   IF(MARCADO-DIAGONAL2-A = "F") THEN
                       ADD 1 TO CONTADOR-ATAQUE-DIAGONAL-2
                   END-IF
               END-IF

           ELSE
             IF WS-CELULA-2(WS-LINHA-AUX-A) = 0
               IF WS-LINHA-AUX-A = 1 THEN
                   MOVE 1 TO SINAL-LINHA-1-A
                   MOVE 1 TO SINAL-COLUNA-2-A
               END-IF
               IF WS-LINHA-AUX-A = 2 THEN
                   MOVE 11 TO SINAL-LINHA-2-A
                   MOVE 11 TO SINAL-COLUNA-2-A
                   MOVE 11 TO SINAL-DIAGONAL-1-A
                   MOVE 11 TO SINAL-DIAGONAL-2-A
               END-IF
               IF WS-LINHA-AUX-A = 3 THEN
                   MOVE 21 TO SINAL-LINHA-3-A
                   MOVE 21 TO SINAL-COLUNA-2-A
               END-IF
             END-IF
           END-IF.
           IF WS-CELULA-3-A(WS-LINHA-AUX-A) = 1 THEN
               IF ((WS-LINHA-AUX-A = 1)
                   AND (MARCADO-LINHA1-A = "F")) THEN
                   ADD 1 TO CONTADOR-ATAQUE-LINHA-1
               END-IF
               IF ((WS-LINHA-AUX-A = 2)
                   AND (MARCADO-LINHA2-A = "F")) THEN
                   ADD 1 TO CONTADOR-ATAQUE-LINHA-2
               END-IF
               IF ((WS-LINHA-AUX-A = 3)
                   AND (MARCADO-LINHA3-A = "F")) THEN
                   ADD 1 TO CONTADOR-ATAQUE-LINHA-3
               END-IF

               IF(MARCADO-COLUNA3-A = "F")
                   THEN
                   ADD 1 TO CONTADOR-ATAQUE-COLUNA-3
               END-IF

               IF ((WS-LINHA-AUX-A = 3)
                   AND (MARCADO-DIAGONAL1-A = "F")) THEN
                   ADD 1 TO CONTADOR-ATAQUE-DIAGONAL-1
               END-IF
               IF ((WS-LINHA-AUX-A = 1)
                   AND (MARCADO-DIAGONAL2-A = "F")) THEN
                   ADD 1 TO CONTADOR-ATAQUE-DIAGONAL-2
               END-IF

           ELSE
             IF WS-CELULA-3(WS-LINHA-AUX-A) = 0
               IF WS-LINHA-AUX-A = 1 THEN
                   MOVE 2 TO SINAL-LINHA-1-A
                   MOVE 2 TO SINAL-COLUNA-3-A
                   MOVE 2 TO SINAL-DIAGONAL-2-A
               END-IF
               IF WS-LINHA-AUX-A = 2 THEN
                   MOVE 12 TO SINAL-LINHA-2-A
                   MOVE 12 TO SINAL-COLUNA-3-A
               END-IF
               IF WS-LINHA-AUX-A = 3 THEN
                   MOVE 22 TO SINAL-LINHA-3-A
                   MOVE 22 TO SINAL-COLUNA-3-A
                   MOVE 22 TO SINAL-DIAGONAL-1-A
               END-IF
             END-IF
           END-IF.

       VERIFICA-SE-HA-VENCEDOR.

      *        LINHA----------------------------------
               IF (CELULA00) = (PLAYER-AUX) THEN
                   IF(CELULA01) = (PLAYER-AUX) THEN
                       IF (CELULA02) = (PLAYER-AUX) THEN
                           MOVE "T" TO VENCEU.
               IF (CELULA10) = (PLAYER-AUX) THEN
                   IF (CELULA11) = (PLAYER-AUX) THEN
                       IF (CELULA12) = (PLAYER-AUX) THEN
                           MOVE "T" TO VENCEU.
               IF (CELULA20) = (PLAYER-AUX) THEN
                   IF (CELULA21) = (PLAYER-AUX) THEN
                       IF (CELULA22) = (PLAYER-AUX) THEN
                           MOVE "T" TO VENCEU.

      *     COLUNA ---------------------------------------------------
               IF (CELULA00) = (PLAYER-AUX) THEN
                   IF (CELULA10) = (PLAYER-AUX) THEN
                       IF (CELULA20) = (PLAYER-AUX) THEN
                           MOVE "T" TO VENCEU.
               IF (CELULA01) = (PLAYER-AUX) THEN
                   IF (CELULA11) = (PLAYER-AUX) THEN
                       IF (CELULA21) = (PLAYER-AUX) THEN
                           MOVE "T" TO VENCEU.
               IF (CELULA02) = (PLAYER-AUX) THEN
                   IF (CELULA12) = (PLAYER-AUX) THEN
                       IF (CELULA22) = (PLAYER-AUX) THEN
                           MOVE "T" TO VENCEU.

      *     DIAGONAL --------------------------------------------------
               IF (CELULA00) = (PLAYER-AUX) THEN
                   IF (CELULA11) = (PLAYER-AUX) THEN
                       IF (CELULA22) = (PLAYER-AUX) THEN
                           MOVE "T" TO VENCEU.
               IF (CELULA02) = (PLAYER-AUX) THEN
                   IF (CELULA11) = (PLAYER-AUX) THEN
                       IF (CELULA20) = (PLAYER-AUX) THEN
                           MOVE "T" TO VENCEU.


       VERIFICA-FIM-JOGO.

               IF CELULAS-OCUPADAS(AUX-CEL-OP) = "V" THEN
                   ADD 1 TO FIM-DE-JOGO
               END-IF.


       JOGO.
           IF RESULTADO-RANDOM-QM-INICIA = 0 THEN
               PERFORM LOGICA-JOGO-USUARIO
               PERFORM LOGICA-JOGO-COBOL
           ELSE
               PERFORM LOGICA-JOGO-COBOL
               PERFORM LOGICA-JOGO-USUARIO
           END-IF.

       LOGICA-JOGO-COBOL.
           IF FIM-DE-JOGO IS NOT EQUAL TO 9 THEN
           IF (VENCEU) = "F" THEN
                   PERFORM JOGADA-COBOL
                   MOVE COBOL-PLAYER TO PLAYER-AUX
                   PERFORM VERIFICA-SE-HA-VENCEDOR
                   IF (VENCEU) = "T" THEN
                       DISPLAY "VOCE PERDEU PARA O COBOL !! "
                       ADD 1 TO PLACAR-COBOL
                       PERFORM MOSTRA-PLACAR
                       MOVE 9 TO FIM-DE-JOGO
                   ELSE
                     MOVE 0 TO AUX-CEL-OP
                     PERFORM VERIFICA-FIM-JOGO WITH TEST AFTER
                     VARYING AUX-CEL-OP
                     FROM 1 BY 1 UNTIL AUX-CEL-OP = 9
                       IF (FIM-DE-JOGO = 9) THEN
                           DISPLAY "DEU VELHA # "
                       ELSE
                           MOVE 0 TO FIM-DE-JOGO
                       END-IF
                   END-IF
           PERFORM MOSTRA-JOGO
              END-IF
           END-IF.


       LOGICA-JOGO-USUARIO.
           IF FIM-DE-JOGO IS NOT EQUAL TO 9 THEN
           IF (VENCEU) = "F" THEN
                   PERFORM JOGADA-USUARIO UNTIL V-USER-JOGOU = "V"
                   MOVE "F" TO V-USER-JOGOU
                   MOVE USER-PLAYER TO PLAYER-AUX
                   PERFORM VERIFICA-SE-HA-VENCEDOR
                   IF (VENCEU) = "T" THEN
                     DISPLAY "VOCE VENCEU !! "
                     ADD 1 TO PLACAR-USUARIO
                     PERFORM MOSTRA-PLACAR
                     MOVE 9 TO FIM-DE-JOGO
                   ELSE
                       MOVE 0 TO AUX-CEL-OP
                     PERFORM VERIFICA-FIM-JOGO WITH TEST AFTER
                     VARYING AUX-CEL-OP
                     FROM 1 BY 1 UNTIL AUX-CEL-OP = 9
                       IF (FIM-DE-JOGO = 9) THEN
                           DISPLAY "DEU VELHA # "
                       ELSE
                           MOVE 0 TO FIM-DE-JOGO
                       END-IF
                   END-IF
            PERFORM MOSTRA-JOGO
              END-IF
           END-IF.


       INICIA-CELULAS-OCUPADAS.
           MOVE "N" TO CELULAS-OCUPADAS(AUX-CEL-OP).

*************BLOQUEIO ---------------------------------------------
       INICIA-BLOQUEIO.
           MOVE 0 TO WS-LINHA(1).
           MOVE 0 TO WS-CELULA-1(1).
           MOVE 0 TO WS-CELULA-2(1).
           MOVE 0 TO WS-CELULA-3(1).
           MOVE 1 TO WS-I(1).

           MOVE 1 TO WS-LINHA(2).
           MOVE 0 TO WS-CELULA-1(2).
           MOVE 0 TO WS-CELULA-2(2).
           MOVE 0 TO WS-CELULA-3(2).
           MOVE 1 TO WS-I(2).

           MOVE 2 TO WS-LINHA(3).
           MOVE 0 TO WS-CELULA-1(3).
           MOVE 0 TO WS-CELULA-2(3).
           MOVE 0 TO WS-CELULA-3(3).
           MOVE 1 TO WS-I(3).



       LE-ARQUIVO-BLOQUEIO.
       OPEN INPUT BLOQUEIO.
        PERFORM UNTIL WS-EOF='Y'
         READ BLOQUEIO INTO BLOQUEIO-LISTA(NUM-LINHA-LISTA)
            AT END MOVE 'Y' TO WS-EOF
           NOT AT END
           ADD 1 TO NUM-LINHA-LISTA
         END-READ
       END-PERFORM.
       CLOSE BLOQUEIO.


       POPULA-MONITORA.
           IF(LINHA-LISTA(BLOQUEIO-LISTA-AUX) = 0) THEN

               IF(CELULA-1-LISTA(BLOQUEIO-LISTA-AUX) = 1) THEN
                   ADD 1 TO CONTA-COORD-M-JOGADAS(1)

               ELSE
                   IF(CELULA-2-LISTA(BLOQUEIO-LISTA-AUX) = 1) THEN
                   ADD 1 TO CONTA-COORD-M-JOGADAS(2)
                   ELSE
                       IF(CELULA-3-LISTA(BLOQUEIO-LISTA-AUX) = 1)
                           THEN
                           ADD 1 TO CONTA-COORD-M-JOGADAS(3)
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF(LINHA-LISTA(BLOQUEIO-LISTA-AUX) = 1) THEN
               IF(CELULA-1-LISTA(BLOQUEIO-LISTA-AUX) = 1) THEN
                   ADD 1 TO CONTA-COORD-M-JOGADAS(4)
               ELSE
                   IF(CELULA-2-LISTA(BLOQUEIO-LISTA-AUX) = 1) THEN
                   ADD 1 TO CONTA-COORD-M-JOGADAS(5)
                   ELSE
                       IF(CELULA-3-LISTA(BLOQUEIO-LISTA-AUX) = 1)
                           THEN
                           ADD 1 TO CONTA-COORD-M-JOGADAS(6)
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF(LINHA-LISTA(BLOQUEIO-LISTA-AUX) = 2) THEN
               IF(CELULA-1-LISTA(BLOQUEIO-LISTA-AUX) = 1) THEN
                   ADD 1 TO CONTA-COORD-M-JOGADAS(7)
               ELSE
                   IF(CELULA-2-LISTA(BLOQUEIO-LISTA-AUX) = 1) THEN
                   ADD 1 TO CONTA-COORD-M-JOGADAS(8)
                   ELSE
                       IF(CELULA-3-LISTA(BLOQUEIO-LISTA-AUX) = 1)
                           THEN
                           ADD 1 TO CONTA-COORD-M-JOGADAS(9)
                       END-IF
                   END-IF
               END-IF
           END-IF.

       ESCREVE-BLOQUEIO-JOGADA.
           OPEN EXTEND BLOQUEIO
               MOVE WS-BLOQUEIO(1) TO BLOQUEIO-ARQUIVO.
               WRITE BLOQUEIO-ARQUIVO
               MOVE WS-BLOQUEIO(2) TO BLOQUEIO-ARQUIVO.
               WRITE BLOQUEIO-ARQUIVO
               MOVE WS-BLOQUEIO(3) TO BLOQUEIO-ARQUIVO.
               WRITE BLOQUEIO-ARQUIVO
               END-WRITE
           CLOSE BLOQUEIO.

 ************ATAQUE -----------------------------------------
       INICIA-ATAQUE.
           MOVE 0 TO WS-LINHA-A(1).
           MOVE 0 TO WS-CELULA-1-A(1).
           MOVE 0 TO WS-CELULA-2-A(1).
           MOVE 0 TO WS-CELULA-3-A(1).
           MOVE 1 TO WS-I-A(1).

           MOVE 1 TO WS-LINHA-A(2).
           MOVE 0 TO WS-CELULA-1-A(2).
           MOVE 0 TO WS-CELULA-2-A(2).
           MOVE 0 TO WS-CELULA-3-A(2).
           MOVE 1 TO WS-I-A(2).

           MOVE 2 TO WS-LINHA-A(3).
           MOVE 0 TO WS-CELULA-1-A(3).
           MOVE 0 TO WS-CELULA-2-A(3).
           MOVE 0 TO WS-CELULA-3-A(3).
           MOVE 1 TO WS-I-A(3).

       ESCREVE-ATAQUE-JOGADA.
           OPEN EXTEND ATAQUE
               MOVE WS-ATAQUE(1) TO ATAQUE-ARQUIVO.
               WRITE ATAQUE-ARQUIVO
               MOVE WS-ATAQUE(2) TO ATAQUE-ARQUIVO.
               WRITE ATAQUE-ARQUIVO
               MOVE WS-ATAQUE(3) TO ATAQUE-ARQUIVO.
               WRITE ATAQUE-ARQUIVO
               END-WRITE
           CLOSE ATAQUE.

       END PROGRAM JOGO-DA-VELHA.
