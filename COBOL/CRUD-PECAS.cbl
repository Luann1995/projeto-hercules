       IDENTIFICATION DIVISION.
       PROGRAM-ID.  DESAFIO11.
      *******************************************************
      ***   AREA DE COMENTARIOS - REMARKS
      ***   OBJETIVO DO PROGRAMA = SISTEMA DE GESTAO DE ESTOQUE
      ***   AUTOR: LUANN
      ***   DATA : XX/XX/20XX
      ******************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ESTOQUE ASSIGN TO 'C:\COBOL\ESTOQUE.TXT'
             ORGANIZATION IS INDEXED
             ACCESS MODE IS RANDOM
             FILE STATUS IS ESTOQUE-STATUS
             RECORD KEY IS  ESTOQUE-CHAVE.
       DATA DIVISION.
       FILE SECTION.
       FD ESTOQUE.
       01 ESTOQUE-REG.
            05 ESTOQUE-CHAVE.
                10 ID-PECA         PIC 9(3).
            05 NOME-PECA           PIC X(30).
            05 PRECO-PECA          PIC 9(3)V99.
            05 QT-PECA             PIC 9(3).
      *>  --------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *>  --------------------------------------------------------------
       77 WRK-OPCAO        PIC X(1).
       77 WRK-MODULO       PIC X(30).
       77 WRK-TECLA        PIC X(1).
       77 ESTOQUE-STATUS   PIC 9(02).
       77 WRK-MSGERRO      PIC X(30).
       77 WRK-CONTALINHA   PIC 9(03) VALUE 0.
       77 WRK-QTREGISTROS  PIC 9(05) VALUE 0.
      *>  --------------------------------------------------------------
       SCREEN SECTION.
      *>  --------------------------------------------------------------
       01 TELA.
            05 LIMPA-TELA.
                10 BLANK SCREEN.
                10 LINE 01 COLUMN 01 PIC X(20) ERASE EOL
                   BACKGROUND-COLOR 3.
                10 LINE 01 COLUMN 25 PIC X(20)
                   BACKGROUND-COLOR 3  FOREGROUND-COLOR 0
                              FROM 'SISTEMA DE ESTOQUE DE PECAS'.
                10 LINE 02 COLUMN 01 PIC X(25) ERASE EOL
                   BACKGROUND-COLOR 1 FROM WRK-MODULO.
      *>  --------------------------------------------------------------
       01 MENU.
      *>  --------------------------------------------------------------
            05 LINE 07 COLUMN 15 VALUE '1 - INCLUIR PECA'.
            05 LINE 08 COLUMN 15 VALUE '2 - CONSULTAR PECA'.
            05 LINE 09 COLUMN 15 VALUE '3 - ALTERAR DADOS DA PECA'.
            05 LINE 10 COLUMN 15 VALUE '4 - EXCLUIR REGISTRO DE PECA'.
            05 LINE 11 COLUMN 15 VALUE '5 - RELATORIO'.
            05 LINE 12 COLUMN 15 VALUE 'X - SAIR'.
            05 LINE 13 COLUMN 15 VALUE 'OPCAO......: '.
            05 LINE 13 COLUMN 28 USING WRK-OPCAO.
      *>  --------------------------------------------------------------
       01 TELA-REGISTRO.
      *>  --------------------------------------------------------------
            05 CHAVE FOREGROUND-COLOR 2.
               10 LINE 10 COLUMN 10 VALUE 'ID PECA '.
               10 COLUMN PLUS 2 PIC 9(3) USING ID-PECA
                   BLANK WHEN ZEROS.
            05 SS-DADOS.
               10 LINE 11 COLUMN 10 VALUE 'NOME DA PECA.... '.
               10 COLUMN PLUS 2 PIC X(20) USING NOME-PECA.
               10 LINE 12 COLUMN 10 VALUE 'PRECO DA PECA... '.
               10 COLUMN PLUS 2 PIC Z(3) USING PRECO-PECA.
               10 LINE 13 COLUMN 10 VALUE 'QUANTIDADE DA PECA... '.
               10 COLUMN PLUS 2 PIC X(3) USING QT-PECA.
      *>  --------------------------------------------------------------
       01 MOSTRA-ERRO.
      *>  --------------------------------------------------------------
             02 MSG-ERRO.
               10 LINE 16 COLUMN 01 ERASE EOL
                             BACKGROUND-COLOR 3.
               10 LINE 16 COLUMN 10 PIC X(30)
                             BACKGROUND-COLOR 5
                             FROM WRK-MSGERRO.
               10 COLUMN PLUS 2 PIC X(01)
                             BACKGROUND-COLOR 3
                             USING WRK-TECLA.
      *>  --------------------------------------------------------------
       PROCEDURE DIVISION.
      *>  --------------------------------------------------------------
       0001-PRINCIPAL SECTION.
            PERFORM 1000-INICIAR.
            PERFORM 2000-PROCESSAR UNTIL WRK-OPCAO EQUAL 'X'.
            PERFORM 3000-FINALIZAR.
            STOP RUN.
      *>  --------------------------------------------------------------
       1000-INICIAR.
      *>  --------------------------------------------------------------
            OPEN I-O ESTOQUE
              IF ESTOQUE-STATUS = 35 THEN
                  OPEN OUTPUT ESTOQUE
                  CLOSE ESTOQUE
                  OPEN I-O ESTOQUE
               END-IF.
            DISPLAY TELA.
            ACCEPT MENU.
      *>  --------------------------------------------------------------
       1100-MONTA-TELA.
      *>  --------------------------------------------------------------
            DISPLAY TELA.
            ACCEPT MENU.
      *>  --------------------------------------------------------------
       2000-PROCESSAR.
      *>  --------------------------------------------------------------
            EVALUATE WRK-OPCAO
              WHEN 1
               PERFORM 5000-INCLUIR
              WHEN 2
               PERFORM 6000-CONSULTAR
              WHEN 3
                PERFORM 7000-ALTERAR
              WHEN 4
                PERFORM 8000-EXCLUIR
              WHEN 5
                PERFORM 9000-RELATORIO
              WHEN OTHER
                IF WRK-OPCAO NOT EQUAL 'X'
                    DISPLAY 'ENTRE COM A OPCAO CORRETA'
                    PERFORM 2000-PROCESSAR
                END-IF
            END-EVALUATE.
              PERFORM 1100-MONTA-TELA.
      *>  --------------------------------------------------------------
       3000-FINALIZAR.
      *>  --------------------------------------------------------------
             CLOSE ESTOQUE.
      *>  --------------------------------------------------------------
       5000-INCLUIR.
      *>  --------------------------------------------------------------
             MOVE 'MODULO - INCLUSAO ' TO WRK-MODULO.
             DISPLAY TELA.
              ACCEPT TELA-REGISTRO.
                WRITE ESTOQUE-REG.
                  IF ESTOQUE-STATUS = 22
                    DISPLAY 'REGISTRO JA EXISTE'
                    ACCEPT WRK-OPCAO
                  END-IF.
                  DISPLAY TELA.
            ACCEPT MENU.
      *>  --------------------------------------------------------------
       6000-CONSULTAR.
      *>  --------------------------------------------------------------
             MOVE 'MODULO - CONSULTA ' TO WRK-MODULO.
             DISPLAY TELA.
               DISPLAY TELA-REGISTRO.
               ACCEPT CHAVE.
                READ ESTOQUE
                  INVALID KEY
                   MOVE 'NAO ENCONTRADO   '  TO WRK-MSGERRO
                  NOT INVALID KEY
                  MOVE '--  ENCONTRADO  --'  TO WRK-MSGERRO
                   DISPLAY SS-DADOS
                 END-READ.
                   ACCEPT MOSTRA-ERRO.
      *>  --------------------------------------------------------------
       7000-ALTERAR.
      *>  --------------------------------------------------------------
             MOVE 'MODULO - ALTERAR ' TO WRK-MODULO.
             DISPLAY TELA.
             DISPLAY TELA-REGISTRO.
              ACCEPT CHAVE.
                READ ESTOQUE
                IF ESTOQUE-STATUS = 0
                    ACCEPT SS-DADOS
                     REWRITE ESTOQUE-REG
                       IF ESTOQUE-STATUS = 0
                            MOVE 'REGISTRO ALTERADO ' TO WRK-MSGERRO
                            ACCEPT MOSTRA-ERRO
                       ELSE
                            MOVE 'REGISTRO NAO ALTERADO' TO WRK-MSGERRO
                            ACCEPT MOSTRA-ERRO
                       END-IF
                 ELSE
                      MOVE 'REGISTO NAO ENCONTRADO ' TO WRK-MSGERRO
                      ACCEPT MOSTRA-ERRO
                END-IF.
      *>  --------------------------------------------------------------
       8000-EXCLUIR.
      *>  --------------------------------------------------------------
             MOVE 'MODULO - EXCLUSAO ' TO WRK-MODULO.
             DISPLAY TELA.
               DISPLAY TELA-REGISTRO.
               ACCEPT CHAVE.
                READ ESTOQUE
                  INVALID KEY
                   MOVE 'NAO ENCONTRADO   '  TO WRK-MSGERRO
                 NOT INVALID KEY
                  MOVE ' DESEJA EXCLUIR  (S/N) ? '  TO WRK-MSGERRO
                   DISPLAY SS-DADOS
                END-READ.
                  ACCEPT MOSTRA-ERRO.
                    IF WRK-TECLA = 'S' AND ESTOQUE-STATUS = 0
                           DELETE ESTOQUE
                            INVALID KEY
                            MOVE 'NAO EXCLUIDO ' TO WRK-MSGERRO
                            ACCEPT  MOSTRA-ERRO
                          END-DELETE
                     END-IF.
      *>  --------------------------------------------------------------
       9000-RELATORIO.
      *>  --------------------------------------------------------------
             MOVE 'MODULO - RELATORIO ' TO WRK-MODULO.
             DISPLAY TELA.
             MOVE 001 TO ID-PECA.
             READ ESTOQUE
                 INVALID KEY
                     MOVE 'NENHUM REGISTRO ENCONTRADO' TO WRK-MSGERRO
                 NOT INVALID KEY
                   DISPLAY '   RELATORIO DAS PECAS ' LINE 09 COLUMN 10
                   DISPLAY '----------------------' LINE 10 COLUMN 10
                   PERFORM 9100-MOSTRA-PECAS UNTIL ESTOQUE-STATUS = 10
             END-READ.
               MOVE 'REGISTROS LIDOS ' TO WRK-MSGERRO.
               MOVE WRK-QTREGISTROS TO WRK-MSGERRO(17:05).
               MOVE 0 TO WRK-QTREGISTROS.
               ACCEPT MOSTRA-ERRO.
      *>  --------------------------------------------------------------
       9100-MOSTRA-PECAS.
      *>  --------------------------------------------------------------
             ADD 1 TO WRK-QTREGISTROS
             DISPLAY ID-PECA LINE 11 COLUMN 10
                     'ID: ' LINE 11 COLUMN 6
                     NOME-PECA LINE 12 COLUMN 13
                     'NOME: ' LINE 12 COLUMN 6
                     PRECO-PECA LINE 13 COLUMN 13
                     'PRECO: ' LINE 13 COLUMN 6
                     QT-PECA LINE 14 COLUMN 18
                     'QUANTIDADE: ' LINE 14 COLUMN 6
             READ ESTOQUE NEXT
             ADD 1 TO WRK-CONTALINHA
             IF WRK-CONTALINHA = 5
               MOVE 'PRESSIONE ALGUMA TECLA ' TO WRK-MSGERRO
               ACCEPT MOSTRA-ERRO
               PERFORM 9100-MOSTRA-PECAS
               MOVE 0 TO WRK-CONTALINHA
             END-IF.
      *>  --------------------------------------------------------------
