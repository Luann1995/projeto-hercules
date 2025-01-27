       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO03.
      ******************************************************
      ***   AREA DE COMENTARIOS - REMARKS
      ***   OBJETIVO DO PROGRAMA = SISTEMA DE INDICAÇAO DE CARTÃO
      ***   AUTOR: LUANN
      ***   DATA : XX/XX/20XX
      ******************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 SELEC-MENU       PIC 9(1)         VALUE ZEROS.
       77 CONTADOR         PIC 9(3)         VALUE ZEROS.
       77 SAL-MIN          PIC 9(4)V99      VALUE 1500,00.
      *>  --------------------------------------------------------------
       PROCEDURE DIVISION.
             PERFORM 0001-ROTINA-PRINCIPAL.

      *>      STOP RUN.
      *>  --------------------------------------------------------------
       0001-ROTINA-PRINCIPAL.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'SISTEMA DE INDICACAO DE CARTOES'
             DISPLAY '---------------------------------------'.
             PERFORM 0100-PERG-1.
             PERFORM 0200-PERG-2.
             PERFORM 0300-PERG-3.
             PERFORM 0400-PERG-4.
             PERFORM 0500-CALC-CARTAO.
             move 0 to contador.
      *>  --------------------------------------------------------------
       0100-PERG-1.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'QUAL SUA IDADE'.
             DISPLAY '1 - MENOR DE 18 ANOS'.
             DISPLAY '2 - ENTRE 18 E 29 ANOS'.
             DISPLAY '3 - ENTRE 30 E 35 ANOS'.
             DISPLAY '4 - ENTRE 36 E 40 ANOS'.
             DISPLAY '5 - MAIS DE 40 ANOS'.
             DISPLAY 'SELECIONE SUA OPCAO...:' ACCEPT SELEC-MENU.
             EVALUATE SELEC-MENU
               WHEN 1
                 ADD 1 TO CONTADOR
               WHEN 2
                 ADD 5 TO CONTADOR
               WHEN 3
                 ADD 10 TO CONTADOR
               WHEN 4
                 ADD 20 TO CONTADOR
               WHEN 5
                 ADD 30 TO CONTADOR
               WHEN OTHER
                 DISPLAY '*** SELECIONE A OPCAO CORRETA ***'
                 PERFORM 0100-PERG-1
             END-EVALUATE.
      *>  --------------------------------------------------------------
       0200-PERG-2.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'QUAL SUA RENDA MENSAL'.
             DISPLAY '1 - MENOR QUE UM SALARIO-MINIMO (R$' SAL-MIN')'.
             DISPLAY '2 - ENTRE R$ ' SAL-MIN ' E R$ ' 4500,00.
             DISPLAY '3 - ENTRE R$ '4600,00 ' E R$ ' 7500,00.
             DISPLAY '4 - ENTRE R$ '7600,00 ' E R$ ' 20000,00.
             DISPLAY '5 - MAIS DE R$ '20000,00.
             DISPLAY 'SELECIONE SUA OPCAO...:' ACCEPT SELEC-MENU.
             EVALUATE SELEC-MENU
               WHEN 1
                 ADD 1 TO CONTADOR
               WHEN 2
                 ADD 5 TO CONTADOR
               WHEN 3
                 ADD 10 TO CONTADOR
               WHEN 4
                 ADD 20 TO CONTADOR
               WHEN 5
                 ADD 30 TO CONTADOR
               WHEN OTHER
                 DISPLAY '*** SELECIONE A OPCAO CORRETA ***'
                 PERFORM 0200-PERG-2
             END-EVALUATE.
      *>  --------------------------------------------------------------
       0300-PERG-3.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'VALOR DOS BENS MOVEIS E IMOVEIS'.
             DISPLAY '1 - INFERIOR A R$ 10.000,00'.
             DISPLAY '2 - ENTRE R$ 10.000,00 E R$ 49.999,00'.
             DISPLAY '3 - ENTRE R$ 50.000,00 E R$ 100.000,00'.
             DISPLAY '4 - ENTRE R$ 100.001,00 E R$ 1.000.000,00'.
             DISPLAY '5 - MAIS DE R$ 1.000.000,00'.
             DISPLAY 'SELECIONE SUA OPCAO...:' ACCEPT SELEC-MENU.
             EVALUATE SELEC-MENU
               WHEN 1
                 ADD 1 TO CONTADOR
               WHEN 2
                 ADD 5 TO CONTADOR
               WHEN 3
                 ADD 10 TO CONTADOR
               WHEN 4
                 ADD 20 TO CONTADOR
               WHEN 5
                 ADD 30 TO CONTADOR
               WHEN OTHER
                 DISPLAY '*** SELECIONE A OPCAO CORRETA ***'
                 PERFORM 0300-PERG-3
             END-EVALUATE.
      *>  --------------------------------------------------------------
       0400-PERG-4.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'INDIQUE SEU REGIME PROFISSIONAL'.
             DISPLAY '1 - EMPREGADO CLT'.
             DISPLAY '2 - MICROEMPREENDEDOR'.
             DISPLAY '3 - PESSOA JURIDICA DE MEDIO PORTE'.
             DISPLAY '4 - EMPRESARIO INDUSTRIAL'.
             DISPLAY '5 - PRODUTOR RURAL'.
             DISPLAY 'SELECIONE SUA OPCAO...:' ACCEPT SELEC-MENU.
             EVALUATE SELEC-MENU
               WHEN 1
                 ADD 1 TO CONTADOR
               WHEN 2
                 ADD 5 TO CONTADOR
               WHEN 3
                 ADD 10 TO CONTADOR
               WHEN 4
                 ADD 20 TO CONTADOR
               WHEN 5
                 ADD 30 TO CONTADOR
               WHEN OTHER
                 DISPLAY '*** SELECIONE A OPCAO CORRETA ***'
                 PERFORM 0400-PERG-4
             END-EVALUATE.
      *>  --------------------------------------------------------------
       0500-CALC-CARTAO.
      *>  --------------------------------------------------------------
             DISPLAY '-----------------------------------'.
             DISPLAY 'PONTOS DO CLIENTE.: ' CONTADOR.
             EVALUATE CONTADOR
               WHEN <= 20
                 DISPLAY 'MELHOR CARTAO PARA O CLIENTE: * POP *'
               WHEN <= 40
                 DISPLAY 'MELHOR CARTAO PARA O CLIENTE: * BRONZE *'
               WHEN <= 60
                 DISPLAY 'MELHOR CARTAO PARA O CLIENTE: * PRATA *'
               WHEN <= 90
                 DISPLAY 'MELHOR CARTAO PARA O CLIENTE: * OURO *'
               WHEN <= 120
                 DISPLAY 'MELHOR CARTAO PARA O CLIENTE: * BLACK *'
             END-EVALUATE.
      *>  --------------------------------------------------------------
      *>  DESAFIOS EXTRAS:
      *> 1 - O EVALUATE SELEC-MENU SE REPETE VÁRIAS VEZES E ISSO É UMA PESSIMA-
      *>  PRATICA, CRIE UM MODULO PARA EVITAR REPETIÇÃO DE CÓDIGO
      *> 2 - ADICIONE UMA NOVA PERGUNTA PARA O CLIENTE E MUDE A PONTUAÇÃO
      *> 3 - ADICIONE UM MODULO PARA MODIFICAR E MOSTRAR O LIMITE DO CARTÃO
