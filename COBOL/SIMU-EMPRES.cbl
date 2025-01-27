       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO12.
      ******************************************************
      ***   AREA DE COMENTARIOS - REMARKS
      ***   OBJETIVO DO PROGRAMA = SIMULADOR DE EMPRESTIMO
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
       77 LIM-CRED         PIC 9(6)V99      VALUE ZEROS.
       77 VL-EPTM          PIC 9(6)V99      VALUE ZEROS.
       77 OPC-TAX-PRAZO    PIC 9(1)         VALUE ZEROS.
       77 VL-PRCLA         PIC 9(5)V99      VALUE ZEROS.
       77 TOT-EPTM         PIC 9(6)V99      VALUE ZEROS.
       77 STAT-PERG        PIC 9(1)         VALUE ZEROS.
      *>  --------------------------------------------------------------
       PROCEDURE DIVISION.
             DISPLAY '---------------------------------------'.
             DISPLAY '** SIMULADOR DE EMPRESTIMO BANCARIO **'
             DISPLAY '---------------------------------------'.
             PERFORM 0001-ROTINA-PRINCIPAL.

           STOP RUN.
      *>  --------------------------------------------------------------
       0001-ROTINA-PRINCIPAL.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'SELECAO DE PERFIL DE CREDITO'
             DISPLAY '---------------------------------------'.
             PERFORM 0100-PERG-1.
             PERFORM 0200-PERG-2.
             PERFORM 0300-PERG-3.
             PERFORM 0400-PERG-4.
             PERFORM 0500-CALC-EMPRESTIMO.
             PERFORM 0600-SELEC-EMPRESTIMO.
             PERFORM 0700-SELEC-TAXAS.
             move 0 to contador.

      *>  --------------------------------------------------------------
       0100-PERG-1.
      *>  --------------------------------------------------------------
             DISPLAY 'QUAL A SUA IDADE'.
             DISPLAY '1 - MENOR DE 18 ANOS'.
             DISPLAY '2 - ENTRE 18 E 29 ANOS'.
             DISPLAY '3 - ENTRE 30 E 35 ANOS'.
             DISPLAY '4 - ENTRE 36 E 40 ANOS'.
             DISPLAY '5 - MAIS DE 40 ANOS'.
             DISPLAY 'SELECIONE SUA OPCAO...:'.
             MOVE 1 TO STAT-PERG.
             PERFORM 0800-ENTRA-DADOS.
      *>  --------------------------------------------------------------
       0200-PERG-2.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'VALOR DA RENDA MENSAL'.
             DISPLAY '1 - MENOR QUE UM SALARIO MINIMO (R$' SAL-MIN')'.
             DISPLAY '2 - ENTRE R$ ' SAL-MIN ' E R$ ' 3000,00.
             DISPLAY '3 - ENTRE R$ '4500,00 ' E R$ ' 7500,00.
             DISPLAY '4 - ENTRE R$ '9000,00 ' E R$ ' 13500,00.
             DISPLAY '5 - MAIS DE R$ '15000,00.
             DISPLAY 'SELECIONE SUA OPCAO...:'.
             MOVE 2 TO STAT-PERG.
             PERFORM 0800-ENTRA-DADOS.
      *>  --------------------------------------------------------------
       0300-PERG-3.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'VALOR DOS BENS MOVEIS E IMOVEIS'.
             DISPLAY '1 - MENOR QUE R$ 10.000,00'.
             DISPLAY '2 - ENTRE R$ 10.000,00 E R$ 49.999,00'.
             DISPLAY '3 - ENTRE R$ 50.000,00 E R$ 100.000,00'.
             DISPLAY '4 - ENTRE R$ 100.001,00 E R$ 1.000.000,00'.
             DISPLAY '5 - MAIS DE R$ 1.000.000,00'.
             DISPLAY 'SELECIONE SUA OPCAO...:'.
             MOVE 3 TO STAT-PERG.
             PERFORM 0800-ENTRA-DADOS.
      *>  --------------------------------------------------------------
       0400-PERG-4.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'INDIQUE SEU REGIME PROFISSIONAL'.
             DISPLAY '1 - EMPREGADO CLT'.
             DISPLAY '2 - MICRO EMPREENDEDOR'.
             DISPLAY '3 - PESSOA JURIDICA DE MEDIO PORTE'.
             DISPLAY '4 - EMPRESARIO INDUSTRIAL'.
             DISPLAY '5 - PRODUTOR RURAL'.
             DISPLAY 'SELECIONE SUA OPCAO...:'.
             MOVE 4 TO STAT-PERG.
             PERFORM 0800-ENTRA-DADOS.
      *>  --------------------------------------------------------------
       0500-CALC-EMPRESTIMO.
      *>  --------------------------------------------------------------
             DISPLAY '-----------------------------------'.
      *>        DISPLAY 'PONTOS DO CLIENTE.: ' CONTADOR.
             EVALUATE CONTADOR
               WHEN <= 20
      *>            DISPLAY 'CREDITO LIMITADO ATE R$ 5.000,00'
                 MOVE 5000 TO LIM-CRED
               WHEN <= 40
      *>            DISPLAY 'CREDITO LIMITADO ATE R$ 10.000,00'
                 MOVE 10000 TO LIM-CRED
               WHEN <= 60
      *>            DISPLAY 'CREDITO LIMITADO ATE R$ 20.000,00'
                 MOVE 20000 TO LIM-CRED
               WHEN <= 90
      *>            DISPLAY 'CREDITO LIMITADO ATE R$ 50.000,00'
                 MOVE 50000 TO LIM-CRED
               WHEN <= 120
      *>            DISPLAY 'CREDITO LIMITADO ATE R$ 100.000,00'
                 MOVE 100000 TO LIM-CRED
             END-EVALUATE.
      *>  --------------------------------------------------------------
       0600-SELEC-EMPRESTIMO.
      *>  --------------------------------------------------------------
             DISPLAY '-----------------------------------'.
             display 'pontos: ' contador.
             DISPLAY 'SEU LIMITE DE CREDITO: R$ ' LIM-CRED.
             DISPLAY 'DIGITE O VALOR DO EMPRESTIMO:'.
             ACCEPT VL-EPTM.
             IF VL-EPTM > LIM-CRED OR VL-EPTM <= 100
               DISPLAY '* VALOR INVALIDO *'
               PERFORM 0600-SELEC-EMPRESTIMO
               ELSE
                 DISPLAY 'VALOR DO EMPRESTIMO R$ ' VL-EPTM
             END-IF.
      *>  --------------------------------------------------------------
       0700-SELEC-TAXAS.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'SELECIONE OS MELHORES PRAZOS E TAXAS'
             DISPLAY '1 - PAGUE EM ATE 5 MESES COM JUROS DE 1,2% a.m'.
             DISPLAY '2 - PAGUE EM ATE 10 MESES COM JUROS DE 1,5% a.m'.
             DISPLAY '3 - PAGUE EM ATE 15 MESES COM JUROS DE 2% a.m'.
             DISPLAY '4 - PAGUE EM ATE 20 MESES COM JUROS DE 2,5% a.m'.
             DISPLAY 'SELECIONE A MELHOR OPCAO: '.
             ACCEPT  OPC-TAX-PRAZO.
             EVALUATE OPC-TAX-PRAZO
               WHEN 1
                 DISPLAY 'PARCELAS A PAGAR: 5'
                 COMPUTE VL-PRCLA = (VL-EPTM*0,012*5)/5 + (VL-EPTM/5)
                 DISPLAY 'VALOR DE CADA PARCELA: R$ ' VL-PRCLA
                 COMPUTE TOT-EPTM= VL-PRCLA * 5
                 DISPLAY 'VALOR TOTAL DO EMPRESTIMO: R$ ' TOT-EPTM
               WHEN 2
                 DISPLAY 'PARCELAS A PAGAR: 10'
                 COMPUTE VL-PRCLA = (VL-EPTM*0,015*10)/10 + (VL-EPTM/10)
                 DISPLAY 'VALOR DE CADA PARCELA: R$ ' VL-PRCLA
                 COMPUTE TOT-EPTM= VL-PRCLA * 10
                 DISPLAY 'VALOR TOTAL DO EMPRESTIMO: R$ ' TOT-EPTM
               WHEN 3
                 DISPLAY 'PARCELAS A PAGAR: 15'
                 COMPUTE VL-PRCLA = (VL-EPTM*0,02*15)/15 + (VL-EPTM/15)
                 DISPLAY 'VALOR DE CADA PARCELA: R$ ' VL-PRCLA
                 COMPUTE TOT-EPTM= VL-PRCLA * 15
                 DISPLAY 'VALOR TOTAL DO EMPRESTIMO: R$ ' TOT-EPTM
               WHEN 4
                 DISPLAY 'PARCELAS A PAGAR: 20'
                 COMPUTE VL-PRCLA = (VL-EPTM*0,025*20)/20 + (VL-EPTM/20)
                 DISPLAY 'VALOR DE CADA PARCELA: R$ ' VL-PRCLA
                 COMPUTE TOT-EPTM= VL-PRCLA * 20
                 DISPLAY 'VALOR TOTAL DO EMPRESTIMO: R$ ' TOT-EPTM
               WHEN OTHER
                 DISPLAY '* OPCAO INVALIDA *'
                 PERFORM 0700-SELEC-TAXAS
             END-EVALUATE.
      *>  --------------------------------------------------------------
       0800-ENTRA-DADOS.
      *>  --------------------------------------------------------------
             ACCEPT SELEC-MENU.
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
                 EVALUATE STAT-PERG
                   WHEN 1
                     PERFORM 0100-PERG-1
                   WHEN 2
                     PERFORM 0200-PERG-2
                   WHEN 3
                     PERFORM 0300-PERG-3
                   WHEN 4
                     PERFORM 0400-PERG-4
                 END-EVALUATE
             END-EVALUATE.
      *>  --------------------------------------------------------------
      *>  O QUE MELHORAR?
      *>  1 - ALEM DE MOSTRAR O VALOR DE CADA PARCELA E O VALOR TOTAL DO
      *>  EMPRESTIMO, EXIBA TAMBEM O VALOR DOS JUROS TOTAIS DO EMPRESTIMO
      *>  E A PORCENTAGEM
      *>  2 - Para evitar casos de inadimplência, modifique o limite de
      *>  empréstimo para R$ 2.000,00 para clientes com pontuação menor
      *>  que 10.
