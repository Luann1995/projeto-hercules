       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO10.
      ******************************************************
      ***   AREA DE COMENTARIOS - REMARKS
      ***   OBJETIVO DO PROGRAMA = SISTEMA DE IDENTIFICAÇÃO DE INVESTI-
      ***   DORES E INDICAÇÃO DE INVESTIMENTOS
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
       77 CONTADOR         PIC 9(2)         VALUE ZEROS.
      *>  --------------------------------------------------------------
       PROCEDURE DIVISION.
             PERFORM 0001-ROTINA-PRINCIPAL.

           STOP RUN.
      *>  --------------------------------------------------------------
       0001-ROTINA-PRINCIPAL.
      *>  --------------------------------------------------------------
             DISPLAY 'SELECIONE AS OPCOES DE 1 AO 5'.
             PERFORM 0100-PERG-1.
             PERFORM 0200-PERG-2.
             PERFORM 0300-PERG-3.
             PERFORM 0400-PERG-4.
             PERFORM 0500-PERG-5.
             PERFORM 0600-TIPO-INVESTIDOR.
      *>  --------------------------------------------------------------
       0100-PERG-1.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'QUAL SEU PRICIPAL OBEJETIVO AO INVESTIR'.
             DISPLAY '1 - PRESERVAR CAPITAL PARA APOSENTADORIA'.
             DISPLAY '2 - OBTER RENDA PARA COMPRAR UM BEM'.
             DISPLAY '3 - GANHAR O MAXIMO DE LUCRO NO CURTO PRAZO'.
             DISPLAY 'SELECIONE SUA OPCAO...:' ACCEPT SELEC-MENU.
             EVALUATE SELEC-MENU
               WHEN 1
                 ADD 1 TO CONTADOR
               WHEN 2
                 ADD 2 TO CONTADOR
               WHEN 3
                 ADD 3 TO CONTADOR
               WHEN OTHER
                 DISPLAY '*** SELECIONE A OPCAO CORRETA ***'
                 PERFORM 0100-PERG-1
               END-EVALUATE.
      *>  --------------------------------------------------------------
       0200-PERG-2.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'QUAL SUA TOLERANCIA AO RISCO'.
             DISPLAY '1 - NAO TOLERO RISCO, PREFIRO ATIVOS SEGUROS'.
             DISPLAY '2 - PERMITO ALGUNS ATIVOS DE RISCO NA CARTEIRA'.
             DISPLAY '3 - PREFIRO ATIVOS COM MAIORES RISCOS'.
             DISPLAY 'SELECIONE SUA OPCAO...:' ACCEPT SELEC-MENU.
             EVALUATE SELEC-MENU
               WHEN 1
                 ADD 1 TO CONTADOR
               WHEN 2
                 ADD 2 TO CONTADOR
               WHEN 3
                 ADD 3 TO CONTADOR
               WHEN OTHER
                 DISPLAY '*** SELECIONE A OPCAO CORRETA ***'
                 PERFORM 0200-PERG-2
               END-EVALUATE.
       *>  --------------------------------------------------------------
       0300-PERG-3.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'PRAZO PREVISTO PARA MANTER SEUS INVESTIMENTOS'.
             DISPLAY '1 - MAIS DE 10 ANOS'.
             DISPLAY '2 - ENTRE 5 E 10 ANOS'.
             DISPLAY '3 - ENTRE 1 E 5 ANOS'.
             DISPLAY 'SELECIONE SUA OPCAO...:' ACCEPT SELEC-MENU.
             EVALUATE SELEC-MENU
               WHEN 1
                 ADD 1 TO CONTADOR
               WHEN 2
                 ADD 2 TO CONTADOR
               WHEN 3
                 ADD 3 TO CONTADOR
               WHEN OTHER
                 DISPLAY '*** SELECIONE A OPCAO CORRETA ***'
                 PERFORM 0300-PERG-3
               END-EVALUATE.
       *>  --------------------------------------------------------------
       0400-PERG-4.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'INDIQUE SEU CONHECIMENTO SOBRE INVESTIMENTOS'.
             DISPLAY '1 - NAO CONHECO NADA SOBRE INVESTIMENTOS'.
             DISPLAY '2 - TENHO UM CONHECIMENTO SUPERFICIAL'.
             DISPLAY '3 - CONHECO BEM O MERCADO FINANCEIRO'.
             DISPLAY 'SELECIONE SUA OPCAO...:' ACCEPT SELEC-MENU.
             EVALUATE SELEC-MENU
               WHEN 1
                 ADD 1 TO CONTADOR
               WHEN 2
                 ADD 2 TO CONTADOR
               WHEN 3
                 ADD 3 TO CONTADOR
               WHEN OTHER
                 DISPLAY '*** SELECIONE A OPCAO CORRETA ***'
                 PERFORM 0400-PERG-4
               END-EVALUATE.
      *>  --------------------------------------------------------------
       0500-PERG-5.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'QUAL SUA PREFERENCIA NA LIQUIDEZ DOS ATIVOS'.
             DISPLAY '1 - POSSO ESPERAR ATE 1 ANO PARA SACAR'.
             DISPLAY '2 - POSSO ESPERAR ATE 6 MESES PARA SACAR'.
             DISPLAY '3 - SAQUE IMEDIATO OU EM ATE 3 DIAS'.
             DISPLAY 'SELECIONE SUA OPCAO...:' ACCEPT SELEC-MENU.
             EVALUATE SELEC-MENU
               WHEN 1
                 ADD 1 TO CONTADOR
               WHEN 2
                 ADD 2 TO CONTADOR
               WHEN 3
                 ADD 3 TO CONTADOR
               WHEN OTHER
                 DISPLAY '*** SELECIONE A OPCAO CORRETA ***'
                 PERFORM 0500-PERG-5
               END-EVALUATE.
      *>  --------------------------------------------------------------
       0600-TIPO-INVESTIDOR.
      *>  --------------------------------------------------------------
             IF CONTADOR >= 5 AND CONTADOR <= 9
                 DISPLAY '--------------------------------'
                 DISPLAY 'SEU PERFIL...: CONSERVADOR'
                 DISPLAY '--------------------------------'
                 DISPLAY 'MELHORES INVESTIMENTOS PARA SEU PERFIL'
                 DISPLAY '1 - FUNDO DE APOSENTADORIA'
                 DISPLAY '2 - CDB, LCI OU LCA'
                 DISPLAY '3 - OUROCAP OU CONSORCIO'
             ELSE
                 IF CONTADOR >= 9 AND CONTADOR <= 13
                   DISPLAY '--------------------------------'
                   DISPLAY 'SEU PERFIL...: MODERADO'
                   DISPLAY '--------------------------------'
                   DISPLAY 'MELHORES INVESTIMENTOS PARA SEU PERFIL'
                   DISPLAY '1 - FUNDOS DE INVESTIMENTOS'
                   DISPLAY '2 - FUNDOS IMOBILIARIOS'
                   DISPLAY '3 - CDB, LCI OU LCA'
                 ELSE
                   DISPLAY '--------------------------------'
                   DISPLAY 'SEU PERFIL...: ARROJADO'
                   DISPLAY '--------------------------------'
                   DISPLAY 'MELHORES INVESTIMENTOS PARA SEU PERFIL'
                   DISPLAY '1 - ETFS OU ACOES'
                   DISPLAY '2 - DEBENTURES OU FUNDOS DE CRIPTOMOEDAS'
                   DISPLAY '3 - COES OU DERIVATIVOS'
                 END-IF
               END-IF.
      *>  --------------------------------------------------------------
      *>  O QUE MELHORAR:
      *>  1- COMBINE ESSE PROGRAMA COM O DA CALCULADORA DE APLICAÇÃO
      *>  FINANCEIRA PARA CRIAR UM PRODUTO UNICO E MAIS COMPLETO. NÃO
      *>  ESQUEÇA DE FAZER AS ADAPTAÇÕES NECESSÁRIAS PARA O PROGRAMA
      *>  FUNCIONAR CORRETAMENTE
      *>   2 - Adicione o perfil “Agressivo” e altere as pontuações
