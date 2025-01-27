       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO06.
      ******************************************************
      ***   AREA DE COMENTARIOS - REMARKS
      ***   OBJETIVO DO PROGRAMA = CALCULADORA DE APLICAÇÕES FINANCEIRAS
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
       77 VL-INV           PIC 9(5)         VALUE ZEROS.
       77 TMP-INV          PIC 9(3)         VALUE ZEROS.
       77 VAL-RESULT       PIC 9(5)V99      VALUE ZEROS.
       77 TX-PD            PIC 9(3)V999     VALUE ZEROS.
      *>  --------------------------------------------------------------
       PROCEDURE DIVISION.
             PERFORM 0100-ROTINA-PRINCIPAL.

           STOP RUN.
      *>  --------------------------------------------------------------
       0100-ROTINA-PRINCIPAL.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'SELECIONE O PRODUTO DE INVESTIMENTO'.
             DISPLAY '1 - INVESTIR EM CDB'.
             DISPLAY '2 - INVESTIR EM LCI OU LCA'.
             DISPLAY '3 - INVESTIR EM FUNDOS IMOBILIARIOS'.
             DISPLAY '4 - INVESTIR EM FUNDOS HEDGE'.
             DISPLAY 'SELECIONE SUA OPCAO...:' ACCEPT SELEC-MENU.
             EVALUATE SELEC-MENU
               WHEN 1
      *>   MOVE A TAXA DE 1% DO CDB PARA A VARIAVEL TX-PD
                 MOVE 0,01 TO TX-PD
                 PERFORM 0400-CALC-PROD-2
               WHEN 2
      *>   MOVE A TAXA 1,5% DO LCI OU LCA PARA A VARIAVEL TX-PD
                 MOVE 0,015 TO TX-PD
                 PERFORM 0400-CALC-PROD-2
               WHEN 3
      *>   MOVE A TAXA DE 2% DOS FUNDOS IMOBILIARIOS PARA A VARI. TX-PD
                 MOVE 0,02 TO TX-PD
                 PERFORM 0400-CALC-PROD-2
               WHEN 4
      *>   MOVE A TAXA DE 2,5% DOS FUNDOS HEDGE PARA A VARIAVEL TX-PD
                 MOVE 0,025 TO TX-PD
                 PERFORM 0400-CALC-PROD-2
               WHEN OTHER
                 DISPLAY '*** SELECIONE A OPCAO CORRETA ***'
                 PERFORM 0100-ROTINA-PRINCIPAL
               END-EVALUATE.
       *>  -------------------------------------------------------------
       0200-ENTRA-DADOS.
      *>   -------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             EVALUATE SELEC-MENU
             WHEN 1
               DISPLAY 'PRODUTO ESCOLHIDO: * CDB *'
             WHEN 2
               DISPLAY 'PRODUTO ESCOLHIDO: * LCI OU LCA *'
             WHEN 3
               DISPLAY 'PRODUTO ESCOLHIDO: * FUNDOS IMOBILIARIOS *'
             WHEN OTHER
               DISPLAY 'PRODUTO ESCOLHIDO: * FUNDOS HEDGE *'
             END-EVALUATE
             DISPLAY 'QUANTO VOCE QUER INVESTIR R$.: '.
             ACCEPT VL-INV.
             DISPLAY 'QUANTOS MESES QUER MANTER O INVESTIMENTO?.: '.
             ACCEPT TMP-INV.
             DISPLAY '---------------------------------------'.
      *>  --------------------------------------------------------------
       0300-CALC-PROD.
      *>  --------------------------------------------------------------
             PERFORM 0200-ENTRA-DADOS
             PERFORM TMP-INV TIMES
              COMPUTE VAL-RESULT = VL-INV * (1 + TX-PD)
              MOVE VAL-RESULT TO VL-INV
              ADD 1 TO CONTADOR
              DISPLAY 'RESULTADO R$ ' VAL-RESULT ' NO MES ' CONTADOR
             END-PERFORM.
      *>  --------------------------------------------------------------
       0400-CALC-PROD-2.
      *>  --------------------------------------------------------------
             PERFORM 0200-ENTRA-DADOS
             COMPUTE VAL-RESULT = VL-INV * (1 + TX-PD) ** TMP-INV.
             DISPLAY 'VALOR FINAL DA APLICACAO R$ ' VAL-RESULT.
      *>  --------------------------------------------------------------
      *>  O QUE PODE MELHORAR?
      *>  1- INSERIR MAIS OPÇÕES DE INVESTIMENTOS
      *>  2- CRIAR UM LOOP PARA REALIZAR MAIS OPERAÇOES ATÉ QUE O USUARIO
      *>  QUEIRA SAIR
      *>  3- VALIDAR OS VALORES DE ENTRADA DO INVESTIMENTO E DOS MESES
      *>  4- INSERIR UM VALOR MINIMO PARA INVESTIR E UM MAXIMO PARA OS MESES
