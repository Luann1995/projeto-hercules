       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO01.
      ******************************************************
      ***   AREA DE COMENTARIOS - REMARKS
      ***   OBJETIVO DO PROGRAMA = SISTEMA PARA CONVERSÃO DE MOEDAS
      ***   AUTOR: LUANN
      ***   DATA : XX/XX/20XX
      ******************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 SELEC-OPCAO      PIC 9(1)        VALUE ZEROS.
       77 VALOR            PIC S9(5)V99     VALUE ZEROS.
       77 CONVERSAO        PIC 9(5)V99      VALUE ZEROS.
       01 DATA-SYSTEMA.
           02 SYS-ANO  PIC 9(4).
           02 SYS-MES  PIC 9(2).
           02 SYS-DIA  PIC 9(2).
       COPY 'BOOK-MOEDAS.CBL'.
      *>  --------------------------------------------------------------
       PROCEDURE DIVISION.
             PERFORM 0001-ROTINA-PRINCIPAL.


      *>      STOP RUN.
      *>  --------------------------------------------------------------
       0001-ROTINA-PRINCIPAL.
      *>  --------------------------------------------------------------
             ACCEPT DATA-SYSTEMA FROM DATE YYYYMMDD.
             DISPLAY SYS-DIA'\'SYS-MES'\'SYS-ANO
             DISPLAY '------------------------------------'.
             DISPLAY 'SISTEMA DE CONVERSAO DE MOEDAS'.
             DISPLAY '------------------------------------'.
             DISPLAY 'SELECIONE A MOEDA PARA CONVERSAO:'.
             DISPLAY '1 - REAL  (BRASIL)'.
             DISPLAY '2 - DOLAR (EUA)'.
             DISPLAY '3 - EURO  (EUROPA)'.
             DISPLAY '4 - YUAN  (CHINA)'.
             ACCEPT SELEC-OPCAO.
             EVALUATE SELEC-OPCAO
               WHEN 1
                 PERFORM 0100-CONVERTE-REAL
               WHEN 2
                 PERFORM 0200-CONVERTE-DOLAR
               WHEN 3
                 PERFORM 0300-CONVERTE-EURO
               WHEN 4
               PERFORM 0400-CONVERTE-YUAN
               WHEN OTHER
                 DISPLAY '* VALOR INVALIDO'
                 PERFORM 0001-ROTINA-PRINCIPAL
             END-EVALUATE.
      *>  --------------------------------------------------------------
       0100-CONVERTE-REAL.
      *>  --------------------------------------------------------------
             DISPLAY 'COVERTENDO REAL PARA OUTRAS MOEDAS'.
             DISPLAY '------------------------------------'.
             DISPLAY 'DIGITE A QUANTIDADE DE REAIS PARA CONVERSAO.:'
             ACCEPT VALOR.
             IF VALOR > 0
               COMPUTE CONVERSAO = VALOR / WRK-DOLAR
               DISPLAY 'R$ ' VALOR ' EM DOLARES: ' CONVERSAO
               COMPUTE CONVERSAO = VALOR / WRK-EURO
               DISPLAY 'R$ ' VALOR ' EM EUROS: ' CONVERSAO
               COMPUTE CONVERSAO = VALOR / WRK-YUAN
               DISPLAY 'R$ ' VALOR ' EM YUANS: ' CONVERSAO
             ELSE
               DISPLAY '* VALOR INVALIDO *'
               PERFORM 0100-CONVERTE-REAL
             END-IF.
      *>  --------------------------------------------------------------
       0200-CONVERTE-DOLAR.
      *>  --------------------------------------------------------------
             DISPLAY 'COVERTENDO DOLAR PARA OUTRAS MOEDAS'.
             DISPLAY '------------------------------------'.
             DISPLAY 'DIGITE A QUANTIDADE DE DOLARES PARA CONVERSAO.:'
             ACCEPT VALOR.
             IF VALOR > 0
               COMPUTE CONVERSAO = VALOR / (WRK-REAL / WRK-DOLAR)
               DISPLAY 'US$ ' VALOR ' EM REAIS: ' CONVERSAO
               COMPUTE CONVERSAO = VALOR / (WRK-EURO / WRK-DOLAR)
               DISPLAY 'US$ ' VALOR ' EM EUROS: ' CONVERSAO
               COMPUTE CONVERSAO = VALOR / (WRK-YUAN / WRK-DOLAR)
               DISPLAY 'US$ ' VALOR ' EM YUANS: ' CONVERSAO
             ELSE
               DISPLAY '* VALOR INVALIDO *'
               PERFORM 0200-CONVERTE-DOLAR
             END-IF.
      *>  --------------------------------------------------------------
       0300-CONVERTE-EURO.
      *>  --------------------------------------------------------------
             DISPLAY 'COVERTENDO EURO PARA OUTRAS MOEDAS'.
             DISPLAY '------------------------------------'.
             DISPLAY 'DIGITE A QUANTIDADE DE EUROS PARA CONVERSAO.:'
             ACCEPT VALOR.
             IF VALOR > 0
               COMPUTE CONVERSAO = VALOR / (WRK-REAL / WRK-EURO)
               DISPLAY 'EU$ ' VALOR ' EM REAIS: ' CONVERSAO
               COMPUTE CONVERSAO = VALOR / (WRK-DOLAR / WRK-EURO)
               DISPLAY 'EU$ ' VALOR ' EM EUROS: ' CONVERSAO
               COMPUTE CONVERSAO = VALOR / (WRK-YUAN / WRK-EURO)
               DISPLAY 'EU$ ' VALOR ' EM YUANS: ' CONVERSAO
             ELSE
               DISPLAY '* VALOR INVALIDO *'
               PERFORM 0300-CONVERTE-EURO
             END-IF.
      *>  --------------------------------------------------------------
       0400-CONVERTE-YUAN.
      *>  --------------------------------------------------------------
             DISPLAY 'COVERTENDO YUANS PARA OUTRAS MOEDAS'.
             DISPLAY '------------------------------------'.
             DISPLAY 'DIGITE A QUANTIDADE DE YUANS PARA CONVERSAO.:'
             ACCEPT VALOR.
             IF VALOR > 0
               COMPUTE CONVERSAO = VALOR / (WRK-REAL / WRK-YUAN)
               DISPLAY 'Y$ ' VALOR ' EM REAIS: ' CONVERSAO
               COMPUTE CONVERSAO = VALOR / (WRK-DOLAR / WRK-YUAN)
               DISPLAY 'Y$ ' VALOR ' EM DOLARES: ' CONVERSAO
               COMPUTE CONVERSAO = VALOR / (WRK-EURO / WRK-YUAN)
               DISPLAY 'Y$ ' VALOR ' EM EUROS: ' CONVERSAO
             ELSE
               DISPLAY '* VALOR INVALIDO *'
               PERFORM 0400-CONVERTE-YUAN
             END-IF.
      *>  --------------------------------------------------------------
      *>  DESAFIOS EXTRAS:
      *> 1 - MUDE OS VALORES DAS VARIAVEIS NO BOOK-MOEDAS E VEJA OS RESULTADOS
      *> 2 - ADICIONE UMA NOVA MOEDA PARA CONVERSÃO, O WON SUL-COREANO
      *> POR EXEMPLO.
