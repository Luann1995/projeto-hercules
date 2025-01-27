       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO05.
      ******************************************************
      ***   AREA DE COMENTARIOS - REMARKS
      ***   OBJETIVO DO PROGRAMA = SISTEMA QUE CALCULA A APOSENTADORIA
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
       77 IDADE            PIC 9(2)         VALUE ZEROS.
       77 SEXO             PIC X(1)         VALUE SPACE.
       77 TEMPO-CONTRIB    PIC 9(2)         VALUE ZEROS.
       77 MEDIA-SALARIAL   PIC 9(5)V99      VALUE ZEROS.
       77 VALOR-BENEF      PIC 9(4)V99      VALUE ZEROS.
       77 SOMA-PONTOS      PIC 9(3)         VALUE ZEROS.
      *>  --------------------------------------------------------------
       PROCEDURE DIVISION.
             PERFORM 0001-ROTINA-PRINCIPAL.

           STOP RUN.
      *>  --------------------------------------------------------------
       0001-ROTINA-PRINCIPAL.
      *>  --------------------------------------------------------------
             DISPLAY '----------------------------------------'
             DISPLAY 'CALCULADORA DE APOSENTADORIA'
             DISPLAY '----------------------------------------'
             DISPLAY 'DIGITE A IDADE DO CONTRIBUINTE..: '.
             ACCEPT IDADE.
             DISPLAY 'DIGITE O TEMPO DE CONTRIBUICAO..: '.
             ACCEPT TEMPO-CONTRIB.
             DISPLAY 'DIGITE A MEDIA SALARIAL DO CONTRIBUINTE..: '.
             ACCEPT MEDIA-SALARIAL.
             DISPLAY 'DIGITE O SEXO DO CONTRIBUINTE (F OU M)..: '.
             ACCEPT SEXO.
             DISPLAY '----------------------------------------'.
             PERFORM 0100-REGRA-PONTOS.
             DISPLAY '----------------------------------------'.
             PERFORM 0200-REGRA-IDADE.
      *>  --------------------------------------------------------------
       0100-REGRA-PONTOS.
      *>  --------------------------------------------------------------
             DISPLAY 'APOSENTADORIA POR PONTOS'
             COMPUTE SOMA-PONTOS = IDADE + TEMPO-CONTRIB.
             EVALUATE SEXO
               WHEN 'M'
                 IF SOMA-PONTOS >= 101 AND TEMPO-CONTRIB >= 35
                    AND IDADE >= 65
                   COMPUTE VALOR-BENEF = (0,6 + (TEMPO-CONTRIB - 20)
                           * 0,02) * MEDIA-SALARIAL
                   DISPLAY 'APOSENTADORIA MENSAL..:R$ ' VALOR-BENEF
                 ELSE
                   PERFORM 0002-MENSAGEM-ERRO
                 END-IF
               WHEN 'F'
                 IF SOMA-PONTOS >= 92 AND TEMPO-CONTRIB >= 29
                    AND IDADE >= 62
                   COMPUTE VALOR-BENEF = (0,6 + (TEMPO-CONTRIB - 15)
                           * 0,02) * MEDIA-SALARIAL
                   DISPLAY 'APOSENTADORIA MENSAL..:R$ ' VALOR-BENEF
                 ELSE
                   PERFORM 0002-MENSAGEM-ERRO
                 END-IF
               WHEN OTHER
                 DISPLAY '*** SEXO DO CONTRIBUINTE INVALIDO ***'
                 PERFORM 0100-REGRA-PONTOS
             END-EVALUATE.
      *>  --------------------------------------------------------------
       0200-REGRA-IDADE.
      *>  --------------------------------------------------------------
             DISPLAY 'APOSENTADORIA POR IDADE'
             EVALUATE SEXO
               WHEN 'M'
                 IF IDADE >= 65 AND TEMPO-CONTRIB >= 15
                   COMPUTE VALOR-BENEF = MEDIA-SALARIAL * 0,7
                   DISPLAY 'APOSENTADORIA MENSAL..:R$ ' VALOR-BENEF
                 ELSE
                   PERFORM 0002-MENSAGEM-ERRO
                 END-IF
               WHEN 'F'
                 IF IDADE >= 62 AND TEMPO-CONTRIB >= 15
                   COMPUTE VALOR-BENEF = MEDIA-SALARIAL * 0,7
                   DISPLAY 'APOSENTADORIA MENSAL..:R$ ' VALOR-BENEF
                 ELSE
                   PERFORM 0002-MENSAGEM-ERRO
                 END-IF
      *>          WHEN OTHER
      *>            DISPLAY '*** SEXO DO CONTRIBUINTE INVALIDO ***'
      *>            PERFORM 0100-REGRA-IDADE
             END-EVALUATE.
      *>  --------------------------------------------------------------
       0002-MENSAGEM-ERRO.
      *>  --------------------------------------------------------------
             DISPLAY '----------------------------------------'.
             DISPLAY 'PEDIDO DE APOSENTADORIA RECUSADO'.
             DISPLAY '----------------------------------------'.
      *>  --------------------------------------------------------------
      *>  O QUE PODE MELHORAR?
      *> 1 - ATRIBUIR VARIAVEIS NO LUGAR DOS NUMEROS SOLTOS PARA FACILITAR
      *>  A MANUTENÇÃO DO CODIGO
      *> 2 - IMPLEMENTAR UMA FUNCIONALIDADE QUE IMPEÇA O CONTRIBUINTE DE
      *>  RECEBER MENOS QUE UM SALARIO MINIMO E MAIS DO QUE O TETO DA
      *>  APOSENTADORIA (ATRIBUIR VARIAVEIS DE MINIMO E MAXIMO)
      *> 3 - O CODIGO ESTÁ PRECISANDO DE VALIDAÇÃO DE ENTRADAS
