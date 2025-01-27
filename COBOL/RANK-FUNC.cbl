       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO07.
      ******************************************************
      ***   AREA DE COMENTARIOS - REMARKS
      ***   OBJETIVO DO PROGRAMA = SISTEMA PARA RANKEAR E PROMOVER FUNCIS
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
       77 CONT-PNT         PIC 9(3)         VALUE ZEROS.
      *>  77 STATUS-MENU      PIC X(2)         VALUE SPACE.
      *>  --------------------------------------------------------------
       PROCEDURE DIVISION.
             PERFORM 0001-ROTINA-PRINCIPAL.

           STOP RUN.
      *>  --------------------------------------------------------------
       0001-ROTINA-PRINCIPAL.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'SISTEMA DE PROMOCOES E RANQUEAMENTO'
             DISPLAY '---------------------------------------'.
             PERFORM 0100-TMP-EMPR.
             PERFORM 0200-FORM-ACDM.
             PERFORM 0300-CURS-INTER.
             PERFORM 0400-CERTIFIC.
             PERFORM 0600-CALC-PROMO.

      *>  --------------------------------------------------------------
       0100-TMP-EMPR.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'SELECIONE O TEMPO DE EMPRESA DO FUNCIONARIO'.
             DISPLAY '1 - 1 ANO OU MENOS'.
             DISPLAY '2 - ENTRE 2 E 4 ANOS'.
             DISPLAY '3 - ENTRE 5 E 9 ANOS'.
             DISPLAY '4 - ENTRE 10 E 19 ANOS'.
             DISPLAY '5 - MAIS DE 20 ANOS'.
             PERFORM 0700-ENTRA-DADOS.
      *>  --------------------------------------------------------------
       0200-FORM-ACDM.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'SELECIONE A FORMACAO ACADEMICA DO FUNCIONARIO'.
             DISPLAY '1 - ENSINO MEDIO'.
             DISPLAY '2 - GRADUACAO'.
             DISPLAY '3 - POS GRADUACAO'.
             DISPLAY '4 - MESTRADO'.
             DISPLAY '5 - DOUTORADO'.
             PERFORM 0700-ENTRA-DADOS.
      *>  --------------------------------------------------------------
       0300-CURS-INTER.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'CURSOS INTERNOS REALIZADOS PELO FUNCIONARIO'.
             DISPLAY '1 - ENTRE 2 E 10 CURSOS'.
             DISPLAY '2 - ENTRE 11 E 25 CURSOS'.
             DISPLAY '3 - ENTRE 26 E 39 CURSOS'.
             DISPLAY '4 - ENTRE 40 59 CURSOS'.
             DISPLAY '5 - MAIS DE 60 CURSO'.
             PERFORM 0700-ENTRA-DADOS.
      *>  --------------------------------------------------------------
        0400-CERTIFIC.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'QUANTAS CERTIFICACOES O FUNCIONARIO POSSUI'.
             DISPLAY '1 - 1 CERTIFICACAO'.
             DISPLAY '2 - 3 CERTIFICACOES'.
             DISPLAY '3 - 5 CERTIFICACOES'.
             DISPLAY '4 - 8 CERTIFICACOES'.
             DISPLAY '5 - MAIS DE 10 CERTIFICACOES'.
             PERFORM 0700-ENTRA-DADOS.
      *>  --------------------------------------------------------------
       0600-CALC-PROMO.
      *>  --------------------------------------------------------------
             DISPLAY '-----------------------------------'.
             DISPLAY 'PONTOS DO FUNCIONARIO.: ' CONT-PNT.
             EVALUATE CONT-PNT
               WHEN <= 20
                 DISPLAY 'MELHOR CARGO PARA O FUNCIONARO: * TRAINEE *'
               WHEN <= 40
                 DISPLAY 'MELHOR CARGO PARA O FUNCIONARO: * JUNIOR *'
               WHEN <= 60
                 DISPLAY 'MELHOR CARGO PARA O FUNCIONARO: * PLENO *'
               WHEN <= 90
                 DISPLAY 'MELHOR CARGO PARA O FUNCIONARO: * SENIOR *'
               WHEN <= 120
                 DISPLAY 'MELHOR CARGO PARA O FUNCIONARO: * GERENTE *'
             END-EVALUATE.
       *> --------------------------------------------------------------
       0700-ENTRA-DADOS.
      *>  --------------------------------------------------------------
             DISPLAY 'SELECIONE SUA OPCAO...:' ACCEPT SELEC-MENU.
             EVALUATE SELEC-MENU
               WHEN 1
                 ADD 1 TO CONT-PNT
               WHEN 2
                 ADD 5 TO CONT-PNT
               WHEN 3
                 ADD 10 TO CONT-PNT
               WHEN 4
                 ADD 20 TO CONT-PNT
               WHEN 5
                 ADD 30 TO CONT-PNT
               WHEN OTHER
                 DISPLAY '*** SELECIONE A OPCAO CORRETA ***'
                 PERFORM 0700-ENTRA-DADOS
               END-EVALUATE.
      *>  --------------------------------------------------------------
      *>  O QUE PODE MELHORAR?
      *>1- E SE O FUNCIONARIO TIVER 9 CERTIFICAÇOES? ALTERE A FUNÇAO DE
      *>   CERTIFICAÇOES PARA MOSTRAR CERTIFICAÇÕES ENTRE INTERVALOS
      *>2- ADICIONE UM QUINTO CRITÉRIO DE PROMOÇAO E ALTERE A PONTUAÇÃO
