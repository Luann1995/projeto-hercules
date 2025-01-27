       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO04.
      ******************************************************
      ***   AREA DE COMENTARIOS - REMARKS
      ***   OBJETIVO DO PROGRAMA = SISTEMA DE APTDAO PROFISSIONAL
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
      *>  --------------------------------------------------------------
       PROCEDURE DIVISION.
             PERFORM 0001-ROTINA-PRINCIPAL.

           STOP RUN.
      *>  --------------------------------------------------------------
       0001-ROTINA-PRINCIPAL.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'SISTEMA DE APTDAO PROFISSIONAL'
             DISPLAY '---------------------------------------'.
             PERFORM 0100-QUESTIONARIO.
      *>  --------------------------------------------------------------
       0100-QUESTIONARIO.
      *>  --------------------------------------------------------------
             DISPLAY 'GOSTA DE PROGRAMAR?'.
             DISPLAY '1 - SIM'
             DISPLAY '2 - NAO'
             ACCEPT SELEC-MENU.
             IF SELEC-MENU = 2
               DISPLAY 'MELHOR AREA PARA VOCE.: * AGILE *'
               ELSE
                 DISPLAY 'PREFERE QUAL AREA?'
                 DISPLAY '1 - BACKEND'
                 DISPLAY '2 - FRONTEND'
                 ACCEPT SELEC-MENU
                 IF SELEC-MENU = 2
                   DISPLAY 'JA USOU HTML E GOSTOU?'
                   DISPLAY '1 - SIM'
                   DISPLAY '2 - NAO USO DROGAS'
                   ACCEPT SELEC-MENU
                   IF SELEC-MENU = 2
                     DISPLAY 'MELHOR AREA PARA VOCE.: * MOBILE *'
                   ELSE
                     DISPLAY 'MELHOR AREA PARA VOCE.: * WEB *'
                   END-IF
                 ELSE
                   DISPLAY 'QUAL A SUA FAIXA ETARIA?'
                   DISPLAY '1 - TENHO MENOS DE 40 ANOS'
                   DISPLAY '2 - TENHO MAIS DE 40 ANOS'
                   ACCEPT SELEC-MENU
                   IF SELEC-MENU = 2
                     DISPLAY 'MELHOR AREA PARA VOCE.: * MAINFRAME *'
                   ELSE
                     DISPLAY 'MELHOR AREA PARA VOCE.: * CLOUD *'
                   END-IF
                 END-IF
             END-IF.
      *>  --------------------------------------------------------------
      *>  O QUE PODE MELHORAR?
      *> 1 - INSIRA VALIDAÇÃO DE DADOS NAS ENTRADAS
      *> 2 - INCLUA MAIS AREAS DE ATUAÇÃO COMO DADOS E CYBERSEC
      *> 3 - INCLUA UM NUMERO LIMITADOS DE VAGAS PARA CARA AREA E CRIE UM-
      *>  METODO DE ALOCAÇÃO AUTOMATICO PARA OUTRA VAGA
