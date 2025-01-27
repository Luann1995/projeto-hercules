       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO02.
      ******************************************************
      ***   AREA DE COMENTARIOS - REMARKS
      ***   OBJETIVO DO PROGRAMA = CALCULADORA DE AREA E VOLUME DE
      ***   SOLIDOS GEOMETRICOS
      ***   AUTOR: LUANN
      ***   DATA : XX/XX/20XX
      ******************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WRK-AREA         PIC 9(04)V99     VALUE ZEROS.
       77 WRK-VOLUME       PIC 9(04)V99     VALUE ZEROS.
       77 WRK-RAIO         PIC 9(04)V99     VALUE ZEROS.
       77 WRK-PI           PIC 9(04)V9999   VALUE 3,1415.
       77 SELEC-MENU       PIC 9(1)         VALUE ZEROS.
       77 REPETIR          PIC X(1)         VALUE SPACES.
      *>  --------------------------------------------------------------
       PROCEDURE DIVISION.
            PERFORM 0001-ROTINA-PRINCIPAL.



           STOP RUN.
      *>  --------------------------------------------------------------
       0001-ROTINA-PRINCIPAL.
      *>  --------------------------------------------------------------
             DISPLAY '------------------------------------'.
             DISPLAY 'CALCULADORA DE FORMAS GEOMETRICAS'.
             DISPLAY '------------------------------------'.
             DISPLAY '1 - AREA E VOLUME DA ESFERA'.
             DISPLAY '2 - AREA E VOLUME DO CUBO'.
             DISPLAY 'SELECIONE SUA OPCAO...:' ACCEPT SELEC-MENU.
             EVALUATE SELEC-MENU
               WHEN 1
                 DISPLAY 'DIGITE O RAIO DA ESFERA EM CM..... : '
                 ACCEPT WRK-RAIO
                 PERFORM 0100-CALC-AREA-ESFERA
                 PERFORM 0200-CALC-VOLUME-ESFERA
               WHEN 2
                 DISPLAY 'DIGITE O LADO DO CUBO EM CM..... : '
                 ACCEPT WRK-RAIO
                 PERFORM 0300-CALC-AREA-CUBO
                 PERFORM 0400-CALC-VOLUME-CUBO
               WHEN OTHER
                 DISPLAY '* OPCAO INVALIDA *'
                 PERFORM 0001-ROTINA-PRINCIPAL
             END-EVALUATE.
             DISPLAY '------------------------------------'.
             DISPLAY 'DESEJA REPETIR (S/N)?'
             ACCEPT REPETIR
             IF REPETIR = 'S'
               PERFORM 0001-ROTINA-PRINCIPAL
             END-IF.
      *>  --------------------------------------------------------------
       0100-CALC-AREA-ESFERA.
      *>  --------------------------------------------------------------
      *>   FORMULA PARA CALCULAR A AREA
             COMPUTE WRK-AREA = 4 * WRK-PI * (WRK-RAIO * WRK-RAIO).
             DISPLAY '-------------------------------'
             DISPLAY 'AREA DA ESFERA...: ' WRK-AREA ' CM QUADRADOS'.
      *>  --------------------------------------------------------------
       0200-CALC-VOLUME-ESFERA.
      *>  --------------------------------------------------------------
      *>   FORMULA PARA CALCULAR O VOLUME
      *>   TENTAR ADICIONAR UM OPERADOR DE POTENCIA
             COMPUTE WRK-VOLUME = (4/3) * WRK-PI * WRK-RAIO * WRK-RAIO
                                   * WRK-RAIO.
             DISPLAY '-------------------------------'
             DISPLAY 'VOLUME DA ESFERA...: ' WRK-VOLUME ' CM CUBICOS'.
      *>  --------------------------------------------------------------
       0300-CALC-AREA-CUBO.
      *>  --------------------------------------------------------------
             *>   FORMULA PARA CALCULAR A AREA
             COMPUTE WRK-AREA = 6 * (WRK-RAIO * WRK-RAIO).
             DISPLAY '-------------------------------'
             DISPLAY 'AREA DO CUBO...: ' WRK-AREA ' CM QUADRADOS'.
      *>  --------------------------------------------------------------
       0400-CALC-VOLUME-CUBO.
      *>  --------------------------------------------------------------
             *>   FORMULA PARA CALCULAR O VOLUME
             COMPUTE WRK-VOLUME = WRK-RAIO * WRK-RAIO * WRK-RAIO.
             DISPLAY '------------------------------   -'
             DISPLAY 'VOLUME DO CUBO...: ' WRK-VOLUME ' CM CUBICOS'.
      *>  --------------------------------------------------------------
      *>   DESAFIOS EXTRAS:
      *>   1- ADICIONAR OUTRAS FORMAR GEOMETRICAS
      *>   2- CRIAR UM BOOK PARA AS VARIAVEIS
      *>   3- MELHORAR A FORMATAÇÃO DAS SAIDAS DE DADOS
      *>   4- O DESENVOLVEDOR DESSE CÓDIGO NÃO SABIA QUE EXISTIA-
      *>   POTENCIAÇÃO EM COBOL (**), REFATORE AS FORMULAS DOS CALCULOS
