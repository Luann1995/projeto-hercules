       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO09.
      ******************************************************
      ***   AREA DE COMENTARIOS - REMARKS
      ***   OBJETIVO DO PROGRAMA = SISTEMA IMC
      ***   AUTOR: LUANN
      ***   DATA : XX/XX/20XX
      ******************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WRK-PESO            PIC 9(03)V99    VALUE ZEROS.
       77 WRK-ALTURA          PIC 9(02)V99    VALUE ZEROS.
       77 WRK-IMC             PIC 9(02)V9     VALUE ZEROS.
       77 WRK-PESO-IDEAL      PIC 9(03)V99    VALUE ZEROS.
       77 WRK-GANHA-PESO      PIC 9(02)V99    VALUE ZEROS.
       77 WRK-PERDE-PESO      PIC 9(02)V99    VALUE ZEROS.
      *>  -------------------------------------------------------------]

        PROCEDURE DIVISION.
           PERFORM 0001-ROTINA-PRINCIPAL.
           STOP RUN.
      *>  -------------------------------------------------------------
       0001-ROTINA-PRINCIPAL.
      *>  -------------------------------------------------------------
           PERFORM 0100-EXIBIR.
           PERFORM 0200-IMC.
           PERFORM 0300-PESO-IDEAL.
      *>   ------------------------------------------------------------
       0100-EXIBIR.
      *>  -------------------------------------------------------------
            DISPLAY 'DIGITE O PESO DO ALUNO EM KG..... : '.
             ACCEPT WRK-PESO.
           DISPLAY 'DIGITE A ALTURA DO ALUNO EM METROS...... : '.
             ACCEPT WRK-ALTURA.
           DISPLAY  '--------- SAIDA DE DADOS ------------'.
             DISPLAY 'PESO : '   WRK-PESO.
             DISPLAY 'ALTURA : ' WRK-ALTURA.
      *>   ------------------------------------------------------------
       0200-IMC.
      *>  -------------------------------------------------------------
      *********** OPERACAO IMC - COMANDO COMPUTE *************
             COMPUTE WRK-IMC = WRK-PESO / WRK-ALTURA ** 2.
             DISPLAY '----------------------------------'
             DISPLAY 'IMC DO ALUNO...: ' WRK-IMC.

      *********** DEFINIR STATUS - COMANDO IF *******************
             IF WRK-IMC < 16
                DISPLAY '--------------------------------------------'
                DISPLAY 'ABAIXO DO PESO IDEAL'
                DISPLAY 'INGERIR MAIS CARBOIDRATOS'
                  ELSE
                      IF WRK-IMC > 16 AND WRK-IMC < 25
                          DISPLAY '----------------------------------'
                          DISPLAY 'PESO IDEAL'
                          DISPLAY 'MANTER A DIETA'
                      ELSE
                          DISPLAY '----------------------------------'
                          DISPLAY 'ACIMA DO PESO IDEAL'
                          DISPLAY 'INGERIR MENOS CARBOIDRATOS'
                      END-IF
              END-IF.
      *>   ------------------------------------------------------------
      *>   MELHORAR ESSE MODULO, ESTÁ CALCULANDO OS DOIS PESOS MAS
      *>   SO RETORNA UM, TAÍ UM DESAFIO
      *>  -------------------------------------------------------------
       0300-PESO-IDEAL.
      *>  -------------------------------------------------------------
            COMPUTE WRK-PESO-IDEAL = (WRK-ALTURA ** 2) * 25.
            COMPUTE WRK-GANHA-PESO = WRK-PESO-IDEAL - WRK-PESO.
            COMPUTE WRK-PERDE-PESO = WRK-PESO - WRK-PESO-IDEAL.
            IF WRK-PESO > WRK-PESO-IDEAL
                DISPLAY '----------------------------------'
                DISPLAY 'PRECISA PERDER: ' WRK-PERDE-PESO ' KG (IMC 25)'
            ELSE
                DISPLAY '----------------------------------'
                DISPLAY 'PRECISA GANHAR: ' WRK-GANHA-PESO ' KG (IMC 25)'
            END-IF.
      *>  -------------------------------------------------------------

      *>   O QUE PODE MELHORAR:
      *>   1-
