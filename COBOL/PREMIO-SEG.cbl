       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO08.
      ******************************************************
      ***   AREA DE COMENTARIOS - REMARKS
      ***   OBJETIVO DO PROGRAMA = SISTEMA QUE CALCULA O PREMIO DE UM-
      ***   SEGURO AUTOMOTIVO
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
      *>  PARAMETRO PARA FAZER A MEDIA DO PREMIO (CONTAGEM DAS PERGUNTAS)
       77 CONT-PARAM       PIC 9(1)         VALUE 5.
       77 MEDIA            PIC 9(2)V9       VALUE ZEROS.
       77 VALOR-VEICULO    PIC 9(6)V99      VALUE ZEROS.
       77 PREMIO           PIC 9(4)V99      VALUE ZEROS.
      *>  --------------------------------------------------------------
       PROCEDURE DIVISION.
             PERFORM 0001-ROTINA-PRINCIPAL.

           STOP RUN.
      *>  --------------------------------------------------------------
       0001-ROTINA-PRINCIPAL.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'CALCULADORA DE PREMIO DE SEGURO AUTOMOTIVO'
             DISPLAY '---------------------------------------'.
             PERFORM 0100-IDADE.
             PERFORM 0200-GENERO.
             PERFORM 0300-ESTADO-CIVIL.
             PERFORM 0400-HISTORICO.
             PERFORM 0500-TIPO-VEICULO.
             PERFORM 0600-CALCULA-PREMIO.
             display 'repetir' ACCEPT selec-menu.
             if selec-menu = 1
                 move 0 to contador
                 PERFORM 0001-ROTINA-PRINCIPAL
             end-if.

      *>  --------------------------------------------------------------
       0100-IDADE.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'SELECIONE A IDADE DO CLIENTE'.
             DISPLAY '1 - ENTRE 18 E 29 ANOS'.
             DISPLAY '2 - ENTRE 30 E 59 ANOS'.
             DISPLAY '3 - MAIS DE 60 ANOS'.
             DISPLAY 'SELECIONE SUA OPCAO...:' ACCEPT SELEC-MENU.
             EVALUATE SELEC-MENU
               WHEN 1
                 ADD 2 TO CONTADOR
               WHEN 2
                 ADD 1 TO CONTADOR
               WHEN 3
                 ADD 1 TO CONTADOR
               WHEN OTHER
                 DISPLAY '*** SELECIONE A OPCAO CORRETA ***'
                 PERFORM 0100-IDADE
               END-EVALUATE.
      *>  --------------------------------------------------------------
       0200-GENERO.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'SELECIONE O GENERO DO CLIENTE'.
             DISPLAY '1 - MASCULINO'.
             DISPLAY '2 - FEMININO'.
             DISPLAY 'SELECIONE SUA OPCAO...:' ACCEPT SELEC-MENU.
             EVALUATE SELEC-MENU
               WHEN 1
                 ADD 2 TO CONTADOR
               WHEN 2
                 ADD 1 TO CONTADOR
               WHEN OTHER
                 DISPLAY '*** SELECIONE A OPCAO CORRETA ***'
                 PERFORM 0200-GENERO
               END-EVALUATE.
      *>  --------------------------------------------------------------
       0300-ESTADO-CIVIL.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'SELECIONE O ESTADO CIVIL DO CLIENTE'.
             DISPLAY '1 - CASADO(A)'.
             DISPLAY '2 - VIUVO(A)'
             DISPLAY '3 - SOLTEIRO(A)'.
             DISPLAY 'SELECIONE SUA OPCAO...:' ACCEPT SELEC-MENU.
             EVALUATE SELEC-MENU
               WHEN 1
                 ADD 1 TO CONTADOR
               WHEN 2
                 ADD 2 TO CONTADOR
               WHEN 3
                 ADD 2 TO CONTADOR
               WHEN OTHER
                 DISPLAY '*** SELECIONE A OPCAO CORRETA ***'
                 PERFORM 0300-ESTADO-CIVIL
               END-EVALUATE.
      *>  --------------------------------------------------------------
       0400-HISTORICO.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'SELECIONE O HISTORICO DE DIRECAO DO CLIENTE'.
             DISPLAY '1 - BOM (POUCAS MULTAS)'.
             DISPLAY '2 - RUIM (MUITAS MULTAS)'.
             DISPLAY 'SELECIONE SUA OPCAO...:' ACCEPT SELEC-MENU.
             EVALUATE SELEC-MENU
               WHEN 1
                 ADD 1 TO CONTADOR
               WHEN 2
                 ADD 2 TO CONTADOR
               WHEN OTHER
                 DISPLAY '*** SELECIONE A OPCAO CORRETA ***'
                 PERFORM 0400-HISTORICO
               END-EVALUATE.
       *>  --------------------------------------------------------------
       0500-TIPO-VEICULO.
      *>  --------------------------------------------------------------
             DISPLAY '---------------------------------------'.
             DISPLAY 'SELECIONE O TIPO DE VEICULO DO CLIENTE'.
             DISPLAY '1 - POPULAR'.
             DISPLAY '2 - LUXO'.
             DISPLAY 'SELECIONE SUA OPCAO...:' ACCEPT SELEC-MENU.
             EVALUATE SELEC-MENU
               WHEN 1
                 ADD 1 TO CONTADOR
               WHEN 2
                 ADD 2 TO CONTADOR
               WHEN OTHER
                 DISPLAY '*** SELECIONE A OPCAO CORRETA ***'
                 PERFORM 0500-TIPO-VEICULO
               END-EVALUATE.
      *>  --------------------------------------------------------------
       0600-CALCULA-PREMIO.
      *>  --------------------------------------------------------------
             DISPLAY 'DIGITE O VALOR DO VEICULO DO CLIENTE...:R$ '.
             ACCEPT VALOR-VEICULO.
             COMPUTE MEDIA = CONTADOR / CONT-PARAM.
             EVALUATE MEDIA
               WHEN 1
                 COMPUTE PREMIO = VALOR-VEICULO * 0,01
                 DISPLAY '-----------------------------------'
                 DISPLAY 'VALOR MENSAL DO SEGURO...:R$ ' PREMIO
                 DISPLAY '-----------------------------------'
               WHEN > 1 AND < 2
                 COMPUTE PREMIO = VALOR-VEICULO * 0,03
                 DISPLAY '-----------------------------------'
                 DISPLAY 'VALOR MENSAL DO SEGURO...:R$ ' PREMIO
                 DISPLAY '-----------------------------------'
               WHEN OTHER
                 COMPUTE PREMIO = VALOR-VEICULO * 0,05
                 DISPLAY '-----------------------------------'
                 DISPLAY 'VALOR MENSAL DO SEGURO...:R$ ' PREMIO
                 DISPLAY '-----------------------------------'
              END-EVALUATE.
      *>  --------------------------------------------------------------
      *>  O QUE PODE MELHORAR?
      *>  1- O CÓDIGO POSSUI MUITA REDUNDANCIA NA PARTE EVALUATE SELEC
      *>  MENU DOS CRITÉRIO, CRIE UM FUNÇÃO PARA CHAMAR O EVALUATE E
      *>  EVITAR REPETIÇAO DE CÓDIGO DESNECESSÁRIO
      *>  2- o banco não quer fazer seguro para carros com valores meno-
      *>  res que R$ 35.000,00 e maiores que R$150.000,00 faça uma vali-
      *>  dação para não aceitar os dados invalidos e dispare uma mensagem
