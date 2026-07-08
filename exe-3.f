C ======================================================================
C  Exercicio 3  (Cap. 1, ex. 12) - Algoritmo de fatoracao de Fermat.
C  Escreve n impar como diferenca de quadrados n = x^2 - y^2.
C  Eficiente quando n tem dois fatores proximos.
C ======================================================================
      program fermat_fatora
      implicit none
      integer*8 n, x, y2, y, f1, f2, passos, maxp
      logical achou

      n    = 2027651281
      maxp = 5000000

      if (mod(n, 2_8) .eq. 0) then
        write(*,'(A,I0,A,I0)') 'n par: ', n, ' = 2 x ', n/2
        stop
      end if

      x = ceiling(sqrt(dble(n)))
      if (x*x .eq. n) then
        write(*,'(I0,A,I0)') n, ' e quadrado perfeito, raiz ', x
        stop
      end if

      passos = 0
      achou  = .false.
      do while (passos .lt. maxp .and. .not. achou)
        y2 = x*x - n
        y  = int(sqrt(dble(y2)), 8)
        if ((y-1)*(y-1) .eq. y2) y = y - 1
        if ((y+1)*(y+1) .eq. y2) y = y + 1
        if (y*y .eq. y2) then
          f1 = x - y
          f2 = x + y
          if (f1 .eq. 1) then
            write(*,'(I0,A)') n, ' e primo (par trivial 1 x n).'
          else
            write(*,'(I0,A,I0,A,I0)') n, ' = ', f1, ' x ', f2
            write(*,'(A,I0)') 'passos do metodo de Fermat: ',
     &            passos + 1
          end if
          achou = .true.
        end if
        x = x + 1
        passos = passos + 1
      end do
      if (.not. achou)
     &  write(*,'(A)') 'Limite de passos atingido sem conclusao.'
      end
