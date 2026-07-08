C ======================================================================
C  Exercicio 4  (Cap. 3, ex. 12) - Frequencia de primos 4n+1 vs 4n+3.
C  Usa o crivo de Eratostenes e tabela pi1(x), pi3(x) e a razao.
C  Sabe-se que a razao tende a 1 quando x -> infinito.
C ======================================================================
      program primos_4n1_4n3
      implicit none
      integer, parameter :: xmax = 100000
      integer, parameter :: passo = 20000
      integer*1 comp(0:xmax)
      integer   i, j, x, c1, c3, prox
      real*8    razao

      do i = 0, xmax
        comp(i) = 0
      end do
      comp(0) = 1
      comp(1) = 1
      i = 2
      do while (i*i .le. xmax)
        if (comp(i) .eq. 0) then
          j = i*i
          do while (j .le. xmax)
            comp(j) = 1
            j = j + i
          end do
        end if
        i = i + 1
      end do

      c1 = 0
      c3 = 0
      prox = passo
      write(*,'(A)') '        x |  pi1(x) |  pi3(x) | pi1/pi3'
      do x = 2, xmax
        if (comp(x) .eq. 0) then
          if (mod(x, 4) .eq. 1) c1 = c1 + 1
          if (mod(x, 4) .eq. 3) c3 = c3 + 1
        end if
        if (x .eq. prox .or. x .eq. xmax) then
          if (c3 .gt. 0) then
            razao = dble(c1) / dble(c3)
          else
            razao = 0.0d0
          end if
          write(*,'(I9,A,I8,A,I8,A,F8.5)') x, ' | ', c1,
     &          ' | ', c3, ' | ', razao
          prox = prox + passo
        end if
      end do
      end
