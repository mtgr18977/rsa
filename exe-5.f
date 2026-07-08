C ======================================================================
C  Exercicio 5  (Cap. 3, ex. 13) - Menor x com pi1(x) > pi3(x).
C  Contra a intuicao, esse x demora a aparecer (x = 26861).
C ======================================================================
      program menor_x_pi1_pi3
      implicit none
      integer, parameter :: lim = 50000
      integer*1 comp(0:lim)
      integer   i, j, x, c1, c3
      logical   achou

      do i = 0, lim
        comp(i) = 0
      end do
      comp(0) = 1
      comp(1) = 1
      i = 2
      do while (i*i .le. lim)
        if (comp(i) .eq. 0) then
          j = i*i
          do while (j .le. lim)
            comp(j) = 1
            j = j + i
          end do
        end if
        i = i + 1
      end do

      c1 = 0
      c3 = 0
      achou = .false.
      do x = 2, lim
        if (comp(x) .eq. 0) then
          if (mod(x, 4) .eq. 1) c1 = c1 + 1
          if (mod(x, 4) .eq. 3) c3 = c3 + 1
        end if
        if (c1 .gt. c3) then
          write(*,'(A,I0)') 'menor x com pi1(x) > pi3(x): x = ', x
          write(*,'(A,I0,A,I0)') 'pi1(x) = ', c1, '   pi3(x) = ', c3
          achou = .true.
          exit
        end if
      end do
      if (.not. achou)
     &  write(*,'(A)') 'Nenhum x no limite satisfaz a desigualdade.'
      end
