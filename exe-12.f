C ======================================================================
C  Exercicio 12  (Cap. 6, ex. 9) - Numeros de Carmichael que sao
C  produto de d primos, todos < 1000. Usa a reducao mod (p-1) do
C  criterio de Korselt, mantendo os numeros pequenos.
C ======================================================================
      program carmichael_d
      implicit none
      integer, parameter :: lim = 1000
      integer*1 comp(0:lim)
      integer   primos(200), np
      integer   i, j, d
      integer   idx(8)

      d = 3

      do i = 0, lim
        comp(i) = 0
      end do
      comp(0) = 1
      comp(1) = 1
      i = 2
      do while (i*i .lt. lim)
        if (comp(i) .eq. 0) then
          j = i*i
          do while (j .lt. lim)
            comp(j) = 1
            j = j + i
          end do
        end if
        i = i + 1
      end do
      np = 0
      do i = 3, lim - 1, 2
        if (comp(i) .eq. 0) then
          np = np + 1
          primos(np) = i
        end if
      end do

      write(*,'(A,I0,A)') 'Numeros de Carmichael com ', d,
     &  ' fatores primos < 1000:'
      call combina(primos, np, d, idx)
      end

      recursive subroutine combina(primos, np, d, idx)
      implicit none
      integer primos(*), np, d, idx(*)
      call passo(primos, np, d, idx, 1, 1)
      end

      recursive subroutine passo(primos, np, d, idx, nivel, inicio)
      implicit none
      integer primos(*), np, d, idx(*), nivel, inicio
      integer i, k
      integer*8 resto, produto
      logical ok
      if (nivel .gt. d) then
        ok = .true.
        do i = 1, d
          resto = 1
          do k = 1, d
            resto = mod(resto * primos(idx(k)),
     &              int(primos(idx(i)) - 1, 8))
          end do
          if (resto .ne. 1) ok = .false.
        end do
        if (ok) then
          write(*,'(A)', advance='no') 'n = '
          do k = 1, d
            if (k .lt. d) then
              write(*,'(I0,A)', advance='no') primos(idx(k)), ' . '
            else
              write(*,'(I0)') primos(idx(k))
            end if
          end do
        end if
        return
      end if
      do i = inicio, np
        idx(nivel) = i
        call passo(primos, np, d, idx, nivel + 1, i + 1)
      end do
      end
