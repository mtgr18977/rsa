C ======================================================================
C  Exercicio 20  (Cap. 9, ex. 10) - Qual numero de Fermat F(m) e
C  divisivel por q = k*2^n + 1. Calcula 2^(2^m) mod q por quadraturas
C  sucessivas; se der q-1, entao q | F(m). E m < n.
C ======================================================================
      program fermat_divide
      implicit none
      integer*8 q, k, s
      integer   n, m
      logical   achou

      q = 2424833

      k = q - 1
      n = 0
      do while (mod(k, 2_8) .eq. 0)
        k = k / 2
        n = n + 1
      end do
      write(*,'(A,I0,A,I0,A,I0,A)') 'q = ', q, ' = ', k, ' . 2^', n,
     &      ' + 1'

      s = mod(2_8, q)
      achou = .false.
      do m = 0, n - 1
        if (s .eq. q - 1) then
          write(*,'(A,I0,A,I0,A)') 'q divide F(', m, ') = 2^(2^',
     &          m, ') + 1'
          achou = .true.
          exit
        end if
        s = mod(s * s, q)
      end do
      if (.not. achou)
     &  write(*,'(A,I0)') 'q nao divide nenhum F(m) com m < ', n
      end
