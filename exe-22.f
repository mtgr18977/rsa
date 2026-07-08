C ======================================================================
C  Exercicio 22  (Cap. 10, ex. 11) - Teste de primalidade de Wilson:
C  n e primo sse (n-1)! = -1 (mod n). Reduz mod n a cada produto e
C  cronometra o maior primo abaixo de 10^k, k = 1..6.
C ======================================================================
      program wilson
      implicit none
      integer*8 ac, nn, i, dez
      integer   k, kmax
      logical   ehprimo
      real*8    t0, t1, dt, ant

      kmax = 6
      ant  = -1.0d0
      write(*,'(A)') 'k |     primo n | (n-1)! mod n | primo? | ms'
      do k = 1, kmax
        dez = 10_8 ** k
        nn = dez - 1
        do while (.not. ehprimo(nn))
          nn = nn - 1
        end do

        call cpu_time(t0)
        ac = 1
        do i = 2, nn - 1
          ac = mod(ac * i, nn)
        end do
        call cpu_time(t1)
        dt = (t1 - t0) * 1000.0d0

        write(*,'(I1,A,I11,A,I12,A,L2,A,F9.3)') k, ' | ', nn,
     &        ' | ', ac, ' | ', (ac .eq. nn - 1), ' | ', dt
        ant = dt
      end do
      write(*,'(A)') 'Tempo ~ n ~ 10^k: multiplica por ~10 a cada k.'
      end

      logical function ehprimo(m)
      implicit none
      integer*8 m, i
      if (m .lt. 2) then
        ehprimo = .false.
        return
      end if
      i = 2
      do while (i*i .le. m)
        if (mod(m, i) .eq. 0) then
          ehprimo = .false.
          return
        end if
        i = i + 1
      end do
      ehprimo = .true.
      end
