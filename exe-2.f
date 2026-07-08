C ======================================================================
C  Exercicio 2  (Cap. 1, ex. 11) - Numeros altamente compostos < r.
C  Um numero e altamente composto se tem mais divisores que qualquer
C  inteiro positivo menor. Lista tambem a fatoracao de cada um.
C ======================================================================
      program altamente_compostos
      implicit none
      integer*8 ndiv
      integer*8 n, r, d, recorde

      r = 5000
      recorde = 0
      write(*,'(A)') '       n | d(n) | fatoracao'
      do n = 1, r - 1
        d = ndiv(n)
        if (d .gt. recorde) then
          recorde = d
          write(*,'(I8,A,I4,A)', advance='no') n, ' | ', d, ' | '
          call fatora(n)
        end if
      end do
      write(*,*)
      write(*,'(A)') 'Observe: expoentes nao crescentes; usa sempre'
      write(*,'(A)') 'os menores primos.'
      end

      integer*8 function ndiv(n)
      implicit none
      integer*8 n, i, d
      d = 0
      i = 1
      do while (i*i .le. n)
        if (mod(n, i) .eq. 0) then
          if (i*i .eq. n) then
            d = d + 1
          else
            d = d + 2
          end if
        end if
        i = i + 1
      end do
      ndiv = d
      end

      subroutine fatora(n0)
      implicit none
      integer*8 n0, n, p, e
      n = n0
      if (n .eq. 1) then
        write(*,'(A)') '1'
        return
      end if
      p = 2
      do while (p*p .le. n)
        if (mod(n, p) .eq. 0) then
          e = 0
          do while (mod(n, p) .eq. 0)
            n = n / p
            e = e + 1
          end do
          if (e .gt. 1) then
            write(*,'(I0,A,I0,A)', advance='no') p, '^', e, '  '
          else
            write(*,'(I0,A)', advance='no') p, '  '
          end if
        end if
        p = p + 1
      end do
      if (n .gt. 1) write(*,'(I0)', advance='no') n
      write(*,*)
      end
