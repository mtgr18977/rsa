C ======================================================================
C  Exercicio 13  (Cap. 6, ex. 10) - Menor pseudoprimo forte para uma
C  base dada, via teste de Miller. Para a base 2 e n = 2047.
C ======================================================================
      program pseudo_forte
      implicit none
      integer, parameter :: lim = 100000
      integer*1 comp(0:lim)
      integer   i, j
      integer*8 b, n, q, k
      logical   millerinc, achou

      b = 2

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

      achou = .false.
      do n = 9, lim, 2
        if (comp(n) .eq. 1) then
          if (millerinc(n, b)) then
            q = n - 1
            k = 0
            do while (mod(q, 2_8) .eq. 0)
              q = q / 2
              k = k + 1
            end do
            write(*,'(A,I0,A,I0)') 'menor pseudoprimo forte base ',
     &            b, ' : n = ', n
            write(*,'(A,I0,A,I0)') 'n-1 = 2^', k, ' . ', q
            achou = .true.
            exit
          end if
        end if
      end do
      if (.not. achou) write(*,'(A)') 'Nenhum encontrado no limite.'
      end

      logical function millerinc(n, base)
      implicit none
      integer*8 powmod
      integer*8 n, base, q, k, t, i
      q = n - 1
      k = 0
      do while (mod(q, 2_8) .eq. 0)
        q = q / 2
        k = k + 1
      end do
      t = powmod(mod(base, n), q, n)
      if (t .eq. 1 .or. t .eq. n - 1) then
        millerinc = .true.
        return
      end if
      do i = 1, k - 1
        t = mod(t * t, n)
        if (t .eq. n - 1) then
          millerinc = .true.
          return
        end if
      end do
      millerinc = .false.
      end

      integer*8 function powmod(base, expo, m)
      implicit none
      integer*8 base, expo, m, b, e, r
      b = mod(base, m)
      e = expo
      r = 1
      do while (e .gt. 0)
        if (iand(e, 1_8) .eq. 1) r = mod(r * b, m)
        b = mod(b * b, m)
        e = ishft(e, -1)
      end do
      powmod = r
      end
