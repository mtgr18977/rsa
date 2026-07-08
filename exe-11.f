C ======================================================================
C  Exercicio 11  (Cap. 6, ex. 8) - Pseudoprimos para as bases 2 e 3.
C  Impares compostos n com 2^(n-1)=1 e 3^(n-1)=1 (mod n).
C  Marca tambem os que sao numeros de Carmichael.
C ======================================================================
      program pseudo_2_3
      implicit none
      integer*8 powmod
      integer, parameter :: r = 100000
      integer*1 comp(0:r)
      integer   i, j
      integer*8 n, total, carm
      logical   ehcarm

      do i = 0, r
        comp(i) = 0
      end do
      comp(0) = 1
      comp(1) = 1
      i = 2
      do while (i*i .le. r)
        if (comp(i) .eq. 0) then
          j = i*i
          do while (j .le. r)
            comp(j) = 1
            j = j + i
          end do
        end if
        i = i + 1
      end do

      total = 0
      carm  = 0
      write(*,'(A)') '      n | Carmichael?'
      do n = 9, r, 2
        if (comp(n) .eq. 1) then
          if (powmod(2_8, n-1, n) .eq. 1 .and.
     &        powmod(3_8, n-1, n) .eq. 1) then
            total = total + 1
            if (ehcarm(n)) then
              carm = carm + 1
              write(*,'(I7,A)') n, ' | sim'
            else
              write(*,'(I7,A)') n, ' | nao'
            end if
          end if
        end if
      end do
      write(*,*)
      write(*,'(A,I0)') 'pseudoprimos base 2 e 3 : ', total
      write(*,'(A,I0)') 'dos quais Carmichael    : ', carm
      end

      logical function ehcarm(n)
      implicit none
      integer*8 n, m, p, e, nf, fat(64)
      integer   i
      m  = n
      nf = 0
      p  = 2
      do while (p*p .le. m)
        if (mod(m, p) .eq. 0) then
          e = 0
          do while (mod(m, p) .eq. 0)
            m = m / p
            e = e + 1
          end do
          if (e .gt. 1) then
            ehcarm = .false.
            return
          end if
          nf = nf + 1
          fat(nf) = p
        end if
        p = p + 1
      end do
      if (m .gt. 1) then
        nf = nf + 1
        fat(nf) = m
      end if
      if (nf .lt. 3) then
        ehcarm = .false.
        return
      end if
      do i = 1, int(nf)
        if (mod(n - 1, fat(i) - 1) .ne. 0) then
          ehcarm = .false.
          return
        end if
      end do
      ehcarm = .true.
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
