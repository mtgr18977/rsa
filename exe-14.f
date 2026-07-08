C ======================================================================
C  Exercicio 14  (Cap. 6, ex. 11) - Pseudoprimos da forma p^2 para a
C  base 2. Usa a dica 2^(p^2) = (2^p)^p (mod p^2). mulmod binario.
C ======================================================================
      program pseudo_quadrado
      implicit none
      integer*8 powmod
      integer, parameter :: r = 50000
      integer*1 comp(0:r)
      integer   i, j, p
      integer*8 pp, m, t, testados

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

      testados = 0
      write(*,'(A)') 'valores de p com p^2 pseudoprimo base 2:'
      do p = 3, r, 2
        if (comp(p) .eq. 0) then
          testados = testados + 1
          pp = int(p, 8)
          m  = pp * pp
          t  = powmod(2_8, pp, m)
          if (powmod(t, pp, m) .eq. 2) then
            write(*,'(A,I0,A,I0)') '  p = ', p, '   p^2 = ', m
          end if
        end if
      end do
      write(*,'(A,I0)') 'primos testados: ', testados
      end

      integer*8 function mulmod(a, b, m)
      implicit none
      integer*8 a, b, m, aa, bb, r
      aa = mod(a, m)
      bb = mod(b, m)
      r = 0
      do while (bb .gt. 0)
        if (iand(bb, 1_8) .eq. 1) r = mod(r + aa, m)
        aa = mod(aa + aa, m)
        bb = ishft(bb, -1)
      end do
      mulmod = r
      end

      integer*8 function powmod(base, expo, m)
      implicit none
      integer*8 mulmod
      integer*8 base, expo, m, b, e, r
      b = mod(base, m)
      e = expo
      r = 1
      do while (e .gt. 0)
        if (iand(e, 1_8) .eq. 1) r = mulmod(r, b, m)
        b = mulmod(b, b, m)
        e = ishft(e, -1)
      end do
      powmod = r
      end
