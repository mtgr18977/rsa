C ======================================================================
C  Exercicio 10  (Cap. 5, ex. 18) - Primos p com a^(p-1) = 1 (mod p^2).
C  Para a base 2 sao os primos de Wieferich: 1093 e 3511.
C  Usa mulmod binario porque o modulo p^2 pode ser grande.
C ======================================================================
      program primos_wieferich
      implicit none
      integer*8 powmod
      integer, parameter :: r = 30000
      integer*1 comp(0:r)
      integer   i, j, p
      integer*8 a, pp, m, achados, testados

      a = 2

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

      achados  = 0
      testados = 0
      write(*,'(A,I0,A,I0)') 'base a = ', a, ',  limite r = ', r
      do p = int(a) + 2, r - 1
        if (comp(p) .eq. 0) then
          testados = testados + 1
          pp = int(p, 8)
          m  = pp * pp
          if (powmod(mod(a, m), pp - 1, m) .eq. 1) then
            write(*,'(A,I0)') 'primo encontrado: p = ', p
            achados = achados + 1
          end if
        end if
      end do
      write(*,'(A,I0)') 'primos testados: ', testados
      write(*,'(A,I0)') 'total de primos de Wieferich: ', achados
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
