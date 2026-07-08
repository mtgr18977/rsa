C ======================================================================
C  Exercicio 6  (Apendice) - Potenciacao modular a^k mod n.
C  Algoritmo de exponenciacao binaria (uma multiplicacao por bit).
C ======================================================================
      program potencia_modular
      implicit none
      integer*8 powmod
      integer*8 a, k, n

      a = 7
      k = 123456789
      n = 1000000007

      write(*,'(A,I0,A,I0,A,I0,A,I0)')
     &  'forma reduzida: ', a, '^', k, ' mod ', n,
     &  ' = ', powmod(a, k, n)
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
