C ======================================================================
C  Exercicio 21  (Cap. 10, ex. 10) - Teste de Pepin para numeros de
C  Fermat: F(n) e primo sse, e so se, 3^((F-1)/2) = -1 (mod F).
C  (F-1)/2 = 2^(2^n - 1): basta elevar 3 ao quadrado (2^n - 1) vezes.
C  Limite n <= 5 (para F caber em 64 bits). mulmod binario.
C ======================================================================
      program pepin
      implicit none
      integer*8 mulmod
      integer*8 f, r
      integer   n, i, quad

      n = 5

      if (n .lt. 1 .or. n .gt. 5) then
        write(*,'(A)') 'Use 1 <= n <= 5 (limite de 64 bits).'
        stop
      end if

      f = ishft(1_8, ishft(1, n)) + 1
      write(*,'(A,I0,A,I0)') 'F(', n, ') = ', f

      r = mod(3_8, f)
      quad = ishft(1, n) - 1
      do i = 1, quad
        r = mulmod(r, r, f)
      end do

      write(*,'(A,I0)') 'quadraturas efetuadas: ', quad
      if (r .eq. f - 1) then
        write(*,'(A,I0,A)') 'F(', n, ') e PRIMO.'
      else
        write(*,'(A,I0,A)') 'F(', n, ') e COMPOSTO.'
      end if
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
