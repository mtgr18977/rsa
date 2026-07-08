C ======================================================================
C  Exercicio 19  (Cap. 9, ex. 9) - Fatores de numeros de Mersenne
C  M(p) = 2^p - 1 pelo metodo de Fermat. Candidatos q = 2rp+1 com
C  q^2 <= M(p). mulmod binario. Limite p <= 61.
C ======================================================================
      program mersenne
      implicit none
      integer*8 powmod
      integer*8 p, m, q, r, testados
      logical   ehprimo, achou

      p = 59

      if (.not. ehprimo(p)) then
        write(*,'(I0,A)') p, ' nao e primo.'
        stop
      end if
      if (p .gt. 61) then
        write(*,'(A)') 'Use p <= 61 (limite de 64 bits).'
        stop
      end if

      m = ishft(1_8, int(p)) - 1
      write(*,'(A,I0,A,I0)') 'M(', p, ') = ', m
      testados = 0
      achou = .false.
      r = 1
      do while (.true.)
        q = 2 * p * r + 1
        if (q*q .gt. m) then
          write(*,'(A,I0)') 'candidatos testados: ', testados
          write(*,'(A,I0,A)') 'M(', p, ') e PRIMO.'
          achou = .true.
          exit
        end if
        testados = testados + 1
        if (powmod(2_8, p, q) .eq. 1) then
          write(*,'(A,I0,A,I0,A)') 'fator: q = ', q, ' divide M(',
     &          p, ')'
          write(*,'(A,I0)') 'candidatos testados: ', testados
          achou = .true.
          exit
        end if
        r = r + 1
      end do
      end

      logical function ehprimo(mm)
      implicit none
      integer*8 mm, i
      if (mm .lt. 2) then
        ehprimo = .false.
        return
      end if
      i = 2
      do while (i*i .le. mm)
        if (mod(mm, i) .eq. 0) then
          ehprimo = .false.
          return
        end if
        i = i + 1
      end do
      ehprimo = .true.
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
