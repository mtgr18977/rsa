C ======================================================================
C  Exercicio 15  (Cap. 7, ex. 10) - Raiz quadrada mod n = p*q, com
C  p, q = 3 (mod 4). Resolve mod p e mod q e combina pelo TCR.
C ======================================================================
      program raiz_mod_pq
      implicit none
      integer*8 powmod, mulmod
      integer*8 p, q, a, n, aa, bp, bq, x
      integer*8 invqp, invpq
      logical   ehprimo

      p = 7
      q = 11
      a = 15

      if (.not. ehprimo(p) .or. .not. ehprimo(q)) then
        write(*,'(A)') 'p e q devem ser primos.'
        stop
      end if
      if (mod(p, 4_8) .ne. 3 .or. mod(q, 4_8) .ne. 3) then
        write(*,'(A)') 'p e q devem deixar resto 3 na divisao por 4.'
        stop
      end if

      n  = p * q
      aa = mod(mod(a, n) + n, n)
      bp = powmod(mod(aa, p), (p + 1) / 4, p)
      if (mod(bp * bp, p) .ne. mod(aa, p)) then
        write(*,'(A)') 'Sem solucao mod p; logo sem solucao mod n.'
        stop
      end if
      bq = powmod(mod(aa, q), (q + 1) / 4, q)
      if (mod(bq * bq, q) .ne. mod(aa, q)) then
        write(*,'(A)') 'Sem solucao mod q; logo sem solucao mod n.'
        stop
      end if

      invqp = powmod(mod(q, p), p - 2, p)
      invpq = powmod(mod(p, q), q - 2, q)
      x = mod(mulmod(mulmod(bp, q, n), invqp, n)
     &      + mulmod(mulmod(bq, p, n), invpq, n), n)

      write(*,'(A,I0)') 'n = p*q = ', n
      write(*,'(A,I0,A,I0)') 'raiz mod p = ', bp, '   raiz mod q = ',
     &      bq
      write(*,'(A,I0,A,I0,A)') 'solucao: x = ', x, ' (mod ', n, ')'
      write(*,'(A,I0)') 'verificacao x^2 mod n = ', mod(x * x, n)
      write(*,'(A)') '(ha quatro solucoes mod n; esta e uma delas)'
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
