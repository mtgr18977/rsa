C ======================================================================
C  Exercicio 8  (Cap. 5, ex. 16) - Inverso modular e a equacao
C  linear a*x = b (mod p). Usa o inverso a^(p-2) do exercicio 7.
C ======================================================================
      program congruencia_linear
      implicit none
      integer*8 powmod
      integer*8 p, a, b, aa, bb, inv, x
      logical   ehprimo

      a = 37
      b = 15
      p = 101

      if (.not. ehprimo(p)) then
        write(*,'(A,I0,A)') 'Atencao: ', p, ' nao e primo.'
      end if
      if (mod(a, p) .eq. 0) then
        write(*,'(A)') 'p divide a: nao ha solucao por inversao.'
        stop
      end if

      aa  = mod(mod(a, p) + p, p)
      bb  = mod(mod(b, p) + p, p)
      inv = powmod(aa, p - 2, p)
      x   = mod(inv * bb, p)
      write(*,'(A,I0)') 'inverso de a mod p : ', inv
      write(*,'(A,I0,A,I0,A)') 'solucao: x = ', x, ' (mod ', p, ')'
      write(*,'(A,I0)') 'verificacao a*x mod p = ', mod(aa * x, p)
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

      integer*8 function powmod(base, expo, m)
      implicit none
      integer*8 base, expo, m, bb, e, r
      bb = mod(base, m)
      e  = expo
      r  = 1
      do while (e .gt. 0)
        if (iand(e, 1_8) .eq. 1) r = mod(r * bb, m)
        bb = mod(bb * bb, m)
        e = ishft(e, -1)
      end do
      powmod = r
      end
