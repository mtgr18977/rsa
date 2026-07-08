C ======================================================================
C  Exercicio 9  (Cap. 5, ex. 17) - Raizes quadradas mod p, p = 4k+3.
C  Se x^2 = a (mod p) tem solucao, ela e a^(k+1) mod p.
C ======================================================================
      program raiz_quad_mod_p
      implicit none
      integer*8 powmod
      integer*8 p, a, k, aa, b, b2
      logical   ehprimo

      p = 103
      a = 13

      if (.not. ehprimo(p)) then
        write(*,'(I0,A)') p, ' nao e primo.'
        stop
      end if
      if (mod(p, 4_8) .ne. 3) then
        write(*,'(I0,A)') p, ' nao e da forma 4k+3.'
        stop
      end if

      aa = mod(mod(a, p) + p, p)
      k  = (p - 3) / 4
      b  = powmod(aa, k + 1, p)
      b2 = mod(b * b, p)
      write(*,'(A,I0)') 'candidata b = a^(k+1) mod p = ', b
      if (b2 .eq. aa) then
        write(*,'(A,I0,A)') 'verificacao b^2 = ', b2, ' = a  OK'
        write(*,'(A,I0,A,I0)') 'solucoes: x = ', b, '  e  x = ',
     &        p - b
      else
        write(*,'(A,I0,A)') 'verificacao b^2 = ', b2, ' /= a'
        write(*,'(A)') 'A equacao nao tem solucao.'
      end if
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
