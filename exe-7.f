C ======================================================================
C  Exercicio 7  (Cap. 5, ex. 14) - Inverso em Zp via a^(p-2).
C  Verifica numericamente o pequeno teorema de Fermat.
C ======================================================================
      program inverso_zp
      implicit none
      integer*8 powmod
      integer*8 p, a, inv, prod, aa
      logical   ehprimo

      p = 101
      a = 37

      if (.not. ehprimo(p)) then
        write(*,'(I0,A)') p, ' nao e primo.'
        stop
      end if
      if (mod(a, p) .eq. 0) then
        write(*,'(A)') 'p divide a: o inverso nao existe.'
        stop
      end if

      aa  = mod(mod(a, p) + p, p)
      inv = powmod(aa, p - 2, p)
      prod = mod(aa * inv, p)
      write(*,'(A,I0)') 'a^(p-2) mod p = ', inv
      write(*,'(A,I0)') 'verificacao: a * a^(p-2) mod p = ', prod
      if (prod .eq. 1) then
        write(*,'(A)') 'e, de fato, o inverso de a em Zp.'
      else
        write(*,'(A)') 'algo esta errado.'
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
