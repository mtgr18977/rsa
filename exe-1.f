C ======================================================================
C  Exercicio 1  (Cap. 1) - Maximo divisor comum e probabilidade de
C  coprimalidade. Gera pares aleatorios e estima a fracao de coprimos,
C  que tende a 6/pi^2 quando o numero de pares cresce.
C ======================================================================
      program mdc_coprimos
      implicit none
      integer*8 mdc
      integer*8 pares, a, b, coprimos, i
      integer   execs, e
      real*8    quoc, soma, teorico
      real*8    ra, rb

      pares = 100000
      execs = 10
      soma  = 0.0d0

      write(*,'(A)') 'exec | pares coprimos |  quociente'
      do e = 1, execs
        coprimos = 0
        do i = 1, pares
          call random_number(ra)
          call random_number(rb)
          a = int(ra * 1.0d9, 8) + 1
          b = int(rb * 1.0d9, 8) + 1
          if (mdc(a, b) .eq. 1) coprimos = coprimos + 1
        end do
        quoc = dble(coprimos) / dble(pares)
        soma = soma + quoc
        write(*,'(I4,A,I14,A,F10.6)') e, ' | ', coprimos,
     &        ' | ', quoc
      end do

      teorico = 6.0d0 / (acos(-1.0d0)**2)
      write(*,*)
      write(*,'(A,F10.6)') 'media dos quocientes : ', soma / execs
      write(*,'(A,F10.6)') 'valor teorico 6/pi^2 : ', teorico
      end

      integer*8 function mdc(a, b)
      implicit none
      integer*8 a, b, x, y, r
      x = a
      y = b
      do while (y .ne. 0)
        r = mod(x, y)
        x = y
        y = r
      end do
      mdc = abs(x)
      end
