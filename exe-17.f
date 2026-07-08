C ======================================================================
C  Exercicio 17  (Cap. 8, ex. 21) - Funcao phi de Euler e a equacao
C  phi(k) = phi(k+1). Calcula phi(k) por fatoracao e usa um crivo de
C  phi para a busca ate um limite.
C ======================================================================
      program phi_euler
      implicit none
      integer, parameter :: lim = 100000
      integer   phi(0:lim+1)
      integer   i, p
      integer*8 k, phik

      k = 360

C     phi(k) por fatoracao
      phik = k
      block
        integer*8 m, pp
        m = k
        pp = 2
        do while (pp*pp .le. m)
          if (mod(m, pp) .eq. 0) then
            phik = phik - phik / pp
            do while (mod(m, pp) .eq. 0)
              m = m / pp
            end do
          end if
          pp = pp + 1
        end do
        if (m .gt. 1) phik = phik - phik / m
      end block
      write(*,'(A,I0,A,I0)') 'phi(', k, ') = ', phik

C     crivo de phi
      do i = 0, lim + 1
        phi(i) = i
      end do
      do p = 2, lim + 1
        if (phi(p) .eq. p) then
          do i = p, lim + 1, p
            phi(i) = phi(i) - phi(i) / p
          end do
        end if
      end do

      write(*,'(A,I0,A)') 'valores de k < ', lim,
     &  ' com phi(k) = phi(k+1):'
      do i = 1, lim - 1
        if (phi(i) .eq. phi(i + 1)) then
          write(*,'(I0,A)', advance='no') i, ' '
        end if
      end do
      write(*,*)
      end
