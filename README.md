EXERCICIOS EM FORTRAN
=====================================================
Numeros Inteiros e Criptografia RSA (S. C. Coutinho)
---------------------

Cada exercicio esta em um arquivo exe-N.f, em Fortran de forma fixa
(compativel com F77) e usa inteiros de 64 bits (integer*8).

COMO COMPILAR E RODAR
---------------------
Um arquivo:      gfortran exe-1.f -o exe-1   &&   ./exe-1
Todos de uma vez: sh compilar.sh

Os parametros de entrada (limites, base, expoente etc.) estao fixados
no inicio de cada programa, com os mesmos valores-padrao da versao web.
Basta editar essas linhas e recompilar para experimentar outros casos.

INDICE
------
- exe-1   MDC e probabilidade de coprimalidade (tende a 6/pi^2)
- exe-2   Numeros altamente compostos < r
- exe-3   Fatoracao pelo algoritmo de Fermat
- exe-4   Frequencia de primos 4n+1 vs 4n+3
- exe-5   Menor x com pi1(x) > pi3(x)
- exe-6   Potenciacao modular a^k mod n
- exe-7   Inverso em Zp via a^(p-2)
- exe-8   Inverso modular e a equacao ax = b (mod p)
- exe-9   Raizes quadradas modulo p = 4k+3
- exe-10  Primos de Wieferich: a^(p-1) = 1 (mod p^2)
- exe-11  Pseudoprimos para as bases 2 e 3
- exe-12  Numeros de Carmichael com d fatores
- exe-13  Menor pseudoprimo forte (teste de Miller)
- exe-14  Pseudoprimos da forma p^2 (base 2)
- exe-15  Raizes quadradas modulo n = p*q (via TCR)
- exe-16  Pseudoprimos p^2 - versao melhorada
- exe-17  Funcao phi de Euler e phi(k) = phi(k+1)
- exe-18  Totientes
- exe-19  Fatores de numeros de Mersenne
- exe-20  Qual numero de Fermat e divisivel por q
- exe-21  Teste de Pepin (numeros de Fermat)
- exe-22  Teste de primalidade via (n-1)! (Wilson)

NOTAS TECNICAS
--------------
* Aritmetica de 64 bits. Onde o modulo pode ser grande (p^2, p*q,
  Mersenne, Pepin), os programas usam uma multiplicacao modular
  binaria (mulmod) que soma em vez de multiplicar, evitando o
  estouro que ocorreria em a*b quando a,b sao grandes.
* Fortran padrao nao tem inteiros de precisao arbitraria. Por isso:
  - exe-19 (Mersenne) exige p <= 61;
  - exe-21 (Pepin)     exige n <= 5;
  - exe-22 (Wilson)    vai ate k = 6.
  Para ir alem seria preciso uma biblioteca de bignum (p. ex. GMP).
* exe-1 usa numeros pseudoaleatorios, entao o quociente varia um
  pouco a cada execucao, sempre proximo de 6/pi^2 ~ 0,6079.
