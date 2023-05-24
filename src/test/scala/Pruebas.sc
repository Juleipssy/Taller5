import Matrices ._

val m1 = matrizAlAzar(3,2)
val m2 = matrizAlAzar(3,2)
val m3 = matrizAlAzar(2,2)
val m4 = matrizAlAzar(2,2)
val m5 = matrizAlAzar(4,2)
val m6 = matrizAlAzar(4,2)


//1.1.1
multMatriz(m1,m2)
multMatriz(m3,m4)
multMatriz(m5,m6)
multMatriz(m6,m5)
multMatriz(m2,m1)

multMatrizPar(m1,m2)
multMatrizPar(m3,m4)
multMatrizPar(m5,m6)
multMatrizPar(m6,m5)
multMatrizPar(m2,m1)

//1.1.2


//1.2.1

val matriz1 = Vector(Vector(0,0), Vector(1,0))
val matriz2 = Vector(Vector(0,0), Vector(1,0))
val matriz3 = Vector(Vector(1,2,0,1), Vector(1,3,0,0), Vector(1,1,1,1), Vector(0,0,0,0))
val matriz4 = Vector(Vector(1,1,1,1), Vector(0,0,0,0), Vector(1,1,1,1), Vector(0,0,0,0))
val matriz5 = Vector(Vector(1))


//subMatriz(matriz1,0,0,1)
subMatriz(matriz2,0,0,2)
subMatriz(matriz3,1,1,3)
subMatriz(matriz4,2,2,2)
subMatriz(matriz5,0,0,1)

//1.2.2
sumMatriz(matriz1,matriz2)
sumMatriz(matriz2,matriz1)
sumMatriz(matriz3,matriz3)
sumMatriz(matriz4,matriz3)
sumMatriz(matriz5,matriz5)

//1.2.3

val matrizXD = Vector(Vector(0,1,2,3,4,5,6,7), Vector(0,1,2,3,4,5,6,7),Vector(0,1,2,3,4,5,6,7),Vector(0,1,2,3,4,5,6,7),Vector(0,1,2,3,4,5,6,7),Vector(0,1,2,3,4,5,6,7),Vector(0,1,2,3,4,5,6,7),Vector(0,1,2,3,4,5,6,7))

multMatrizRec(matriz1,matriz2)
multMatrizRec(matriz3,matriz3)
multMatrizRec(matrizXD,matrizXD)
/*
multMatrizRec(m1,m2)
multMatrizRec(m1,m2)*/

//1.2.4
multMatrizRecPar(matriz1,matriz2)
/*multMatrizRecPar(m1,m2)
multMatrizRecPar(m1,m2)
multMatrizRecPar(m1,m2)
multMatrizRecPar(m1,m2)
*/
multStrassen(matriz1,matriz2)
multStrassen(matriz4,matriz3)
multStrassen(matriz5,matriz5)
multStrassen(matriz2,matriz1)
multStrassen(matriz3,matriz4)

multStrassenPar(matriz1,matriz2)
multStrassenPar(matriz4,matriz3)
multStrassenPar(matriz5,matriz5)
multStrassenPar(matriz2,matriz1)
multStrassenPar(matriz3,matriz4)

//1.3.1
restaMatriz(m1,m2)
restaMatriz(m1,m2)
restaMatriz(m1,m2)
restaMatriz(m1,m2)
restaMatriz(m1,m2)


