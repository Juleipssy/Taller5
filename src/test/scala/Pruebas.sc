import Matrices ._
import Benchmark._

//Generamos matrices de varios tamaños para las pruebas

//1x1
val a1x1 = matrizAlAzar(1,2)
val b1x1 = matrizAlAzar(1,2)
//2x2
val a2x2 = matrizAlAzar(2,2)
val b2x2 = matrizAlAzar(2,2)
//4x4
val a4x4 = matrizAlAzar(4,2)
val b4x4 = matrizAlAzar(4,2)
//8x8
val a8x8 = matrizAlAzar(8,2)
val b8x8 = matrizAlAzar(8,2)
//16x16
val a16x16= matrizAlAzar(16,2)
val b16x16 = matrizAlAzar(16,2)
/*
MATRICES USADAS PARA TIEMPOS DE EJECUCION

//32x32
val a32x32= matrizAlAzar(32,2)
val b32x32 = matrizAlAzar(32,2)
//64x64
val a64x64= matrizAlAzar(64,2)
val b64x64 = matrizAlAzar(64,2)

//128x128
val a128x128= matrizAlAzar(128,2)
val b128x128 = matrizAlAzar(128,2)

//256x256
val a256x256= matrizAlAzar(256,2)
val b256x256 = matrizAlAzar(256,2)

//512x512
val a512x512= matrizAlAzar(512,2)
val b512x512 = matrizAlAzar(512,2)

//1024x1024
val a1024x1024 = matrizAlAzar(1024,2)
val b1024x1024 = matrizAlAzar(1024,2)
*/

multMatriz(a1x1,b1x1)
multMatriz(a2x2,b2x2)
multMatriz(a8x8,b8x8)
multMatriz(a16x16,b16x16)
multMatriz(b1x1,a1x1)

multMatrizPar(a1x1,b1x1)
multMatrizPar(a2x2,b2x2)
multMatrizPar(a8x8,b8x8)
multMatrizPar(a16x16,b16x16)
multMatrizPar(b1x1,a1x1)

subMatriz(a8x8,0,0,2)
subMatriz(a16x16,1,1,3)
subMatriz(a2x2,0,0,2)
subMatriz(b4x4,0,0,1)
subMatriz(a2x2,1,1,1)

sumMatriz(a8x8,b8x8)
sumMatriz(a2x2,b2x2)
sumMatriz(b2x2,a2x2)
sumMatriz(a4x4,a4x4)
sumMatriz(a1x1,b1x1)

multMatrizRec(a8x8,b8x8)
multMatrizRec(a2x2,b2x2)
multMatrizRec(b2x2,a2x2)
multMatrizRec(a4x4,a4x4)
multMatrizRec(a1x1,b1x1)

multMatrizRecPar(a8x8,b8x8)
multMatrizRecPar(a2x2,b2x2)
multMatrizRecPar(b2x2,a2x2)
multMatrizRecPar(a4x4,a4x4)
multMatrizRecPar(a1x1,b1x1)

multStrassen(a8x8,b8x8)
multStrassen(a2x2,b2x2)
multStrassen(b2x2,a2x2)
multStrassen(a4x4,a4x4)
multStrassen(a1x1,b1x1)

multStrassenPar(a8x8,b8x8)
multStrassenPar(a2x2,b2x2)
multStrassenPar(b2x2,a2x2)
multStrassenPar(a4x4,a4x4)
multStrassenPar(a1x1,b1x1)

//1.3.1
restaMatriz(a8x8,b8x8)
restaMatriz(a2x2,b2x2)
restaMatriz(b2x2,a2x2)
restaMatriz(a4x4,a4x4)
restaMatriz(a1x1,b1x1)

//FUNCIONES USADAS PARA OBTENER TIEMPOS DE EJECUCUCION (RECOMENDACION: NO EJECUTAR, TIEMPOS DE ESPERA EXTENSOS)

/*
compararAlgoritmos(multMatriz,multMatrizPar)(a1x1,b1x1)
compararAlgoritmos(multMatriz,multMatrizPar)(a2x2,b2x2)
compararAlgoritmos(multMatriz,multMatrizPar)(a4x4,b4x4)
compararAlgoritmos(multMatriz,multMatrizPar)(a8x8,b8x8)
compararAlgoritmos(multMatriz,multMatrizPar)(a16x16,b16x16)
compararAlgoritmos(multMatriz,multMatrizPar)(a32x32,b32x32)
compararAlgoritmos(multMatriz,multMatrizPar)(a64x64,b64x64)
compararAlgoritmos(multMatriz,multMatrizPar)(a128x128,b128x128)
compararAlgoritmos(multMatriz,multMatrizPar)(a256x256,b256x256)
compararAlgoritmos(multMatriz,multMatrizPar)(a512x512,b512x512)
compararAlgoritmos(multMatriz,multMatrizPar)(a1024x1024,b1024x1024)

compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a1x1,b1x1)
compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a2x2,b2x2)
compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a4x4,b4x4)
compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a8x8,b8x8)
compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a16x16,b16x16)
compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a32x32,b32x32)
compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a64x64,b64x64)
compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a128x128,b128x128)
compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a256x256,b256x256)
compararAlgoritmos(multMatrizRec,multMatrizRecPar)(a512x512,b512x512)
compararAlgoritmos(multMatriz,multMatrizPar)(a1024x1024,b1024x1024)

compararAlgoritmos(multStrassen,multStrassenPar)(a1x1,b1x1)
compararAlgoritmos(multStrassen,multStrassenPar)(a2x2,b2x2)
compararAlgoritmos(multStrassen,multStrassenPar)(a4x4,b4x4)
compararAlgoritmos(multStrassen,multStrassenPar)(a8x8,b8x8)
compararAlgoritmos(multStrassen,multStrassenPar)(a16x16,b16x16)
compararAlgoritmos(multStrassen,multStrassenPar)(a32x32,b32x32)
compararAlgoritmos(multStrassen,multStrassenPar)(a64x64,b64x64)
compararAlgoritmos(multStrassen,multStrassenPar)(a128x128,b128x128)
compararAlgoritmos(multStrassen,multStrassenPar)(a256x256,b256x256)
compararAlgoritmos(multStrassen,multStrassenPar)(a512x512,b512x512)
*/