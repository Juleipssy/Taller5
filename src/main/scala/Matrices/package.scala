

//juleipssy daianne cely archila
//jose manuel palma oquendo


  import common._
  import scala.util.Random

package object Matrices {
  val random = new Random()

  type Matriz = Vector[Vector[Int]]

  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    //Crea una matriz de enteros cuadrada de long x long ,
    // con valores aleatorios entre 0 y vals
    val v = Vector.fill(long, long) {
      random.nextInt(vals)
    }
    v
  }

  /////////////////////////////////////////////////////////////////////////////////////

  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l, l)((i, j) => m(j)(i))
  }

  ///////////////////////////////////////////////////////////////////////////////////////

  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map({ case (i, j) => (i * j) }).sum
  }

  ///////////////////////////////////////////////////////////////////////////////////////

  // Ejercicio 1.1.1

  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    Vector.tabulate(l, l)((i, j) =>
      (0 until l).map(k => m1(i)(k) * m2(k)(j)).sum
    )
  }

  ////////////////////////////////////////////////////////////////////////////////////

  // Ejercicio 1.1.2 ***********HAY ERROR******

  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    val m2Transpuesta = transpuesta(m2)

    val resultado = Vector.tabulate(l, l)((i, j) =>
      (0 until l).foldLeft(0)((acc, k) =>
        acc + m1(i)(k) * m2Transpuesta(j)(k)
      )
    )
    parallel(m2Transpuesta,resultado)
    resultado
  }

  ////////////////////////////////////////////////////////////////////////////////////

  // Ejercicio 1.2.1   ********DUDAS SOBRE .MAP ***************************

  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    Vector.tabulate(l, l)((x, y) => m(i + x)(y + j))
  }

  //////////////////////////////////////////////////////////////////////////////////////

  // Ejercicio 1.2.2
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    Vector.tabulate(l, l)((i, j) => m1(i)(j) + m2(i)(j))
  }

  ////////////////////////////////////////////////////////////////////////////////////////////


  // Ejercicio 1.2.3
def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {

  val l = m1.length
  if (l == 1) {
    Vector(Vector(m1(0)(0) * m2(0)(0)))
  } else {
    val mitad = l / 2

    val a11 = subMatriz(m1, 0, 0, mitad)
    val a12 = subMatriz(m1, 0, mitad, mitad)
    val a21 = subMatriz(m1, mitad, 0, mitad)
    val a22 = subMatriz(m1, mitad, mitad, mitad)

    val b11 = subMatriz(m2, 0, 0, mitad)
    val b12 = subMatriz(m2, 0, mitad, mitad)
    val b21 = subMatriz(m2, mitad, 0, mitad)
    val b22 = subMatriz(m2, mitad, mitad, mitad)

    val c11 = sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21))
    val c12 = sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22))
    val c21 = sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21))
    val c22 = sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))

    Vector.tabulate(l) { i =>
      Vector.tabulate(l) { j =>
        if (i < mitad && j < mitad) c11(i)(j)
        else if (i < mitad && j >= mitad) c12(i)(j - mitad)
        else if (i >= mitad && j < mitad) c21(i - mitad)(j)
        else c22(i - mitad)(j - mitad)
      }
    }
  }
}

  ////////////////////////////////////////////////////////////////////////////////////////////

  // Ejercicio 1.2.4
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {

    val l = m1.length
    if (l == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val mitad = l / 2

      val a11 = subMatriz(m1, 0, 0, mitad)
      val a12 = subMatriz(m1, 0, mitad, mitad)
      val a21 = subMatriz(m1, mitad, 0, mitad)
      val a22 = subMatriz(m1, mitad, mitad, mitad)

      val b11 = subMatriz(m2, 0, 0, mitad)
      val b12 = subMatriz(m2, 0, mitad, mitad)
      val b21 = subMatriz(m2, mitad, 0, mitad)
      val b22 = subMatriz(m2, mitad, mitad, mitad)

      val task1 = task(sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21)))
      val task2 = task(sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22)))
      val task3 = task(sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21)))
      val task4 = task(sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22)))

      val c11 = task1.join()
      val c12 = task2.join()
      val c21 = task3.join()
      val c22 = task4.join()

      Vector.tabulate(l) { i =>
        Vector.tabulate(l) { j =>
          if (i < mitad && j < mitad) c11(i)(j)
          else if (i < mitad && j >= mitad) c12(i)(j - mitad)
          else if (i >= mitad && j < mitad) c21(i - mitad)(j)
          else c22(i - mitad)(j - mitad)
        }
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////

  // Ejercicio 1.3.1
  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    Vector.tabulate(l, l)((i, j) => m1(i)(j) - m2(i)(j))
  }


  ///////////////////////////////////////////////////////////////////////////////////////////////////

  // Ejercicio 1.3.2

  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length

    if (n <= 1) {
      Vector.tabulate(n, n)((i, j) => m1(i)(0) * m2(0)(j))
    } else {
      val half = n / 2

      val a11 = subMatriz(m1, 0, 0, half)
      val a12 = subMatriz(m1, 0, half, half)
      val a21 = subMatriz(m1, half, 0, half)
      val a22 = subMatriz(m1, half, half, half)

      val b11 = subMatriz(m2, 0, 0, half)
      val b12 = subMatriz(m2, 0, half, half)
      val b21 = subMatriz(m2, half, 0, half)
      val b22 = subMatriz(m2, half, half, half)

      val p1 = multStrassen(sumMatriz(a11, a22), sumMatriz(b11, b22))
      val p2 = multStrassen(sumMatriz(a21, a22), b11)
      val p3 = multStrassen(a11, restaMatriz(b12, b22))
      val p4 = multStrassen(a22, restaMatriz(b21, b11))
      val p5 = multStrassen(sumMatriz(a11, a12), b22)
      val p6 = multStrassen(restaMatriz(a21, a11), sumMatriz(b11, b12))
      val p7 = multStrassen(restaMatriz(a12, a22), sumMatriz(b21, b22))

      val c11 = sumMatriz(sumMatriz(p1, p4), restaMatriz(p7, p5))
      val c12 = sumMatriz(p3, p5)
      val c21 = sumMatriz(p2, p4)
      val c22 = sumMatriz(restaMatriz(p1, p2), sumMatriz(p3, p6))

      val result = Vector.tabulate(n, n)((i, j) =>
        if (i < half && j < half) c11(i)(j)
        else if (i < half && j >= half) c12(i)(j - half)
        else if (i >= half && j < half) c21(i - half)(j)
        else c22(i - half)(j - half)
      )
      result
    }
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////

  // Ejercicio 1.3.3
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length

    if (n <= 1) {
      Vector.tabulate(n, n)((i, j) => m1(i)(0) * m2(0)(j))
    } else {
      val half = n / 2

      val a11 = subMatriz(m1, 0, 0, half)
      val a12 = subMatriz(m1, 0, half, half)
      val a21 = subMatriz(m1, half, 0, half)
      val a22 = subMatriz(m1, half, half, half)

      val b11 = subMatriz(m2, 0, 0, half)
      val b12 = subMatriz(m2, 0, half, half)
      val b21 = subMatriz(m2, half, 0, half)
      val b22 = subMatriz(m2, half, half, half)

      val task1 = task(multStrassen(sumMatriz(a11, a22), sumMatriz(b11, b22)))
      val task2 = task(multStrassen(sumMatriz(a21, a22), b11))
      val task3 = task(multStrassen(a11, restaMatriz(b12, b22)))
      val task4 = task(multStrassen(a22, restaMatriz(b21, b11)))
      val task5 = task(multStrassen(sumMatriz(a11, a12), b22))
      val task6 = task(multStrassen(restaMatriz(a21, a11), sumMatriz(b11, b12)))
      val task7 = task(multStrassen(restaMatriz(a12, a22), sumMatriz(b21, b22)))

      val p1 = task1.join()
      val p2 = task2.join()
      val p3 = task3.join()
      val p4 = task4.join()
      val p5 = task5.join()
      val p6 = task6.join()
      val p7 = task7.join()

      val c11 = sumMatriz(sumMatriz(p1, p4), restaMatriz(p7, p5))
      val c12 = sumMatriz(p3, p5)
      val c21 = sumMatriz(p2, p4)
      val c22 = sumMatriz(restaMatriz(p1, p2), sumMatriz(p3, p6))

      parallel(c11,c12,c21,c22)

      val result = Vector.tabulate(n, n)((i, j) =>
        if (i < half && j < half) c11(i)(j)
        else if (i < half && j >= half) c12(i)(j - half)
        else if (i >= half && j < half) c21(i - half)(j)
        else c22(i - half)(j - half)
      )
      result
    }
  }
}



