

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

  def multMatrizPar(m1: Matrix, m2: Matrix): Matrix = {
    val l = m1.length
    Vector.tabulate(l, l) { (i, j) =>
      (0 until l).map(k => m1(i)(k) * m2(k)(j)).sum
    }.par
  }



/*  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length

    val C = Vector.tabulate(l, l)((i, j) => {
      (0 until l).par.map(k => m1(i)(k) * m2(k)(j)).sum
    })
    C

  }

  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val l1 = m1.length
    val l2 = m2.length
    val m2T = transpuesta(m2)

    if (l1 >= 4) {
      val mid = l1 / 2
      val (m1R, m1L) = m1 splitAt (mid)
      val (r1, r2) = parallel(multMatrizPar(m1R, m2), multMatrizPar(m1L, m2))
      r1 ++ r2
    }
    else {
      Vector.tabulate(l1, l2)((i, j) => prodPunto(m1(i), m2T(j)))
    }
  }

*/

  ////////////////////////////////////////////////////////////////////////////////////

  // Ejercicio 1.2.1   ********DUDAS SOBRE .MAP ***************************

/*
  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    val submatriz = (0 until l).map(a => (0 until l).map(c => m(i + a)(j + c)).toVector).toVector

    submatriz
  }


  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    val submatriz = for {
      a <- i until (i + l)
      b = for {
        c <- j until (j + l)
      } yield m(a)(c)
    } yield b.toVector

    submatriz.toVector
  }*/

  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    Vector.tabulate(l, l)((x, y) => m(i + x)(y + j))
  }


  //////////////////////////////////////////////////////////////////////////////////////

  // Ejercicio 1.2.2
/*
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    val resultado = (0 until l).map(i => (0 until l).map(j => m1(i)(j) + m2(i)(j)).toVector).toVector
    resultado
  }
*/
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    Vector.tabulate(l, l)((i, j) => m1(i)(j) + m2(i)(j))
  }

/*
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length

    val resultado = for (i <- 0 until l) yield {
      for (j <- 0 until l) yield {
        m1(i)(j) + m2(i)(j)
      }
    }
    resultado.map(_.toVector).toVector
  }
*/


  ////////////////////////////////////////////////////////////////////////////////////////////


  // Ejercicio 1.2.3


  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {

    def unirMatriz(m1: Matriz, m2: Matriz): Matriz = {
      val l = m1.length
      Vector.tabulate(l)((i) => m1(i) ++ m2(i))
    }

    val l = m1.length
    if (l == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {

      val medio = l / 2

      val a11 = subMatriz(m1, 0, 0, medio)
      val a12 = subMatriz(m1, 0, medio, medio)
      val a21 = subMatriz(m1, medio, 0, medio)
      val a22 = subMatriz(m1, medio, medio, medio)

      val b11 = subMatriz(m2, 0, 0, medio)
      val b12 = subMatriz(m2, 0, medio, medio)
      val b21 = subMatriz(m2, medio, 0, medio)
      val b22 = subMatriz(m2, medio, medio, medio)

      val c11 = sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21))
      val c12 = sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22))
      val c21 = sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21))
      val c22 = sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))

      val c1 = unirMatriz(c11, c12)
      val c2 = unirMatriz(c21, c22)

      unirMatriz(c1, c2)
    }
  }




  ////////////////////////////////////////////////////////////////////////////////////////////

  // Ejercicio 1.2.4
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {

    def unirMatriz(m1: Matriz, m2: Matriz): Matriz = {

      val l = m1.length
      Vector.tabulate(l)((i) => m1(i) ++ m2(i))
    }

    val l = m1.length
    if (l == 1) Vector(Vector(m1(0)(0) * m2(0)(0)))
    else {

      val medio = l / 2

      val mulIzquierda = task(for (i <- (0 to 1).toVector;
                                   j <- (0 to 1).toVector) yield
        multMatrizRecPar(subMatriz(m1, l * i / 2, 0, medio), subMatriz(m2, 0, l * j / 2, medio)))

      val mulDerecha = task(for (i <- (0 to 1).toVector;
                                 j <- (0 to 1).toVector) yield
        multMatrizRecPar(subMatriz(m1, l * i / 2, medio, medio), subMatriz(m2, medio, l * j / 2, medio)))

      val Izquierda = mulIzquierda.join()
      val derecha = mulDerecha.join()

      val subMatrizT = for (i <- (0 to 3).toVector) yield sumMatriz(Izquierda(i), derecha(i))

      unirMatriz(subMatrizT(0), subMatrizT(1)) ++ unirMatriz(subMatrizT(2), subMatrizT(3))

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

  def multiStrassen(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length

    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val medio = n / 2

      val a11 = subMatriz(m1, 0, 0, medio)
      val a12 = subMatriz(m1, 0, medio, medio)
      val a21 = subMatriz(m1, medio, 0, medio)
      val a22 = subMatriz(m1, medio, medio, medio)

      val b11 = subMatriz(m2, 0, 0, medio)
      val b12 = subMatriz(m2, 0, medio, medio)
      val b21 = subMatriz(m2, medio, 0, medio)
      val b22 = subMatriz(m2, medio, medio, medio)

      // Calcular las submatrices recursivamente
      val p1 = multiStrassen(sumMatriz(a11, a22), sumMatriz(b11, b22))
      val p2 = multiStrassen(sumMatriz(a21, a22), b11)
      val p3 = multiStrassen(a11, restaMatriz(b12, b22))
      val p4 = multiStrassen(a22, restaMatriz(b21, b11))
      val p5 = multiStrassen(sumMatriz(a11, a12), b22)
      val p6 = multiStrassen(restaMatriz(a21, a11), sumMatriz(b11, b12))
      val p7 = multiStrassen(restaMatriz(a12, a22), sumMatriz(b21, b22))

      val c11 = restaMatriz(sumMatriz(sumMatriz(p1, p4), p7), p5)
      val c12 = sumMatriz(p3, p5)
      val c21 = sumMatriz(p2, p4)
      val c22 = restaMatriz(sumMatriz(sumMatriz(p1, p3), p6), p2)

      val arriba = Vector.tabulate(n)(i => c11(i) ++ c12(i))
      val abajo = Vector.tabulate(n)(i => c21(i) ++ c22(i))
      arriba ++ abajo
    }
  }


  /////////////////////////////////////////////////////////////////////////////////////////////////////

  // Ejercicio 1.3.3
  /*
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {

  }*/
}



