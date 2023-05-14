

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

  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    val C = Vector.tabulate(l, l)((i, j) =>
      (0 until l).map(k => m1(i)(k) * m2(k)(j)).sum
    )
    C
  }


  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    val m2 = transpuesta(m2)
    Vector.tabulate(l, l)((i, j) => prodPunto(m1(i), m2(j)))
  }



  ////////////////////////////////////////////////////////////////////////////////////

  // Ejercicio 1.1.2 ***********HAY ERROR******


  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
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



  ////////////////////////////////////////////////////////////////////////////////////

  // Ejercicio 1.2.1   ********DUDAS SOBRE .MAP ***************************


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
  }

  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    Vector.tabulate(l, l)((x, y) => m(i + x)(y + j))
  }


  //////////////////////////////////////////////////////////////////////////////////////

  // Ejercicio 1.2.2

  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    val resultado = (0 until l).map(i => (0 until l).map(j => m1(i)(j) + m2(i)(j)).toVector).toVector
    resultado
  }

  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    Vector.tabulate(l, l)((i, j) => m1(i)(j) + m2(i)(j))
  }


  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length

    val resultado = for (i <- 0 until l) yield {
      for (j <- 0 until l) yield {
        m1(i)(j) + m2(i)(j)
      }
    }
    resultado.map(_.toVector).toVector
  }



  ////////////////////////////////////////////////////////////////////////////////////////////


  // Ejercicio 1.2.3 ********* TENGO MIS DUDAS **********


  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {

    def unirMatriz(m1: Matriz, m2: Matriz): Matriz = {
      val l = m1.length
      Vector.tabulate(l)((i) => m1(i) ++ m2(i))
    }

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


  // Ejercicio 1.2.4 *****************FALTA COLOCARLO*********************
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
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

  }


  /////////////////////////////////////////////////////////////////////////////////////////////////////

  // Ejercicio 1.3.3
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {

  }



}
