import java.text.DecimalFormat
import java.util.Locale
import java.util.concurrent.Executors

import net.sourceforge.jeval.Evaluator
import scala.collection.JavaConversions
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Await, Future}
import scala.concurrent.ExecutionContext._
import scala.util.{Success, Random}
import scala.concurrent.duration.Duration

object Main {

  implicit class VectorOps(vector: Vector) {
    def coords = vector.coords

    def genOp(f: (((Double, Double)) => Double), vector: Vector) =
      Vector(coords.zip(vector.coords).map(f(_)))

    def checkSize(v1: Vector, v2: Vector) =
      assert(v1.coords.size == v2.coords.size, "Vectors should have same size")

    def +(v: Vector) = {
      checkSize(vector, v)
      genOp(x => x._1 + x._2, v)
    }

    def -(v: Vector) = {
      checkSize(vector, v)
      genOp(x => x._1 - x._2, v)
    }

    def *(mult: Double) = {
      Vector(coords.map(_ * mult))
    }
  }

  implicit class DoubleOps(d: Double) {
    def *(vector: Vector) = vector * d

    val precision = 0.00004d

    def ~=(d2: Double) = (d - d2).abs <= precision

  }

  case class Vector(coords: Seq[Double]) {
    def apply(n: Int) = coords(n)

    override def equals(that: scala.Any): Boolean = that match {
      case Vector(thatCoords) =>
        coords.zip(thatCoords).forall { case (d: Double, d2: Double) => d ~= d2 }
      case _ => false
    }
  }

  def getUnique(from: Seq[Vector], not: Seq[Vector]): Option[Vector] = from.find(vec => !not.contains(vec))

  def mutation(A: Vector, B: Vector, C: Vector, F: Double) = C + F * (A - B)

  def crossBreeding(X: Vector, C: Vector, cbf: (Double, Double) => Double): Vector =
    Vector(X.coords.zip(C.coords).map { zipped =>
      cbf(zipped._1, zipped._2)
    })


  import scala.collection.JavaConversions._

  def readFromFile(file: String): (Vector => Double) = {
    val func = io.Source.fromURL(getClass.getResource(s"/$file")).getLines().next()
    (vec: Vector) => {
      val vars = vec.coords.zipWithIndex.map(
        x => "x" + (x._2 + 1) -> String.format(Locale.US, "%.9f", Double.box(x._1))).toMap
      val eval = new Evaluator()
      eval.setVariables(vars)
      eval.getNumberResult(func)
    }
  }

  def foo(x: Double, y: Double) = 0.8d * x + 0.2 * y

  def crossWithProbability(P: Double) =
    (x1: Double, x2: Double) => if (Random.nextDouble() > P) x1 else x2


  def main(args: Array[String]) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val func = readFromFile("func")
    val withProbabilityMerge = crossWithProbability(P = 0.5d) // inheritance probability
    val codeGrayMerge = CodeGray.codeGrayMerge _
    val fooMerge = foo _

    val N = 4 // vector size
    val Guard = 2 // for generating
    val F = 0.6d // [0.4, 1.0] mutation power
    val PopulationSize = 200
    val population = 1 to PopulationSize map (_ => Vector(Seq.fill(N)(Random.nextInt(Guard).toDouble + Random.nextDouble)))
    val Iteration = 200 // iteration count

    val curriedGA: ((Double, Double) => Double) => (Vector, Double) = ga(func, population, _, Iteration, F)
    val res1 = Future((curriedGA(withProbabilityMerge), "probability"))
    val res2 = Future((curriedGA(codeGrayMerge), "code gray"))
    val res3 = Future(curriedGA(fooMerge), "mix")
    println(Await.result(Future.sequence(Seq(res1, res2, res3)), Duration.Inf).mkString("\n"))
    Success

  }

  def ga(func: (Vector) => Double,
         population: Seq[Vector],
         merge: (Double, Double) => Double,
         iteration: Int,
         mutationPower: Double) = {
    @tailrec
    def go(iteration: Int, population: Seq[Vector]): Seq[Vector] = {
      val newPopulation = population.map(
        X => {
          val unique = for {
            a <- getUnique(population, Seq(X))
            b <- getUnique(population, Seq(X, a))
            c <- getUnique(population, Seq(X, a, b))
          } yield (a, b, c)
          unique match {
            case Some((a, b, c)) =>
              val crossed = crossBreeding(X, mutation(a, b, c, mutationPower), merge)
              if (func(crossed) < func(X)) crossed else X
            case None => X
          }
        })
      if (iteration == 0) population else go(iteration - 1, newPopulation)
    }
    go(iteration, population).map(x => (x, func(x))).sortWith((a, b) => a._2 < b._2).head
  }

}
