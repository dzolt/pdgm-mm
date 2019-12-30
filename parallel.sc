import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.util.Random

def task[T](body: => T): Future[T] = {
  Future{
    val result = body
    result}
}

def parallel[A,B](taskA: => A, taskB: => B): (A,B) = {
  val right = task(taskB)
  val left = task(taskA)

  val resultParallel = for{
    result1 <- left
    result2 <- right
  }yield(result1,result2)

 /* resultParallel.onComplete {
    case Success(res) => result = (resultParallel._1,res._2)
    case Failure(e) => e.printStackTrace()
  }*/
  Await.result(resultParallel, Duration.Inf)
}

val coinss = List(10,5,2,1)
def countChange(money: Int, coins: List[Int]): Int = {
  if(money == 0)1
  else if (coins.isEmpty || money < 0)0
  else countChange(money - coins.head, coins) + countChange(money,coins.tail)
}


def ParcountChange(money: Int, coins: List[Int]): Int = {
  if(money == 0)1
  else if (coins.isEmpty || money < 0)0
  else {
    val (left, right) = parallel(ParcountChange(money - coins.head, coins), ParcountChange(money, coins.tail))
    left+right
  }

}
countChange(50,coinss)
ParcountChange(50, coinss)