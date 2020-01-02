object TreeParallel extends App{
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A]( elem:A, left:BT[A], right:BT[A]) extends BT[A]

  //tree creation
  ///////////////////////////////////////////////////////////////////////////////////
  val r = scala.util.Random;
  def createTree(depth:Int) : BT[Int] = {
    def fillTree(acc:BT[Int], depth:Int): BT[Int] ={
      depth match
      {
        case 0 => acc
        case _ => Node(r.nextInt(18), fillTree(acc,depth -1), fillTree(acc,depth-1))
      }
    }
    fillTree(Empty,depth)
  }
  /////////////////////////////////////////////////////////////////////////////////
  val tree = createTree(15)
  println(tree)

  //dlugosc od kazdego node'a do roota
  ////////////////////////////////////////////////////////////////////////////////
  def internalPath[A](tree: BT[A]) : Int =
  {
    def helper[A](node: BT[A], depth:Int): Int = {
      node match {
        case Empty => 0
        case Node(_, left, right) => depth + helper(left,depth + 1) + helper(right, depth + 1)
      }
    }
    helper(tree,0)
  }
  def parInternalPath[A](tree: BT[A]) : Int =
  {
    def parHelper[A](node: BT[A], depth:Int): Int = {
      node match {
        case Empty => 0
        case Node(_, left, right) => {
          val (lef, righ) = ConcurrentTask.parallel(parHelper(left, depth + 1), parHelper(right, depth + 1))
          depth + lef + righ
        }
      }
    }
    parHelper(tree,0)
  }
  ////////////////////////////////////////////////////////////////////////////////

  //suma
  ////////////////////////////////////////////////////////////////////////////////
  def sum(tree: BT[Int]): Int = {
    tree match {
      case Empty => 0
      case Node(value, left, right) => value + sum(left) + sum(right)
    }
  }
  def sumPar(tree: BT[Int], depth: Int): Int = {
    tree match {
      case Empty => 0
      case Node(value, left, right) =>
      {
        val (lef,righ) = ConcurrentTask.parallel(sumPar(left,0),sumPar(right,0))
        value + lef + righ
      }
    }
  }
  ////////////////////////////////////////////////////////////////////////////////

  println(MyTime.time(sum(tree)))
  println(MyTime.time(sumPar(tree, 0)))

  def update(tree: BT[Int]): BT[Int] = {
    tree match{
      case Empty =>Empty
      case Node(value, left, right) =>  Node(value*value, update(left), update(right))
    }
  }
  def updatePar(tree: BT[Int], depth:Int): BT[Int] = {
    tree match{
      case Empty =>Empty
      case Node(value, left, right)  => {
        if(depth <= 5) update(Node(value, left, right) )
        val (lef, righ) = ConcurrentTask.parallel(updatePar(left,depth + 1), updatePar(right, depth + 1))
        Node(value*value, lef, righ)
      }
    }
  }
  println(tree)
  println(MyTime.time(update(tree)))
  println(MyTime.time(updatePar(tree,0)))
}
