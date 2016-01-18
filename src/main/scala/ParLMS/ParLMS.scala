package ChiselLMS

import Chisel._
import util.Random

class ParLMS(val featureLength : Int, val stepSize : Float) extends Module {
  def group[A](list : List[A], size : Int) : List[List[A]] = list.foldLeft( (List[List[A]](), 0) ) { (r, c) => 
    r match {
      case (head :: tail, num) =>
        if (num < size) ( (c :: head) :: tail, num + 1)
        else            ( List(c) :: head :: tail, 1)
    case (Nil, num) => (List(List(c)),1)
    }
  }._1.foldLeft(List[List[A]]())( (r,c) => c.reverse :: r)

  def buildLevel[A](list : List[A], op : (A, A) => A) : List[A] = group(list, 2).map(l => l.reduce[A](op(_,_)))

  def buildTree[A](list : List[List[A]], op : (A, A) => A) : List[List[A]] = {
    val stage = buildLevel(list.last, op)
    val tree : List[List[A]] = list :+ stage
    if (stage.length == 1) tree else buildTree(tree, op)
  }

  def reduceTree[A](list : List[A], op :(A, A) => A) : A = buildTree(List(list), op).last.head

  val io = new Bundle {
    val x = Vec.fill(featureLength){Flo().asInput}
    val y = Flo().asInput
    val pred = Flo().asOutput
  }

  // Weights
  val w = Vec.fill(featureLength){Reg(init=Flo(0))}

  // Dot Product - Using an Adder Tree
  // pred = vi.*alphai
  val prodList = (io.x zip w).map(p => p._1*p._2)

  val op: (Flo, Flo) => Flo = _+_
  val pred = reduceTree(prodList.toList, op)
  val step = Flo(stepSize)*(io.y - pred)

  // Update w.
  val wUpdate = (w zip io.x).map(p => p._1 := p._1 + step*p._2)

  io.pred := pred
}

class ParLMSTests(c: ParLMS, isTrace: Boolean=true) extends Tester(c, isTrace) {
  // A method to calculate the update in software.
  def update(x: List[Float], y: Float, w: List[Float], stepSize: Float): (List[Float], Float) = {
    // Calculate prediction and error.
    val op : (Float, Float) => Float = _+_
    val ybar = c.reduceTree((w, x).zipped.map(_*_), op)
    val err = y - ybar
    val step = stepSize*err

    // Calculate new weights.
    val w_update = (w, x.map((f: Float) => f*step)).zipped.map(_+_)
    (w_update, ybar)
  }

  // Get some parameters from the hardware.
  val n = c.featureLength
  val stepSize = c.stepSize

  // Set up number of trials and a success flag.
  val examples: Int = 100
  var success: Boolean = true

  // Set up the parameters for the experiment.
  var w = List.fill(n){ 0.0f }
  val rand = new Random()
  val std = 2.0f
  val weights = List.fill(n){ std*rand.nextGaussian().toFloat }
  val noise = 1e-4f

  // Run the simulation.
  for (i <- 0 until examples) {
    // Generate random inputs and the output with noise.
    val x = List.fill(n){ std*rand.nextGaussian().toFloat }
    val y = (x, weights).zipped.map(_*_).reduce(_+_) + noise*rand.nextGaussian().toFloat

    // Calculate new weights and the a priori prediction in software.
    val (wtmp, ybar) = update(x, y, w, stepSize)
    w = wtmp

    // Connect x and y to the hardware.
    poke(c.io.y, Flo(y).litValue())
    for (i <- 0 until n) {
      poke(c.io.x(i), Flo(x(i)).litValue())
    }

    //Check the output.
    success = success && expect(c.io.pred, Flo(ybar).litValue())
    step(1)
  }
  success
}
