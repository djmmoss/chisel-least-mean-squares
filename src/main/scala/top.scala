package ChiselLMS

import Chisel._


object Top {
    def main(args: Array[String]): Unit = {
        val featureLength = 8
        val stepSize = 0.01f
        val theArgs = args.slice(1, args.length)
        args(0) match {
            case "ParLMS" =>
                chiselMainTest(theArgs, () => Module(new ParLMS(featureLength, stepSize))) {
                    c => new ParLMSTests(c)}
        }
    }
}
