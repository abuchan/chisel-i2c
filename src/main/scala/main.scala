package I2C

import Chisel._

object I2C {
  def main(args: Array[String]): Unit = {
    val tutArgs = args.slice(1, args.length)
    val res =
    args(0) match {
      case "I2CMaster" => 
        chiselMainTest(tutArgs, () => Module(new I2CMaster())){
          c => new I2CMasterTests(c)}
      case "I2CStream" => 
        chiselMainTest(tutArgs, () => Module(new I2CStream())){
          c => new I2CStreamTests(c)}
    }
  }
}
