package esmeta

@main def main(command: String, args: String*): Unit = {
  println(s"$command ${args.mkString(" ")}")
}
