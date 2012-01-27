package torture

object torture extends Application
{
  override def main(args: Array[String]) =
  {
    new Prog().generate(1000, 1024)
  }
}
