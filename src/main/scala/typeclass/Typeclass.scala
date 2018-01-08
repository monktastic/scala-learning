package typeclass

/**
  * Created by aditpras on 12/11/17.
  */


trait Printable[A] {
  def format(value: A): String
}


object PrintableInstances {
  implicit val stringPrintable = new Printable[String] {
    def format(input: String) = input
  }

  implicit val intPrintable = new Printable[Int] {
    def format(input: Int) = input.toString
  }
}


object Printable {
  def format[A](input: A)(implicit p: Printable[A]): String =
    p.format(input)

  def format2[A: Printable](input: A): String =
    implicitly[Printable[A]].format(input)

  def print[A](input: A)(implicit p: Printable[A]): Unit =
    println(format(input))
}



import typeclass.PrintableInstances._


object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String = {
      println("This class is " + this.getClass.getName)
      p.format(value)
    }
    def print(implicit p: Printable[A]): Unit = println(value.format)
  }
}


object Main {
  final case class Cat(name: String, age: Int, color: String)

  implicit val catPrintable = new Printable[Cat] {
    def format(cat: Cat) = {
      implicitly[Printable[String]].format(cat.name)
      val name  = Printable.format(cat.name)
      val age   = Printable.format(cat.age)
      val color = Printable.format(cat.color)
      s"$name is a $age year-old $color cat."
    }
  }

  import PrintableSyntax._
  
  def main(args: Array[String]): Unit = {
    val cat = Cat("billi", 5, "red")
    cat.print
  }
}

