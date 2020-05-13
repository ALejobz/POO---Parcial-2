import org.scalatest.FunSuite
import sistEdificio._
import usuario._
import salon._
import ocupacion._

class sistemaTDD extends FunSuite
{
    var palmas : sistema = new sistema()
    var p1 : salon = new salon("palm")
    var p2 : salon = new salon("linux")
    palmas.registrar("usu","pas",0)
    palmas.registrar("usu2","pass",1)
    palmas.agregarSalon(p1)
    palmas.agregarSalon(p2)
    palmas.reservar("palm", 9)
    palmas.clasear("linux",7)


    test("Al pasar hora si pasa la hora")
    {
        palmas.pasarHora()
        assert(palmas._min == 45)
    }

    test("Al pasar hora otra vez si pasa la hora")
    {
        palmas.pasarHora()
        assert(palmas._min == 50)
    }

    test("Imprime los salones")
    {
        //palmas.primReservas()
        palmas.pasarHora()
        palmas.pasarHora()
        //palmas.pasarHora()
        assert(palmas._min == 0)
    }

    test("La hora deben ser las 7")
    {
        assert(palmas._hora == 7)
    }

    test("las luces del salon linux deben estar encendidas")
    {
        assert(palmas.returnSalon("linux")._luz == true)
    }

    test("las luces deben de estar apagadas")
    {
        palmas.pasarHora(15)
        println(palmas._hora + " " +palmas._min)
        assert(palmas.returnSalon("linux")._luz == false)
    }

    test("las luces del otro salon deben estar encendidas")
    {
        assert(palmas.returnSalon("palm")._luz == true)
    }


}