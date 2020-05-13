package sistEdificio
import salon._
import ocupacion._
import usuario._
import scala.util.control.Breaks._

class sistema 
{
    //ATRIBUTOS
    var _listaSalones : List[salon] = List()
    var _listaUsuario : List[usuario] = List()
    var _hora : Int = 6
    var _min : Int = 10
    var _listaMin : List[Int] = List(0, 5, 10, 45 ,50 ,55) // poscicion 0 -> cambio de hora, 1 -> Apagar Aire
                                                           //2 -> apagar luz, 3 -> abrir puertas, 4 prender Aire, 5 -> prende luz
    //GETTERS
    def listaSalones = _listaSalones
    def listaUsuario = _listaUsuario
    def hora = _hora
    def min = _min
    def listaMin = _listaMin

    //METODOS

    def encontrar(lis : List[Int], num : Int) : Int =
    {
        var con : Int = 0
        for(i <- lis)
        {
            if(i == num)
            {
                return con
            }
            con = con + 1
        }
        return con
    }

    def pasarHora() : Unit =
    {
        var n1 : Int = encontrar(listaMin,min)
        
        n1 = n1 + 1

        if(n1 == (listaMin.length))
        {
            n1 = 0
        }

        n1 match
        {
            case 0 => cambioHora()
            case 1 => closeAire()
            case 2 => closeLuz()
            case 3 => openPuertas()
            case 4 => openAire()
            case 5 => openLuz()
        }

    }

    def pasarHora(num : Int) : Unit =
    {
        var con :Int = 0
        while(con < num)
        {
            this.pasarHora()
            con += 1
        }
    }

    def returnSalon(nom : String) : salon =
    {
        for(i <- this._listaSalones)
        {
            if(nom == i._nombreSalon)
            {
                return i
            }
        }
        return this._listaSalones.head
    }

    def cambioHora() : Unit =
    {
        this._hora += 1
        for(i <- _listaSalones)
        {
            i.cambiarOcupacion(hora)
        }
        this._min = 0 
    }

    def closeAire() : Unit =
    {
        for(i <- _listaSalones)
        {
            if(i.verifyOcupado(hora) == false)
            {
                i._temperatura = 0
            }
        }
        this._min = 5
    }

    def closeLuz() : Unit =
    {
        for(i <- _listaSalones)
        {
            if(i.verifyOcupado(hora) == false)
            {
                i.apagarLuz()
            }
        }
        this._min = 10
    }

    def openPuertas() : Unit =
    {

        for(i <- _listaSalones)
        {
            if(i.verifyOcupado(hora + 1) == true)
            {

                i.abrir()
            }
        }
        this._min = 45

    }
    
    def openAire() : Unit =
    {
        for(i <- _listaSalones)
        {
            if(i.verifyOcupado(hora + 1) == true)
            {
                i._temperatura = 23
            }
        }
        this._min = 50
    }

    def openLuz() : Unit =
    {
        for(i <- _listaSalones)
        {
            if(i.verifyOcupado(hora + 1) == true)
            {
                i.prenderLuz()
            }
        }
        this._min = 55
    }

    def añadirSalon(nom : String) : Unit =
    {
        var sal : salon = new salon(nom)
        _listaSalones = sal :: _listaSalones
    }

    def impr(ho : Int, tip :Int) : Unit =
    {
        if(tip == 1)
        {
            println("De " + ho + " a " + (ho + 2) +" hay una Clase")
        }
        else
        {
            println("De " + ho + " a " + (ho + 2) +" hay una reserva")
        }

    }

    def primReservas() : Unit =
    {
        for(i <- _listaSalones)
        {
            println("================================================================")
            println("El salon " + i._nombreSalon + " tiene las siguientes reservas :")
            var ho : Int = 7
            var pim : Boolean = false
            while(ho < 18)
            {
                pim = false
                for(j <- i._listOcupacion)
                {
                    if(j._horaEntrada == ho)
                    {
                        impr(ho,j._tipo)
                        pim = true
                    }
                }
                
                if(pim == false)
                {
                    println("De " + ho + " a " + (ho + 2) + " el salon esta libre")
                }
                ho += 2
            
            }
            println("===================================================================")
        }
    }

    def registrar(nom : String, pas: String,tip : Int) : Unit =
    {
        var usu : usuario = new usuario(nom,pas,0)
        this._listaUsuario = usu :: _listaUsuario
    }

    def reservar(nom : String, hor : Int) : Boolean = 
    {
        for(i <- this._listaSalones)
        {
            if(i._nombreSalon == nom)
            {
                if(i._mantenimieto == false)
                {
                    var ocupa: ocupacion = new ocupacion(2,hor)
                    i.agregarHorario(ocupa)
                    return true
                }
                else
                {
                    return false
                }
            }    
        }
        return false
    }

    // def eliminarReservar(nom : String, hor : Int) : Boolean = 
    // {
    //     for(i <- this._listaSalones)
    //     {
    //         if(i._nombreSalon == nom)
    //         {
    //             i._listOcupacion = i._listOcupacion.filter(_horaEntrada != hor)
    //             return true
    //         }
    //         else
    //         {
    //             return false
    //         }   
    //     }
    //     return false
    // }

    def clasear(nom : String, hor : Int) : Boolean = 
    {
        for(i <- this._listaSalones)
        {
            if(i._nombreSalon == nom)
            {
                if(i._mantenimieto == false)
                {
                    var ocupa: ocupacion = new ocupacion(1,hor)
                    i.agregarHorario(ocupa)
                    return true
                }
                else
                {
                    return false
                }
            }    
        }
        return false
    }

    def ponerMantenimiento(nom :String) : Boolean =
    {
        for(i <- this._listaSalones)
        {
            if(i._nombreSalon == nom)
            {
                i.cambiarMantenimiento()
                return true
            }
        }
        return false
    }

    def agregarSalon(nom : String) : Unit =
    {
        var sal : salon = new salon(nom)
        this._listaSalones = sal :: this._listaSalones
    }

    def agregarSalon(sal : salon) : Unit =
    {
        this._listaSalones = sal :: this._listaSalones
    }

}