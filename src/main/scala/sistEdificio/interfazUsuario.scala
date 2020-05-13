package sistEdificio

import ocupacion._
import salon._
import usuario._
import scala.util._
import scala.io._

object interfazUsuario extends App
{
    var sistemaCerrado : Boolean = false
    var lagos : sistema = new sistema()
    while(sistemaCerrado == false)
    {
        bienvenida()
        println("Cerrado")
        sistemaCerrado = true
    }

    def bienvenida() : Unit =
    {
        println("|| Hora: " + lagos._hora + " : "  + lagos._min + " ||")
        println("Bievenido al edificio inteligente de la Pontificia Universidad Javeriana")
        println("Por favor inicie sesion o registrese para continuar")
        println("1-> Registrarse \n2-> Iniciar sesión")
        var opcion : Int = StdIn.readInt()
        opcion match
        {
            case 1 => {
                        var resultado = registrar()
                        resultado match
                        {
                            case Success(s) => println("Paso a registrarse")
                            case Failure(f) => println(f)
                        }
                    }
            case 2 => {
                var resultado = login()
                        resultado match
                        {
                            case Success(s) => println("paso a loguearse")
                            case Failure(f) => println(f)
                        }
                        }
        } 
    }

    def registrar() : Try[Unit] = 
    {
        return Try{
            println("|| Hora: " + lagos._hora + " : "  + lagos._min + " ||")
            println("Ingrese username")
            var us : String = StdIn.readLine()
            println("Ingrese contraseña")
            var pa : String = StdIn.readLine()
            println("Ingrese rango al que pertenece \n0 -> usuario normal \n1 -> administrador")
            var ra : Int = StdIn.readInt()
            lagos.registrar(us,pa,ra)
            println("Usuario registrado correctamente")
            ra match
            {
                case 0 => {
                            var resultado = inicioNormal()
                            resultado match
                            {
                                case Success(s) => println("Sesion iniciada correctamente")
                                case Failure(f) => println(f)
                            }
                        }

                case 1 => {
                                var resultado2 = inicioAdmin()
                                resultado2 match
                                {
                                    case Success(s) => println("Sesion iniciada correctamente")
                                    case Failure(f) => println(f)
                                }
                        }
                case _ => registrar()
            }
        }
    }

    def login() : Try[Unit] =
    {
        return Try{
            println("|| Hora: " + lagos._hora + " : "  + lagos._min + " ||")
            println("Ingrese username")
            var us : String = StdIn.readLine()
            println("Ingrese contraseña")
            var pa : String = StdIn.readLine()
            if(lagos._listaUsuario.isEmpty != true)
            {
                for(i <- lagos._listaUsuario)
                {
                    if(i._user == us)
                    {
                        i._tipoUsu match
                        {
                            case 0 => inicioNormal()
                            case 1 => inicioAdmin()
                        }
                    }
                }
                println("No se encontró su usuario, vuelvalo a ingresar")
                login()
                lagos.pasarHora()
            }
            else
            {
                registrar()
                lagos.pasarHora()
            }
        }
    }

    def inicioNormal() : Try[Unit] =
    {
        return Try{
            println("|| Hora: " + lagos._hora + " : "  + lagos._min + " ||")
            println("Bienvenido usuario ¿Qué desea hacer? \n1 -> Reservar un salon\n2 -> Mostrar reservas\n3 -> Cerrar sesion")
            var op : Int = StdIn.readInt()
            if(op == 1)
            {
                println("Introduzca salon a reservar")
                var sal : String = StdIn.readLine
                println("Introduzca la hora de comienzo de su reserva") 
                var tim : Int = StdIn.readInt
                if ( lagos.reservar(sal,tim) )
                {
                    println("Su reserva fue realizada con exito")
                }
                else
                {
                    println("Su reserva no fue completada con exito, intentelo otra vez")
                }
                inicioNormal()
                lagos.pasarHora()

            }
            if(op == 2)
            {
                lagos.primReservas()
                lagos.pasarHora()
            }
            // if(op == 3)
            // {
            //     println("Introduzca salon a eliminar reserva")
            //     var sal : String = StdIn.readLine
            //     println("Introduzca la hora de su reserva") 
            //     var tim : Int = StdIn.readInt
            //     if ( lagos.eliminarReserva(sal,tim) )
            //     {
            //         println("Su reserva fue eliminada con exito")
            //     }
            //     else
            //     {
            //         println("Su reserva no fue eliminada con exito, intentelo otra vez")
            //     }
            //     inicioNormal()
            //     lagos.pasarHora()
            // }
            if(op == 3)
            {
                bienvenida()
            }
        }
    }

    def inicioAdmin() : Try[Unit] =
    {
        return Try{
            println("|| Hora: " + lagos._hora + " : "  + lagos._min + " ||")
            println("Bienvenido usuario ¿Qué desea hacer? \n1 -> Reservar un salon\n2 -> Mostrar reservas\n3 -> Agendar clase")
            println("4 -> poner en mantenimiento salon\n5 -> Quitar mantenimiento salon \n6 -> Agregar salon")
            println("7 -> verificar luz \n8 -> verificar temperatura \n9 -> avanzar tiempo \n10 -> Cerrar Sistema\n")
            var op : Int = StdIn.readInt()
            if(op == 1)
            {
                println("Introduzca salon a reservar")
                var sal : String = StdIn.readLine
                println("Introduzca la hora de comienzo de su reserva") 
                var tim : Int = StdIn.readInt
                if ( lagos.reservar(sal,tim) )
                {
                    println("Su reserva fue realizada con exito")
                }
                else
                {
                    println("Su reserva no fue completada con exito, intentelo otra vez")
                }
                lagos.pasarHora()
                inicioAdmin()
            

            }
            if(op == 2)
            {
                lagos.primReservas()
                lagos.pasarHora()
                inicioAdmin()
            }

            if(op == 3)
            {
                println("Introduzca salon a agendar clase")
                var sal : String = StdIn.readLine
                println("Introduzca la hora de comienzo de la clase") 
                var tim : Int = StdIn.readInt
                if ( lagos.clasear(sal,tim) )
                {
                    println("Su reserva fue realizada con exito")
                }
                else
                {
                    println("Su reserva no fue completada con exito, intentelo otra vez")
                }
                lagos.pasarHora()
                inicioAdmin()

            }
            if(op == 4)
            {
                println("Introduzca salon al cual desea poner en mantenimiento")
                var sal : String = StdIn.readLine
                if(lagos.ponerMantenimiento(sal))
                {
                    println("La asignacion se hizo correctamente")
                    lagos.pasarHora()
                    inicioAdmin()
                }
                else
                {
                    println("La asignacion no fue exitosa")
                    lagos.pasarHora()
                    inicioAdmin()
                }
            }

            if(op == 5)
            {
                println("Introduzca salon al cual desea quitar del mantenimiento")
                var sal : String = StdIn.readLine
                if(lagos.ponerMantenimiento(sal))
                {
                    println("El salon se quito correctamente")
                    lagos.pasarHora()
                    inicioAdmin()
                }
                else
                {
                    println("Hubo un fallo al intentar quitar el mantenimiento")
                    lagos.pasarHora()
                    inicioAdmin()
                }
            }
            if(op == 6)
            {
                println("Ingrese nombre del salon que desea agregar")
                var sal : String = StdIn.readLine
                lagos.agregarSalon(sal)
                println("Salon agregado")
                lagos.pasarHora()
                inicioAdmin()
            }
            if(op == 7)
            {
                println("Ingrese nombre del salon que desea verificar el estado de la luz")
                var sal : String = StdIn.readLine
                if(lagos.returnSalon(sal)._luz)
                {
                    println("La luz se encuentra encendida")
                    lagos.pasarHora()
                    inicioAdmin()
                }
                else
                {
                    println("La luz se encuentra apagada")
                    lagos.pasarHora()
                    inicioAdmin()
                }
            }
            if( op == 8)
            {   
                println("Ingrese nombre del salon que desea verificar la temperatura")
                var sal : String = StdIn.readLine
                var n : Int = lagos.returnSalon(sal)._temperatura
                println("La temperatura del salon es " + n)
                lagos.pasarHora()
                inicioAdmin()
            }
            if(op == 9)
            {
                println("Ingrese numero del tiempo que quiere avanzar")
                var tim : Int = StdIn.readInt
                lagos.pasarHora(tim)
                println("Horas avanzadas correctamente")

            }
            if(op == 10)
            {
                println("Sitema cerrado")
            }
        }
    }
}

