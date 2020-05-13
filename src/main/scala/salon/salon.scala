package salon
import ocupacion._

class salon
{
    //ATRIBUTOS

    var _listOcupacion : List[ocupacion] = List()
    var _temperatura : Int = 0 //Si la temperatura es 0, el Aire esta apagado
    var _luz : Boolean = false
    var _abierto : Boolean = false
    var _ocupado : ocupacion = _
    var _mantenimieto : Boolean = false
    var _nombreSalon : String = _

    //GETTERS
    
    def listOcupacion = _listOcupacion
    def temperatura = _temperatura
    def luz = _luz
    def abierto = _abierto
    def ocupado = _ocupado
    def mantenimieto = _mantenimieto
    def nombreSalon = _nombreSalon

    //SETTERS

    def nombreSalon_= (nuevoNombre : String) = _nombreSalon = nuevoNombre
    def temperatura_= (nuevaTemperatura : Int) = _temperatura = nuevaTemperatura

    //METODOS

    def this(nom : String)
    {
        this()
        _nombreSalon = nom
    }

    /* def cambiarLuz() : Unit =
    {
        if(luz == False)
        {
            this._luz = True
        }
        else
        {
            this._luz = False
        }
    } */

    def prenderLuz() : Unit =
    {
        this._luz = true
    }

    def apagarLuz() : Unit =
    {
        this._luz = false
    }

    def abrir() : Unit =
    {
        this._abierto = true
    }

    def cambiarMantenimiento() : Unit =
    {
        if(mantenimieto == false)
        {
            this._mantenimieto = true
        }
        else
        {
            this._mantenimieto = false
        }
    }

    def agregarHorario(nuevaOcupacion : ocupacion) : Boolean =
    {
        for(i <- listOcupacion)
        {
            if(i.horaEntrada == nuevaOcupacion.horaEntrada)
            {
                return false
            }
        }

        this._listOcupacion = nuevaOcupacion :: this._listOcupacion
        return true
    }

    def cambiarOcupacion(hora : Int) : Boolean =
    {
        if(mantenimieto != true)
        {
            for(i <- listOcupacion)
            {
                if(i.horaEntrada == hora)
                {
                    this._ocupado = i
                    return true
                }
            }
            this._ocupado = new ocupacion(0,0)
            return false
            
        }
        return false
    }

    def verifyOcupado(hora : Int) : Boolean =
    {
        for(i <- listOcupacion)
        {
           if(i.horaEntrada == hora)
            {
                return true
            }
        }
        return false
    }





}
