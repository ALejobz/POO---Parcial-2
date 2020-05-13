package ocupacion

class ocupacion
{
    // ATRIBUTOS

    var _tipo : Int = 0 //0 -> NADA 1-> CLASE 2-> RESERVA
    var _idReserva : Int = 0
    var _horaEntrada : Int = 0
    var _horaSalida : Int = 0

    // GETTERS

    def tipo = _tipo
    def horaEntrada = _horaEntrada
    def horaSalida = _horaSalida

    //METODOS

    def this(tipoOcu : Int,horaEnt : Int)
    {
        this()
        _tipo = tipoOcu
        _horaEntrada = horaEnt
        _horaSalida = horaEnt + 2
    }



}