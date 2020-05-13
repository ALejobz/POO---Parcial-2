package usuario

class usuario
{
    var _tipoUsu: Int =_ 
    var _user : String =_
    var _pass : String =_

    //Getters
    def tipoUsu = _tipoUsu
    def user = _user
    def pass = _pass

    //Metodos
    def this(nom : String, pas : String,tip : Int)
    {
        this()
        _tipoUsu = tip
        _user = nom
        _pass = pas
    }
}