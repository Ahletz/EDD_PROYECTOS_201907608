module Menu
    implicit none

    public :: Mensaje
    
    contains

    subroutine Mensaje() !ESTRUCTURA PARA MENSAJE DE MENU DE SELECCION
        print *, '||------|| SELECCIONE UNA DE LAS SIGUIENTES OPCIONES: ||------||'
        print *, '|| 1. PARAMETROS INICIALES.'
        print *, '|| 2. EJECUTAR PASO.'
        print *, '|| 3. ESTADO EN MEMORIA ESTRUCTURAS'
        print *, '|| 4. REPORTES.'
        print *, '|| 5. ACERCA DE '
        print *, '|| 6. SALIR. '
    end subroutine Mensaje



    
end module Menu
    

