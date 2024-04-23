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


    subroutine Menu1() !MUESTRA DEL PRIMER MENU PRINCIPAL

        integer :: a !VARAIBLE QUE TOMA LA OPCION ELEGIDA
        a = 0 !INICIALIZACION DE VARIABLE DECISIONES 
        
        do while (a/=6) !CICLO PAARA REPETIR MENU HASTA SELECCIONAR UN DATO CORRECTO

            call Mensaje() !MOSTRAR MENSAJE MENU
            read(*,*) a  !LEER SELECCION USUARIO
            call Seleccion(a)

        end do

    end subroutine Menu1

    subroutine DatosEstudiante() !SUBRUTINA QUE MUESTRA LOS DATOS DEL ESTUDIANTE
        print *, '||---------------------------------------------||'
        print *, '|| NOMBRE: LUDWING ALEXANDER LOPEZ ORTIZ       ||'
        print *, '|| CARNET: 201907608                           ||'
        print *, '|| CARRERA: INGENIERIA EN CIENCIAS Y SISTEMAS. ||'
        print *, '|| CONTACTO: ludwingalexander230@gmail.com     ||'
        print *, '||---------------------------------------------||'

    end subroutine DatosEstudiante

    subroutine Seleccion(x) !SUBRUTINA DE CASOS SEGUN LA SELECCION INGRESADA

        implicit none
        
        integer, intent (in) :: x

        !OPCIONES DEPENDIENDO EL CASO SELECCIONADO 

        if ( x == 1 ) then
            
        end if

        if ( x == 2 ) then
            
        end if

        if ( x == 3 ) then
            
        end if

        if ( x == 4 ) then
            
        end if

        if ( x == 5 ) then
            call DatosEstudiante() !LLAMADA ACCIONES PARA MOSTRAR DATOS DEL ESTUDIANTE
        end if


    end subroutine Seleccion



    
end module Menu
    

