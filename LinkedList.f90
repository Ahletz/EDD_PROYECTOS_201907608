module ListaSimple

    implicit none

    !estructura del nodo

    type Nodo 

    integer :: ventanilla ! entero que guarda el nmero de ventanillas 

    type(Nodo), pointer :: next => null()

    end type Nodo


    !declarar primer nodo y su siguiente 

    type(Nodo), pointer :: head => null() !apntador nodo cabeza
    type(Nodo), pointer :: now => null() !apuntador nodo actual 

    contains

    !subrutina para poder agregar un nuuevo nodo a la lista 

    subroutine Add(window)

        integer, intent(in) :: window !definimos tipo de dato ingresado
        type (Nodo), pointer :: NewNodo !creamos un nuevo nodo llamado NewNodo

        !crear nuevo nodo 

        allocate(NewNodo)
        NewNodo%ventanilla = window !asignar valor de la ventanillas
        NewNodo%next => null() !apntar al sigueinte nodo vacio


        !si la lista esta vaciaS
        if(.not. associated(head)) then !si la cabeza esta vacia 
            head => NewNodo !cabeza se velve el nodo actual
            now => NewNodo !actual es el nuevo nodo

        else 

            !si la lista contiene un valor, entonces

            now%next => NewNodo !el sigueinte se vuelve el nuevo nodo 
            now => NewNodo !el actual es el nuevo nodo 

        end if 

    end subroutine Add !terminamos la sbrutina



    !Mostrar los datos guardados dentro de la lista 

    subroutine Show()

        type(Nodo), pointer :: now !nodo actual

        now => head !apunta a la cabea de la lista

        !recorrer la lista y mostrar los datos 

        write(*,*) ' DATOS EN LA LISTA: '

        do while(associated(now)) !ciclo para recorrer la lista de datos 
            write(*,*) now%ventanilla !mostrar el dato del numero de la ventana 
            now=>now%next !apuntar al siguiente nodo de la lista 

        end do

    end subroutine Show



end module ListaSimple