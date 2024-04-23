module Pila

    implicit none

    type  Nodo !tipo nod 

    integer :: NumVentanilla !ventanilla de la pila
    character :: Imagen !imagen a la que pertenece 
    type (Nodo ), pointer :: next => null() !apuntador siguiente null 

    end type Nodo

    type (nodo), pointer :: top => null() !punta apunta a nulo
    type (Nodo), pointer :: now => null() !actual apunta a nulo


    contains 

    subroutine Push(Num, Img)

        !capturamos los datos ingresados 
        integer, intent(in) :: Num
        character, intent(in) :: Img

        !intanciamos la direccion
        type(Nodo), pointer :: New

        allocate(New) !asignamos memoria
        New%NumVentanilla = Num !asignamos los datos
        New%Imagen = Img

        !apuntamos al siguiente nulo 
        New%next => null()

        !si la lista esta vaciaS
        if(.not. associated(top)) then !si la cabeza esta vacia 
            top => New !cabeza se velve el nodo actual
            now => New !actual es el nuevo nodo

        else 

            !si la lista contiene un valor, entonces
            now%next => New !el sigueinte se vuelve el nuevo nodo 
            now => New !el actual es el nuevo nodo 

        end if 
    end subroutine Push

    subroutine Pop()

        type(Nodo), pointer :: now 

        if (.not. associated(now)) then

            print *, 'NO HAY ELEMENTOS EN LA PILA'

        else

            now => top !definimos el nodo actual como la cabeza
            top => top%next !la cabeza pasa a apuntar a la siguiente cabeza
            deallocate(now) !eliminamos el segmento de memoria de ese nodo

        end if

    end subroutine Pop


end module Pila  