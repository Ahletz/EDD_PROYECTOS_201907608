module ListasListas

    implicit none 

    private

    type :: sub_nodo

        character(:), allocatable :: Img !nombre de la imagen 
        type(sub_nodo), pointer :: lista => null() !subnodo tipo slitas apunta a nullo

    end type sub_nodo

    type :: nodo !estructura nodo

        character(len = 10) :: nombre 
        type(nodo), pointer :: next => null() !nodo siguiente apunta a nada
        type(nodo), pointer :: last => null() !nodo anterior apunta a nada
        type(sub_nodo), pointer :: lista => null() !apunta a una lista vacia

        contains
    !procedimiento realizables
        procedure :: Add
        procedure :: Print

    end type nodo

    type, public :: LIsta_Listas

        type (nodo), pointer :: head => null()
        type (nodo), pointer :: tail => null()

        contains
    !procedimiento realizables
        procedure :: Insert
        procedure :: Show
        procedure :: Delete
        procedure :: Graficar


    end type LIsta_Listas


    contains 

    subroutine Insert(self, Img, nombre)

        class(LIsta_Listas), intent (inout) :: self
        character (len=*), intent(in) :: Img !capturando dato de la imagen en cola
        character(len =*), intent(in) :: nombre !capturando datos del nombre

        type(nodo), pointer :: aux !nodo auxiliar
        type(nodo), pointer :: now !nodo actual cread
        allocate(now) !aloamiento de memoria del nodo

        if(.not. associated(self%head)) then

            allocate(aux) !si la lista no esta vacia se aloja en la memoria el nodo auxiliar
            aux%nombre  = nombre !asignamos el nombre del cliente a la lista de clientes

            self%head => aux !cabeza a punta a aux 
            self%tail => aux !cola apunta a aux
            call aux%Add(Img) !agreamos el valor llamando la funcion agregar a la lista de espera del indice

        else

        end if






    end subroutine Insert


end module ListasListas