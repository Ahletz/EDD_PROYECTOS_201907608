module Cola_recepcion

    implicit none !eliminar implicidad
    private 

    !estructura del nodo(atributos del nodo)
    type, public :: Nodo 
        private 
        integer :: id
        character(len = 50) :: nombre 
        integer :: imgG
        integer :: imgM
        integer :: imgP
        type(Nodo), pointer :: next
    end type Nodo

    type, public :: Cola

    private
    !definir a donde apuntaran 
    type(Nodo), pointer :: head => null() !cabeza a punta a nulo
    type(Nodo), pointer :: tail => null() !cola apunta a nulo
    
    contains
    !metodos que contrendra el tipo cola
    procedure :: Add
    procedure :: Delete
    procedure :: Show
    procedure :: Graficar

    end type Cola

    contains

    ! AGREGAR UN ELEMENTO A LA COLA

    subroutine Add(this, id, nombre, imgG, imgP)

        class(Cola), intent(inout) :: this !indicar this cola
        !variables de entrada
        integer, intent(in):: id
        integer, intent(in):: imgG
        integer, intent(in):: imgP
        character(len=*) :: nombre
    
        !apuntador del nodo actual 
        type(Nodo), pointer :: now 
        allocate(now)
        
        !indicar el contenido del nodo
        now%id = id
        now%nombre = nombre
        now%imgG = imgG
        now%imgP = imgP 
    
        !agregar el nodo a la cola
        if (.not. associated(this%head)) then !si el nodo se encuentra vacío
            this%head => now !a donde apunta cabeza
            this%tail => now !a donde apunta la cola
        else
            this%tail%next => now !cola del siguiente apunta a actual
            this%tail => now !cola actual apunta actual 
        end if
    
    end subroutine Add

    subroutine Delete(this)

        class(Cola), intent(inout) :: this !indicar this cola
        type(Nodo), pointer :: now 

        if (.not. associated(this%head)) then !si la cola se encuentra vacia
            print *, 'Cola de recepcion vacia' !mensaje de cola vacia
            return !salir de la funcion
        end if

        !eliminar primer elemento de la cola reasignando memoria
        now => this%head !actual pasa a ser la cabeza
        this%head => this%head%next !cabeza pasa a ser el nodo siguiente
        deallocate(now) !se elimina el actual de la memoria 

    end subroutine Delete

    subroutine Show(this)

        class(Cola), intent(inout) :: this !indicar this cola
        type(Nodo), pointer :: now 

        now => this%head !apuntador actual a cabeza de la cola

        print *, 'contenido de la cola'
        
        do while (associated(now)) !si hay elemento de la cola
            print *, now%id !imprimir id de la persona y su nombre
            print *, now%nombre
            now => now%next !cambio de nodo
        end do 

    end subroutine Show

    subroutine Graficar(this)

        class(Cola), intent(inout) :: this !indicar this cola
        type(Nodo), pointer :: now 

        now => this%head !apuntador actual a cabeza de la cola

        print *, 'contenido de la cola'
        
        do while (associated(now)) !si hay elemento de la cola
            print *, now%id !imprimir id de la persona y su nombre
            print *, now%nombre
            now => now%next !cambio de nodo
        end do 

    end subroutine Graficar


end module Cola_recepcion