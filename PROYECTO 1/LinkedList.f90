module ListaSimple

    implicit none

    !estructura del nodo

    type Nodo 

    integer :: ventanilla ! entero que guarda el numero de ventanillas 
    
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

        allocate(NewNodo) !decirle que apunte a un nodo lista
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


    subroutine Graficar()

        type (Nodo), pointer :: now !indicar nodo 

        character(len=30) :: contenido
        integer :: numero !numero de nodo en entero
        character(len=20) :: num1 !numero en string
        character(len=20) :: num2 !numero en string

        integer :: unit

        now => head !indicar nodo apunta a cabeza de lista
        numero = 1 !numerdacion de los nodos

        ! Abrir el archivo DOT
        open(unit, file='Grafica1.dot', status='replace')
        write(unit, *) 'digraph ListaSimple {'

         write(unit,*) 'node [shape=box, style="rounded,filled", fillcolor="lightblue", fontname="Arial"];'
         write(unit,*) 'rankdir = LR;'

        do while(associated(now))

            write (contenido, '(I0)')  now%ventanilla !convertir el valor entero en un string
            write (num1, '(I0)')  numero !convertir el valor en un string
            write (num2, '(I0)')  numero+1 !convertir el valor en un string

            write(*,*)'ventanilla: '//contenido

            write(unit,*)trim(' Nodo'//num1//' [label="Ventanilla '//contenido//'"];')
            

            if (associated(now%next)) then
                write(unit, *)trim('Nodo'//num1//'-> Nodo'//num2//';')
            end if

            numero = numero + 1 !aumentar el numero del nodo al siguiente
            now =>now%next !siugiente nodo

        end do 

        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)

        ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpng -o Grafica1.png Grafica1.dot')

 
    end subroutine Graficar


end module ListaSimple