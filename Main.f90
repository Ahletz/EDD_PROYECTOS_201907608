program Main

    use Menu
    use ListaSimple
    implicit none

    ! mensaje "hola mundo" fortran 
    ! print *, 'Hola Mundo!'

    !call Menu1()

    call Add(1)
    call Add(2)
    call Add(3)

    call Show()
    call Graficar("Grafiquita.dot")
  end program Main