program temp
  type domainptr
  class(*), allocatable :: item
  end type domainptr
  type(domainptr),dimension(10) :: pointers
  integer :: i = 0
  character :: f
  class(*), allocatable :: lol
  class(*), pointer :: data
  do i = 1,10
    read(*,*) f
    allocate(pointers(i)%item,source=f)
  end do
  
  do i = 1,10
    lol = pointers(i)%item
    select type (lol)
    type is(CHARACTER(*))
    write(*,*) lol
    end select
  end do

  contains

end program temp