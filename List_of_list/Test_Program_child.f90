program test_link
  use Child_linked_list
  implicit none

  type vector
      double precision, dimension(3):: vec
  end type vector
  type(vector)::Vel
  
  type(List):: L
  integer::i,j,length,index,val
  real :: T1,T2,F
  character(len = 1000) :: mystr
  character(:), allocatable :: str2

  class(*), pointer :: data
  ! !-------------
  ! !Append items
  ! !-------------
  print*, "Length Of Required List"
  read(*,*) length
  
  do i = 1,1000
    mystr(i:i) = "a"
  end do
  call srand(123456789)
  call cpu_time(T1)
  do i=1,length
    j = rand()*900 + 100
    ! print*,j
    str2 = mystr(1:j)
    ! print*, "appending"
    call L%push(j) 
    data => L%get(1)
    ! print *, str2
  end do
  call cpu_time(T2)
  i = 1
  write(*,*) T2-T1

  print*, "index"
  read(*,*) index
  data => L%get(index)
  select type (data)
    type is (integer)
    print*, data
  end select
  call L%remove(index);

  data => L%get(index)
  select type (data)
    type is (integer)
    print*, data
  end select

  call srand(123456789)
  call cpu_time(T1)
  do while (i<=100)
    j = rand()*length
    data => L%get(j)
    i = i+1
  end do  
  call cpu_time(T2)

  write(*,*) (T2-T1)
  write(*,*)'Done'

  !-------------
  !Destroy the list and frees the memmory
  !-------------
  call cpu_time(T1)
  call L%destroy()
  call cpu_time(T2)

  if(allocated(str2)) deallocate(str2);

  write(*,*) T2-T1

end program test_link