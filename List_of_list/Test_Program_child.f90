program test_link
  use stdlib_child_list
  implicit none

  type vector
      double precision, dimension(3):: vec
  end type vector
  
  type(child_list):: L
  type(Node), pointer :: head
  integer::i,j,length
  real :: T1,T2
  character(len = 1000) :: mystr
  character(:), allocatable :: str2

  class(*), pointer :: data
  ! !-------------
  ! !Append items
  ! !-------------
  print*, "Length Of Required List"
  read(*,*) length
  
  call L%insert(1,1);
  print*, L%size()
  call L%pop();
  print*, L%size()

  call L%insert(1,1);
  call L%reverse()
  ! print list
  head => L%head
  i = 1
  do while(associated(head))
    print*,"print ",i
    data=>head%item
    select type (data)
      type is (integer)
      print*, data
    end select
    data => L%get(i)
    head => head%next
  end do
  !end print

  do i = 1,1000
    mystr(i:i) = "a"
  end do
  call srand(123456789)
  call cpu_time(T1)
  do i=1,length
    j = INT(rand()*900) + 100
    str2 = mystr(1:j)
    call L%push(i) 
  end do
  call cpu_time(T2)
  i = 1
  write(*,*) T2-T1

  i = 1
  call srand(123456789)
  call cpu_time(T1)
  do while (i<=10)
    data => L%get(i)
    select type (data)
      type is (integer)
      print*, data
    end select
    i = i+1
  end do  
  call L%reverse()
  i = 1
  do while (i<=10)
    data => L%get(i)
    select type (data)
      type is (integer)
      print*, data
    end select
    i = i+1
  end do  
  call cpu_time(T2)

  write(*,*) (T2-T1)
  write(*,*)'Done'

  !-------------
  !Destroy the list and frees the memmory
  !-------------
  call cpu_time(T1)
  call L%clear()
  call cpu_time(T2)

  if(allocated(str2)) deallocate(str2);

  write(*,*) T2-T1

end program test_link