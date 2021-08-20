program test_link
  use stdlib_linked_list
  implicit none

  type vector
      double precision, dimension(3):: vec
  end type vector
  
  type(linked_list):: L,L2,L3
  type(parent_node), pointer :: head
  type(node), pointer :: current_node
  integer::i,j,length
  real :: T1,T2
  character(len = 1000) :: mystr
  character(:), allocatable :: str2

  class(*), pointer :: data
  ! !-------------
  ! !Append items
  ! !-------------
  print*, "Length Of Required List"
  ! read(*,*) Length
  Length = 30000
  i = 1
  do while(Length>0)
    call L%push(i)
    Length = Length - 1
    i = i+1
  end do

  
  head => L%head;
  do while(associated(head))
    print*, head%child%size()
    head => head%next
  end do

  read(*,*) i;
  read(*,*) j;

  call L%splice(i,j)
  print*, "khatam"
  head => L%head;

  do while(associated(head))
    print*, head%child%size()
    head => head%next
  end do

  print*, L%size()
  print*, L%number_of_parent_nodes()
  
  call L2%push(150)
  call L2%push(250)
  call L2%push(350)
  print*,"absorb L2"
  ! print list

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

end program test_link