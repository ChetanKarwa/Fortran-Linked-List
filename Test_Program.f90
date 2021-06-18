program test_link
  use Linked_List
  implicit none
  
  type struct
      integer:: a=1,b=2,c=3
      double precision::d=5
  end type struct
  type(struct):: Vel2

  type vector
      double precision, dimension(3):: vec
  end type vector
  type(vector)::Vel
  
  type(Parent_List):: L
  integer::i,j,length
  real :: T1,T2,F

  class(*), pointer :: data

  do i=1,size(Vel%vec)
      Vel%vec(i) = i
  end do
  ! !-------------
  ! !Append items
  ! !-------------
  print*, "Length Of Required List"
  read(*,*) length
  
  call cpu_time(T1)
  do i=1,length
    call L%append(i)
  end do
  call cpu_time(T2)
  i = 1

  write(*,*) T2-T1

  call srand(123456789)
  call cpu_time(T1)
  do while (i<=1000)
    j = rand()*length
    data => L%get(j)
    select type (data)
    type is (integer)
    write(*,*) data 
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
  call L%destroy()
  call cpu_time(T2)

  write(*,*) T2-T1

end program test_link