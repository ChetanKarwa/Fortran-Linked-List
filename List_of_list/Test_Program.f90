program test_link
  use stdlib_linked_list
  implicit none
  
  type struct
      integer:: a=1,b=2,c=3
      double precision::d=5
  end type struct

  type vector
      double precision, dimension(3):: vec
  end type vector
  type(vector)::Vel
  
  type(linked_list):: L
  type(Node),pointer :: child_head
  integer::i,j,length,index
  real :: T1,T2
  character(len = 1000) :: mystr
  character(:), allocatable :: str2

  class(*), pointer :: data

  call L%append(1)
end program test_link