module linked_list_2

  !initialising node and list
  public :: Node, List

  ! Defining Node struct
  type Node
    type(Node), pointer :: next=>null()
    type(Node), pointer :: prev=>null()
    Integer, public     :: item
    contains
    procedure, private  :: destroy => destroy_node
    procedure, private  :: destroy_all => destroy_all_node
  end type Node 

  ! Defining List struct
  type List 
    Integer, private    :: size
    type(Node), pointer :: head=>null()
    type(Node), pointer :: tail=>null()
    contains
    procedure:: append    => append_at_tail
    procedure:: destroy   => destroy_whole_list
    ! procedure:: remove    => pop_node_at_index
    procedure:: get       => get_node_at_index
    ! procedure:: get_size  => get_list_size 
  end type List

  contains

  !make a node
  pure function initialise_node(item) result(new_node)
    implicit none
    type(Node)                      :: new_node
    integer, intent(in), optional   :: item
    new_node%item = item
  end function initialise_node

  !appending to the list
  pure subroutine append_at_tail( this_list, item )
    implicit none
    class(List), intent(inout)      :: this_list
    integer, intent(in), optional   :: item
    if (this_list%size>0) then
      allocate(this_list%tail%next, source=initialise_node(item))
      this_list%tail%next%prev => this_list%tail
      this_list%tail => this_list%tail%next
    else
      allocate(this_list%head, source=initialise_node(item))
      this_list%tail => this_list%head
    end if
    this_list%size = this_list%size + 1
  end subroutine append_at_tail

  function get_node_at_index( this_list, node_index ) result (return_item)
    implicit none
    class(List), intent(inout)  :: this_list
    integer, intent(in)         :: node_index
    integer                     :: return_item
    type(Node), pointer         :: current_node
    integer                     :: count

    !iterating through the list to reach the nth node
    return_item = 0
    current_node => this_list%head
    if(this_list%size>=node_index) then
      do count=1,node_index-1
        current_node => current_node%next
      end do
      return_item = current_node%item
    end if
  end function get_node_at_index

  pure subroutine destroy_node(this_node)
    implicit none
    !initialising:
    class(Node), intent(inout) :: this_node
    !Nullify it's pointers
    nullify(this_node%next)
    nullify(this_node%prev)
  end subroutine destroy_node

  pure subroutine destroy_whole_list( this_list )
    implicit none
    !Entrada:
    class(List), intent(inout) :: this_list
    !Local:
    this_list%size = 0
    if (associated(this_list%head)) then
      call this_list%head%destroy_all()
      deallocate(this_list%head)
      nullify(this_list%head)
      nullify(this_list%tail)
    end if
  end subroutine destroy_whole_list

  pure recursive subroutine destroy_all_node( this_node )
    implicit none
    !Entrada:
    class(Node), intent(inout) :: this_node
    !Local:
    !Deallocate it's item
    !Nullify it's pointers
    if (associated(this_node%next)) then
      call this_node%next%destroy_all()
      deallocate(this_node%next)
      nullify(this_node%next)
    end if
    nullify(this_node%prev)
  end subroutine destroy_all_node

end module linked_list_2


program testpro
  use linked_list_2
  implicit none 
  integer       :: i 
  type(List)    :: L 
  real          :: T1,T2

  call cpu_time(T1)
  do i=1,100000
    call L%append(i)
  end do
  call cpu_time(T2)

  write(*,*) T1,T2
  
  call L%destroy()
end program testpro