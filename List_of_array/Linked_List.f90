module linked_list
  implicit none  
  
  ! making Node and List struct globally available
  public:: Node, List, arrayItem
  integer, parameter :: maxSize = 10000

  type arrayItem
    class(*), allocatable :: item
  end type arrayItem

  ! Definfing the struct Node
  type Node
    type(Node), pointer :: next => null()
    type(Node), pointer :: prev => null()
    type(arrayItem), dimension(maxSize):: array
    integer             :: size
    contains
    procedure, private :: destroy => node_destroyed
    ! procedure, private :: destroy_all => all_nodes_detroyed
  end type Node

  ! Definfing the struct List
  type List
    integer, private :: num_nodes = 0
    type(Node), pointer :: head => null()
    type(Node), pointer :: tail => null()
    contains
    procedure:: append => append_at_tail
    procedure:: destroy => destroy_whole_list
    ! procedure:: remove => pop_node_at_index
    procedure:: get => get_node_at_index
  end type List

  contains

  ! making a new_node
  pure function initialise_node( item ) result( new_node )
    implicit none
    type(Node) :: new_node
    class(*), intent(in) :: item
    ! allocating item to the new node
    allocate(new_node%array(1)%item,source=item) 
    new_node%size = 1
  end function initialise_node

  !appending to the list
  pure subroutine append_at_tail( this_list, item )
    implicit none

    ! initialisation of list to be used and item
    class(list), intent(inout) :: this_list
    class(*), intent(in) :: item
    integer :: num_nodes
    ! Finding if its a first node or the list already have a node
    if (associated(this_list%tail)) then
    num_nodes = this_list%tail%size
        if(num_nodes == maxSize) then
        allocate(this_list%tail%next, source=initialise_node(item))
        this_list%tail%next%prev => this_list%tail
        this_list%tail => this_list%tail%next
        this_list%num_nodes = this_list%num_nodes + 1
        else
          allocate(this_list%tail%array(num_nodes+1)%item,source=item)
          this_list%tail%size = num_nodes+1
        end if
    else
        allocate(this_list%head, source=initialise_node(item))
        this_list%tail => this_list%head
        this_list%num_nodes = 1
    end if
  end subroutine append_at_tail

  function get_node_at_index( this_list, node_index ) result (return_item)
    implicit none
    
    class(list), intent(inout) :: this_list
    integer, intent(in):: node_index
    class(*), pointer :: return_item
    type(node), pointer:: current_node
    integer:: count
    !iterating through the list to reach the nth node
    current_node => this_list%head
    count = node_index
    do while ( associated(current_node) )
      if (count<=current_node%size) then
        return_item => current_node%array(count)%item
        nullify(current_node)
        return
      end if
      current_node => current_node%next
      count = count-current_node%size
    end do
    nullify(current_node)
    allocate(return_item,source = "Wrong Input")
  end function get_node_at_index

  subroutine destroy_whole_list( this_list )
    implicit none
    !Entrada:
    class(list), intent(inout) :: this_list
    !Local:
    type(node), pointer:: current_node
    write(*,*) this_list%num_nodes
    do while (this_list%num_nodes>0)
      current_node => this_list%head
      if (associated(current_node%next)) then
        nullify(current_node%next%prev)
        this_list%head => current_node%next
      end if
      call current_node%destroy()
      deallocate(current_node)
      this_list%num_nodes = this_list%num_nodes - 1
    end do
  end subroutine destroy_whole_list

  ! Delete a node from the list and frees the memory in the item.
  pure subroutine node_destroyed( this_node )
    implicit none
    !initialising:
    class(node), intent(inout) :: this_node
    integer                    :: i

    !Deallocate it's item
    do i=1,maxSize
    if (allocated(this_node%array(i)%item)) deallocate(this_node%array(i)%item)
    end do
    !Nullify it's pointers
    nullify(this_node%next)
    nullify(this_node%prev)
  end subroutine node_destroyed

  ! pure subroutine all_nodes_detroyed( this_node )
  !   implicit none
  !   !Entrada:
  !   class(node), intent(inout) :: this_node
  !   !Local:
  !   type(node), pointer :: current_node
  !   type(node), pointer :: next_node
  !   !Deallocate it's item
  !   current_node = this_node
  !   next_node => current_node%next
  !   do
  !     deallocate(current_node)
  !     if (.not. associated(next_node)) exit
  !     current_node => next_node
  !     next_node => current_node%next
  !   end do

  ! end subroutine all_nodes_detroyed
end module linked_list
