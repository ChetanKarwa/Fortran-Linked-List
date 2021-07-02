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
    ! procedure, private :: destroy => node_destroyed
    ! procedure, private :: destroy_all => all_nodes_detroyed
  end type Node

  ! Definfing the struct List
  type List
    integer, private :: num_nodes = 0
    type(Node), pointer :: head => null()
    type(Node), pointer :: tail => null()
    contains
    procedure:: append => append_at_tail
    ! procedure:: destroy => destroy_whole_list
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
        else
          allocate(this_list%tail%array(num_nodes+1)%item,source=item)
          this_list%tail%size = num_nodes+1
        end if
    else
        allocate(this_list%head, source=initialise_node(item))
        this_list%tail => this_list%head
    end if

    !incrementing number of nodes
    this_list%num_nodes = this_list%num_nodes + 1
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

  ! ! Pop out a node from the list, by a given number.
  ! pure subroutine pop_node_at_index( this_list, node_index )
  !   implicit none
    
  !   class(list), intent(inout) :: this_list
  !   integer, intent(in):: node_index

  !   type(node), pointer:: current_node
  !   integer:: count

  !   !iterating through the list to reach the nth node
  !   current_node => this_list%head
  !   count = 1
  !   do while ( associated(current_node) )
  !     if (count==node_index) then
  !       if (associated(current_node%prev).and.associated(current_node%next)) then
  !         !List Node is in mid
  !         current_node%next%prev => current_node%prev
  !         current_node%prev%next => current_node%next
    
  !       else if (associated(current_node%prev)) then
  !         !List tail
  !         nullify(current_node%prev%next)
  !         this_list%tail => current_node%prev
    
  !       else if (associated(current_node%next)) then
  !         !List head
  !         nullify(current_node%next%prev)
  !         this_list%head => current_node%next
  !       end if
    
  !       !Destroy node content and Free it's memory
  !       call current_node%destroy()  
  !       deallocate(current_node)
    
  !       !Reduce the count by 1
  !       this_list%num_nodes = this_list%num_nodes - 1
  !       return
  !     end if
  !     current_node => current_node%next
  !     count = count+1
  !   end do
  ! end subroutine pop_node_at_index

  ! pure subroutine destroy_whole_list( this_list )
  !   implicit none
  !   !Entrada:
  !   class(list), intent(inout) :: this_list
  !   !Local:
  !   type(node), pointer:: current_node

  !   do while (.not. this_list%num_nodes==0)
  !     current_node => this_list%head
  !     if (associated(current_node%next)) then
  !       nullify(current_node%next%prev)
  !       this_list%head => current_node%next
  !     end if
  !     call current_node%destroy()
  !     deallocate(current_node)
  !     this_list%num_nodes = this_list%num_nodes - 1
  !   end do
    
  ! end subroutine destroy_whole_list

  ! ! Delete a node from the list and frees the memory in the item.
  ! pure subroutine node_destroyed( this_node )
  !   implicit none
  !   !initialising:
  !   class(node), intent(inout) :: this_node

  !   !Deallocate it's item
  !   if (allocated(this_node%item)) deallocate(this_node%item)
  !   !Nullify it's pointers
  !   nullify(this_node%next)
  !   nullify(this_node%prev)
  ! end subroutine node_destroyed

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
