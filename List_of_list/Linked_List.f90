module Linked_List
  use Child_linked_list
  use OMP_LIB
  implicit none

  public :: Parent_Node,Parent_List

  type Parent_Node
    type(Parent_Node), pointer :: next => null()
    type(Parent_Node), pointer :: prev => null()
    type(List) , allocatable          :: child
    contains
    procedure :: size=>child_length
  end type Parent_Node

  type Parent_List
    integer, private           :: num_parent_nodes
    type(Parent_Node), pointer :: head => null()
    type(Parent_Node), pointer :: tail => null()
    contains
    procedure :: append => append_at_child_tail
    procedure :: append_new_child => append_in_new_child
    procedure :: destroy => destroy_whole_parent_list
    procedure :: get => get_element_at_index_in_parent
    ! procedure :: get => get_node_at_index
  end type Parent_List

  contains

  pure function child_length(this_parent_node) result(size)
    implicit none
    class(Parent_Node), intent(in) :: this_parent_node
    integer :: size
    size = this_parent_node%child%size()
  end function child_length

  pure subroutine append_at_child_tail(this_parent_list,item)
    implicit none
    ! initialisation of list to be used and item
    class(Parent_List), intent(inout) :: this_parent_list
    class(*), intent(in) :: item

    ! Finding if its a first node or the list already have a node
    if(this_parent_list%num_parent_nodes==0) then
      call this_parent_list%append_new_child(item)
    else if (this_parent_list%tail%child%size()==10000) then
      call this_parent_list%append_new_child(item)
    else
      call this_parent_list%tail%child%append(item)
    end if
  end subroutine append_at_child_tail

  pure function initialise_parent_node( item ) result( new_node )
    implicit none
    type(Parent_Node) :: new_node
    type(List), intent(in) :: item
    ! allocating item to the new node
    allocate(new_node%child, source=item)
  end function initialise_parent_node

  pure subroutine append_in_new_child(this_parent_list,item)
    implicit none
    ! initialisation of list to be used and item
    class(Parent_List), intent(inout) :: this_parent_list
    class(*), intent(in)  :: item
    type(List)            :: new_child

    call new_child%append(item)
    if(this_parent_list%num_parent_nodes==0)then
      allocate(this_parent_list%head, source=initialise_parent_node(new_child))
      this_parent_list%tail => this_parent_list%head
    else
      allocate(this_parent_list%tail%next, source=initialise_parent_node(new_child))
      this_parent_list%tail%next%prev => this_parent_list%tail
      this_parent_list%tail => this_parent_list%tail%next
    end if
    this_parent_list%num_parent_nodes = this_parent_list%num_parent_nodes + 1
  end subroutine append_in_new_child

  subroutine destroy_whole_parent_list( this_parent_list )
    implicit none
    !Entrada:
    class(Parent_List), intent(inout) :: this_parent_list
    !Local:
    type(Parent_Node), pointer:: current_node
    type(Parent_Node), pointer:: next_node
    integer                   :: count
    integer                   :: number_threads

    !$OMP PARALLEL PRIVATE(current_node,count,next_node)
    number_threads = omp_get_num_threads()
    write(*,*) 'Number of threads:', number_threads
    count = this_parent_list%num_parent_nodes
    next_node => this_parent_list%head
    do while (count>0)
      current_node => next_node
      if(modulo(count,number_threads)==omp_get_thread_num()) then
        call current_node%child%destroy()
        deallocate(current_node%child)
      end if
      next_node => current_node%next
      count = count-1
    end do
    !$OMP END PARALLEL

    ! count = this_parent_list%num_parent_nodes
    ! do while (count>0)
    !   current_node => this_parent_list%head
    !   if (associated(current_node%next)) then
    !     nullify(current_node%next%prev)
    !     this_parent_list%head => current_node%next
    !   end if
    !   ! call current_node%child%destroy()
    !   ! next_node => current_node%next
    !   count = count-1
    !   if(allocated(current_node%child)) deallocate(current_node%child)
    !   nullify(current_node%next)
    !   nullify(current_node%prev)
    !   this_parent_list%num_parent_nodes = this_parent_list%num_parent_nodes - 1
    ! end do

  end subroutine destroy_whole_parent_list

  function get_element_at_index_in_parent( this_parent_list , node_index ) result (return_item)
    implicit none

    class(Parent_List), intent(inout) :: this_parent_list
    integer, intent(in):: node_index
    class(*), pointer :: return_item
    type(Parent_Node), pointer:: current_node
    integer:: count
    !iterating through the list to reach the nth node
    count = node_index
    current_node => this_parent_list%head
    do while ( associated(current_node) )
      if(count<=current_node%child%size()) then
        return_item => current_node%child%get(count)
        return
      else
        count = count - current_node%child%size()
        current_node => current_node%next
      end if
    end do
    nullify(current_node)
    allocate(return_item,source = "Wrong Input")
  end function get_element_at_index_in_parent

end module Linked_List