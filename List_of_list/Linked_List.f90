module Linked_List 
  use Child_linked_list
  implicit none 

  public :: Parent_Node,Parent_List

  type Parent_Node
    type(Parent_Node), pointer :: next => null()
    type(Parent_Node), pointer :: prev => null()
    type(List) , allocatable          :: child 
    contains 
    procedure :: size=>child_length
    procedure :: split=> split_into_two_nodes
    procedure, private :: destroy => parent_node_destroyed
  end type Parent_Node

  type Parent_List
    integer, private           :: num_parent_nodes
    integer, private           :: total_nodes
    type(Parent_Node), pointer :: head => null()
    type(Parent_Node), pointer :: tail => null()
    contains
    procedure :: append => append_at_child_tail
    procedure :: append_new_child => append_in_new_child
    procedure :: remove => pop_node_at_index_parent
    procedure :: destroy => destroy_whole_parent_list
    procedure :: get => get_element_at_index_in_parent 
    procedure :: insert => insert_in_parent_at_index
    procedure :: size => get_total_nodes
  end type Parent_List

  contains

  pure function get_total_nodes (this_parent_list) result (length)
    implicit none 
    class(Parent_List), intent(in) :: this_parent_list
    integer                        :: length
    length = this_parent_list%total_nodes
  end function get_total_nodes

  pure function child_length(this_parent_node) result(size)
    implicit none 
    class(Parent_Node), intent(in) :: this_parent_node
    integer :: size 
    size = this_parent_node%child%size()
  end function child_length

  subroutine append_at_child_tail(this_parent_list,item)
    implicit none 
    ! initialisation of list to be used and item
    class(Parent_List), intent(inout) :: this_parent_list
    class(*), intent(in) :: item
    integer :: temp

    ! Finding if its a first node or the list already have a node
    if(this_parent_list%num_parent_nodes==0) then
      call this_parent_list%append_new_child(item)
    else
      if(this_parent_list%tail%child%size()>9000) then
        temp = 10000-this_parent_list%tail%child%size()
        if(rand()*1000>=temp) then
          call this_parent_list%tail%split();
          this_parent_list%num_parent_nodes = this_parent_list%num_parent_nodes + 1;
          if(associated(this_parent_list%tail%next)) this_parent_list%tail => this_parent_list%tail%next
        end if
      end if
      call this_parent_list%tail%child%push(item)
    end if
    this_parent_list%total_nodes = this_parent_list%total_nodes + 1
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

    call new_child%push(item)
    if(this_parent_list%num_parent_nodes==0)then
      allocate(this_parent_list%head, source=initialise_parent_node(new_child))
      this_parent_list%tail => this_parent_list%head
    else
      allocate(this_parent_list%tail%next, source=initialise_parent_node(new_child))
      this_parent_list%tail%next%prev => this_parent_list%tail
      this_parent_list%tail => this_parent_list%tail%next
      this_parent_list%tail%child%head%prev => this_parent_list%tail%prev%child%tail
      this_parent_list%tail%prev%child%tail%next => this_parent_list%tail%child%head
    end if
    this_parent_list%num_parent_nodes = this_parent_list%num_parent_nodes + 1
  end subroutine append_in_new_child

  pure subroutine destroy_whole_parent_list( this_parent_list )
    implicit none
    !Entrada:
    class(Parent_List), intent(inout) :: this_parent_list
    !Local:
    type(Parent_Node), pointer:: current_node
    do while (.not. this_parent_list%num_parent_nodes==0)
      current_node => this_parent_list%head
      if (associated(current_node%next)) then
        nullify(current_node%next%prev)
        this_parent_list%head => current_node%next
      end if
      call current_node%child%destroy()
      call current_node%destroy()
      deallocate(current_node)
      this_parent_list%num_parent_nodes = this_parent_list%num_parent_nodes - 1
    end do
      
  end subroutine destroy_whole_parent_list

  ! Delete a node from the list and frees the memory in the item.
  pure subroutine parent_node_destroyed( this_parent_list )
    implicit none
    !initialising:
    class(parent_node), intent(inout) :: this_parent_list

    !Deallocate it's item
    if (allocated(this_parent_list%child)) deallocate(this_parent_list%child)
    !Nullify it's pointers
    nullify(this_parent_list%next)
    nullify(this_parent_list%prev)
  end subroutine parent_node_destroyed

  function get_element_at_index_in_parent( this_parent_list , node_index ) result (return_item)
    implicit none
    class(Parent_List), intent(inout) :: this_parent_list
    integer, intent(in):: node_index
    class(*), pointer :: return_item
    type(Parent_Node), pointer:: current_node
    integer:: count
    !iterating through the list to reach the nth node
    if(this_parent_list%total_nodes==0) return
    count = node_index
    if(count<=0) count = 1;
    if(count>=this_parent_list%total_nodes) count = this_parent_list%total_nodes;
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

  pure subroutine pop_node_at_index_parent(this_parent_list, node_index)
    implicit none
        
    class(Parent_List), intent(inout) :: this_parent_list
    integer, intent(in):: node_index

    type(Parent_Node), pointer:: current_node
    integer:: count

    count = node_index
    current_node => this_parent_list%head
    if(node_index<=0) return;
    if(node_index>this_parent_list%total_nodes) return;
    
    do while(count>current_node%child%size())
      count=count-current_node%child%size()
      current_node => current_node%next
    end do
    call current_node%child%remove(count);
    if (current_node%child%size() == 0) then
      if (associated(current_node%prev).and.associated(current_node%next)) then
        !List Node is in mid
        current_node%next%prev => current_node%prev
        current_node%prev%next => current_node%next
  
      else if (associated(current_node%prev)) then
        !List tail
        nullify(current_node%prev%next)
        this_parent_list%tail => current_node%prev
  
      else if (associated(current_node%next)) then
        !List head
        nullify(current_node%next%prev)
        this_parent_list%head => current_node%next
      end if
      !Destroy node content and Free it's memory
      call current_node%destroy()  
      deallocate(current_node)
      !Reduce the count by 1
      this_parent_list%num_parent_nodes = this_parent_list%num_parent_nodes - 1
    end if
    this_parent_list%total_nodes = this_parent_list%total_nodes-1
  end subroutine pop_node_at_index_parent

  subroutine insert_in_parent_at_index(this_parent_list, item, node_index)
    implicit none 
    class(Parent_List), intent(inout) :: this_parent_list
    integer, intent(in):: node_index
    class(*), intent(in)       :: item
    type(Parent_Node), pointer:: current_node

    integer :: count, temp
    count = node_index
    current_node => this_parent_list%head
    if(this_parent_list%total_nodes == 0) then
      call this_parent_list%append(item);
      return
    end if
    if(count<=0) count = 1;
    if(count>this_parent_list%total_nodes) count = this_parent_list%total_nodes+1;
    do while(count>current_node%child%size())
      count= count - current_node%child%size()
      current_node => current_node%next
    end do
    
    if(current_node%child%size()>9000) then
      temp = 10000-current_node%child%size()
      if(rand()*1000>=temp) then
        call current_node%split();
        this_parent_list%num_parent_nodes = this_parent_list%num_parent_nodes + 1;
        if(associated(this_parent_list%tail%next)) this_parent_list%tail => this_parent_list%tail%next
      end if
    end if
    do while(count>current_node%child%size())
      count= count - current_node%child%size()
      current_node => current_node%next
    end do
    call current_node%child%insert(item,count);
    this_parent_list%total_nodes = this_parent_list%total_nodes + 1
  end subroutine insert_in_parent_at_index

  pure subroutine split_into_two_nodes(this_parent_node)
    class(Parent_Node), intent(inout) :: this_parent_node;
    type(Parent_Node), pointer        :: next_parent_node;
    type(node), pointer               :: old_child_tail;
    type(List)                        :: new_list
    integer :: node_child_size
    integer :: count
    node_child_size = this_parent_node%child%size();
    node_child_size = node_child_size/2;
    count = 1
    old_child_tail => this_parent_node%child%head
    do while(count<node_child_size)
      count = count+1
      old_child_tail => old_child_tail%next
    end do
    new_list%head => old_child_tail%next
    new_list%tail => this_parent_node%child%tail
    this_parent_node%child%tail => old_child_tail
    call new_list%set_size(this_parent_node%child%size()-node_child_size)
    call this_parent_node%child%set_size(node_child_size)

    if(associated(this_parent_node%next)) then
      next_parent_node => this_parent_node%next
      allocate(this_parent_node%next, source=initialise_parent_node(new_list))
      this_parent_node%next%next => next_parent_node
      this_parent_node%next%prev => next_parent_node%prev
      next_parent_node%prev => this_parent_node%next
    else
      allocate(this_parent_node%next, source=initialise_parent_node(new_list))
      next_parent_node = this_parent_node
      next_parent_node%next%prev => next_parent_node 
    end if
  end subroutine split_into_two_nodes

end module Linked_List