module Child_linked_list
    implicit none  
    
    ! making Node and List struct globally available
    public:: Node, List
  
    ! Definfing the struct Node
    type Node
      type(Node), pointer :: next => null()
      type(Node), pointer :: prev => null()
      class(*), allocatable :: item
      contains
      procedure, private :: destroy => node_destroyed
      procedure, private :: destroy_all => all_nodes_destroyed
    end type Node
  
    ! Definfing the struct List
    type List
      integer, private :: num_nodes = 0
      type(Node), pointer :: head => null()
      type(Node), pointer :: tail => null()
      contains
      procedure:: push => push_at_tail
      procedure:: insert => insert_at_index
      procedure:: destroy => destroy_whole_list
      procedure:: remove => pop_node_at_index
      procedure:: pop => pop_node_at_tail
      procedure:: get => get_node_at_index
      procedure:: size => get_length
      procedure:: set_size => set_length
      procedure:: replace => replace_at_index
      procedure:: reverse => reverse_list
    end type List
  
    contains
  

    pure function get_length (this_list) result (length)
      implicit none 
      class(list), intent(in)  :: this_list
      integer                     :: length 
      length = this_list%num_nodes
    end function get_length

    pure subroutine set_length (this_list, length)
      implicit none 
      class(list), intent(inout)  :: this_list
      integer, intent(in)      :: length 
      this_list%num_nodes = length
    end subroutine set_length

    ! making a new_node
    pure function initialise_node( item ) result( new_node )
      implicit none
      type(node) :: new_node
      class(*), intent(in), optional :: item
      ! allocating item to the new node
      allocate(new_node%item, source=item)
    end function initialise_node
  
  
    !pushing to the list
    pure subroutine push_at_tail( this_list, item )
      implicit none
   
      ! initialisation of list to be used and item
      class(list), intent(inout) :: this_list
      class(*), intent(in) :: item
  
      ! Finding if its a first node or the list already have a node
      if (associated(this_list%tail)) then
          allocate(this_list%tail%next, source=initialise_node(item))
          this_list%tail%next%prev => this_list%tail
          this_list%tail => this_list%tail%next
      else
          allocate(this_list%head, source=initialise_node(item))
          this_list%tail => this_list%head
      end if
  
      !incrementing number of nodes
      this_list%num_nodes = this_list%num_nodes + 1
    end subroutine push_at_tail
  
    function get_node_at_index( this_list, node_index ) result (return_item)
      implicit none
      
      class(list), intent(inout) :: this_list
      integer, intent(in):: node_index
      class(*), pointer :: return_item
      type(node), pointer:: current_node
      integer:: count
      !iterating through the list to reach the nth node
      current_node => this_list%head
      count = 1
      do while ( associated(current_node) )
        if (count==node_index) then
          return_item => current_node%item
          nullify(current_node)
          return
        end if
        current_node => current_node%next
        count = count+1
      end do
      nullify(current_node)
      allocate(return_item,source = "Wrong Input")
    end function get_node_at_index

    ! Pop out a node from the list, by a given number.
    pure subroutine pop_node_at_index( this_list, node_index )
      implicit none
      
      class(list), intent(inout) :: this_list
      integer, intent(in):: node_index
  
      type(node), pointer:: current_node
      integer:: count
      !iterating through the list to reach the nth node
      current_node => this_list%head
      if(node_index<=0) return;
      if(node_index>this_list%num_nodes) return;
      count = 1
      do while ( associated(current_node) )
        if (count==node_index) then
          if (associated(current_node%prev).and.associated(current_node%next)) then
            !List Node is in mid
            current_node%next%prev => current_node%prev
            current_node%prev%next => current_node%next
      
          else if (associated(current_node%prev)) then
            !List tail
            nullify(current_node%prev%next)
            this_list%tail => current_node%prev
      
          else if (associated(current_node%next)) then
            !List head
            nullify(current_node%next%prev)
            this_list%head => current_node%next
          end if
      
          !Destroy node content and Free it's memory
          call current_node%destroy()  
          deallocate(current_node)
      
          !Reduce the count by 1
          this_list%num_nodes = this_list%num_nodes - 1
          return
        end if
        current_node => current_node%next
        count = count+1
      end do
    end subroutine pop_node_at_index

    ! Pop out a node from the list
    pure subroutine pop_node_at_tail( this_list )
      implicit none
      
      class(list), intent(inout) :: this_list
  
      type(node), pointer:: current_node

      if(this_list%num_nodes == 0) return;

      current_node => this_list%tail
      nullify(current_node%prev%next)
      this_list%tail => current_node%prev
      
      !Destroy node content and Free it's memory
      call current_node%destroy()  
      deallocate(current_node)
      
      !Reduce the count by 1
      this_list%num_nodes = this_list%num_nodes - 1
    end subroutine pop_node_at_tail
  
    pure subroutine destroy_whole_list( this_list )
      implicit none
      !Entrada:
      class(list), intent(inout) :: this_list
      !Local:
      type(node), pointer:: current_node

      do while (.not. this_list%num_nodes==0)
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
  
      !Deallocate it's item
      if (allocated(this_node%item)) deallocate(this_node%item)
      !Nullify it's pointers
      nullify(this_node%next)
      nullify(this_node%prev)
    end subroutine node_destroyed
  
    pure subroutine all_nodes_destroyed( this_node )
      implicit none
      !Entrada:
      class(node), intent(inout) :: this_node
      !Local:
      type(node), pointer :: current_node
      type(node), pointer :: next_node
      !Deallocate it's item
      current_node = this_node
      next_node => current_node%next
      do
        deallocate(current_node)
        if (.not. associated(next_node)) exit
        current_node => next_node
        next_node => current_node%next
      end do

    end subroutine all_nodes_destroyed

    pure subroutine insert_at_index( this_list, item ,node_index )
      implicit none

      class(list), intent(inout) :: this_list 
      integer, intent(in)        :: node_index
      class(*), intent(in)       :: item
      type(node), pointer        :: current_node
      type(node), pointer        :: next_node

      integer :: count
      count = node_index-1;
      if(count>=this_list%num_nodes) then
        call this_list%push(item)
      else if(count<=0) then
        current_node => this_list%head
        allocate(this_list%head,source = initialise_node(item))
        this_list%head%next => current_node
        current_node%prev   => this_list%head
      else
        current_node => this_list%head
        do while(count>1)
          count = count-1;
          current_node => current_node%next;
        end do
        next_node => current_node%next
        allocate(current_node%next,source = initialise_node(item))
        current_node%next%prev => current_node
        current_node%next%next => next_node
        current_node => current_node%next 
        current_node%next%prev => current_node
      end if
      this_list%num_nodes = this_list%num_nodes + 1;
    end subroutine insert_at_index

    pure subroutine replace_at_index( this_list, item ,node_index )
      implicit none

      class(list), intent(inout) :: this_list 
      integer, intent(in)        :: node_index
      class(*), intent(in)       :: item
      type(node), pointer        :: current_node
      integer :: count
      
      count = node_index;
      if(count<1 .or. count>this_list%num_nodes) return;
      current_node => this_list%head;
      do while(count>1)
        count = count-1;
        current_node => current_node%next;
      end do
      current_node%item = item
    end subroutine replace_at_index

    pure subroutine reverse_list (this_list)
      class(list), intent(inout) :: this_list 
      type(node), pointer        :: temp_node
      type(node), pointer        :: curr_node

      nullify(temp_node) 
      curr_node => this_list%head
      do while (associated(curr_node))
        temp_node => curr_node%prev;
        curr_node%prev => curr_node%next;
        curr_node%next => temp_node;            
        curr_node => curr_node%prev;
      end do
      
      temp_node=> this_list%head
      this_list%head => this_list%tail
      this_list%tail => temp_node

    end subroutine reverse_list
end module Child_linked_list

