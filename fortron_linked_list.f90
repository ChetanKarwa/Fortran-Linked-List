module linked_list
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
      procedure, private :: destroy_all => all_nodes_detroyed
    end type Node
  
    ! Definfing the struct List
    type List
      integer, private :: num_nodes = 0
      type(Node), pointer :: head => null()
      type(Node), pointer :: tail => null()
      contains
      procedure:: append => append_at_tail
      procedure:: destroy => destroy_whole_list
      procedure:: remove => pop_node_at_index
      procedure:: get => get_node_at_index
    end type List
  
    contains
  
    ! making a new_node
    pure function initialise_node( item ) result( new_node )
      implicit none
      type(node) :: new_node
      class(*), intent(in), optional :: item
      ! allocating item to the new node
      allocate(new_node%item, source=item)
    end function initialise_node
  
  
    !appending to the list
    pure subroutine append_at_tail( this_list, item )
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
    end subroutine append_at_tail
  
    ! Pop out a node from the list, by a given number.
    pure subroutine pop_node_at_index( this_list, node_index )
      implicit none
      
      class(list), intent(inout) :: this_list
      integer, intent(in):: node_index
  
      type(node), pointer:: current_node
      integer:: count
  
      !iterating through the list to reach the nth node
      current_node => this_list%head
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
            this_list%num_nodes = this_list%num_nodes - 1;
            return
          end if
          current_node => current_node%next
          count = count+1
      end do
    end subroutine pop_node_at_index

    function get_node_at_index( this_list, node_index ) result (return_item)
      implicit none
      
      class(list), intent(inout) :: this_list
      integer, intent(in):: node_index
      class(*), allocatable :: return_item
      type(node), pointer:: current_node
      integer:: count
      allocate(return_item,source = "Wrong Input")
  
      !iterating through the list to reach the nth node
      current_node => this_list%head
      count = 1
      do while ( associated(current_node) )
          if (count==node_index) then
            return_item = current_node%item
            return
          end if
          current_node => current_node%next
          count = count+1
      end do
    end function get_node_at_index
  
  
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
  
    pure subroutine destroy_whole_list( this_list )
      implicit none
      !Entrada:
      class(list), intent(inout) :: this_list
      !Local:
    
      this_list%num_nodes = 0
      if (associated(this_list%head)) then
          call this_list%head%destroy_all()
          deallocate(this_list%head)
          nullify(this_list%head)
          nullify(this_list%tail)
      end if
      
    end subroutine destroy_whole_list
  
    pure recursive subroutine all_nodes_detroyed( this_node )
      implicit none
      !Entrada:
      class(node), intent(inout) :: this_node
      !Local:
      
      !Deallocate it's item
      if (allocated(this_node%item)) deallocate(this_node%item)
      !Nullify it's pointers
      if (associated(this_node%next)) then
          call this_node%next%destroy_all()
          deallocate(this_node%next)
          nullify(this_node%next)
      end if
      nullify(this_node%prev)
      
    end subroutine all_nodes_detroyed
  
  
  
  end module linked_list
program test_link
  use linked_list
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
  
  type(list):: L
  type(node), pointer:: curr
  integer::i,j
  real :: T1,T2,F

  class(*), allocatable :: data

  do i=1,size(Vel%vec)
      Vel%vec(i) = i
  end do
  call cpu_time(T1)
  ! !-------------
  ! !Append items
  ! !-------------
  do i=1,1000000000
    call L%append(i)
  end do
  call cpu_time(T2)
  write(*,*)'The list until now:'

  write(*,*)'--------------'

  write(*,*)'New List after pop'
  
  write(*,*) T1,T2
  F = 0
  call cpu_time(T1)
  do i=1,100000
  do j=90,100
    data = L%get(j);
    select type (data)
      type is (integer)
          F = F+data;
      class default
          write(*,*)'other'
    end select
  end do
  end do
  call cpu_time(T2)

  write(*,*) T1,T2
  write(*,*)'Done'

  !-------------
  !Destroy the list and frees the memmory
  !-------------
  call L%destroy()

end program test_link
