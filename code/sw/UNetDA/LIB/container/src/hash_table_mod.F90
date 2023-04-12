module hash_table_mod

  use linked_list_mod

  implicit none

  private

  public hash_table_type
  public create_hash_table_iterator
  public hash_table_iterator
  public hash_table_iterator_type

  type hash_table_item_type
    type(linked_list_type) chain
  end type hash_table_item_type

  type hash_table_iterator_type
    type(hash_table_type), pointer :: table => null()
    integer key_index
    character(:), allocatable :: key
    character(:), allocatable :: next_key
    class(*), pointer :: value
  contains
    procedure :: ended => hash_table_iterator_ended
    procedure :: next  => hash_table_iterator_next
  end type hash_table_iterator_type

  type hash_table_type
    character(30), allocatable :: keys(:)
    type(hash_table_item_type), allocatable :: items(:)
    integer :: chunk_size = 1000
    integer :: size = 0
    real :: max_load_factor = 0.6
  contains
    procedure :: init   => hash_table_init
    procedure :: index_number => hash_table_index_number
    procedure :: expand => hash_table_expand
    procedure :: insert => hash_table_insert
    procedure :: remove => hash_table_remove
    procedure :: value  => hash_table_value
    procedure :: hashed => hash_table_hashed
    procedure :: clear  => hash_table_clear
    final :: hash_table_final
  end type hash_table_type

contains

  subroutine hash_table_init(this, chunk_size, max_load_factor)

    class(hash_table_type), intent(inout) :: this
    integer, intent(in), optional :: chunk_size
    real, intent(in), optional :: max_load_factor

    call this%clear()

    if (present(chunk_size)) this%chunk_size = chunk_size
    if (present(max_load_factor)) this%max_load_factor = max_load_factor

    allocate(this%keys (this%chunk_size))
    allocate(this%items(this%chunk_size))

  end subroutine hash_table_init

  ! key -> hash code -> index
  function hash_code(key)

    character(*), intent(in) :: key
    integer hash_code

    integer i

    hash_code = 0
    do i = 1, len_trim(key)
      hash_code = hash_code + i * iachar(key(i:i))
    end do

  end function hash_code

  ! FIXME: When table is expanded, the index may be changed!
  ! index_number = hash_code mod table_size
  function hash_table_index_number(this, hash_code)

    class(hash_table_type), intent(in) :: this
    integer, intent(in) :: hash_code
    integer hash_table_index_number

    hash_table_index_number = mod(hash_code, size(this%items) + 1) + 1

  end function hash_table_index_number

  subroutine hash_table_expand(this)

    class(hash_table_type), intent(inout) :: this

    print *, '[Error]: hash_table_mod: We need to implement hash_table_expand subroutine!'
    stop 5

  end subroutine hash_table_expand

  subroutine hash_table_insert(this, key, value)

    class(hash_table_type), intent(inout) :: this
    character(*), intent(in) :: key
    class(*), intent(in) :: value

    integer i

    i = this%index_number(hash_code(key))
    if (.not. this%hashed(key)) then
      this%size = this%size + 1
      this%keys(this%size) = key
    end if
    call this%items(i)%chain%insert(key, value, nodup=.true.)

  end subroutine hash_table_insert

  subroutine hash_table_remove(this, key)

    class(hash_table_type), intent(inout) :: this
    character(*), intent(in) :: key

    integer i

    if (this%hashed(key)) then
      this%size = this%size - 1
      i = this%index_number(hash_code(key))
      call this%items(i)%chain%clear()
    end if

  end subroutine hash_table_remove

  function hash_table_value(this, key)

    class(hash_table_type), intent(in) :: this
    character(*), intent(in) :: key
    class(*), pointer :: hash_table_value

    integer i

    i = this%index_number(hash_code(key))
    hash_table_value => this%items(i)%chain%value(key)

  end function hash_table_value

  function hash_table_hashed(this, key)

    class(hash_table_type), intent(in) :: this
    character(*), intent(in) :: key
    logical hash_table_hashed

    integer i

    i = this%index_number(hash_code(key))
    hash_table_hashed = associated(this%items(i)%chain%value(key))

  end function hash_table_hashed

  subroutine create_hash_table_iterator(table, iter)

    type(hash_table_type), intent(in), target :: table
    type(hash_table_iterator_type) iter

    iter = hash_table_iterator(table)

  end subroutine create_hash_table_iterator

  function hash_table_iterator(table)

    type(hash_table_type), intent(in), target :: table
    type(hash_table_iterator_type) hash_table_iterator

    hash_table_iterator%table => table
    hash_table_iterator%key_index = 1
    if (table%size > 0) then
      hash_table_iterator%value => table%value(table%keys(1))
      hash_table_iterator%key = table%keys(1)
      if (table%size > 1) hash_table_iterator%next_key = table%keys(2)
    end if

  end function hash_table_iterator

  function hash_table_iterator_ended(this)

    class(hash_table_iterator_type), intent(in) :: this
    logical hash_table_iterator_ended

    hash_table_iterator_ended = this%key_index > this%table%size

  end function hash_table_iterator_ended

  subroutine hash_table_iterator_next(this)

    class(hash_table_iterator_type), intent(inout) :: this

    this%key_index = this%key_index + 1
    if (this%key_index <= this%table%size) then
      this%value => this%table%value(this%next_key)
      this%key = this%next_key
      if (this%key_index + 1 <= this%table%size) this%next_key = this%table%keys(this%key_index + 1)
    end if

  end subroutine hash_table_iterator_next

  recursive subroutine hash_table_clear(this)

    class(hash_table_type), intent(inout) :: this

    if (allocated(this%keys )) deallocate(this%keys )
    if (allocated(this%items)) deallocate(this%items)
    this%size = 0

  end subroutine hash_table_clear

  recursive subroutine hash_table_final(this)

    type(hash_table_type), intent(inout) :: this

    call this%clear()

  end subroutine hash_table_final

end module hash_table_mod
