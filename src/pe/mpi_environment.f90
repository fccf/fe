
module mpi_environment
!< MPI environment interface 

use PENF, only : I4P, I8P, R4P, R8P
use XH5For_utils

#ifdef ENABLE_MPI
#ifdef MPI_MOD
    use mpi
#endif
implicit none
#ifdef MPI_H
    include 'mpif.h'
#endif
#else
implicit none
#endif

#include "assert.i90"

private

    integer(I4P), private, parameter :: MPI_ENV_STATE_START = 0
    integer(I4P), private, parameter :: MPI_ENV_STATE_INIT  = 1

    !-----------------------------------------------------------------
    ! XH5FOR State Transition Diagram
    !-----------------------------------------------------------------
    ! - This diagram controls the basic life cycle of the XH5For library
    !-----------------------------------------------------------------
    !       INIT STATE      |     ACTION      |      FINAL STATE
    !-----------------------------------------------------------------
    ! START                 | Free            | START
    ! START                 | Initialize      | INIT
    !-----------------------------------------------------------------
    ! INIT                  | Free            | START

    type mpi_env_t
    private
    !-----------------------------------------------------------------
    !< MPI environment type
    !-----------------------------------------------------------------
        integer(I4P) :: comm  = 0                    !< MPI communicator
        integer(I4P) :: root  = 0                    !< MPI root processor
        integer(I4P) :: rank  = 0                    !< MPI rank
        integer(I4P) :: info  = 0                    !< MPI info
        integer(I4P) :: size  = 1                    !< MPI communicator size
        logical(I4P) :: parallel = .false.           !< Flag to check if MPI was initialized
        integer(I4P) :: State = MPI_ENV_STATE_START  !< MPI communicator size
    contains
    private
        procedure         :: mpi_env_allgather_single_int_value_I4P
        procedure         :: mpi_env_allgather_single_int_value_I8P
        procedure         :: mpi_env_broadcast_int_I4P
        procedure         :: mpi_env_broadcast_int_I8P
        procedure         :: mpi_env_broadcast_int_I4P_array
        procedure         :: mpi_env_broadcast_int_I8P_array
        procedure         :: mpi_env_broadcast_string
        procedure, public :: Initialize                   => mpi_env_Initialize
        procedure, public :: Free                         => mpi_env_Free
        procedure, public :: get_comm                     => mpi_env_get_comm
        procedure, public :: get_root                     => mpi_env_get_root
        procedure, public :: get_rank                     => mpi_env_get_rank
        procedure, public :: get_info                     => mpi_env_get_info
        procedure, public :: get_comm_size                => mpi_env_get_comm_size
        procedure, public :: is_root                      => mpi_env_is_root
        procedure, public :: is_parallel                  => mpi_env_is_parallel
        procedure, public :: mpi_wtime                    => mpi_env_wtime
        procedure, public :: barrier                      => mpi_env_barrier
        generic,   public :: mpi_allgather                => mpi_env_allgather_single_int_value_I4P, &
                                                             mpi_env_allgather_single_int_value_I8P
        generic,   public :: mpi_broadcast                => mpi_env_broadcast_int_I4P,       &
                                                             mpi_env_broadcast_int_I8P,       &
                                                             mpi_env_broadcast_int_I4P_array, &
                                                             mpi_env_broadcast_int_I8P_array, &
                                                             mpi_env_broadcast_string
    end type mpi_env_t

public :: mpi_env_t

contains

    subroutine mpi_env_initialize(this, comm, info, root, mpierror)
    !-----------------------------------------------------------------
    !< Default MPI values initialization
    !-----------------------------------------------------------------
        class(mpi_env_t),       intent(INOUT) :: this                 !< MPI environment
        integer(I4P), optional, intent(IN)    :: comm                 !< MPI communicator
        integer(I4P), optional, intent(IN)    :: info                 !< MPI info
        integer(I4P), optional, intent(IN)    :: root                 !< MPI root processor
        integer(I4P), optional, intent(INOUT) :: mpierror             !< MPI error
        integer(I4P)                          :: mpierr               !< Aux variable for MPI error checking
    !-----------------------------------------------------------------
        assert(this%State == MPI_ENV_STATE_START)
        mpierr    = 0
        this%comm = 0
        this%root = 0
        this%rank = 0
        this%info = 0
        this%size = 1
#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
        call MPI_Initialized(this%parallel, mpierr)
        if(this%parallel) then
            if(present(info)) then
                call MPI_INFO_DUP( info, this%info, mpierr)
            else
                this%info = MPI_INFO_NULL
            endif
            if(present(comm)) then
                call MPI_COMM_DUP(comm, this%comm, mpierr)
            else
                call MPI_COMM_DUP(MPI_COMM_WORLD, this%comm, mpierr)
            endif
            call MPI_COMM_RANK (this%comm, this%rank, mpierr)
            call MPI_COMM_SIZE (this%comm, this%size, mpierr)
            if(present(root)) then
                this%root = root
            else
                this%root = 0_I4P
            endif
        endif
#endif
        if(present(mpierror)) mpierror = mpierr
        this%State = MPI_ENV_STATE_INIT
    end subroutine mpi_env_initialize


    subroutine mpi_env_Free(this)
    !-----------------------------------------------------------------
    !< Free MPI derived type
    !-----------------------------------------------------------------
        class(mpi_env_t), intent(INOUT) :: this                       !< MPI environment
    !-----------------------------------------------------------------
        ! No allocatable variables. Default initialization
        this%comm = 0_I4P
        this%root = 0_I4P
        this%rank = 0_I4P
        this%info = 0_I4P
        this%size = 1_I4P
        this%State = MPI_ENV_STATE_START
    end subroutine mpi_env_Free

    function mpi_env_get_comm(this) result(comm)
    !-----------------------------------------------------------------
    !< Return MPI communicator
    !-----------------------------------------------------------------
        class(mpi_env_t), intent(IN) :: this                          !< MPI environment
        integer(I4P)                 :: comm                          !< MPI communicator
    !-----------------------------------------------------------------
        assert(this%State == MPI_ENV_STATE_INIT)
        comm = this%comm
    end function mpi_env_get_comm

    function mpi_env_get_root(this) result(root)
    !-----------------------------------------------------------------
    !< Return MPI root processor
    !-----------------------------------------------------------------
        class(mpi_env_t), intent(IN) :: this                          !< MPI environment
        integer(I4P)                 :: root                          !< MPI root processor
    !-----------------------------------------------------------------
        assert(this%State == MPI_ENV_STATE_INIT)
        root = this%root
    end function mpi_env_get_root

    function mpi_env_get_rank(this) result(rank)
    !-----------------------------------------------------------------
    !< Return MPI rank
    !-----------------------------------------------------------------
        class(mpi_env_t), intent(IN) :: this                          !< MPI environment
        integer(I4P)                 :: rank                          !< MPI rank
    !-----------------------------------------------------------------
        assert(this%State == MPI_ENV_STATE_INIT)
        rank = this%rank
    end function mpi_env_get_rank

    function mpi_env_get_info(this) result(info)
    !-----------------------------------------------------------------
    !< Return MPI info
    !-----------------------------------------------------------------
        class(mpi_env_t), intent(IN) :: this                          !< MPI environment
        integer(I4P)                 :: info                          !< MPI info
    !-----------------------------------------------------------------
        assert(this%State == MPI_ENV_STATE_INIT)
        info = this%info
    end function mpi_env_get_info

    function mpi_env_get_comm_size(this) result(size)
    !-----------------------------------------------------------------
    !< Return MPI communicator size
    !-----------------------------------------------------------------
        class(mpi_env_t), intent(IN) :: this                          !< MPI environment
        integer(I4P)                 :: size                          !< MPI communicator size
    !-----------------------------------------------------------------
        assert(this%State == MPI_ENV_STATE_INIT)
        size = this%size
    end function mpi_env_get_comm_size

    subroutine mpi_env_barrier(this, mpierror)
    !-----------------------------------------------------------------
    !< MPI_barrier interface
    !-----------------------------------------------------------------
        class(mpi_env_t),          intent(IN)    :: this                !< MPI environment
        integer(I4P), optional,    intent(INOUT) :: mpierror            !< MPI error
        integer(I4P)                             :: mpierr              !< Aux variable for MPI error
    !-----------------------------------------------------------------
        assert(this%State == MPI_ENV_STATE_INIT)
        mpierr = 0
#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
        if(this%parallel) then
            call MPI_BARRIER(this%comm, mpierr)
        endif
#endif
        if(present(mpierror)) mpierror = mpierr
    end subroutine mpi_env_barrier

    subroutine mpi_env_allgather_single_int_value_I4P(this, send_data, recv_data, mpierror)
    !-----------------------------------------------------------------
    !< MPI_allgather interface for a single I4P value per task
    !-----------------------------------------------------------------
        class(mpi_env_t),          intent(IN)    :: this                !< MPI environment
        integer(I4P),              intent(IN)    :: send_data           !< MPI_allgather send data
        integer(I4P), allocatable, intent(INOUT) :: recv_data(:)        !< MPI_allgather receive data
        integer(I4P), optional,    intent(INOUT) :: mpierror            !< MPI error
        integer(I4P)                             :: mpierr              !< Aux variable for MPI error
        integer(I4P)                             :: comm_size           !< MPI communicator size aux variable
    !-----------------------------------------------------------------
        assert(this%State == MPI_ENV_STATE_INIT)
        if(present(mpierror)) mpierror = 0
        comm_size = this%get_comm_size()
        if(allocated(recv_data)) deallocate(recv_data); allocate(recv_data(comm_size))
#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
        if(this%parallel) then
            call MPI_ALLGATHER(send_data, 1_I4P, MPI_INTEGER, recv_data, 1_I4P, MPI_INTEGER, this%comm, mpierr)
        else
            recv_data(:comm_size) = send_data
        endif
#else
        recv_data(:comm_size) = send_data
#endif
        if(present(mpierror)) mpierror = mpierr
    end subroutine mpi_env_allgather_single_int_value_I4P

    subroutine mpi_env_allgather_single_int_value_I8P(this, send_data, recv_data, mpierror)
    !-----------------------------------------------------------------
    !< MPI_allgather interface for a single I8P value per task
    !-----------------------------------------------------------------
        class(mpi_env_t),          intent(IN)    :: this                !< MPI environment
        integer(I8P),              intent(IN)    :: send_data           !< MPI_allgather send data
        integer(I8P), allocatable, intent(INOUT) :: recv_data(:)        !< MPI_allgather receive data
        integer(I4P), optional,    intent(INOUT) :: mpierror            !< MPI error
        integer(I4P)                             :: mpierr              !< Aux variable for MPI error
        integer(I4P)                             :: comm_size           !< MPI communicator size aux variable
    !-----------------------------------------------------------------
        assert(this%State == MPI_ENV_STATE_INIT)
        if(present(mpierror)) mpierror = 0
        comm_size = this%get_comm_size()
        if(allocated(recv_data)) deallocate(recv_data); allocate(recv_data(comm_size))
#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
        if(this%parallel) then
            call MPI_ALLGATHER(send_data, 1_I4P, MPI_LONG, recv_data, 1_I4P, MPI_LONG, this%comm, mpierr)
        else
            recv_data(:comm_size) = send_data
        endif
#else
        recv_data(:comm_size) = send_data
#endif
        if(present(mpierror)) mpierror = mpierr
    end subroutine mpi_env_allgather_single_int_value_I8P


    subroutine mpi_env_broadcast_int_I4P(this, send_data, mpierror)
    !-----------------------------------------------------------------
    !< MPI_allgather interface for a single I4P value per task
    !-----------------------------------------------------------------
        class(mpi_env_t),          intent(IN)    :: this                !< MPI environment
        integer(I4P),              intent(INOUT) :: send_data           !< MPI_broadcast send data
        integer(I4P), optional,    intent(INOUT) :: mpierror            !< MPI error
        integer(I4P)                             :: mpierr              !< Aux variable for MPI error
    !-----------------------------------------------------------------
        assert(this%State == MPI_ENV_STATE_INIT)
        if(present(mpierror)) mpierror = 0
#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
        if(this%parallel) call MPI_BCAST (send_data, 1_I4P, MPI_INTEGER, this%root, this%comm, mpierr)
#endif
        if(present(mpierror)) mpierror = mpierr
    end subroutine mpi_env_broadcast_int_I4P


    subroutine mpi_env_broadcast_int_I8P(this, send_data, mpierror)
    !-----------------------------------------------------------------
    !< MPI_allgather interface for a single I4P value per task
    !-----------------------------------------------------------------
        class(mpi_env_t),          intent(IN)    :: this                !< MPI environment
        integer(I8P),              intent(INOUT) :: send_data           !< MPI_broadcast send data
        integer(I4P), optional,    intent(INOUT) :: mpierror            !< MPI error
        integer(I4P)                             :: mpierr              !< Aux variable for MPI error
    !-----------------------------------------------------------------
        assert(this%State == MPI_ENV_STATE_INIT)
        if(present(mpierror)) mpierror = 0
#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
        if(this%parallel) call MPI_BCAST (send_data, 1_I4P, MPI_LONG_LONG_INT, this%root, this%comm, mpierr)
#endif
        if(present(mpierror)) mpierror = mpierr
    end subroutine mpi_env_broadcast_int_I8P


    subroutine mpi_env_broadcast_int_I4P_array(this, send_data, mpierror)
    !-----------------------------------------------------------------
    !< MPI_allgather interface for a single I4P value per task
    !-----------------------------------------------------------------
        class(mpi_env_t),          intent(IN)    :: this                !< MPI environment
        integer(I4P), allocatable, intent(INOUT) :: send_data(:)        !< MPI_broadcast send data
        integer(I4P), optional,    intent(INOUT) :: mpierror            !< MPI error
        integer(I4P)                             :: data_size           !< Send data size
        integer(I4P)                             :: mpierr              !< Aux variable for MPI error
    !-----------------------------------------------------------------
        assert(this%State == MPI_ENV_STATE_INIT)
        if(present(mpierror)) mpierror = 0
#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
        if(this%is_root()) data_size = size(send_data,dim=1)
        if(this%parallel) call MPI_BCAST (data_size, 1_I4P, MPI_INTEGER, this%root, this%comm, mpierr)
        if(.not. this%is_root()) then
            if(allocated(send_data)) deallocate(send_data)
            allocate(send_data(data_size))
        endif
        if(this%parallel) call MPI_BCAST (send_data, data_size, MPI_INTEGER, this%root, this%comm, mpierr)
#endif
        if(present(mpierror)) mpierror = mpierr
    end subroutine mpi_env_broadcast_int_I4P_array


    subroutine mpi_env_broadcast_int_I8P_array(this, send_data, mpierror)
    !-----------------------------------------------------------------
    !< MPI_allgather interface for a single I4P value per task
    !-----------------------------------------------------------------
        class(mpi_env_t),          intent(IN)    :: this                !< MPI environment
        integer(I8P), allocatable, intent(INOUT) :: send_data(:)        !< MPI_broadcast send data
        integer(I4P), optional,    intent(INOUT) :: mpierror            !< MPI error
        integer(I4P)                             :: data_size           !< Send data size
        integer(I4P)                             :: mpierr              !< Aux variable for MPI error
    !-----------------------------------------------------------------
        assert(this%State == MPI_ENV_STATE_INIT)
        if(present(mpierror)) mpierror = 0
#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
        if(this%is_root()) data_size = size(send_data,dim=1)
        if(this%parallel) call MPI_BCAST (data_size, 1_I4P, MPI_INTEGER, this%root, this%comm, mpierr)
        if(.not. this%is_root()) then
            if(allocated(send_data)) deallocate(send_data)
            allocate(send_data(data_size))
        endif
        if(this%parallel) call MPI_BCAST (send_data, data_size, MPI_LONG_LONG_INT, this%root, this%comm, mpierr)
#endif
        if(present(mpierror)) mpierror = mpierr
    end subroutine mpi_env_broadcast_int_I8P_array


    subroutine mpi_env_broadcast_string(this, send_data, mpierror)
    !-----------------------------------------------------------------
    !< MPI_allgather interface for a deferred length character array
    !-----------------------------------------------------------------
        class(mpi_env_t),              intent(IN)    :: this                !< MPI environment
        character(len=:), allocatable, intent(INOUT) :: send_data           !< MPI_broadcast send data
        integer(I4P),     optional,    intent(INOUT) :: mpierror            !< MPI error
        integer(I4P)                                 :: data_size           !< Send data size
        integer(I4P)                                 :: mpierr              !< Aux variable for MPI error
    !-----------------------------------------------------------------
        assert(this%State == MPI_ENV_STATE_INIT)
        if(present(mpierror)) mpierror = 0
#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
        if(this%is_root()) data_size = len(send_data)
        if(this%parallel) call MPI_BCAST (data_size, 1_I4P, MPI_INTEGER, this%root, this%comm, mpierr)
        if(.not. this%is_root()) then
            if(allocated(send_data)) deallocate(send_data)
            allocate(character(len=data_size) :: send_data)
        endif
        if(this%parallel) call MPI_BCAST (send_data, data_size, MPI_CHAR, this%root, this%comm, mpierr)
#endif
        if(present(mpierror)) mpierror = mpierr
    end subroutine mpi_env_broadcast_string


    function mpi_env_wtime(this) result(time)
    !-----------------------------------------------------------------
    !< Call MPI_Wtime()
    !-----------------------------------------------------------------
        class(mpi_env_t), intent(IN)  :: this                         !< MPI environment
        real(R8P)                     :: time                         !< MPI_WTime returned time
    !-----------------------------------------------------------------
        assert(this%State == MPI_ENV_STATE_INIT)
#if defined(ENABLE_MPI) && (defined(MPI_MOD) || defined(MPI_H))
        if(this%parallel) then
            time = MPI_WTIME()
        else
            time = Wtime()
        endif
#else
        time = Wtime()
#endif
    end function mpi_env_wtime


    function mpi_env_is_root(this) result(is_root)
    !-----------------------------------------------------------------
    !< Is the current task the root processor?
    !-----------------------------------------------------------------
        class(mpi_env_t), intent(IN)  :: this                         !< MPI environment
        logical                       :: is_root                      !< Boolean variable, True if is root task
    !-----------------------------------------------------------------
        assert(this%State == MPI_ENV_STATE_INIT)
        is_root = this%get_rank() == this%get_root()
    end function mpi_env_is_root


    function mpi_env_is_parallel(this) result(is_parallel)
    !-----------------------------------------------------------------
    !< Is the current task the root processor?
    !-----------------------------------------------------------------
        class(mpi_env_t), intent(IN)  :: this                         !< MPI environment
        logical                       :: is_parallel                  !< Boolean variable, True if is root task
    !-----------------------------------------------------------------
        assert(this%State == MPI_ENV_STATE_INIT)
        is_parallel = this%parallel
    end function mpi_env_is_parallel

end module mpi_environment
