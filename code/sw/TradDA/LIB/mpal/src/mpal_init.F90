
! Initialization before mpal is used
subroutine mpal_init_bits(sbits, half_option)

    integer, intent(in), optional :: sbits
    logical, intent(in), optional :: half_option

#ifdef RPE_ON
    if (present(sbits)) then
        RPE_DEFAULT_SBITS = sbits
        if (present(half_option)) RPE_IEEE_HALF = half_option
    end if
#endif

#ifdef CADNA_ON
    call cadna_init(-1)
#endif

end subroutine mpal_init_bits

subroutine mpal_finalize()

#ifdef CADNA_ON
    call cadna_end()
#endif

end subroutine mpal_finalize