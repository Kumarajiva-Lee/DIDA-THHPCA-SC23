
#define CADNA 0
#define RPE 1
#define MONITOR 0
#define ERROR_MAX 1e-2

#if (CADNA == 1)
#define CADNA_ON
#endif

#if (RPE == 1)
#define RPE_ON
#endif

#if (MONITOR == 1)
#define MONITOR_ON
#endif

#define GET_MPAL_VAL(x) (x%rpe_st%val)