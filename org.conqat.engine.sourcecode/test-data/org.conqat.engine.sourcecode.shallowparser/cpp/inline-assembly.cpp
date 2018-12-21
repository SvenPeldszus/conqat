// this file collects several variants of inline assembly we encountered

#if defined(__GNUC__)
#	define OSEKOSReadTB() ({ttLocalTickType tb;__asm volatile("\tmftb\t%0":"=r"(tb):);tb;})
#elif defined(__VBCC__)
//	__regsused("") __writesmem("") __readsmem("") int OSEKOSReadTB(void)="\tmftb\t3";
#elif defined(__DCC__)
__asm int OSEKOSReadTB(void)
{
!
\tmftb\t3
}
#else
#	error "Unsupported compiler!"
#endif


__asm void *OSEKOSReadSP(void)
{
.set noreorder
! "r3"
	mr	3,1
.set reorder
}
__asm void OSEKOSSetSP(void *sp)
{
.set noreorder
!
%reg sp
	mr	1,sp
.set reorder
}
__asm void OSEKOSSetDEC(unsigned int x)
{
.set noreorder
!
%reg x
	mtspr	22,x
.set reorder
}

