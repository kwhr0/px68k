#define NEWMPU
//#define CURMPU
//#define TESTLOG

#if defined(TESTLOG) || defined(CURMPU) && defined(NEWMPU)
#define START_IO		0
#else
#define START_IO		0xc00000
#endif
