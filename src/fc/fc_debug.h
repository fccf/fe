#ifndef __FILE__
#error __FILE__ does not work!
#endif

#ifndef __LINE__
#error __LINE__ does not work!
#endif

#ifndef __DATE__
#error __DATE__ does not work!
#endif

#ifndef __TIME__
#error __TIME__ does not work!
#endif

#define info(level,format) if(level<=debug_level_) write(debug_unit(level), format)

#define error(X) call debug_error(X,__FILE__,__LINE__)

#define assert(X) if(.not.(X)) error('Faild assertation ('//'X'//')')
