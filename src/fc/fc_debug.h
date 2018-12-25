#ifndef __FILE__
#error __FILE__ does not work!
#endif

#ifndef __LINE__
#error __LINE__ does not work!
#endif

#define debug_info(level,format) if(level<=debug_level_) write(debug_unit(level), format)

#define error(X) call debug_error(X,__FILE__,__LINE__)

#define assert(X) if(.not.(X)) error('Faild assertation ('//'X'//')')

#define assert_true(X) call debug_assert_true(X,__FILE__, __LINE__)

#define assert_equal(X,Y) call debug_assert_equal(X,Y,__FILE__, __LINE__)
