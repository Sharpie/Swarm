#import <defobj/FArguments.h>
#import <defobj/FCall.h>
#import <objc/objc.h>

extern void objc_setup_call (FArguments_c *fa, id obj, SEL sel);
extern void objc_add_primitive (FArguments_c *fa, fcall_type_t type, void *val);
extern void objc_set_return_type (FCall_c *fc, FArguments_c *fa);
extern void objc_call (FArguments_c *fa);
