// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objc/objc.h>
#import <objc/objc-api.h>
#import <objc/encoding.h>

inline const char* skip_type_qualifiers (const char* type) ;
const char* skip_typespec (const char* type) ;
inline const char* skip_offset (const char* type) ;
const char* skip_argspec (const char* type) ;
int get_number_of_arguments (const char* type) ;

