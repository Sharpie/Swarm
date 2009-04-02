// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <Swarm/objectbase.h>
#import <Swarm/SwarmObject.h>

// Options for the format of the string returned when probing an unsigned
// char or a char (there is a choice between "%d %c", "%c" or "%d"...)

@interface Probe: SwarmObject <Probe>
{
  Class probedClass;
  id probedObject;
  const char *probedType;
  BOOL safety;
  id <Symbol> language;
  id <Symbol> stringReturnType;
  id objectToNotify;  // could be an object or a list
}

+ createBegin: aZone;
- createEnd;

- setObjectToNotify: anObject;
- getObjectToNotify;

- setProbedClass: (Class)aClass;
- setProbedObject: object;
- createEnd;

- clone: aZone;

- (Class)getProbedClass;
- (const char *)getProbedType;

- (val_t)guessValue: (const char *)str;

- setSafety;
- unsetSafety;
@end

#define CONVERT(ftype, type, p)                           \
  switch (ftype)                                          \
    {                                                     \
    case fcall_type_void:                                 \
    case fcall_type_object:                               \
    case fcall_type_class:                                \
    case fcall_type_string:                               \
    case fcall_type_selector:                             \
    case fcall_type_jobject:                              \
    case fcall_type_jstring:                              \
    case fcall_type_jselector:                            \
    case fcall_type_iid:                                  \
      abort ();                                           \
    case fcall_type_boolean:                              \
      ret = (type) (p)->boolean;                          \
      break;                                              \
    case fcall_type_uchar:                                \
      ret = (type) (p)->uchar;                            \
      break;                                              \
    case fcall_type_schar:                                \
      ret = (type) (p)->schar;                            \
      break;                                              \
    case fcall_type_ushort:                               \
      ret = (type) (p)->ushort;                           \
      break;                                              \
    case fcall_type_sshort:                               \
      ret = (type) (p)->sshort;                           \
      break;                                              \
    case fcall_type_uint:                                 \
      ret = (type) (p)->uint;                             \
      break;                                              \
    case fcall_type_sint:                                 \
      ret = (type) (p)->sint;                             \
      break;                                              \
    case fcall_type_ulong:                                \
      ret = (type) (p)->ulong;                            \
      break;                                              \
    case fcall_type_slong:                                \
      ret = (type) (p)->slong;                            \
      break;                                              \
    case fcall_type_ulonglong:                            \
      ret = (type) (p)->ulonglong;                        \
      break;                                              \
    case fcall_type_slonglong:                            \
      ret = (type) (p)->slonglong;                        \
      break;                                              \
    case fcall_type_float:                                \
      ret = (type) (p)->_float;                           \
      break;                                              \
    case fcall_type_double:                               \
      ret = (type) (p)->_double;                          \
      break;                                              \
    case fcall_type_long_double:                          \
      ret = (type) (p)->_long_double;                     \
      break;                                              \
    }

