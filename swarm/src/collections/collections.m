// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         collections.m
Description:  function to initialize activity group
Library:      collections
*/

#import <objc/objc.h>
id LiteralString;

#define LiteralString LiteralString_

#include "collections.xm"
#import <collections/Collection.h>

#undef LiteralString

//
// _collections_implement() -- generate implementations for defobj module
//
void _collections_implement( void )
{
  [id_Array_c        setTypeImplemented: Array       ];
  [id_List_any       setTypeImplemented: List        ];
  [id_List_linked    setTypeImplemented: List        ];
  [id_List_mlinks    setTypeImplemented: List        ];
  [id_Map_c          setTypeImplemented: Map         ];
  [id_Set_c          setTypeImplemented: Set         ];
  [id_OrderedSet_c   setTypeImplemented: OrderedSet  ];
  [id_String_c       setTypeImplemented: String      ];
  [id_InputStream_c  setTypeImplemented: InputStream ];
  [id_OutputStream_c setTypeImplemented: OutputStream];
}

//
// _collections_initialize() -- initialize global data for collections module
//
void _collections_initialize (void)
{
  LiteralString = [String customizeBegin: globalZone];
  [LiteralString setLiteralFlag: YES];
  LiteralString = [LiteralString customizeEnd];

  defsymbol (ArchiverLiteral);
  defsymbol (ArchiverQuote);
  defsymbol (ArchiverEOL);
  defsymbol (ArchiverDot);
}




