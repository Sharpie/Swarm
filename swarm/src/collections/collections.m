// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         collections.m
Description:  function to initialize activity group
Library:      collections
*/

#include "collections.xm"
#import <collections/Collection.h>

//
// _collections_implement() -- generate implementations for defobj module
//
void
_collections_implement (void)
{
  [id_Array_c setTypeImplemented: Array];
  [id_List_any setTypeImplemented: List];
  [id_List_linked setTypeImplemented: List];
  [id_List_mlinks setTypeImplemented: List];
  [id_Map_c setTypeImplemented: Map];
  [id_Set_c setTypeImplemented: Set];
  [id_OrderedSet_c setTypeImplemented: OrderedSet];
  [id_String_c setTypeImplemented: String];
  [id_InputStream_c setTypeImplemented: InputStream];
  [id_OutputStream_c setTypeImplemented: OutputStream];
  [id_ArchiverKeyword_c setTypeImplemented: ArchiverKeyword];
  [id_ArchiverValue_c setTypeImplemented: ArchiverValue];
  [id_ArchiverArray_c setTypeImplemented: ArchiverArray];
  [id_ArchiverPair_c setTypeImplemented: ArchiverPair];
  [id_ArchiverList_c setTypeImplemented: ArchiverList];
  [id_Permutation_c setTypeImplemented: Permutation];
  [id_PermutationItem_c setTypeImplemented: PermutationItem];
  [id_PermutedIndex_c setTypeImplemented: PermutedIndex];
  [id_ListShuffler_c setTypeImplemented: ListShuffler];
}

//
// _collections_initialize() -- initialize global data for collections module
//
void
_collections_initialize (void)
{
  defsymbol (ArchiverLiteral);
  defsymbol (ArchiverQuote);
  defsymbol (ArchiverEOL);
  defsymbol (ArchiverDot);
}




