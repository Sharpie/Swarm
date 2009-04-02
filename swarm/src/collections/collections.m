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
#if SWARM_OBJC_DONE
  setBit (((Class) id_List_linked)->info, _CLS_DEFINEDCLASS, 0);
  setBit (((Class) id_List_mlinks)->info, _CLS_DEFINEDCLASS, 0);
#else
  swarm_class_setDefinedClassBit((ObjcClass)id_List_linked, 0);
  swarm_class_setDefinedClassBit((ObjcClass)id_List_mlinks, 0);
#endif
  
  [id_ListIndex_linked setTypeImplemented: ListIndex];
  [id_ListIndex_mlinks setTypeImplemented: ListIndex];
  [id_Map_c setTypeImplemented: Map];
  [id_MapIndex_c setTypeImplemented: MapIndex];
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
  [id_ArchiverQuoted_c setTypeImplemented: ArchiverQuoted];
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




