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

BOOL stringp (id obj);
BOOL literal_string_p (id obj);
BOOL archiver_list_p (id obj);
BOOL symbolp (id obj);
BOOL quotedp (id obj);

BOOL keywordp (id obj);
BOOL valuep (id obj);
BOOL nil_value_p (id obj);
BOOL arrayp (id obj);
BOOL pairp (id obj);
BOOL cons_literal_p (id obj);
BOOL list_literal_p (id obj);
BOOL quote_literal_p (id obj);
