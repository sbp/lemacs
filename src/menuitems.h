/****************************************************************************
 ***
 ***        (c) Copyright 1990 by Sun Microsystems Inc.,  All Rights Reserved.
 ***        Derived from Lucid's Energize connection/editoption.h
 ***
*****************************************************************************/

#ifndef MENUITEMS
#define MENUITEMS

#include <editorreq.h>

/*
 * The MenuItem structure describes an editor command menu item.
 */

typedef struct {
  char*		name;		/* command name */
  BITS32	id1;		/* first Energize id of the command */
  BITS32	id2;		/* second Energize id of the command */
  BITS16	flags;		/* flags as defined in editorreq.h */
} MenuItem;

/*
 * A MenuList is a dynamically variable size array of MenuItems.
 */

typedef struct {
  int		size;		/* allocated size */
  int		length;		/* current filled length */
  MenuItem*	items;		/* the options themselves */
} MenuList;

MenuList*
AllocateMenuList (int initialSize);

void
FreeMenuList (MenuList* list);

void
ClearMenuList (MenuList* list);

void
InsertMenuItem (MenuList* list,
		char* cmdName, BITS32 cmdId1, BITS32 cmdId2, BITS16 flags);
void
AppendMenuList (MenuList* head, MenuList* tail);

#endif /* MENUITEMS */
