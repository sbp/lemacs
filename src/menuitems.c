/****************************************************************************
 ***
 ***        (c) Copyright 1990 by Sun Microsystems Inc.,  All Rights Reserved.
 ***        Derived from Lucid's Energize connection/editoption.c
 ***
*****************************************************************************/

#include "config.h"
#include "menuitems.h"

#include "connmemory.h"		/* cast-free malloc interface */
#include "editorside.h"		/* just for NIL */

MenuList*
AllocateMenuList (int initialSize) {
  /*
   * Allocate a new MenuList structure. Note that size may be 0.
   * The size will grow as needed when calling InsertMenuItem.
   */
  MenuList* list = new(MenuList, 1);
  MenuItem* options;
  if (list == NIL)
    return NIL;
  list->size = initialSize;
  list->length = 0;
  list->items = new(MenuItem, list->size);
  if (list->items == NIL) {	/* unable to allocate memory for items */
    free(list);
    return NIL;
  };
  return list;
};

void
FreeMenuList (MenuList* list) {
  /*
   * Free storage associated with menu list and free the list.
   * The individual command name strings are not freed.
   */
  free(list->items);
  free(list);
};

void
ClearMenuList (MenuList* list) {
  /*
   * reset fill point to 0.
   */
  list->length = 0;
};

void
InsertMenuItem (MenuList* list,
		char* cmdName, BITS32 cmdId1, BITS32 cmdId2, BITS16 flags) {
  /*
   * insert a new edit option.
   * will eventually reallocate the items field if preallocated storage is
   * exceeded. Reallocation is performed by doubling allocated size if
   * size is > 2, else by setting size to 2.
   */
  MenuItem* item;
  if (list->length == list->size) {
    if (list->size < 2)
      list->size = 2;
    else
      list->size *= 2;
    list->items = grow(MenuItem, list->items, list->size);
    if (list->items == NIL)
      return;
  };
  item = &(list->items[list->length]);
  item->name = cmdName;
  item->id1 = cmdId1;
  item->id2 = cmdId2;
  item->flags = flags;
  list->length += 1;
};

void
AppendMenuList (MenuList* head, MenuList* tail) {
  /*
   * append the items of tail at the end of head.
   * This allows to easily merge edit_items list.
   * Could be optimized to go faster...
   */
  int i;
  for (i = 0; i < tail->length; i++)
    InsertMenuItem(head,
		   tail->items[i].name,
		   tail->items[i].id1, tail->items[i].id2,
		   tail->items[i].flags);
};
