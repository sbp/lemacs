
/*
** Generated by X-Designer 
*/
/*
**LIBS: -lXm -lXt -lX11
*/

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>


Widget button1;


create_widget0( parent )
Widget parent;
{
	Display *display = XtDisplay ( parent );
	Widget children[9];      /* Children to manage */
	Arg al[64];           /* Arg List */
	register int ac = 0;      /* Arg Count */
	char from_s [256];    /* For font list conversion */
	XrmValue from_value, to_value; /* ditto */
	int fg, bg;           /* colour values for pixmaps */ 
	XmString *list_items; /* For list items */
	int list_item;        /* Index for list_items */
	XmString xmstrings[15];    /* temporary storage for XmStrings */
	Widget widget0;
	Widget widget1;
	Widget widget2;
	Widget widget3;
	Widget widget4;
	Widget widget5;
	Widget widget6;
	Widget widget7;
	Widget widget8;
	Widget widget9;
	Widget widget11;
	Widget widget12;
	Widget widget13;
	Widget widget14;
	Widget widget15;
	Widget widget16;
	Widget widget17;
	Widget widget18;
	Widget widget19;
	Widget widget20;
	Widget widget21;
	Widget widget22;
	Widget widget23;
	Widget widget24;

	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	widget0 = XmCreateDialogShell ( parent, "dialog", al, ac );
	ac = 0;
	XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
	widget1 = XmCreateForm ( widget0, "searchDialog", al, ac );
	ac = 0;
	XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;
	widget2 = XmCreateRowColumn ( widget1, "widget2", al, ac );
	ac = 0;
	widget3 = XmCreateFrame ( widget2, "widget3", al, ac );
	XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;
	XtSetArg(al[ac], XmNpacking, XmPACK_TIGHT); ac++;
	widget4 = XmCreateRadioBox ( widget3, "directionBox", al, ac );
	ac = 0;
	widget5 = XmCreateToggleButtonGadget ( widget4, "directionForward", al, ac );
	widget6 = XmCreateToggleButtonGadget ( widget4, "directionBackward", al, ac );
	widget7 = XmCreateToggleButton ( widget2, "regexpSearch", al, ac );
	widget8 = XmCreateToggleButton ( widget2, "caseSearch", al, ac );
	widget9 = XmCreateRowColumn ( widget1, "widget9", al, ac );
	button1 = XmCreatePushButton ( widget9, "button1", al, ac );
	widget11 = XmCreatePushButton ( widget9, "button2", al, ac );
	widget12 = XmCreatePushButton ( widget9, "button3", al, ac );
	widget13 = XmCreatePushButton ( widget9, "button4", al, ac );
	widget14 = XmCreateLabel ( widget1, "iconSeparator", al, ac );
	widget15 = XmCreateLabel ( widget1, "iconSeparator", al, ac );
	widget16 = XmCreateRowColumn ( widget1, "widget16", al, ac );
	widget17 = XmCreatePushButton ( widget16, "gotoStart", al, ac );
	widget18 = XmCreatePushButton ( widget16, "scrollBack", al, ac );
	widget19 = XmCreatePushButton ( widget16, "scrollForward", al, ac );
	widget20 = XmCreatePushButton ( widget16, "gotoEnd", al, ac );
	widget21 = XmCreateLabel ( widget1, "searchLabel", al, ac );
	widget22 = XmCreateLabel ( widget1, "replaceLabel", al, ac );
	widget23 = XmCreateTextField ( widget1, "searchText", al, ac );
	widget24 = XmCreateTextField ( widget1, "replaceText", al, ac );
	XtSetArg(al[ac], XmNdefaultButton, button1); ac++;
	XtSetValues ( widget1,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 4); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget24); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNbottomOffset, 4); ac++;
	XtSetArg(al[ac], XmNbottomWidget, widget15); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 4); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNrightOffset, 4); ac++;
	XtSetArg(al[ac], XmNrightWidget, widget9); ac++;
        XtSetValues ( widget2,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 4); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomOffset, 4); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNrightOffset, 4); ac++;
	XtSetArg(al[ac], XmNrightWidget, widget16); ac++;
        XtSetValues ( widget9,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 67); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 7); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget14,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomOffset, 4); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 4); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNrightOffset, 4); ac++;
	XtSetArg(al[ac], XmNrightWidget, widget9); ac++;
        XtSetValues ( widget15,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 4); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomOffset, 4); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNrightOffset, 4); ac++;
        XtSetValues ( widget16,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 5); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNrightOffset, 0); ac++;
	XtSetArg(al[ac], XmNrightWidget, widget22); ac++;
        XtSetValues ( widget21,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 41); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 4); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget22,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 4); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 4); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget22); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNrightOffset, 4); ac++;
	XtSetArg(al[ac], XmNrightWidget, widget9); ac++;
        XtSetValues ( widget23,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 4); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget23); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 4); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget22); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNrightOffset, 4); ac++;
	XtSetArg(al[ac], XmNrightWidget, widget9); ac++;
        XtSetValues ( widget24,al, ac );
	ac = 0;
	children[ac++] = widget5;
	children[ac++] = widget6;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = widget4;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = widget3;
	children[ac++] = widget7;
	children[ac++] = widget8;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = button1;
	children[ac++] = widget11;
	children[ac++] = widget12;
	children[ac++] = widget13;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = widget17;
	children[ac++] = widget18;
	children[ac++] = widget19;
	children[ac++] = widget20;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = widget2;
	children[ac++] = widget9;
	children[ac++] = widget14;
	children[ac++] = widget15;
	children[ac++] = widget16;
	children[ac++] = widget21;
	children[ac++] = widget22;
	children[ac++] = widget23;
	children[ac++] = widget24;
	XtManageChildren(children, ac);
	ac = 0;
}

