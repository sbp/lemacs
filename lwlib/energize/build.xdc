
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
#include <Xm/Separator.h>
#include <Xm/ToggleBG.h>


Widget button1;


create_widget0( parent )
Widget parent;
{
	Display *display = XtDisplay ( parent );
	Widget children[15];      /* Children to manage */
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
	Widget widget4;
	Widget widget5;
	Widget widget6;
	Widget widget7;
	Widget widget8;
	Widget widget9;
	Widget widget10;
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
	Widget widget25;
	Widget widget26;
	Widget widget27;
	Widget widget28;
	Widget widget29;
	Widget widget30;
	Widget widget31;
	Widget widget32;
	Widget widget33;
	Widget widget34;
	Widget widget35;
	Widget widget36;
	Widget widget37;
	Widget widget38;
	Widget widget39;

	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	widget0 = XmCreateDialogShell ( parent, "dialog", al, ac );
	ac = 0;
	XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
	widget1 = XmCreateForm ( widget0, "buildDialog", al, ac );
	ac = 0;
	XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;
	XtSetArg(al[ac], XmNpacking, XmPACK_COLUMN); ac++;
	XtSetArg(al[ac], XmNentryAlignment, XmALIGNMENT_CENTER); ac++;
	XtSetArg(al[ac], XmNadjustLast, FALSE); ac++;
	XtSetArg(al[ac], XmNisAligned, TRUE); ac++;
	widget2 = XmCreateRowColumn ( widget1, "row", al, ac );
	ac = 0;
	button1 = XmCreatePushButton ( widget2, "button1", al, ac );
	widget4 = XmCreatePushButton ( widget2, "button2", al, ac );
	widget5 = XmCreatePushButton ( widget2, "button3", al, ac );
	widget6 = XmCreatePushButton ( widget2, "button4", al, ac );
	XtSetArg(al[ac], XmNmappedWhenManaged, FALSE); ac++;
	widget7 = XmCreatePushButton ( widget2, "separator_button", al, ac );
	ac = 0;
	widget8 = XmCreatePushButton ( widget2, "button5", al, ac );
	widget9 = XmCreateSeparator ( widget1, "widget9", al, ac );
	XtSetArg(al[ac], XmNlabelType, XmPIXMAP); ac++;
	widget10 = XmCreateLabel ( widget1, "dbox-question", al, ac );
	ac = 0;
	widget11 = XmCreateLabel ( widget1, "iconSeparator", al, ac );
	widget12 = XmCreateLabel ( widget1, "errorLabel", al, ac );
	widget13 = XmCreateFrame ( widget1, "widget13", al, ac );
	XtSetArg(al[ac], XmNspacing, 1); ac++;
	XtSetArg(al[ac], XmNmarginWidth, 1); ac++;
	XtSetArg(al[ac], XmNmarginHeight, 1); ac++;
	XtSetArg(al[ac], XmNorientation, XmVERTICAL); ac++;
	widget14 = XmCreateRadioBox ( widget13, "errorBox", al, ac );
	ac = 0;
	widget15 = XmCreateToggleButtonGadget ( widget14, "errorContinue", al, ac );
	widget16 = XmCreateToggleButtonGadget ( widget14, "errorStop", al, ac );
	widget17 = XmCreateLabel ( widget1, "compileLabel", al, ac );
	widget18 = XmCreateFrame ( widget1, "widget18", al, ac );
	XtSetArg(al[ac], XmNspacing, 1); ac++;
	XtSetArg(al[ac], XmNmarginWidth, 1); ac++;
	XtSetArg(al[ac], XmNmarginHeight, 1); ac++;
	XtSetArg(al[ac], XmNorientation, XmVERTICAL); ac++;
	widget19 = XmCreateRadioBox ( widget18, "compileBox", al, ac );
	ac = 0;
	widget20 = XmCreateToggleButtonGadget ( widget19, "compileFull", al, ac );
	widget21 = XmCreateToggleButtonGadget ( widget19, "compileIncremental", al, ac );
	widget22 = XmCreateLabel ( widget1, "linkLabel", al, ac );
	widget23 = XmCreateFrame ( widget1, "widget23", al, ac );
	XtSetArg(al[ac], XmNnumColumns, 2); ac++;
	XtSetArg(al[ac], XmNspacing, 1); ac++;
	XtSetArg(al[ac], XmNmarginWidth, 1); ac++;
	XtSetArg(al[ac], XmNmarginHeight, 1); ac++;
	XtSetArg(al[ac], XmNorientation, XmVERTICAL); ac++;
	widget24 = XmCreateRadioBox ( widget23, "linkBox", al, ac );
	ac = 0;
	widget25 = XmCreateToggleButtonGadget ( widget24, "linkFull", al, ac );
	widget26 = XmCreateToggleButtonGadget ( widget24, "linkIncremental", al, ac );
	widget27 = XmCreateToggleButtonGadget ( widget24, "linkNo", al, ac );
	widget28 = XmCreateLabel ( widget1, "debugLabel", al, ac );
	widget29 = XmCreateFrame ( widget1, "widget29", al, ac );
	XtSetArg(al[ac], XmNnumColumns, 2); ac++;
	XtSetArg(al[ac], XmNspacing, 1); ac++;
	XtSetArg(al[ac], XmNmarginWidth, 1); ac++;
	XtSetArg(al[ac], XmNmarginHeight, 1); ac++;
	XtSetArg(al[ac], XmNorientation, XmVERTICAL); ac++;
	widget30 = XmCreateRadioBox ( widget29, "debugBox", al, ac );
	ac = 0;
	widget31 = XmCreateToggleButtonGadget ( widget30, "debugRun", al, ac );
	widget32 = XmCreateToggleButtonGadget ( widget30, "debugStart", al, ac );
	widget33 = XmCreateToggleButtonGadget ( widget30, "debugNo", al, ac );
	widget34 = XmCreateLabel ( widget1, "subTargetLabel", al, ac );
	widget35 = XmCreateFrame ( widget1, "widget35", al, ac );
	XtSetArg(al[ac], XmNspacing, 1); ac++;
	XtSetArg(al[ac], XmNmarginWidth, 1); ac++;
	XtSetArg(al[ac], XmNmarginHeight, 1); ac++;
	XtSetArg(al[ac], XmNpacking, XmPACK_TIGHT); ac++;
	widget36 = XmCreateRadioBox ( widget35, "subTargetBox", al, ac );
	ac = 0;
	widget37 = XmCreateToggleButtonGadget ( widget36, "subTargetAll", al, ac );
	widget38 = XmCreateToggleButtonGadget ( widget36, "subTargetNone", al, ac );
	XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING); ac++;
	widget39 = XmCreateLabel ( widget1, "message", al, ac );
	ac = 0;
	XtSetArg(al[ac], XmNdefaultButton, button1); ac++;
	XtSetValues ( widget1,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomOffset, 4); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 4); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNrightOffset, 4); ac++;
        XtSetValues ( widget2,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNbottomOffset, 4); ac++;
	XtSetArg(al[ac], XmNbottomWidget, widget2); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 0); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNrightOffset, 0); ac++;
        XtSetValues ( widget9,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 4); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 4); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget10,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 6); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget10); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNbottomOffset, 4); ac++;
	XtSetArg(al[ac], XmNbottomWidget, widget9); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 4); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget11,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNbottomOffset, -4); ac++;
	XtSetArg(al[ac], XmNbottomWidget, widget13); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 6); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget13); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget12,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 0); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget35); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 4); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget35); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget13,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNbottomOffset, -4); ac++;
	XtSetArg(al[ac], XmNbottomWidget, widget18); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 6); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget18); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget17,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNbottomOffset, 4); ac++;
	XtSetArg(al[ac], XmNbottomWidget, widget34); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 4); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget10); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget18,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNbottomOffset, -4); ac++;
	XtSetArg(al[ac], XmNbottomWidget, widget23); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 6); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget23); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget22,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 0); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget18); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 4); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget18); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget23,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNbottomOffset, -4); ac++;
	XtSetArg(al[ac], XmNbottomWidget, widget29); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 6); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget29); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget28,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 0); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget13); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 4); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget13); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget29,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNbottomOffset, -4); ac++;
	XtSetArg(al[ac], XmNbottomWidget, widget35); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 6); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget35); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget34,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNbottomOffset, 4); ac++;
	XtSetArg(al[ac], XmNbottomWidget, widget9); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 4); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget10); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget35,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 4); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNbottomOffset, 4); ac++;
	XtSetArg(al[ac], XmNbottomWidget, widget17); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 4); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget10); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNrightOffset, 4); ac++;
        XtSetValues ( widget39,al, ac );
	ac = 0;
	children[ac++] = button1;
	children[ac++] = widget4;
	children[ac++] = widget5;
	children[ac++] = widget6;
	children[ac++] = widget7;
	children[ac++] = widget8;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = widget15;
	children[ac++] = widget16;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = widget14;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = widget20;
	children[ac++] = widget21;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = widget19;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = widget25;
	children[ac++] = widget26;
	children[ac++] = widget27;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = widget24;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = widget31;
	children[ac++] = widget32;
	children[ac++] = widget33;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = widget30;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = widget37;
	children[ac++] = widget38;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = widget36;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = widget2;
	children[ac++] = widget9;
	children[ac++] = widget10;
	children[ac++] = widget11;
	children[ac++] = widget12;
	children[ac++] = widget13;
	children[ac++] = widget17;
	children[ac++] = widget18;
	children[ac++] = widget22;
	children[ac++] = widget23;
	children[ac++] = widget28;
	children[ac++] = widget29;
	children[ac++] = widget34;
	children[ac++] = widget35;
	children[ac++] = widget39;
	XtManageChildren(children, ac);
	ac = 0;
}


