
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
#include <Xm/SeparatoG.h>
#include <Xm/ToggleBG.h>


Widget apply;


create_widget0( parent )
Widget parent;
{
	Display *display = XtDisplay ( parent );
	Widget children[20];      /* Children to manage */
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

	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	widget0 = XmCreateDialogShell ( parent, "widget0", al, ac );
	ac = 0;
	XtSetArg(al[ac], XmNshadowType, XmSHADOW_OUT); ac++;
	widget1 = XmCreateFrame ( widget0, "classPanel", al, ac );
	ac = 0;
	XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
	widget2 = XmCreateForm ( widget1, "panel", al, ac );
	ac = 0;
	widget3 = XmCreateLabel ( widget2, "nameLabel", al, ac );
	widget4 = XmCreateTextField ( widget2, "nameText", al, ac );
	widget5 = XmCreateLabel ( widget2, "fileLabel", al, ac );
	widget6 = XmCreateTextField ( widget2, "fileText", al, ac );
	widget7 = XmCreateLabel ( widget2, "projectLabel", al, ac );
	widget8 = XmCreateTextField ( widget2, "projectText", al, ac );
	apply = XmCreatePushButton ( widget2, "apply", al, ac );
	XtSetArg(al[ac], XmNseparatorType, XmSHADOW_ETCHED_IN); ac++;
	widget10 = XmCreateSeparatorGadget ( widget2, "separator", al, ac );
	ac = 0;
	widget11 = XmCreateToggleButton ( widget2, "dataMemberToggle", al, ac );
	widget12 = XmCreateTextField ( widget2, "dataMemberText", al, ac );
	widget13 = XmCreateToggleButton ( widget2, "memberFunctionToggle", al, ac );
	widget14 = XmCreateTextField ( widget2, "memberFunctionText", al, ac );
	widget15 = XmCreateToggleButton ( widget2, "friends", al, ac );
	widget16 = XmCreateToggleButton ( widget2, "constructors", al, ac );
	widget17 = XmCreateToggleButton ( widget2, "inherited", al, ac );
	widget18 = XmCreateToggleButton ( widget2, "whereDefined", al, ac );
	widget19 = XmCreateToggleButton ( widget2, "public", al, ac );
	widget20 = XmCreateToggleButton ( widget2, "protected", al, ac );
	widget21 = XmCreateToggleButton ( widget2, "private", al, ac );
	XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;
	widget22 = XmCreateRadioBox ( widget2, "nameStyle", al, ac );
	ac = 0;
	widget23 = XmCreateToggleButtonGadget ( widget22, "nameOnly", al, ac );
	widget24 = XmCreateToggleButtonGadget ( widget22, "prototype", al, ac );
	widget25 = XmCreateToggleButtonGadget ( widget22, "mangled", al, ac );
	XtSetArg(al[ac], XmNdefaultButton, apply); ac++;
	XtSetValues ( widget2,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 10); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget3,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 10); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget3); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNrightOffset, 10); ac++;
	XtSetArg(al[ac], XmNrightWidget, apply); ac++;
        XtSetValues ( widget4,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget4); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 0); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget3); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget5,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget4); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 0); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget4); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNrightOffset, 0); ac++;
	XtSetArg(al[ac], XmNrightWidget, widget4); ac++;
        XtSetValues ( widget6,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget6); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 0); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget3); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget7,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget6); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 0); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget4); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNrightOffset, 0); ac++;
	XtSetArg(al[ac], XmNrightWidget, widget4); ac++;
        XtSetValues ( widget8,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNrightOffset, 10); ac++;
        XtSetValues ( apply,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget8); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftOffset, 0); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNrightOffset, 0); ac++;
        XtSetValues ( widget10,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget14); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 0); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget3); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget11,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget14); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 0); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget14); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNrightOffset, 0); ac++;
	XtSetArg(al[ac], XmNrightWidget, widget4); ac++;
        XtSetValues ( widget12,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget10); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 0); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget3); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget13,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget10); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 10); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget13); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNrightOffset, 0); ac++;
	XtSetArg(al[ac], XmNrightWidget, widget4); ac++;
        XtSetValues ( widget14,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget17); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomOffset, 10); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 10); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget22); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget15,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget12); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 10); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget17); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget16,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget12); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 10); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget21); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget17,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget16); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomOffset, 10); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 10); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget15); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget18,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget12); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 0); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget3); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget19,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget12); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 10); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget19); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget20,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget12); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 10); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget20); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget21,al, ac );
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopWidget, widget19); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomOffset, 10); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftOffset, 0); ac++;
	XtSetArg(al[ac], XmNleftWidget, widget3); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
        XtSetValues ( widget22,al, ac );
	ac = 0;
	children[ac++] = widget23;
	children[ac++] = widget24;
	children[ac++] = widget25;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = widget3;
	children[ac++] = widget4;
	children[ac++] = widget5;
	children[ac++] = widget6;
	children[ac++] = widget7;
	children[ac++] = widget8;
	children[ac++] = apply;
	children[ac++] = widget10;
	children[ac++] = widget11;
	children[ac++] = widget12;
	children[ac++] = widget13;
	children[ac++] = widget14;
	children[ac++] = widget15;
	children[ac++] = widget16;
	children[ac++] = widget17;
	children[ac++] = widget18;
	children[ac++] = widget19;
	children[ac++] = widget20;
	children[ac++] = widget21;
	children[ac++] = widget22;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = widget2;
	XtManageChildren(children, ac);
	ac = 0;
}


