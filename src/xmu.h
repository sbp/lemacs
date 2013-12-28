/* Compatibility routines based on code from the MIT Xmu library */

#ifndef HAVE_XMU
int XmuCursorNameToIndex (const char *name);
int XmuReadBitmapDataFromFile (const char *filename, unsigned int *width,
                               unsigned int *height, unsigned char **datap,
                               int *x_hot, int *y_hot);
int XmuPrintDefaultErrorMessage (Display *dpy, XErrorEvent *event, FILE *fp);
#endif
