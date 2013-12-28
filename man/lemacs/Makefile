# Makefile for the   GNU Emacs Reference Manual.
#
# based on FSF Makefile, 11 August 1990
# Modified by Ben Wing, February 1994

# Redefine `TEX' if `tex' does not invoke plain TeX. For example:
# TEX=platex

TEX=tex

# Where the TeX macros are kept:
texmacrodir = /usr/local/lib/tex/macros

# Where the Emacs hierarchy lives ($EMACS in the INSTALL document for Emacs.)
# For example: 
# emacslibdir = /usr/local/gnu/lib/emacs

# Directory where Emacs is installed, by default:
emacslibdir = /usr/local/emacs

# Unless you have a nonstandard Emacs installation, these shouldn't have to 
# be changed.
prefix = /usr/local
infodir = ${prefix}/info

# The name of the manual:

NAME=lemacs

# List of all the texinfo files in the manual:

srcs = lemacs.texinfo abbrevs.texinfo basic.texinfo buffers.texinfo \
  building.texinfo calendar.texinfo cmdargs.texinfo custom.texinfo \
  display.texinfo entering.texinfo files.texinfo fixit.texinfo \
  glossary.texinfo gnu.texinfo help.texinfo indent.texinfo \
  keystrokes.texinfo killing.texinfo lemacs.texinfo m-x.texinfo \
  major.texinfo mark.texinfo menus.texinfo mini.texinfo misc.texinfo \
  mouse.texinfo new.texinfo picture.texinfo programs.texinfo \
  regs.texinfo rmail.texinfo screen.texinfo search.texinfo \
  sending.texinfo text.texinfo trouble.texinfo undo.texinfo \
  windows.texinfo

all: ../../info/$(NAME).info

$(NAME).dvi: $(srcs) ../texindex
	# Avoid losing old contents of aux file entirely.
	-mv $(NAME).aux $(NAME).oaux
	# First shot to define xrefs:
	$(TEX) $(NAME).texinfo
	../texindex $(NAME).??
	$(TEX) $(NAME).texinfo

../../info/$(NAME).info: $(srcs) ../makeinfo
	../makeinfo $(NAME).texinfo

clean:
	rm -f *.toc *.aux *.log *.cp *.cps *.fn *.fns *.tp *.tps \
              *.vr *.vrs *.pg *.pgs *.ky *.kys
	rm -f make.out core 

../texindex:
	(cd ..; make texindex)

../makeinfo:
	(cd ..; make makeinfo)
