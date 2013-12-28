# Makefile for the   GNU Emacs Lisp Reference Manual.
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

VERSION=2.02.2
NAME=lispref

# Uncomment this line for permuted index.
# permuted_index = 1 

# List of all the texinfo files in the manual:

srcs = lispref.texinfo back.texinfo \
  abbrevs.texinfo annotations.texinfo anti.texinfo backups.texinfo \
  locals.texinfo buffers.texinfo commands.texinfo compile.texinfo \
  control.texinfo debugging.texinfo display.texinfo edebug-inc.texinfo \
  errors.texinfo \
  eval.texinfo extents.texinfo files.texinfo functions.texinfo \
  help.texinfo hooks.texinfo internals.texinfo \
  internationalization.texinfo intro.texinfo keymaps.texinfo \
  lists.texinfo loading.texinfo macros.texinfo maps.texinfo \
  markers.texinfo menus.texinfo minibuf.texinfo modes.texinfo \
  numbers.texinfo objects.texinfo os.texinfo positions.texinfo \
  processes.texinfo screens.texinfo searching.texinfo sequences.texinfo \
  streams.texinfo strings.texinfo symbols.texinfo syntax.texinfo \
  text.texinfo tips.texinfo tooltalk.texinfo variables.texinfo \
  windows.texinfo x-windows.texinfo index.unperm index.perm

all: ../../info/$(NAME).info

$(NAME).dvi: $(srcs) index.texinfo ../texindex
	# Avoid losing old contents of aux file entirely.
	-mv $(NAME).aux $(NAME).oaux
	# First shot to define xrefs:
	$(TEX) $(NAME).texinfo
	if [ a${permuted_index} != a ]; \
	then \
	  ./permute-index; \
	  mv permuted.fns $(NAME).fns; \
	else \
	  ../texindex $(NAME).??; \
	fi
	$(TEX) $(NAME).texinfo

index.texinfo:
	if [ a${permuted_index} != a ]; \
	then \
	  ln -s index.perm index.texinfo; \
	else \
	  ln -s index.unperm index.texinfo; \
	fi

../../info/$(NAME).info: $(srcs) index.texinfo ../makeinfo
	../makeinfo $(NAME).texinfo

clean:
	rm -f *.toc *.aux *.log *.cp *.cps *.fn *.fns *.tp *.tps \
              *.vr *.vrs *.pg *.pgs *.ky *.kys
	rm -f make.out core 
	rm -f index.texinfo

../texindex:
	(cd ..; make texindex)

../makeinfo:
	(cd ..; make makeinfo)
