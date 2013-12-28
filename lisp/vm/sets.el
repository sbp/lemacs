;;; Set functions
;;; Copyright (C) 1993 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@uunet.uu.net) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to kyle@wonderworks.com

(provide 'sets)

(defconst sets-version "1.01"
  "Version number of the sets implementation.")

(defconst sets-typetag '(set)
  "Value is used to distinguish sets from other vectors.
The value of this variable is stored in each set structure,
but not the set itself.")

(defconst sets-vector-length 6
  "Length of the vector used as a set structure.")

(defvar sets-id 0
  "A number that is assigned to each set so that each has a unique identifier.
The value of this variable is incremented each time a new set is created.")

(defun sets-typetag-of (set) (aref set 0))
(defun sets-id-of (set) (aref set 1))
(defun sets-cardinality-of (set) (aref set 2))
(defun sets-=-function-of (set) (aref set 3))
(defun sets-<-function-of (set) (aref set 4))
(defun sets-tree-of (set) (aref set 5))

(defun sets-set-typetag-of (set tag) (aset set 0 tag))
(defun sets-set-id-of (set id) (aset set 1 id))
(defun sets-set-cardinality-of (set n) (aset set 2 n))
(defun sets-set-=-function-of (set f) (aset set 3 f))
(defun sets-set-<-function-of (set f) (aset set 4 f))
(defun sets-set-tree-of (set tree) (aset set 5 tree))

;;; The heart of the set implementation is a binary tree.  Left
;;; children < right children.  The tree is rebalanced when a
;;; certain degree of imbalance is detected.

(defun sets-tree-value-of (tree) (aref tree 0))
(defun sets-tree-left-of (tree) (aref tree 1))
(defun sets-tree-right-of (tree) (aref tree 2))

(defun sets-set-tree-value-of (tree value) (aset tree 0 value))
(defun sets-set-tree-left-of (tree left) (aset tree 1 left))
(defun sets-set-tree-right-of (tree right) (aset tree 2 right))

(defun sets-setp (object)
  "Returns t if OBJECT is a set."
  (and (vectorp object)
       (= (length object) sets-vector-length)
       (eq sets-typetag (sets-typetag-of object))))

(defun sets-check-set (object)
  "If OBJECT is not a set, signal wrong-type-argument."
  (if (not (sets-setp object))
      (signal 'wrong-type-argument (list 'setp object))))

(defun sets-make-set (&optional =-func <-func)
  "Returns an empty set.
Sets can contain any type of Emacs Lisp object as well as other sets.

You can also create sets capable of containing your own user
defined types.  To do this you need to write Lisp functions that
do the 'equals' and 'less-than' comparison for objects of your
defined type.  Then you must pass these two functions to this
function as its first and second arguments respectively.  You
will be able to use all the set manipulation functions on the
returned set.  Unless your comparison functions can also handle
other object types, you will not be able to reliably insert other object
types into sets created to handle your private types."
  (let ((s (make-vector sets-vector-length nil)))
    (sets-set-typetag-of s sets-typetag)
    (sets-set-id-of s sets-id)
    (setq sets-id (1+ sets-id))
    (sets-set-cardinality-of s 0)
    (sets-set-=-function-of s (or =-func 'sets-=))
    (sets-set-<-function-of s (or <-func 'sets-<))
    (sets-set-tree-of s nil)
    s ))

(defun sets-make-tree () (make-vector 3 nil))

;; This function does the insertion, deletion and
;; check-for-membership set operations.
(defun sets-set-xxxxxx (set job &rest items)
  (sets-check-set set)
  (let (newtree member parent tree level
	(=-func (sets-=-function-of set))
	(<-func (sets-<-function-of set)))
    (while items
      (setq tree (sets-tree-of set)
	    level 1)
      (cond
       ((null tree)
	;; set is empty
	(cond
	 ((eq job 'insert)
	  ;; inserting a node into an empty tree, easy.
	  (setq tree (sets-make-tree))
	  (sets-set-tree-value-of tree (car items))
	  (sets-set-tree-of set tree)
	  (sets-set-cardinality-of set 1))
	 ;; For the deletion and member check cases nothing needs
	 ;; to be done.  But since we now know the tree is empty
	 ;; there's no need to cycle through all the items in the
	 ;; list--- obviously they aren't in the tree.  So we set
	 ;; items to nil to get out of the loop.
	 (t (setq items nil))))
       (t
	;; climb the tree, looking for the item
	(while tree
	 (cond
	  ;; check item against current node for equality
	  ((funcall =-func (car items) (sets-tree-value-of tree))
	   (cond
	    ((eq job 'insert)
	     ;; item is already in the tree, quit climbing
	     (setq tree nil))
	    ((eq job 'member)
	     ;; found the member we're looking for, note it and quit climbing
	     (setq tree nil
		   member t))
	    ((eq job 'delete)
	     ;; We've found the value we want to delete.
	     ;;
	     ;; Deletion is the most complicated aspect of
	     ;; maintaining a tree because the orphaned children
	     ;; have to be relinked.  If the node we're deleting
	     ;; has two children they both can't be given to the
	     ;; parent.
	     ;;
	     ;; First we see if we have an easy case.  Check to
	     ;; see if we only have one child.  If so, we can
	     ;; give that child to the parent and the deletion is
	     ;; done.
	     (cond
	      ((and (sets-tree-left-of tree) (null (sets-tree-right-of tree)))
	       ;; child on the left, none on the right
	       (cond
		((null parent)
		 ;; current node is the root node, replace root with left child
		 (sets-set-tree-of set (sets-tree-left-of tree)))
		((eq (sets-tree-left-of parent) tree)
		 ;; current node is parent's left child, replace
		 ;; parent's link to current node with link to child
		 (sets-set-tree-left-of parent (sets-tree-left-of tree)))
		(t
		 ;; current node is parent's right child, replace
		 ;; parent's link to current node with link to child
		 (sets-set-tree-right-of parent (sets-tree-left-of tree)))))
	      ((and (sets-tree-right-of tree) (null (sets-tree-left-of tree)))
	       ;; child on right, none on the left
	       (cond
		((null parent)
		 ;; current node is the root, replace root with right child
		 (sets-set-tree-of set (sets-tree-right-of tree)))
		((eq (sets-tree-left-of parent) tree)
		 ;; current node is parent's left child, replace
		 ;; parent's link to current node with link to child
		 (sets-set-tree-left-of parent (sets-tree-right-of tree)))
		(t
		 ;; current node is parent's right child, replace
		 ;; parent's link to current node with link to child
		 (sets-set-tree-right-of parent (sets-tree-right-of tree)))))
	      ;; We may be luckier still and happen to be
	      ;; deleting a leaf node, i.e. a node with no
	      ;; children.
	      ((null (sets-tree-right-of tree))
	       ;; deleting a leaf node
	       (cond
		((null parent)
		 ;; current node is the root, nullify root
		 (sets-set-tree-of set nil))
		((eq (sets-tree-left-of parent) tree)
		 ;; current node is parent's left child, nullify it
		 (sets-set-tree-left-of parent nil))
		(t
		 ;; current node is parent's right child, nullify it
		 (sets-set-tree-right-of parent nil))))
	      ;; OK, we're left with the tough case; the current
	      ;; node has two children.
	      ;; 
	      ;; The parent can inherit either child but the
	      ;; other child must be given to the node just
	      ;; before or just after the current node with
	      ;; respect to to an inorder traversal of the tree.
	      ;;
	      ;; Strategy:
	      ;; We find both the 'before' and 'after' nodes and
	      ;; give the appropriate child to the one that's
	      ;; highest in the tree, in the hopes that this will
	      ;; help with long term tree balancing.  The other
	      ;; child will be given to the parent of the current
	      ;; node.
	      ;; 
	      ;; To find the 'before' node, go to the left child
	      ;; and then go right as far as possible.  To find
	      ;; the 'after' node, to the right child and then go
	      ;; left as far as possible.
	      (t
	       (let ((before (sets-tree-left-of tree))
		     (before-level 0)
		     (after (sets-tree-right-of tree))
		     (after-level 0))
		 ;; find the 'before' node
		 (while (sets-tree-left-of before)
		   (setq before (sets-tree-left-of before)
			 before-level (1+ before-level)))
		 ;; find the 'after' node
		 (while (sets-tree-right-of after)
		   (setq after (sets-tree-right-of after)
			 after-level (1+ after-level)))
		 ;; Now attach appropriate child to the node we
		 ;; found that's highest in the tree (lowest
		 ;; level)
		 (cond
		  ((< before-level after-level)
		   ;; We give the child to the 'before' node.
		   ;; The 'before' node has no right child.  (If
		   ;; it did, its child would be the 'before'
		   ;; node.)  Attach the right child of the
		   ;; current node to the 'before' node.
		   (sets-set-tree-right-of before (sets-tree-right-of tree))
		   ;; now give the other child to the parent.
		   (cond
		    ((null parent)
		     ;; current node is root, make left child be root
		     (sets-set-tree-of set (sets-tree-left-of tree)))
		    ((eq (sets-tree-left-of parent) tree)
		     ;; current node is parent's left child,
		     ;; replace parent's link to current node
		     ;; with link to child.
		     (sets-set-tree-left-of parent (sets-tree-left-of tree)))
		    (t
		     ;; current node is parent's right child,
		     ;; replace parent's link to current node
		     ;; with link to child.
		     (sets-set-tree-right-of
		      parent (sets-tree-left-of tree)))))
		  (t
		   ;; We give the child to the 'after' node.
		   ;; The 'after' node has no left child.  (If
		   ;; it did, its child would be the 'after'
		   ;; node.)  Attach the right child of the
		   ;; current node to the 'after' node.
		   (sets-set-tree-left-of after (sets-tree-left-of tree))
		   (cond
		    ((null parent)
		     ;; current node is the root, replace root with right child
		     (sets-set-tree-of set (sets-tree-right-of tree)))
		    ((eq (sets-tree-left-of parent) tree)
		     ;; current node is parent's left child,
		     ;; replace parent's link to current node
		     ;; with link to child.
		     (sets-set-tree-left-of parent (sets-tree-right-of tree)))
		    (t
		     ;; current node is parent's right child,
		     ;; replace parent's link to current node
		     ;; with link to child.
		     (sets-set-tree-right-of
		      parent (sets-tree-right-of tree)))))))))
	     ;; finally, decrease the cardinality (item count) of the set
	     (sets-set-cardinality-of set (1- (sets-cardinality-of set)))
	     ;; quit climbing the tree, we're done with this item
	     (setq tree nil))))
	  ;; check if item is less than the current node
	  ((funcall <-func (car items) (sets-tree-value-of tree))
	   (cond
	    ;; If there's a left node, go to it.
	    ((sets-tree-left-of tree)
	     (setq parent tree
		   tree (sets-tree-left-of tree)
		   level (1+ level)))
	    ;; There's no left node, the search is over.  For the
	    ;; member check and deletion cases the search failed,
	    ;; note it and quit climbing.
	    ((eq job 'member) (setq tree nil))
	    ((eq job 'delete) (setq tree nil))
	    ;; For insertions, make the new node the left child
	    ((eq job 'insert)
	     (setq newtree (sets-make-tree))
	     (sets-set-tree-value-of newtree (car items))
	     (sets-set-tree-left-of tree newtree)
	     (sets-set-cardinality-of
	      set (1+ (sets-cardinality-of set)))
	     ;; done with this insertion, quit climbing the tree
	     (setq tree nil)
	     (if (< (sets-cardinality-of set) (lsh 1 (1- level)))
		 (sets-balance-set-tree set)))))
	  ;; If there's a right node, go to it.
	  ((sets-tree-right-of tree)
	   (setq parent tree
		 tree (sets-tree-right-of tree)
		 level (1+ level)))
	  ;; There's no right node, the search is over.  For the
	  ;; member check and deletion cases the search failed,
	  ;; note it and quit climbing.
	  ((eq job 'member) (setq tree nil))
	  ((eq job 'delete) (setq tree nil))
	  ;; For insertions, make the new node the right child
	  ((eq job 'insert)
	   (setq newtree (sets-make-tree))
	   (sets-set-tree-value-of newtree (car items))
	   (sets-set-tree-right-of tree newtree)
	   (sets-set-cardinality-of
	    set (1+ (sets-cardinality-of set)))
	   ;; done with this insertion, quit climbing the tree
	   (setq tree nil)
	   (if (< (sets-cardinality-of set) (lsh 1 (1- level)))
	       (sets-balance-set-tree set)))))))
      ;; move to the next time and do it all again
      (setq items (cdr items)))
    (if (eq job 'member) member set)))

(defun sets-set-insert (set &rest items)
  "Insert remaining arguments into SET.
Returns SET."
  (apply 'sets-set-xxxxxx set 'insert items))

(defun sets-set-delete (set &rest items)
  "Delete remaining arguments from SET.
Returns SET."
  (apply 'sets-set-xxxxxx set 'delete items))

(defun sets-set-member (set item)
  "Returns t if SET contains ITEM."
  (funcall 'sets-set-xxxxxx set 'member item))

(defun sets-inorder-maptree (tree function)
  (if tree
      (progn
	(and (sets-tree-left-of tree)
	     (sets-inorder-maptree (sets-tree-left-of tree) function))
	(funcall function tree)
	(and (sets-tree-right-of tree)
	     (sets-inorder-maptree (sets-tree-right-of tree) function)))))

(defun sets-inorder-maptreevalues (tree function)
  (if tree
      (progn
	(and (sets-tree-left-of tree)
	     (sets-inorder-maptreevalues (sets-tree-left-of tree) function))
	(funcall function (sets-tree-value-of tree))
	(and (sets-tree-right-of tree)
	     (sets-inorder-maptreevalues
	      (sets-tree-right-of tree) function)))))

(defun sets-balance-set-tree (set)
  (if (> (sets-cardinality-of set) 2)
      (let ((value-vector (make-vector (sets-cardinality-of set) nil))
	    (node-vector (make-vector (1+ (sets-cardinality-of set)) nil))
	    fill link-to stop-at
	    (tree (sets-tree-of set)))
	(setq fill 0)
	(sets-inorder-maptree
	 tree
	 (function
	  (lambda (tree)
	    (aset value-vector fill (sets-tree-value-of tree))
	    (aset node-vector fill tree)
	    (setq fill (1+ fill)))))
	(setq fill 0
	      link-to 1
	      stop-at (sets-cardinality-of set))
	(catch 'done
	  (while t
	    (sets-set-tree-left-of (aref node-vector fill)
				   (aref node-vector link-to))
	    (setq link-to (1+ link-to))
	    (and (> link-to stop-at) (setq link-to stop-at))
	    (sets-set-tree-right-of (aref node-vector fill)
				    (aref node-vector link-to))
	    (setq link-to (1+ link-to))
	    (and (> link-to stop-at) (setq link-to stop-at))
	    (setq fill (1+ fill))
	    (and (= fill stop-at) (throw 'done t))))
	(setq fill 0)
	(sets-inorder-maptree
	 (aref node-vector 0)
	 (function
	  (lambda (tree)
	    (sets-set-tree-value-of tree (aref value-vector fill))
	    (setq fill (1+ fill)))))
	(sets-set-tree-of set (aref node-vector 0)))))

(defun sets-print-set (set)
  "Print SET using {...} notation.
The null set is printed as {}."
  (princ "{")
  (let (first-node-printed)
  (sets-inorder-maptree
   (sets-tree-of set)
   (function
    (lambda (tree)
      (if first-node-printed
	  (princ " ")
	(setq first-node-printed tree))
      (prin1 (sets-tree-value-of tree)))))
  (princ "}")
  set ))

(defun sets-set-union (&rest sets)
  "Return the union of all the set arguments.
That is, the returned set will contain all the elements of the sets
passed to this function."
  (let (newset)
    (if sets
	(progn
	  (sets-check-set (car sets))
	  (setq newset (sets-copy-set (car sets))
		sets (cdr sets))
	  (while sets
	    (sets-check-set (car sets))
	    (sets-inorder-maptree
	     (sets-tree-of (car sets))
	     (function
	      (lambda (tree)
		(sets-set-insert newset (sets-tree-value-of tree)))))
	    (setq sets (cdr sets)))))
    (or newset (sets-make-set))))

(defun sets-set-intersection (&rest sets)
  "Return the intersection of all the set arguments.
That is, the returned set will contain all the common elements of the sets
passed to this function."
  (let (newset current-intersection)
    (if sets
	(progn
	  (sets-check-set (car sets))
	  (setq newset (sets-copy-set (car sets))
		current-intersection (car sets)
		sets (cdr sets))
	  (while sets
	    (sets-check-set (car sets))
	    (sets-inorder-maptree
	     (sets-tree-of current-intersection)
	     (function
	      (lambda (tree)
		(if (not (sets-set-member (car sets)
					  (sets-tree-value-of tree)))
		    (sets-set-delete newset (sets-tree-value-of tree))))))
	    (setq current-intersection (sets-copy-set newset)
		  sets (cdr sets)))))
    (or newset (sets-make-set))))

(defun sets-type (object)
  (cond ((numberp object) 'number)
	((stringp object) 'string)
	((symbolp object) 'symbol)
	((sets-setp object) 'set)
	((consp object) 'cons)
	((vectorp object) 'vector)
	((markerp object) 'marker)
	((bufferp object) 'buffer)
	((windowp object) 'window)
	;; anything else is too exotic to worry about
	(t 'exotic)))

(defun sets-= (p q)
  (if (or (atom p) (atom q))
      ;; maybe symbols should be an exception here
      (eq p q)
    (let ((type-p (sets-type p))
	  (type-q (sets-type q)))
      (if (not (eq type-p type-q))
	  nil
	(cond
	 ((consp p)
	  (let ((return-value t))
	    (while (and (consp p) (consp q) return-value)
	      (setq return-value (sets-= (car p) (car q))
		    p (cdr p)
		    q (cdr q)))
	    (and return-value (sets-= p q))))
	 ((vectorp p)
	  (let ((return-value t)
		(i 0)
		(stop-at (length p)))
	    (if (not (= stop-at (length q)))
		nil
	      (while (and (< i stop-at) return-value)
		(setq return-value (sets-= (aref p i) (aref q i)))
		(setq i (1+ i)))
	      return-value)))
	 ((sets-setp p) (= (sets-id-of p) (sets-id-of q)))
	 (t (string= (format "%S" p) (format "%S" q))))))))

(defun sets-< (p q)
  (let ((type-p (sets-type p))
	(type-q (sets-type q)))
    (cond
     ((not (eq type-p type-q))
      (string< type-p type-q))
     ((numberp p)
      (< p q))
     ((memq type-p '(string symbol))
      (string< p q))
     ((markerp p)
      (< p q))
     ((consp p)
      (while (and (consp p) (consp q) (sets-= (car p) (car q)))
	(setq p (cdr p)
	      q (cdr q)))
      (sets-< p q))
     ((vectorp p)
      (let ((return-value t)
	    (i 0)
	    (stop-at (length p)))
	(if (not (= stop-at (length q)))
	    (if (< stop-at (length q))
		t
	      nil)
	  (while (and (< i stop-at) return-value)
	    (setq return-value (sets-= (aref p i) (aref q i)))
	    (setq i (1+ i)))
	  (if return-value
	      nil
	    (sets-< p q)))))
     ((sets-setp p) (< (sets-id-of p) (sets-id-of q)))
     (t (string< (format "%S" p) (format "%S" q))))))

(defun sets-copy-set (set)
  (sets-check-set set)
  (let ((newset (sets-make-set (sets-=-function-of set)
			       (sets-<-function-of set))))
    (sets-inorder-maptree
     (sets-tree-of set)
     (function
      (lambda (tree)
	(sets-set-insert newset (sets-tree-value-of tree)))))
    newset ))

(defun sets-set (&rest items)
  "Returns a new set containing all of the arguments."
  (apply 'sets-set-insert (sets-make-set) items))

(defun sets-mapset (function set)
  "Call FUNCTION with each element of SET as an argument."
  (sets-check-set set)
  (sets-inorder-maptreevalues (sets-tree-of set) function))
