;;; org Twiki and Foswiki export --- Derek Feichtinger
;;;                                  <derek.feichtinger@psi.ch>

;; based on ox-confluence by Sébastien Delafond

;;; Commentary:
;;
;; ox-twiki.el lets you convert Org files to twiki buffers
;; using the ox.el export engine.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;	 (require 'ox-twiki)
;;
;; Export Org files to twiki:
;; M-x org-twiki-export-as-twiki RET
;;
;;; Code:

(require 'ox)
;;(require 'ox-ascii)
(require 'ox-tiddly)

;; Define the backend itself
(org-export-define-derived-backend 'twiki 'ascii
  :translate-alist '((bold . org-twiki-bold)
		     (example-block . org-twiki-example-block)
		     (fixed-width . org-twiki-fixed-width)
		     (footnote-definition . org-twiki-empty)
		     (footnote-reference . org-twiki-empty)
		     (headline . org-twiki-headline)
		     (italic . org-twiki-italic)
		     (item . org-twiki-item)
		     (link . org-twiki-link)
		     (paragraph . org-twiki-paragraph)
;;		     (plain-list . org-twiki-plain-list)
		     (section . org-twiki-section)
		     (src-block . org-twiki-src-block)
		     (strike-through . org-twiki-strike-through)
		     (table . org-twiki-table)
		     (table-cell . org-twiki-table-cell)
		     (table-row . org-twiki-table-row)
		     (template . org-twiki-template)
		     (underline . org-twiki-underline)
		     (verbatim . org-twiki-verbatim))
  :menu-entry '(?w 1
		   ((?f "As Foswiki/Twiki buffer" org-twiki-export-as-twiki))))

;;;;;;;;;;
;; debugging helpers
(defun org-enclose-element-property (plist property tag)
  (format "<%s>%s</%s>" tag (org-element-property property plist) tag)
)

(defun plist-get-keys (pl)
  (let (result)
      (loop for (key val) on pl by #'cddr
	    do (push key result))
      result)
)
;;;;;;;;;;

;; All the functions we use
(defun org-twiki-bold (bold contents info)
  (format "*%s*" contents))

(defun org-twiki-empty (empty contents info)
  "")

(defun org-twiki-plain-list (plain-list contents info)
  ;; (format "<TYPE>%s</TYPE>\n<STRUCTURE>%s</STRUCTURE>\n<CONTENTS>%s</CONTENTS>\n<PLAIN-LIST>%s</PLAIN-LIST>"
  ;; 	  (org-element-property :type plain-list)
  ;; 	  (org-element-property :structure plain-list)
  ;; 	  contents plain-list)

  ;; (concat (org-enclose-element-property plain-list :type "TYPE")
  ;; 	  (org-enclose-element-property plain-list :structure "STRUCTURE")
  ;; 	  (format "\n<CONTENTS>%s</CONTENTS>" contents))
  contents
  )

(defun org-twiki-item (item contents info)
  ;; (message "<ITEM_PROPS>%s</ITEM_PROPS>" (plist-get-keys (cadr item)))
  ;; (message "<ITEM_CONTENTS>%s</ITEM_CONTENTS>" contents)
  (let* ((beg (org-element-property :begin item))
	 (struct (org-element-property :structure item))
	 (itemstruct (assoc beg struct))
	 (parent (org-element-property :parent item))
	 (ltype (org-element-property :type parent))
	 (indices (org-list-get-item-number
					(org-element-property :begin item)
					struct
					(org-list-prevs-alist struct)
					(org-list-parents-alist struct))))
    (concat 
     ;; (format "beg: %s; " beg)
     ;; (format "str: %s; " itemstruct)
     ;; (format "type: %s; " ltype)
     ;; (format "\n  olpa: %s; " (org-list-prevs-alist struct))
     ;; (format "\n  olpara: %s; " (org-list-parents-alist struct))
     ;; (format "number: %s;\n" indices)	    
     (make-string (* 3  (length indices)) ? )
     (if (eq ltype 'ordered) "1. " "* ")
     contents))
)

(defun org-twiki-example-block (example-block contents info)
  (format "\n<verbatim>\n%s</verbatim>\n"
	  (org-export-format-code-default example-block info))
  )

(defun org-twiki-italic (italic contents info)
  (format "//%s//" contents))

(defun org-twiki-fixed-width (fixed-width contents info)
  "A fixed-width line starts with a colon character and a
whitespace or an end of line. Fixed width areas can contain any
number of consecutive fixed-width lines."
  (format "\n<verbatim>\n%s</verbatim>\n"
	  (org-element-property :value fixed-width))
)

(defun org-twiki-verbatim (verbatim contents info)
  "Transcode VERBATIM from Org to Twiki.
CONTENTS is nil.  INFO is a plist holding contextual
information.

lines using the =string= markup end up here"
  (format "=%s=" (org-element-property :value verbatim)))

(defun org-twiki-headline (headline contents info)
  (let ((low-level-rank (org-export-low-level-p headline info))
        (text (org-export-data (org-element-property :title headline)
                               info))
        (level (org-export-get-relative-level headline info)))
    ;; Else: Standard headline.
    (format "---%s %s\n%s" (make-string level ?+) text
            (if (org-string-nw-p contents) contents
              ""))))

(defun org-twiki-link (link desc info)
  (let ((raw-link (org-element-property :raw-link link)))
    (concat "[[" raw-link
            (when (org-string-nw-p desc) (format "][%s" desc))
            "]]")))

;; replace all newlines in paragraphs (includes list item text, which
;; also constitutes a paragraph
(defun org-twiki-paragraph (paragraph contents info)
  (replace-regexp-in-string "\n" " " contents))

(defun org-twiki-section (section contents info)
  contents)

(defun org-twiki-src-block (src-block contents info)
  (let* ((srclang (org-element-property :language src-block))
	 (lang (cond ((equal srclang "shell") "bash")
		     (t srclang))))
    (format "%%CODE{\"%s\"}%%\n%s%%ENDCODE%%\n"	lang    
	    (org-export-format-code-default src-block info)))
  )

(defun org-twiki-strike-through (strike-through contents info)
  (format "-%s-" contents))

(defun org-twiki-table (table contents info)
  contents)

(defun org-twiki-table-row  (table-row contents info)
  (concat
   (if (org-string-nw-p contents) (format "|%s" contents)
     "")
   (when (org-export-table-row-ends-header-p table-row info)
     "|")))

(defun org-twiki-table-cell  (table-cell contents info)
  (let ((table-row (org-export-get-parent table-cell)))
    (concat
     (when (org-export-table-row-starts-header-p table-row info)
       "|")
     contents "|")))

(defun org-twiki-template (contents info)
  (let ((depth (plist-get info :with-toc)))
    (concat (when depth "%TOC%\n\n") contents)))

(defun org-twiki-underline (underline contents info)
  (format "_%s_" contents))

;; (defun org-twiki--block (language theme contents)
;;   (concat "\{code:theme=" theme
;;           (when language (format "|language=%s" language))
;;           "}\n"
;;           contents
;;           "\{code\}\n"))

;; main interactive entrypoint
;;;###autoload
(defun org-twiki-export-as-twiki
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title, table
of contents and footnote definitions from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org Twiki Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'twiki "*Org Twiki Export*"
    async subtreep visible-only body-only ext-plist (lambda () (text-mode))))

(provide 'ox-twiki)
