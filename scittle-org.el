;;; scittle-org.el --- Org export support for Scittle blocks  -*- lexical-binding: t; -*-

;; This file provides helpers to:
;; - Edit #+begin_src scittle blocks using clojurescript-mode
;; - Export scittle src blocks as <script type="application/x-scittle">...</script>
;; - Inject required runtime scripts into HTML export when scittle blocks are present

;;; Commentary:
;;
;; Usage:
;;   (require 'scittle-org)
;;   (scittle-org-setup)
;;
;; Then in Org:
;;   #+begin_src scittle :exports none
;;   (js/console.log "hi")
;;   #+end_src
;;
;; On HTML export, the block will be embedded as a scittle script tag and the
;; required JS runtimes will be injected into the HTML head.
;;
;;; Code:

(require 'org)
(require 'ox)
(require 'ox-html)

(defgroup scittle-org nil
  "Org export support for Scittle source blocks."
  :group 'org-export)

(defcustom scittle-org-scittle-url
  "https://cdn.jsdelivr.net/npm/scittle@0.6.15/dist/scittle.js"
  "URL for the Scittle runtime."
  :type 'string)

(defcustom scittle-org-include-reagent t
  "When non-nil, inject React/ReactDOM and scittle.reagent."
  :type 'boolean)

(defcustom scittle-org-react-url
  "https://unpkg.com/react@17/umd/react.production.min.js"
  "URL for React (UMD build)."
  :type 'string)

(defcustom scittle-org-react-dom-url
  "https://unpkg.com/react-dom@17/umd/react-dom.production.min.js"
  "URL for ReactDOM (UMD build)."
  :type 'string)

(defcustom scittle-org-scittle-reagent-url
  "https://cdn.jsdelivr.net/npm/scittle@0.6.15/dist/scittle.reagent.js"
  "URL for scittle.reagent runtime."
  :type 'string)

(defun scittle-org--head-snippet ()
  "Return HTML <script> tags needed for scittle execution."
  (concat
   (format "<script src=\"%s\" type=\"application/javascript\"></script>\n"
	   scittle-org-scittle-url)
   (when scittle-org-include-reagent
     (concat
      (format "<script crossorigin src=\"%s\"></script>\n" scittle-org-react-url)
      (format "<script crossorigin src=\"%s\"></script>\n" scittle-org-react-dom-url)
      (format "<script src=\"%s\" type=\"application/javascript\"></script>\n"
	      scittle-org-scittle-reagent-url)))))

(defun scittle-org--buffer-has-scittle-src-p ()
  "Return non-nil if current Org buffer contains a scittle src block."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^[ \t]*#\\+begin_src[ \t]+scittle\\(\\s-\\|$\\)" nil t)))

(defun scittle-org--src-block-exports (src-block info)
  "Determine exports behavior for SRC-BLOCK.

Returns one of symbols: 'code, 'both, or 'none.

We treat anything other than code/both as 'none (run only)."
  (let* ((exports (or (org-element-property :exports src-block)
		      (plist-get info :exports)
		      'code)))
    (cond
     ((memq exports '(both)) 'both)
     ((memq exports '(code)) 'code)
     (t 'none))))

(defun scittle-org--html-transcode-src-block (src-block contents info)
  "Transcode scittle SRC-BLOCK into HTML script tag.

Respects :exports:
- code: show as code block (normal org HTML)
- none: run only (script tag)
- both: show code block + script tag"
  (let* ((code (org-element-property :value src-block))
	 (exports (scittle-org--src-block-exports src-block info)))
    ;; Mark for injection.
    (setq-local scittle-org--needs-runtime t)
    (pcase exports
      ('both (concat (org-html-src-block src-block contents info)
		     "\n<script type=\"application/x-scittle\">\n"
		     code
		     "</script>\n"))
      ('code (org-html-src-block src-block contents info))
      (_     (concat "<script type=\"application/x-scittle\">\n"
		     code
		     "</script>\n")))))

(defun scittle-org--export-filter (output backend info)
  "Inject scittle runtime scripts into HTML OUTPUT if needed."
  (when (and (org-export-derived-backend-p backend 'html)
	     (bound-and-true-p scittle-org--needs-runtime))
    (replace-regexp-in-string
     "</head>"
     (concat (scittle-org--head-snippet) "</head>")
     output
     t t)))

(defun scittle-org--before-processing (_backend)
  "Org export hook: reset internal flags per-export."
  (setq-local scittle-org--needs-runtime (scittle-org--buffer-has-scittle-src-p)))

;;;###autoload
(defun scittle-org-enable ()
  "Enable scittle-org export integration."
  (interactive)
  ;; Editing support in org-src buffers.
  (add-to-list 'org-src-lang-modes '("scittle" . clojurescript))

  ;; Ensure our backend modifications get applied.
  (add-hook 'org-export-before-processing-hook #'scittle-org--before-processing)
  (add-hook 'org-export-filter-final-output-functions #'scittle-org--export-filter)

  ;; Override HTML translation for src blocks.
  ;; We do this by defining a derived backend and making it the default HTML backend.
  (org-export-define-derived-backend 'scittle-html 'html
    :translate-alist '((src-block . scittle-org--src-block-dispatch)))

  (setq org-html-backend 'scittle-html))

(defun scittle-org--src-block-dispatch (src-block contents info)
  "Dispatch src-block transcoding.

If language is scittle, use scittle transcoder; otherwise fall back to org's."
  (let ((lang (org-element-property :language src-block)))
    (if (and lang (string= (downcase lang) "scittle"))
	(scittle-org--html-transcode-src-block src-block contents info)
      (org-html-src-block src-block contents info))))

;;;###autoload
(defun scittle-org-disable ()
  "Disable scittle-org export integration."
  (interactive)
  (remove-hook 'org-export-before-processing-hook #'scittle-org--before-processing)
  (remove-hook 'org-export-filter-final-output-functions #'scittle-org--export-filter)
  ;; Don't try to undo org-html-backend here because users may have their own backend.
  )

;;;###autoload
(defun scittle-org-setup ()
  "Convenience helper to enable scittle-org."
  (interactive)
  (scittle-org-enable))

(provide 'scittle-org)
;;; scittle-org.el ends here
