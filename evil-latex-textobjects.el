;;; evil-latex-textobjects.el --- LaTeX text objects for evil

;; Copyright (C) 2015  Hans-Peter Deifel
;; Copyright (C) 2020  Qianchuan Ye

;; Author: Hans-Peter Deifel <hpd@hpdeifel.de>
;;         Qianchuan Ye <yeqianchuan@gmail.com>
;; Keywords: tex, wp, convenience, vi, evil
;; Version: 1.0-git
;; Package-Requires: ((evil "1.0") (auctex "11.88"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Provides a minor mode that installs several additional LaTeX
;;; specific text objects for evil-mode:
;;;
;;;  "  Quote                `` .. '' or " .. "
;;;  \  Display math         \[ .. \] or \( .. \)
;;;  $  Inline math          $ .. $
;;;  m  TeX macro            \foo{..}
;;;  e  LaTeX environment    \begin{foo}..\end{foo}
;;;
;;; To enable this mode in LaTeX buffers, add this to your init file:
;;;
;;; (require 'evil-latex-textobjects)
;;; (add-hook 'LaTeX-mode-hook #'evil-latex-textobjects-setup)

;;; Code:

(require 'evil)
(require 'latex)

(defun evil-latex-textobjects--select-block (blocks beg end type count inc)
  (let (result)
    (dolist (blk blocks result)
      (let ((range (pcase blk
                     (`(,open . ,close)
                      (ignore-errors
                        (evil-select-paren open close beg end type count inc)))
                     (char
                      (ignore-errors
                        (evil-select-quote char beg end type count inc))))))
        (pcase range
          (`(,range-x ,range-y . ,_)
           (when (and (>= (or beg (point)) range-x)
                      (<= (or end (point)) range-y))
             (pcase result
               (`(,result-x ,result-y . ,_)
                (when (< (- range-y range-x)
                         (- result-y result-x))
                  (setq result range)))
               (_
                (setq result range))))))))))

(evil-define-text-object evil-latex-textobjects-inner-quote (count &optional beg end type)
  "Select inner quote"
  (evil-latex-textobjects--select-block
   '(("``" . "''") ?\")
   beg end type count nil))

(evil-define-text-object evil-latex-textobjects-a-quote (count &optional beg end type)
  "Select a quote"
  (evil-latex-textobjects--select-block
   '(("``" . "''") ?\")
   beg end type count t))

(evil-define-text-object evil-latex-textobjects-inner-dollar (count &optional beg end type)
  "Select inner dollar"
  (evil-select-quote ?$ beg end type count nil))

(evil-define-text-object evil-latex-textobjects-a-dollar (count &optional beg end type)
  "Select a dollar"
  (evil-select-quote ?$ beg end type count t))

(evil-define-text-object evil-latex-textobjects-inner-math (count &optional beg end type)
  "Select inner \\[ \\] or \\( \\)."
  (evil-latex-textobjects--select-block
   '(("\\\\\\[" . "\\\\\\]") ("\\\\(" . "\\\\)"))
   beg end type count nil))

(evil-define-text-object evil-latex-textobjects-a-math (count &optional beg end type)
  "Select a \\[ \\] or \\( \\)."
  (evil-latex-textobjects--select-block
   '(("\\\\\\[" . "\\\\\\]") ("\\\\(" . "\\\\)"))
   beg end type count t))

(defun evil-latex-textobjects-macro-beginning ()
  "Return (start . end) of the macro-beginning to the left of point.

If no enclosing macro is found, return nil.
For example for \macro{foo|bar} it returns the start and end of \"\macro{\""
  (let ((beg (TeX-find-macro-start)))
    (when beg
      (save-excursion
        (goto-char beg)
        (forward-char)                  ; backslash
        (skip-chars-forward "A-Za-z@*") ; macro-name
        (when (looking-at "{\\|\\[")
          (forward-char))                ; opening brace
        (cons beg (point))))))

(defun evil-latex-textobjects-macro-end ()
  "Return (start . end) of the end of the enclosing macro.

If no such macro can be found, return nil"
  (let ((end (TeX-find-macro-end)))
    (when end
      (save-excursion
        (goto-char end)
        (when (looking-back "}\\|\\]")
          (backward-char))               ; closing brace
        (cons (point) end)))))

(evil-define-text-object evil-latex-textobjects-a-macro (count &optional beg end type)
  "Select a TeX macro"
  :extend-selection nil
  (let ((beg (evil-latex-textobjects-macro-beginning))
        (end (evil-latex-textobjects-macro-end)))
    (if (and beg end)
        (list (car beg) (cdr end))
      (error "No enclosing macro found"))))

(evil-define-text-object evil-latex-textobjects-inner-macro (count &optional beg end type)
  "Select inner TeX macro"
  :extend-selection nil
  (let ((beg (evil-latex-textobjects-macro-beginning))
        (end (evil-latex-textobjects-macro-end)))
    (cond
     ((or (null beg) (null end))
      (error "No enclosing macro found"))
     ((= (cdr beg) (car end))           ; macro has no content
      (list (1+ (car beg))              ; return macro boundaries excluding \
            (cdr beg)))
     (t (list (cdr beg) (car end))))))

(defun evil-latex-textobjects-env-beginning ()
  "Return (start . end) of the \\begin{foo} to the left of point."
  (let (beg)
    (save-excursion
      (LaTeX-find-matching-begin)       ; we are at backslash
      (setq beg (point))
      (skip-chars-forward "^{")         ; goto opening brace
      (forward-sexp)                    ; goto closing brace
      (cons beg (point)))))

(defun evil-latex-textobjects-env-end ()
  "Return (start . end) of the \\end{foo} to the right of point."
  (let (end)
    (save-excursion
      (LaTeX-find-matching-end)         ; we are at closing brace
      (setq end (point))
      (backward-sexp)                   ; goto opening brace
      (search-backward "\\")            ; goto backslash
      (cons (point) end))))


(evil-define-text-object evil-latex-textobjects-an-env (count &optional beg end type)
  "Select a LaTeX environment"
  :extend-selection nil
  (let ((beg (evil-latex-textobjects-env-beginning))
        (end (evil-latex-textobjects-env-end)))
    (list (car beg) (cdr end))))

(evil-define-text-object evil-latex-textobjects-inner-env (count &optional beg end type)
  "Select a LaTeX environment"
  :extend-selection nil
  (let ((beg (evil-latex-textobjects-env-beginning))
        (end (evil-latex-textobjects-env-end)))
    (list (cdr beg) (car end))))

;;;###autoload
(defun evil-latex-textobjects-setup ()
  (define-key evil-inner-text-objects-map "\"" #'evil-latex-textobjects-inner-quote)
  (define-key evil-outer-text-objects-map "\"" #'evil-latex-textobjects-a-quote)
  (define-key evil-inner-text-objects-map "$" #'evil-latex-textobjects-inner-dollar)
  (define-key evil-outer-text-objects-map "$" #'evil-latex-textobjects-a-dollar)
  (define-key evil-inner-text-objects-map "\\" #'evil-latex-textobjects-inner-math)
  (define-key evil-outer-text-objects-map "\\" #'evil-latex-textobjects-a-math)
  (define-key evil-outer-text-objects-map "m" #'evil-latex-textobjects-a-macro)
  (define-key evil-inner-text-objects-map "m" #'evil-latex-textobjects-inner-macro)
  (define-key evil-outer-text-objects-map "e" #'evil-latex-textobjects-an-env)
  (define-key evil-inner-text-objects-map "e" #'evil-latex-textobjects-inner-env))

(provide 'evil-latex-textobjects)

;;; evil-latex-textobjects.el ends here
