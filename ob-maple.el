;;; ob-maple.el --- org-babel functions for Maple evaluation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Joseph S. Riel

;; Author: Joseph S. Riel <joer@san.rr.com>
;;         Bo Jakobsen <boj@boj.dk>
;; Keywords: languages, tools
;; Version: 1.0.0
;; Package-Requires: ((maplev "3.2.0"))
;; URL: https://github.com/JoeRiel/ob-maple

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides some basic hooks to interact with Maple
;; in an org file.

;;; Code:

(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'cl-lib)
;; (require 'maplev)

(declare-function maplev-config "maplev-config")
(declare-function maplev--pmaple-buffer "maplev-pmaple")
(declare-function maplev--pmaple-process "maplev-pmaple")
(declare-function maplev-pmaple-direct "maplev-pmaple" (input &optional delete))

(defvar maplev-config)
(defvar maplev-pmaple-prompt)

(defvar org-src-lang-modes)
(add-to-list 'org-src-lang-modes '("maple" . maplev))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("maple" . "mpl"))

(defcustom org-babel-maple-command
  "maple2025 -q -B -c 'interface(prettyprint=0)'"
  "String used to execute Maple code.
The command (maple) should be the installed name of command-line Maple."
  :group 'org-babel
  :group 'maplev
  :type 'string)

(defcustom org-babel-maple-mode 'maplev-mode
  "Mode for use in running Maple interactively."
  :group 'org-babel
  :group 'maplev
  :type 'symbol)

(defun org-babel-execute:maple (body params)
  "Execute a block of Maple code with org-babel.
This function is called by `org-babel-execute-src-block'.
BODY is a string of the contents of the block.
PARAMS is a list of cons-cells of the form \(:key . \"value\"\).

When the :session header argument names a session, the block is
evaluated in a persistent pmaple REPL (Read-Eval-Print-Loop) so that
state (assignments, procedures, packages) carries across blocks.  Each
distinct session name gets its own pmaple buffer and Maple process, so
`:session a' and `:session b' are independent; a bare `:session' shares
the interactive \\[pmaple] buffer.  Session evaluation currently
captures printed output only; the :results header should be
`output' (the `value' result-type is supported only for the non-session,
one-shot evaluator).

With `:results graphics file' and a `:file', the block's last value
should be a plot (a `plot(...)' call or a plot structure); it is
exported to FILE as a PNG and FILE is returned so Org inserts an
image link.  Do NOT call `maplev:-Plot' in such a block -- that is
for the interactive REPL and would open a viewer; just end the block
with the plot itself (see `org-babel-maple--plot-export-string').

With a `:latex' header (e.g. `:latex yes' or `:latex inline') the
captured output is wrapped in LaTeX math delimiters so it renders as
a fragment.  Combine it with `:results output drawer' so Org inserts
the fragment inside a replaceable `:results:' drawer; then
`org-latex-preview' (C-c C-x C-l) shows it inline.  (Avoid `raw' here:
raw results have no delimiters, so re-running the block *appends*
instead of replacing.)  See `org-babel-maple--maybe-latex'."
  (let ((session (cdr (assoc :session params)))
        (graphics-file (org-babel-maple--graphics-file params)))
    (cond
     ;; Graphics: write a PNG and return its filename for the image link.
     (graphics-file
      (if (or (null session) (string= "none" session))
          ;; One-shot evaluation: span a fresh Maple for this block.
          (progn
            (org-babel-eval
             org-babel-maple-command
             (org-babel-maple--graphics-body body params graphics-file))
            graphics-file)
        (org-babel-maple--evaluate-session-graphics
         (org-babel-maple--get-session-buffer session)
         (org-babel-maple--expand-session-body body params)
         graphics-file params)))
     ;; Text result (possibly wrapped as a LaTeX fragment).
     (t
      (org-babel-maple--maybe-latex
       (if (or (null session) (string= "none" session))
           ;; One-shot evaluation: spawn a fresh Maple for this block.
           (org-babel-eval org-babel-maple-command
                           (org-babel-expand-body:maple body params))
         ;; Session evaluation: reuse the maplev pmaple REPL.
         (org-babel-maple--evaluate-session
          (org-babel-maple--get-session-buffer session)
          (org-babel-maple--expand-session-body body params)))
       params)))))

(defun org-babel-maple--maybe-latex (result params)
  "Wrap RESULT in LaTeX math delimiters when the `:latex' header asks for it.
PARAMS is an alist that may contain the `:latex' header, which selects
the delimiters: `inline' uses \\(...\\); any other non-empty
value (e.g. `yes', t, `display') uses display math \\[...\\].  A
nil/`no'/`none' value, or an empty RESULT, leaves RESULT unchanged.

This only marks the text as a LaTeX fragment; for it to render inline
in the buffer use `:results output drawer' (Org inserts the fragment
inside a `:results:' drawer, which is re-run replaceable and still
previewable) and `org-latex-preview' / `org-toggle-latex-fragment'.
Prefer `drawer' over `raw': a raw result has no delimiters, so
re-evaluating the block appends instead of replacing.  In Maple,
produce the body with `latex(expr);' (it prints the LaTeX)."
  (let ((mode (cdr (assoc :latex params))))
    (if (and mode
             (not (member mode '("no" "nil" "none")))
             result
             (not (string-empty-p (string-trim result))))
        (let ((tex (string-trim result)))
          (if (string= mode "inline")
              (concat "\\(" tex "\\)")
            (concat "\\[" tex "\\]")))
      result)))

(defun org-babel-maple--graphics-file (params)
  "Return the :file value when PARAMS request a graphics/file result, else nil.
A block with `:results graphics file :file foo.png' has both \"file\"
in its result-params and a `:file' header; that is the signal to
export a plot rather than capture text."
  (let ((file (cdr (assoc :file params)))
        (result-params (cdr (assoc :result-params params))))
    (when (and file (member "file" result-params))
      file)))

(defun org-babel-maple--plot-export-string (file params)
  "Return Maple code exporting the last computed value (a plot) to FILE.

FILE is expanded to an absolute path for Maple; the leading `%' is
Maple's ditto operator (the value of the previous top-level result), so
the user's block must end with the plot.

PARAMS is an alist which may contain the plot dimensions in pixels:
`:height' (default is 400) and `:width' (default is 600).

This uses the top-level `Export' command, which writes the PNG without
opening a viewer (unlike `maplev:-Plot')."

  (format
   (concat "_obmaple_plt := %%:\n"
           "Export(%S, _obmaple_plt, ':-format' = \"png\""
           ", ':-height' = %s, ':-width' = %s, ':-legacy' = false):\n")
   (expand-file-name file)
   (or (cdr (assoc :height params)) 400)
   (or (cdr (assoc :width params)) 600)))

(defun org-babel-maple--graphics-body (body params file)
  "Wrap BODY so its last value is exported to FILE as a PNG.
Used by the one-shot (non-session) path; PARAMS supplies any
variable assignments and the plot dimensions."
  (concat (org-babel-variable-assignments:maple params) "\n"
          body "\n"
          (org-babel-maple--plot-export-string file params)))

(defun org-babel-maple--session-buffer-name (session)
  "Return the pmaple buffer name to use for SESSION.
SESSION is the :session header value.  A bare `:session' (org
passes \"\" or \"yes\") shares the base buffer used by \\[pmaple],
preserving the milestone-1 behavior.  Every explicit session name
gets its own dedicated buffer, derived by tagging the base name,
so `:session a' and `:session b' map to independent buffers (and
hence independent Maple processes).

The base name comes from `maplev--pmaple-buffer', so a maplev-config
must already be assigned when this is called."
  (let ((base (maplev--pmaple-buffer)))         ; "Maple (<maple>)"
    (if (or (null session) (member session '("" "yes")))
        base
      ;; Insert " [SESSION]" right after the leading "Maple", keeping the
      ;; " (<maple>)" suffix so the name still identifies the binary.
      (if (string-prefix-p "Maple" base)
          (concat "Maple [" session "]" (substring base (length "Maple")))
        (concat base " [" session "]")))))

(defmacro org-babel-maple--with-session-buffer (bufname &rest body)
  "Evaluate BODY with `maplev--pmaple-buffer' overridden to return BUFNAME.
The maplev pmaple machinery (process lookup in `maplev--pmaple-process',
startup in `maplev-pmaple--start-process', the `maplev:-Setup' argument
built by `maplev-pmaple--get-pmaple-and-options', and `maplev-pmaple-direct')
all consult `maplev--pmaple-buffer' for the target buffer name.  Rebinding
it for the dynamic extent of BODY lets us host several named sessions on
top of the existing single-buffer machinery without editing the maplev
source tree."
  (declare (indent 1) (debug (form body)))
  (let ((name (make-symbol "name")))
    `(let ((,name ,bufname))
       (cl-letf (((symbol-function 'maplev--pmaple-buffer)
                  (lambda () ,name)))
         ,@body))))

(defun org-babel-maple--get-session-buffer (&optional session)
  "Return the buffer of a live pmaple session, starting one if needed.
SESSION is the value of the :session header argument; nil or the
string \"none\" means no session.  Distinct session names map to
distinct pmaple buffers and Maple processes (see
`org-babel-maple--session-buffer-name'); a bare `:session' shares the
interactive \\[pmaple] / `maplev-pmaple-pop-to-buffer' buffer.

The session reuses the maplev pmaple machinery via
`org-babel-maple--with-session-buffer'."
  (unless (or (null session)
              (and (stringp session) (string= session "none")))
    ;; A maplev-config is required so the pmaple buffer is named and
    ;; configured consistently; inherit it from `maplev-config-default'.
    (unless maplev-config (maplev-config))
    (let ((bufname (org-babel-maple--session-buffer-name session)))
      (org-babel-maple--add-session-to-alist session bufname)
      (org-babel-maple--with-session-buffer bufname
        (let ((buffer (process-buffer (maplev--pmaple-process))))
          ;; Wait until Maple has finished booting and is sitting at a
          ;; fresh prompt.  Otherwise the boot prompts land inside the
          ;; first `maplev-pmaple-direct' capture region, desynchronizing
          ;; output.
          (org-babel-maple--wait-for-prompt buffer)
          buffer)))))

(defun org-babel-maple--wait-for-prompt (buffer &optional timeout)
  "Block until the pmaple process in BUFFER is idle at a fresh prompt.
Give up after TIMEOUT seconds (default 30)."
  (with-current-buffer buffer
    (let ((proc (get-buffer-process buffer))
          (deadline (+ (float-time) (or timeout 30))))
      (while (and proc
                  (process-live-p proc)
                  (< (float-time) deadline)
                  (not (save-excursion
                         (goto-char (point-max))
                         (looking-back (regexp-quote maplev-pmaple-prompt)
                                       (max (point-min)
                                            (- (point) (1+ (length maplev-pmaple-prompt))))))))
        (accept-process-output proc 0.05)))))

(defun org-babel-maple--expand-session-body (body params)
  "Expand BODY for session evaluation, prepending variable assignments.
PARAMS is the src-block header alist.  Unlike
`org-babel-expand-body:maple', this never wraps BODY in the
`value' result-type redirection, since sessions capture printed
output."
  (concat (org-babel-variable-assignments:maple params) "\n" body))

(defun org-babel-maple--evaluate-session (buffer body)
  "Send BODY to the pmaple session BUFFER and return cleaned output.
`maplev-pmaple-direct' looks up its target buffer and process through
`maplev--pmaple-buffer', so override it to BUFFER's name for the call;
otherwise a named session would be sent to the base pmaple buffer."
  (org-babel-maple--with-session-buffer (buffer-name buffer)
    (with-current-buffer buffer
      (org-babel-maple--clean-output (maplev-pmaple-direct body)))))

(defun org-babel-maple--evaluate-session-graphics (buffer body file params)
  "Run BODY in session BUFFER, export its last plot to FILE, return FILE.
BODY is sent first (its last value becomes Maple's `%'), then a second
send exports that value to FILE via `org-babel-maple--plot-export-string'.
FILE (the raw :file header) is returned so Org inserts an image link;
PARAMS supplies the plot dimensions."
  (org-babel-maple--with-session-buffer (buffer-name buffer)
    (with-current-buffer buffer
      (maplev-pmaple-direct body)
      (maplev-pmaple-direct (org-babel-maple--plot-export-string file params))))
  file)

(defun org-babel-maple--clean-output (output)
  "Return OUTPUT stripped of pmaple prompt strings, ANSI codes, and stray CRs.
`maplev-pmaple-direct' normally returns clean text already; this is
defensive so the result is well-formed under an interactive PTY too."
  (when output
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (re-search-forward "\e\\[[0-9;]*m" nil t) (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "\r+" nil t) (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward (concat "^" (regexp-quote maplev-pmaple-prompt)) nil t)
        (replace-match ""))
      (string-trim (buffer-string)))))

(defun org-babel-expand-body:maple (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((body (concat
               (org-babel-variable-assignments:maple params)
               "\n" body "\n")))
    (if (eq 'value (cdr (assoc :result-type params)))
        (setq body (concat
		    "try\n"
                    "    writeto(\"babel.out\"):\n"
                    body
		    "_val := %:\n"
		    "catch:\n"
		    "    _val := StringTools:-FormatMessage(lastexception[2..-1]);\n"
		    "finally\n"
                    "    writeto('terminal'):\n"
		    "end try;\n"
                    "_val;")))
    body))

(defun org-babel-maple--var-to-maple (var)
  "Convert VAR to a Maple expression."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-maple--var-to-maple var ", ") "]")
    (format "%S" (if (stringp var) (substring-no-properties var) var))))

(defun org-babel-variable-assignment:maple (param)
  "Return a string of a Maple assignment to a variable.
When PARAM has the form `(:var VAR . VALUE)', the string `VAR := VALUE'
is returned."
  (if (eq (car param) :var)
      (format "%s := %s;"
              (cadr param)
              (org-babel-maple--var-to-maple (cddr param)))))

(defun org-babel-variable-assignments:maple (params)
  "Return a string of Maple statements assigning the header variables.
Elements of PARAMS with the format \(:var VAR . VALUE\) are
converted to Maple assignments `VAR := VALUE'."
  (mapconcat
   #'org-babel-variable-assignment:maple
   params "\n"))

;; session functions

(defvar org-babel-maple--session-alist nil
  "Association list mapping session to pmaple buffer.")

(defun org-babel-maple--add-session-to-alist (session bufname)
  "Add \(SESSION . BUFNAME\) to `org-babel-maple--session-alist' and return BUFNAME.
If BUFNAME is nil, remove it from the alist."
  (setf (alist-get session org-babel-maple--session-alist nil 'remove #'equal) bufname))

(defun org-babel-maple--get-sessions ()
  "Return the list of sessions in `org-babel-maple--session-alist'."
  (mapcar #'car org-babel-maple--session-alist))

(defun org-babel-maple--kill-session (session)
  "Kill SESSION, along with its assocated process and buffer.
Query for SESSION."
  (interactive (list (completing-read "Choose session: " (org-babel-maple--get-sessions)
                                      nil t
                                      (caar org-babel-maple--session-alist))))
  (let ((bufname (cdr (assoc session org-babel-maple--session-alist))))
    (when bufname
      (let ((kill-buffer-query-functions nil)) ; prevent query when killing the buffer and process
        (kill-buffer bufname)
        (setq org-babel-maple--session-alist (assoc-delete-all session org-babel-maple--session-alist))))))

(defun org-babel-maple--kill-all-sessions ()
  "Kill all sessions."
  (interactive)
  (dolist (element org-babel-maple--session-alist)
    (org-babel-maple--kill-session (car element))))

;; Bind a few keys

;; These work, but not sure the choices.  They don't conflict with the
;; standard org-babel-map.  I'd rather bind k to
;; org-babel-maple--kill-session (k means kill one, K means kill all)
;; but currently k is bound to org-babel-remove-result-one-or-many.

(define-key org-babel-map (kbd "S") 'org-babel-maple--kill-session)
(define-key org-babel-map (kbd "K") 'org-babel-maple--kill-all-sessions)

(provide 'ob-maple)

;;; ob-maple.el ends here
