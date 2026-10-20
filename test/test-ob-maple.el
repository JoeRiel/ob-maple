;;; test-ob-maple.el --- tests for ob-maple.el  -*- lexical-binding: t -*-

;; Copyright (c) 2026 Joseph S. Riel
;; Author: Joseph S. Riel

;;; Commentary:

;;; Code:


(add-to-list 'load-path (file-name-concat "~/org/org-mode/testing/"))
(require 'org-test)
(require 'maplev)
(require 'cl-lib)

;; Simple test

(ert-deftest ob-maple/simple ()
  "Writes the output."
  (cl-letf ((org-confirm-babel-evaluate
             (lambda (lang body)
               (not (member lang '("maple"))))))
    (org-test-with-temp-text "
#+begin_src maple :results value
\"hello\";
#+end_src"
    (org-babel-next-src-block)
    (should (string= "\"hello\"\n" (org-babel-execute-src-block))))))


;; Test :var

(ert-deftest ob-maple/var ()
  "Writes the output."
  (cl-letf ((org-confirm-babel-evaluate
             (lambda (lang body)
               (not (member lang '("maple"))))))
    (org-test-with-temp-text "
#+begin_src maple :var K=32 :results value
K^2;
#+end_src"
    (org-babel-next-src-block)
    (should (string= "1024\n" (org-babel-execute-src-block))))))

;; Test :latex

(ert-deftest ob-maple/latex ()
  "Writes the output."
  (cl-letf ((org-confirm-babel-evaluate
             (lambda (lang body)
               (not (member lang '("maple"))))))
    (org-test-with-temp-text "
#+begin_src maple :results output :latex yes
latex(int(g(x),x));
#+end_src"
    (org-babel-next-src-block)
    (should (string= "\\[\\int g \\! \\left(x \\right)d x\\]"
                     (org-babel-execute-src-block))))))


;;; test-ob-maple.el ends here
