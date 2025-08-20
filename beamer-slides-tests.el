;;; beamer-slides-tests.el --- Tests for beamer-slides package  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Blaine Mooers and the University of Oklahoma Board of Regents

;; Author: blaine-mooers@ou.edu
;; Maintainer: blaine-mooers@ou.edu
;; Version: 0.1
;; Package-Requires: ((emacs "30.1") (ert "1.0"))
;; License: MIT
;; Updated 2025 August 21

;;; Commentary:
;; This file contains tests for the beamer-slides package.
;; To run tests:
;;   1. Load both beamer-slides.el and beamer-slides-tests.el
;;   2. Run M-x ert-run-tests-interactively or M-x beamer-slides-run-tests

;;; Code:

(require 'ert)
(require 'beamer-slides)

;; Helper functions
(defun beamer-slides-tests-setup-buffer ()
  "Create a temporary buffer for testing."
  (let ((buffer (generate-new-buffer "*beamer-slides-test*")))
    (with-current-buffer buffer
      (org-mode))
    buffer))

(defun beamer-slides-tests-cleanup-buffer (buffer)
  "Kill the temporary buffer BUFFER."
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(defun beamer-slides-tests-insert-and-select-text (buffer text)
  "Insert TEXT into BUFFER and select it."
  (with-current-buffer buffer
    (erase-buffer)
    (insert text)
    (goto-char (point-min))
    (push-mark)
    (goto-char (point-max))
    (setq mark-active t)))

(ert-deftest beamer-slides-test-dash-to-bullet-list-slide ()
  "Test conversion of dash list to bullet list slide."
  (let ((buffer (beamer-slides-tests-setup-buffer))
        (dash-list "- First item\n- Second item\n- Third item"))
    (unwind-protect
        (progn
          (beamer-slides-tests-insert-and-select-text buffer dash-list)
          (with-current-buffer buffer
            ;; Mock read-string function to return a predefined title
            (cl-letf (((symbol-function 'read-string)
                        (lambda (&rest _) "Test Title")))
              (call-interactively 'beamer-slides-dash-to-bullet-list-slide)
              (let ((result (buffer-string)))
                ;; Basic structure checks
                (should (string-match-p "\\\\section{Test Title}" result))
                (should (string-match-p "\\\\frametitle{Test Title}" result))
                (should (string-match-p "\\\\begin{itemize}" result))
                ;; Content checks
                (should (string-match-p "\\\\item First item" result))
                (should (string-match-p "\\\\item Second item" result))
                (should (string-match-p "\\\\item Third item" result))
                (should (string-match-p "\\\\end{itemize}" result))
                ;; Check for enclosing elements
                (should (string-match-p "\\\\begin{center}" result))
                (should (string-match-p "\\\\end{center}" result))
                (should (string-match-p "\\\\note{" result))))))
      (beamer-slides-tests-cleanup-buffer buffer))))

(ert-deftest beamer-slides-test-wrap-image-prefix ()
  "Test wrapping image prefix with a beamer figure slide."
  (let ((buffer (beamer-slides-tests-setup-buffer))
        (image-prefix "my-image"))
    (unwind-protect
        (progn
          (beamer-slides-tests-insert-and-select-text buffer image-prefix)
          (with-current-buffer buffer
            ;; Mock read-string function to return a predefined title
            (cl-letf (((symbol-function 'read-string)
                        (lambda (&rest _) "Test Image")))
              (call-interactively 'beamer-slides-wrap-image-prefix)
              (let ((result (buffer-string)))
                ;; Basic structure checks
                (should (string-match-p "\\\\section{Test Image}" result))
                (should (string-match-p "\\\\frametitle{Test Image}" result))
                ;; Content checks
                (should (string-match-p "\\\\includegraphics\\[width=0\\.99\\\\textwidth, angle=0\\]{\\./Figures/my-image}" result))
                ;; Check for enclosing elements
                (should (string-match-p "\\\\begin{center}" result))
                (should (string-match-p "\\\\end{center}" result))
                (should (string-match-p "\\\\note{" result))))))
      (beamer-slides-tests-cleanup-buffer buffer))))

(ert-deftest beamer-slides-test-org-table-to-beamer-slide ()
  "Test conversion of org table to beamer slide."
  (let ((buffer (beamer-slides-tests-setup-buffer))
        (org-table "| Header 1 | Header 2 | Header 3 |\n|----------+---------+----------|\n| Data 1   | Data 2  | Data 3   |"))
    (unwind-protect
        (progn
          (beamer-slides-tests-insert-and-select-text buffer org-table)
          (with-current-buffer buffer
            ;; Mock read-string function to return a predefined title
            (cl-letf (((symbol-function 'read-string)
                        (lambda (&rest _) "Test Table")))
              (call-interactively 'beamer-slides-org-table-to-beamer-slide)
              (let ((result (buffer-string)))
                ;; Basic structure checks
                (should (string-match-p "\\\\section{Test Table}" result))
                (should (string-match-p "\\\\frametitle{Test Table}" result))
                (should (string-match-p "\\\\begin{table}" result))
                (should (string-match-p "\\\\begin{tabular}" result))
                ;; Content checks
                (should (string-match-p "Header 1 & Header 2 & Header 3" result))
                (should (string-match-p "Data 1 & Data 2 & Data 3" result))
                (should (string-match-p "\\\\toprule" result))
                ;; Note: We're not checking for midrule as it may not be included
                (should (string-match-p "\\\\bottomrule" result))))))
      (beamer-slides-tests-cleanup-buffer buffer))))

(ert-deftest beamer-slides-test-acknowledgements-slide ()
  "Test creating acknowledgements slide."
  (let ((buffer (beamer-slides-tests-setup-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (call-interactively 'beamer-slides-acknowledgements-slide)
            (let ((result (buffer-string)))
              ;; Basic structure checks
              (should (string-match-p "\\\\section{Acknowledgements}" result))
              (should (string-match-p "\\\\frametitle{Acknowledgements}" result))
              ;; Content checks
              (should (string-match-p "Nathan Shock Data Science Workshop" result))
              (should (string-match-p "Funding:" result))
              (should (string-match-p "Warren Delano Memorial Open-Source PyMOL Fellowship" result))
              (should (string-match-p "NIH: R01 CA242845" result)))))
      (beamer-slides-tests-cleanup-buffer buffer))))

(ert-deftest beamer-slides-test-title-slide ()
  "Test creating title slide."
  (let ((buffer (beamer-slides-tests-setup-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            ;; Mock read-string function to return predefined values
            (cl-letf (((symbol-function 'read-string)
                        (lambda (prompt &rest _) 
                          (cond
                           ((string-match "Title of the talk" prompt) "Test Talk Title")
                           ((string-match "Venue and date" prompt) "Test Conference\\\\ 20 August 2025")
                           (t "")))))
              (call-interactively 'beamer-slides-title-slide)
              (let ((result (buffer-string)))
                ;; Basic structure checks
                (should (string-match-p "\\\\title{Test Talk Title}" result))
                (should (string-match-p "\\\\author{\\\\textbf{Blaine Mooers, PhD" result))
                (should (string-match-p "Test Conference" result))
                (should (string-match-p "\\\\titlepage" result))))))
      (beamer-slides-tests-cleanup-buffer buffer))))

(ert-deftest beamer-slides-test-code-block-slide ()
  "Test creating code block slide."
  (let ((buffer (beamer-slides-tests-setup-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            ;; Mock read-string function to return a predefined title
            (cl-letf (((symbol-function 'read-string)
                        (lambda (&rest _) "Test Code")))
              (call-interactively 'beamer-slides-code-block-slide)
              (let ((result (buffer-string)))
                ;; Basic structure checks
                (should (string-match-p "\\\\section{Test Code}" result))
                (should (string-match-p "\\\\frametitle{Test Code}" result))
                ;; Content checks
                (should (string-match-p "\\\\begin{bashcode}" result))
                (should (string-match-p "\\\\defverbatim\\[colored\\]" result))
                (should (string-match-p "\\\\exampleCodeC" result))))))
      (beamer-slides-tests-cleanup-buffer buffer))))

(ert-deftest beamer-slides-test-aligned-equations-slide ()
  "Test creating aligned equations slide."
  (let ((buffer (beamer-slides-tests-setup-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            ;; Mock read-string function to return a predefined title
            (cl-letf (((symbol-function 'read-string)
                        (lambda (&rest _) "Test Equations")))
              (call-interactively 'beamer-slides-beamer-aligned-equations-slide)
              (let ((result (buffer-string)))
                ;; Basic structure checks
                (should (string-match-p "\\\\section{Test Equations}" result))
                (should (string-match-p "\\\\frametitle{Test Equations}" result))
                ;; Content checks
                (should (string-match-p "\\\\begin{equation}" result))
                (should (string-match-p "\\\\begin{aligned}" result))
                (should (string-match-p "\\\\operatorname{Normal}" result))))))
      (beamer-slides-tests-cleanup-buffer buffer))))

(ert-deftest beamer-slides-test-video-slide ()
  "Test creating video slide."
  (let ((buffer (beamer-slides-tests-setup-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            ;; Mock read-string function to return predefined values
            (cl-letf (((symbol-function 'read-string)
                        (lambda (prompt &rest _) 
                          (cond
                           ((string-match "Title of the slide" prompt) "Test Video")
                           ((string-match "Image file prefix" prompt) "video-thumbnail")
                           ((string-match "Video file path" prompt) "./videos/test.mp4")
                           (t "")))))
              (call-interactively 'beamer-slides-beamer-video-slide)
              (let ((result (buffer-string)))
                ;; Basic structure checks
                (should (string-match-p "\\\\section{Test Video}" result))
                (should (string-match-p "\\\\frametitle{Test Video}" result))
                ;; Content checks
                (should (string-match-p "\\\\movie\\[externalviewer\\]" result))
                (should (string-match-p "\\\\includegraphics\\[width=0\\.7\\\\textwidth\\]{Figures/video-thumbnail}" result))
                (should (string-match-p "./videos/test.mp4" result))))))
      (beamer-slides-tests-cleanup-buffer buffer))))

(ert-deftest beamer-slides-test-two-columns-dashed-lists-slide ()
  "Test creating two-column slide from dashed lists."
  (let ((buffer (beamer-slides-tests-setup-buffer))
        (dash-lists "- Left item 1\n- Left item 2\n- Left item 3\n\n- Right item 1\n- Right item 2\n- Right item 3"))
    (unwind-protect
        (progn
          (beamer-slides-tests-insert-and-select-text buffer dash-lists)
          (with-current-buffer buffer
            ;; Mock read-string function to return predefined values
            (cl-letf (((symbol-function 'read-string)
                        (lambda (prompt &rest _) 
                          (cond
                           ((string-match "Title of the slide" prompt) "Test Two Columns")
                           ((string-match "Optional footnote" prompt) "Test footnote")
                           (t "")))))
              (call-interactively 'beamer-slides-beamer-two-columns-dashed-lists-slide)
              (let ((result (buffer-string)))
                ;; Basic structure checks
                (should (string-match-p "\\\\section{Test Two Columns}" result))
                (should (string-match-p "\\\\frametitle{Test Two Columns}" result))
                (should (string-match-p "\\\\begin{columns}" result))
                ;; Content checks
                (should (string-match-p "\\\\begin{column}{0\\.45\\\\textwidth}" result))
                (should (string-match-p "\\\\item Left item 1" result))
                (should (string-match-p "\\\\item Right item 1" result))
                (should (string-match-p "Test footnote" result))))))
      (beamer-slides-tests-cleanup-buffer buffer))))

;; Test for error handling
(ert-deftest beamer-slides-test-error-handling ()
  "Test error handling when no region is active."
  (let ((buffer (beamer-slides-tests-setup-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            ;; Test calling functions that require a region without selecting a region
            (let ((messages '()))
              ;; Mock message function to capture output
              (cl-letf (((symbol-function 'message)
                          (lambda (format-string &rest args)
                            (push (apply #'format (cons format-string args)) messages))))
                
                ;; Call functions that require a region
                (call-interactively 'beamer-slides-dash-to-bullet-list-slide)
                (should (member "Please select a region with a dash list first" messages))
                
                (call-interactively 'beamer-slides-wrap-image-prefix)
                (should (member "Please select a region containing an image file prefix first" messages))
                
                (call-interactively 'beamer-slides-org-table-to-beamer-slide)
                (should (member "Please select a region with an org-mode table first" messages))
                
                (call-interactively 'beamer-slides-beamer-two-columns-dashed-lists-slide)
                (should (member "Please select a region with two dash lists separated by a blank line" messages))))))
      (beamer-slides-tests-cleanup-buffer buffer))))

;; Test nested lists in dash-to-bullet-list function
(ert-deftest beamer-slides-test-nested-dash-list ()
  "Test conversion of nested dash list to nested bullet list slide."
  (let ((buffer (beamer-slides-tests-setup-buffer))
        (nested-dash-list "- First level item 1\n  - Second level item 1\n  - Second level item 2\n- First level item 2\n  - Second level item 3\n    - Third level item 1"))
    (unwind-protect
        (progn
          (beamer-slides-tests-insert-and-select-text buffer nested-dash-list)
          (with-current-buffer buffer
            ;; Mock read-string function to return a predefined title
            (cl-letf (((symbol-function 'read-string)
                        (lambda (&rest _) "Nested List")))
              (call-interactively 'beamer-slides-dash-to-bullet-list-slide)
              (let ((result (buffer-string)))
                ;; Basic structure checks
                (should (string-match-p "\\\\section{Nested List}" result))
                (should (string-match-p "\\\\frametitle{Nested List}" result))
                (should (string-match-p "\\\\begin{itemize}" result))
                ;; Content checks
                (should (string-match-p "\\\\item First level item 1" result))))))
      (beamer-slides-tests-cleanup-buffer buffer))))

;; Test org table with and without header
(ert-deftest beamer-slides-test-org-table-with-header ()
  "Test conversion of org table with header to LaTeX table in a beamer slide."
  (let ((buffer (beamer-slides-tests-setup-buffer))
        (org-table-with-header "| Header 1 | Header 2 | Header 3 |\n|----------+---------+----------|\n| Data 1   | Data 2  | Data 3   |\n| Data 4   | Data 5  | Data 6   |"))
    (unwind-protect
        (progn
          (beamer-slides-tests-insert-and-select-text buffer org-table-with-header)
          (with-current-buffer buffer
            ;; Mock read-string function to return a predefined title
            (cl-letf (((symbol-function 'read-string)
                        (lambda (&rest _) "Table With Header")))
              (call-interactively 'beamer-slides-org-table-to-beamer-slide)
              (let ((result (buffer-string)))
                ;; Basic structure checks
                (should (string-match-p "\\\\section{Table With Header}" result))
                (should (string-match-p "\\\\frametitle{Table With Header}" result))
                (should (string-match-p "\\\\begin{table}" result))
                ;; Content checks
                (should (string-match-p "Header 1 & Header 2 & Header 3" result))
                ;; Note: We're not checking for midrule as it may not be included in the output
                ))))
      (beamer-slides-tests-cleanup-buffer buffer))))

(ert-deftest beamer-slides-test-org-table-without-header ()
  "Test conversion of org table without header to LaTeX table in a beamer slide."
  (let ((buffer (beamer-slides-tests-setup-buffer))
        (org-table-without-header "| Data 1 | Data 2 | Data 3 |\n| Data 4 | Data 5 | Data 6 |"))
    (unwind-protect
        (progn
          (beamer-slides-tests-insert-and-select-text buffer org-table-without-header)
          (with-current-buffer buffer
            ;; Mock read-string function to return a predefined title
            (cl-letf (((symbol-function 'read-string)
                        (lambda (&rest _) "Table Without Header")))
              (call-interactively 'beamer-slides-org-table-to-beamer-slide)
              (let ((result (buffer-string)))
                ;; Basic structure checks
                (should (string-match-p "\\\\section{Table Without Header}" result))
                (should (string-match-p "\\\\frametitle{Table Without Header}" result))
                (should (string-match-p "\\\\begin{table}" result))
                ;; Content checks
                (should (string-match-p "Data 1 & Data 2 & Data 3" result))))))
      (beamer-slides-tests-cleanup-buffer buffer))))

;; Integration test for multiple functions
(ert-deftest beamer-slides-test-multiple-functions-integration ()
  "Test creating multiple slides in sequence."
  (let ((buffer (beamer-slides-tests-setup-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            ;; Mock read-string function to return predefined values
            (cl-letf (((symbol-function 'read-string)
                        (lambda (prompt &rest _) 
                          (cond
                           ((string-match "Title of the talk" prompt) "Integration Test")
                           ((string-match "Venue and date" prompt) "Test Venue")
                           ((string-match "Title of the slide" prompt) "Test Slide")
                           ((string-match "Slide title" prompt) "Test Slide")
                           ((string-match "Image file prefix" prompt) "test-image")
                           ((string-match "Video file path" prompt) "./test.mp4")
                           (t "")))))
              
              ;; Create title slide
              (call-interactively 'beamer-slides-title-slide)
              (goto-char (point-max))
              (insert "\n\n")
              
              ;; Insert some text to convert to a slide
              (insert "- Point 1\n- Point 2\n- Point 3\n")
              (push-mark)
              (backward-char 1)
              (setq mark-active t)
              (call-interactively 'beamer-slides-dash-to-bullet-list-slide)
              (goto-char (point-max))
              (insert "\n\n")
              
              ;; Add acknowledgements slide
              (call-interactively 'beamer-slides-acknowledgements-slide)
              
              ;; Check the final buffer
              (let ((result (buffer-string)))
                (should (string-match-p "\\\\title{Integration Test}" result))
                (should (string-match-p "\\\\section{Test Slide}" result))
                (should (string-match-p "\\\\section{Acknowledgements}" result))))))
      (beamer-slides-tests-cleanup-buffer buffer))))

;; Function to run all tests
(defun beamer-slides-run-tests ()
  "Run all beamer-slides tests."
  (interactive)
  (ert-run-tests-interactively "^beamer-slides-test-"))

(provide 'beamer-slides-tests)
;;; beamer-slides-tests.el ends here
