;;; beamer-slides.el --- Interactive functions to accelarate the assembly of beamer slideshows  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Blaine Mooers and the University of Oklahoma Board of Regents

;; Author: blaine-mooers@ou.edu
;; Maintainer: blaine-mooers@ou.edu
;; URL: https://github.com/MooersLab/bemaer-slides-el
;; Version: 0.7
;; Keywords: latex, beamer slideshows, slide templates
;; License: MIT
;; Updated 2025 August 20

;;; This package is known to work (insofar as it's tested) with Emacs 30.1.

;;; Commentary:

;;; Code:



(defun beamer-slides-dash-to-bullet-list-slide()
  "Convert an 'org-mode' dash list to a complete beamer slide with section.
This function operates on the selected region, converting org-mode
dash list items into a complete beamer frame with centered itemized list.
It prompts for a title to use in both section and frametitle,
and leaves a prompt inside the note section."
  (interactive)
  (if (not (region-active-p))
      (message "Please select a region with a dash list first")
    (let* ((slide-title (read-string "Slide title: "))
           (region-text (buffer-substring (region-beginning) (region-end)))
           (lines (split-string region-text "\n" t))
           (current-level 0)
           (previous-level 0)
           (item-list "")
           (current-line "")
           (beamer-slide "\\section{%s}\n\\begin{frame}\n  \\frametitle{%s}\n  \\begin{center}\n"))

      ;; Format the section and frame title
      (setq beamer-slide (format beamer-slide slide-title slide-title))

      ;; Add the beginning of the itemize environment
      (setq item-list "\\begin{itemize}[font=$\\bullet$\\scshape\\bfseries]\n")

      ;; Process each line of the region
      (dolist (line lines)
        (setq current-line (string-trim line))
        ;; Check if this is a dash list item and calculate its indentation level
        (when (string-match "^\\(\\s-*\\)-\\s-+\\(.*\\)" current-line)
          (let* ((indent (match-string 1 current-line))
                 (item-content (match-string 2 current-line))
                 (level (/ (length indent) 2))) ;; Assuming 2 spaces per level

            ;; Handle level changes
            (cond
             ;; Going deeper (nested level)
             ((> level current-level)
              (dotimes (_ (- level current-level))
                (setq item-list (concat item-list "    \\begin{itemize}\n"))))

             ;; Going up (ending nested levels)
             ((< level current-level)
              (dotimes (_ (- current-level level))
                (setq item-list (concat item-list "    \\end{itemize}\n")))))

            ;; Add indentation based on current level
            (setq item-list
                  (concat item-list
                          (make-string (* (+ 1 level) 4) ? ) ;; 4 spaces per level + base indent
                          "\\item "
                          item-content
                          "\n"))

            ;; Update current level
            (setq previous-level current-level)
            (setq current-level level))))

      ;; Close any remaining open itemize environments
      (dotimes (_ (+ 1 current-level))  ;; +1 for the base level
        (if (= _ current-level)
            (setq item-list (concat item-list "\\end{itemize}"))
          (setq item-list (concat item-list "    \\end{itemize}\n"))))

      ;; Complete the slide with center environment and frame closing
      ;; Note contains a prompt for the presenter
      (setq beamer-slide (concat beamer-slide
                                 item-list
                                 "\n\\end{center}\n\\end{frame}"
                                 "\n\\note{\n  Add speaker notes here for " slide-title "...\n  }\n"))

      ;; Replace the region with the new beamer slide
      (delete-region (region-beginning) (region-end))
      (insert beamer-slide))))


(defun beamer-slides-wrap-image-prefix ()
  "Wrap the image filename prefix in the current region with a beamer figure slide.
Takes the selected text as an image file prefix and prompts for a slide title,
then generates a complete beamer frame with a centered figure and note section.
Leaves the cursor inside the note section for immediate editing."
  (interactive)
  (if (not (region-active-p))
      (message "Please select a region containing an image file prefix first")
    (let* ((image-prefix (buffer-substring (region-beginning) (region-end)))
           (slide-title (read-string "Slide title: "))
           (beamer-slide (concat "\\section{" slide-title "}\n"
                                "\\begin{frame}\n"
                                "\\frametitle{" slide-title "}\n"
                                "\\begin{center}\n"
                                "    \\includegraphics[width=0.99\\textwidth, angle=0]{./Figures/" image-prefix "}\n"
                                "\\end{center}\n"
                                "\\end{frame}\n"
                                "\\note{\n"
                                "  Add speaker notes here for " slide-title "...\n"
                                "  }\n")))

      ;; Replace the region with the new beamer slide
      (delete-region (region-beginning) (region-end))
      (insert beamer-slide)
  
      ;; Move cursor to the note section for immediate editing
      (search-backward "Add speaker notes here")
      (beginning-of-line)))))


(defun beamer-slides-org-table-to-beamer-slide ()
  "Convert an org-mode table to a LaTeX table in a beamer slide.
This function operates on the selected region containing an org-mode table,
converts it to a LaTeX table, and wraps it in a beamer slide with section.
The leftmost column will be left-aligned, while other columns remain centered.
Prompts for slide title to use in both section and frametitle."
  (interactive)
  (if (not (region-active-p))
      (message "Please select a region with an org-mode table first")
    (let* ((slide-title (read-string "Slide title: "))
           (region-text (buffer-substring (region-beginning) (region-end)))
           (org-table-lines (split-string region-text "\n" t))
           (has-header nil)
           (column-count 0)
           (latex-table "")
           (beamer-slide "\\section{%s}\n\\begin{frame}\n  \\frametitle{%s}\n  \\begin{center}\n"))

      ;; Format the section and frame title
      (setq beamer-slide (format beamer-slide slide-title slide-title))

      ;; Start LaTeX table
      (setq latex-table "\\begin{table}\n  \\centering\n  \\begin{tabular}{")

      ;; Determine number of columns and if there's a header
      (when org-table-lines
        (let ((first-line (car org-table-lines)))
          (when (string-match-p "^\\s-*|[-+]+|\\s-*$" first-line)
            (setq has-header t))

          ;; Count columns from first content line
          (let ((content-line (if has-header
                                (if (> (length org-table-lines) 2)
                                    (nth 2 org-table-lines)
                                  (car org-table-lines))
                                (car org-table-lines))))
            (setq column-count (- (length (split-string content-line "|" t)) 0)))))

      ;; Add column specifiers - leftmost is 'l', others are 'c'
      (setq latex-table (concat latex-table "l")) ;; First column left-aligned
      (dotimes (_ (1- column-count))
        (setq latex-table (concat latex-table "c"))) ;; Remaining columns centered
      (setq latex-table (concat latex-table "}\n    \\toprule\n"))

      ;; Process table rows
      (let ((is-header t)
            (header-done nil)
            (skip-line nil))
        (dolist (line org-table-lines)
          (setq skip-line nil)

          ;; Skip separator lines
          (when (string-match-p "^\\s-*|[-+]+|\\s-*$" line)
            (setq skip-line t))

          (unless skip-line
            ;; Process table row
            (let* ((cells (split-string line "|" t))
                   (trimmed-cells (mapcar #'string-trim cells))
                   (latex-row "    "))

              ;; Build the row
              (let ((cell-index 0))
                (dolist (cell trimmed-cells)
                  (setq latex-row (concat latex-row cell))
                  (setq cell-index (1+ cell-index))
                  (when (< cell-index (length trimmed-cells))
                    (setq latex-row (concat latex-row " & ")))))

              ;; Add the row to the table
              (setq latex-table (concat latex-table latex-row " \\\\\n"))

              ;; Add midrule after header
              (when (and is-header has-header (not header-done))
                (setq latex-table (concat latex-table "    \\midrule\n"))
                (setq header-done t)
                (setq is-header nil))))))

      ;; Complete the LaTeX table
      (setq latex-table (concat latex-table "    \\bottomrule\n  \\end{tabular}\n"))

      ;; Close the table environment
      (setq latex-table (concat latex-table "\\end{table}\n"))

      ;; Complete the slide
      (setq beamer-slide (concat beamer-slide
                                 latex-table
                                 "\\end{center}\n\\end{frame}"
                                 "\n\\note{\n  Add speaker notes here for " slide-title "...\n  }\n"))

      ;; Replace the region with the new beamer slide
      (delete-region (region-beginning) (region-end))
      (insert beamer-slide))))


(defun beamer-slides-acknowledgements-slide ()
  "Insert a beamer slide for acknowledgements with the standard format.
Creates a section, frame, and formatted itemized lists for people
and funding sources."
  (interactive)
  (let ((beamer-slide "\\section{Acknowledgements}
\\begin{frame}
\\frametitle{Acknowledgements}
\\Large{
\\begin{itemize}[font=$\\bullet$\\scshape\\bfseries]
    \\item Nathan Shock Data Science Workshop
\\end{itemize}
\\vspace{2mm}
Funding:
\\begin{itemize}[font=$\\bullet$\\scshape\\bfseries]
    \\item Warren Delano Memorial Open-Source PyMOL Fellowship
    \\item NIH: R01 CA242845, R01 AI088011
    \\item NIH: P20 GM103640, P30 CA225520, P30 AG050911-07S1
    \\item OCAST HR20-002
    \\item PHF Team Science Grant
\\end{itemize}
}
\\end{frame}
\\note{
  Add speaker notes here for acknowledgements...
}"))

    ;; Insert the beamer slide at current point
    (insert beamer-slide)))


(defun beamer-slides-title-slide ()
  "Insert a beamer title slide with the standard format.
Prompts for the title of the talk and the venue/date information."
  (interactive)
  (let* ((talk-title (read-string "Title of the talk: "))
         (venue-info (read-string "Venue and date (e.g., 'SSRL/LCLS User Meeting\\\\ 25 September 2025'): "))
         (beamer-slide (format "%%title info
\\title{%s}
\\author{\\textbf{Blaine Mooers, PhD \\\\ blaine-mooers@ouhsc.edu \\\\ 405-271-8300}}
\\institute{{Department of Biochemistry \\& Physiology}\\\\[2pt]{University of Oklahoma Health Sciences, Oklahoma City}}
%% to hide auto date,use \\date{}
\\date{%s}
\\begin{document}%% title slide
\\section{Title slide}
{
\\setbeamertemplate{footline}{} %% no page number here
\\frame{
  \\titlepage
  \\note{
} } }
\\note{
7:06
Hi, I am Blaine Mooers.
I will be talking about
I am an associate professor of Biochemistry and Molecular Biology at the University of Oklahoma Health Sciences Center in Oklahoma City.
}" talk-title venue-info))

    ;; Insert the beamer slide at current point
    (insert beamer-slide)

    ;; Move cursor to the end of the first note section
    (search-backward "} } }")
    (search-backward "\\note{")
    (forward-char 7))) ;; Position cursor after "\\note{" for easy editing


(defun beamer-slides-code-block-slide ()
  "Insert a beamer slide with a code block using the standard format.
Prompts for the title of the slide, which is also used for the section.
Positions cursor in the code block for immediate editing."
  (interactive)
  (let* ((slide-title (read-string "Title of the slide: "))
         (beamer-slide (format "%%%%%%%%%%%%%%%%%%%% slide No. %%%%%%%%%%%%%%%%%%%%
\\section{%s}
\\defverbatim[colored]\\exampleCodeC{
\\Large{
\\begin{bashcode}
    Insert code here indented by four spaces
\\end{bashcode}
}
}
\\begin{frame}
\\frametitle{%s}
\\exampleCodeC
\\footnotesize{\\url{https://github.com/clojupyter/clojupyter/blob/master/doc/library.md}}
\\end{frame}
\\note{
  Add speaker notes here for %s...
}" slide-title slide-title slide-title)))

    ;; Insert the beamer slide at current point
    (insert beamer-slide)

    ;; Move cursor to the code block for immediate editing
    (search-backward "Insert code here indented by four spaces")
    (replace-match "")
    (forward-char 4))) ;; Position cursor after indentation


(defun beamer-slides-beamer-aligned-equations-slide ()
  "Insert a beamer slide with aligned equations using the standard format.
Prompts for the title of the slide, which is also used for the section.
Positions cursor in the equation block for immediate editing."
  (interactive)
  (let* ((slide-title (read-string "Title of the slide: "))
         (beamer-slide (format "\\section{%s}
\\begin{frame}
\\frametitle{%s}
\\Large{
\\begin{center}
\\begin{equation}
\\begin{aligned}
y_{i} & \\sim \\operatorname{Normal}\\left(\\mu_{i}, \\sigma\\right) \\\\
\\mu_{i} &=\\alpha+\\beta x_{i} \\\\
\\alpha & \\sim \\operatorname{Normal}(0,100) \\\\
\\beta & \\sim \\operatorname{Normal}(0,1) \\\\
\\sigma & \\sim \\operatorname{Exponential}(1)
\\end{aligned}
\\end{equation}
\\end{center}
}
\\end{frame}
\\note{
  Add speaker notes here for %s...
}" slide-title slide-title slide-title)))

    ;; Insert the beamer slide at current point
    (insert beamer-slide)

    ;; Move cursor to the equation block for immediate editing
    (search-backward "\\begin{aligned}")
    (forward-line 1)
    (beginning-of-line)))


(defun beamer-slides-beamer-video-slide ()
  "Insert a beamer slide with a video element using the standard format.
Prompts for the title of the slide, image file prefix, and video path.
Uses the title for both section and frametitle."
  (interactive)
  (let* ((slide-title (read-string "Title of the slide: "))
         (image-prefix (read-string "Image file prefix (without path/extension): "))
         (video-path (read-string "Video file path (e.g., ./videos/intro212.mov): "))
         (beamer-slide (format "\\section{%s}
\\begin{frame}
\\frametitle{%s}
\\centering
    \\movie[externalviewer]{\\includegraphics[width=0.7\\textwidth]{Figures/%s}}{%s}\\\\
%%\\Large{open ACA abstracts,<open:https://virtual.oxfordabstracts.com/\\#/event/29303/homepage>}
\\end{frame}
\\note{
  Add speaker notes here for %s...
}" slide-title slide-title image-prefix video-path slide-title)))

    ;; Insert the beamer slide at current point
    (insert beamer-slide)

    ;; Move cursor to the note section for immediate editing
    (search-backward "Add speaker notes here")
        (beginning-of-line)))


(defun beamer-slides-beamer-two-columns-dashed-lists-slide ()
  "Convert two org-mode dashed lists to a two-column beamer slide.
Takes two selected regions with org-mode dash lists and converts them
to a two-column beamer slide with itemized lists.
Prompts for the slide title and optional footnote text."
  (interactive)
  (if (not (region-active-p))
      (message "Please select a region with two dash lists separated by a blank line")
    (let* ((slide-title (read-string "Title of the slide: "))
           (footnote-text (read-string "Optional footnote text (leave empty for none): "))
           (region-text (buffer-substring (region-beginning) (region-end)))
           (lists (split-string region-text "\n\n" t))
           (left-list-text (if (>= (length lists) 1) (car lists) ""))
           (right-list-text (if (>= (length lists) 2) (nth 1 lists) ""))
           (left-items "")
           (right-items "")
           (beamer-slide-template "\\section{%s}
\\begin{frame}
\\frametitle{%s}
\\begin{large}
\\begin{columns}
    \\begin{column}{0.45\\textwidth}
        \\begin{itemize}[font=$\\bullet$\\scshape\\bfseries]
%s
        \\end{itemize}
    \\end{column}
    \\begin{column}{0.45\\textwidth}
        \\begin{itemize}[font=$\\bullet$\\scshape\\bfseries]
%s
        \\end{itemize}
    \\end{column}
    \\end{columns}
\\end{large}
    \\vspace{2em}
  %s
\\end{frame}
\\note{
  Add speaker notes here for %s...
}"))
;; Process left column list
(dolist (line (split-string left-list-text "\n" t))
  (when (string-match "^\\s-*-\\s-+\\(.*\\)" line)
    (let ((item-content (match-string 1 line)))
      (setq left-items (concat left-items "            \\item " item-content "\n")))))

;; Process right column list
(dolist (line (split-string right-list-text "\n" t))
  (when (string-match "^\\s-*-\\s-+\\(.*\\)" line)
    (let ((item-content (match-string 1 line)))
      (setq right-items (concat right-items "            \\item " item-content "\n")))))

;; Format the slide
(let ((beamer-slide (format beamer-slide-template
                            slide-title
                            slide-title
                            left-items
                            right-items
                            (or footnote-text "")
                            slide-title)))

  ;; Replace the region with the new beamer slide
  (delete-region (region-beginning) (region-end))
  (insert beamer-slide)

  ;; Move cursor to the note section
  (search-backward "Add speaker notes here")
    (beginning-of-line)))))

(provide 'beamer-slides)
;;; beamer-slides.el ends here
