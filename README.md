![Version](https://img.shields.io/static/v1?label=beamer-slides-el&message=0.7&color=brightcolor)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
# Library of elisp functions for creating Beamer slides inside Emacs

This is a library of custom functions designed to accelerate the slide-by-slide assembly of a beamer presentation for scientific talks.
You are promoted to add the title of the slide, which is also used for the section title.
The prompt is left in the notes section to promote recording some metadata about the slide.
Some functions wrap the slide around an org-mode list or the name of a figure.

## Installation with use-package and straight
Add this to your `init.el` file and reload Emacs or evaluate in the scratch buffer.
Straight will `git clone` this repo and store it in the `repos` subfolder of your `.emacs.d` folder.
```elisp
(use-package beamer-slides
  :straight
  '(:type git
    :repo "https://github.com/MooersLab/beamer-slides-el.git"
    :files ("mooerslab.el")))
```
The functions will always be available.

## Usage
If you have the package **vertico** installed, enter `M-x beamer-slides-` to see a list of functions in the minibuffer.
If you have installed the package **marginalia**, you will also see the document string's first line describing what the function does.
Use `C-n` repeatedly to navigate downward to the function that you want to select and execute.

## Alternate approach of loading the package
Add this function to your `init.el` file to load the file of home-made functions manually.
Edit the file path to customize to your system:
```elisp
;;;## beamer-slides-load
;; Inspried https://sachachua.com/dotemacs/index.html#org4dd39d0
(defun beamer-slides-load ()
  "Load beamer-slides.el file."
  (interactive)
  (let ((file-path "~/6112MooersLabGitHubLabRepos/beamer-slides/beamer-slides.el"))
    (if (file-exists-p (expand-file-name file-path))
        (load-file file-path)
      (message "Cannot find beamer-slides.el file"))))
```
Enter `M-x beamer-slides-load` to load the functions.

## Available Functions

| Function | Description |
|----------|-------------|
| `beamer-slides-dash-to-bullet-list-slide` | Convert an org-mode dash list to a complete beamer slide with section |
| `beamer-slides-wrap-image-prefix` | Wrap the image filename prefix in the current region with a beamer figure slide |
| `beamer-slides-org-table-to-beamer-slide` | Convert an org-mode table to a LaTeX table in a beamer slide |
| `beamer-slides-acknowledgements-slide` | Insert a beamer slide for acknowledgements with the standard format |
| `beamer-slides-title-slide` | Insert a beamer title slide with the standard format |
| `beamer-slides-code-block-slide` | Insert a beamer slide with a code block using the standard format |
| `beamer-slides-beamer-aligned-equations-slide` | Insert a beamer slide with aligned equations using the standard format |
| `beamer-slides-beamer-video-slide` | Insert a beamer slide with a video element using the standard format |
| `beamer-slides-beamer-two-columns-dashed-lists-slide` | Convert two org-mode dashed lists to a two-column beamer slide |

## Status
Passed 14 tests. Plan to add more advanced functions soon.
## Update history
|Version      | Changes                                                                                                                                  | Date                |
|:------------|:-----------------------------------------------------------------------------------------------------------------------------------------|:--------------------|
| Version 0.1 |   Added badges, funding, and update table. Initial commit.                                                                              | 2025 August 20   |
## Sources of funding
- NIH: R01 CA242845
- NIH: R01 AI088011
- NIH: P30 CA225520 (PI: R. Mannel)
- NIH: P20 GM103640 and P30 GM145423 (PI: A. West)

