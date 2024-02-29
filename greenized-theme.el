;;; greenized.el --- The green version of solarized (fork of naysayer color theme)

;; Original Author Nick Aversano <nickav@users.noreply.github.com>
;; Author: Thiago Lopes <thiagolopes@pm.me>
;; Version: 0.1
;; Filename: greenized.el
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/thiagolopes/emacs.d
;; License: GPL-3+

;;; Commentary:
;; The green version of solarized.

;;; Code:
(unless (>= emacs-major-version 24)
  (error "The greenized theme requires Emacs 24 or later!"))

(deftheme greenized "The green solarized color theme.")

;; Solarized colors

(require 'use-package)
(use-package naysayer-theme)
(load-theme 'naysayer)

;; TODO add options to: italic off, comentary style off, modeline border off;

(let ((background "#062329")
      (gutters    "#062329")
      (gutter-fg  "#062329")
      (gutters-active "#062329")
      (builtin      "#ffffff")
      (selection  "#0000ff")
      (text       "#d1b897")
      (comments   "#44b340")
      (punctuation "#8cde94")
      (keywords "#ffffff")
      (variables "#c1d1e3")
      (functions "#ffffff")
      (methods    "#c1d1e3")
      (strings    "#2ec09c")
      (constants "#7ad0c6")
      (macros "#8cde94")
      (numbers "#7ad0c6")
      (white     "#ffffff")
      (error "#ff0000")
      (warning "#ffaa00")
      (highlight-line "#0b3335")
      (line-fg "#126367")
      (black "#000000")

      (border-width '(2 . 2))
      (background-darker "#041b20")
      (smaller 0.9)

      (yellow "#b58900")
      (orange "#cb4b16")
      (red "#dc322f")
      (magenta "#d33682")
      (blue "#268bd2")
      (green "#859900")
      (cyan "#2aa198")
      (violet "#6c71c4"))

  (defcustom greenized-theme-yellow  yellow  "Primary colors - yellow"  :type 'string :group 'solarized)
  (defcustom greenized-theme-orange  orange  "Primary colors - orange"  :type 'string :group 'solarized)
  (defcustom greenized-theme-red     red     "Primary colors - red"     :type 'string :group 'solarized)
  (defcustom greenized-theme-magenta magenta "Primary colors - magenta" :type 'string :group 'solarized)
  (defcustom greenized-theme-blue    blue    "Primary colors - blue"    :type 'string :group 'solarized)
  (defcustom greenized-theme-green   green   "Primary colors - green"   :type 'string :group 'solarized)
  (defcustom greenized-theme-cyan    cyan    "Primary colors - cyan"    :type 'string :group 'solarized)

  (custom-theme-set-faces
   'greenized
   ;; *****************************************   CHANGES START HERE   *****************************************
   ;; Error, Warning, Info, Success
   `(error ((t (:foreground ,red :height ,smaller))))
   `(warning ((t (:foreground ,yellow :height ,smaller))))
   `(info ((t (:foreground ,magenta :height ,smaller))))
   `(success ((t (:foreground ,green :height ,smaller))))
   `(tooltip ((t (:foreground ,white :background ,line-fg))))

   ;; Company
   `(company-tooltip ((t (:foreground ,white :background ,line-fg))))
   ;; `(company-scrollbar-fg ((t (:background ,strings))))
   ;; `(company-scrollbar-bg ((t (:background ,text))))
   `(company-tooltip-annotation-selection ((t (:foreground ,white :background ,selection))))
   `(company-tooltip-selection ((t (:foreground ,text :background ,background-darker))))
   `(company-tooltip-common ((t (:foreground ,white))))

   ;; CTRLF
   `(ctrlf-highlight-active ((t (:background ,selection :foreground ,white))))
   `(ctrlf-highlight-passive ((t (:background ,background-darker :foreground ,strings))))

   ;; Swipper
   `(swiper-line-face ((t (:background ,background-darker))))
   `(swiper-match-face-2 ((t (:background ,strings :foreground ,black))))

   ;; Font lock
   `(font-lock-keyword-face ((t (:foreground ,keywords :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,constants))))
   `(font-lock-builtin-face ((t (:foreground ,builtin :slant italic))))
   `(font-lock-comment-face ((t (:foreground ,comments :height ,smaller :background ,background-darker))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comments :height ,smaller :background ,background-darker))))
   ;; `(font-lock-type-face              ((t (:foreground ,punctuation))))
   ;; `(font-lock-variable-name-face     ((t (:foreground ,variables))))
   ;; `(font-lock-string-face            ((t (:foreground ,strings))))
   ;; `(font-lock-doc-face               ((t (:foreground ,comments))))
   ;; `(font-lock-function-name-face     ((t (:foreground ,functions))))
   ;; `(font-lock-doc-string-face        ((t (:foreground ,strings))))
   ;; `(font-lock-preprocessor-face      ((t (:foreground ,macros))))
   ;; `(font-lock-warning-face           ((t (:foreground ,warning))))

   ;; Flycheck
   `(flycheck-posframe-background-face ((t (:background ,background-darker))))
   `(flycheck-posframe-border-face ((t (:foreground ,background-darker))))
   `(flycheck-inline-error ((t (:background ,background-darker))))
   `(flycheck-inline-warning ((t (:background ,background-darker))))
   `(flycheck-inline-info ((t (:background ,background-darker))))
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,red)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,red :weight normal :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yellow)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:forground ,yellow :weight normal :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,green)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:forground ,green :weight normal :underline t))))

   ;; Flymake
   `(flymake-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,red)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,red :weight normal :underline t))))
   `(flymake-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yellow)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:forground ,yellow :weight normal :underline t))))
   `(flymake-note
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,green)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:forground ,green :weight normal :underline t))))

   ;; bm
   `(bm-persistent-face ((t (:background ,background-darker))))

   ;; mode-line
   `(mode-line ((t (:inverse-video unspecified
                                   :underline unspecified
                                   :foreground ,background
                                   :background ,text
                                   :box (:line-width ,border-width
                                                     :style released-button)))))
   `(mode-line-inactive ((t (:inverse-video unspecified
                                            :underline unspecified
                                            :foreground ,text
                                            :background ,background-darker
                                            :box (:line-width ,border-width
                                                              :style released-button
                                                              :foreground ,text)))))

   `(cursor ((t (:background ,punctuation))))
   `(which-func ((t (:inverse-video unspecified
                                    :underline unspecified
                                    :foreground ,background
                                    :weight bold
                                    :box (:line-width ,border-width
                                                      :style released-button
                                                      :foreground ,text)))))
   `(show-paren-match ((t (:background nil
                                       :foreground ,white
                                       :bold t
                                       :inverse-video nil))))
   `(solaire-default-face ((t (:background ,background-darker))))
   `(symbol-overlay-default-face ((t (:bold t))))
   `(whitespace-space ((t (:background ,background :foreground ,line-fg))))

   ;; Ido
   `(ido-first-match ((t (:foreground ,white :bold t))))

   ;; LSP
   `(lsp-face-highlight-textual ((t (:background ,background-darker))))

   ;; Pulsar
   `(pulsar-generic ((t (:background ,selection))))
   `(pulsar-red ((t (:background ,red))))
   `(pulsar-gree ((t (:background ,green))))
   `(pulsar-yellow ((t (:background ,yellow))))
   `(pulsar-blue ((t (:background ,blue))))
   `(pulsar-magenta ((t (:background ,magenta))))
   `(pulsar-cyan ((t (:background ,cyan))))

   ;; rainbow
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,violet))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,yellow))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,red))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,violet))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,yellow))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,red))))
  )

  (custom-theme-set-variables
    'greenized
    '(linum-format " %5i ")
  )
)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; *****************************************************************************

(provide-theme 'greenized)

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'greenized-theme)
;;; greenized-theme.el ends here
