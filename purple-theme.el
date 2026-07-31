;;; purple-theme.el --- purple like rain
;;; commentary:
;;; purple like rain
;;; by TLP (Thiago Lopes)
;;; credits: modus-themes

(require 'modus-themes)

(setq purple-pallete
      (append
       '((bg-main    "#2C001E")
         ;; (fg-main    "#DAB686")
         (fg-main    "#d3b58d")
         (bg-dim     "#2c001e")

         (preprocessor "#4e9a06")
         (builtin      "white")
         (comment      "#8B7355")
         (docstring    "#A89968")
         (fnname       "#fce94f")
         (keyword      "#8AE234")
         (string       "#00CDCD")
         (name         "#E8D5C4")
         (keybind      "#FFD700")
         ;; (type         "#c4a000")
         (type         fg-main)
         (variable     "#c4a000")
         (variable-use "#CDA876")
         (constant     "#FFA500")

         (bg-mode-line-inactive      "#1F0914")
         (fg-mode-line-inactive      "#8B7355")

         (border-mode-line-inactive  "#4A2835")
         )))

(setq purple-custom-faces
      (append
       '(
         `(font-lock-negation-char-face ((,c :foreground "white")))
         `(mode-line-active   ((,c (:inherit variable-pitch :background "#200019" :foreground "white"))))
         `(mode-line-inactive ((,c (:inherit variable-pitch))))
         )))

(modus-themes-theme
 'purple
 'modus-themes
 "Purple like rain"
 'dark
 'modus-vivendi-palette
 'standard-dark-pallete
 'purple-pallete
 'purple-custom-faces)

;;; purple-theme.el ends here
