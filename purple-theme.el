;;; purple-theme.el --- purple like rain
;;; commentary:
;;; purple like rain
;;; by TLP (Thiago Lopes)
;;; credits: modus-themes
;;;          https://gist.github.com/moxwel/26aa7555d673f9e345c8930fc5ff7a2b

(require 'modus-themes)

(setq purple-pallete
      (append
       '(
         (bg-main    "#300A24")
         ;; (fg-main    "#D3B58D")
         (fg-main    "#D3D7CF")
         (bg-dim     bg-main)

         (black  "#555753")
         (blue   "#729FCF")
         (cyan   "#34E2E2")
         (green  "#8AE234")
         (purple "#AD7FA8")
         (red    "#EF2929")
         (white  "#EEEEEC")
         (yellow "#FCE94F")

         (keyword      "#4e9a06")
         (builtin      white)
         (comment      "#75507B")
         (docstring    comment)
         (fnname       yellow)
         (preprocessor green)
         (string       cyan)
         (name         "#E8D5C4")
         (keybind      "#FFD700")
         (type         "#D3B58D")
         (variable     "#C4A000")
         (variable-use "#CDA876")
         (property     variable)
         (constant     "#FFA500")

         (bg-mode-line-inactive      "#1F0914")
         (fg-mode-line-inactive      "#8B7355")

         (border-mode-line-inactive  "#4A2835")
         )))

(setq purple-custom-faces
      (append
       '(
         `(font-lock-negation-char-face ((,c :foreground "white")))
         `(mode-line-active   ((,c (:inherit variable-pitch :background "#190019" :foreground "#D3D7CF"))))
         `(mode-line-inactive ((,c (:inherit variable-pitch :foreground "#D3B58D"))))
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
