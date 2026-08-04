;;; greenade-theme.el --- Green Theme -*- lexical-binding: t; -*-
;;; commentary:
;;;     Inspired by Jonathan Blow theme
;;;
;;; by TLP (Thiago Lopes)
;;; credits: modus-themes

(require 'modus-themes)

(setq greenade-palette
      (append
       '(
         (bg-main "#041818")
         (fg-main "#d3b58d")
         (cursor "lightgreen")
         (bg-region "blue")
         (fg-region "white")
         (fg-prompt "white")
         (bg-dim "#081D1D")

         (builtin "white")
         (string "#0fdfaf")
         (comment "#3fdf1f")
         (docstring "#3fdf1f")
         (constant fg-main)
         (fnname "white")
         (fnname-call fg-main)
         (keyword "white")
         (preprocessor fg-main)
         (docstring fg-main)
         (string fg-main)
         (type fg-main)
         (variable "#c8d4ec")
         (variable-use "white")
         (rx-escape string) ; compare with `string'
         (name "white")
         (keybind "white")
         (font-lock-negation-char-face fg-main)

         (bg-mode-line-active "#080808")
         (border-mode-line-active "#080808")
         (fg-mode-line-active "lightgreen")
         (bg-mode-line-inactive      "#202020")
         (border-mode-line-inactive      "#202020")

         (bg-completion "#404040")
         (bg-completion-match-0 "blue")
         (fg-completion-match-0 "white")
         (fg-completion-match-1 string)

         ;; (red             "#FF6188")
         ;; (green           "#A9DC76")
         ;; (yellow          "#FFD866")
         ;; (blue            "#AB9DF2")
         ;; (magenta         "#FC9867")
         ;; (cyan            "#78DCE8")
         )))

(setq greenade-custom-faces
      (append
       '(
         `(font-lock-negation-char-face ((,c :foreground "white")))
         `(eglot-mode-line ((,c :foreground "#041818")))
         `(mode-line-buffer-id ((,c :foreground "white" :bold t)))
         )))

(modus-themes-theme
 'greenade
 'modus-themes
 "Green like Grenade"
 'dark
 'modus-vivendi-palette
 'standard-dark-palette
 'greenade-palette
 'greenade-custom-faces)

;;; greenade-theme.el ends here
