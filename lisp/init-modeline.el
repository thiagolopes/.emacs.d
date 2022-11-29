(defun modeline/buffer-state ()
  (if (buffer-modified-p) " <*MOD*>"
    " <SAVED>"))

(defun modeline/projectile-name-or-none ()
  (if (projectile-project-name)
      (format "[%s] " (projectile-project-name))))

(defun dither-xpm (color1 color2)
  "Return an XPM dither string representing."
  (format "/* XPM */
static char * dither[] = {
\"12 18 2 1\",
\".	c %s\",
\"	c %s\",
\"....... . . \",
\".. . . .    \",
\"..... . . . \",
\".... . .    \",
\"....... .   \",
\".. . . .    \",
\"..... . . . \",
\".. . . .    \",
\"....... . . \",
\".. . .      \",
\"..... . . . \",
\".... . .    \",
\"....... .   \",
\".. . . .    \",
\"..... . . . \",
\".. . . .    \",
\"....... . . \",
\".. . .      \"};" color1 color2))

(defun pl/hex-color (color)
  "Get the hexadecimal value of COLOR."
  (when color
    (let ((srgb-color (color-name-to-rgb color)))
      (apply 'color-rgb-to-hex srgb-color))))

(defun pl/make-xpm (name color1 color2 data)
  "Return an XPM image with NAME using COLOR1 and COLOR2 bits specified in DATA.
COLOR1 signifies enabled, and COLOR2 signifies disabled."
  (when window-system
    (create-image
     (concat
      (format "/* XPM */
static char * %s[] = {
\"%i %i 2 1\",
\". c %s\",
\"  c %s\",
"
	      (downcase (replace-regexp-in-string " " "_" name))
	      (length (car data))
	      (length data)
	      (or (pl/hex-color color1) "None")
	      (or (pl/hex-color color2) "None"))
      (let ((len (length data))
	    (idx 0))
	(apply 'concat
	       (mapcar #'(lambda (dl)
			   (setq idx (+ idx 1))
			   (concat
			    "\""
			    (concat
			     (mapcar #'(lambda (d)
					 (if (eq d 0)
					     (string-to-char " ")
					   (string-to-char ".")))
				     dl))
			    (if (eq idx len)
				"\"};"
			      "\",\n")))
		       data))))
     'xpm t :scale 1 :ascent 'center)))

(defun pl/percent-xpm
    (height pmax pmin winend winstart width color1 color2)
  "Generate percentage xpm of HEIGHT for PMAX to PMIN given WINEND and WINSTART.
Use WIDTH and COLOR1 and COLOR2."
  (let* ((height- (1- height))
	 (fillstart (round (* height- (/ (float winstart) (float pmax)))))
	 (fillend (round (* height- (/ (float winend) (float pmax)))))
	 (data nil)
	 (i 0))
    (while (< i height)
      (setq data (cons
		  (if (and (<= fillstart i)
			   (<= i fillend))
		      (append (make-list width 1))
		    (append (make-list width 0)))
		  data))
      (setq i (+ i 1)))
    (pl/make-xpm "percent" color1 color2 (reverse data))))


(defun powerline-hud (&optional color1 color2 width)
  "Return XPM of relative buffer location using FACE1 and FACE2 of optional WIDTH."
  (unless width (setq width 3))
  (let ((color2 (face-attribute 'region :background))
	(color1 (face-attribute 'success :foreground))
	(height (frame-char-height))
	pmax
	pmin
	(ws (window-start))
	(we (window-end)))
    (save-restriction
      (widen)
      (setq pmax (point-max))
      (setq pmin (point-min)))
    (pl/percent-xpm height pmax pmin we ws
		    (* (frame-char-width) width) color1 color2)))

(defun highlight-block ()
  (let ((color1 (face-attribute 'mode-line-inactive :background))
	(color2 (face-attribute 'font-lock-comment-face :foreground)))
    (list :background color1 :foreground color2 :weight 'bold)))

(setq-default mode-line-format
	      (list
	       '(:eval (modeline/buffer-state))
	       " "
	       '(:eval (propertize "%b" 'face (highlight-block)))
	       " => "
	       "%f "
	       '(:eval (propertize (modeline/projectile-name-or-none) 'face '(:foreground "white")))
	       "-"
	       '(vc-mode vc-mode)

	       '(:eval (propertize " " 'display `(space :align-to (- (+ right right-fringe right-margin) 20))))
	       '(:eval (propertize " " 'display (powerline-hud)))
	       "%o "))


(provide 'init-modeline)
