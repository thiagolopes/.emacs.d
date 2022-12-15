;; -*- lexical-binding: t; -*-

(defconst RIGHT_PADDING 3)

(defun buffer-state ()
  (if (buffer-modified-p) " <*MOD*>"
    " <SAVED>"))

(defun projectile-name-or-none ()
  (if (projectile-project-name)
      (format "[%s]" (projectile-project-name))))

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

(defun hex-color (color)
  "Get the hexadecimal value of COLOR."
  (when color
    (let ((srgb-color (color-name-to-rgb color)))
      (apply 'color-rgb-to-hex srgb-color))))

(defun total-lines ()
  (number-to-string (count-lines (point-min) (point-max))))

(defun make-xpm (name color1 color2 data)
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
	      (or (hex-color color1) "None")
	      (or (hex-color color2) "None"))
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

(defun percent-xpm
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
    (make-xpm "percent" color1 color2 (reverse data))))


(defun powerline-hud (&optional width)
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
    (percent-xpm height pmax pmin we ws
		 (* (frame-char-width) width) color1 color2)))

(defun highlight-block ()
  (let ((color1 (face-attribute 'mode-line-inactive :background))
	(color2 (face-attribute 'font-lock-comment-face :foreground)))
    (list :background color1 :foreground color2 :weight 'bold)))


(defun line-length (n)
  "Length of the Nth line."
  (save-excursion
    (goto-char (point-min))
    (if (zerop (forward-line (1- n)))
	(- (line-end-position)
	   (line-beginning-position)))))



(defun mode-line-fill-right (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
	      'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
	      'face face))

(defun mode-line-fill-center (face reserve)
  "Return empty space using FACE to the center of remaining space leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
	      'display `((space :align-to (- (+ center (.5 . right-margin)) ,reserve
					     (.5 . left-margin))))
	      'face face))



(defun reserve-left/middle ()
  (/ (length (format-mode-line mode-line-align-middle)) 2))

(defun reserve-middle/right ()
  (+ RIGHT_PADDING (length (format-mode-line mode-line-align-right))))

(setq mode-line-align-left
      (list
       '(:eval
	 (propertize " " 'face
		     (list
		      :height 0.3
		      :background (face-attribute 'success :foreground))))
       '(:eval
	 (propertize " " 'face
		     (list
		      :height 0.6
		      :background (face-attribute 'mode-line-inactive :background))))
       '(:eval (propertize "%*" 'face (highlight-block)))
       '(:eval (propertize "%b" 'face (highlight-block)))
       '(:eval
	 (propertize " " 'face
		     (list
		      :height 0.3
		      :background (face-attribute 'mode-line-inactive :background))))
       '(:eval
	 (propertize " " 'face
		     (list
		      :height 0.3)))
       '(:eval
	 (propertize " " 'face
		     (list
		      :height 0.3)))
       '(:eval (propertize (projectile-name-or-none) 'face '(:foreground "white")))
       '(vc-mode vc-mode)
       ))

(setq mode-line-align-right
      (list
       '(:eval global-mode-string)
       " %o "
       '(:eval (if (column-number-mode) "C%c " ""))
       '(line-number-mode "L%l")
       ":"
       '(:eval (total-lines))
       " "
       '(:eval (propertize " " 'display (powerline-hud)))))

(setq mode-line-align-middle
      '(""))

(setq-default mode-line-format
	      (list
	       mode-line-align-left
	       '(:eval (mode-line-fill-center 'mode-line
					      (reserve-left/middle)))
	       mode-line-align-middle
	       '(:eval
		 (mode-line-fill-right 'mode-line
				       (reserve-middle/right)))
	       mode-line-align-right))

(provide 'init-modeline)
