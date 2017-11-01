;; THEME is handled by circadian.el

;;; INPUT FONT SETTINGS
;;; http://input.fontbureau.com/download/index.html
;;;; 	Four Style Family
;;;;		Regular: InputMonoNarrow-Light.ttf
;;;;		Italic: InputMonoNarrow-LightItalic.ttf
;;;;		Bold: InputMonoNarrow-Medium.ttf
;;;;		Bold Italic: InputMonoNarrow-MediumItalic.ttf
;;;;
;;;;	Alternates
;;;;		--asterisk=height
;;;;		--braces=straight
;;;;		--i=topserif
;;;;		--l=serifs_round
;;;;
;;;;	Line Height: 1.3×
;;; END INPUT FONT SETTINGS

;; The font I love (with a fallback).
(cond
 ((member "Input" (font-family-list))
  (set-face-attribute 'default nil :font "Input:10"))
 ((member "Source Code Pro" (font-family-list))
  (set-face-attribute 'default nil :font "Source Code Pro:10")))

;; Disable the menu bar
(menu-bar-mode -1)
;; Disable the toolbar
(tool-bar-mode -1)
;; Disable scroll bars
(scroll-bar-mode -1)

;; Make the fringe background the same as the window. Creates a nice padding.
(set-face-attribute 'fringe nil :background nil)
