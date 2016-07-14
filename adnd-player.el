;; AD&D Player Major-mode.  Looks like VM, processes spool file and such.
;; $Id: adnd-player.el,v 1.2 1999/03/17 18:01:49 erik Exp $

(provide 'adnd-player)
;; Need this
(require 'cl)
(require 'adnd-lib)

(defvar adnd-player-version "0.01"
  "Mode version")

(defvar adnd-character-file "~/adnd/characters.raw"
  "Location of the characters.raw file.")

;; Our objective is to have a sort of split-screen thing with an
;; index/summary that shows character names, player names, and a tally
;; for keeping track of XP.  We also need something to add monster XP
;; to the total.

(defvar adnd-player-mode-map (make-keymap "AD&D")
  "AD&D Player-mode keymap")

;; Making a major-mode as opposed to a minor-mode is likely to be
;; different.  Remember, we want this to be robust.  Study the init
;; stuff for good code like VM before leaping to any conclusions.

(defun adnd-player-init ()
  "Set up the screen and load the files."
  )
