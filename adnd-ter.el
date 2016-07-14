;; Terrain list, so it doesn't need to be processed every time.
;; $Id: adnd-ter.el,v 1.4 1999/04/14 05:24:59 erik Exp $

(provide 'adnd-ter)
(require 'cl)

; List of terrains and other things that can be used for creating
; encounter tables.
(setq adnd-terrain-alist '(("Any" . t) ("Temp/Trop" . t) 
			   ("Prime Material" . t) ("Subterranean" . t) 
			   ("Forests" . t) ("Hills/Rough" . t) 
			   ("Temperate" . t) ("Baator" . t) 
			   ("Special" . t) ("Land" . t) 
			   ("Arctic/Sub-Arctic" . t) ("Jungle" . t) 
			   ("Sub-Trop/Trop" . t) ("Water, Fresh" . t) 
			   ("Remote" . t) ("Water, Ocean" . t) 
			   ("Tropical" . t) ("Swamp" . t) 
			   ("Mountains" . t) ("Rural" . t) 
			   ("Plains" . t) ("Sub-Arctic" . t) 
			   ("Sub-Arc/Trop" . t) ("Urban" . t) 
			   ("Coast" . t) ("Alternate" . t) 
			   ("Elysium" . t) ("Water, Salt" . t) 
			   ("Desert" . t) ("Temp/SubTrop" . t) 
			   ("Arctic" . t) ("Temp/SubArc" . t) 
			   ("Water" . t) ("Air" . t) ("Elem, Air" . t) 
			   ("Elem, Earth" . t) ("Elem, Fire" . t) 
			   ("Elem, Water" . t) ("Sub-Tropical" . t) 
			   ("Abyss" . t) ("Islands" . t) 
			   ("General" . t) ("Earth" . t) 
			   ("Fire" . t) ("Astral" . t) ("Limbo" . t) 
			   ("Demi-, Shadow" . t) ("Paraelem, Magma" . t) 
			   ("Quasielem, Steam" . t) ("Paraelem, Smoke" . t) 
			   ("Lower" . t) ("Ethereal" . t) 
			   ("Forgotten Realms" . t) ("Demi-, Dread" . t) 
			   ("Upper" . t) ("Beastlands" . t) 
			   ("Outlands" . t) ("Planescape" . t) 
			   ("Varies" . t) ("Gray Waste" . t) 
			   ("Paraelem, Ooze" . t) ("Quasielem, Lightning" . t) 
			   ("Arcadia" . t) ("Demi-, Time" . t) 
			   ("Arborea" . t) ("Elysium" . t)
			   ("Beastlands" . t)
			   ("Greyhawk" . t) ("done" . t)))

;; Time to use up more memory.  Ow.
(setq adnd-planes-alist '(("Prime Material" . nil) ("Ethereal" . nil)
			  ("Astral" . nil)
			  ;; Outer Planes
			  ("Arcadia" . "LNG") ("Mount Celestia" . "LG")
			  ("Bytopia" . "NGL") ("Elysium" . "NG")
			  ("Beastlands" . "NGC") ("Arborea" . "CG")
			  ("Ysgard" . "CGN") ("Limbo" . "CN")
			  ("Pandemonium" . "CEN") ("Abyss" . "CE")
			  ("Carceri" . "NEC") ("Gray Waste" . "NE")
			  ("Gehenna" . "NEL") ("Baator" . "LE")
			  ("Acheron" . "LEN") ("Mechanus" . "LN")
			  ;; Inner Planes
			  ("Elem, Earth" . "I") ("Elem, Air" . "I")
			  ("Elem, Fire" . "I") ("Elem, Water" . "I")
			  ;; Para-
			  ("Paraelem, Smoke" . "I") ("Paraelem, Ice" . "I")
			  ("Paraelem, Ooze" . "I") ("Paraelem, Magma" . "I")
			  ;; Quasi-
			  ("Quasielem, Lightning" . "I") ("Quasielem, Steam" . "I")
			  ("Quasielem, Mineral" . "I") ("Quasielem, Radiance" . "I")
			  ("Quasielem, Salt" . "I") ("Quasielem, Vacuum" . "I")
			  ("Quasielem, Ash" . "I") ("Quasielem, Dust" . "I")
			  ("Energy, Positive" . "I") ("Energy, Negative" . "I")))

; Can't do this...takes much too long.
;(defmacro adnd-terrain-alist ()
;  '(if (null adnd-terrain-alist)
;       (setq adnd-terrain-alist (adnd-make-terrain-alist))
;     adnd-terrain-alist))

; This function takes *forever*.  Don't run it unless you have to.
(defun adnd-make-terrain-alist ()
  "Makes a terrain and Plane list for creating encounter tables."
  (save-excursion
    (let (terlist ter cl pl i)
      (setq pl 0)
      (set-buffer (adnd-buffer))
      (goto-char (point-min))
      ;; Process a line at a time.
      (while (search-forward-regexp "^\\(.*\\)|$" nil t)
	(message (format "Working %d" pl))
	(setq pl (+ 1 pl))
	(setq ter (split-string (match-string 1) "|"))
	(if (>= (list-length ter) 31)
	    (progn
	      (setq i (car (subseq ter 4 5)))
	      (unless (member i cl)
		(push i cl))
	      (setq i (car (subseq ter 5 6)))
	      (unless (member i cl)
		(push i cl))
	      (setq i (car (subseq ter 29 30)))
	      (unless (member i cl)
		(push i cl)))))
      (while cl
	(setq terlist (cons (cons (pop cl) t) terlist)))
      terlist)))

;; End of file (whew!)
