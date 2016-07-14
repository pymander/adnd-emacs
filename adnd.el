;; AD&D Functions to make writing DMing stuff in Emacs easier
;;
;; $Id: adnd.el,v 1.23 1999/04/20 15:38:22 erik Exp $
;;
;; Things left to do:
;; 1. Provide better GNU Emacs minor-mode support (Done -- needs testing)
;; 2. Provide AD&D minor-mode menu (Done -- mostly)
;; 3. Start work on AD&D major-mode for player tracking.  Look at VM
;;    for ideas

(provide 'adnd)

;; Duh.  GNU Emacs doesn't dump with cl for some reason.  We NEED these
;; functions, as we deal with ridiculously long lists here.
(require 'cl)
(require 'easymenu)
(require 'adnd-lib)
(require 'adnd-ter)

;; Seed the random number generator.
(random t)

(defvar adnd-minor-mode nil
  "T, if the region is active in the `adnd-minor-mode'.")

(make-variable-buffer-local 'adnd-minor-mode)
(defvar adnd-minor-mode-map (make-sparse-keymap))

(defconst adnd-version "0.10")

(define-key adnd-minor-mode-map "\C-ce" 'adnd-make-encounter)
(define-key adnd-minor-mode-map "\C-cr" 'adnd-roll)
(define-key adnd-minor-mode-map "\C-ct" 'adnd-make-encounter-table)
(define-key adnd-minor-mode-map "\C-cc" 'adnd-encounter-roll)
(define-key adnd-minor-mode-map "\C-ca" 'adnd-add-custom-monster)
(define-key adnd-minor-mode-map "\C-c\C-r" 'adnd-refresh-monster-alist)
(define-key adnd-minor-mode-map "\C-cd" 'adnd-list-spheres)
(define-key adnd-minor-mode-map "\C-cp" 'adnd-priest-spells)

(defconst adnd-xemacs-p (string-match "XEmacs" emacs-version))

;; Set up a minor mode.  There might be a good way to clean this up, but
;; I want both XEmacs and GNU Emacs compatability here.
(if adnd-xemacs-p
    (add-minor-mode 'adnd-minor-mode " AD&D" adnd-minor-mode-map)
  (progn
    (or (assq 'adnd-minor-mode minor-mode-map-alist)
	(setq minor-mode-map-alist
	      (cons (cons 'adnd-minor-mode adnd-minor-mode-map)
		    minor-mode-map-alist)))
    (or (assq 'adnd-minor-mode minor-mode-alist)
	(setq minor-mode-alist 
	      (cons '(adnd-minor-mode " AD&D") minor-mode-alist)))))

(easy-menu-define
 adnd-minor-mode-menu (if adnd-xemacs-p nil (list adnd-minor-mode-map))
 "AD&D minor mode menu"
 '("AD&D"
   ["Create Encounter" adnd-make-encounter t]
   ["Encounter Table" adnd-make-encounter-table t]
   ["Random Encounter" adnd-encounter-roll t]
   ["Deity Information" adnd-list-spheres t]
   ["Pick Priest Spells" adnd-priest-spells t]
   ["Roll Dice" adnd-roll t]
   ["Add Custom Monster" adnd-add-custom-monster t]
   ["Reread Monsters" adnd-refresh-monster-alist t]))

(defun adnd-minor-mode (&optional arg)
  "Toggle AD&D minor mode.  There are, of course, keybindings.
\\{adnd-minor-mode-map}"
  (interactive "P")
  (setq adnd-minor-mode
	(if (null arg)
	    (not adnd-minor-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if adnd-minor-mode
      (easy-menu-add adnd-minor-mode-menu)
    (easy-menu-remove adnd-minor-mode-menu)))
    
;; Monster DB format:
;; Creature; AC; HD; THAC0; Terrain; Climate; Att #; Damage; No App; SA;
;; SD; MR; Size; Move; XP; Activity; Align; Treasure; INT; Page; Type;
;; Remarks; Notes; Frequency; Comments; Organization; Diet; Morale;
;; Campaign; Plane; Source
(defun adnd-make-encounter (&optional arg argmon)
  "Long and ugly function to print out an AD&D monster encounter.  It
does, however, work nicely and saves me a lot of time, even if it is
slightly less attractive than Barbara Bush's left butt cheek."
  (interactive "p")
  (if (not (integerp arg)) (setq arg 0))
  (save-excursion
    (let (mlist amon name ac hd thaco ter clim att dmg napp sa sd mr sz mv
		xp act al treas int page type remark note freq comm org
		diet ml camp plane src)
      (if (stringp argmon)
	  (setq amon (adnd-find-monster argmon))
	(setq amon (adnd-find-monster)))
      ;; Now a long loop where we set all of the above variables.  Ugly,
      ;; I know -- I should be using my plist function here to save
      ;; sanity, perhaps.
      (setq name (pop amon)
	    ac   (pop amon)
	    hd   (pop amon)
	    thaco (pop amon)
	    ter   (pop amon)
	    clim  (pop amon)
	    att  (pop amon)
	    dmg  (pop amon)
	    napp (pop amon)
	    sa  (pop amon)
	    sd  (pop amon)
	    mr  (pop amon)
	    sz (pop amon)
	    mv (pop amon)
	    xp (pop amon)
	    act (pop amon)
	    al (pop amon)
	    treas (pop amon)
	    int (pop amon)
	    page (pop amon)
	    type (pop amon)
	    remark (pop amon)
	    note (pop amon)
	    freq (pop amon)
	    comm (pop amon)
	    org (pop amon)
	    diet (pop amon)
	    ml (pop amon)
	    camp (pop amon)
	    plane (pop amon)
	    src (pop amon))

      ;; Reuse the mlist variable.  Here's where we figure out the #APP
      ;; value.
      (setq mlist (string-to-int napp))
      (if (or (>= arg 4) (null napp) (= 0 mlist))
	  (setq napp (string-to-int (read-string "How many? ")))
	(let (min max)
	  (setq mlist (split-string napp "[- ]"))
	  ;; If it looks funny, just cop out.  We don't need to deal
	  ;; with the MMDB's weird idiosynchrocies just yet.  That's the
	  ;; next version.
	  (if (= (list-length mlist) 2)
	      (progn
		(setq min (string-to-int (pop mlist))
		      max (- (string-to-int (pop mlist)) min)
		      napp (+ min (random max))))
	    (setq napp (string-to-int (pop mlist))))))

      ;; Now that we have all the info, we print everything out.
      (insert (format "%s: #APP %d; AL %s; AC %s; Mv %s; HD %s; hp %s; THAC0 %s; #ATT %s; Dmg %s; "
		      name napp al ac mv hd 
		      (adnd-hitdice (int-to-string napp) hd)
		      thaco att dmg))
      (setq sa (string-to-int sa)
	    sd (string-to-int sd))
      (if (= sa 1)
	  (insert "SA See below; "))
      (if (= sd 1)
	  (insert "SD See below; "))
      (insert (format "MR %s; SZ %s; ML %s; XP %s"
	       mr sz ml xp))
      (newline)
      (insert (format "Remarks: %s %s %s; %s" remark note comm page))
      (newline)
      (insert
       (if (string= treas "nil")
	   "Treasure: none"
	 (format "Treasure (%s): %s"
		 treas
		 (adnd-treasure-roll treas))))
      (newline)
      name)))

(defun adnd-make-encounter-table (arg)
  "Makes an encounter table for specified terrains.  With
\\[universal-argument] it prompts you for everything.  Without, it
generates the whole thing randomly."
  (interactive "p")
  (save-excursion
    (let (tlist tin cmon mlist cfreq i blist ulist)
      (setq blist (list (adnd-monst-buffer)
			(adnd-cust-buffer)))
      (insert "\nEncounter Table: ")
      (while (not 
	      (string-equal "done"
			    (setq tin 
				  (completing-read 
				   "Terrain (enter `done' when complete): " 
				   adnd-terrain-alist nil t))))
	(unless (or (member tin tlist) (= 0 (length tin)))
	  (if (null tlist)
	      (insert tin)
	    (insert ", " tin))
	  (push tin tlist)))
      (save-excursion
	(dolist (tin tlist)
	  (setq blist (list (adnd-monst-buffer)
			    (adnd-cust-buffer)))
	  (message (format "Getting monsters for %s" tin))
	  (while blist
	    (set-buffer (pop blist))
	    (goto-char (point-min))
	    (while (search-forward-regexp
		    (concat "^\\(.*" tin ".*\\)$") nil t)
	      (setq cmon (adnd-make-monster-plist 
			  (split-string (match-string 1) "|")))
	      (unless (member (plist-get cmon 'name) ulist)
		(push (plist-get cmon 'name) ulist)
		(setq mlist (cons (cons (plist-get cmon 'name)
					(plist-get cmon 'freq)) 
				  mlist)))))))
      (setq i 2)
      (message "Creating table...")
      (while (<= i 20)
	(or
	 (cond ((or (<= i 3) (>= i 19))
		(setq cfreq "VR"))
	       ((or (<= i 5) (>= i 17))
		(setq cfreq "R"))
	       ((or (<= i 8) (>= i 14))
		(setq cfreq "UC")))
	 (setq cfreq "C"))
	;; Without universal argument, make a random table.
	(if (>= arg 4)
	    ;; Prompt for monsters.
	    (setq cmon (completing-read (format "Monsters (%s): " cfreq)
					mlist 
					(lambda (arg)
					  (string-equal cfreq (cdr arg)))))
	  ;; Randomly pick monsters.
	  (setq cmon (adnd-enc-shuffle mlist cfreq)))
	(insert (format "\n %2d: %s (%s)" i cmon cfreq))
	(setq i (+ 1 i))))))

(defun adnd-enc-shuffle (mlist cfreq)
  (interactive)
  (let (clist cm)
    (while mlist
      (setq cm (pop mlist))
      (if (string-equal cfreq (cdr cm))
	  (push (car cm) clist)))
    (setq cm (random (list-length clist)))
    (nth cm clist)))

(defun adnd-encounter-roll (&optional arg)
  "Interactively rolls on an encounter table and generates an encounter.
If used with \\[universal-argument], will have adnd-make-encounter ask
for a number of creatures to generate instead of determining randomly."
  (interactive "p")
  (let (etable etbpoint etepoint cmon i)
    (setq cmon (list "nothing"))
    (save-excursion
      (or (search-backward-regexp "^Encounter Table: \\(.*\\)$" nil t)
	  (search-forward-regexp "^Encounter Table: \\(.*\\)$" nil t))
      (setq etable (match-string 1))
      (beginning-of-line)
      (setq etbpoint (point))
      (unless (yes-or-no-p (format "Use table for %s? " etable))
	nil)
      (search-forward-regexp "^ 20: .*$" nil t)
      (end-of-line)
      (setq etepoint (point))
      (narrow-to-region etbpoint etepoint)
      (goto-char (point-min))
      (setq i 2)
      ;; Create an alist of all the monsters in the table.
      (while (<= i 20)
	(search-forward-regexp (concat "^ " (format "%2d" i)
				       ": \\(.*\\) (.*)$") nil t)
	(push (match-string 1) cmon)
	(setq i (+ 1 i)))
      (widen)
      (goto-char etepoint)
      (setq i (+ (adnd-roll 1 8 0 t) (adnd-roll 1 12 0 t)))
      (insert "\n\n")
      (setq i (nth (- i 1) cmon))
      (if (string-equal "nil" i)
	  (insert "No encounter.")
	(adnd-make-encounter arg i)))))


;; End of file.
