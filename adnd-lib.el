;; Contains frequently-used functions for all of the AD&D stuff.
;; $Id: adnd-lib.el,v 1.12 1999/04/20 15:38:22 erik Exp $

(provide 'adnd-lib)
(require 'cl)

(defconst adnd-lib-version "0.02")

(defvar adnd-monster-db "~erik/src/adnd/monster.raw"
  "Flat text monster database path.")

(defvar adnd-priest-db "~erik/src/adnd/priestspell.raw")
(defvar adnd-mage-db "~erik/src/adnd/magespell.raw")

(defvar adnd-custom-monster-db "~erik/adnd/custom.raw"
  "Flat text custom monster database for new monsters")

(defvar adnd-monst-buffer nil)
(defvar adnd-cust-buffer nil)
(defvar adnd-priest-buffer nil)
(defvar adnd-monster-alist nil)
(defvar adnd-terrain-alist nil)
(defvar adnd-gods-alist nil)

(defconst adnd-monster-property-list
  '(name ac hd thaco ter clim att dmg napp sa sd mr sz mv
	 xp act al treas int page type remark note freq comm org
	 diet ml camp plane src))

(defconst adnd-treasure-alist
  '(("A" 25 "1000-3000" 30 "200-2000" 40 "1000-6000" 35 "300-1800"
     60 "10-40" 50 "2-12" 30 "3")
    ("B" 50 "1000-6000" 25 "1000-3000" 25 "200-2000" 25 "100-1000"
     30 "1-8" 20 "1-4" 10 "A|W")
    ("C" 20 "1000-1000" 30 "1000-6000" nil nil 10 "100-600"
     25 "1-6" 20 "1-3" 10 "2")
    ("D" 10 "1000-6000" 15 "1000-10000" 50 "1000-3000" 15 "100-600"
     30 "1-10" 25 "1-6" 15 "2|P")
    ("E" 5 "1000-6000" 25 "1000-10000" 25 "1000-4000" 25 "300-1800"
     15 "1-12" 10 "1-6" 25 "3|S")
    ("F" nil nil 10 "3000-18000" 40 "1000-6000" 15 "1000-4000"
     20 "2-20" 10 "1-8" 30 "5")
    ("G" nil nil nil nil 50 "2000-20000" 50 "1000-10000"
     30 "3-18" 25 "1-6" 35 "5")
    ("H" 25 "3000-18000" 40 "2000-20000" 55 "2000-20000" 40 "1000-8000"
     50 "3-30" 50 "2-20" 15 "6")
    ("I" nil nil nil nil nil nil 30 "100-600"
     55 "2-12" 50 "2-8" 15 "1")
    ;; Individual and small lair treasures
    ("J" 100 "3-24")
    ("K" nil nil 100 "3-18")
    ("L" nil nil nil nil nil nil 100 "2-12")
    ("M" nil nil nil nil 100 "2-8")
    ("N" nil nil nil nil nil nil 100 "1-6")
    ("O" 100 "10-40" 100 "10-30")
    ("P" nil nil 100 "10-60" nil nil 100 "1-20")
    ("Q" nil nil nil nil nil nil nil nil
     100 "1-4")
    ("R" nil nil nil nil 100 "2-20" 100 "10-60"
     100 "2-8" 100 "1-3")
    ("S" nil nil nil nil nil nil nil nil nil nil nil nil
     100 "1-8P")
    ("T" nil nil nil nil nil nil nil nil nil nil nil nil
     100 "1-4S")
    ("U" nil nil nil nil nil nil nil nil
     90 "2-16" 80 "1-6" 70 "1")
    ("V" nil nil nil nil nil nil nil nil nil nil nil nil
     100 "2")
    ("W" nil nil nil nil 100 "5-30" 100 "1-8"
     60 "2-16" 50 "1-8" 60 "2")
    ("X" nil nil nil nil nil nil nil nil nil nil nil nil
     100 "2P")
    ("Y" nil nil nil nil 100 "200-1200")
    ("Z" 100 "100-300" 100 "100-400" 100 "100-600" 100 "100-400"
     55 "1-6" 50 "2-12" 50 "3"))
  "Long plist for generating treasure.  Modify in the source file to suite your campaign.  It's a const, see.")

(defconst adnd-priest-spells
  '((1) (2) (2 1) (3 2) (3 3 1) (3 3 2) (3 3 2 1) (3 3 3 2)
    (4 4 3 2 1) (4 4 3 3 2) (5 4 4 3 2 1) (6 5 5 3 2 2)
    (6 6 6 4 2 2) (6 6 6 5 3 2 1) (6 6 6 6 4 2 1)
    (7 7 7 6 4 3 1) (7 7 7 7 5 3 2) (8 8 8 8 6 4 2)
    (9 9 8 8 6 4 2) (9 9 9 8 7 5 2)))

(defmacro adnd-monst-buffer ()
  '(if (and adnd-monst-buffer (buffer-name adnd-monst-buffer))
       adnd-monst-buffer
     (setq adnd-monst-buffer (find-file-noselect adnd-monster-db 'nowarn))))

(defmacro adnd-cust-buffer ()
  '(if (and adnd-cust-buffer (buffer-name adnd-cust-buffer))
       adnd-cust-buffer
     (setq adnd-cust-buffer (find-file-noselect adnd-custom-monster-db
						'nowarn))))

(defmacro adnd-priest-buffer ()
  '(if (and adnd-priest-buffer (buffer-name adnd-priest-buffer))
       adnd-priest-buffer
     (setq adnd-priest-buffer (find-file-noselect adnd-priest-db 'nowarn))))

(defmacro adnd-priest-spell-alist ()
  '(if (null adnd-priest-spell-alist)
       (setq adnd-priest-spell-alist (adnd-make-spell-alist (adnd-priest-buffer)))
     adnd-priest-spell-alist))

(defmacro adnd-monster-alist ()
  '(if (null adnd-monster-alist)
       (setq adnd-monster-alist (adnd-make-monster-alist))
     adnd-monster-alist))

(defun adnd-make-spell-alist (buf)
  "Makes a matching list for spells."
  (save-excursion
    (let (slist spell)
      (set-buffer buf)
      (goto-char (point-min))
      (while (search-forward-regexp "^\\(.*\\)$" nil t)
	(setq spell (split-string (match-string 1) "|")
	      slist (cons (cons (nth 0 spell) (nth 1 spell))
			  slist)))
      slist)))

(defun adnd-make-monster-alist ()
  "Makes a matching list for the monsters.  Really groovy."
  (save-excursion
    (let (monlist mon)
      (set-buffer (adnd-monst-buffer))
      (goto-char (point-min))
      (while (search-forward-regexp "^\\([^|]+\\)|" nil t)
	;; Here is how we make an alist.  Ouch.
	(setq monlist (cons (cons (match-string 1) t) monlist)))
      ;; Add custom monsters
      (set-buffer (adnd-cust-buffer))
      (goto-char (point-min))
      (while (search-forward-regexp "^\\([^|]+\\)|" nil t)
	(setq monlist (cons (cons (match-string 1) t) monlist)))
      (nreverse monlist))))

(defun adnd-hitdice (&optional number hd)
  "Asks for a number of monsters and their hit dice, then prints out the results at point."
  (interactive)
  (if (null number)
      (setq number (string-to-int (read-string "Number of creatures? " "1")))
    (setq number (string-to-int number)))
  (if (null hd)
    (setq hd (read-string "Hit dice? " "1+1")))
  (let (dice mod num i hp ostr)
    (setq i (split-string hd "+")
	  num 0)
    (if (null i)
	(setq dice 0
	      mod 0)
      (setq dice (string-to-int (car i))
	    i (cdr i)))
    (if (null i)
	(setq mod 0)
      (setq mod (string-to-int (car i))))
    (while (< num number)
      (setq num (+ 1 num))
      ;; Add a sorting procedure in there somewhere, if you feel like it.
      (setq ostr (concat ostr (format "%d, " (adnd-roll dice 8 mod t)))))
    (string-match "^\\(.*\\), $" ostr)
    (setq ostr (match-string 1 ostr))
    ostr))

(defun adnd-treasure-roll (&optional tstr)
  "Treasure rolling function.  Pass an optional treasure string like
the one from the monster database.  This string should look like this:
`A, H, (Qx5)'."
  (interactive)
  (if (null tstr)
      (setq tstr (read-string "Treasure type: ")))
  (save-excursion
    (let (tlist tidx mlist mult i chance amount cur coins
		cp sp gp pp gems art magic)
      (setq cp 0
	    pp 0
	    sp 0
	    gp 0
	    gems 0
	    art 0
	    magic "")
      (setq tlist (split-string tstr "[, \(\)]+"))
      (while tlist
	(setq tidx (pop tlist))
	(unless (= (length tidx) 0)
	  (if (string-match "^\\([A-Z]\\)x\\([0-9]+\\)$" tidx)
	      (setq tidx (match-string 1 tstr)
		    mult (string-to-int (match-string 2 tstr)))
	    (setq mult 1))
	  (setq mlist adnd-treasure-alist)
	  (while (and mlist (not (string= tidx (caar mlist))))
	    (setq mlist (cdr mlist)))
	  (setq i (cdar mlist))
	  (setq coins '("cp" "sp" "gp" "pp" "gems" "art" "magic"))
	  (while i
	    (if (integerp (car i))
		(progn
		  (setq chance (- (pop i) 1)
			amount (pop i)
			cur (pop coins))
		  (if (<= (random 100) chance)
		      (if (string= cur "magic")
			  (setq magic (concat magic 
					      (if (> (length magic) 2)
						  "; ")
					      (adnd-magic-item amount)))
			(progn
			  (setq tidx (* (adnd-range amount) mult))
			  (cond ((string= cur "cp")
				 (setq cp (+ cp tidx)))
				((string= cur "sp")
				 (setq sp (+ sp tidx)))
				((string= cur "gp")
				 (setq gp (+ gp tidx)))
				((string= cur "pp")
				 (setq pp (+ pp tidx)))
				((string= cur "art")
				 (setq art (+ art tidx)))
				((string= cur "gems")
				 (setq gems (+ gems tidx))))))))
	    (pop coins)
	    (pop i)
	    (pop i)))))
      (concat (if (> cp 0)
		  (format "%dcp, " cp))
	      (if (> sp 0)
		  (format "%dsp, " sp))
	      (if (> gp 0)
		  (format "%dgp, " gp))
	      (if (> pp 0)
		  (format "%dpp, " pp))
	      (if (> gems 0)
		  (format "%d gems, " gems))
	      (if (> art 0)
		  (format "%d art objects, " art))
	      (if (> (length magic) 1)
		  (format "magic items: %s" magic))))))

(defun adnd-range (rstr)
  (let (b e (x (split-string rstr "[- ]+")))
    (setq b (string-to-int (pop x))
	  e (- (string-to-int (pop x)) b))
    ;(insert (format "B: %d  E: %d\n" b e))
    (+ 1 (random e) b)))

(defun adnd-magic-item (amount)
  (let (kur klist kt i mlist mstr
	(min 0)
	(max 0)
	(amlist (split-string amount "|")))
    (while amlist
      (setq min 0
	    max 0
	    kt nil
	    kur (pop amlist)
	    klist (string-to-list kur))
      (while klist
	(setq kur (char-to-string (pop klist)))
	(if (> (string-to-int kur) 0)
	    (if (= min 0)
		(setq min (string-to-int kur))
	      (setq max (string-to-int kur)))
	  (setq kt kur)))
      (cond ((and (> min 0) (> max 0))
	     (setq i 1
		   max (+ 1 min (random (- max min)))))
	    ((and (= min 0) (= max 0))
	     (setq i 1
		   max 1))
	    ((> min 0)
	     (setq i 1
		   max min)))
      (while (<= i max)
	(setq i (+ 1 i))
	(push 
	 (if (null kt)
	     (adnd-misc-magic)
	   (adnd-mag-by-char kt))
	 mlist)))
    (setq mstr (pop mlist))
    (while mlist
      (setq mstr (concat mstr "; " (pop mlist))))
    mstr))

(defun adnd-mag-by-char (ch)
  "Kludgy and scary."
  (cond ((string= ch "W")
	 (adnd-weapon))
	((string= ch "A")
	 (adnd-armor))
	((string= ch "S")
	 (adnd-scroll))
	((string= ch "P")
	 (adnd-potion))))

(defun adnd-misc-magic ()
  "Rolls a miscellaneous magic item."
  (let ((i (random 100)))
    (cond ((< i 20)
	   (adnd-potion))
	  ((< i 35)
	   (adnd-scroll))
	  ((< i 40)
	   (adnd-ring))
	  ((< i 41)
	   (adnd-rod))
	  ((< i 42)
	   (adnd-stave))
	  ((< i 45)
	   (adnd-wand))
	  ((< i 60)
	   (adnd-misc))
	  ((< i 75)
	   (adnd-armor))
	  ((< i 100)
	   (adnd-weapon)))))

(defun adnd-potion ()
  "Generates a potion."
  (let ((potion-list '("Animal Control" "Clairaudience" "Clairvoyance"
		       "Climbing" "Delusion" "Diminution" "Dragon Control" 
		       "Elixer of Health" "Elixer of Madness" "Elixer of Youth"
		       "ESP" "Extra-healing" "Fire Breath" "Fire Resistance"
		       "Flying" "Gaseous Form" "DM's Choice" "Giant Control"
		       "Giant Strength" "Growth" "Healing" "Heroism"
		       "Human Control" "Invisibility" "Invulnerability"
		       "Levitation" "Longevity" "Oil of Acid Resistance"
		       "Oil of Disenchantment" "Oil of Elemental Invulnerability"
		       "Oil of Etherealness" "Oil of Fiery Burning"
		       "Oil of Fumbling" "Oil of Impact" "Oil of Slipperiness"
		       "Oil of Timelessness" "Philter of Glibness"
		       "Philter of Love" "Philter of Persuasiveness"
		       "Philter of Stammering and Stuttering" "Plant Control"
		       "Poison" "Polymorph Self" "Rainbow Hues" "Speed"
		       "Super-heroism" "Sweet Water" "Treasure Finding"
		       "Undead Control" "Ventriloquism" "Vitality"
		       "Water Breathing" "Healing")))
    (concat "Potion - " 
	    (nth (random (list-length potion-list)) potion-list))))

(defmacro adnd-ring ()
  "Returns a random ring."
  '(let ((ring-list '("Animal Friendship" "Blinking" "Chameleon Power"
		     "Clumsiness" "Contrariness" "Delusion"
		     "Djinni Summoning" "Elemental Command" "Feather Falling"
		     "Fire Resistance" "Free Action" "Human Influence"
		     "Invisibility" "Jumping" "Mammal Control"
		     "Mind Shielding" "Protection" "Protection"
		     "the Ram" "Regeneration" "Shocking Grasp" "Shooting Stars"
		     "Spell Storing" "Spell Turning" "Sustenance" "Swimming"
		     "Telekinesis" "Truth" "Warmth" "Water Walking"
		     "Weakness" "Multiple Wishes" "Three Wishes" "Wizardry"
		     "X-Ray Vision" "DM's Choice")))
    (concat "Ring of "
	    (nth (random (list-length ring-list)) ring-list))))

(defmacro adnd-rod ()
  "REturns a random rod."
  '(let ((rod-list '("Absorption" "Alertness" "Beguiling" "Cancellation"
		    "Flailing" "Lordly Might" "Passage" "Resurrection"
		    "Rulership" "Security" "Smiting" "Splendor"
		    "Terror" "DM's Choice")))
    (concat "Rod of "
	    (nth (random (list-length rod-list)) rod-list))))

(defmacro adnd-stave ()
  "Returns a random stave."
  '(let ((stave-list '("Mace" "Command" "Curing" "the Magi" "Power"
		      "Serpent" "Slinging" "Spear" "Striking"
		      "Swarming Insects" "Thunder & Lightning"
		      "Withering" "Woodlands" "DM's Choice")))
    (concat "Staff of "
	    (nth (random (list-length stave-list)) stave-list))))

(defmacro adnd-wand ()
  "Returns a random magic wand."
  '(let ((wand-list '("Conjuration" "Earth and Stone" "Enemy Detection"
		      "Fear" "Fire" "Flame Extinguishing" "Frost"
		      "Illumination" "Illusion" "Lightning"
		      "Magic Detection" "Magic Missiles" "Negation"
		      "Metal and Mineral Detection" "Paralyzation"
		      "Polymorphing" "Secret Door and Trap Location"
		      "Size Alteration" "Wonder" "DM's Choice")))
     (concat "Wand of "
	     (nth (random (list-length wand-list)) wand-list))))

(defmacro adnd-misc ()
  "Returns a random miscellaneous magic item."
  '(let ((misc-list '("Book" "Jewelry" "Robe" "Boot" "Girdle"
		      "Container" "Candle" "Household Item"
		      "Musical Instrument" "Weird Thing")))
     (concat "Misc - "
	     (nth (random (list-length misc-list)) misc-list))))

(defmacro adnd-armor ()
  "Returns a random set of magical armor."
  '(let ((armor-list '("Banded mail" "Brigandine" "Chain mail"
		       "Field plate" "Full plate" "Leather" "Plate mail"
		       "Ring mail" "Scale mail" "Shield" "Splint mail"
		       "Studded leather" "DM's Choice"))
	 (spec-list '("Armor of Command" "Armor of Blending"
		      "Armor of Missile Attraction" "Armor of Rage"
		      "Elven Chain Mail" "Plate Mail of Etherealness"
		      "Plate Mail of Fear" "Plate Mail of Vulnerability"
		      "Large Shield +1, +4 vs. Missiles"
		      "Shield -1, Missile Attractor"))
	 (plus-list '("-1" "-1" "+1" "+1" "+2" "+2" "+3" "+3" "+4" 
		      "+4" "+5")))
     (if (< (random 100) 5)  ;; Check for special armor
	 (nth (random (list-length spec-list)) spec-list))
       (concat (nth (random (list-length armor-list)) armor-list) " "
	       (nth (random (list-length plus-list)) plus-list))))

(defmacro adnd-weapon ()
  "Returns a random magical weapon."
  '(let ((weapon-list '("2d6 Arrows" "3d6 Arrows" "4d6 Arrows" "Axe"
			"Battle Axe" "2d10 Bolts" "2d6 Bolts"
			"3d4 Sling Bullets" "Dagger" "Dagger" "Dagger"
			"3d4 Darts" "Flail" "1d2 Javelins" "Knife"
			"Lance" "Mace" "Mace" "Military Pick"
			"Morning Star" "Pole Arm" "Scimitar" "Halbard"
			"Spear" "Trident" "Warhammer" "Maul" "Trident"
			"Quarterstaff" "DM's Choice"))
	 (st-plus-list '("-1" "+1" "+1" "+2" "+2" "+3" "+4"))
	 (sw-plus-list '("-1" "+1" "+2" "+3" "+3" "+4" "+5"))
	 (spec-list '("Arrow of Direction" "Arrow of Slaying"
		      "Throwing Axe +2" "Axe of Hurling" "Bow +1"
		      "Crossbow of Accuracy +3" "Crossbow of Distance"
		      "Crossbow of Speed" "Dagger +1, +2 vs. Tiny or Small Creatures"
		      "Dagger +2, +3 vs. Large Creatures" "Longtooth Dagger +2"
		      "Dagger of Throwing" "Dagger of Venom"
		      "Dart of Homing" "Dwarven Throwing Hammer +3"
		      "Hammer of Thunderbolts" "Hornblade" "Javelin of Lightning"
		      "Javelin of Piercing" "Buckle Knife" "Mace of Disruption"
		      "Net of Entrapment" "Net of Snaring" "Scimitar of Speed"
		      "Cursed Backbiter Spear" "Trident of Fish Command"
		      "Trident of Warning" "Trident of Yearning" "Sun Blade"
		      "Sword +1, +2 vs. magic-using creatures"
		      "Sword +1, +3 vs. shape-changers"
		      "Sword +1, +3 vs. regenerating creatures"
		      "Sword +1, +4 vs. reptiles" "Cursed Sword +1"
		      "Sword +1, Flame Tongue" "Sword +1, Luck Blade"
		      "Sword +2, Dragon Slayer" "Sword +2, Giant Slayer"
		      "Sword +2, Nine Lives Stealer" "Sword +3, Frost Brand"
		      "Sword +4, Defender" "Sword +5, Defender"
		      "Sword +5, Holy Avenger" "Cursed Sword -2"
		      "Sword of Dancing" "Sword of Life Stealing" 
		      "Sword of Sharpness" "Sword of the Planes"
		      "Sword of Wounding" "Cursed Berserking Sword"
		      "Short Sword of Quickness +2" "Vorpal Sword")))
	 (if (< (random 100) 15) ; Check for special
	     (nth (random (list-length spec-list)) spec-list)
	   (if (< (random 100) 20) ; Check for Sword
	       (concat "Sword " (nth (random (list-length sw-plus-list))
				     sw-plus-list))
	     (concat (nth (random (list-length weapon-list)) weapon-list)
		     " "
		     (nth (random (list-length st-plus-list)) st-plus-list))))))

(defmacro adnd-scroll ()
  "Returns a random magic scroll."
  '(let ((prot-list '("Map" "Protection: Acid" "Protection: Cold"
		      "Protection: Dragon Breath" "Protection: Electricity"
		      "Protection: Elementals" "Protection: Fire"
		      "Protection: Gas" "Protection: Lycanthropes"
		      "Protection: Magic" "Protection: Petrification"
		      "Protection: Plants" "Protection: Poison"
		      "Protection: Possession" "Protection: Undead"
		      "Protection: Water" "Cursed" "DM's Choice")))
     (concat "Scroll - "
	     (if (< (random 100) 34) ; Protection scroll
		 (nth (random (list-length prot-list)) prot-list)
	       (concat
		(if (< (random 100) 30) ; Priest scroll
		    "Priest "
		  "Mage ")
		(int-to-string (+ 1 (random 7)))
		" spells")))))

(defun adnd-parse-die-string (dstr)
  "Parses a string of the format XdY+Z, where X is the number of dice, Y is the die type, and Z is a modifier."
  (let (d num r type mod)
    (setq d (split-string dstr "[dD+]")
	  num (string-to-number (pop d)))
    (if (null d)
	(setq type 0
	      mod 0)
      (setq type (string-to-number (pop d))))
    (if (null d)
	(setq mod 0)
      (setq mod (string-to-number (pop d))))
    (list num type mod)))

(defun adnd-roll (&optional num dice mod inst)
  "Rolls dice. Default die type is d8, by the way."
  (interactive)
  (if (null num)
      (setq num (read-string "Roll: ")))
  (if (stringp num) 
      (let (x)
	(if (null inst)
	    (insert (format "%s: " num)))
	(setq x (adnd-parse-die-string num)
	      num (pop x)
	      dice (pop x)
	      mod (pop x))))
  (if (or (null dice) (= dice 0))
      (setq dice 8))
  (let ((i 1)
	(total 0))
    (while (<= i num)
      (setq i (+ i 1)
	    total (+ total (+ 1 (random dice)))))
    (unless (null mod)
      (setq total (+ total mod)))
    (if (null inst)
	(insert (format "%d" total)))
    total))

(defun adnd-find-monster (&optional monst)
  "Find a monster and print out some stuff about it."
  (interactive)
  (if (null monst)
      (setq monst 
	    (completing-read "Monster name: " (adnd-monster-alist) nil t)))
  (save-excursion
    (let (monline monlist)
      (set-buffer (adnd-monst-buffer))
      (goto-char (point-min))
      (unless (search-forward-regexp (concat "^\\(" monst "|.*\\)$") nil t)
	(set-buffer (adnd-cust-buffer))
	(goto-char (point-min))
	(search-forward-regexp (concat "^\\(" monst "|.*\\)$") nil t))
      (setq monline (match-string 1)
	    monlist (split-string monline "|"))
      monlist)))

(defun adnd-make-monster-plist (monst)
  "Creates a plist for a monster.  Needs to be passed a list like the one returned from `adnd-find-monster'."
  (let ((mon-plist)
	(props adnd-monster-property-list))
    (while props
      (setq mon-plist (plist-put mon-plist (pop props) (pop monst))))
    mon-plist))

;; Monster DB format:
;; Creature; AC; HD; THAC0; Terrain; Climate; Att #; Damage; No App; SA;
;; SD; MR; Size; Move; XP; Activity; Align; Treasure; INT; Page; Type;
;; Remarks; Notes; Frequency; Comments; Organization; Diet; Morale;
;; Campaign; Plane; Source
(defun adnd-add-custom-monster (&optional monst)
  "Adds a new monster to the custom monster database."
  (interactive)
  (save-excursion
    (let (ostr cp (props adnd-monster-property-list))
      (while props
	(setq cp (pop props))
	(setq ostr
	      (concat ostr
		      (read-string (format "%s: " cp)) "|")))
      (set-buffer (adnd-cust-buffer))
      (goto-char (point-max))
      (insert (format "%s\n" ostr)))))

(defmacro adnd-refresh-monster-alist ()
  '(setq adnd-monster-alist (adnd-make-monster-alist)))

;; Below here we'll put all of the groovy stuff for parsing the gods files.
(defmacro adnd-gods-alist ()
  '(if (null adnd-gods-alist)
       (setq adnd-gods-alist (adnd-make-gods-alist))
     adnd-gods-alist))

(defun adnd-make-gods-alist ()
  "Makes the alist for deities.  Duh."
  (let ((glist adnd-gods)
	aout)
    (while glist
      (setq aout (cons (cons (caar glist) t) aout)
	    glist (cdr glist)))
    aout))

(defun adnd-list-spheres (&optional deity)
  "Prints out the correct major and minor spheres at point."
  (interactive)
  (if (featurep 'adnd-gods)
      nil
    (require 'adnd-gods))
  (save-excursion
    (let (major minor tmp)
      (if (null deity)
	  (setq deity
		(completing-read "Deity name: "
				 (adnd-gods-alist) nil t)))
      (setq tmp adnd-gods)
      (while tmp
	(if (string= deity (caar tmp))
	    (progn
	      (setq tmp (pop tmp))
	      (pop tmp)
	      (setq major (pop tmp)
		    minor (pop tmp)))
	  (setq tmp (cdr tmp))))
      (if (and (null major) (null minor))
	  (progn
	    (error "Deity \"%s\" does not exist" deity)
	    nil))
      (insert (format "DEITY: %s" deity))
      (newline)
      (insert (format "MAJOR: %s" (join-list major ", ")))
      (newline)
      (insert (format "MINOR: %s" (join-list minor ", ")))
      (newline))))

(defun join-list (jlist &optional sep)
  (if (null sep)
      (setq sep " "))
  (let (str)
    (while jlist
      (setq str (concat str
			(if (> (length str) 0)
			    sep)
			(pop jlist))))
    str))

(defun adnd-priest-spells (&optional arg)
  "Pick some spells for your priest.  Prefixed with \\[universal-argument], you get to pick a priest's wisdom.  No matter what, you get to pick a deity and a level.  Someday these will be picked automagically."
  (interactive "p")
  (let (lvl deity major minor wis bonus-spells spells curlvl tmp
	    spell-alist cspells)
    (setq curlvl 0
	  wis (if (>= arg 4)
		  (string-to-int (read-string "Wisdom: " "15"))
		0)
	  bonus-spells (cond ((= wis 13)
			      (list 1))
			     ((= wis 14)
			      (list 2))
			     ((= wis 15)
			      (list 2 1))
			     ((= wis 16)
			      (list 2 2))
			     ((= wis 17)
			      (list 2 2 1))
			     ((= wis 18)
			      (list 2 2 1 1))
			     ((= wis 19)
			      (list 3 2 1 2)))
	  lvl (- (string-to-int (read-string "Level: " "1")) 1)
	  spells (nth lvl adnd-priest-spells)
	  deity (completing-read "Deity name: "
				 (adnd-gods-alist) nil t)
	  tmp adnd-gods)
    (while tmp
      (if (string= deity (caar tmp))
	  (progn
	    (setq tmp (pop tmp))
	    (pop tmp)
	    (setq major (pop tmp)
		  minor (pop tmp)))
	(setq tmp (cdr tmp))))
    (setq spell-alist (adnd-deity-spells major minor))
    (while spells
      (setq cspells nil
	    curlvl (+ curlvl 1)
	    tmp (if bonus-spells
		    (+ (pop bonus-spells) (pop spells))
		  (pop spells)))
      (while (> tmp 0)
	(push (completing-read (format "Level %d (%d left): "
				       curlvl tmp)
			       spell-alist
			       (lambda (x)
				 (= (cdr x) curlvl))
			       t)
	      cspells)
	(setq tmp (- tmp 1)))
      (insert (format "%d: %s" curlvl (join-list cspells "; ")))
      (newline))))

(defun adnd-deity-spells (major minor)
  "Makes a list of spells for a specified deity or cleric, given major and minor spheres of access.  Yes, this is going to be tricky."
  (push "All" major)
  (save-excursion
    (let (slist spell lvl spheres)
      (set-buffer (adnd-priest-buffer))
      (goto-char (point-min))
      (while (search-forward-regexp "^\\(.*|.*\\)$" nil t)
	(setq spell (split-string (match-string 1) "|")
	      lvl (string-to-int (nth 1 spell))
	      spheres (split-string (nth 5 spell) ", "))
	(while spheres
	  (if (or (member (car spheres) major)
		  (and (<= lvl 4) (member (car spheres) minor)))
	      (setq slist (cons (cons (nth 0 spell) lvl) slist)))
	  (setq spheres (cdr spheres))))
      slist)))
    

;; End of file.
