(defun word-match (word pattern)
  (let ((match t))
  (dotimes (i (length pattern))
    (let ((pchar (aref pattern i))
          (wchar (aref word i)))
      (if (or (equal pchar ?.)
              (equal pchar wchar)
              ) t (setq match nil))))
  match
  ))

(defun filter-by-known-positions (pattern word-list)
  (let (matched-list)
    (dolist (word word-list matched-list)
            (if (word-match word pattern)
                (setq matched-list (cons word matched-list))
                ))))

(defun has-any-letters (letters word )
    (let ((match))
      (dotimes (i (length letters))
        (let ((letter (aref letters i)))
          (if (cl-search (byte-to-string letter) word)
               (setq match t ))))
      match
      ))

(defun has-all-letters (letters word )
  (let ((match t))
    (dotimes (i (length letters))
      (let ((letter (aref letters i)))
        (if (not (cl-search (byte-to-string letter) word))
            (setq match nil ))))
    match
    ))

(defun filter-by-excluded-letters (letters word-list )
  (let (matched-list)
    (dolist (word word-list matched-list)
      (if (not (has-any-letters letters word))
          (setq matched-list (cons word matched-list))))))

(defun filter-by-required-letters (letters word-list)
  (let ((matched-list word-list))
      (dolist (word matched-list )
        (if (not (has-all-letters letters word))
              (setq matched-list (remove word matched-list))))
      matched-list
      ))

(defun filter-by-used-positions (used-positions-patterns matched-list)
  (let ((filtered-list matched-list))
  (dolist (word filtered-list)
    (dolist (pattern used-positions-patterns)
      (if (word-match word pattern)
            (setq filtered-list  (remove word filtered-list ))
            )))
  filtered-list))

;; This will do search, but with manually-created set of patterns...
(defun wordle-manual-run (pattern must-have-letters filter-letters &optional used-positions-patterns)
  (or used-positions-patterns (setq used-positions-patterns '()))
  (filter-by-used-positions used-positions-patterns
                         (filter-by-required-letters must-have-letters
                                                  (filter-by-excluded-letters filter-letters
                                                                             (filter-by-known-positions pattern wordle--word-list)))))

; Create pattern of letters that exist somewhere in the string
(defun build-must-have-letters (pattern)
  (let ((case-fold-search nil))(replace-regexp-in-string "[A-Z.]" "" pattern )))

(defun build-known-positions-pattern (match-pattern)
  (let ((case-nil))
    (downcase  (replace-regexp-in-string "[a-z]" "." match-pattern))))

(defun build-must-not-have-letters (guess pattern must-have-letters)
  (let ((result ""))
    (dotimes (i (length pattern))
      (let ((pchar (aref pattern i)))
        (if (eq pchar ?.)
            (let ((letter (aref guess i)))
                (if (not (seq-contains must-have-letters letter ))
                    (setq result (format "%s%c" result letter)))))))
    result))

(defun wordp (c) (= ?w (char-syntax c)))
(defun lowercasep (c) (and (wordp c) (= c (downcase c))))

(defun build-mask (letter pos len)
  (let ((result ""))
  (dotimes (i len)
    (setq result (format "%s%c" result (if (eq i pos) letter ?. ))))
  result
 ))

(defun build-used-positions (pattern-mask)
  (let ((result nil))
    (dotimes (i (length pattern-mask))
      (let ((pchar (aref pattern-mask i)))
        (if (lowercasep pchar)
              (setq result (cons (build-mask pchar i wordle--word-length) result)))))
    result
    ))

(defun get-all-must-have-letters (pattern-mask must-have-letters)
    (concat must-have-letters
     (replace-regexp-in-string "[.]" "" (downcase pattern-mask))))

(defun w2 (guesses &optional must-have-letters word-list )
  (let ((must-have-letters (or must-have-letters "")))
    (if (or (not guesses) (eq (length guesses) 0))
      ;; Exit with the passed word list...
       word-list
      (let* (
            (guess (first guesses))
            (pattern-mask (second guesses))
            (case-fold-search nil)
            (must-have-letters (get-all-must-have-letters pattern-mask must-have-letters)))
        (w2
       ;; Work on the rest of the list in next call 
         (cddr guesses)
         must-have-letters
         (filter-by-used-positions (build-used-positions pattern-mask) 
                                (filter-by-excluded-letters (build-must-not-have-letters guess pattern-mask must-have-letters)
                                                           (filter-by-required-letters (build-must-have-letters pattern-mask )
                                                                                    (filter-by-known-positions (build-known-positions-pattern pattern-mask) word-list )))))))))
(setq wordle--word-length 5)

(defun wordle (guesses)
  (w2 guesses "" wordle--word-list))


(setq wordle--word-list
      '(
        "abbey" "abled" "abode" "about" "abuse" "abyss" "acids" "acred" "acres"
        "actor" "added" "addle" "adept"
        "admit" "adore" "adult" "again" "agent" "agile" "agony" "agree" "ahead" "aisle"
        "alarm" "alert" "alias" "alien" "align" "alike" "alive"
        "allow" "aloes" "alone" "along" "aloof" "aloud" "altar" "alter"
        "amend" "amino" "among" "amour" "ample" "anode"
        "anole" "apart" "apres" "aptly" "arbor" "argon" "armed" "arrow" "ashen" "asked" "assay" "audio"
        "avail" "avert" "avoid" "awake" "awoke" "axial"
        "badge" "badly" "bails" "bales" "balls" "bakes" "bands"
        "banes" "bangs" "banks" "bards" "barge" "basal" "basic" "basil"
        "basin" "bates" "baths" "baton" "beams" "beans" "beard" "bears"
        "beast" "beats" "beaus" "beeps" "begin" "belie" "bells" "belts"
        "bench" "bendy" "berry" "beset" "besot" "bible" "bilks"
        "bills" "billy" "bikes" "birds" "bison" "bites" "blade" "blame"
        "bland" "blank" "blaze" "bleak" "bleat" "blend" "bloat" "block" "bloke" "blond"
        "blown" "blows" "blurb" "blush" "boats" "boing"
        "bolts" "bombs" "bones" "booth" "booty" "boron" "bowls" "boxes"
        "brach" "brain" "brash" "brass" "brats" "brawl" "bread"
        "break" "breed" "bribe" "brick" "bride" "brief" "brine" "bring" "brink"
        "brisk" "broad" "broil" "broke" "brood" "brook" "broom" "broth"
        "brows" "brunt" "board" "bonus" "books" "bowel" "built"
        "bulbs" "bulky" "burns" "burnt" "buses" "busts" "butte" "bytes"
        "cabin" "cable" "cache" "cages" "cakes" "calls" "canal" "candy" "caner" "canid" "cards" "cared" "cargo"
        "carry" "carts" "casts" "catty" "cause" "ceded" "cells" "chaff" "chair" "chalk"
        "chard" "charm" "chart" "chase" "chaos" "chaps" "cheat" "check" "cheek" "chest" "chews"
        "child" "chile" "chill" "cited" "clads" "claim" "clams"
        "clans" "claps" "claws" "clays" "clean" "clear" "click" "clone" "close"
        "cloth" "clots" "clown" "chimp" "china" "chins"
        "choke" "chord" "chore" "coats" "coded" "coeds" "coins" "colon" "comet" "comic"
        "cones" "congo" "conic" "cooly" "coral" "cords" "cores" "corns" "corps" "couch"
        "cough" "court" "crack" "crane" "crank" "crash" "crawl" "crazy" "creek" "crest" "crews" "cries" "crime"
        "crook" "crops" "cross" "crown" "crows" "cuffs" "curry" "curse" "cysts"
        "daily" "dance" "darts" "datum" "deals" "dealt"
        "death" "debit" "debut" "decal" "decay" "decks"
        "defer" "deity" "delay" "delta" "demon" "dense" "depot" "depth" "desks" "deter" "devil" "devon"
        "diets" "dimes" "dimly" "dined" "diner"
        "dines" "ditch" "dizzy" "docks" "dodge" "doggy" "doles" "dolly" "domes"
        "doses" "doubt" "dowel" "dower" "dowry" "dozes"
        "drain" "drawn" "draws" "dread" "dried" "drift" "drill" "drink" "droid" "droit"
        "drops" "dross" "drove" "drown" "drunk" "ducks"
        "duels" "duped" "dusky" "dusty" "dwarf" "dwell"
        "eagle" "earls" "early" "ebony" "edema" "edict"
        "eight" "elate" "elder" "elect" "elite"
        "email" "empty" "ended" "ender" "enemy" "enjoy"
        "ensue" "enter" "entry" "envoy" "epoch" "equal" "equip"
        "erect" "essay" "ether" "event" "every" "evils" "exams" "expel" "extra"
        "fable" "faces" "facts" "fails" "faint" "fairs" "fakes" "falls" "famed" "fancy"
        "fangs" "farce" "farms" "fatal" "fated" "fatty"
        "fauna" "fazes" "fears" "feast" "feats" "feint" "ferns" "ferry" "fewer" "fifth" "filth"
        "files" "filet" "final" "finch" "finds" "fined" "finer" "fines" "fires"
        "firms" "first" "fixed" "fixes" "fjord" "flair" "flaps"
        "flash" "flask" "flaws" "fleet" "flesh" "flies" "flock"
        "flood" "flora" "flour" "flown" "flows" "flues" "fluid"
        "focal" "focus" "foist" "folia" "foods" "fools" "forge"
        "forty" "forum" "fouls" "found" "fowls" "frail" "frame"
        "frank" "fraud" "frays" "freed" "freer" "fresh"
        "fried" "fries" "frill" "fritz" "frond" "front" "frost"
        "froth" "froze" "fruit" "fudge" "furor" "furry" "fuzzy"
        "gaits" "galls" "gangs" "gaped" "gauge" "gauze" "gazed" "gears" "geese" "genet" "genre" "genus"
        "germs" "ghost" "ghoul" "girls" "given" "gives" "glare"
        "glass" "glaze" "glide" "gloom" "glory" "gloss"
        "glove" "glued" "glyph" "grips" "gross" "going" "gorge" "goyim"
        "grain" "grand" "grass" "great" "greed" "greek" "greet" "grill"
        "groom" "grout" "guard" "guess" "guide" "guile" "guilt"
        "habit" "hacks" "hairs" "hairy" "hands" "hangs" "hanky" "happy"
        "hards" "harps" "harsh" "haste" "hatch" "hauls" "havoc" "hawks" "hazel" "heard"
        "hears" "heart" "heath" "hedge" "heirs" "hello" "helps" "herbs" "hewed" "hicks"
        "hides" "hikes" "hills" "hinge" "hints" "hippo"
        "hires" "hoard" "hobby" "holes" "holly" "homes"
        "hones" "honey" "honor" "hooks" "hoped" "horns" "horse"
        "hotel" "hound" "hours" "house" "hovel" "human" "humid" "humor"
        "hunch" "hunks" "hurry" "husky" "hyped"
        "ideal" "ideas" "image" "index" "inert" "infer" "infra" "ingot"
        "inlet" "ionic" "input" "irons" "irony" "issue" "items"
        "jails" "japan" "jeans" "jelly" "jerry" "jesse" "jewel" "joins" "joint" "jolly" "judge"
        "kappa" "kayak" "kills" "kinds" "kites" "kitty" "knock" "known"
        "labor" "lairs" "laity" "lakes" "lamed" "lands" "lanes" "lapse" "lards"
        "large" "laser" "lasts" "later" "latex" "lathe" "latte" "lauds" "laugh" "leads" "leafs"
        "leaks" "leaps" "learn" "lease" "least" "ledge" "leech" "leers" "lemur" "lends"
        "libel" "liber" "lifts" "liege" "light" "likes" "limbs"
        "limes" "limit" "liner" "lines" "links" "lints" "lions" "liter" "litho" "liver"
        "lives" "lobes" "locks" "locus" "lofty" "loins" "looks"
        "looms" "loose" "loses" "louse" "loved" "loves" "lower" "loyal" "lucid" "lucky" "luger" "luges" "lunar"
        "lunch" "lungs" "lures" "lying" "lyric"
        "macaw" "maced" "maids" "mails" "maize" "major" "males" "malls" "malty" "manes" "mange" "mania"
        "manor" "march" "marry" "masks" "mason" "mates" "maths" "mayor" "meant" "medal"
        "media" "mercy" "merge" "metal" "mewls" "micro" "midst" "miles" "mills" "minds" "miner" "mired"
        "misty" "mixer" "modal" "model" "modem" "modes" "moist"
        "molar" "molds" "monde" "money" "monks" "month"
        "moods" "moola" "moors" "moose" "moped" "moral" "morph" "moron" "motor" "mould"
        "mourn" "mouse" "mouth" "moved" "mucus" "muffs" "mules" "mural"
        "nance" "nasty" "navel" "nears" "neath" "necks" "needs"
        "needy" "nerve" "nests" "never" "newer" "niece"
        "ninth" "noise" "noisy" "nonce" "norms" "noses" "notes" "north" "nouns" "nurse" "nylon"
        "oaths" "obese" "ocean" "oddly" "odors" "often" "omega" "onion" "optic" "order"
        "organ" "otter" "outer" "ovary" "owned" "oxide" "ozone"
        "packs" "paddy" "padre" "pangs" "pages" "paint" "pairs"
        "paled" "pangs" "pares" "parks" "parse" "parts" "party"
        "parry" "paste" "pasty" "pales" "pants" "pasha" "passe"
        "paths" "pause" "paved" "peace" "peach" "peaks"
        "pearl" "pears" "pease" "peers" "penal" "penny" "peril"
        "perps" "pesos" "pests" "petty" "phase" "phone"
        "photo" "picks" "picky" "piece" "piers" "pines" "piled"
        "piler" "piles" "pills" "pinch" "pipes" "pivot" "pixel" "pizza" "plaid"
        "plain" "plane" "plans" "plant" "plate" "playa" "plaza" "plead"
        "pleas" "pleat" "plied" "plink" "plods" "plume" "plush" "poems"
        "point" "poise" "poles" "polis" "polyp" "pools" "popes" "porch" "ports" "posed"
        "poses" "posts" "pound" "poxes" "pries"
        "print" "privy" "probe" "prole" "prone" "prong" "proof" "prose" "proud" "prove"
        "prune" "puffy" "pulse" "punch" "pupil" "puppy" "purge" "purse"
        "quark" "quart" "queen" "quick" "quiet" "quilt" "quote"
        "radar" "radio" "raked" "rages" "rails" "rains" "rainy" "raise" "ranch" "randy"
        "ranks" "rapid" "raven" "razor" "reach" "react" "reads"
        "realm" "reams" "reads" "reeds" "reels" "refer" "reign" "reins" "renal" "renew"
        "repel" "reply" "reset" "rests" "rhyme" "rifle"
        "right" "rigid" "riled" "riley" "risks" "risky" "rival" "roads" "roast" "rocks" "rocky"
        "rogue" "rolls" "rooms" "roots" "ropes"
        "roses" "rotor" "rouge" "rough" "round" "route" "royal" "ruled" "ruler" "rugby" "ruins" "rupee" "rural"
        "sable" "sacks" "safer" "sages" "saint" "saith" "salad" "sales" "sally" "salon" "sands" "santo"
        "saran" "satin" "sauna" "saved" "saves" "savor" "scale" "scalp"
        "scarf" "scold" "scone" "scope" "score" "scout" "scots" "scrub"
        "seals" "sears" "seeks" "seems" "sense" "serum" "sever" "sewer" "shack" "shady" "shame"
        "share" "shark" "sharp" "shear" "sheds" "sheet" "shell"
        "shine" "shiny" "shirt" "shook" "shoot" "shots" "siege" "sighs" "silky"
        "silly" "since" "singe" "sings" "sinks" "sinus" "sixty" "sized" "sizes" "skier" "skies"
        "skirt" "slain" "slang" "sleet" "slews" "slick" "slime" "slope"
        "slump" "slung" "skull" "smack" "smith" "snack" "snake" "sneer" "sober" "socks"
        "soils" "solar" "solve" "sorry" "sound" "space" "spare" "spark" "spasm" "spear"
        "spell" "spent" "spies" "spike" "spine" "spire" "spite" "split"
        "spoil" "spook" "spool" "spoon" "spots" "spray" "stair" "stale"
        "stage" "stain" "stake" "stamp" "stand" "stank" "stare"
        "stars" "start" "stash" "state" "stays" "stead" "steal"
        "steak" "steam" "steep" "steer" "stein" "stick" "sting" "stink"
        "stint" "stock" "stole" "stomp" "stone" "stony" "stood" "stool" "store" "storm" "story" "stoic"
        "stout" "stray" "stuck" "style" "sunny" "super" "surge" "swamp" "swarm" "sweat"
        "sweet" "swell" "swept" "swill"
        "swine" "swore" "swung" "synod" "syrup"
        "table" "taboo" "tacos" "tails" "taken" "talks" "tales" "tanks" "tapes" "tapis" "tarot"
        "tarps" "tauts" "teach" "teams" "tears" "teens" "teeth" "telly" "temps" "tends" "tenor" "terse"
        "tests" "texts" "thank" "theme" "there" "these" "thick" "thief""thigh" "thine" "thing"
        "thorn" "those" "throw" "thuds" "tibia" "tides" "tiger"
        "tiled" "tiler" "timed" "timer" "times"
        "timid" "tints" "tired" "tires" "tithe" "today" "toils" "token" "tombs" "tomes"
        "tommy" "tonal" "toney" "tonic" "topic"
        "torch" "total" "totem" "tours" "towed" "tower" "towns" "toxic" "toxin" "trace" "trade"
        "trail" "trans" "traps" "trash" "trawl" "trays" "tread" "trend" "trial" "tribe" "trick" "tries"
        "trips" "troop" "trope" "trove" "truce" "truck" "trunk"
        "truth" "tulip" "tumor" "tuned" "tunes" "turns"
        "tutor" "tying" "tykes" "typed" "twigs" "twins"
        "ulcer" "undue" "unite" "until" "urged" "urine" "usage" "using" "usual" "usury" "utter"
        "vague" "valid" "valor" "value" "valve" "vapor" "veins"
        "venge" "venom" "venue" "venus" "verge" "verse"
        "vexed" "vices" "video" "views" "viper" "vigor" "viola"
        "viral" "virus" "vivid" "volts" "voted" "voter" "votes" "vowel"
        "wacks" "wails" "walls" "wards" "wares" "warns" "warps"
        "warty" "watch" "watts" "waves" "wavey" "waxed" "weans" "weeds"
        "wages" "walks" "wands" "waved" "weave" "wedge" "weeks" "weens"
        "weeps" "weigh" "welch" "wells" "welsh" "welts"
        "whale" "wharf" "wheat" "wheel" "where" "which" "while" "whine" "widow"
        "wight" "wills" "winds" "wings" "wiped" "wiper" "wired" "wires"
        "witch" "witty" "woman" "women" "wonks" "words" "world"
        "worry" "worse" "worst" "worth" "would" "wound" "woven" "wraps" "wrath" "wrest" "write" "wrong" "wrote"
        "yacht" "yeast" "yikes" "yours" "youth"
        ))

; Example usage.
(wordle
 '(
   ;; guess and mask pairs go here.
   )
 )
