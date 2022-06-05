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
        "abbey" "abled" "abide" "abode" "about" "abuse" "abyss" "acids" "acred" "acres" "acted"
        "actor" "added" "addle" "adept"
        "admit" "adopt" "adore" "adorn" "adult" "again" "agent"
        "agile" "agony" "agree" "ahead" "aided" "aisle"
        "alarm" "album" "alert" "algae" "alias" "alien" "align" "alike" "alive" "allot"
        "allow" "aloes" "aloft" "alone" "along" "aloof" "aloud" "altar" "alter"
        "amend" "amino" "among" "amour" "ample" "ankle" "anode"
        "anole" "aorta" "apart" "apply" "apres" "aptly" "arbor" "ardor" "arena" "argon" "arise"
        "armed" "armor" "arose" "arrow" "ashen" "asked" "askew" "assay" "atoll" "audio"
        "avail" "avert" "avoid" "awake" "aware" "awful" "awoke" "axial" "axiom"
        "badge" "badly" "bails" "bales" "balls" "bakes" "bands"
        "banes" "bangs" "banks" "bards" "barge" "basal" "basic" "basil"
        "basin" "bates" "baths" "baton" "bayou" "beams" "beans" "beard" "bears"
        "beast" "beats" "beaus" "beeps" "began" "begin" "begun" "belie" "belle" "bells" "below" "belts"
        "bench" "bends" "bendy" "berry" "beset" "besot" "bevel" "bezel" "bible" "biddy" "bilks"
        "bills" "billy" "bikes" "birch" "birds" "bison" "bites" "black" "blade" "blame"
        "bland" "blank" "blast" "blaze" "bleak" "bleat" "blend" "blimp" "blind"
        "bloat" "block" "bloke" "blond" "blood"
        "blown" "blows" "blurb" "blush" "boats" "boing"
        "bolts" "bombs" "bones" "boost" "booth" "booty" "boron" "bowls" "boxes"
        "brach" "brain" "brand" "brash" "brass" "brats" "brawl" "bread"
        "break" "breed" "bribe" "brick" "bride" "brief" "brine" "bring" "brink" "briny"
        "brisk" "broad" "broil" "broke" "brood" "brook" "broom" "broth"
        "brows" "brunt" "board" "bonus" "books" "bowel" "built"
        "bulbs" "bulky" "burns" "burnt" "buses" "busts" "butte" "bytes"
        "cabin" "cable" "cache" "cages" "cakes" "calls" "camel" "canal" "candy" "caner" "canid" "canny"
        "canon" "cards" "cared" "cargo" "carry" "carts" "casts" "cater" "catty" "cause"
        "ceded" "cells" "chaff" "chair" "chalk" "chard" "charm" "chart" "chase" "chaos"
        "chaps" "cheat" "check" "cheek" "chest" "chess" "chews" "chick" "chief" "child" "chile" "chill" "chime"
        "chips" "chuck" "cited" "clads" "claim" "clamp"
        "clams" "clans" "claps" "clash" "clasp" "claws" "clays"
        "clean" "clear" "click" "cling" "clone" "close" "cloth" "clots"
        "clove" "clown" "clues" "chimp" "china" "chins"
        "choke" "chord" "chore" "civic" "coast" "coats" "coded" "coeds" "coins" "colon" "comet" "comic" "comma"
        "cones" "congo" "conic" "cooly" "coral" "cords" "cores" "corns" "corps" "couch"
        "cough" "court" "crack" "craft" "crane" "crank" "crash"
        "crawl" "crazy" "creak" "creek" "crept" "crest" "crews" "cries" "crime" "crisp"
        "crook" "crops" "cross" "crowd" "crown" "crows" "cuffs" "cured" "curio" "curls" "curry" "curse" "cycle" "cysts"
        "daily" "daisy" "dance" "darts" "dates" "datum" "deals" "dealt"
        "death" "debit" "debut" "decal" "decay" "decks"
        "defer" "deity" "delay" "delta" "demon" "dense" "depot" "depth" "desks" "deter" "deuce" "devil" "devon"
        "diets" "dimes" "dimly" "dined" "diner"
        "dines" "ditch" "dizzy" "docks" "dodge" "doggy" "doles" "dolly" "domes" "donor" "donut"
        "doses" "doubt" "dowel" "dower" "dowry" "dozes"
        "drain" "drawn" "draws" "dread" "dried" "drier" "drift" "drill" "drink" "droid" "droit"
        "drops" "dross" "drove" "drown" "drunk" "ducks"
        "duels" "dummy" "dunce" "duped" "dusky" "dusty" "dwarf" "dwell" "dwelt"
        "eagle" "earls" "early" "earth" "ebony" "edema" "edict" "eerie"
        "eight" "eject" "elate" "elder" "elect" "elite" "elope"
        "email" "empty" "ended" "ender" "enemy" "enjoy"
        "ensue" "enter" "entry" "envoy" "epoch" "epoxy" "equal" "equip"
        "erect" "essay" "ether" "ethos" "event" "every" "evict" "evils" "exams" "expel" "extra" "exalt" "exult"
        "fable" "faces" "faced" "facts" "fails" "faint" "fairs" "fakes" "falls" "famed" "fancy"
        "fangs" "farce" "farms" "fatal" "fated" "fatty"
        "fauna" "fazes" "fears" "feast" "feats" "feint" "ferns"
        "ferry" "fewer" "ficus" "fiery" "fifth" "fight" "filth"
        "files" "filet" "final" "finch" "finds" "fined" "finer" "fines" "fires"
        "firms" "first" "fists" "fixed" "fixes" "fjord" "flags" "flair" "flaps"
        "flare" "flash" "flask" "flaws" "fleet" "flesh" "flick" "flies" "flock"
        "flood" "flora" "floss" "flour" "flout" "flown" "flows" "flues" "fluff" "fluid" "flush"
        "focal" "focus" "foist" "folia" "foods" "fools" "forge"
        "forty" "forum" "fouls" "found" "fowls" "frail" "frame"
        "frank" "fraud" "frays" "freed" "freer" "fresh"
        "fried" "fries" "frill" "fritz" "frond" "front" "frost"
        "froth" "froze" "fruit" "fudge" "funny" "furor" "furry" "fuzzy"
        "gaffe" "gaits" "galls" "gamma" "gangs" "gaped" "gauge" "gauze" "gavel"
        "gazed" "gears" "gecko" "geese" "genet" "genre" "genus"
        "germs" "ghost" "ghoul" "girls" "given" "gives" "glare"
        "glass" "glaze" "glide" "gloat" "gloom" "glory" "gloss"
        "glove" "glued" "glyph" "grips" "gross" "going" "gorge" "goyim" "grail"
        "grain" "grand" "grant" "grass" "grave" "gravy" "graze"
        "great" "greed" "greek" "greet" "grill" "gripe" "groan"
        "groom" "grout" "guard" "guava" "guess" "guide" "guile" "guilt" "gypsy"
        "habit" "hacks" "hairs" "hairy" "hands" "handy" "hangs" "hanky" "happy"
        "hards" "harem" "harps" "harsh" "haste" "hatch" "hated"
        "hauls" "haunt" "havoc" "hawks" "hazel" "heard"
        "hears" "heart" "heath" "heats" "hedge" "heirs" "helix" "hello" "helps" "herbs" "hewed" "hicks"
        "hides" "hikes" "hills" "hinge" "hints" "hippo"
        "hires" "hoard" "hobby" "holds" "holes" "holly" "homes"
        "hones" "honey" "honor" "hooks" "hoped" "horns" "horse"
        "hotel" "hound" "hours" "house" "hovel" "human" "humid" "humor"
        "hunch" "hunks" "hurry" "husky" "hydro" "hyena" "hyped"
        "ideal" "ideas" "idler" "idols" "igloo" "image" "imply" "index" "inept" "inert" "infer" "infra" "ingot"
        "inlet" "inter" "ionic" "input" "irons" "irony" "issue" "items"
        "jails" "japan" "jeans" "jelly" "jerry" "jesse" "jewel"
        "joins" "joint" "joist" "jolly" "joust" "judge" "juror"
        "kappa" "kayak" "kills" "kinds" "kites" "kitty" "knead" "kneed" "knife" "knock" "known"
        "labor" "lairs" "laity" "lakes" "lamed" "lambs" "lamps" "lands" "lanes" "lapel" "lapse" "lards"
        "large" "laser" "lasts" "later" "latex" "lathe" "latte" "lauds" "laugh" "leads" "leafs"
        "leaks" "leant" "leaps" "learn" "lease" "least" "ledge" "leech" "leers" "lemur" "lends"
        "libel" "liber" "lifts" "liege" "light" "likes" "limbs"
        "limes" "limit" "liner" "lines" "links" "lints" "lions" "liter" "litho" "liver"
        "lives" "loads" "loans" "lobes" "locks" "locus" "lofty" "logos" "loins" "looks"
        "looms" "loose" "loser" "loses" "louse" "loved" "loves" "lower"
        "lowly" "loyal" "lucid" "lucky" "luger" "luges" "lunar"
        "lunch" "lungs" "lures" "lying" "lyric"
        "macaw" "maced" "macro" "maids" "mails" "maize" "major" "males" "malls" "malty" "manes" "mange" "mania"
        "manor" "march" "marry" "masks" "mason" "mates" "maths" "mayor" "maxim" "means" "meant" "mecca" "medal"
        "media" "melon" "mercy" "merge" "metal" "metro" "mewls" "micro" "midst"
        "might" "miles" "mills" "milky" "minds" "mined" "miner" "mired" "miser"
        "misty" "mixer" "modal" "model" "modem" "modes" "moist"
        "molar" "molds" "moldy" "monde" "money" "monks" "month"
        "moods" "moola" "moors" "moose" "moped" "moral" "morph" "moron" "motor" "mould"
        "mourn" "mouse" "mouth" "moved" "movie" "moxie" "mucus" "muffs" "mules" "mural" "murky" "myths"
        "nance" "nasty" "natal" "navel" "nears" "neath" "necks" "needs"
        "needy" "nerve" "nests" "never" "newer" "niche" "niece"
        "ninth" "noise" "noisy" "nomad" "nonce" "norms" "noses" "notes"
        "north" "notch" "nouns" "nurse" "nylon" "nymph"
        "oaths" "obese" "ocean" "occur" "oddly" "odors" "often" "older" "omega" "onion" "optic" "order"
        "organ" "otter" "ounce" "outer" "ovary" "overt" "owned" "oxide" "ozone"
        "packs" "paddy" "padre" "palsy" "pangs" "pagan" "pages" "paint" "pairs"
        "paled" "panel" "pangs" "pares" "parks" "parse" "parts" "party"
        "parry" "pasta" "paste" "pasty" "pales" "pants" "parka" "pasha" "passe"
        "paths" "pause" "paved" "payee" "peace" "peach" "peaks"
        "pearl" "pears" "pease" "pecan" "peers" "penal" "penny" "peril"
        "perps" "pesos" "pests" "petal" "petty" "phase" "phone"
        "photo" "picks" "picky" "piece" "piers" "pines" "piled"
        "piler" "piles" "pills" "pilot" "pinch" "pipes" "pithy" "pivot" "pixel" "pixie" "pizza" "plaid"
        "plain" "plane" "plans" "plant" "plate" "playa" "plaza" "plead"
        "pleas" "pleat" "plied" "plink" "plods" "plots" "plume" "plush" "poems"
        "point" "poise" "poles" "polis" "polyp" "pools" "popes" "porch" "ports" "posed"
        "poses" "posts" "pound" "poxes" "prank" "preen" "pries" "primo"
        "print" "prior" "privy" "probe" "prole" "prone" "prong" "proof" "prose" "proud" "prove"
        "proxy" "prune" "puffy" "pulpy" "pulse" "punch" "pupil" "puppy" "purge" "purse"
        "quail" "quark" "quart" "queen" "quest" "quick" "quiet" "quilt" "quirk" "quote"
        "radar" "radio" "raked" "rages" "rails" "rains" "rainy" "raise" "rally" "ranch" "randy"
        "ranks" "rapid" "raven" "razor" "reach" "react" "reads" "ready"
        "realm" "reams" "reads" "recur" "reeds" "reels" "refer" "reign" "reins" "renal" "renew"
        "repel" "reply" "reset" "resin" "rests" "rhyme" "rifle"
        "right" "rigid" "riled" "riley" "rinse" "risen" "risks" "risky"
        "rival" "roads" "roast" "robot" "rocks" "rocky"
        "rogue" "rolls" "rooms" "roots" "ropes"
        "roses" "rotor" "rouge" "rough" "round" "rouse" "route"
        "royal" "ruder" "ruled" "ruler" "rugby" "ruins" "rupee" "rural"
        "sable" "sacks" "sadly" "safer" "sages" "saint"
        "saith" "salad" "sales" "sally" "salon" "sands" "santo"
        "saran" "satin" "sauna" "saute" "saved" "saves" "savor" "scald" "scale" "scalp"
        "scare" "scarf" "scold" "scone" "scope" "score" "scout" "scots" "scree" "screw" "scrub"
        "seals" "sears" "seedy" "seeks" "seems" "sells" "sense"
        "serum" "sever" "sewer" "shack" "shady" "shall" "shame"
        "share" "shark" "sharp" "shave" "shawl" "shear" "sheds" "sheet" "sheik" "shell" "shift"
        "shine" "shiny" "shirt" "shook" "shoot" "shots"
        "shout" "shown" "showy" "shunt" "shush" "siege" "sighs" "sight" "silky"
        "silly" "since" "singe" "sings" "sinks" "sinus" "sites" "sixty" "sized" "sizes" "skier" "skies"
        "skimp" "skirt" "slabs" "slain" "slang" "sleep" "sleet" "slews" "slick" "slide" "slime" "slope" "slosh"
        "slump" "slung" "skull" "smack" "smell" "smile" "smith" "snack" "snake" "sneer" "soapy" "sober" "socks"
        "soils" "solar" "solid" "solve" "sooty" "sorry" "sorts" "sound"
        "space" "spade" "spare" "spark" "spasm" "speak" "spear"
        "spell" "spent" "spies" "spike" "spine" "spire" "spite" "split"
        "spoil" "spook" "spool" "spoon" "spots" "spout" "spray" "squad" "stack" "staff" "stair" "stale" "stalk"
        "stage" "staid" "stain" "stake" "stamp" "stand" "stank" "stare"
        "stars" "start" "stash" "state" "stays" "stead" "steal"
        "steak" "steam" "steep" "steer" "stein" "stick" "still" "sting" "stink"
        "stint" "stock" "stole" "stomp" "stone" "stony" "stood" "stool" "store" "storm" "story" "stoic"
        "stout" "stove" "stray" "stuck" "study" "style"
        "sunny" "super" "surge" "surly" "swamp" "swarm" "sweat"
        "sweet" "swell" "swept" "swill"
        "swine" "swing" "swore" "sworn" "swung" "synod" "syrup"
        "table" "taboo" "tacos" "tails" "taken" "talks" "tales" "tanks" "tapes" "tapis" "tarot"
        "tarps" "taste" "tauts" "teach" "teams" "tears" "tease"
        "teens" "teeth" "telly" "tempo" "temps" "tempt" "tends" "tenor" "terra" "terse"
        "tests" "texts" "thank" "their" "theme" "there" "these" "thick" "thief""thigh" "thine" "thing" "think"
        "thorn" "those" "throw" "thuds" "tiara" "tibia" "tides" "tiger"
        "tiled" "tiler" "timed" "timer" "times"
        "timid" "tints" "tired" "tires" "titan" "tithe" "today" "toils" "token" "tombs" "tomes"
        "tommy" "tonal" "toney" "tonic" "topic"
        "torch" "total" "totem" "tough" "tours" "towed"
        "tower" "towns" "toxic" "toxin" "trace" "trade"
        "trail" "train" "tramp" "trans" "traps" "trash" "trawl"
        "trays" "tread" "trend" "trial" "tribe" "trick" "tries"
        "trips" "troop" "trope" "trove" "truce" "truck" "trunk"
        "truth" "tulip" "tumor" "tuned" "tunes" "turns"
        "tutor" "tying" "tykes" "typed" "twigs" "twins"
        "udder" "ulcer" "undue" "unite" "until" "upset" "urged"
        "urine" "usage" "using" "usual" "usury" "utile" "utter"
        "vague" "valid" "valor" "value" "valve" "vapor" "veins"
        "venge" "venom" "venue" "venus" "verge" "verse"
        "vexed" "vicar" "vices" "video" "views" "viper" "vigor" "viola"
        "viral" "virus" "vivid" "vogue" "volts" "voted" "voter" "votes" "vowel"
        "wacks" "wages" "wails" "waits" "walls" "walks" "wands" "wards" "wares" "warns" "warps"
        "warty" "watch" "water" "watts" "waves" "wavey" "waxed" "weans" "weeds"
        "waved" "weave" "wedge" "weeks" "weens"
        "weeps" "weigh" "welch" "wells" "welsh" "welts"
        "whale" "wharf" "wheat" "wheel" "where" "which" "while" "whine" "widow"
        "wight" "wills" "winds" "wings" "wiped" "wiper" "wired" "wires"
        "witch" "witty" "woman" "women" "wonks" "woods" "woody" "words" "world"
        "worry" "worse" "worst" "worth" "would" "wound" "woven" "wraps" "wrath" "wrest" "write" "wrong" "wrote"
        "yacht" "yeast" "yield" "yikes" "young" "yours" "youth"
        "zesty"
        ))

; Example usage.
(wordle
 '(
   ;; guess and mask pairs go here.

   )
 )
