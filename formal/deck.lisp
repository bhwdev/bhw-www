; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
; SPDX-License-Identifier: AGPL-3.0-or-later

; Proofs about DECK-NEXT of src/acl2/deck.lisp, which bhw/plp/song's
; DECK-NEXT runs on the identities of a deck's slides (section, part).
;
; Proved:
;   1. when X is in DECK, DECK-NEXT gives an index at which X stands;
;   2. when X stands after INDEX, the result is the first such place after
;      INDEX (no earlier place is skipped);
;   3. when it stands only up to INDEX, the result is the first place in the
;      deck (the wrap-around);
;   4. NIL exactly when X is not in DECK.

(in-package "BHW/ACL2")

(include-book "../src/acl2/deck")

(defthm index-from-member
  (implies (member-equal x l)
           (index-from x l 0))
  :hints (("Goal" :induct (index-from x l 0))))

(defthm index-from-is-x
  (implies (index-from x l k)
           (equal (nth (index-from x l k) l) x)))

(defthm index-from-implies-member
  (implies (index-from x l k)
           (member-equal x l))
  :hints (("Goal" :induct (index-from x l k))))

(defthm index-from-at-least-k
  (implies (and (index-from x l k) (natp k))
           (<= k (index-from x l k))))

(defthm index-from-first
  (implies (and (natp k) (natp j) (<= k j) (< j (len l))
                (equal (nth j l) x))
           (and (index-from x l k)
                (<= (index-from x l k) j))))

(defthm deck-next-stands
  (implies (member-equal x deck)
           (equal (nth (deck-next deck index x) deck) x)))

(defthm deck-next-nil-iff
  (equal (null (deck-next deck index x))
         (not (member-equal x deck))))

(defthm deck-next-after-index
  (implies (and (natp index) (natp j) (< index j) (< j (len deck))
                (equal (nth j deck) x))
           (and (< index (deck-next deck index x))
                (<= (deck-next deck index x) j)))
  :hints (("Goal" :do-not-induct t
                  :in-theory (disable index-from-first index-from-at-least-k)
                  :use ((:instance index-from-first (l deck) (k (+ 1 index)))
                        (:instance index-from-at-least-k (l deck) (k (+ 1 index)))))))

(defthm deck-next-wraps
  (implies (and (member-equal x deck)
                (natp index)
                (not (index-from x deck (+ 1 index))))
           (equal (deck-next deck index x)
                  (index-from x deck 0))))
