(ns advent-of-code-2017.day-4-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-4 :refer :all]))

(deftest no-repetitions-test
  (testing "output"
    (is (= true (no-repetitions? '("aa" "bb" "cc" "dd" "ee"))))
    (is (= false (no-repetitions? '("aa" "bb" "cc" "dd" "aa"))))
    (is (= true (no-repetitions? '("aa" "bb" "cc" "dd" "aaa"))))))

(deftest part-1-soln
  (testing "truthiness"
    (is (= 386 (count-valid-passphrases "resources/day-4/input")))))

(deftest no-anagrams?-test
  (testing "output"
    (is (= true (no-anagrams? '("abcde" "fghij"))))
    (is (= false (no-anagrams? '("abcde" "xyz" "ecdab"))))
    (is (= true (no-anagrams? '("a" "ab" "abc" "abd" "abf" "abj"))))
    (is (= true (no-anagrams? '("iiii" "oiii" "ooii" "oooi" "oooo"))))
    (is (= false (no-anagrams? '("oiii" "ioii" "iioi" "iiio"))))))

(deftest part-2-soln
  (testing "truthiness"
    (is (= 208 (count-valid-passphrases no-anagrams? "resources/day-4/input")))))

;; abcde fghij is a valid passphrase.

;; abcde xyz ecdab is not valid - the letters from the third word can
;; be rearranged to form the first word.

;; a ab abc abd abf abj is a valid passphrase, because all letters
;; need to be used when forming another word.

;; iiii oiii ooii oooi oooo is valid.

;; oiii ioii iioi iiio is not valid - any of these words can be
;; rearranged to form any other word.

