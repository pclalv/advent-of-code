(ns advent-of-code-2017.day-7)
(require 'clojure.set)

;; --- Day 7: Recursive Circus ---
;; Wandering further through the circuits of the computer, you come
;; upon a tower of programs that have gotten themselves into a bit of
;; trouble. A recursive algorithm has gotten out of hand, and now
;; they're balanced precariously in a large tower.

;; One program at the bottom supports the entire tower. It's holding a
;; large disc, and on the disc are balanced several more
;; sub-towers. At the bottom of these sub-towers, standing on the
;; bottom disc, are other programs, each holding their own disc, and
;; so on. At the very tops of these sub-sub-sub-...-towers, many
;; programs stand simply keeping the disc below them balanced but with
;; no disc of their own.

;; You offer to help, but first you need to understand the structure
;; of these towers. You ask each program to yell out their name, their
;; weight, and (if they're holding a disc) the names of the programs
;; immediately above them balancing on that disc. You write this
;; information down (your puzzle input). Unfortunately, in their
;; panic, they don't do this in an orderly fashion; by the time you're
;; done, you're not sure which program gave which information.

;; For example, if your list is the following:

;; pbga (66)
;; xhth (57)
;; ebii (61)
;; havc (66)
;; ktlj (57)
;; fwft (72) -> ktlj, cntj, xhth
;; qoyq (66)
;; padx (45) -> pbga, havc, qoyq
;; tknk (41) -> ugml, padx, fwft
;; jptl (61)
;; ugml (68) -> gyxo, ebii, jptl
;; gyxo (61)
;; cntj (57)
;; ...then you would be able to recreate the structure of the towers that looks like this:

;;                   gyxo
;;                 /
;;           ugml - ebii
;;           /     \
;;          |       jptl
;;         |
;;        |         pbga
;;       /        /
;; tknk --- padx - havc
;;       \        \
;;        |        qoyq
;;        |
;;        |        ktlj
;;        \      /
;;         fwft - cntj
;;               \
;;                xhth

;; In this example, tknk is at the bottom of the tower (the bottom
;; program), and is holding up ugml, padx, and fwft. Those programs
;; are, in turn, holding up other programs; in this example, none of
;; those programs are holding up any other programs, and are all the
;; tops of their own towers. (The actual tower balancing in front of
;; you is much larger.)

;; Before you're ready to help them, you need to make sure your
;; information is correct. What is the name of the bottom program?

(def program-children-delimiter-re #" -> ")
(def child-delimiter-re #", ")
(def program-name-weight-re #"(\w+)\s\((\d+)\)")
(def path-prefix "resources/day-7/")

(defn parse-program-line [program-line]
  (let [[program-raw children-raw] (-> program-line
                                       (clojure.string/split program-children-delimiter-re))
        [_ program_name weight] (re-find program-name-weight-re program-raw)]
    (if children-raw
      {:program-name program_name
       :weight (Integer/parseInt weight)
       :children (->> (clojure.string/split children-raw child-delimiter-re)
                      (apply sorted-set))}
      {:program-name program_name
       :weight (Integer/parseInt weight)
       :children #{}})))

(defn parse-program-file-contents [program-file-contents]
  (map parse-program-line
       program-file-contents))

(defn read-program-file [file]
  (let [resource-path (str "resources/day-7/" file)]
    (with-open [rdr (clojure.java.io/reader resource-path)]
      (doall (->> rdr
                  (line-seq)
                  (map parse-program-line))))))

(defn find-root-program [filename]
  (let [programs (read-program-file filename)
        parent-programs (filter #(->> %
                                      (:children)
                                      (empty?)
                                      (not))
                                programs)
        child-programs (->> parent-programs
                            (map :children)
                            (apply clojure.set/union))
        root-program (first (filter #(not (contains? child-programs %))
                                    (map :program-name parent-programs)))]
    root-program))

;; --- Part Two ---
;; The programs explain the situation: they can't get down. Rather,
;; they could get down, if they weren't expending all of their energy
;; trying to keep the tower balanced. Apparently, one program has the
;; wrong weight, and until it's fixed, they're stuck here.

;; For any program holding a disc, each program standing on that disc
;; forms a sub-tower. Each of those sub-towers are supposed to be the
;; same weight, or the disc itself isn't balanced. The weight of a
;; tower is the sum of the weights of the programs in that tower.

;; In the example above, this means that for ugml's disc to be
;; balanced, gyxo, ebii, and jptl must all have the same weight, and
;; they do: 61.

;; However, for tknk to be balanced, each of the programs standing on
;; its disc and all programs above it must each match. This means that
;; the following sums must all be the same:

;; ugml + (gyxo + ebii + jptl) = 68 + (61 + 61 + 61) = 251
;; padx + (pbga + havc + qoyq) = 45 + (66 + 66 + 66) = 243
;; fwft + (ktlj + cntj + xhth) = 72 + (57 + 57 + 57) = 243

;; As you can see, tknk's disc is unbalanced: ugml's stack is heavier
;; than the other two. Even though the nodes above ugml are balanced,
;; ugml itself is too heavy: it needs to be 8 units lighter for its
;; stack to weigh 243 and keep the towers balanced. If this change
;; were made, its weight would be 60.

;; Given that exactly one program is the wrong weight, what would its
;; weight need to be to balance the entire tower?

;; what if we assoc-parent, and then take all of the children who are
;; not parents; then, beginning with those children we traverse back through the parents and add that child's weight to the 

(defn children-of [potential-children]
  (fn [parent]
    (let [children-of-parent (:children parent)
          parent-name (:program-name parent)]
      (->> potential-children
           (filter #(contains? children-of-parent
                               (:program-name %)))
           (map #(assoc % :parent-name parent-name))))))

(defn assoc-weight-of-children [all-programs]
  (fn [parent]
    (let [children ((children-of all-programs) parent)
          children-weight-of-childrens (map (assoc-weight-of-children all-programs) children)
          weight-of-children (->> children-weight-of-childrens
                                  (map #(select-keys % '(:weight :weight-of-children)))
                                  (map vals)
                                  (flatten)
                                  (reduce +))]
      (assoc parent :weight-of-children weight-of-children))))

(defn assoc-total-weight [program]
  (assoc program :total-weight (+ (:weight program)
                                  (or (:weight-of-children program)
                                      0))))

(defn find-unbalanced [programs]
  (let [total-weight-counts (->> programs
                                 (group-by :total-weight)
                                 (map (fn [[total-weight programs]]
                                        [total-weight (count programs)])))
        unalike-total-weight (->> total-weight-counts
                                  (filter (fn [[total-weight count-programs]]
                                            (= 1 count-programs)))
                                  (first)
                                  (first))
        common-total-weight (->> total-weight-counts
                                  (filter (fn [[total-weight count-programs]]
                                            (not (= 1 count-programs))))
                                  (first)
                                  (first))
        correction (- common-total-weight unalike-total-weight)
        unbalanced-program (->> programs
                                (filter #(= unalike-total-weight (:total-weight %)))
                                (first))
        corrected-weight (->> unbalanced-program
                              (:weight)
                              (+ correction))
        unbalanced-program' (assoc unbalanced-program :corrected-weight corrected-weight)]
    unbalanced-program'))

(defn balanced? [all-programs root-program]
  (let [children ((children-of all-programs) root-program)]
    (if (empty? children)
      true
      (let [children-balanced? (every? (partial balanced? all-programs) children)
            children-total-weights-equal? (->> children
                                               (map assoc-total-weight)
                                               (map :total-weight)
                                               (apply =))]
        (if (and children-balanced? children-total-weights-equal?)
          true
          false)))))

(defn find-unbalanced' [all-programs parents]
  (let [children (->> parents
                      (map (children-of all-programs))
                      (flatten))]
    (if (every? (partial balanced? all-programs) parents)
      (let [grandparent-names (->> parents
                                   (map :parent-name)
                                   (set))
            grandparents (filter #(contains? grandparent-names
                                             (:program-name %))
                                 all-programs)
            unbalanced-grandparent (->> grandparents
                                        (filter #(not (balanced? all-programs %)))
                                        (first))
            unbalanced-parents (->> parents
                                    (filter #(= (:program-name unbalanced-grandparent)
                                                (:parent-name %))))]
        (find-unbalanced unbalanced-parents))
      (recur all-programs children))))

(defn balance-tower [file]
  (let [programs' (->> file
                       (read-program-file))
        assoc-weight-of-children' (assoc-weight-of-children programs')
        programs (->> programs'
                      (map assoc-weight-of-children')
                      (map assoc-total-weight))
        root-program-name (find-root-program file)
        root-program (->> programs
                          (filter #(= root-program-name
                                      (:program-name %))))]
    (find-unbalanced' programs root-program)))

(def unbalanced-tier '({:program-name "hipirzc", :weight 989, :children #{"geibkdq" "hhzjn" "mpxfrig" "oilsjpr" "vpmtex" "xzjdfbr"}, :weight-of-children 1440, :total-weight 2429, :parent-name "alywuv"}
                       {:program-name "cczfm", :weight 749, :children #{"pnghx" "putdqt" "sbcyifk" "tusimd" "uxblgk"}, :weight-of-children 1680, :total-weight 2429, :parent-name "alywuv"}
                       {:program-name "koxzwq", :weight 1633, :children #{"bmwankt" "geuopn" "qmdbgke" "ransdtt"}, :weight-of-children 796, :total-weight 2429, :parent-name "alywuv"}
                       {:program-name "mlhga", :weight 83, :children #{"enswibe" "oywnenv" "qfitmrm" "qshflsm" "tefmun" "vcgyi"}, :weight-of-children 2346, :total-weight 2429, :parent-name "alywuv"}
                       {:program-name "awddb", :weight 1643, :children #{"fhyfd" "npsuo" "rrrxeic"}, :weight-of-children 786, :total-weight 2429, :parent-name "alywuv"}
                       {:program-name "qqktjg", :weight 1859, :children #{"cjxuj" "pbeegf" "pnmkw"}, :weight-of-children 570, :total-weight 2429, :parent-name "alywuv"}
                       {:program-name "odzzie", :weight 1585, :children #{"aikmwxj" "ixlern" "qmhaa" "yffid"}, :weight-of-children 844, :total-weight 2429, :parent-name "alywuv"}
                       {:program-name "vjvjnc", :weight 988, :children #{"comyzns" "hoyipx" "qoxsu"}, :weight-of-children 645, :total-weight 1633, :parent-name "mnyng"}
                       {:program-name "clxlfgu", :weight 86, :children #{"crxuc" "jzjxhyw" "kbjpgv" "lhhlg" "vuldvce" "yciccp" "zwaky"}, :weight-of-children 1547, :total-weight 1633, :parent-name "mnyng"}
                       {:program-name "plvjk", :weight 1222, :children #{"ksvwjw" "rwaeimg" "uglvj"}, :weight-of-children 411, :total-weight 1633, :parent-name "mnyng"}
                       {:program-name "ykshd", :weight 464, :children #{"kjljvr" "qufymlj" "simlhnw"}, :weight-of-children 993, :total-weight 1457, :parent-name "rhsztrd"}
                       {:program-name "xykcof", :weight 1152, :children #{"jfhqmi" "kivlu" "ptmqqpk" "vvkgsba" "zfotzg"}, :weight-of-children 305, :total-weight 1457, :parent-name "rhsztrd"}
                       {:program-name "etdmt", :weight 35, :children #{"ahuboc" "egcyvrv" "emsacsk" "emvml" "ffyfj" "srjbcye"}, :weight-of-children 1422, :total-weight 1457, :parent-name "rhsztrd"}
                       {:program-name "psblh", :weight 90, :children #{"kfhcuvf" "uspzk" "wxakaqf"}, :weight-of-children 831, :total-weight 921, :parent-name "nvdoenr"}
                       {:program-name "coxtcw", :weight 153, :children #{"jdfrbhy" "orulm" "xiksvn"}, :weight-of-children 768, :total-weight 921, :parent-name "nvdoenr"}
                       {:program-name "foyabk", :weight 390, :children #{"iejyzgu" "iieig" "onwqbz"}, :weight-of-children 531, :total-weight 921, :parent-name "nvdoenr"}
                       {:program-name "moxiw", :weight 184, :children #{"cqtkhqi" "hughxts" "kvvjhrt" "nfbcs" "oagvuu" "pohqjzd" "qzzhap"}, :weight-of-children 1631, :total-weight 1815, :parent-name "jfdck"}
                       {:program-name "marnqj", :weight 1283, :children #{"mkrrlbv" "upair" "vqkwlq" "wsrmfr"}, :weight-of-children 540, :total-weight 1823, :parent-name "jfdck"}
                       {:program-name "sxijke", :weight 987, :children #{"dlzlo" "jlsxjq" "jmazezm"}, :weight-of-children 828, :total-weight 1815, :parent-name "jfdck"}
                       {:program-name "fnlapoh", :weight 1284, :children #{"foldo" "xlxct" "yxljri"}, :weight-of-children 531, :total-weight 1815, :parent-name "jfdck"}
                       {:program-name "ojgdow", :weight 15, :children #{"boipij" "bzznyu" "gwgnf" "otmpv" "wdjgdoh" "wiqsy"}, :weight-of-children 1800, :total-weight 1815, :parent-name "jfdck"}
                       {:program-name "rlsnxvu", :weight 628, :children #{"tfrjlk" "wnvmb" "xdkin"}, :weight-of-children 705, :total-weight 1333, :parent-name "szmnwnx"}
                       {:program-name "nnoaqvv", :weight 433, :children #{"frklhh" "isixko" "ugeudg"}, :weight-of-children 900, :total-weight 1333, :parent-name "szmnwnx"}
                       {:program-name "mgayiu", :weight 93, :children #{"bhtwp" "euueza" "ksvdw" "pvnjsr" "txfnjo"}, :weight-of-children 1240, :total-weight 1333, :parent-name "szmnwnx"}
                       {:program-name "kupsxf", :weight 724, :children #{"klvncy" "ygbecwq" "zxtwo"}, :weight-of-children 609, :total-weight 1333, :parent-name "szmnwnx"}
                       {:program-name "xmhil", :weight 37, :children #{"gmktbew" "gzrmt" "wloqry"}, :weight-of-children 222, :total-weight 259, :parent-name "fqvvrgx"}
                       {:program-name "dbzwyez", :weight 195, :children #{"kocwei" "lurly" "mcvws" "xdraxfj"}, :weight-of-children 64, :total-weight 259, :parent-name "fqvvrgx"}
                       {:program-name "bmhlcgg", :weight 103, :children #{"mpuhqjm" "yoabvhm"}, :weight-of-children 156, :total-weight 259, :parent-name "fqvvrgx"}
                       {:program-name "krgdzw", :weight 70, :children #{"dfnijm" "fjiemrs" "gvtnin" "jtejd" "oxvcp" "sdllj" "yzvvyb"}, :weight-of-children 2016, :total-weight 2086, :parent-name "arjsnz"}
                       {:program-name "rwrusdg", :weight 1351, :children #{"abmlupq" "atkstsn" "efsbeb"}, :weight-of-children 735, :total-weight 2086, :parent-name "arjsnz"}
                       {:program-name "olhbx", :weight 1384, :children #{"ljeztky" "xvpopw" "zlmvme"}, :weight-of-children 702, :total-weight 2086, :parent-name "arjsnz"}
                       {:program-name "dapjjl", :weight 26, :children #{"aqksi" "dgehtvc" "elxydx" "ghlkk" "pbxzzdi" "rnjqmh"}, :weight-of-children 1554, :total-weight 1580, :parent-name "thuzqqo"}
                       {:program-name "lvbjtf", :weight 1466, :children #{"exuelye" "kuqnndb"}, :weight-of-children 114, :total-weight 1580, :parent-name "thuzqqo"}
                       {:program-name "bijhyzu", :weight 368, :children #{"chpjfv" "gkgka" "jdvfkii" "wgnoil"}, :weight-of-children 1212, :total-weight 1580, :parent-name "thuzqqo"}
                       {:program-name "wnmbn", :weight 13, :children #{"egoezf" "nodgjd" "uicpy" "vrrmn"}, :weight-of-children 1548, :total-weight 1561, :parent-name "bbeclr"}
                       {:program-name "jipgo", :weight 772, :children #{"klkodi" "qgptpig" "tcclaa"}, :weight-of-children 789, :total-weight 1561, :parent-name "bbeclr"}
                       {:program-name "zwqgf", :weight 661, :children #{"bxdxjoa" "obbei" "pcqhhy" "xqvmqe"}, :weight-of-children 900, :total-weight 1561, :parent-name "bbeclr"}
                       {:program-name "sxbee", :weight 32, :children #{"cafxx" "fplzsya" "hweub" "xhymdo" "yftqma" "yhxkh"}, :weight-of-children 1506, :total-weight 1538, :parent-name "fwlspf"}
                       {:program-name "qigsvd", :weight 218, :children #{"gmpoe" "ixszebq" "wdycxsw" "ymqmr"}, :weight-of-children 1320, :total-weight 1538, :parent-name "fwlspf"}
                       {:program-name "xxiymn", :weight 110, :children #{"bqfsjz" "diila" "fvtmh" "lgtpuqr" "ryrtqj" "yikzq"}, :weight-of-children 1428, :total-weight 1538, :parent-name "fwlspf"}
                       {:program-name "qpoodx", :weight 1001, :children #{"adwzzi" "ghebwaz" "gqzczp"}, :weight-of-children 537, :total-weight 1538, :parent-name "fwlspf"}
                       {:program-name "ubvvr", :weight 288, :children #{"hrpzu" "msjeeks" "ozizlok"}, :weight-of-children 1035, :total-weight 1323, :parent-name "mhfci"}
                       {:program-name "fqqzx", :weight 669, :children #{"fafvyk" "syrnxgz" "xofjd"}, :weight-of-children 654, :total-weight 1323, :parent-name "mhfci"}
                       {:program-name "pegoi", :weight 27, :children #{"fwxtdml" "kxpffcy" "qsjwlq" "tfmvb"}, :weight-of-children 1296, :total-weight 1323, :parent-name "mhfci"}
                       {:program-name "katovn", :weight 559, :children #{"aonrg" "ffizjl" "nbbctar" "ondvq"}, :weight-of-children 1104, :total-weight 1663, :parent-name "bqzfjn"}
                       {:program-name "jgckfb", :weight 568, :children #{"bcjpkwf" "dvuug" "iwcpvdx" "mzhyo" "ungcoot"}, :weight-of-children 1095, :total-weight 1663, :parent-name "bqzfjn"}
                       {:program-name "txywrwk", :weight 787, :children #{"indhpfx" "lbflpug" "sxcxmg"}, :weight-of-children 876, :total-weight 1663, :parent-name "bqzfjn"}
                       {:program-name "fklkig", :weight 1573, :children #{"owhxvku" "ykcmvla" "yrdpurl"}, :weight-of-children 90, :total-weight 1663, :parent-name "bqzfjn"}
                       {:program-name "vwqquk", :weight 25, :children #{"gvzze" "kndpli" "ouxdvp" "plevjup" "qtbnqo" "sqauq" "ythoww"}, :weight-of-children 1638, :total-weight 1663, :parent-name "bqzfjn"}
                       {:program-name "tpvcmkz", :weight 1111, :children #{"dtyaj" "xwmjpb" "zqkgqs"}, :weight-of-children 552, :total-weight 1663, :parent-name "bqzfjn"}
                       {:program-name "xjsyk", :weight 217, :children #{"qylvdk" "zhjzpaa"}, :weight-of-children 40, :total-weight 257, :parent-name "myhfh"}
                       {:program-name "howpl", :weight 191, :children #{"qlkhbf" "rnqtg"}, :weight-of-children 66, :total-weight 257, :parent-name "myhfh"}
                       {:program-name "upvrm", :weight 79, :children #{"effjvcu" "uwrqxmj"}, :weight-of-children 178, :total-weight 257, :parent-name "myhfh"}
                       {:program-name "feuirvn", :weight 64, :children #{"auzdz" "futsmzx" "hlkvwq" "jpvyg" "mibwe" "ufses" "yjlzpv"}, :weight-of-children 2562, :total-weight 2626, :parent-name "fzerj"}
                       {:program-name "vxbzw", :weight 1698, :children #{"bzjze" "lsxnn" "owprin" "uosltv"}, :weight-of-children 928, :total-weight 2626, :parent-name "fzerj"}
                       {:program-name "rklic", :weight 832, :children #{"gedajwo" "ilsqjz" "olntlo" "oprsej" "tdqxy" "ylfgaps"}, :weight-of-children 1794, :total-weight 2626, :parent-name "fzerj"}
                       {:program-name "rzvrn", :weight 1852, :children #{"bkzzc" "ioyiklx" "isbppt"}, :weight-of-children 774, :total-weight 2626, :parent-name "fzerj"}
                       {:program-name "sqdhc", :weight 79, :children #{"cwoyko" "eqpuq" "mdyhk" "pfyexc" "zscqv" "zspohe"}, :weight-of-children 1956, :total-weight 2035, :parent-name "wustjt"}
                       {:program-name "quinb", :weight 1927, :children #{"jnojpf" "qtzor"}, :weight-of-children 108, :total-weight 2035, :parent-name "wustjt"}
                       {:program-name "kcekden", :weight 1504, :children #{"fmvdadz" "pmakzzf" "zgbzqf"}, :weight-of-children 531, :total-weight 2035, :parent-name "wustjt"}
                       {:program-name "kzeubgd", :weight 270, :children #{"kxouhtb" "wrcpxdx" "wwritm"}, :weight-of-children 735, :total-weight 1005, :parent-name "zwwrwfy"}
                       {:program-name "corqny", :weight 57, :children #{"cakpt" "crlgf" "gzoyqay" "xkfuz"}, :weight-of-children 948, :total-weight 1005, :parent-name "zwwrwfy"}
                       {:program-name "sbxkokp", :weight 341, :children #{"gxzgsaa" "sjyso" "snxgfl" "unllgjg"}, :weight-of-children 664, :total-weight 1005, :parent-name "zwwrwfy"}
                       {:program-name "fquyicp", :weight 113, :children #{"fudpn" "fyptb" "sfbpnnz" "ymrfvp"}, :weight-of-children 892, :total-weight 1005, :parent-name "zwwrwfy"}
                       {:program-name "zjxspcp", :weight 1244, :children #{"pqjrycz" "sdstw" "tiwxhoa"}, :weight-of-children 411, :total-weight 1655, :parent-name "tgqnyb"}
                       {:program-name "rtngius", :weight 83, :children #{"boxtod" "iceaj" "igrih" "kqecnz" "tgxuhxp" "wpwlg"}, :weight-of-children 1572, :total-weight 1655, :parent-name "tgqnyb"}
                       {:program-name "uqgco", :weight 161, :children #{"errrzm" "mvwcdq" "mynuy" "ocmuu" "uvogit" "uwuouhv"}, :weight-of-children 1494, :total-weight 1655, :parent-name "tgqnyb"}
                       {:program-name "wfuoll", :weight 1715, :children #{"axlitg" "cciwzc" "qpafkjx"}, :weight-of-children 978, :total-weight 2693, :parent-name "whgqhb"}
                       {:program-name "nqehtaf", :weight 1857, :children #{"iouczne" "krzuab" "mmdrry" "vkmtnn"}, :weight-of-children 836, :total-weight 2693, :parent-name "whgqhb"}
                       {:program-name "wnshgr", :weight 1978, :children #{"afjqx" "avzxg" "mqgculh" "pzxfm" "ylmixxx"}, :weight-of-children 715, :total-weight 2693, :parent-name "whgqhb"}
                       {:program-name "cmnjg", :weight 843, :children #{"awpyosw" "hhdth" "kdmuzrx" "ompzpgp" "ovvmrux"}, :weight-of-children 1850, :total-weight 2693, :parent-name "whgqhb"}
                       {:program-name "olqgu", :weight 53, :children #{"bajtat" "igiqnkc" "ocobkpo" "tobzv" "walei" "zglws"}, :weight-of-children 2640, :total-weight 2693, :parent-name "whgqhb"}
                       {:program-name "bymyuke", :weight 511, :children #{"dadlbnn" "dlluqgz" "htwkr"}, :weight-of-children 813, :total-weight 1324, :parent-name "ytbnpd"}
                       {:program-name "rdihhfv", :weight 408, :children #{"dqury" "mskbq" "ozjka" "qrpis"}, :weight-of-children 916, :total-weight 1324, :parent-name "ytbnpd"}
                       {:program-name "uohmo", :weight 432, :children #{"qzeljtb" "stflbsr" "ustkg" "zznkz"}, :weight-of-children 892, :total-weight 1324, :parent-name "ytbnpd"}
                       {:program-name "ogpunov", :weight 34, :children #{"eqsrff" "tszcogp" "uwaxli" "wugret" "zdljb" "zgmrmd"}, :weight-of-children 1290, :total-weight 1324, :parent-name "ytbnpd"}
                       {:program-name "vetgu", :weight 556, :children #{"ssbjh" "wohob" "yierd"}, :weight-of-children 768, :total-weight 1324, :parent-name "ytbnpd"}
                       {:program-name "ctdey", :weight 628, :children #{"aezwvo" "ruxqapz" "vdpnz"}, :weight-of-children 696, :total-weight 1324, :parent-name "ytbnpd"}))

(def unbalanced-tier-children '({:program-name "hipirzc", :weight 989, :children #{"geibkdq" "hhzjn" "mpxfrig" "oilsjpr" "vpmtex" "xzjdfbr"}, :weight-of-children 1440, :total-weight 2429, :parent-name "alywuv"}
                                {:program-name "cczfm", :weight 749, :children #{"pnghx" "putdqt" "sbcyifk" "tusimd" "uxblgk"}, :weight-of-children 1680, :total-weight 2429, :parent-name "alywuv"}
                                {:program-name "koxzwq", :weight 1633, :children #{"bmwankt" "geuopn" "qmdbgke" "ransdtt"}, :weight-of-children 796, :total-weight 2429, :parent-name "alywuv"}
                                {:program-name "mlhga", :weight 83, :children #{"enswibe" "oywnenv" "qfitmrm" "qshflsm" "tefmun" "vcgyi"}, :weight-of-children 2346, :total-weight 2429, :parent-name "alywuv"}
                                {:program-name "awddb", :weight 1643, :children #{"fhyfd" "npsuo" "rrrxeic"}, :weight-of-children 786, :total-weight 2429, :parent-name "alywuv"}
                                {:program-name "qqktjg", :weight 1859, :children #{"cjxuj" "pbeegf" "pnmkw"}, :weight-of-children 570, :total-weight 2429, :parent-name "alywuv"}
                                {:program-name "odzzie", :weight 1585, :children #{"aikmwxj" "ixlern" "qmhaa" "yffid"}, :weight-of-children 844, :total-weight 2429, :parent-name "alywuv"}
                                {:program-name "vjvjnc", :weight 988, :children #{"comyzns" "hoyipx" "qoxsu"}, :weight-of-children 645, :total-weight 1633, :parent-name "mnyng"}
                                {:program-name "clxlfgu", :weight 86, :children #{"crxuc" "jzjxhyw" "kbjpgv" "lhhlg" "vuldvce" "yciccp" "zwaky"}, :weight-of-children 1547, :total-weight 1633, :parent-name "mnyng"}
                                {:program-name "plvjk", :weight 1222, :children #{"ksvwjw" "rwaeimg" "uglvj"}, :weight-of-children 411, :total-weight 1633, :parent-name "mnyng"}
                                {:program-name "ykshd", :weight 464, :children #{"kjljvr" "qufymlj" "simlhnw"}, :weight-of-children 993, :total-weight 1457, :parent-name "rhsztrd"}
                                {:program-name "xykcof", :weight 1152, :children #{"jfhqmi" "kivlu" "ptmqqpk" "vvkgsba" "zfotzg"}, :weight-of-children 305, :total-weight 1457, :parent-name "rhsztrd"}
                                {:program-name "etdmt", :weight 35, :children #{"ahuboc" "egcyvrv" "emsacsk" "emvml" "ffyfj" "srjbcye"}, :weight-of-children 1422, :total-weight 1457, :parent-name "rhsztrd"}
                                {:program-name "psblh", :weight 90, :children #{"kfhcuvf" "uspzk" "wxakaqf"}, :weight-of-children 831, :total-weight 921, :parent-name "nvdoenr"}
                                {:program-name "coxtcw", :weight 153, :children #{"jdfrbhy" "orulm" "xiksvn"}, :weight-of-children 768, :total-weight 921, :parent-name "nvdoenr"}
                                {:program-name "foyabk", :weight 390, :children #{"iejyzgu" "iieig" "onwqbz"}, :weight-of-children 531, :total-weight 921, :parent-name "nvdoenr"}
                                {:program-name "moxiw", :weight 184, :children #{"cqtkhqi" "hughxts" "kvvjhrt" "nfbcs" "oagvuu" "pohqjzd" "qzzhap"}, :weight-of-children 1631, :total-weight 1815, :parent-name "jfdck"}
                                {:program-name "marnqj", :weight 1283, :children #{"mkrrlbv" "upair" "vqkwlq" "wsrmfr"}, :weight-of-children 540, :total-weight 1823, :parent-name "jfdck"}
                                {:program-name "sxijke", :weight 987, :children #{"dlzlo" "jlsxjq" "jmazezm"}, :weight-of-children 828, :total-weight 1815, :parent-name "jfdck"}
                                {:program-name "fnlapoh", :weight 1284, :children #{"foldo" "xlxct" "yxljri"}, :weight-of-children 531, :total-weight 1815, :parent-name "jfdck"}
                                {:program-name "ojgdow", :weight 15, :children #{"boipij" "bzznyu" "gwgnf" "otmpv" "wdjgdoh" "wiqsy"}, :weight-of-children 1800, :total-weight 1815, :parent-name "jfdck"}
                                {:program-name "rlsnxvu", :weight 628, :children #{"tfrjlk" "wnvmb" "xdkin"}, :weight-of-children 705, :total-weight 1333, :parent-name "szmnwnx"}
                                {:program-name "nnoaqvv", :weight 433, :children #{"frklhh" "isixko" "ugeudg"}, :weight-of-children 900, :total-weight 1333, :parent-name "szmnwnx"}
                                {:program-name "mgayiu", :weight 93, :children #{"bhtwp" "euueza" "ksvdw" "pvnjsr" "txfnjo"}, :weight-of-children 1240, :total-weight 1333, :parent-name "szmnwnx"}
                                {:program-name "kupsxf", :weight 724, :children #{"klvncy" "ygbecwq" "zxtwo"}, :weight-of-children 609, :total-weight 1333, :parent-name "szmnwnx"}
                                {:program-name "xmhil", :weight 37, :children #{"gmktbew" "gzrmt" "wloqry"}, :weight-of-children 222, :total-weight 259, :parent-name "fqvvrgx"}
                                {:program-name "dbzwyez", :weight 195, :children #{"kocwei" "lurly" "mcvws" "xdraxfj"}, :weight-of-children 64, :total-weight 259, :parent-name "fqvvrgx"}
                                {:program-name "bmhlcgg", :weight 103, :children #{"mpuhqjm" "yoabvhm"}, :weight-of-children 156, :total-weight 259, :parent-name "fqvvrgx"}
                                {:program-name "krgdzw", :weight 70, :children #{"dfnijm" "fjiemrs" "gvtnin" "jtejd" "oxvcp" "sdllj" "yzvvyb"}, :weight-of-children 2016, :total-weight 2086, :parent-name "arjsnz"}
                                {:program-name "rwrusdg", :weight 1351, :children #{"abmlupq" "atkstsn" "efsbeb"}, :weight-of-children 735, :total-weight 2086, :parent-name "arjsnz"}
                                {:program-name "olhbx", :weight 1384, :children #{"ljeztky" "xvpopw" "zlmvme"}, :weight-of-children 702, :total-weight 2086, :parent-name "arjsnz"}
                                {:program-name "dapjjl", :weight 26, :children #{"aqksi" "dgehtvc" "elxydx" "ghlkk" "pbxzzdi" "rnjqmh"}, :weight-of-children 1554, :total-weight 1580, :parent-name "thuzqqo"}
                                {:program-name "lvbjtf", :weight 1466, :children #{"exuelye" "kuqnndb"}, :weight-of-children 114, :total-weight 1580, :parent-name "thuzqqo"}
                                {:program-name "bijhyzu", :weight 368, :children #{"chpjfv" "gkgka" "jdvfkii" "wgnoil"}, :weight-of-children 1212, :total-weight 1580, :parent-name "thuzqqo"}
                                {:program-name "wnmbn", :weight 13, :children #{"egoezf" "nodgjd" "uicpy" "vrrmn"}, :weight-of-children 1548, :total-weight 1561, :parent-name "bbeclr"}
                                {:program-name "jipgo", :weight 772, :children #{"klkodi" "qgptpig" "tcclaa"}, :weight-of-children 789, :total-weight 1561, :parent-name "bbeclr"}
                                {:program-name "zwqgf", :weight 661, :children #{"bxdxjoa" "obbei" "pcqhhy" "xqvmqe"}, :weight-of-children 900, :total-weight 1561, :parent-name "bbeclr"}
                                {:program-name "sxbee", :weight 32, :children #{"cafxx" "fplzsya" "hweub" "xhymdo" "yftqma" "yhxkh"}, :weight-of-children 1506, :total-weight 1538, :parent-name "fwlspf"}
                                {:program-name "qigsvd", :weight 218, :children #{"gmpoe" "ixszebq" "wdycxsw" "ymqmr"}, :weight-of-children 1320, :total-weight 1538, :parent-name "fwlspf"}
                                {:program-name "xxiymn", :weight 110, :children #{"bqfsjz" "diila" "fvtmh" "lgtpuqr" "ryrtqj" "yikzq"}, :weight-of-children 1428, :total-weight 1538, :parent-name "fwlspf"}
                                {:program-name "qpoodx", :weight 1001, :children #{"adwzzi" "ghebwaz" "gqzczp"}, :weight-of-children 537, :total-weight 1538, :parent-name "fwlspf"}
                                {:program-name "ubvvr", :weight 288, :children #{"hrpzu" "msjeeks" "ozizlok"}, :weight-of-children 1035, :total-weight 1323, :parent-name "mhfci"}
                                {:program-name "fqqzx", :weight 669, :children #{"fafvyk" "syrnxgz" "xofjd"}, :weight-of-children 654, :total-weight 1323, :parent-name "mhfci"}
                                {:program-name "pegoi", :weight 27, :children #{"fwxtdml" "kxpffcy" "qsjwlq" "tfmvb"}, :weight-of-children 1296, :total-weight 1323, :parent-name "mhfci"}
                                {:program-name "katovn", :weight 559, :children #{"aonrg" "ffizjl" "nbbctar" "ondvq"}, :weight-of-children 1104, :total-weight 1663, :parent-name "bqzfjn"}
                                {:program-name "jgckfb", :weight 568, :children #{"bcjpkwf" "dvuug" "iwcpvdx" "mzhyo" "ungcoot"}, :weight-of-children 1095, :total-weight 1663, :parent-name "bqzfjn"}
                                {:program-name "txywrwk", :weight 787, :children #{"indhpfx" "lbflpug" "sxcxmg"}, :weight-of-children 876, :total-weight 1663, :parent-name "bqzfjn"}
                                {:program-name "fklkig", :weight 1573, :children #{"owhxvku" "ykcmvla" "yrdpurl"}, :weight-of-children 90, :total-weight 1663, :parent-name "bqzfjn"}
                                {:program-name "vwqquk", :weight 25, :children #{"gvzze" "kndpli" "ouxdvp" "plevjup" "qtbnqo" "sqauq" "ythoww"}, :weight-of-children 1638, :total-weight 1663, :parent-name "bqzfjn"}
                                {:program-name "tpvcmkz", :weight 1111, :children #{"dtyaj" "xwmjpb" "zqkgqs"}, :weight-of-children 552, :total-weight 1663, :parent-name "bqzfjn"}
                                {:program-name "xjsyk", :weight 217, :children #{"qylvdk" "zhjzpaa"}, :weight-of-children 40, :total-weight 257, :parent-name "myhfh"}
                                {:program-name "howpl", :weight 191, :children #{"qlkhbf" "rnqtg"}, :weight-of-children 66, :total-weight 257, :parent-name "myhfh"}
                                {:program-name "upvrm", :weight 79, :children #{"effjvcu" "uwrqxmj"}, :weight-of-children 178, :total-weight 257, :parent-name "myhfh"}
                                {:program-name "feuirvn", :weight 64, :children #{"auzdz" "futsmzx" "hlkvwq" "jpvyg" "mibwe" "ufses" "yjlzpv"}, :weight-of-children 2562, :total-weight 2626, :parent-name "fzerj"}
                                {:program-name "vxbzw", :weight 1698, :children #{"bzjze" "lsxnn" "owprin" "uosltv"}, :weight-of-children 928, :total-weight 2626, :parent-name "fzerj"}
                                {:program-name "rklic", :weight 832, :children #{"gedajwo" "ilsqjz" "olntlo" "oprsej" "tdqxy" "ylfgaps"}, :weight-of-children 1794, :total-weight 2626, :parent-name "fzerj"}
                                {:program-name "rzvrn", :weight 1852, :children #{"bkzzc" "ioyiklx" "isbppt"}, :weight-of-children 774, :total-weight 2626, :parent-name "fzerj"}
                                {:program-name "sqdhc", :weight 79, :children #{"cwoyko" "eqpuq" "mdyhk" "pfyexc" "zscqv" "zspohe"}, :weight-of-children 1956, :total-weight 2035, :parent-name "wustjt"}
                                {:program-name "quinb", :weight 1927, :children #{"jnojpf" "qtzor"}, :weight-of-children 108, :total-weight 2035, :parent-name "wustjt"}
                                {:program-name "kcekden", :weight 1504, :children #{"fmvdadz" "pmakzzf" "zgbzqf"}, :weight-of-children 531, :total-weight 2035, :parent-name "wustjt"}
                                {:program-name "kzeubgd", :weight 270, :children #{"kxouhtb" "wrcpxdx" "wwritm"}, :weight-of-children 735, :total-weight 1005, :parent-name "zwwrwfy"}
                                {:program-name "corqny", :weight 57, :children #{"cakpt" "crlgf" "gzoyqay" "xkfuz"}, :weight-of-children 948, :total-weight 1005, :parent-name "zwwrwfy"}
                                {:program-name "sbxkokp", :weight 341, :children #{"gxzgsaa" "sjyso" "snxgfl" "unllgjg"}, :weight-of-children 664, :total-weight 1005, :parent-name "zwwrwfy"}
                                {:program-name "fquyicp", :weight 113, :children #{"fudpn" "fyptb" "sfbpnnz" "ymrfvp"}, :weight-of-children 892, :total-weight 1005, :parent-name "zwwrwfy"}
                                {:program-name "zjxspcp", :weight 1244, :children #{"pqjrycz" "sdstw" "tiwxhoa"}, :weight-of-children 411, :total-weight 1655, :parent-name "tgqnyb"}
                                {:program-name "rtngius", :weight 83, :children #{"boxtod" "iceaj" "igrih" "kqecnz" "tgxuhxp" "wpwlg"}, :weight-of-children 1572, :total-weight 1655, :parent-name "tgqnyb"}
                                {:program-name "uqgco", :weight 161, :children #{"errrzm" "mvwcdq" "mynuy" "ocmuu" "uvogit" "uwuouhv"}, :weight-of-children 1494, :total-weight 1655, :parent-name "tgqnyb"}
                                {:program-name "wfuoll", :weight 1715, :children #{"axlitg" "cciwzc" "qpafkjx"}, :weight-of-children 978, :total-weight 2693, :parent-name "whgqhb"}
                                {:program-name "nqehtaf", :weight 1857, :children #{"iouczne" "krzuab" "mmdrry" "vkmtnn"}, :weight-of-children 836, :total-weight 2693, :parent-name "whgqhb"}
                                {:program-name "wnshgr", :weight 1978, :children #{"afjqx" "avzxg" "mqgculh" "pzxfm" "ylmixxx"}, :weight-of-children 715, :total-weight 2693, :parent-name "whgqhb"}
                                {:program-name "cmnjg", :weight 843, :children #{"awpyosw" "hhdth" "kdmuzrx" "ompzpgp" "ovvmrux"}, :weight-of-children 1850, :total-weight 2693, :parent-name "whgqhb"}
                                {:program-name "olqgu", :weight 53, :children #{"bajtat" "igiqnkc" "ocobkpo" "tobzv" "walei" "zglws"}, :weight-of-children 2640, :total-weight 2693, :parent-name "whgqhb"}
                                {:program-name "bymyuke", :weight 511, :children #{"dadlbnn" "dlluqgz" "htwkr"}, :weight-of-children 813, :total-weight 1324, :parent-name "ytbnpd"}
                                {:program-name "rdihhfv", :weight 408, :children #{"dqury" "mskbq" "ozjka" "qrpis"}, :weight-of-children 916, :total-weight 1324, :parent-name "ytbnpd"}
                                {:program-name "uohmo", :weight 432, :children #{"qzeljtb" "stflbsr" "ustkg" "zznkz"}, :weight-of-children 892, :total-weight 1324, :parent-name "ytbnpd"}
                                {:program-name "ogpunov", :weight 34, :children #{"eqsrff" "tszcogp" "uwaxli" "wugret" "zdljb" "zgmrmd"}, :weight-of-children 1290, :total-weight 1324, :parent-name "ytbnpd"}
                                {:program-name "vetgu", :weight 556, :children #{"ssbjh" "wohob" "yierd"}, :weight-of-children 768, :total-weight 1324, :parent-name "ytbnpd"}
                                {:program-name "ctdey", :weight 628, :children #{"aezwvo" "ruxqapz" "vdpnz"}, :weight-of-children 696, :total-weight 1324, :parent-name "ytbnpd"}))
