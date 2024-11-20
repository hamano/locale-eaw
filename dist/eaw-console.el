;;; eaw.el --- Fix east asian ambiguous width issue for emacs
;; Author: HAMANO Tsukasa <code@cuspy.org>
;; URL: https://github.com/hamano/locale-eaw
;; Version: 12
;; MIT License

(provide 'eaw-console)

(setq code-half '(
  #xa1
  #xa4
  (#xa7.#xa8)
  #xaa
  (#xad.#xae)
  (#xb0.#xb4)
  (#xb6.#xba)
  (#xbc.#xbf)
  #xc6
  #xd0
  (#xd7.#xd8)
  (#xde.#xe1)
  #xe6
  (#xe8.#xea)
  (#xec.#xed)
  #xf0
  (#xf2.#xf3)
  (#xf7.#xfa)
  #xfc
  #xfe
  #x101
  #x111
  #x113
  #x11b
  (#x126.#x127)
  #x12b
  (#x131.#x133)
  #x138
  (#x13f.#x142)
  #x144
  (#x148.#x14b)
  #x14d
  (#x152.#x153)
  (#x166.#x167)
  #x16b
  #x1ce
  #x1d0
  #x1d2
  #x1d4
  #x1d6
  #x1d8
  #x1da
  #x1dc
  #x251
  #x261
  #x2c4
  #x2c7
  (#x2c9.#x2cb)
  #x2cd
  #x2d0
  (#x2d8.#x2db)
  #x2dd
  #x2df
  (#x391.#x3a1)
  (#x3a3.#x3a9)
  (#x3b1.#x3c1)
  (#x3c3.#x3c9)
  #x401
  (#x410.#x44f)
  #x451
  #x2010
  (#x2013.#x2016)
  (#x2018.#x2019)
  (#x201c.#x201d)
  (#x2020.#x2022)
  (#x2024.#x2027)
  (#x2032.#x2033)
  #x2035
  #x203e
  #x2074
  #x207f
  (#x2081.#x2084)
  #x20ac
  #x2103
  #x2105
  #x2109
  #x2113
  #x2116
  #x2122
  #x2126
  #x212b
  (#x2153.#x2154)
  (#x215b.#x215e)
  #x2189
  #x2200
  (#x2202.#x2203)
  (#x2207.#x2208)
  #x220b
  #x220f
  #x2211
  #x2215
  #x221a
  (#x221d.#x2220)
  #x2223
  #x2225
  (#x2227.#x222c)
  #x222e
  (#x2234.#x2237)
  (#x223c.#x223d)
  #x2248
  #x224c
  #x2252
  (#x2260.#x2261)
  (#x2264.#x2267)
  (#x226a.#x226b)
  (#x226e.#x226f)
  (#x2282.#x2283)
  (#x2286.#x2287)
  #x2295
  #x2299
  #x22a5
  #x22bf
  #x2312
  (#x2500.#x254b)
  (#x2550.#x2573)
  (#x2580.#x258f)
  (#x2592.#x2595)
  #x25d8
  #x25e6
  (#x2654.#x2667)
  (#x2669.#x266f)
  #x273d
  (#x2b56.#x2b59)
  (#xee00.#xee0b)
  #xfffd
))
(setq code-wide '(
  (#x2030.#x2031)
  #x203b
  #x2121
  #x213b
  #x214f
  (#x2160.#x2182)
  (#x2190.#x21ff)
  #x2318
  #x2325
  (#x2460.#x24ff)
  (#x25a0.#x25d7)
  (#x25d9.#x25e5)
  (#x25e7.#x2653)
  #x2668
  (#x2670.#x2712)
  #x2744
  #x2747
  #x2763
  (#x2776.#x2793)
  (#x27dd.#x27de)
  (#x27f5.#x27ff)
  #x2b33
  (#x3248.#x324f)
  (#xe000.#xedff)
  (#xee0c.#xf8ff)
  (#x1f000.#x1f02b)
  (#x1f030.#x1f093)
  (#x1f0a0.#x1f0f5)
  (#x1f100.#x1faf8)
  (#xf0000.#x10fffd)
))

(defun eaw-init ()
  (when (= emacs-major-version 22)
    (utf-translate-cjk-set-unicode-range code-wide))
  (when (>= emacs-major-version 23)
    (while (char-table-parent char-width-table)
      (setq char-width-table (char-table-parent char-width-table)))
    (let ((table (make-char-table nil)))
      (mapc (lambda (range) (set-char-table-range table range 1))
            code-half)
      (mapc (lambda (range) (set-char-table-range table range 2))
            code-wide)
      (optimize-char-table table)
      (set-char-table-parent table char-width-table)
      (setq char-width-table table))))

(eaw-init)

;;; eaw.el ends here
