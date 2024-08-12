;;; eaw.el --- Fix east asian ambiguous width issue for emacs
;; Author: HAMANO Tsukasa <code@cuspy.org>
;; URL: https://github.com/hamano/locale-eaw
;; Version: 12
;; MIT License

(provide 'eaw-fullwidth)

(setq code-half '(
))
(setq code-wide '(
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
  #x2030
  (#x2032.#x2033)
  #x2035
  #x203b
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
  (#x2121.#x2122)
  #x2126
  #x212b
  (#x2153.#x2154)
  (#x215b.#x215e)
  (#x2160.#x216b)
  (#x2170.#x2179)
  #x2189
  (#x2190.#x2199)
  (#x21b8.#x21b9)
  #x21d2
  #x21d4
  #x21e7
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
  (#x2460.#x24e9)
  (#x24eb.#x254b)
  (#x2550.#x2573)
  (#x2580.#x258f)
  (#x2592.#x2595)
  (#x25a0.#x25a1)
  (#x25a3.#x25a9)
  (#x25b2.#x25b3)
  (#x25b6.#x25b7)
  (#x25bc.#x25bd)
  (#x25c0.#x25c1)
  (#x25c6.#x25c8)
  #x25cb
  (#x25ce.#x25d1)
  (#x25e2.#x25e5)
  #x25ef
  (#x2605.#x2606)
  #x2609
  (#x260e.#x260f)
  #x261c
  #x261e
  #x2640
  #x2642
  (#x2660.#x2661)
  (#x2663.#x2665)
  (#x2667.#x266a)
  (#x266c.#x266d)
  #x266f
  (#x269e.#x269f)
  #x26bf
  (#x26c6.#x26cd)
  (#x26cf.#x26d3)
  (#x26d5.#x26e1)
  #x26e3
  (#x26e8.#x26e9)
  (#x26eb.#x26f1)
  #x26f4
  (#x26f6.#x26f9)
  (#x26fb.#x26fc)
  (#x26fe.#x26ff)
  #x273d
  (#x2776.#x277f)
  (#x2b56.#x2b59)
  (#x3248.#x324f)
  (#xe000.#xf8ff)
  #xfffd
  (#x1f100.#x1f10a)
  (#x1f110.#x1f12d)
  (#x1f130.#x1f169)
  (#x1f170.#x1f18d)
  (#x1f18f.#x1f190)
  (#x1f19b.#x1f1ac)
  (#xf0000.#xffffd)
  (#x100000.#x10fffd)
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
