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
  (#x2663.#x2664)
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
  (#x2b56.#x2b57)
  #x2b59
  (#x3248.#x324f)
  (#xe00b.#xe09f)
  (#xe0a4.#xe0af)
  #xe0c9
  #xe0cb
  #xe0d3
  (#xe0d5.#xe1ff)
  (#xe2aa.#xe2ff)
  (#xe3e4.#xe5f9)
  (#xe6b3.#xe6ff)
  (#xe7c6.#xea5f)
  #xea89
  (#xea8d.#xea8e)
  #xeac8
  (#xeaca.#xeacb)
  #xeb0a
  #xeb4f
  (#xebec.#xefff)
  #xf00f
  (#xf01f.#xf020)
  #xf03f
  #xf04f
  #xf05f
  #xf06f
  #xf07f
  #xf08f
  #xf09f
  #xf0af
  (#xf0b3.#xf0bf)
  #xf0cf
  #xf0df
  #xf0ef
  #xf0ff
  #xf10f
  #xf11f
  #xf12f
  #xf13f
  #xf14f
  #xf15f
  #xf16f
  #xf17f
  #xf18f
  #xf19f
  #xf1af
  #xf1bf
  #xf1cf
  #xf1df
  #xf1ef
  #xf1ff
  #xf20f
  (#xf21f.#xf220)
  #xf23f
  #xf24f
  #xf25f
  #xf26f
  #xf27f
  #xf28f
  #xf29f
  #xf2af
  #xf2bf
  #xf2cf
  #xf2df
  (#xf2e1.#xf2ff)
  (#xf373.#xf3ff)
  (#xf534.#xf8ff)
  (#xfe00.#xfe0f)
  #xfffd
  (#x1f100.#x1f10a)
  (#x1f110.#x1f12d)
  (#x1f130.#x1f169)
  (#x1f170.#x1f18d)
  (#x1f18f.#x1f190)
  (#x1f19b.#x1f1ac)
  (#xe0100.#xe01ef)
  #xf0000
  (#xf1af1.#xffffd)
  (#x100000.#x10fffd)
))
(setq code-wide '(
  (#x23fb.#x23fe)
  #x2630
  #x2665
  #x26a1
  (#x276c.#x2771)
  #x2b58
  (#xe000.#xe00a)
  (#xe0a0.#xe0a3)
  (#xe0b0.#xe0c8)
  #xe0ca
  (#xe0cc.#xe0d2)
  #xe0d4
  (#xe200.#xe2a9)
  (#xe300.#xe3e3)
  (#xe5fa.#xe6b2)
  (#xe700.#xe7c5)
  (#xea60.#xea88)
  (#xea8a.#xea8c)
  (#xea8f.#xeac7)
  #xeac9
  (#xeacc.#xeb09)
  (#xeb0b.#xeb4e)
  (#xeb50.#xebeb)
  (#xf000.#xf00e)
  (#xf010.#xf01e)
  (#xf021.#xf03e)
  (#xf040.#xf04e)
  (#xf050.#xf05e)
  (#xf060.#xf06e)
  (#xf070.#xf07e)
  (#xf080.#xf08e)
  (#xf090.#xf09e)
  (#xf0a0.#xf0ae)
  (#xf0b0.#xf0b2)
  (#xf0c0.#xf0ce)
  (#xf0d0.#xf0de)
  (#xf0e0.#xf0ee)
  (#xf0f0.#xf0fe)
  (#xf100.#xf10e)
  (#xf110.#xf11e)
  (#xf120.#xf12e)
  (#xf130.#xf13e)
  (#xf140.#xf14e)
  (#xf150.#xf15e)
  (#xf160.#xf16e)
  (#xf170.#xf17e)
  (#xf180.#xf18e)
  (#xf190.#xf19e)
  (#xf1a0.#xf1ae)
  (#xf1b0.#xf1be)
  (#xf1c0.#xf1ce)
  (#xf1d0.#xf1de)
  (#xf1e0.#xf1ee)
  (#xf1f0.#xf1fe)
  (#xf200.#xf20e)
  (#xf210.#xf21e)
  (#xf221.#xf23e)
  (#xf240.#xf24e)
  (#xf250.#xf25e)
  (#xf260.#xf26e)
  (#xf270.#xf27e)
  (#xf280.#xf28e)
  (#xf290.#xf29e)
  (#xf2a0.#xf2ae)
  (#xf2b0.#xf2be)
  (#xf2c0.#xf2ce)
  (#xf2d0.#xf2de)
  #xf2e0
  (#xf300.#xf372)
  (#xf400.#xf533)
  (#xf0001.#xf1af0)
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
