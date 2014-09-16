(setq east-asian-ambiguous '(
  #x00A1 ; INVERTED EXCLAMATION MARK
  #x00A4 ; CURRENCY SIGN
  #x00A7 ; SECTION SIGN
  #x00A8 ; DIAERESIS
  #x00AA ; FEMININE ORDINAL INDICATOR
  #x00AD ; SOFT HYPHEN
  #x00AE ; REGISTERED SIGN
  #x00B0 ; DEGREE SIGN
  #x00B1 ; PLUS-MINUS SIGN
  #x00B2 ; SUPERSCRIPT TWO
  #x00B3 ; SUPERSCRIPT THREE
  #x00B4 ; ACUTE ACCENT
  #x00B6 ; PILCROW SIGN
  #x00B7 ; MIDDLE DOT
  #x00B8 ; CEDILLA
  #x00B9 ; SUPERSCRIPT ONE
  #x00BA ; MASCULINE ORDINAL INDICATOR
  #x00BC ; VULGAR FRACTION ONE QUARTER
  #x00BD ; VULGAR FRACTION ONE HALF
  #x00BE ; VULGAR FRACTION THREE QUARTERS
  #x00BF ; INVERTED QUESTION MARK
  #x00C6 ; LATIN CAPITAL LETTER AE
  #x00D0 ; LATIN CAPITAL LETTER ETH
  #x00D7 ; MULTIPLICATION SIGN
  #x00D8 ; LATIN CAPITAL LETTER O WITH STROKE
  #x00DE ; LATIN CAPITAL LETTER THORN
  #x00DF ; LATIN SMALL LETTER SHARP S
  #x00E0 ; LATIN SMALL LETTER A WITH GRAVE
  #x00E1 ; LATIN SMALL LETTER A WITH ACUTE
  #x00E6 ; LATIN SMALL LETTER AE
  #x00E8 ; LATIN SMALL LETTER E WITH GRAVE
  #x00E9 ; LATIN SMALL LETTER E WITH ACUTE
  #x00EA ; LATIN SMALL LETTER E WITH CIRCUMFLEX
  #x00EC ; LATIN SMALL LETTER I WITH GRAVE
  #x00ED ; LATIN SMALL LETTER I WITH ACUTE
  #x00F0 ; LATIN SMALL LETTER ETH
  #x00F2 ; LATIN SMALL LETTER O WITH GRAVE
  #x00F3 ; LATIN SMALL LETTER O WITH ACUTE
  #x00F7 ; DIVISION SIGN
  #x00F8 ; LATIN SMALL LETTER O WITH STROKE
  #x00F9 ; LATIN SMALL LETTER U WITH GRAVE
  #x00FA ; LATIN SMALL LETTER U WITH ACUTE
  #x00FC ; LATIN SMALL LETTER U WITH DIAERESIS
  #x00FE ; LATIN SMALL LETTER THORN
  #x0101 ; LATIN SMALL LETTER A WITH MACRON
  #x0111 ; LATIN SMALL LETTER D WITH STROKE
  #x0113 ; LATIN SMALL LETTER E WITH MACRON
  #x011B ; LATIN SMALL LETTER E WITH CARON
  #x0126 ; LATIN CAPITAL LETTER H WITH STROKE
  #x0127 ; LATIN SMALL LETTER H WITH STROKE
  #x012B ; LATIN SMALL LETTER I WITH MACRON
  #x0131 ; LATIN SMALL LETTER DOTLESS I
  #x0132 ; LATIN CAPITAL LIGATURE IJ
  #x0133 ; LATIN SMALL LIGATURE IJ
  #x0138 ; LATIN SMALL LETTER KRA
  #x013F ; LATIN CAPITAL LETTER L WITH MIDDLE DOT
  #x0140 ; LATIN SMALL LETTER L WITH MIDDLE DOT
  #x0141 ; LATIN CAPITAL LETTER L WITH STROKE
  #x0142 ; LATIN SMALL LETTER L WITH STROKE
  #x0144 ; LATIN SMALL LETTER N WITH ACUTE
  #x0148 ; LATIN SMALL LETTER N WITH CARON
  #x0149 ; LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
  #x014A ; LATIN CAPITAL LETTER ENG
  #x014B ; LATIN SMALL LETTER ENG
  #x014D ; LATIN SMALL LETTER O WITH MACRON
  #x0152 ; LATIN CAPITAL LIGATURE OE
  #x0153 ; LATIN SMALL LIGATURE OE
  #x0166 ; LATIN CAPITAL LETTER T WITH STROKE
  #x0167 ; LATIN SMALL LETTER T WITH STROKE
  #x016B ; LATIN SMALL LETTER U WITH MACRON
  #x01CE ; LATIN SMALL LETTER A WITH CARON
  #x01D0 ; LATIN SMALL LETTER I WITH CARON
  #x01D2 ; LATIN SMALL LETTER O WITH CARON
  #x01D4 ; LATIN SMALL LETTER U WITH CARON
  #x01D6 ; LATIN SMALL LETTER U WITH DIAERESIS AND MACRON
  #x01D8 ; LATIN SMALL LETTER U WITH DIAERESIS AND ACUTE
  #x01DA ; LATIN SMALL LETTER U WITH DIAERESIS AND CARON
  #x01DC ; LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE
  #x0251 ; LATIN SMALL LETTER ALPHA
  #x0261 ; LATIN SMALL LETTER SCRIPT G
  #x02C4 ; MODIFIER LETTER UP ARROWHEAD
  #x02C7 ; CARON
  #x02C9 ; MODIFIER LETTER MACRON
  #x02CA ; MODIFIER LETTER ACUTE ACCENT
  #x02CB ; MODIFIER LETTER GRAVE ACCENT
  #x02CD ; MODIFIER LETTER LOW MACRON
  #x02D0 ; MODIFIER LETTER TRIANGULAR COLON
  #x02D8 ; BREVE
  #x02D9 ; DOT ABOVE
  #x02DA ; RING ABOVE
  #x02DB ; OGONEK
  #x02DD ; DOUBLE ACUTE ACCENT
  #x02DF ; MODIFIER LETTER CROSS ACCENT
  #x0391 ; GREEK CAPITAL LETTER ALPHA
  #x0392 ; GREEK CAPITAL LETTER BETA
  #x0393 ; GREEK CAPITAL LETTER GAMMA
  #x0394 ; GREEK CAPITAL LETTER DELTA
  #x0395 ; GREEK CAPITAL LETTER EPSILON
  #x0396 ; GREEK CAPITAL LETTER ZETA
  #x0397 ; GREEK CAPITAL LETTER ETA
  #x0398 ; GREEK CAPITAL LETTER THETA
  #x0399 ; GREEK CAPITAL LETTER IOTA
  #x039A ; GREEK CAPITAL LETTER KAPPA
  #x039B ; GREEK CAPITAL LETTER LAMDA
  #x039C ; GREEK CAPITAL LETTER MU
  #x039D ; GREEK CAPITAL LETTER NU
  #x039E ; GREEK CAPITAL LETTER XI
  #x039F ; GREEK CAPITAL LETTER OMICRON
  #x03A0 ; GREEK CAPITAL LETTER PI
  #x03A1 ; GREEK CAPITAL LETTER RHO
  #x03A3 ; GREEK CAPITAL LETTER SIGMA
  #x03A4 ; GREEK CAPITAL LETTER TAU
  #x03A5 ; GREEK CAPITAL LETTER UPSILON
  #x03A6 ; GREEK CAPITAL LETTER PHI
  #x03A7 ; GREEK CAPITAL LETTER CHI
  #x03A8 ; GREEK CAPITAL LETTER PSI
  #x03A9 ; GREEK CAPITAL LETTER OMEGA
  #x03B1 ; GREEK SMALL LETTER ALPHA
  #x03B2 ; GREEK SMALL LETTER BETA
  #x03B3 ; GREEK SMALL LETTER GAMMA
  #x03B4 ; GREEK SMALL LETTER DELTA
  #x03B5 ; GREEK SMALL LETTER EPSILON
  #x03B6 ; GREEK SMALL LETTER ZETA
  #x03B7 ; GREEK SMALL LETTER ETA
  #x03B8 ; GREEK SMALL LETTER THETA
  #x03B9 ; GREEK SMALL LETTER IOTA
  #x03BA ; GREEK SMALL LETTER KAPPA
  #x03BB ; GREEK SMALL LETTER LAMDA
  #x03BC ; GREEK SMALL LETTER MU
  #x03BD ; GREEK SMALL LETTER NU
  #x03BE ; GREEK SMALL LETTER XI
  #x03BF ; GREEK SMALL LETTER OMICRON
  #x03C0 ; GREEK SMALL LETTER PI
  #x03C1 ; GREEK SMALL LETTER RHO
  #x03C3 ; GREEK SMALL LETTER SIGMA
  #x03C4 ; GREEK SMALL LETTER TAU
  #x03C5 ; GREEK SMALL LETTER UPSILON
  #x03C6 ; GREEK SMALL LETTER PHI
  #x03C7 ; GREEK SMALL LETTER CHI
  #x03C8 ; GREEK SMALL LETTER PSI
  #x03C9 ; GREEK SMALL LETTER OMEGA
  #x0401 ; CYRILLIC CAPITAL LETTER IO
  #x0410 ; CYRILLIC CAPITAL LETTER A
  #x0411 ; CYRILLIC CAPITAL LETTER BE
  #x0412 ; CYRILLIC CAPITAL LETTER VE
  #x0413 ; CYRILLIC CAPITAL LETTER GHE
  #x0414 ; CYRILLIC CAPITAL LETTER DE
  #x0415 ; CYRILLIC CAPITAL LETTER IE
  #x0416 ; CYRILLIC CAPITAL LETTER ZHE
  #x0417 ; CYRILLIC CAPITAL LETTER ZE
  #x0418 ; CYRILLIC CAPITAL LETTER I
  #x0419 ; CYRILLIC CAPITAL LETTER SHORT I
  #x041A ; CYRILLIC CAPITAL LETTER KA
  #x041B ; CYRILLIC CAPITAL LETTER EL
  #x041C ; CYRILLIC CAPITAL LETTER EM
  #x041D ; CYRILLIC CAPITAL LETTER EN
  #x041E ; CYRILLIC CAPITAL LETTER O
  #x041F ; CYRILLIC CAPITAL LETTER PE
  #x0420 ; CYRILLIC CAPITAL LETTER ER
  #x0421 ; CYRILLIC CAPITAL LETTER ES
  #x0422 ; CYRILLIC CAPITAL LETTER TE
  #x0423 ; CYRILLIC CAPITAL LETTER U
  #x0424 ; CYRILLIC CAPITAL LETTER EF
  #x0425 ; CYRILLIC CAPITAL LETTER HA
  #x0426 ; CYRILLIC CAPITAL LETTER TSE
  #x0427 ; CYRILLIC CAPITAL LETTER CHE
  #x0428 ; CYRILLIC CAPITAL LETTER SHA
  #x0429 ; CYRILLIC CAPITAL LETTER SHCHA
  #x042A ; CYRILLIC CAPITAL LETTER HARD SIGN
  #x042B ; CYRILLIC CAPITAL LETTER YERU
  #x042C ; CYRILLIC CAPITAL LETTER SOFT SIGN
  #x042D ; CYRILLIC CAPITAL LETTER E
  #x042E ; CYRILLIC CAPITAL LETTER YU
  #x042F ; CYRILLIC CAPITAL LETTER YA
  #x0430 ; CYRILLIC SMALL LETTER A
  #x0431 ; CYRILLIC SMALL LETTER BE
  #x0432 ; CYRILLIC SMALL LETTER VE
  #x0433 ; CYRILLIC SMALL LETTER GHE
  #x0434 ; CYRILLIC SMALL LETTER DE
  #x0435 ; CYRILLIC SMALL LETTER IE
  #x0436 ; CYRILLIC SMALL LETTER ZHE
  #x0437 ; CYRILLIC SMALL LETTER ZE
  #x0438 ; CYRILLIC SMALL LETTER I
  #x0439 ; CYRILLIC SMALL LETTER SHORT I
  #x043A ; CYRILLIC SMALL LETTER KA
  #x043B ; CYRILLIC SMALL LETTER EL
  #x043C ; CYRILLIC SMALL LETTER EM
  #x043D ; CYRILLIC SMALL LETTER EN
  #x043E ; CYRILLIC SMALL LETTER O
  #x043F ; CYRILLIC SMALL LETTER PE
  #x0440 ; CYRILLIC SMALL LETTER ER
  #x0441 ; CYRILLIC SMALL LETTER ES
  #x0442 ; CYRILLIC SMALL LETTER TE
  #x0443 ; CYRILLIC SMALL LETTER U
  #x0444 ; CYRILLIC SMALL LETTER EF
  #x0445 ; CYRILLIC SMALL LETTER HA
  #x0446 ; CYRILLIC SMALL LETTER TSE
  #x0447 ; CYRILLIC SMALL LETTER CHE
  #x0448 ; CYRILLIC SMALL LETTER SHA
  #x0449 ; CYRILLIC SMALL LETTER SHCHA
  #x044A ; CYRILLIC SMALL LETTER HARD SIGN
  #x044B ; CYRILLIC SMALL LETTER YERU
  #x044C ; CYRILLIC SMALL LETTER SOFT SIGN
  #x044D ; CYRILLIC SMALL LETTER E
  #x044E ; CYRILLIC SMALL LETTER YU
  #x044F ; CYRILLIC SMALL LETTER YA
  #x0451 ; CYRILLIC SMALL LETTER IO
  #x2010 ; HYPHEN
  #x2013 ; EN DASH
  #x2014 ; EM DASH
  #x2015 ; HORIZONTAL BAR
  #x2016 ; DOUBLE VERTICAL LINE
  #x2018 ; LEFT SINGLE QUOTATION MARK
  #x2019 ; RIGHT SINGLE QUOTATION MARK
  #x201C ; LEFT DOUBLE QUOTATION MARK
  #x201D ; RIGHT DOUBLE QUOTATION MARK
  #x2020 ; DAGGER
  #x2021 ; DOUBLE DAGGER
  #x2022 ; BULLET
  #x2024 ; ONE DOT LEADER
  #x2025 ; TWO DOT LEADER
  #x2026 ; HORIZONTAL ELLIPSIS
  #x2027 ; HYPHENATION POINT
  #x2030 ; PER MILLE SIGN
  #x2032 ; PRIME
  #x2033 ; DOUBLE PRIME
  #x2035 ; REVERSED PRIME
  #x203B ; REFERENCE MARK
  #x203E ; OVERLINE
  #x2074 ; SUPERSCRIPT FOUR
  #x207F ; SUPERSCRIPT LATIN SMALL LETTER N
  #x2081 ; SUBSCRIPT ONE
  #x2082 ; SUBSCRIPT TWO
  #x2083 ; SUBSCRIPT THREE
  #x2084 ; SUBSCRIPT FOUR
  #x20AC ; EURO SIGN
  #x2103 ; DEGREE CELSIUS
  #x2105 ; CARE OF
  #x2109 ; DEGREE FAHRENHEIT
  #x2113 ; SCRIPT SMALL L
  #x2116 ; NUMERO SIGN
  #x2121 ; TELEPHONE SIGN
  #x2122 ; TRADE MARK SIGN
  #x2126 ; OHM SIGN
  #x212B ; ANGSTROM SIGN
  #x2153 ; VULGAR FRACTION ONE THIRD
  #x2154 ; VULGAR FRACTION TWO THIRDS
  #x215B ; VULGAR FRACTION ONE EIGHTH
  #x215C ; VULGAR FRACTION THREE EIGHTHS
  #x215D ; VULGAR FRACTION FIVE EIGHTHS
  #x215E ; VULGAR FRACTION SEVEN EIGHTHS
  #x2160 ; ROMAN NUMERAL ONE
  #x2161 ; ROMAN NUMERAL TWO
  #x2162 ; ROMAN NUMERAL THREE
  #x2163 ; ROMAN NUMERAL FOUR
  #x2164 ; ROMAN NUMERAL FIVE
  #x2165 ; ROMAN NUMERAL SIX
  #x2166 ; ROMAN NUMERAL SEVEN
  #x2167 ; ROMAN NUMERAL EIGHT
  #x2168 ; ROMAN NUMERAL NINE
  #x2169 ; ROMAN NUMERAL TEN
  #x216A ; ROMAN NUMERAL ELEVEN
  #x216B ; ROMAN NUMERAL TWELVE
  #x2170 ; SMALL ROMAN NUMERAL ONE
  #x2171 ; SMALL ROMAN NUMERAL TWO
  #x2172 ; SMALL ROMAN NUMERAL THREE
  #x2173 ; SMALL ROMAN NUMERAL FOUR
  #x2174 ; SMALL ROMAN NUMERAL FIVE
  #x2175 ; SMALL ROMAN NUMERAL SIX
  #x2176 ; SMALL ROMAN NUMERAL SEVEN
  #x2177 ; SMALL ROMAN NUMERAL EIGHT
  #x2178 ; SMALL ROMAN NUMERAL NINE
  #x2179 ; SMALL ROMAN NUMERAL TEN
  #x2190 ; LEFTWARDS ARROW
  #x2191 ; UPWARDS ARROW
  #x2192 ; RIGHTWARDS ARROW
  #x2193 ; DOWNWARDS ARROW
  #x2194 ; LEFT RIGHT ARROW
  #x2195 ; UP DOWN ARROW
  #x2196 ; NORTH WEST ARROW
  #x2197 ; NORTH EAST ARROW
  #x2198 ; SOUTH EAST ARROW
  #x2199 ; SOUTH WEST ARROW
  #x21B8 ; NORTH WEST ARROW TO LONG BAR
  #x21B9 ; LEFTWARDS ARROW TO BAR OVER RIGHTWARDS ARROW TO BAR
  #x21D2 ; RIGHTWARDS DOUBLE ARROW
  #x21D4 ; LEFT RIGHT DOUBLE ARROW
  #x21E7 ; UPWARDS WHITE ARROW
  #x2200 ; FOR ALL
  #x2202 ; PARTIAL DIFFERENTIAL
  #x2203 ; THERE EXISTS
  #x2207 ; NABLA
  #x2208 ; ELEMENT OF
  #x220B ; CONTAINS AS MEMBER
  #x220F ; N-ARY PRODUCT
  #x2211 ; N-ARY SUMMATION
  #x2215 ; DIVISION SLASH
  #x221A ; SQUARE ROOT
  #x221D ; PROPORTIONAL TO
  #x221E ; INFINITY
  #x221F ; RIGHT ANGLE
  #x2220 ; ANGLE
  #x2223 ; DIVIDES
  #x2225 ; PARALLEL TO
  #x2227 ; LOGICAL AND
  #x2228 ; LOGICAL OR
  #x2229 ; INTERSECTION
  #x222A ; UNION
  #x222B ; INTEGRAL
  #x222C ; DOUBLE INTEGRAL
  #x222E ; CONTOUR INTEGRAL
  #x2234 ; THEREFORE
  #x2235 ; BECAUSE
  #x2236 ; RATIO
  #x2237 ; PROPORTION
  #x223C ; TILDE OPERATOR
  #x223D ; REVERSED TILDE
  #x2248 ; ALMOST EQUAL TO
  #x224C ; ALL EQUAL TO
  #x2252 ; APPROXIMATELY EQUAL TO OR THE IMAGE OF
  #x2260 ; NOT EQUAL TO
  #x2261 ; IDENTICAL TO
  #x2264 ; LESS-THAN OR EQUAL TO
  #x2265 ; GREATER-THAN OR EQUAL TO
  #x2266 ; LESS-THAN OVER EQUAL TO
  #x2267 ; GREATER-THAN OVER EQUAL TO
  #x226A ; MUCH LESS-THAN
  #x226B ; MUCH GREATER-THAN
  #x226E ; NOT LESS-THAN
  #x226F ; NOT GREATER-THAN
  #x2282 ; SUBSET OF
  #x2283 ; SUPERSET OF
  #x2286 ; SUBSET OF OR EQUAL TO
  #x2287 ; SUPERSET OF OR EQUAL TO
  #x2295 ; CIRCLED PLUS
  #x2299 ; CIRCLED DOT OPERATOR
  #x22A5 ; UP TACK
  #x22BF ; RIGHT TRIANGLE
  #x2312 ; ARC
  #x2460 ; CIRCLED DIGIT ONE
  #x2461 ; CIRCLED DIGIT TWO
  #x2462 ; CIRCLED DIGIT THREE
  #x2463 ; CIRCLED DIGIT FOUR
  #x2464 ; CIRCLED DIGIT FIVE
  #x2465 ; CIRCLED DIGIT SIX
  #x2466 ; CIRCLED DIGIT SEVEN
  #x2467 ; CIRCLED DIGIT EIGHT
  #x2468 ; CIRCLED DIGIT NINE
  #x2469 ; CIRCLED NUMBER TEN
  #x246A ; CIRCLED NUMBER ELEVEN
  #x246B ; CIRCLED NUMBER TWELVE
  #x246C ; CIRCLED NUMBER THIRTEEN
  #x246D ; CIRCLED NUMBER FOURTEEN
  #x246E ; CIRCLED NUMBER FIFTEEN
  #x246F ; CIRCLED NUMBER SIXTEEN
  #x2470 ; CIRCLED NUMBER SEVENTEEN
  #x2471 ; CIRCLED NUMBER EIGHTEEN
  #x2472 ; CIRCLED NUMBER NINETEEN
  #x2473 ; CIRCLED NUMBER TWENTY
  #x2474 ; PARENTHESIZED DIGIT ONE
  #x2475 ; PARENTHESIZED DIGIT TWO
  #x2476 ; PARENTHESIZED DIGIT THREE
  #x2477 ; PARENTHESIZED DIGIT FOUR
  #x2478 ; PARENTHESIZED DIGIT FIVE
  #x2479 ; PARENTHESIZED DIGIT SIX
  #x247A ; PARENTHESIZED DIGIT SEVEN
  #x247B ; PARENTHESIZED DIGIT EIGHT
  #x247C ; PARENTHESIZED DIGIT NINE
  #x247D ; PARENTHESIZED NUMBER TEN
  #x247E ; PARENTHESIZED NUMBER ELEVEN
  #x247F ; PARENTHESIZED NUMBER TWELVE
  #x2480 ; PARENTHESIZED NUMBER THIRTEEN
  #x2481 ; PARENTHESIZED NUMBER FOURTEEN
  #x2482 ; PARENTHESIZED NUMBER FIFTEEN
  #x2483 ; PARENTHESIZED NUMBER SIXTEEN
  #x2484 ; PARENTHESIZED NUMBER SEVENTEEN
  #x2485 ; PARENTHESIZED NUMBER EIGHTEEN
  #x2486 ; PARENTHESIZED NUMBER NINETEEN
  #x2487 ; PARENTHESIZED NUMBER TWENTY
  #x2488 ; DIGIT ONE FULL STOP
  #x2489 ; DIGIT TWO FULL STOP
  #x248A ; DIGIT THREE FULL STOP
  #x248B ; DIGIT FOUR FULL STOP
  #x248C ; DIGIT FIVE FULL STOP
  #x248D ; DIGIT SIX FULL STOP
  #x248E ; DIGIT SEVEN FULL STOP
  #x248F ; DIGIT EIGHT FULL STOP
  #x2490 ; DIGIT NINE FULL STOP
  #x2491 ; NUMBER TEN FULL STOP
  #x2492 ; NUMBER ELEVEN FULL STOP
  #x2493 ; NUMBER TWELVE FULL STOP
  #x2494 ; NUMBER THIRTEEN FULL STOP
  #x2495 ; NUMBER FOURTEEN FULL STOP
  #x2496 ; NUMBER FIFTEEN FULL STOP
  #x2497 ; NUMBER SIXTEEN FULL STOP
  #x2498 ; NUMBER SEVENTEEN FULL STOP
  #x2499 ; NUMBER EIGHTEEN FULL STOP
  #x249A ; NUMBER NINETEEN FULL STOP
  #x249B ; NUMBER TWENTY FULL STOP
  #x249C ; PARENTHESIZED LATIN SMALL LETTER A
  #x249D ; PARENTHESIZED LATIN SMALL LETTER B
  #x249E ; PARENTHESIZED LATIN SMALL LETTER C
  #x249F ; PARENTHESIZED LATIN SMALL LETTER D
  #x24A0 ; PARENTHESIZED LATIN SMALL LETTER E
  #x24A1 ; PARENTHESIZED LATIN SMALL LETTER F
  #x24A2 ; PARENTHESIZED LATIN SMALL LETTER G
  #x24A3 ; PARENTHESIZED LATIN SMALL LETTER H
  #x24A4 ; PARENTHESIZED LATIN SMALL LETTER I
  #x24A5 ; PARENTHESIZED LATIN SMALL LETTER J
  #x24A6 ; PARENTHESIZED LATIN SMALL LETTER K
  #x24A7 ; PARENTHESIZED LATIN SMALL LETTER L
  #x24A8 ; PARENTHESIZED LATIN SMALL LETTER M
  #x24A9 ; PARENTHESIZED LATIN SMALL LETTER N
  #x24AA ; PARENTHESIZED LATIN SMALL LETTER O
  #x24AB ; PARENTHESIZED LATIN SMALL LETTER P
  #x24AC ; PARENTHESIZED LATIN SMALL LETTER Q
  #x24AD ; PARENTHESIZED LATIN SMALL LETTER R
  #x24AE ; PARENTHESIZED LATIN SMALL LETTER S
  #x24AF ; PARENTHESIZED LATIN SMALL LETTER T
  #x24B0 ; PARENTHESIZED LATIN SMALL LETTER U
  #x24B1 ; PARENTHESIZED LATIN SMALL LETTER V
  #x24B2 ; PARENTHESIZED LATIN SMALL LETTER W
  #x24B3 ; PARENTHESIZED LATIN SMALL LETTER X
  #x24B4 ; PARENTHESIZED LATIN SMALL LETTER Y
  #x24B5 ; PARENTHESIZED LATIN SMALL LETTER Z
  #x24B6 ; CIRCLED LATIN CAPITAL LETTER A
  #x24B7 ; CIRCLED LATIN CAPITAL LETTER B
  #x24B8 ; CIRCLED LATIN CAPITAL LETTER C
  #x24B9 ; CIRCLED LATIN CAPITAL LETTER D
  #x24BA ; CIRCLED LATIN CAPITAL LETTER E
  #x24BB ; CIRCLED LATIN CAPITAL LETTER F
  #x24BC ; CIRCLED LATIN CAPITAL LETTER G
  #x24BD ; CIRCLED LATIN CAPITAL LETTER H
  #x24BE ; CIRCLED LATIN CAPITAL LETTER I
  #x24BF ; CIRCLED LATIN CAPITAL LETTER J
  #x24C0 ; CIRCLED LATIN CAPITAL LETTER K
  #x24C1 ; CIRCLED LATIN CAPITAL LETTER L
  #x24C2 ; CIRCLED LATIN CAPITAL LETTER M
  #x24C3 ; CIRCLED LATIN CAPITAL LETTER N
  #x24C4 ; CIRCLED LATIN CAPITAL LETTER O
  #x24C5 ; CIRCLED LATIN CAPITAL LETTER P
  #x24C6 ; CIRCLED LATIN CAPITAL LETTER Q
  #x24C7 ; CIRCLED LATIN CAPITAL LETTER R
  #x24C8 ; CIRCLED LATIN CAPITAL LETTER S
  #x24C9 ; CIRCLED LATIN CAPITAL LETTER T
  #x24CA ; CIRCLED LATIN CAPITAL LETTER U
  #x24CB ; CIRCLED LATIN CAPITAL LETTER V
  #x24CC ; CIRCLED LATIN CAPITAL LETTER W
  #x24CD ; CIRCLED LATIN CAPITAL LETTER X
  #x24CE ; CIRCLED LATIN CAPITAL LETTER Y
  #x24CF ; CIRCLED LATIN CAPITAL LETTER Z
  #x24D0 ; CIRCLED LATIN SMALL LETTER A
  #x24D1 ; CIRCLED LATIN SMALL LETTER B
  #x24D2 ; CIRCLED LATIN SMALL LETTER C
  #x24D3 ; CIRCLED LATIN SMALL LETTER D
  #x24D4 ; CIRCLED LATIN SMALL LETTER E
  #x24D5 ; CIRCLED LATIN SMALL LETTER F
  #x24D6 ; CIRCLED LATIN SMALL LETTER G
  #x24D7 ; CIRCLED LATIN SMALL LETTER H
  #x24D8 ; CIRCLED LATIN SMALL LETTER I
  #x24D9 ; CIRCLED LATIN SMALL LETTER J
  #x24DA ; CIRCLED LATIN SMALL LETTER K
  #x24DB ; CIRCLED LATIN SMALL LETTER L
  #x24DC ; CIRCLED LATIN SMALL LETTER M
  #x24DD ; CIRCLED LATIN SMALL LETTER N
  #x24DE ; CIRCLED LATIN SMALL LETTER O
  #x24DF ; CIRCLED LATIN SMALL LETTER P
  #x24E0 ; CIRCLED LATIN SMALL LETTER Q
  #x24E1 ; CIRCLED LATIN SMALL LETTER R
  #x24E2 ; CIRCLED LATIN SMALL LETTER S
  #x24E3 ; CIRCLED LATIN SMALL LETTER T
  #x24E4 ; CIRCLED LATIN SMALL LETTER U
  #x24E5 ; CIRCLED LATIN SMALL LETTER V
  #x24E6 ; CIRCLED LATIN SMALL LETTER W
  #x24E7 ; CIRCLED LATIN SMALL LETTER X
  #x24E8 ; CIRCLED LATIN SMALL LETTER Y
  #x24E9 ; CIRCLED LATIN SMALL LETTER Z
  #x24EB ; NEGATIVE CIRCLED NUMBER ELEVEN
  #x24EC ; NEGATIVE CIRCLED NUMBER TWELVE
  #x24ED ; NEGATIVE CIRCLED NUMBER THIRTEEN
  #x24EE ; NEGATIVE CIRCLED NUMBER FOURTEEN
  #x24EF ; NEGATIVE CIRCLED NUMBER FIFTEEN
  #x24F0 ; NEGATIVE CIRCLED NUMBER SIXTEEN
  #x24F1 ; NEGATIVE CIRCLED NUMBER SEVENTEEN
  #x24F2 ; NEGATIVE CIRCLED NUMBER EIGHTEEN
  #x24F3 ; NEGATIVE CIRCLED NUMBER NINETEEN
  #x24F4 ; NEGATIVE CIRCLED NUMBER TWENTY
  #x24F5 ; DOUBLE CIRCLED DIGIT ONE
  #x24F6 ; DOUBLE CIRCLED DIGIT TWO
  #x24F7 ; DOUBLE CIRCLED DIGIT THREE
  #x24F8 ; DOUBLE CIRCLED DIGIT FOUR
  #x24F9 ; DOUBLE CIRCLED DIGIT FIVE
  #x24FA ; DOUBLE CIRCLED DIGIT SIX
  #x24FB ; DOUBLE CIRCLED DIGIT SEVEN
  #x24FC ; DOUBLE CIRCLED DIGIT EIGHT
  #x24FD ; DOUBLE CIRCLED DIGIT NINE
  #x24FE ; DOUBLE CIRCLED NUMBER TEN
  #x24FF ; NEGATIVE CIRCLED DIGIT ZERO
  #x2500 ; BOX DRAWINGS LIGHT HORIZONTAL
  #x2501 ; BOX DRAWINGS HEAVY HORIZONTAL
  #x2502 ; BOX DRAWINGS LIGHT VERTICAL
  #x2503 ; BOX DRAWINGS HEAVY VERTICAL
  #x2504 ; BOX DRAWINGS LIGHT TRIPLE DASH HORIZONTAL
  #x2505 ; BOX DRAWINGS HEAVY TRIPLE DASH HORIZONTAL
  #x2506 ; BOX DRAWINGS LIGHT TRIPLE DASH VERTICAL
  #x2507 ; BOX DRAWINGS HEAVY TRIPLE DASH VERTICAL
  #x2508 ; BOX DRAWINGS LIGHT QUADRUPLE DASH HORIZONTAL
  #x2509 ; BOX DRAWINGS HEAVY QUADRUPLE DASH HORIZONTAL
  #x250A ; BOX DRAWINGS LIGHT QUADRUPLE DASH VERTICAL
  #x250B ; BOX DRAWINGS HEAVY QUADRUPLE DASH VERTICAL
  #x250C ; BOX DRAWINGS LIGHT DOWN AND RIGHT
  #x250D ; BOX DRAWINGS DOWN LIGHT AND RIGHT HEAVY
  #x250E ; BOX DRAWINGS DOWN HEAVY AND RIGHT LIGHT
  #x250F ; BOX DRAWINGS HEAVY DOWN AND RIGHT
  #x2510 ; BOX DRAWINGS LIGHT DOWN AND LEFT
  #x2511 ; BOX DRAWINGS DOWN LIGHT AND LEFT HEAVY
  #x2512 ; BOX DRAWINGS DOWN HEAVY AND LEFT LIGHT
  #x2513 ; BOX DRAWINGS HEAVY DOWN AND LEFT
  #x2514 ; BOX DRAWINGS LIGHT UP AND RIGHT
  #x2515 ; BOX DRAWINGS UP LIGHT AND RIGHT HEAVY
  #x2516 ; BOX DRAWINGS UP HEAVY AND RIGHT LIGHT
  #x2517 ; BOX DRAWINGS HEAVY UP AND RIGHT
  #x2518 ; BOX DRAWINGS LIGHT UP AND LEFT
  #x2519 ; BOX DRAWINGS UP LIGHT AND LEFT HEAVY
  #x251A ; BOX DRAWINGS UP HEAVY AND LEFT LIGHT
  #x251B ; BOX DRAWINGS HEAVY UP AND LEFT
  #x251C ; BOX DRAWINGS LIGHT VERTICAL AND RIGHT
  #x251D ; BOX DRAWINGS VERTICAL LIGHT AND RIGHT HEAVY
  #x251E ; BOX DRAWINGS UP HEAVY AND RIGHT DOWN LIGHT
  #x251F ; BOX DRAWINGS DOWN HEAVY AND RIGHT UP LIGHT
  #x2520 ; BOX DRAWINGS VERTICAL HEAVY AND RIGHT LIGHT
  #x2521 ; BOX DRAWINGS DOWN LIGHT AND RIGHT UP HEAVY
  #x2522 ; BOX DRAWINGS UP LIGHT AND RIGHT DOWN HEAVY
  #x2523 ; BOX DRAWINGS HEAVY VERTICAL AND RIGHT
  #x2524 ; BOX DRAWINGS LIGHT VERTICAL AND LEFT
  #x2525 ; BOX DRAWINGS VERTICAL LIGHT AND LEFT HEAVY
  #x2526 ; BOX DRAWINGS UP HEAVY AND LEFT DOWN LIGHT
  #x2527 ; BOX DRAWINGS DOWN HEAVY AND LEFT UP LIGHT
  #x2528 ; BOX DRAWINGS VERTICAL HEAVY AND LEFT LIGHT
  #x2529 ; BOX DRAWINGS DOWN LIGHT AND LEFT UP HEAVY
  #x252A ; BOX DRAWINGS UP LIGHT AND LEFT DOWN HEAVY
  #x252B ; BOX DRAWINGS HEAVY VERTICAL AND LEFT
  #x252C ; BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
  #x252D ; BOX DRAWINGS LEFT HEAVY AND RIGHT DOWN LIGHT
  #x252E ; BOX DRAWINGS RIGHT HEAVY AND LEFT DOWN LIGHT
  #x252F ; BOX DRAWINGS DOWN LIGHT AND HORIZONTAL HEAVY
  #x2530 ; BOX DRAWINGS DOWN HEAVY AND HORIZONTAL LIGHT
  #x2531 ; BOX DRAWINGS RIGHT LIGHT AND LEFT DOWN HEAVY
  #x2532 ; BOX DRAWINGS LEFT LIGHT AND RIGHT DOWN HEAVY
  #x2533 ; BOX DRAWINGS HEAVY DOWN AND HORIZONTAL
  #x2534 ; BOX DRAWINGS LIGHT UP AND HORIZONTAL
  #x2535 ; BOX DRAWINGS LEFT HEAVY AND RIGHT UP LIGHT
  #x2536 ; BOX DRAWINGS RIGHT HEAVY AND LEFT UP LIGHT
  #x2537 ; BOX DRAWINGS UP LIGHT AND HORIZONTAL HEAVY
  #x2538 ; BOX DRAWINGS UP HEAVY AND HORIZONTAL LIGHT
  #x2539 ; BOX DRAWINGS RIGHT LIGHT AND LEFT UP HEAVY
  #x253A ; BOX DRAWINGS LEFT LIGHT AND RIGHT UP HEAVY
  #x253B ; BOX DRAWINGS HEAVY UP AND HORIZONTAL
  #x253C ; BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
  #x253D ; BOX DRAWINGS LEFT HEAVY AND RIGHT VERTICAL LIGHT
  #x253E ; BOX DRAWINGS RIGHT HEAVY AND LEFT VERTICAL LIGHT
  #x253F ; BOX DRAWINGS VERTICAL LIGHT AND HORIZONTAL HEAVY
  #x2540 ; BOX DRAWINGS UP HEAVY AND DOWN HORIZONTAL LIGHT
  #x2541 ; BOX DRAWINGS DOWN HEAVY AND UP HORIZONTAL LIGHT
  #x2542 ; BOX DRAWINGS VERTICAL HEAVY AND HORIZONTAL LIGHT
  #x2543 ; BOX DRAWINGS LEFT UP HEAVY AND RIGHT DOWN LIGHT
  #x2544 ; BOX DRAWINGS RIGHT UP HEAVY AND LEFT DOWN LIGHT
  #x2545 ; BOX DRAWINGS LEFT DOWN HEAVY AND RIGHT UP LIGHT
  #x2546 ; BOX DRAWINGS RIGHT DOWN HEAVY AND LEFT UP LIGHT
  #x2547 ; BOX DRAWINGS DOWN LIGHT AND UP HORIZONTAL HEAVY
  #x2548 ; BOX DRAWINGS UP LIGHT AND DOWN HORIZONTAL HEAVY
  #x2549 ; BOX DRAWINGS RIGHT LIGHT AND LEFT VERTICAL HEAVY
  #x254A ; BOX DRAWINGS LEFT LIGHT AND RIGHT VERTICAL HEAVY
  #x254B ; BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
  #x2550 ; BOX DRAWINGS DOUBLE HORIZONTAL
  #x2551 ; BOX DRAWINGS DOUBLE VERTICAL
  #x2552 ; BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
  #x2553 ; BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE
  #x2554 ; BOX DRAWINGS DOUBLE DOWN AND RIGHT
  #x2555 ; BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE
  #x2556 ; BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE
  #x2557 ; BOX DRAWINGS DOUBLE DOWN AND LEFT
  #x2558 ; BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
  #x2559 ; BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
  #x255A ; BOX DRAWINGS DOUBLE UP AND RIGHT
  #x255B ; BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
  #x255C ; BOX DRAWINGS UP DOUBLE AND LEFT SINGLE
  #x255D ; BOX DRAWINGS DOUBLE UP AND LEFT
  #x255E ; BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
  #x255F ; BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
  #x2560 ; BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
  #x2561 ; BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
  #x2562 ; BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE
  #x2563 ; BOX DRAWINGS DOUBLE VERTICAL AND LEFT
  #x2564 ; BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE
  #x2565 ; BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE
  #x2566 ; BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
  #x2567 ; BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
  #x2568 ; BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
  #x2569 ; BOX DRAWINGS DOUBLE UP AND HORIZONTAL
  #x256A ; BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
  #x256B ; BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE
  #x256C ; BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
  #x256D ; BOX DRAWINGS LIGHT ARC DOWN AND RIGHT
  #x256E ; BOX DRAWINGS LIGHT ARC DOWN AND LEFT
  #x256F ; BOX DRAWINGS LIGHT ARC UP AND LEFT
  #x2570 ; BOX DRAWINGS LIGHT ARC UP AND RIGHT
  #x2571 ; BOX DRAWINGS LIGHT DIAGONAL UPPER RIGHT TO LOWER LEFT
  #x2572 ; BOX DRAWINGS LIGHT DIAGONAL UPPER LEFT TO LOWER RIGHT
  #x2573 ; BOX DRAWINGS LIGHT DIAGONAL CROSS
  #x2580 ; UPPER HALF BLOCK
  #x2581 ; LOWER ONE EIGHTH BLOCK
  #x2582 ; LOWER ONE QUARTER BLOCK
  #x2583 ; LOWER THREE EIGHTHS BLOCK
  #x2584 ; LOWER HALF BLOCK
  #x2585 ; LOWER FIVE EIGHTHS BLOCK
  #x2586 ; LOWER THREE QUARTERS BLOCK
  #x2587 ; LOWER SEVEN EIGHTHS BLOCK
  #x2588 ; FULL BLOCK
  #x2589 ; LEFT SEVEN EIGHTHS BLOCK
  #x258A ; LEFT THREE QUARTERS BLOCK
  #x258B ; LEFT FIVE EIGHTHS BLOCK
  #x258C ; LEFT HALF BLOCK
  #x258D ; LEFT THREE EIGHTHS BLOCK
  #x258E ; LEFT ONE QUARTER BLOCK
  #x258F ; LEFT ONE EIGHTH BLOCK
  #x2592 ; MEDIUM SHADE
  #x2593 ; DARK SHADE
  #x2594 ; UPPER ONE EIGHTH BLOCK
  #x2595 ; RIGHT ONE EIGHTH BLOCK
  #x25A0 ; BLACK SQUARE
  #x25A1 ; WHITE SQUARE
  #x25A3 ; WHITE SQUARE CONTAINING BLACK SMALL SQUARE
  #x25A4 ; SQUARE WITH HORIZONTAL FILL
  #x25A5 ; SQUARE WITH VERTICAL FILL
  #x25A6 ; SQUARE WITH ORTHOGONAL CROSSHATCH FILL
  #x25A7 ; SQUARE WITH UPPER LEFT TO LOWER RIGHT FILL
  #x25A8 ; SQUARE WITH UPPER RIGHT TO LOWER LEFT FILL
  #x25A9 ; SQUARE WITH DIAGONAL CROSSHATCH FILL
  #x25B2 ; BLACK UP-POINTING TRIANGLE
  #x25B3 ; WHITE UP-POINTING TRIANGLE
  #x25B6 ; BLACK RIGHT-POINTING TRIANGLE
  #x25B7 ; WHITE RIGHT-POINTING TRIANGLE
  #x25BC ; BLACK DOWN-POINTING TRIANGLE
  #x25BD ; WHITE DOWN-POINTING TRIANGLE
  #x25C0 ; BLACK LEFT-POINTING TRIANGLE
  #x25C1 ; WHITE LEFT-POINTING TRIANGLE
  #x25C6 ; BLACK DIAMOND
  #x25C7 ; WHITE DIAMOND
  #x25C8 ; WHITE DIAMOND CONTAINING BLACK SMALL DIAMOND
  #x25CB ; WHITE CIRCLE
  #x25CE ; BULLSEYE
  #x25CF ; BLACK CIRCLE
  #x25D0 ; CIRCLE WITH LEFT HALF BLACK
  #x25D1 ; CIRCLE WITH RIGHT HALF BLACK
  #x25E2 ; BLACK LOWER RIGHT TRIANGLE
  #x25E3 ; BLACK LOWER LEFT TRIANGLE
  #x25E4 ; BLACK UPPER LEFT TRIANGLE
  #x25E5 ; BLACK UPPER RIGHT TRIANGLE
  #x25EF ; LARGE CIRCLE
  #x2605 ; BLACK STAR
  #x2606 ; WHITE STAR
  #x2609 ; SUN
  #x260E ; BLACK TELEPHONE
  #x260F ; WHITE TELEPHONE
  #x2614 ; UMBRELLA WITH RAIN DROPS
  #x2615 ; HOT BEVERAGE
  #x261C ; WHITE LEFT POINTING INDEX
  #x261E ; WHITE RIGHT POINTING INDEX
  #x2640 ; FEMALE SIGN
  #x2642 ; MALE SIGN
  #x2660 ; BLACK SPADE SUIT
  #x2661 ; WHITE HEART SUIT
  #x2663 ; BLACK CLUB SUIT
  #x2664 ; WHITE SPADE SUIT
  #x2665 ; BLACK HEART SUIT
  #x2667 ; WHITE CLUB SUIT
  #x2668 ; HOT SPRINGS
  #x2669 ; QUARTER NOTE
  #x266A ; EIGHTH NOTE
  #x266C ; BEAMED SIXTEENTH NOTES
  #x266D ; MUSIC FLAT SIGN
  #x266F ; MUSIC SHARP SIGN
  #x273D ; HEAVY TEARDROP-SPOKED ASTERISK
  #x2776 ; DINGBAT NEGATIVE CIRCLED DIGIT ONE
  #x2777 ; DINGBAT NEGATIVE CIRCLED DIGIT TWO
  #x2778 ; DINGBAT NEGATIVE CIRCLED DIGIT THREE
  #x2779 ; DINGBAT NEGATIVE CIRCLED DIGIT FOUR
  #x277A ; DINGBAT NEGATIVE CIRCLED DIGIT FIVE
  #x277B ; DINGBAT NEGATIVE CIRCLED DIGIT SIX
  #x277C ; DINGBAT NEGATIVE CIRCLED DIGIT SEVEN
  #x277D ; DINGBAT NEGATIVE CIRCLED DIGIT EIGHT
  #x277E ; DINGBAT NEGATIVE CIRCLED DIGIT NINE
  #x277F ; DINGBAT NEGATIVE CIRCLED NUMBER TEN
  #xFFFD ; REPLACEMENT CHARACTER
))
