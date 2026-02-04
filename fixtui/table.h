/*
 * Replacement table: pairs of {"before", "after"} UTF-8 strings.
 * Before and after MUST have the same byte length.
 */
static const struct {
    const char *before;
    const char *after;
} table[] = {
    {"●", "⬤"}, // U+25CF BLACK CIRCLE -> U+2B24 BLACK LARGE CIRCLE
    {"○", "◦"}, // U+25CB WHITE CIRCLE -> U+25E6 WHITE BULLET
    {"◯", "◦"}, // U+25EF LARGE CIRCLE -> U+25E6 WHITE BULLET
    {"■", "▪"}, // U+25A0 BLACK SQUARE -> U+25AA BLACK SMALL SQUARE
    {"▲", "▴"}, // U+25B2 BLACK UP-POINTING TRIANGLE -> U+25B4 SMALL BLACK UP-POINTING TRIANGLE
    {"▼", "▾"}, // U+25BC BLACK DOWN-POINTING TRIANGLE -> U+25BE SMALL BLACK DOWN-POINTING TRIANGLE
    {"←", "￩"}, // U+2190 LEFTWARDS ARROW -> U+FFE9 HALFWIDTH LEFTWARDS ARROW
    {"↑", "￪"}, // U+2191 UPWARDS ARROW -> U+FFEA HALFWIDTH UPWARDS ARROW
    {"→", "￫"}, // U+2192 RIGHTWARDS ARROW -> U+FFEB HALFWIDTH RIGHTWARDS ARROW
    {"↓", "￬"}, // U+2193 DOWNWARDS ARROW -> U+FFEC HALFWIDTH DOWNWARDS ARROW
};
