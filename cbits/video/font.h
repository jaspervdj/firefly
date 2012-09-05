#ifndef FF_VIDEO_FONT_H
#define FF_VIDEO_FONT_H

#include <ft2build.h>
#include FT_FREETYPE_H

#include "video/image.h"

typedef struct {
    ff_image *image;
    float advance;
    float top;
    float left;
} ff_glyph;

ff_glyph *ff_glyphFromGlyphSlot(FT_GlyphSlot glyphSlot);
void ff_glyphFree(ff_glyph *glyph);

struct ff_glyphNodeStruct {
    unsigned long codepoint;
    ff_glyph *glyph;
    struct ff_glyphNodeStruct *next;
};

typedef struct ff_glyphNodeStruct ff_glyphNode;

ff_glyphNode *ff_glyphNodeCreate(unsigned long codepoint, ff_glyph *glyph);
void ff_glyphNodeFree(ff_glyphNode *glyphNode);

typedef struct {
    int size;
    FT_Library library;
    FT_Face face;

    ff_glyphNode **glyphNodes;
    int numGlyphNodes;
} ff_font;

ff_font *ff_fontFromTtf(const char *filePath, int size);
void ff_fontFree(ff_font *font);

ff_glyph *ff_fontLoadGlyph(ff_font *font, unsigned long codepoint);
ff_glyph *ff_fontLookupGlyph(ff_font *font, unsigned long codepoint);

double ff_fontStringWidth(ff_font *font,
        const unsigned long *string, int stringLength);

#endif
