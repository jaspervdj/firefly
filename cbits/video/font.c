#include "video/font.h"

ff_glyph *ff_glyphFromGlyphSlot(FT_GlyphSlot glyphSlot) {
    ff_glyph *glyph;
    FT_Bitmap bitmap = glyphSlot->bitmap;
    int width = bitmap.width;
    int height = bitmap.rows;
    int x, y;

    glyph = malloc(sizeof(ff_glyph));
    glyph->advance = (float) glyphSlot->advance.x / 64;
    glyph->left = (float) glyphSlot->metrics.horiBearingX / 64;
    glyph->top = (float) glyphSlot->metrics.horiBearingY / 64;
    
    GLubyte *pixels = malloc(width * height * 2 * sizeof(GLubyte));

    for(y = 0; y < height; y++) {
        for(x = 0; x < width; x++) {
            pixels[y * width * 2 + x * 2] = 0xff;
            pixels[y * width * 2 + x * 2 + 1] = bitmap.buffer[width * y + x];
        }
    }

    glyph->image = ff_imageCreate(width, height, 2, pixels);

    free(pixels);
    return glyph;
}

void ff_glyphFree(ff_glyph *glyph) {
    ff_imageFree(glyph->image);
    free(glyph);
}

ff_glyphNode *ff_glyphNodeCreate(unsigned long codepoint, ff_glyph *glyph) {
    ff_glyphNode *glyphNode = malloc(sizeof(ff_glyphNode));
    glyphNode->codepoint = codepoint;
    glyphNode->glyph = glyph;
    glyphNode->next = 0;
    return glyphNode;
}

void ff_glyphNodeFree(ff_glyphNode *glyphNode) {
    if(glyphNode->next) ff_glyphNodeFree(glyphNode->next);
    ff_glyphFree(glyphNode->glyph);
    free(glyphNode);
}

ff_font *ff_fontFromTtf(const char *filePath, int size) {
    FT_Library library;
    FT_Face face;
    ff_font *font;
    int i;

#ifdef DEBUG
    printf("video/font/ff_fontFromTtf(\"%s\", %d)\n", filePath, size);
#endif

    if(FT_Init_FreeType(&library)) {
        fprintf(stderr, "video/font/ff_fontFromTtf: Can't init FreeType 2");
        return 0;
    }

    if(FT_New_Face(library, filePath, 0, &face)) {
        fprintf(stderr,
                "video/font/ff_fontFromTtf: Can't read \"%s\" as TTF Font\n",
                filePath);
        FT_Done_FreeType(library);
        return 0;
    }

    font = malloc(sizeof(ff_font));
    font->size = size;
    font->library = library;
    font->face = face;

    FT_Set_Pixel_Sizes(face, 0, (FT_UInt) size);

    /* Make space for glyphs, nothing is loaded yet */
    font->numGlyphNodes = 512;
    font->glyphNodes = malloc(font->numGlyphNodes * sizeof(ff_glyphNode*));
    for(i = 0; i < font->numGlyphNodes; i++) {
        font->glyphNodes[i] = 0;
    }

    return font;
}

void ff_fontFree(ff_font *font) {
    int i;

#ifdef DEBUG
    printf("video/font/ff_fontFree(_)\n");
#endif

    FT_Done_Face(font->face);
    FT_Done_FreeType(font->library);

    /* Free all glyphs */
    for(i = 0; i < font->numGlyphNodes; i++) {
        if(font->glyphNodes[i]) ff_glyphNodeFree(font->glyphNodes[i]);
    }
    free(font->glyphNodes);

    free(font);
}

ff_glyph *ff_fontLoadGlyph(ff_font *font, unsigned long codepoint) {
    int index;
    FT_GlyphSlot glyphSlot;

#ifdef DEBUG
    printf("video/font/ff_fontLookupGlyph(_, %lu)\n", codepoint);
#endif

    index = FT_Get_Char_Index(font->face, (FT_ULong) codepoint);
    FT_Load_Glyph(font->face, index, FT_LOAD_RENDER);
    glyphSlot = font->face->glyph;
    return ff_glyphFromGlyphSlot(glyphSlot);
}

ff_glyph *ff_fontLookupGlyph(ff_font *font, unsigned long codepoint) {
    int i = (int) (codepoint % font->numGlyphNodes); 
    ff_glyphNode *node;
    ff_glyph *glyph;

    node = font->glyphNodes[i];

    /* No root glyph node, load */
    if(!node) {
        glyph = ff_fontLoadGlyph(font, codepoint); 
        font->glyphNodes[i] = ff_glyphNodeCreate(codepoint, glyph);
        return glyph;

    /* Root node, browse list until found */
    } else {
        /* It's the root node! */
        if (node->codepoint == codepoint) return node->glyph;

        /* Or some other node... */
        while(node->next) {
            node = node->next;
            if(node->codepoint == codepoint) return node->glyph;
        }

        /* Or it's not loaded yet */
        glyph = ff_fontLoadGlyph(font, codepoint);
        node->next = ff_glyphNodeCreate(codepoint, glyph);
        return glyph;
    }
}

double ff_fontStringWidth(ff_font *font,
        const unsigned long *string, int stringLength) {
    double width = 0;
    int i;
    ff_glyph *glyph;

    /* Take the advance of each character */
    for(i = 0; i + 1 < stringLength; i++) {
        glyph = ff_fontLookupGlyph(font, string[i]);
        width += (double) glyph->advance;
    }

    /* And the width of the last */
    if(i < stringLength) {
        glyph = ff_fontLookupGlyph(font, string[i]);
        width += (double) glyph->image->width;
    }

    return width;
}
