#include <cinttypes>
#include <cstring>
#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <png.h>
#include <zlib.h>

namespace kxclib_fastpng {
  int write_png_rgba(const char* filename, int width, int height, int32_t *buffer)
  {
    int code = 0;
    FILE *fp = NULL;
    png_structp png_ptr = NULL;
    png_infop info_ptr = NULL;
    png_bytep row = NULL;

    // Open file for writing (binary mode)
    fp = fopen(filename, "wb");
    if (fp == NULL) {
      fprintf(stderr, "Could not open file %s for writing\n", filename);
      code = 1;
      goto finalise;
    }

    // Initialize write structure
    png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
    if (png_ptr == NULL) {
      fprintf(stderr, "Could not allocate write struct\n");
      code = 1;
      goto finalise;
    }

    // Initialize info structure
    info_ptr = png_create_info_struct(png_ptr);
    if (info_ptr == NULL) {
      fprintf(stderr, "Could not allocate info struct\n");
      code = 1;
      goto finalise;
    }

    // Setup Exception handling
    if (setjmp(png_jmpbuf(png_ptr))) {
      fprintf(stderr, "Error during png creation\n");
      code = 1;
      goto finalise;
    }

    png_init_io(png_ptr, fp);

    // configure png writing options
    png_set_filter(png_ptr, 0, PNG_FILTER_NONE);
    png_set_compression_level(png_ptr, Z_DEFAULT_COMPRESSION);

    // Write header (8 bit colour depth)
    png_set_IHDR(png_ptr, info_ptr, width, height,
                 8, PNG_COLOR_TYPE_RGBA, PNG_INTERLACE_NONE,
                 PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);

    png_write_info(png_ptr, info_ptr);

    // Allocate memory for one row (3 bytes per pixel - RGB)
    row = (png_bytep) malloc(width*4 * sizeof(png_byte));

    // Write image data
    int x, y;
    for (y=0 ; y<height ; y++) {
      for (x=0 ; x<width ; x++) {
        int32_t val = buffer[x*height + y];
        png_byte *ptr = row+x*4;
        ptr[3] = (val>>8*3) & 0xFF;
        ptr[0] = (val>>8*2) & 0xFF;
        ptr[1] = (val>>8*1) & 0xFF;
        ptr[2] = (val>>8*0) & 0xFF;
      }
      png_write_row(png_ptr, row);
    }

    // End write
    png_write_end(png_ptr, NULL);

    finalise:
    if (fp != NULL) fclose(fp);
    if (info_ptr != NULL) png_free_data(png_ptr, info_ptr, PNG_FREE_ALL, -1);
    if (png_ptr != NULL) png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
    if (row != NULL) free(row);

    return code;
  }

}

struct cachedstr {
  char *buff;
  cachedstr(value ocstr) {
    mlsize_t slen = caml_string_length(ocstr);
    buff = new char[slen+1];
    memcpy(buff, String_val(ocstr), slen);
    buff[slen] = '\0';
  }
  ~cachedstr() {
    delete[] buff;
  }
};

extern "C" {

CAMLprim
value fastpng_libpng_write_rgba(value caml_filename, value ba) {
  intnat *dims = Caml_ba_array_val(ba)->dim;
  intnat width = dims[0], height = dims[1];
  cachedstr filename(caml_filename);
  int32_t *data = (int32_t *)Caml_ba_data_val(ba);
  kxclib_fastpng::write_png_rgba(filename.buff, width, height, data);
  return Val_unit;
}

}
