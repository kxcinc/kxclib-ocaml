#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <zlib.h>

using namespace std;

namespace kxclib_fastpng {
  int write_png_rgba(const char *filename, int width, int height, int32_t *buffer, int compression_level);
}

int main(int argc, char *argv[])
{
    int width = 9;
    int height = 12;
    
    int32_t *buff = new int32_t[width*height];
    for (int x = 0; x < width; ++x)
      for (int y = 0; y < height; ++y) {
        int32_t color;
        if (x == y) {
          color = 0xffcccccc;
        } else if (y % 3 == 0) {
          color = 0xffff0000;
        } else if (y % 3 == 1) {
          color = 0xff00ff00;
        } else if (y % 3 == 2) {
          color = 0xff0000ff;
        }
        buff[x*height+y] = color;
      }

  return kxclib_fastpng::write_png_rgba("test.png", width, height, buff, Z_DEFAULT_COMPRESSION);
}

// hack to make CMake happy
extern "C" {
size_t caml_string_length() {
  return -1;
}
}
