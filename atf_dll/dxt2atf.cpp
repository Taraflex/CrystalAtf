/*
Copyright (c) 2012 Adobe Systems Incorporated

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#include <iostream>
#include <fstream>
#include <sstream>
#include <system.hpp>

#include <windows.h>
#include <process.h>

#include "3rdparty/jpegxr/jpegxr.h"
#include "3rdparty/jpegxr/jxr_priv.h"
#include "3rdparty/lzma/LzmaLib.h"

#define DXT5 894720068
#define DXT1 827611204

#define EXPORT extern "C" __declspec (dllexport)

#define multithread

int32_t gTrimFlexBits = 0; // JXR setting
int32_t gJxrQuality = 0; // JXR setting
jxr_color_fmt_t gJxrFormat = JXR_YUV444; // JXR setting

using namespace std;

template<typename Type>
void trace(Type x) {
	std::cout << x << std::endl;
}

inline uint8_t read_uint8(istream &file) {
	return file.get();
}

inline uint16_t read_uint16(istream &file) {
	return (read_uint8(file) << 0) | (read_uint8(file) << 8);
}

inline void write_uint24(uint32_t v, ostream &ofile) {
	ofile.put((v >> 16)&0xFF);
	ofile.put((v >> 8)&0xFF);
	ofile.put((v >> 0)&0xFF);
}

inline void write_uint24(uint32_t v, uint8_t *ofile) {
	ofile[2] = ((v >> 16) & 0xFF);
	ofile[1] = ((v >> 8) & 0xFF);
	ofile[0] = ((v >> 0) & 0xFF);
}

union DDS_header {
	struct {
		unsigned int dwMagic;
		unsigned int dwSize;
		unsigned int dwFlags;
		unsigned int dwHeight;
		unsigned int dwWidth;
		unsigned int dwPitchOrLinearSize;
		unsigned int dwDepth;
		unsigned int dwMipMapCount;
		unsigned int dwReserved1[11];

		// DDPIXELFORMAT
		struct {
			unsigned int dwSize;
			unsigned int dwFlags;
			unsigned int dwFourCC;
			unsigned int dwRGBBitCount;
			unsigned int dwRBitMask;
			unsigned int dwGBitMask;
			unsigned int dwBBitMask;
			unsigned int dwAlphaBitMask;
		} sPixelFormat;

		// DDCAPS2
		struct {
			unsigned int dwCaps1;
			unsigned int dwCaps2;
			unsigned int dwDDSX;
			unsigned int dwReserved;
		} sCaps;
		unsigned int dwReserved2;
	};

	char data[128];
};

struct ImageData {
	uint32_t size;

	uint8_t *raw;

	uint8_t *dxt5_alp; // dxt1 color map
	uint8_t *dxt5_abt; // dxt1 bit data
	uint16_t *dxt5_col; // dxt1 color map
	uint8_t *dxt5_bit; // dxt1 bit data

	uint16_t *dxt1_col; // dxt1 color map
	uint8_t *dxt1_bit; // dxt1 bit data

	uint16_t *pvrtc_col; // pvrtc color map
	uint8_t *pvrtc_d0; // pvrtc data top
	uint32_t *pvrtc_d1; // pvrtc data bottom

	uint32_t *etc1_col; // etc1 color 24bit
	uint8_t *etc1_d0; // etc1 data top
	uint32_t *etc1_d1; // etc1 data bottom
};

static unsigned int tile_width_in_MB[4096 * 2] = {0};
static unsigned int tile_height_in_MB[4096 * 2] = {0};

static bool SetJPEGXRCommon(jxr_container_t container, jxr_image_t image,
	bool alpha, int32_t w, int32_t h) {

	jxr_set_BANDS_PRESENT(image, JXR_BP_ALL);
	jxr_set_TRIM_FLEXBITS(image, gTrimFlexBits);
	jxr_set_OVERLAP_FILTER(image, 0);
	jxr_set_DISABLE_TILE_OVERLAP(image, 1);
	jxr_set_FREQUENCY_MODE_CODESTREAM_FLAG(image, 0);
	jxr_set_PROFILE_IDC(image, 111);
	jxr_set_LEVEL_IDC(image, 255);
	jxr_set_LONG_WORD_FLAG(image, 1);
	jxr_set_ALPHA_IMAGE_PLANE_FLAG(image, alpha ? 1 : 0);

	if (w < 32 || h < 64 || w * h < 64 * 64) {
		jxr_set_NUM_VER_TILES_MINUS1(image, 1);
		jxr_set_NUM_HOR_TILES_MINUS1(image, 1);
		tile_width_in_MB[0] = 0;
		tile_height_in_MB[1] = 0;
		jxr_set_TILE_WIDTH_IN_MB(image, tile_width_in_MB);
		jxr_set_TILE_HEIGHT_IN_MB(image, tile_height_in_MB);
	}
	else if (h < 256) {
		jxr_set_NUM_VER_TILES_MINUS1(image, 1);
		jxr_set_NUM_HOR_TILES_MINUS1(image, 4);
		tile_width_in_MB[0] = w / 16;
		tile_height_in_MB[0] = h / 16 / 4;
		tile_height_in_MB[1] = h / 16 / 4;
		tile_height_in_MB[2] = h / 16 / 4;
		tile_height_in_MB[3] = h / 16 / 4;
		jxr_set_TILE_WIDTH_IN_MB(image, tile_width_in_MB);
		jxr_set_TILE_HEIGHT_IN_MB(image, tile_height_in_MB);
	}
	else {
		jxr_set_NUM_VER_TILES_MINUS1(image, 1);
		jxr_set_NUM_HOR_TILES_MINUS1(image, 8);
		tile_width_in_MB[0] = w / 16;
		tile_height_in_MB[0] = h / 16 / 8;
		tile_height_in_MB[1] = h / 16 / 8;
		tile_height_in_MB[2] = h / 16 / 8;
		tile_height_in_MB[3] = h / 16 / 8;
		tile_height_in_MB[4] = h / 16 / 8;
		tile_height_in_MB[5] = h / 16 / 8;
		tile_height_in_MB[6] = h / 16 / 8;
		tile_height_in_MB[7] = h / 16 / 8;
		jxr_set_TILE_WIDTH_IN_MB(image, tile_width_in_MB);
		jxr_set_TILE_HEIGHT_IN_MB(image, tile_height_in_MB);
	}

	jxr_set_pixel_format(image, jxrc_get_pixel_format(container));

	return true;
}

static void SetJPEGXRQuality(jxr_image_t image, int32_t quality) {
	if (quality == 0) {
		jxr_set_QP_LOSSLESS(image);
	}
	else {
		if (quality < 16) {
			quality = quality * 2;
		}
		else if (quality <= 48) {
			quality = quality + 18;
		}
		else {
			quality = quality + 18 + 2;
		}
		jxr_set_QP_UNIFORM(image, quality);
	}
}

static bool SetJPEG8(jxr_container_t container, jxr_image_t image,
	int32_t quality, int32_t w, int32_t h) {
	jxr_set_INTERNAL_CLR_FMT(image, JXR_YONLY, 1);
	jxr_set_OUTPUT_CLR_FMT(image, JXR_OCF_YONLY);
	jxr_set_OUTPUT_BITDEPTH(image, JXR_BD8);
	SetJPEGXRCommon(container, image, false, w, h);
	SetJPEGXRQuality(image, quality);
	return true;
}

static bool SetJPEGX565(jxr_container_t container, jxr_image_t image,
	int32_t quality, int32_t w, int32_t h) {
	jxr_set_INTERNAL_CLR_FMT(image, gJxrFormat, 1);
	jxr_set_OUTPUT_CLR_FMT(image, JXR_OCF_RGB);
	jxr_set_OUTPUT_BITDEPTH(image, JXR_BD565);
	SetJPEGXRCommon(container, image, false, w, h);
	SetJPEGXRQuality(image, quality);
	return true;
}

static bool SetJPEGXRaw(jxr_container_t container, jxr_image_t image,
	int32_t quality, bool alpha, int32_t w, int32_t h) {

	jxr_set_INTERNAL_CLR_FMT(image, gJxrFormat, 4);
	jxr_set_OUTPUT_CLR_FMT(image, JXR_OCF_RGB);
	jxr_set_OUTPUT_BITDEPTH(image, JXR_BD8);
	SetJPEGXRCommon(container, image, alpha, w, h);
	SetJPEGXRQuality(image, quality);
	return true;
}

static void Read8Data_DXT5(jxr_image_t image, int mx, int my, int *data) {
	ImageData *imageData = (ImageData*)jxr_get_user_data(image);
	int32_t w = jxr_get_IMAGE_WIDTH(image);
	int32_t h = jxr_get_IMAGE_HEIGHT(image);
	int32_t n = jxr_get_IMAGE_CHANNELS(image);
	for (int32_t y = 0; y < 16; y++) {
		int32_t dy = (my * 16) + y;
		for (int32_t x = 0; x < 16; x++) {
			int32_t dx = (mx * 16) + x;
			if (dy < h && dx < w) {
				uint8_t p = imageData->dxt5_alp[(dy * w) + dx];
				data[(y * 16 + x) * n + 0] = p;
			}
			else {
				data[(y * 16 + x) * n + 0] = 0;
			}
		}
	}
}

static void Read565Data_DXT5(jxr_image_t image, int mx, int my, int *data) {
	ImageData *imageData = (ImageData*)jxr_get_user_data(image);
	int32_t w = jxr_get_IMAGE_WIDTH(image);
	int32_t h = jxr_get_IMAGE_HEIGHT(image);
	int32_t n = jxr_get_IMAGE_CHANNELS(image);
	for (int32_t y = 0; y < 16; y++) {
		int32_t dy = (my * 16) + y;
		for (int32_t x = 0; x < 16; x++) {
			int32_t dx = (mx * 16) + x;
			if (dy < h && dx < w) {
				uint32_t p = imageData->dxt5_col[(dy * w) + dx];
				if (p) {
					int32_t r = (p >> 11) & 0x1F;
					int32_t g = (p >> 5) & 0x3F;
					int32_t b = (p >> 0) & 0x1F;
					// Bug in JPEG-XR encoder: it wants 666 instead of 565
					r = (r << 1) | (r >> 4);
					b = (b << 1) | (b >> 4);
					data[(y * 16 + x) * n + 2] = r;
					data[(y * 16 + x) * n + 1] = g;
					data[(y * 16 + x) * n + 0] = b;
				}
				else {
					data[(y * 16 + x) * n + 2] = 0;
					data[(y * 16 + x) * n + 1] = 0;
					data[(y * 16 + x) * n + 0] = 0;
				}
			}
			else {
				data[(y * 16 + x) * n + 2] = 0;
				data[(y * 16 + x) * n + 1] = 0;
				data[(y * 16 + x) * n + 0] = 0;
			}
		}
	}
}

static void Read565Data_DXT1(jxr_image_t image, int mx, int my, int *data) {
	ImageData *imageData = (ImageData*)jxr_get_user_data(image);
	int32_t w = jxr_get_IMAGE_WIDTH(image);
	int32_t h = jxr_get_IMAGE_HEIGHT(image);
	int32_t n = jxr_get_IMAGE_CHANNELS(image);
	for (int32_t y = 0; y < 16; y++) {
		int32_t dy = (my * 16) + y;
		for (int32_t x = 0; x < 16; x++) {
			int32_t dx = (mx * 16) + x;
			if (dy < h && dx < w) {
				uint32_t p = imageData->dxt1_col[(dy * w) + dx];
				if (p) {
					int32_t r = (p >> 11) & 0x1F;
					int32_t g = (p >> 5) & 0x3F;
					int32_t b = (p >> 0) & 0x1F;
					// Bug in JPEG-XR encoder: it wants 666 instead of 565
					r = (r << 1) | (r >> 4);
					b = (b << 1) | (b >> 4);
					data[(y * 16 + x) * n + 2] = r;
					data[(y * 16 + x) * n + 1] = g;
					data[(y * 16 + x) * n + 0] = b;
				}
				else {
					data[(y * 16 + x) * n + 2] = 0;
					data[(y * 16 + x) * n + 1] = 0;
					data[(y * 16 + x) * n + 0] = 0;
				}
			}
			else {
				data[(y * 16 + x) * n + 2] = 0;
				data[(y * 16 + x) * n + 1] = 0;
				data[(y * 16 + x) * n + 0] = 0;
			}
		}
	}
}

void Read8888Data(jxr_image_t image, int mx, int my, int *data) {
	ImageData *imageData = (ImageData*)jxr_get_user_data(image);
	int32_t w = jxr_get_IMAGE_WIDTH(image);
	int32_t h = jxr_get_IMAGE_HEIGHT(image);
	int32_t n = jxr_get_IMAGE_CHANNELS(image);
	int32_t a = jxr_get_ALPHACHANNEL_FLAG(image);
	n += a;

	uint8_t *raw = imageData->raw;

	for (size_t y = 0; y < 16; y++) {
		size_t dy = ((my * 16) + y) * w + mx * 16;
		size_t _y = 0;
		size_t y16 = y * 16;

		for (size_t x = 0; x < 16; x++) {
			size_t dx = (dy + x) * 4;
			_y = (y16 + x) * n;

			data[_y + 2] = raw[dx + 0];
			data[_y + 1] = raw[dx + 1];
			data[_y] = raw[dx + 2];
			data[_y + 3] = raw[dx + 3];

		}
	}
}

void Read888Data(jxr_image_t image, int mx, int my, int *data) {
	ImageData *imageData = (ImageData*)jxr_get_user_data(image);
	int32_t w = jxr_get_IMAGE_WIDTH(image);
	int32_t h = jxr_get_IMAGE_HEIGHT(image);
	int32_t n = jxr_get_IMAGE_CHANNELS(image);

	uint8_t *raw = imageData->raw;

	for (size_t y = 0; y < 16; y++) {
		size_t dy = ((my * 16) + y) * w + mx * 16;
		size_t _y = 0;
		size_t y16 = y * 16;

		for (size_t x = 0; x < 16; x++) {
			size_t dx = (dy + x) * 3;
			_y = (y16 + x) * n;

			data[_y + 2] = raw[dx];
			data[_y + 1] = raw[dx + 1];
			data[_y + 0] = raw[dx + 2];

		}
	}
}

size_t LzmaSlowCompress(uint8_t *src, uint8_t *dst, size_t len) {
	// size_t  sln = 0x7FFFFFFF;
	int32_t slc = 3;
	int32_t spb = 2;

	size_t bufferLen = len * 2 + 4096;
	size_t propsLen = LZMA_PROPS_SIZE;

	LzmaCompress(dst + LZMA_PROPS_SIZE, &bufferLen, (const unsigned char *)src,
		len, (unsigned char *)dst, &propsLen, 9, 1 << 20, slc, 0, spb, 273, 1);

	return bufferLen + LZMA_PROPS_SIZE;
}

inline uint8_t log2(uint32_t x) {
	return (((x & 0xAAAAAAAA) ? 1 : 0)) | (((x & 0xCCCCCCCC) ? 1 : 0) << 1) |
		(((x & 0xF0F0F0F0) ? 1 : 0) << 2) | (((x & 0xFF00FF00) ? 1 : 0) << 3) |
		(((x & 0xFFFF0000) ? 1 : 0) << 4);
}

void write_header(uint32_t w, uint32_t h, uint8_t format, uint8_t textureCount,
	ostream &ofile) {
	ofile.put('A');
	ofile.put('T');
	ofile.put('F');
	ofile.put(uint8_t(0));
	ofile.put(uint8_t(0));
	ofile.put(uint8_t(0));
	ofile.put(format);
	ofile.put(log2(w));
	ofile.put(log2(h));
	ofile.put(textureCount);
}

typedef struct _ThreadData {
	size_t w;
	size_t h;
	uint8_t *buffer;
	uint8_t *alpha_buffer;
	size_t bufferLen;
	size_t alpha_bufferLen;
	ImageData imageData;
	bool dxt1;
	bool alpha;
	size_t complete;
} THREADDATA, *PTHREADDATA;

DWORD WINAPI CompressInThread(LPVOID data) {
#define _data ((PTHREADDATA)data)
	int w = _data->w;
	int h = _data->h;

	if (_data->dxt1) {
		_data->buffer =
			new uint8_t[max(1, w / 4) * max(1, h / 4)*sizeof(uint32_t) * 2 +
			LZMA_PROPS_SIZE + 4096];

		_data->bufferLen = LzmaSlowCompress(_data->imageData.dxt1_bit,
			_data->buffer, max(1, w / 4) * max(1, h / 4)*sizeof(uint32_t));
	}
	else {
		if (_data->alpha) {
			_data->alpha_buffer =
				new uint8_t[max(1, w / 4) * max(1, h / 4)*sizeof(uint32_t) * 8 +
				LZMA_PROPS_SIZE + 4096];
			_data->alpha_bufferLen = LzmaSlowCompress(_data->imageData.dxt5_abt,
				_data->alpha_buffer, max(1, w / 4) * max(1, h / 4) * 6);
			_data->alpha = false;
			CompressInThread(data);
		}
		else {
			_data->buffer =
				new uint8_t[max(1, w / 4) * max(1, h / 4)*sizeof(uint32_t) * 8 +
				LZMA_PROPS_SIZE + 4096];

			_data->bufferLen = LzmaSlowCompress(_data->imageData.dxt5_bit,
				_data->buffer, max(1, w / 4) * max(1, h / 4) * 6);

		}
	}
	_data->complete++;
#undef _data
	return 0;
}
#define GETTHREAD(function,data)(CreateThread(NULL, 0, (function), (data), 0, NULL))

bool write_dxt1(int32_t w, int32_t h, istream &ifile, ostream &ofile) {
	ImageData imageData;
	imageData.dxt1_col = new uint16_t[max(2, (w / 4)) * max(2, (h / 4) * 2)];
	uint16_t *cl0 = imageData.dxt1_col;
	uint16_t *cl1 = imageData.dxt1_col + max(1, w / 4) * max(1, h / 4);
	imageData.dxt1_bit = new uint8_t[max(1, w / 4) * max(1, h / 4) * 4];
	uint8_t *bit = imageData.dxt1_bit;
	for (int32_t d = 0; d < max(1, w / 4) * max(1, h / 4); d++) {
		uint16_t c0 = read_uint16(ifile);
		*cl0++ = c0;
		uint16_t c1 = read_uint16(ifile);
		*cl1++ = c1;
		*bit++ = read_uint8(ifile);
		*bit++ = read_uint8(ifile);
		*bit++ = read_uint8(ifile);
		*bit++ = read_uint8(ifile);
	}

	PTHREADDATA data = (PTHREADDATA) HeapAlloc(GetProcessHeap(),
		HEAP_ZERO_MEMORY, sizeof(THREADDATA));
	data->w = w;
	data->h = h;
	data->imageData = imageData;
	data->dxt1 = true;
#ifdef multithread
	HANDLE bitThread = GETTHREAD(CompressInThread, data);
#else
	CompressInThread(data);
#endif
	jxr_container_t container = jxr_create_container();
	jxrc_start_file(container);

	if (jxrc_begin_ifd_entry(container) != 0) {
		cerr << "Could not create ATF file!\n\n";
		return false;
	}
	jxrc_set_pixel_format(container, JXRC_FMT_16bppBGR565);
	jxrc_set_image_shape(container, max(1, w / 4), max(2, h / 2));
	jxrc_set_separate_alpha_image_plane(container, 0);
	jxrc_set_image_band_presence(container, JXR_BP_ALL);
	static unsigned char window_params[5] = {0, 0, 0, 0, 0};
	jxr_image_t image = jxr_create_image(max(1, w / 4), max(2, h / 2),
		window_params);

	if (!image) {
		return false;
	}

	SetJPEGX565(container, image, gJxrQuality, max(1, w / 4), max(2, h / 2));

	jxrc_begin_image_data(container);
	jxr_set_block_input(image, Read565Data_DXT1);
	jxr_set_user_data(image, &imageData);
	if (jxr_write_image_bitstream(image, container) != 0) {
		cerr << "JPEGXR encoding error!\n\n";
		return false;
	}
	jxr_destroy(image);
	jxrc_write_container_post(container);
	//
#ifdef multithread
	WaitForSingleObject(bitThread, INFINITE);
	CloseHandle(bitThread);
#endif

	write_uint24(data->bufferLen, ofile);
	ofile.write((const char *)data->buffer, data->bufferLen);

	delete[]data->buffer;

	write_uint24(container->wb.len(), ofile);
	ofile.write((const char *)container->wb.buffer(), container->wb.len());

	jxr_destroy_container(container);

	delete[]imageData.dxt1_col;
	delete[]imageData.dxt1_bit;

	HeapFree(GetProcessHeap(), 0, data);

	return true;
}

bool write_dxt5(int32_t w, int32_t h, istream &ifile, ostream &ofile) {

	ImageData imageData;
	imageData.dxt5_alp = new uint8_t[max(2, (w / 4)) * max(2, (h / 4) * 2)];
	imageData.dxt5_col = new uint16_t[max(2, (w / 4)) * max(2, (h / 4) * 2)];
	uint8_t *al0 = imageData.dxt5_alp;
	uint8_t *al1 = imageData.dxt5_alp + max(1, w / 4) * max(1, h / 4);
	uint16_t *cl0 = imageData.dxt5_col;
	uint16_t *cl1 = imageData.dxt5_col + max(1, w / 4) * max(1, h / 4);
	imageData.dxt5_abt = new uint8_t[max(1, w / 4) * max(1, h / 4) * 6];
	imageData.dxt5_bit = new uint8_t[max(1, w / 4) * max(1, h / 4) * 4];
	uint8_t *abt = (uint8_t*)imageData.dxt5_abt;
	uint8_t *bit = (uint8_t*)imageData.dxt5_bit;
	for (int32_t d = 0; d < max(1, w / 4) * max(1, h / 4); d++) {

		uint8_t a0 = read_uint8(ifile);
		*al0++ = a0;
		uint8_t a1 = read_uint8(ifile);
		*al1++ = a1;

		*abt++ = read_uint8(ifile);
		*abt++ = read_uint8(ifile);
		*abt++ = read_uint8(ifile);
		*abt++ = read_uint8(ifile);
		*abt++ = read_uint8(ifile);
		*abt++ = read_uint8(ifile);

		uint16_t c0 = read_uint16(ifile);
		*cl0++ = c0;
		uint16_t c1 = read_uint16(ifile);
		*cl1++ = c1;
		*bit++ = read_uint8(ifile);
		*bit++ = read_uint8(ifile);
		*bit++ = read_uint8(ifile);
		*bit++ = read_uint8(ifile);

	}

	PTHREADDATA data = (PTHREADDATA) HeapAlloc(GetProcessHeap(),
		HEAP_ZERO_MEMORY, sizeof(THREADDATA));

	data->w = w;
	data->h = h;
	data->imageData = imageData;
	data->complete = 0;
	data->dxt1 = false;

	data->alpha = true;
#ifdef multithread
	HANDLE bitThread = GETTHREAD(CompressInThread, data);
#else
	CompressInThread(data);
#endif
	//
	jxr_container_t container = jxr_create_container();
	jxrc_start_file(container);

	if (jxrc_begin_ifd_entry(container) != 0) {
		cerr << "Could not create ATF file!\n\n";
		return false;
	}
	jxrc_set_pixel_format(container, JXRC_FMT_8bppGray);
	jxrc_set_image_shape(container, max(1, w / 4), max(2, h / 2));
	jxrc_set_separate_alpha_image_plane(container, 0);
	jxrc_set_image_band_presence(container, JXR_BP_ALL);
	static unsigned char window_params[5] = {0, 0, 0, 0, 0};
	jxr_image_t image = jxr_create_image(max(1, w / 4), max(2, h / 2),
		window_params);

	if (!image) {
		return false;
	}

	SetJPEG8(container, image, gJxrQuality, max(1, w / 4), max(2, h / 2));

	jxrc_begin_image_data(container);
	jxr_set_block_input(image, Read8Data_DXT5);
	jxr_set_user_data(image, &imageData);

	if (jxr_write_image_bitstream(image, container) != 0) {
		cerr << "JPEGXR encoding error!\n\n";
		return false;
	}
	jxr_destroy(image);
	jxrc_write_container_post(container);
	//
	jxr_container_t container2 = jxr_create_container();
	jxrc_start_file(container2);

	if (jxrc_begin_ifd_entry(container2) != 0) {
		cerr << "Could not create ATF file!\n\n";
		return false;
	}
	jxrc_set_pixel_format(container2, JXRC_FMT_16bppBGR565);
	jxrc_set_image_shape(container2, max(1, w / 4), max(2, h / 2));
	jxrc_set_separate_alpha_image_plane(container2, 0);
	jxrc_set_image_band_presence(container2, JXR_BP_ALL);
	static unsigned char window_params2[5] = {0, 0, 0, 0, 0};
	jxr_image_t image2 = jxr_create_image(max(1, w / 4), max(2, h / 2),
		window_params2);

	if (!image2) {
		return false;
	}

	SetJPEGX565(container2, image2, gJxrQuality, max(1, w / 4), max(2, h / 2));

	jxrc_begin_image_data(container2);
	jxr_set_block_input(image2, Read565Data_DXT5);
	jxr_set_user_data(image2, &imageData);

	if (jxr_write_image_bitstream(image2, container2) != 0) {
		cerr << "JPEGXR encoding error!\n\n";
		return false;
	}

	jxr_destroy(image2);
	jxrc_write_container_post(container2);
#ifdef multithread
	WaitForSingleObject(bitThread, INFINITE);
	CloseHandle(bitThread);
#endif
	write_uint24(data->alpha_bufferLen, ofile);
	ofile.write((const char *)data->alpha_buffer, data->alpha_bufferLen);
	delete[]data->alpha_buffer;

	write_uint24(container->wb.len(), ofile);
	ofile.write((const char *)container->wb.buffer(), container->wb.len());

	write_uint24(data->bufferLen, ofile);
	ofile.write((const char *)data->buffer, data->bufferLen);
	delete[]data->buffer;

	write_uint24(container2->wb.len(), ofile);
	ofile.write((const char *)container2->wb.buffer(), container2->wb.len());

	jxr_destroy_container(container);
	jxr_destroy_container(container2);

	delete[]imageData.dxt5_alp;
	delete[]imageData.dxt5_abt;
	delete[]imageData.dxt5_col;
	delete[]imageData.dxt5_bit;

	HeapFree(GetProcessHeap(), 0, data);

	return true;
}

size_t write_raw_jxr(uint32_t w, uint32_t h, istream &ifile, ostream &ofile,
	bool hasAlpha) {

	ImageData imageData;
	imageData.raw = new uint8_t[w * h * (hasAlpha ? 4 : 3)];
	ifile.read(imageData.raw, w * h * (hasAlpha ? 4 : 3));

	jxr_container_t container = jxr_create_container();
	jxrc_start_file(container);

	if (hasAlpha)
		jxrc_set_pixel_format(container, JXRC_FMT_32bppBGRA);
	else
		jxrc_set_pixel_format(container, JXRC_FMT_24bppBGR);

	jxrc_set_image_shape(container, w, h);
	jxrc_set_separate_alpha_image_plane(container, 0);
	jxrc_set_image_band_presence(container, JXR_BP_ALL);

	static unsigned char window_params[5] = {0, 0, 0, 0, 0};
	jxr_image_t image = jxr_create_image(w, h, window_params);

	SetJPEGXRaw(container, image, gJxrQuality, hasAlpha, w, h);

	jxrc_begin_image_data(container);
	if (hasAlpha)
		jxr_set_block_input(image, Read8888Data);
	else
		jxr_set_block_input(image, Read888Data);

	jxr_set_user_data(image, &imageData);

	if (jxr_write_image_bitstream(image, container) != 0) {
		cerr << "JPEGXR encoding error!\n";
		return 0;
	}

	jxr_destroy(image);
	jxrc_write_container_post(container);
	size_t len = container->wb.len();
	write_uint24(len, ofile);
	ofile.write((const char *)container->wb.buffer(), container->wb.len());

	jxr_destroy_container(container);

	delete[]imageData.raw;

	return len;
}

EXPORT int convert(uint8_t *header, uint8_t *data, size_t datalen,
	AnsiString name) {

	ofstream ofile(name.c_str(), ios::out | ios::binary);
	if (!ofile.is_open()) {
		return 0;
	}

	DDS_header *dds = (DDS_header*)header;
	std::stringstream *ifile =
		new std::stringstream
		(std::ios_base::out | std::ios_base::in | std::ios_base::binary);

	ifile->write((char *)data, datalen);
	ifile->seekp(0);

	if (dds->dwMipMapCount < 1)
		dds->dwMipMapCount = 1;

	bool headerPatched = false;
	if (dds->sPixelFormat.dwRGBBitCount == 0 && dds->dwMipMapCount == 1 &&
		(dds->dwWidth > 1024 || dds->dwHeight > 1024)) {
		dds->dwMipMapCount = 2;
		headerPatched = true;
	} // заплатка

	bool hasAlpha = dds->sPixelFormat.dwRGBBitCount == 32;

	if (dds->sPixelFormat.dwFourCC) {
		write_header(dds->dwWidth, dds->dwHeight,
			(dds->sPixelFormat.dwFourCC == DXT5) ? 4 : 2,
			dds->dwMipMapCount, ofile);
	}
	else {
		write_header(dds->dwWidth, dds->dwHeight, (hasAlpha) ? 1 : 0,
			dds->dwMipMapCount, ofile);
	}

	if (headerPatched)
		dds->dwMipMapCount = 1; // заплатка

	uint32_t w = dds->dwWidth;
	uint32_t h = dds->dwHeight;

	size_t offset = 0;

	for (size_t c = 0; c < dds->dwMipMapCount; c++) {
		if (dds->sPixelFormat.dwFourCC == DXT5) {
			if (!write_dxt5(w, h, *ifile, ofile))
				break;
		}
		else if (dds->sPixelFormat.dwFourCC == DXT1) {
			if (!write_dxt1(w, h, *ifile, ofile))
				break;
		}
		else {
			if (!write_raw_jxr(w, h, *ifile, ofile, hasAlpha))
				break;
		}
		if (c + 1 < dds->dwMipMapCount && dds->sPixelFormat.dwFourCC != 0) {
			write_uint24(0, ofile);
			write_uint24(0, ofile);
			write_uint24(0, ofile);
			write_uint24(0, ofile);
			write_uint24(0, ofile);
			write_uint24(0, ofile);
		}
		w /= 2;
		h /= 2;
	}

	delete ifile;

	size_t filesize = ofile.tellp();
	filesize -= 6;
	ofile.seekp(3);

	write_uint24(filesize, ofile);

	ofile.seekp(0, std::ios_base::end);

	ofile.flush();
	ofile.close();

	return 0;
}
