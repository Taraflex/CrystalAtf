{ ************************************************************************** }
{ }
{ CrystalDXT is a small part of a large casual engine CrystalEngine, that is }
{ responsible for interaction with the dds files in DXT1/5 formats. }
{ }
{ CrystalDXT is partly based on the famous library "libsquish" (and its }
{ update in the NVIDIA Texture Tools). Favorable library difference is }
{ careful optimization, that gives a good performance boost, slightly better }
{ quality, smaller code size and maximum ease of use. In addition the library }
{ contains functionality for working with DXT data. }
{ }
{ Copyright: Dmitry Mozulyov (aka Devil) }
{ email: softforyou@inbox.ru }
{ icq: 250481638 }
{ ************************************************************************** }

unit CrystalDXT;



// compiler options
{$ifdef fpc}
{$mode delphi}
{$asmmode intel}
{$endif}
{$V+}{$B-}{$X+}{$T+}{$P+}{$H+}{$J-}{$MINENUMSIZE 1}
{$O+}{$R-}{$I-}{$Q-}
{$ifndef VER140}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$endif}

interface

uses {$IFDEF MSWINDOWS}
  Windows
{$ELSE}
{$MESSAGE ERROR 'You should add another libraries to use platform specific threads'}
{$ENDIF};

const
  DXT_BLOCK_SIZES: array [boolean] of integer = (16, 8);

type
  AtfFormat = (Auto, DXT1, DXT5, RGBA, RGB);

  TBGRA = packed record
    B, G, R, A: byte;
  end;

  TBGR = packed record
    B, G, R: byte;
  end;

  PBGRA = ^TBGRA;
  PBGR = ^TBGR;

  TBGRABlock = array [0 .. 15] of TBGRA;
  PBGRABlock = ^TBGRABlock;

  TDxt1Block = array [0 .. 7] of byte;
  PDxt1Block = ^TDxt1Block;

  TDxt5Block = array [0 .. 15] of byte;
  PDxt5Block = ^TDxt5Block;

  TDxtAlphaKind = (akAlpha, akTransparent, akNone);

  TMagicChars = array [0 .. 3] of ansichar;
  PMagicChars = ^TMagicChars;

  TDxtHeader = packed record
    dwMagic: TMagicChars; // DirectDrawSurface Magic = 'DDS '
    __Size: LongWord; // not important const = 124
    __Flags: LongWord; // not important texture flags
    dwHeight: LongWord; // height of image
    dwWidth: LongWord; // width of image
    dwLinearSize: LongWord; // texture size
    dwDepth: LongWord;
    dwNumMimps: LongWord;
    dwReserved1: array [0 .. 43] of byte;
    dwSize: LongWord;
    dwFlags: LongWord;
    dwFourCC: LongWord;
    dwRGBBitCount: LongWord;
    dwRBitMask: LongWord;
    dwGBitMask: LongWord;
    dwBBitMask: LongWord;
    dwAlphaBitMask: LongWord;
    dwCaps1: LongWord;
    dwCaps2: LongWord;
    dwCaps3: LongWord;
    dwCaps4: LongWord;
    dwReserved2: LongWord;
  end;

  PDxtHeader = ^TDxtHeader;

  TAtfHeader = array [0..10] of byte;
  PAtfHeader = ^TAtfHeader;

  TDxtCallback = procedure(const Handle: pointer; var BGRABlock: TBGRABlock;
    const X, Y: integer);

  // Header
function DxtHeader(const Width, Height: integer; const nummips: integer;
  Format: AtfFormat): TDxtHeader;

// Flip
procedure DxtFlip(const DxtBlocks: pointer; const DXT1: boolean;
  const BlocksCount: integer); overload;
procedure DxtFlip(var Dest: TDxt1Block); overload;
procedure DxtFlip(var Dest: TDxt5Block); overload;

// Decompressing
procedure DxtDecompress(const Dest: pointer; const DxtBlock: pointer;
  const DXT1: boolean; const line_size: integer = 16); overload;
procedure DxtDecompress(var Dest: TBGRABlock; const Src: TDxt1Block); overload;
procedure DxtDecompress(var Dest: TBGRABlock; const Src: TDxt5Block); overload;

// Compressing
procedure DxtCompress(const Dest: pointer; const BGRABlock: TBGRABlock;
  const DXT1: boolean); overload;
procedure DxtCompress(var Dest: TDxt1Block;
  const BGRABlock: TBGRABlock); overload;
procedure DxtCompress(var Dest: TDxt5Block;
  const BGRABlock: TBGRABlock); overload;

// Compressing, data mode
procedure DxtCompressData(const Dest: pointer; const BGRABlock: TBGRABlock;
  const DXT1: boolean); overload;
procedure DxtCompressData(var Dest: TDxt1Block;
  const BGRABlock: TBGRABlock); overload;
procedure DxtCompressData(var Dest: TDxt5Block;
  const BGRABlock: TBGRABlock); overload;

// Image
function DxtImageSize(const Format: AtfFormat;
  const ImageWidth, ImageHeight: integer): integer;
function DxtImageAlpha(const DxtBlocks: pointer; const DXT1: boolean;
  const ImageWidth, ImageHeight: integer): TDxtAlphaKind;
procedure DxtImageFlip(const DxtBlocks: pointer; const DXT1: boolean;
  const ImageWidth, ImageHeight: integer);
procedure DxtImageResize(const DxtBlocks: pointer; const DXT1: boolean;
  const ImageWidth, ImageHeight: integer; const DestDxtBlocks: pointer;
  const DestImageWidth, DestImageHeight: integer);
procedure DxtImageDecompress(const DxtBlocks: pointer; const DXT1: boolean;
  const ImageWidth, ImageHeight: integer; const Handle: pointer;
  const Callback: TDxtCallback);
procedure DxtImageCompress(const DxtBlocks: pointer; const DXT1: boolean;
  const ImageWidth, ImageHeight: integer; const Handle: pointer;
  const Callback: TDxtCallback; const DataMode: boolean = false);
procedure DxtImageCompressParallel(const DxtBlocks: pointer;
  const DXT1: boolean; const ImageWidth, ImageHeight: integer;
  const Handle: pointer; const Callback: TDxtCallback;
  const DataMode: boolean = false);

implementation

const
  DDS_DXT1 = $31545844;
  DDS_DXT3 = $33545844;
  DDS_DXT5 = $35545844;

const
  DDS_HEADER_FLAGS_TEXTURE = $00001007;
  // DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH | DDSD_PIXELFORMAT
  DDS_HEADER_FLAGS_MIPMAP = $00020000; // DDSD_MIPMAPCOUNT
  DDS_HEADER_FLAGS_VOLUME = $00800000; // DDSD_DEPTH
  DDS_HEADER_FLAGS_PITCH = $00000008; // DDSD_PITCH
  DDS_HEADER_FLAGS_LINEARSIZE = $00080000; // DDSD_LINEARSIZE

const
  DDS_FOURCC = $00000004;
  DDS_RGB = $00000040;
  DDS_RGBA = $00000041;

const
  DDS_SURFACE_FLAGS_TEXTURE = $00001000; // DDSCAPS_TEXTURE
  DDS_SURFACE_FLAGS_MIPMAP = $00400008; // DDSCAPS_COMPLEX | DDSCAPS_MIPMAP
  DDS_SURFACE_FLAGS_CUBEMAP = $00000008; // DDSCAPS_COMPLEX

const
  DDS_CUBEMAP_POSITIVEX = $00000600;
  // DDSCAPS2_CUBEMAP | DDSCAPS2_CUBEMAP_POSITIVEX
  DDS_CUBEMAP_NEGATIVEX = $00000A00;
  // DDSCAPS2_CUBEMAP | DDSCAPS2_CUBEMAP_NEGATIVEX
  DDS_CUBEMAP_POSITIVEY = $00001200;
  // DDSCAPS2_CUBEMAP | DDSCAPS2_CUBEMAP_POSITIVEY
  DDS_CUBEMAP_NEGATIVEY = $00002200;
  // DDSCAPS2_CUBEMAP | DDSCAPS2_CUBEMAP_NEGATIVEY
  DDS_CUBEMAP_POSITIVEZ = $00004200;
  // DDSCAPS2_CUBEMAP | DDSCAPS2_CUBEMAP_POSITIVEZ
  DDS_CUBEMAP_NEGATIVEZ = $00008200;
  // DDSCAPS2_CUBEMAP | DDSCAPS2_CUBEMAP_NEGATIVEZ

  DDS_CUBEMAP_ALLFACES = (DDS_CUBEMAP_POSITIVEX or DDS_CUBEMAP_NEGATIVEX or
    DDS_CUBEMAP_POSITIVEY or DDS_CUBEMAP_NEGATIVEY or DDS_CUBEMAP_POSITIVEZ or
    DDS_CUBEMAP_NEGATIVEZ);

function DxtHeader(const Width, Height: integer; const nummips: integer;
  Format: AtfFormat): TDxtHeader;
begin
  FillChar(Result, sizeof(Result), #0);

  Result.dwMagic := 'DDS ';
  Result.__Size := 124;
  if nummips > 1 then
  begin
    Result.__Flags := DDS_HEADER_FLAGS_TEXTURE or DDS_SURFACE_FLAGS_MIPMAP;
    Result.dwCaps1 := DDS_SURFACE_FLAGS_TEXTURE or DDS_SURFACE_FLAGS_MIPMAP;
    Result.dwNumMimps := nummips;
  end
  else
  begin
    Result.__Flags := DDS_HEADER_FLAGS_TEXTURE;
    Result.dwCaps1 := DDS_SURFACE_FLAGS_TEXTURE;
    Result.dwNumMimps := 0;
  end;

  if Format in [DXT1, DXT5] then
  begin
    Result.__Flags := Result.__Flags or DDS_HEADER_FLAGS_LINEARSIZE;
    Result.dwLinearSize := DxtImageSize(Format, Width, Height);
  end
  else
  begin
    Result.__Flags := Result.__Flags or DDS_HEADER_FLAGS_PITCH;
    if Format = RGB then
      Result.dwLinearSize := Width * 3
    else if Format = RGBA then
      Result.dwLinearSize := Width * 4;
  end;

  Result.dwWidth := Width;
  Result.dwHeight := Height;
  Result.dwDepth := 0;

  with Result do
  begin
    dwSize := 32;
    case Format of
      RGBA:
        begin
          dwFlags := DDS_RGBA;
          dwRGBBitCount := 32;
          dwFourCC := 0;
          dwRBitMask := $00FF0000;
          dwGBitMask := $0000FF00;
          dwBBitMask := $000000FF;
          dwAlphaBitMask := $FF000000;
        end;
      RGB:
        begin
          dwFlags := DDS_RGB;
          dwFourCC := 0;
          dwRGBBitCount := 24;
          dwRBitMask := $00FF0000;
          dwGBitMask := $0000FF00;
          dwBBitMask := $000000FF;
          dwAlphaBitMask := $00000000;
        end;
    else
      begin
        dwFlags := DDS_FOURCC;
        dwRGBBitCount := 0;
        dwRBitMask := 0;
        dwGBitMask := 0;
        dwBBitMask := 0;
        dwAlphaBitMask := 0;
        case Format of
          DXT1:
            dwFourCC := DDS_DXT1;
          DXT5:
            dwFourCC := DDS_DXT5;
        end;
      end;
    end;
  end;
end;

procedure DxtFlip(const DxtBlocks: pointer; const DXT1: boolean;
  const BlocksCount: integer);
asm
  DD $577EC985,$7401E283,$04508B0F,$5089CA0F,$08C08304,$C3F27549,$8B565753,$CA0F0C50,$8B0C5089,$508B0170
  DD $89F78904,$00E681D3,$81FFFFFF,$FFFF00E2,$0CEFC1FF,$C10CEBC1,$E2C10CE6,$01588A0C,$DA09FE09,$89047089
  DD $C0830150,$C3754910,$C35B5F5E
end;

procedure DxtFlip(var Dest: TDxt1Block);
asm
  mov edx, 1
  mov ecx, 1
  jmp DxtFlip
end;

procedure DxtFlip(var Dest: TDxt5Block);
asm
  mov edx, 1
  xor ecx, ecx
  jmp DxtFlip
end;

function DxtImageSize(const Format: AtfFormat;
  const ImageWidth, ImageHeight: integer): integer;
begin
  if Format = RGBA then
    Result := ImageWidth * ImageHeight * 4
  else if Format = RGB then
    Result := ImageWidth * ImageHeight * 3
  else
    Result := ((ImageWidth + 3) div 4) * ((ImageHeight + 3) div 4) *
      DXT_BLOCK_SIZES[(Format = DXT1)];
end;

function DxtImageAlpha(const DxtBlocks: pointer; const DXT1: boolean;
  const ImageWidth, ImageHeight: integer): TDxtAlphaKind;
asm
  pop ebp

  // fastcall -> stdcall
  sub esp, 12
  mov [esp+4], eax
  mov eax, [esp+12]
  mov [esp], eax
  mov [esp+ 8], edx
  mov [esp+12], ecx

  // 976 bytes dump
  DD $57E58955,$EC835356,$085D8B5C,$80104D8B,$74000C7D,$14558B77,$607ED285,$00D045C7,$85000000,$C7497EC9
  DD $0000D845,$458B0000,$03C083D0,$8BD44589,$8B660453,$433B6603,$8B217702,$C683D875,$D07D8B03,$39CC5D89
  DD $8F0F147D,$00000218,$4708EAC1,$7DD47D39,$CC5D8BEE,$8308C383,$3B04D845,$C77FD84D,$04D04583,$39D0558B
  DD $A77F1455,$000002B8,$5CC48300,$C95F5E5B,$900010C2,$8514458B,$C7E97EC0,$0000AC45,$45C60000,$C98500D4
  DD $02538E0F,$45C70000,$000000CC,$AC758B00,$8903C683,$3B81D075,$FFFF0500,$02C3840F,$038A0000,$8401538A
  DD $800975C0,$840F05FA,$000002E0,$88E84588,$D038E955,$023F860F,$B60F0000,$9C4589C0,$89D2B60F,$E0D19855
  DD $03D84589,$45899C45,$42348DC0,$492493BF,$F7F08992,$BC5589EF,$FEC1D601,$88F28902,$758BEA55,$89E6D198
  DD $458BB875,$02E0C19C,$8BB04589,$C6019C75,$89B87503,$89EFF7F0,$5589A045,$C1D601A4,$F28902FE,$8BEB5588
  DD $7503B875,$B8758998,$89B07503,$89EFF7F0,$D601B455,$8902FEC1,$EC5588F2,$C198758B,$758902E6,$C07503B0
  DD $EFF7F089,$89C04589,$D601C455,$8902FEC1,$ED5588F2,$0398558B,$458BB055,$02348DD8,$EFF7F089,$8BDC5589
  DD $148DDC45,$02FAC106,$8BEE5588,$458BB855,$50348D9C,$EFF7F089,$89D84589,$048DDC55,$02F8C116,$8BEF4588
  DD $D0890253,$0000000D,$00003DFF,$840FFF00,$000001C8,$83CC758B,$558B03C6,$D85589AC,$47EBDF89,$3D03E8C1
  DD $000000FF,$012A840F,$E8C10000,$00FF3D03,$840F0000,$000000AC,$3D03E8C1,$000000FF,$00B4840F,$E8C10000
  DD $00FF3D03,$840F0000,$000000BC,$8BD845FF,$5539D055,$C48F0FD8,$8B000000,$5539D855,$8BB17E14,$05EBCC55
  DD $7FF23942,$7ED139DF,$83C38913,$5C8A07E3,$DB84E81D,$45C61A75,$906601D4,$3D03E8C1,$000000FF,$478BDA75
  DD $00000D05,$D0EBFF00,$E874C3FE,$C483C031,$5F5E5B5C,$0010C2C9,$EBD8458B,$00768D0F,$4002EAC1,$8F0FF039
  DD $FFFFFDD7,$F07EC139,$E383D389,$03FB8303,$01B8E675,$83000000,$5E5B5CC4,$10C2C95F,$00768D00,$0D05478B
  DD $FF000000,$3D03E8C1,$000000FF,$FF4C850F,$478BFFFF,$00000D05,$E8C1FF00,$00FF3D03,$850F0000,$FFFFFF44
  DD $0D05478B,$FF000000,$8BD845FF,$5539D055,$3C8E0FD8,$89FFFFFF,$10C383FB,$04CC4583,$0FCC4D3B,$FFFDBD8F
  DD $AC4583FF,$AC758B04,$0F147539,$FFFD958F,$D47D80FF,$E9837500,$FFFFFD68,$0D05478B,$FF000000,$FFFEC9E9
  DD $F0B60FFF,$8DFAB60F,$67B8B714,$F7666666,$88FAD1EA,$148DEA55,$D855893F,$8936048D,$C289C045,$5503F201
  DD $6667B8D8,$EAF76666,$5588FAD1,$D8558BEB,$5503FA01,$6667B8C0,$EAF76666,$5588FAD1,$BE148DEC,$666667B8
  DD $D1EAF766,$ED5588FA,$00EE45C6,$FFEF45C6,$FFFE32E9,$047B83FF,$33850FFF,$E9FFFFFD,$FFFFFF55,$FFFFE281
  DD $850F00FF,$FFFFFE2C,$84E8458A,$FE2074C0,$3A840FC0,$E9FFFFFF,$FFFFFEB0,$00E845C7,$C7020105,$0403EC45
  DD $F0E9FF00,$C6FFFFFD,$E901D445,$FFFFFF19
end;

procedure DxtImageFlip(const DxtBlocks: pointer; const DXT1: boolean;
  const ImageWidth, ImageHeight: integer);
var
  i: integer;
  Buffer, P1, P2: pointer;
  BlockSize, WidthSize: integer;
  BlocksWidth, BlocksHeight: integer;
begin
  if (ImageWidth <= 0) or (ImageHeight <= 0) then
    exit;

  if (ImageHeight <= 4) then
  begin
    DxtFlip(DxtBlocks, DXT1, (ImageWidth + 3) shr 2);
    exit;
  end;

  BlockSize := DXT_BLOCK_SIZES[DXT1];
  BlocksWidth := (ImageWidth + 3) shr 2;
  BlocksHeight := (ImageHeight + 3) shr 2;
  WidthSize := BlocksWidth * BlockSize;

  GetMem(Buffer, WidthSize);
  try
    for i := 0 to (BlocksHeight shr 1) do
    begin
      P1 := pointer(integer(DxtBlocks) + i * WidthSize);
      P2 := pointer(integer(DxtBlocks) + (BlocksHeight - 1 - i) * WidthSize);

      if (integer(P1) < integer(P2)) then
      begin
        Move(P1^, Buffer^, WidthSize);
        Move(P2^, P1^, WidthSize);
        Move(Buffer^, P2^, WidthSize);

        DxtFlip(P1, DXT1, BlocksWidth);
        DxtFlip(P2, DXT1, BlocksWidth);
      end
      else
      begin
        if (P1 = P2) then
          DxtFlip(P1, DXT1, BlocksWidth);
      end;
    end;
  finally
    FreeMem(Buffer);
  end;
end;

procedure DxtImageResize(const DxtBlocks: pointer; const DXT1: boolean;
  const ImageWidth, ImageHeight: integer; const DestDxtBlocks: pointer;
  const DestImageWidth, DestImageHeight: integer);
var
  SrcBlock, DestBlock: pointer;
  BlockSize: integer;
  SrcWidth, SrcHeight, DestWidth, DestHeight: integer;
  WidthToCopy, HeightToCopy, Y: integer;

  procedure ZeroBlocks(const Count: integer);
  var
    i: integer;
  begin
    if (Count <= 0) then
      exit;

    if (DXT1) then
      for i := 0 to Count - 1 do
      begin
        plongword(DestBlock)^ := $00000000;
        plongword(integer(DestBlock) + 4)^ := $FFFFFFFF;

        inc(integer(DestBlock), 8);
      end
    else
      for i := 0 to Count - 1 do
      begin
        plongword(DestBlock)^ := $00000500;
        plongword(integer(DestBlock) + 4)^ := $00000000;
        plongword(integer(DestBlock) + 8)^ := $00000000;
        plongword(integer(DestBlock) + 12)^ := $00000000;

        inc(integer(DestBlock), 16);
      end;
  end;

  procedure CopyBlocks(const Count: integer);
  var
    Size: integer;
  begin
    if (Count <= 0) then
      exit;
    Size := Count * BlockSize;

    Move(SrcBlock^, DestBlock^, Size);
    inc(integer(SrcBlock), Size);
    inc(integer(DestBlock), Size);
  end;

begin
  SrcWidth := (ImageWidth + 3) div 4;
  SrcHeight := (ImageHeight + 3) div 4;
  DestWidth := (DestImageWidth + 3) div 4;
  DestHeight := (DestImageHeight + 3) div 4;
  if (DestWidth <= 0) or (DestHeight <= 0) or (SrcWidth < 0) or (SrcHeight < 0)
  then
    exit;

  SrcBlock := DxtBlocks;
  DestBlock := DestDxtBlocks;
  BlockSize := DXT_BLOCK_SIZES[DXT1];

  if (SrcWidth = 0) or (SrcHeight = 0) then
  begin
    ZeroBlocks(DestWidth * DestHeight);
    exit;
  end;

  WidthToCopy := DestWidth;
  if (WidthToCopy > SrcWidth) then
    WidthToCopy := SrcWidth;
  HeightToCopy := DestHeight;
  if (HeightToCopy > SrcHeight) then
    HeightToCopy := SrcHeight;
  for Y := 0 to HeightToCopy - 1 do
  begin
    CopyBlocks(WidthToCopy);

    if (DestWidth > SrcWidth) then
      ZeroBlocks(DestWidth - SrcWidth)
    else if (DestWidth < SrcWidth) then
      inc(integer(SrcBlock), (SrcWidth - DestWidth) * BlockSize);
  end;

  if (HeightToCopy < DestHeight) then
    ZeroBlocks((DestHeight - HeightToCopy) * DestWidth);
end;

procedure DxtImageDecompress(const DxtBlocks: pointer; const DXT1: boolean;
  const ImageWidth, ImageHeight: integer; const Handle: pointer;
  const Callback: TDxtCallback);
var
  X, Y: integer;
  Block: pointer;
  BlockSize: integer;
  BGRABlock: TBGRABlock;
begin
  Block := DxtBlocks;
  BlockSize := DXT_BLOCK_SIZES[DXT1];

  Y := 0;
  while (Y < ImageHeight) do
  begin
    X := 0;
    while (X < ImageWidth) do
    begin
      // decompress bgra block
      DxtDecompress(@BGRABlock, Block, DXT1);

      // fill bgra block in image
      Callback(Handle, BGRABlock, X, Y);

      // increment
      inc(integer(Block), BlockSize);
      inc(X, 4);
    end;

    inc(Y, 4);
  end;
end;

// internal function for image compression
// called from the normal function or multi-threaded
procedure DxtInternalImageCompress(const DxtBlocks: pointer;
  const Mask, ImageWidth, ImageHeight: integer; const Handle: pointer;
  const Callback: TDxtCallback);
type
  TRealDxtCompress = procedure(const Dest: pointer; const BGRABlock: TBGRABlock;
    const Mask: integer);
var
  X, Y: integer;
  Block: pointer;
  BlockSize: integer;
  BGRABlock: TBGRABlock;
begin
  Block := DxtBlocks;
  BlockSize := DXT_BLOCK_SIZES[Mask and 1 <> 0];

  Y := 0;
  while (Y < ImageHeight) do
  begin
    X := 0;
    while (X < ImageWidth) do
    begin
      // get bgra block from image
      Callback(Handle, BGRABlock, X, Y);

      // compress bgra block with dxt algorithm
      TRealDxtCompress(@DxtCompress)(Block, BGRABlock, Mask);

      // increment
      inc(integer(Block), BlockSize);
      inc(X, 4);
    end;

    inc(Y, 4);
  end;
end;

procedure DxtImageCompress(const DxtBlocks: pointer; const DXT1: boolean;
  const ImageWidth, ImageHeight: integer; const Handle: pointer;
  const Callback: TDxtCallback; const DataMode: boolean = false);
var
  Mask: integer;
begin
  Mask := ord(DXT1);
  if (DataMode) then
    Mask := Mask or 2;

  DxtInternalImageCompress(DxtBlocks, Mask, ImageWidth, ImageHeight, Handle,
    Callback);
end;

procedure DxtDecompress(var Dest: TBGRABlock; const Src: TDxt1Block);
asm
  mov ecx, [esp]
  mov dword ptr [esp], $00000010
  push ecx
  mov ecx, 1
  jmp DxtDecompress
end;

procedure DxtDecompress(var Dest: TBGRABlock; const Src: TDxt5Block);
asm
  mov ecx, [esp]
  mov dword ptr [esp], $00000010
  push ecx
  xor ecx, ecx
  jmp DxtDecompress
end;

procedure DxtDecompress(const Dest: pointer; const DxtBlock: pointer;
  const DXT1: boolean; const line_size: integer = 16);
const
  BITS_5 = int64((1 shl 5) - 1);
  BITS_6 = int64((1 shl 6) - 1);

  MASK_6: int64 = (BITS_6 shl 5) or (BITS_6 shl (32 + 5));
  MASK_5: int64 = (BITS_5 shl 9) or (BITS_5 shl 20) or (BITS_5 shl (32 + 9)) or
    (BITS_5 shl (32 + 20));
  MASK_FF: int64 = $FF000000FF000000;

  SHR_5_MUL: int64 = $4000020040000200;
  DIV3_MUL: int64 = $5556555655565556;
  ALPHA_DIV5: int64 = $3334333433343334;
  ALPHA_DIV7: int64 = $2493249324932493;
var
  // work "tables"
  alphas: array [0 .. 7] of byte;
  colors: array [0 .. 3] of TBGRA;
  asm
    not ecx
    push esi
    and ecx, 1
    push edi
    lea edx, [edx + ecx*8]
    push ebx
    mov dword ptr [alphas+0], ecx
    // *********


    // **************** COLORS TABLE INITIALIZATION ****************
    { | } // unpack 2 colors: 565|565 --> bgra|bgra
    { | } mov edi, [edx]
    { | } pxor mm7, mm7
    { | } movd mm0, edi
    { | } mov esi, [edx+4]
    { | } punpcklwd mm0, mm7
    { | } test edi, edi
    { | } movq mm1, mm0
    { | } jnz @non_zero_colors
    { | }    // if all the palette is black
    { | }    movq mm2, MASK_FF
    { | }    shl ecx, 31
    { | }    movq qword ptr [colors+0*4], mm2
    { | }    sar ecx, 31
    { | }    movq qword ptr [colors+2*4], mm2
    { | }    mov byte ptr [colors+4*4-1], cl
    { | } jmp @fill_color_table_end
    { | } @non_zero_colors:
    { | } psllq mm0, 9
    { | } pand mm1, MASK_6
    { | } pand mm0, MASK_5
    { | } psrlq mm1, 3
    { | } pmulhw mm0, SHR_5_MUL
    { | } movq mm3, mm1
    { | } movq mm2, mm0
    { | } psrld mm1, 6
    { | } paddw mm0, mm0
    { | } psrlw mm2, 4
    { | } por mm1, mm3
    { | } por mm0, MASK_FF
    { | } psllq mm1, 8
    { | } por mm0, mm2
    { | } por mm0, mm1
    { | } movq mm1, mm0
    { | } movq qword ptr [colors+0*4], mm0 // 2 base colors
    { | } movd edi, mm1
    { | } // split mm0 to mm0 and mm1, get another colors
    { | } punpcklbw mm0, mm7
    { | } test ecx, ecx
    { | } punpckhbw mm1, mm7
    { | } jnz @mode_4
    { | }    cmp edi, dword ptr [colors+1*4]
    { | }    ja @mode_4
    { | } @mode_3:
    { | }    paddw mm0, mm1
    { | }    movq mm2, mm7
    { | }    psrlw mm0, 1
    { | } jmp @mode_end
    { | } @mode_4:
    { | }    movq mm2, mm0
    { | }    movq mm4, DIV3_MUL
    { | }    paddw mm0, mm0
    { | }    movq mm3, mm1
    { | }    paddw mm0, mm3
    { | }    paddw mm1, mm1
    { | }    pmulhw mm0, mm4
    { | }    paddw mm2, mm1
    { | }    pmulhw mm2, mm4
    { | } @mode_end:
    { | } packuswb mm0, mm2
    { | } movq qword ptr [colors+2*4], mm0 // 2 "interpolated" colors
    { | } @fill_color_table_end:
    // <<<<----- initialization of table Colors


    // **************** COLOR FILLING FROM TABLE *****************
    { | } cmp esi, $FFFFFFFF
    { | }   je @fill_colors_same
    { | }   cmp esi, $AAAAAAAA
    { | }   jne @fill_colors_std
    { | } @fill_colors_same:
    { | }   and esi, 3
    { | }   mov edi, line_size
    { | }   movd mm0, dword ptr [colors + 4*esi]
    { | }   punpckldq mm0, mm0
    { | }   movq [eax+0], mm0
    { | }   movq [eax+8], mm0
    { | }   movq [eax+0+edi], mm0
    { | }   movq [eax+8+edi], mm0
    { | }   movq [eax+0+edi*2], mm0
    { | }   movq [eax+8+edi*2], mm0
    { | }   add eax, edi
    { | }   movq [eax+0+edi*2], mm0
    { | }   movq [eax+8+edi*2], mm0
    { | }   sub eax, edi
    { | } jmp @fill_colors_done
    { | } @fill_colors_std:
    { | } mov dword ptr [alphas+4], eax
    { | } mov ecx, 4
    { | } @loop:
    { | }   mov ebx, esi
    { | }   mov edi, esi
    { | }   and ebx, $3
    { | }   and edi, $c
    { | }   mov ebx, dword ptr [colors+ebx*4]
    { | }   mov edi, dword ptr [colors+edi]
    { | }   shr esi, 4
    { | }   mov [eax+0], ebx
    { | }   mov [eax+4], edi
    { | }   mov ebx, esi
    { | }   mov edi, esi
    { | }   and ebx, $3
    { | }   and edi, $c
    { | }   mov ebx, dword ptr [colors+ebx*4]
    { | }   mov edi, dword ptr [colors+edi]
    { | }   mov [eax+8], ebx
    { | }   mov [eax+$c], edi
    { | }   shr esi, 4
    { | }   add eax, line_size
    { | } dec ecx
    { | } jnz @loop
    { | } mov eax, dword ptr [alphas+4]
    { | } @fill_colors_done:
    // <<<<----- color filling from table


    // have it alpha channel (isDxt5)
    cmp dword ptr [alphas+0], 0
    je @exit

    // **************** ALPHAS TABLE INITIALIZATION ****************
    { | } mov ecx, [edx-8]
    { | } mov ebx, ecx // to compare with $500
    { | } mov esi, ecx // low (alpha0)
    { | } and ebx, $FFFF
    { | } jnz @non_zero_alpas
    { | }    // if zero palette
    { | }    movq [alphas], mm7
    { | }    jmp @fill_alpha_table_end
    { | } @non_zero_alpas:
    { | } mov edi, ecx // high (alpha1)
    { | } cmp ebx, $500
    { | } jne @fill_alpha_table
    { | }    // fill consts table 0,5,1,2,3,4,0,255
    { | }    mov dword ptr [alphas + 0], $02010500
    { | }    mov dword ptr [alphas + 4], $FF000403
    { | } jmp @fill_alpha_table_end
    { | } @fill_alpha_table:
    { | } shr edi, 8
    { | } and esi, $FF
    { | } and edi, $FF
    { | } mov dword ptr [alphas], ecx // 2 base palette elements
    { | } cmp esi, edi
    { | } ja @alpha_mode_7
    { | } @alpha_mode_5:
    { | }    movq mm0, ALPHA_DIV5
    { | }    lea ebx, [4*esi+edi]
    { | }    mov dword ptr [alphas + 6-2], $FF000000 // 2 last palette elements
    { | }    sub edi, esi
    { | } jmp @alpha_mode_end
    { | } @alpha_mode_7:
    { | }    lea ebx, [edi + edi*2]
    { | }    movq mm0, ALPHA_DIV7
    { | }    lea ecx, [esi*2 + ebx]
    { | }    lea ebx, [esi + ebx*2]
    { | }    lea ecx, [ecx + edi*2]
    { | }    shl ebx, 16
    { | }    or ecx, ebx
    { | }    movd mm1, ecx
    { | }    pmulhw mm1, mm0
    { | }    packuswb mm1, mm7
    { | }    movd ecx, mm1
    { | }    lea ebx, [esi*2 + esi]
    { | }    shl ecx, 16
    { | }    lea ebx, [ebx*2 + edi]
    { | }    mov dword ptr [alphas + 6-2], ecx
    { | }    sub edi, esi
    { | } @alpha_mode_end:
    { | } // ebx = base
    { | } // edi = addition (alpha1-alpha0)
    { | } lea ecx, [ebx+edi*2]
    { | } lea esi, [ecx+edi]
    { | } add edi, ebx
    { | } shl esi, 16
    { | } or esi, ecx
    { | } shl edi, 16
    { | } movd mm1, esi
    { | } or edi, ebx
    { | } psllq  mm1, 32
    { | } movd mm2, edi
    { | } por mm1, mm2
    { | } pmulhw mm0, mm1
    { | } packuswb mm0, mm7
    { | } movd dword ptr [alphas + 2], mm0
    { | } @fill_alpha_table_end:
    // <<<<------- alphas table initialization


    // **************** ALPHAS FILLING (Dxt5) ****************
    { | } mov EDI, line_size
    { | } dec eax // add eax, 3-4
    { | } mov line_size, 2 // set it counter
    { | } mov ESI, [edx-6]
    { | } @fill_alpha_loop:
    { | } and ESI, $00FFFFFF
    { | } jz  @fill_alpha_same
    { | } cmp ESI, $00FFFFFF
    { | } jne @fill_alpha_std
    { | } @fill_alpha_same:
    { | }    // fill one value
    { | }    and ESI, 7
    { | }    mov ecx, eax
    { | }    movzx ebx, byte ptr [alphas+ESI]
    { | }    lea eax, [eax + edi*2]
    { | }    cmp ebx, 255
    { | }    je @fill_alpha_continue // if 255 - do nothing
    { | }    mov eax, ecx
    { | }    mov [eax + 4+0], bl
    { | }    mov [eax + 4+4], bl
    { | }    mov [eax + 4+8], bl
    { | }    mov [eax + 4+12], bl
    { | }    add eax, edi
    { | }    mov [eax + 4+0], bl
    { | }    mov [eax + 4+4], bl
    { | }    mov [eax + 4+8], bl
    { | }    mov [eax + 4+12], bl
    { | }    add eax, edi
    { | } jmp @fill_alpha_continue
    { | } @fill_alpha_std:
    { | }    // standard filling
    { | }    mov ecx, ESI
    { | }    shr ESI, 3
    { | }    and ecx, 7
    { | }    add eax, 4
    { | }    mov ebx, dword ptr [alphas + ecx]
    { | }    mov ecx, ESI
    { | }    mov [eax], bl // 0
    { | }    shr ESI, 3
    { | }    and ecx, 7
    { | }    add eax, 4
    { | }    mov ebx, dword ptr [alphas + ecx]
    { | }    mov ecx, ESI
    { | }    mov [eax], bl // 1
    { | }    shr ESI, 3
    { | }    and ecx, 7
    { | }    add eax, 4
    { | }    mov ebx, dword ptr [alphas + ecx]
    { | }    mov ecx, ESI
    { | }    mov [eax], bl // 2
    { | }    shr ESI, 3
    { | }    and ecx, 7
    { | }    add eax, 4
    { | }    mov ebx, dword ptr [alphas + ecx]
    { | }    mov ecx, ESI
    { | }    mov [eax], bl // 3
    { | }    shr ESI, 3
    { | }    and ecx, 7
    { | }    lea eax, [eax+edi-12]
    { | }    mov ebx, dword ptr [alphas + ecx]
    { | }    mov ecx, ESI
    { | }    mov [eax], bl // 4
    { | }    shr ESI, 3
    { | }    and ecx, 7
    { | }    add eax, 4
    { | }    mov ebx, dword ptr [alphas + ecx]
    { | }    mov ecx, ESI
    { | }    mov [eax], bl // 5
    { | }    shr ESI, 3
    { | }    and ecx, 7
    { | }    add eax, 4
    { | }    mov ebx, dword ptr [alphas + ecx]
    { | }    mov ecx, ESI
    { | }    mov [eax], bl // 6
    { | }    shr ESI, 3
    { | }    and ecx, 7
    { | }    add eax, 4
    { | }    mov ebx, dword ptr [alphas + ecx]
    { | }    mov [eax], bl // 7
    { | }    lea eax, [eax+edi-16]
    { | } @fill_alpha_continue:
    { | } mov ESI, [edx-3]
    { | } dec line_size // ñ÷¸ò÷èê
    { | } jnz @fill_alpha_loop
    // <<<<------- alphas filling (Dxt5)

    // *********
  @exit:
    emms
    pop ebx
    pop edi
    pop esi
end;

type
  TDxtCallbackColorsCompress = procedure(const Dest, ColourSet,
    DxtConstsData: pointer); cdecl;
  TDxtCallbackAlphaCompress = procedure(const Dest, BGRA,
    DxtConstsData: pointer); cdecl;

  TDxtConstsData = record
    (* <callbacks & single_color_table> *)
    ColorsCompress: TDxtCallbackColorsCompress;
    AlphaCompress: TDxtCallbackAlphaCompress;
    MT_ColorsCompress: TDxtCallbackColorsCompress;
    MT_AlphaCompress: TDxtCallbackAlphaCompress;
    ColorCalculationDriver: pointer;
    ColorTable: pointer;
    (* <consts...> *)
  end;

  PDxtConstsData = ^TDxtConstsData;

const
  DXT_CONSTS_SIZE = 320;

const
  DXT_CONSTS_DATA: array [0 .. 336 - 1] of byte = ($00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $10, $10, $10, $10, $10, $10, $70, $3F, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $80,
    $3F, $00, $00, $80, $3F, $00, $00, $80, $3F, $00, $00, $80, $3F, $FF, $FF,
    $7F, $7F, $FF, $FF, $7F, $7F, $FF, $FF, $7F, $7F, $FF, $FF, $7F, $7F, $00,
    $00, $00, $3F, $00, $00, $00, $3F, $00, $00, $00, $3F, $00, $00, $80, $3E,
    $AB, $AA, $AA, $3E, $AB, $AA, $AA, $3E, $AB, $AA, $AA, $3E, $39, $8E, $E3,
    $3D, $AB, $AA, $2A, $3F, $AB, $AA, $2A, $3F, $AB, $AA, $2A, $3F, $39, $8E,
    $E3, $3E, $39, $8E, $63, $3E, $39, $8E, $63, $3E, $39, $8E, $63, $3E, $39,
    $8E, $63, $3E, $00, $00, $F8, $41, $00, $00, $7C, $42, $00, $00, $F8, $41,
    $00, $00, $00, $00, $08, $21, $04, $3D, $21, $08, $82, $3C, $08, $21, $04,
    $3D, $00, $00, $00, $00, $00, $00, $40, $4B, $00, $00, $40, $4B, $00, $00,
    $40, $4B, $00, $00, $00, $00, $00, $40, $00, $00, $00, $40, $00, $00, $00,
    $00, $05, $00, $04, $00, $03, $00, $00, $00, $00, $00, $01, $00, $02, $00,
    $02, $00, $01, $00, $00, $00, $00, $00, $03, $00, $04, $00, $05, $00, $00,
    $00, $00, $00, $01, $00, $02, $00, $03, $00, $04, $00, $05, $00, $06, $00,
    $07, $00, $07, $00, $06, $00, $05, $00, $04, $00, $03, $00, $02, $00, $01,
    $00, $00, $00, $00, $06, $00, $00, $00, $02, $00, $03, $00, $04, $00, $05,
    $00, $01, $00, $07, $00, $01, $00, $07, $00, $06, $00, $05, $00, $04, $00,
    $03, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $34, $33,
    $34, $33, $34, $33, $34, $33, $93, $24, $93, $24, $93, $24, $93, $24, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00);

const
  DXT_SINGLECOLORS: array [0 .. 2048 - 1] of byte = ($00, $00, $00, $00, $00,
    $01, $00, $01, $00, $02, $00, $02, $00, $03, $00, $03, $00, $04, $00, $04,
    $00, $05, $00, $05, $00, $06, $00, $06, $00, $07, $00, $07, $00, $08, $00,
    $08, $00, $09, $00, $09, $00, $0A, $00, $0A, $00, $0B, $00, $0B, $00, $0C,
    $00, $0C, $00, $0D, $00, $0D, $00, $0E, $00, $0E, $00, $0F, $00, $0F, $00,
    $10, $00, $10, $00, $11, $00, $11, $00, $12, $00, $12, $00, $13, $00, $13,
    $00, $14, $00, $14, $00, $15, $00, $15, $00, $16, $00, $16, $00, $17, $00,
    $17, $00, $18, $00, $18, $00, $19, $00, $19, $00, $1A, $00, $1A, $00, $1B,
    $00, $1B, $00, $1C, $00, $1C, $00, $1D, $00, $1D, $00, $1E, $00, $1E, $00,
    $1F, $00, $1F, $01, $1F, $00, $20, $02, $1F, $00, $21, $03, $1F, $00, $22,
    $04, $1F, $00, $23, $05, $1F, $00, $24, $06, $1F, $00, $25, $07, $1F, $00,
    $26, $08, $1F, $00, $27, $09, $1F, $00, $28, $0A, $1F, $00, $29, $0B, $1F,
    $00, $2A, $0C, $1F, $00, $2B, $0D, $1F, $00, $2C, $0E, $1F, $00, $2D, $0F,
    $1F, $00, $2E, $00, $2E, $00, $2F, $00, $2F, $00, $30, $00, $30, $00, $31,
    $00, $31, $00, $32, $00, $32, $00, $33, $00, $33, $00, $34, $00, $34, $00,
    $35, $00, $35, $00, $36, $00, $36, $00, $37, $00, $37, $00, $38, $00, $38,
    $00, $39, $00, $39, $00, $3A, $00, $3A, $00, $3B, $00, $3B, $00, $3C, $00,
    $3C, $00, $3D, $00, $3D, $00, $3E, $00, $3E, $00, $3F, $00, $3F, $01, $3F,
    $10, $30, $02, $3F, $10, $31, $03, $3F, $10, $32, $04, $3F, $10, $33, $05,
    $3F, $10, $34, $06, $3F, $10, $35, $07, $3F, $10, $36, $08, $3F, $10, $37,
    $09, $3F, $10, $38, $0A, $3F, $10, $39, $0B, $3F, $10, $3A, $0C, $3F, $10,
    $3B, $0D, $3F, $10, $3C, $0E, $3F, $10, $3D, $0F, $3F, $10, $3E, $10, $3E,
    $10, $3F, $10, $3F, $11, $3F, $11, $3F, $12, $3F, $12, $3F, $13, $3F, $13,
    $3F, $14, $3F, $14, $3F, $15, $3F, $15, $3F, $16, $3F, $16, $3F, $17, $3F,
    $17, $3F, $18, $3F, $18, $3F, $19, $3F, $19, $3F, $1A, $3F, $1A, $3F, $1B,
    $3F, $1B, $3F, $1C, $3F, $1C, $3F, $1D, $3F, $1D, $3F, $1E, $3F, $1E, $3F,
    $1F, $3F, $1F, $3F, $20, $3F, $20, $3F, $21, $3F, $30, $30, $22, $3F, $30,
    $31, $23, $3F, $30, $32, $24, $3F, $30, $33, $25, $3F, $30, $34, $26, $3F,
    $30, $35, $27, $3F, $30, $36, $28, $3F, $30, $37, $29, $3F, $30, $38, $2A,
    $3F, $30, $39, $2B, $3F, $30, $3A, $2C, $3F, $30, $3B, $2D, $3F, $30, $3C,
    $2E, $3F, $30, $3D, $2F, $3F, $30, $3E, $30, $3E, $30, $3F, $30, $3F, $31,
    $3F, $31, $3F, $32, $3F, $32, $3F, $33, $3F, $33, $3F, $34, $3F, $34, $3F,
    $35, $3F, $35, $3F, $36, $3F, $36, $3F, $37, $3F, $37, $3F, $38, $3F, $38,
    $3F, $39, $3F, $39, $3F, $3A, $3F, $3A, $3F, $3B, $3F, $3B, $3F, $3C, $3F,
    $3C, $3F, $3D, $3F, $3D, $3F, $3E, $3F, $3E, $3F, $3F, $3F, $00, $00, $00,
    $00, $00, $00, $00, $01, $00, $01, $00, $01, $00, $01, $00, $02, $00, $02,
    $00, $02, $00, $02, $00, $03, $00, $03, $00, $03, $00, $03, $00, $04, $00,
    $04, $00, $04, $00, $04, $00, $05, $00, $05, $00, $05, $00, $05, $00, $06,
    $00, $06, $00, $06, $00, $06, $00, $07, $00, $07, $00, $07, $00, $07, $01,
    $07, $01, $07, $00, $08, $00, $08, $02, $07, $02, $07, $00, $09, $00, $09,
    $03, $07, $03, $07, $00, $0A, $00, $0A, $00, $0A, $00, $0B, $00, $0B, $00,
    $0B, $00, $0B, $00, $0C, $00, $0C, $00, $0C, $00, $0C, $00, $0D, $00, $0D,
    $00, $0D, $00, $0D, $00, $0E, $00, $0E, $00, $0E, $00, $0E, $00, $0F, $00,
    $0F, $00, $0F, $00, $0F, $01, $0F, $01, $0F, $00, $10, $00, $10, $02, $0F,
    $02, $0F, $00, $11, $00, $11, $03, $0F, $03, $0F, $00, $12, $00, $12, $00,
    $12, $00, $13, $00, $13, $00, $13, $00, $13, $00, $14, $00, $14, $00, $14,
    $00, $14, $00, $15, $00, $15, $00, $15, $00, $15, $00, $16, $00, $16, $00,
    $16, $00, $16, $00, $17, $00, $17, $00, $17, $00, $17, $01, $17, $01, $17,
    $00, $18, $00, $18, $02, $17, $02, $17, $00, $19, $00, $19, $03, $17, $03,
    $17, $00, $1A, $00, $1A, $00, $1A, $00, $1B, $00, $1B, $00, $1B, $00, $1B,
    $00, $1C, $00, $1C, $00, $1C, $00, $1C, $00, $1D, $00, $1D, $00, $1D, $00,
    $1D, $00, $1E, $00, $1E, $00, $1E, $00, $1E, $00, $1F, $00, $1F, $00, $1F,
    $00, $1F, $01, $1F, $01, $1F, $04, $1C, $04, $1C, $02, $1F, $02, $1F, $04,
    $1D, $04, $1D, $03, $1F, $03, $1F, $04, $1E, $04, $1E, $04, $1E, $04, $1F,
    $04, $1F, $04, $1F, $04, $1F, $05, $1F, $05, $1F, $05, $1F, $05, $1F, $06,
    $1F, $06, $1F, $06, $1F, $06, $1F, $07, $1F, $07, $1F, $07, $1F, $07, $1F,
    $08, $1F, $08, $1F, $08, $1F, $08, $1F, $09, $1F, $09, $1F, $0C, $1C, $0C,
    $1C, $0A, $1F, $0A, $1F, $0C, $1D, $0C, $1D, $0B, $1F, $0B, $1F, $0C, $1E,
    $0C, $1E, $0C, $1E, $0C, $1F, $0C, $1F, $0C, $1F, $0C, $1F, $0D, $1F, $0D,
    $1F, $0D, $1F, $0D, $1F, $0E, $1F, $0E, $1F, $0E, $1F, $0E, $1F, $0F, $1F,
    $0F, $1F, $0F, $1F, $0F, $1F, $10, $1F, $10, $1F, $10, $1F, $10, $1F, $11,
    $1F, $11, $1F, $14, $1C, $14, $1C, $12, $1F, $12, $1F, $14, $1D, $14, $1D,
    $13, $1F, $13, $1F, $14, $1E, $14, $1E, $14, $1E, $14, $1F, $14, $1F, $14,
    $1F, $14, $1F, $15, $1F, $15, $1F, $15, $1F, $15, $1F, $16, $1F, $16, $1F,
    $16, $1F, $16, $1F, $17, $1F, $17, $1F, $17, $1F, $17, $1F, $18, $1F, $18,
    $1F, $18, $1F, $18, $1F, $19, $1F, $19, $1F, $1C, $1C, $1C, $1C, $1A, $1F,
    $1A, $1F, $1C, $1D, $1C, $1D, $1B, $1F, $1B, $1F, $1C, $1E, $1C, $1E, $1C,
    $1E, $1C, $1F, $1C, $1F, $1C, $1F, $1C, $1F, $1D, $1F, $1D, $1F, $1D, $1F,
    $1D, $1F, $1E, $1F, $1E, $1F, $1E, $1F, $1E, $1F, $1F, $1F, $1F, $1F, $00,
    $00, $00, $01, $01, $00, $01, $00, $01, $01, $02, $00, $02, $01, $03, $00,
    $03, $00, $03, $01, $04, $00, $04, $00, $04, $01, $05, $00, $05, $01, $06,
    $00, $06, $00, $06, $01, $07, $00, $07, $00, $07, $01, $08, $00, $08, $01,
    $00, $11, $09, $00, $09, $01, $0A, $00, $02, $10, $0A, $01, $0B, $00, $0B,
    $01, $03, $11, $0C, $00, $0C, $01, $0D, $00, $05, $10, $0D, $01, $0E, $00,
    $0E, $01, $06, $11, $0F, $00, $0F, $01, $0F, $02, $10, $00, $10, $01, $0F,
    $04, $11, $00, $11, $01, $12, $00, $0F, $07, $12, $01, $13, $00, $13, $01,
    $0F, $0A, $14, $00, $14, $01, $15, $00, $0F, $0D, $15, $01, $16, $00, $16,
    $01, $0F, $10, $17, $00, $17, $01, $18, $00, $10, $10, $18, $01, $19, $00,
    $19, $01, $11, $11, $1A, $00, $1A, $01, $1B, $00, $13, $10, $1B, $01, $1C,
    $00, $1C, $01, $14, $11, $1D, $00, $1D, $01, $1E, $00, $16, $10, $1E, $01,
    $1F, $00, $1F, $01, $17, $11, $20, $00, $1F, $03, $20, $01, $21, $00, $21,
    $01, $1F, $06, $22, $00, $22, $01, $23, $00, $1F, $09, $23, $01, $24, $00,
    $24, $01, $1F, $0C, $25, $00, $25, $01, $26, $00, $1F, $0F, $26, $01, $27,
    $00, $27, $01, $1F, $12, $28, $00, $28, $01, $29, $00, $21, $10, $29, $01,
    $2A, $00, $2A, $01, $22, $11, $2B, $00, $2B, $01, $2C, $00, $24, $10, $2C,
    $01, $2D, $00, $2D, $01, $25, $11, $2E, $00, $2E, $01, $2F, $00, $27, $10,
    $2F, $01, $2F, $02, $30, $00, $30, $01, $31, $00, $2F, $05, $31, $01, $32,
    $00, $32, $01, $2F, $08, $33, $00, $33, $01, $34, $00, $2F, $0B, $34, $01,
    $35, $00, $35, $01, $2F, $0E, $36, $00, $36, $01, $37, $00, $2F, $11, $37,
    $01, $38, $00, $38, $01, $30, $11, $39, $00, $39, $01, $3A, $00, $32, $10,
    $3A, $01, $3B, $00, $3B, $01, $33, $11, $3C, $00, $3C, $01, $3D, $00, $35,
    $10, $3D, $01, $3E, $00, $3E, $01, $36, $11, $3F, $00, $3F, $01, $3F, $02,
    $38, $10, $3F, $03, $3F, $04, $3F, $05, $39, $11, $3F, $06, $3F, $07, $3F,
    $08, $3B, $10, $3F, $09, $3F, $0A, $3F, $0B, $3C, $11, $3F, $0C, $3F, $0D,
    $3F, $0E, $3E, $10, $3F, $0F, $3F, $10, $37, $20, $3F, $11, $3F, $12, $3F,
    $13, $38, $21, $3F, $14, $3F, $15, $3F, $16, $3A, $20, $3F, $17, $3F, $18,
    $3F, $19, $3B, $21, $3F, $1A, $3F, $1B, $3F, $1C, $3D, $20, $3F, $1D, $3F,
    $1E, $3F, $1F, $3E, $21, $3F, $20, $3F, $21, $37, $31, $3F, $22, $3F, $23,
    $3F, $24, $39, $30, $3F, $25, $3F, $26, $3F, $27, $3A, $31, $3F, $28, $3F,
    $29, $3F, $2A, $3C, $30, $3F, $2B, $3F, $2C, $3F, $2D, $3D, $31, $3F, $2E,
    $3F, $2F, $3F, $2F, $3F, $30, $3F, $31, $3F, $32, $3F, $32, $3F, $33, $3F,
    $34, $3F, $35, $3F, $35, $3F, $36, $3F, $37, $3F, $38, $3F, $38, $3F, $39,
    $3F, $3A, $3F, $3B, $3F, $3B, $3F, $3C, $3F, $3D, $3F, $3E, $3F, $3E, $3F,
    $3F, $00, $00, $00, $00, $00, $01, $00, $01, $01, $00, $01, $00, $01, $00,
    $01, $01, $01, $01, $02, $00, $02, $00, $00, $04, $02, $01, $02, $01, $02,
    $01, $03, $00, $03, $00, $03, $00, $03, $01, $01, $05, $03, $02, $03, $02,
    $04, $00, $04, $00, $04, $01, $04, $01, $05, $00, $05, $00, $05, $00, $03,
    $05, $05, $01, $06, $00, $06, $00, $04, $04, $06, $01, $06, $01, $06, $01,
    $07, $00, $07, $00, $07, $00, $07, $01, $05, $05, $07, $02, $07, $02, $08,
    $00, $08, $00, $08, $01, $08, $01, $09, $00, $09, $00, $09, $00, $07, $05,
    $09, $01, $0A, $00, $0A, $00, $08, $04, $0A, $01, $0A, $01, $0A, $01, $0B,
    $00, $0B, $00, $0B, $00, $0B, $01, $09, $05, $0B, $02, $0B, $02, $0C, $00,
    $0C, $00, $0C, $01, $0C, $01, $0D, $00, $0D, $00, $0D, $00, $0B, $05, $0D,
    $01, $0E, $00, $0E, $00, $0C, $04, $0E, $01, $0E, $01, $0E, $01, $0F, $00,
    $0F, $00, $0F, $00, $0F, $01, $0D, $05, $0F, $02, $0F, $02, $10, $00, $10,
    $00, $10, $01, $10, $01, $11, $00, $11, $00, $11, $00, $0F, $05, $11, $01,
    $12, $00, $12, $00, $10, $04, $12, $01, $12, $01, $12, $01, $13, $00, $13,
    $00, $13, $00, $13, $01, $11, $05, $13, $02, $13, $02, $14, $00, $14, $00,
    $14, $01, $14, $01, $15, $00, $15, $00, $15, $00, $13, $05, $15, $01, $16,
    $00, $16, $00, $14, $04, $16, $01, $16, $01, $16, $01, $17, $00, $17, $00,
    $17, $00, $17, $01, $15, $05, $17, $02, $17, $02, $18, $00, $18, $00, $18,
    $01, $18, $01, $19, $00, $19, $00, $19, $00, $17, $05, $19, $01, $1A, $00,
    $1A, $00, $18, $04, $1A, $01, $1A, $01, $1A, $01, $1B, $00, $1B, $00, $1B,
    $00, $1B, $01, $19, $05, $1B, $02, $1B, $02, $1C, $00, $1C, $00, $1C, $01,
    $1C, $01, $1D, $00, $1D, $00, $1D, $00, $1B, $05, $1D, $01, $1E, $00, $1E,
    $00, $1C, $04, $1E, $01, $1E, $01, $1E, $01, $1F, $00, $1F, $00, $1F, $00,
    $1F, $01, $1D, $05, $1F, $02, $1F, $02, $1C, $08, $1F, $03, $1F, $03, $1F,
    $03, $1F, $04, $1F, $04, $1F, $04, $1F, $05, $1D, $09, $1F, $06, $1F, $06,
    $1C, $0C, $1F, $07, $1F, $07, $1F, $07, $1F, $08, $1F, $08, $1F, $08, $1F,
    $09, $1D, $0D, $1F, $0A, $1F, $0A, $1C, $10, $1F, $0B, $1F, $0B, $1F, $0B,
    $1F, $0C, $1F, $0C, $1F, $0C, $1F, $0D, $1D, $11, $1F, $0E, $1F, $0E, $1C,
    $14, $1F, $0F, $1F, $0F, $1F, $0F, $1F, $10, $1F, $10, $1F, $10, $1F, $11,
    $1D, $15, $1F, $12, $1F, $12, $1C, $18, $1F, $13, $1F, $13, $1F, $13, $1F,
    $14, $1F, $14, $1F, $14, $1F, $15, $1D, $19, $1F, $16, $1F, $16, $1C, $1C,
    $1F, $17, $1F, $17, $1F, $17, $1F, $18, $1F, $18, $1F, $18, $1F, $19, $1D,
    $1D, $1F, $1A, $1F, $1A, $1F, $1A, $1F, $1B, $1F, $1B, $1F, $1B, $1F, $1C,
    $1F, $1C, $1F, $1C, $1F, $1D, $1F, $1D, $1F, $1E, $1F, $1E, $1F, $1E, $1F,
    $1F, $1F, $1F);

var
  DxtConstsData: PDxtConstsData; { setted in initialization section }
  SSE: boolean = false;
  CPU_COUNT: integer = 1;

procedure DXT_ColorsCompress; forward;
procedure DXT_CompressSSE; forward;
procedure DXT_CompressFPU; forward;
procedure DXT_AlphaCompress; forward;
procedure DXT_MT_ColorsCompress(const Dest, ColourSet, DxtConstsData: pointer);
  cdecl; forward;
procedure DXT_MT_AlphaCompress(const Dest, BGRA, DxtConstsData: pointer);
  cdecl; forward;

procedure DxtCompress(const Dest: pointer; const BGRABlock: TBGRABlock;
  const DXT1: boolean { integer } );
asm
  // fastcall --> stdcall
  // + callbacks initialization

  push ecx
  push edx
  push eax
  push dword ptr [esp+12] // ret

  mov eax, DxtConstsData
  mov [esp+16], eax

  // callbacks and tables
  (* TDxtConstsData = record
  ColorsCompress: pointer;
  AlphaCompress: pointer;
  MT_ColorsCompress: pointer;
  MT_AlphaCompress: pointer;
  ColorCalculationDriver: pointer;
  ColorTable: pointer;
  ...
  end; *)

  cmp [EAX].TDxtConstsData.ColorTable, 0
  jne @run
  mov [EAX].TDxtConstsData.ColorsCompress, OFFSET DXT_ColorsCompress
  mov [EAX].TDxtConstsData.AlphaCompress, OFFSET DXT_AlphaCompress
  mov [EAX].TDxtConstsData.MT_ColorsCompress, OFFSET DXT_MT_ColorsCompress
  mov [EAX].TDxtConstsData.MT_AlphaCompress, OFFSET DXT_MT_AlphaCompress
  mov [EAX].TDxtConstsData.ColorCalculationDriver, OFFSET DXT_CompressSSE
  cmp SSE, 0
  jnz @fill_colortable
  // if have no SSE
  mov [EAX].TDxtConstsData.ColorCalculationDriver, OFFSET DXT_CompressFPU
@fill_colortable:
  mov [EAX].TDxtConstsData.ColorTable, OFFSET  DXT_SINGLECOLORS


@run:
  // 2960 bytes dump
  DD $57E58955,$EC815356,$0000013C,$8A10458B,$E0831055,$01E28302,$FED69588,$840FFFFF,$000002EF,$C7084D8B
  DD $FFFEF085,$000080FF,$D08D8900,$85FFFFFE,$85950FC0,$FFFFFED8,$FEF485C6,$C601FFFF,$FFFED785,$DB3100FF
  DD $8D8DF631,$FFFFFEF8,$B68D1FEB,$00000000,$C83184C6,$FF000000,$FED785C6,$4601FFFF,$0F10FE83,$00009584
  DD $0C7D8B00,$89B7148B,$FFE281D7,$C100FFFF,$548918EF,$FF8108B1,$000000FF,$FF850D74,$F7C0940F,$F48520D8
  DD $3BFFFFFE,$FFFEF0BD,$80B572FF,$FFFED8BD,$840F00FF,$000000DA,$000000B8,$00FFBFFF,$D0090000,$4489DB85
  DD $257408B1,$85C6C031,$FFFFFEEC,$40953B00,$75FFFFFF,$00C4E90F,$543B0000,$840F4881,$000000B4,$7CD83940
  DD $319C88F1,$000000C8,$9D948946,$FFFFFF40,$809D7C89,$10FE8343,$FF6B850F,$858AFFFF,$FFFFFED6,$FED8958A
  DD $8588FFFF,$FFFFFEF8,$FEF99588,$858AFFFF,$FFFFFED7,$FEF4958A,$9D89FFFF,$FFFFFEFC,$FEFA8588,$9588FFFF
  DD $FFFFFEFB,$FED6BD80,$7500FFFF,$0FC08408,$0001E985,$75DB8500,$D08D8B69,$80FFFFFE,$FFFED6BD,$01C700FF
  DD $00000000,$FF0441C7,$75FFFFFF,$08758B10,$000006C7,$46C70000,$00000004,$3CC48100,$5B000001,$C25D5F5E
  DD $F8890010,$E918E0C1,$FFFFFF26,$0026748D,$FEEC8588,$958AFFFF,$FFFFFEEC,$C8319488,$8B000000,$01808554
  DD $855489FA,$FEADE980,$458BFFFF,$00F82510,$F8830000,$85940F70,$FFFFFEF4,$83840F4B,$80000003,$FFFEF4BD
  DD $840F00FF,$0000056E,$8B14758B,$C0850846,$0560840F,$74890000,$4C890824,$BD8B0424,$FFFFFED0,$FF243C89
  DD $D6BD80D0,$00FFFFFE,$FF6F850F,$BD80FFFF,$FFFFFEFB,$4E840F00,$8B000005,$C0310C55,$3120C283,$0C758BC9
  DD $8A07428A,$E083034A,$07E18307,$C108E1C1,$C8090BE0,$4A8AC931,$07E1830B,$090EE1C1,$8AC931C8,$E1830F4A
  DD $11E1C107,$C931C809,$83134A8A,$E1C107E1,$31C80914,$174A8AC9,$C107E183,$C80917E1,$4A8AC931,$1F528A1B
  DD $8107E183,$0000FFE2,$1AE1C100,$091DE2C1,$084D8BC8,$4189D009,$8AC03104,$E0830746,$0BE0C107,$568AD231
  DD $07E28303,$0908E2C1,$8AD231D0,$E2830B56,$0EE2C107,$D231D009,$830F568A,$E2C107E2,$31D00911,$13568AD2
  DD $C107E283,$D00914E2,$568AD231,$07E28317,$0917E2C1,$8AD231D0,$E2831B56,$1AE2C107,$D231D009,$C61F568A
  DD $E2C10001,$89D0091D,$41C60141,$C4810501,$0000013C,$5D5F5E5B,$8B0010C2,$C2830855,$01F88308,$9589C919
  DD $FFFFFED0,$01F88341,$8D88F619,$FFFFFED8,$8901E683,$FFFEF0B5,$FD02E9FF,$DB85FFFF,$FE0F840F,$7D80FFFF
  DD $880F00C0,$000007F5,$FEF485C7,$0000FFFF,$FF310000,$C7F63166,$FFFEEC85,$000000FF,$C17D8000,$A9880F00
  DD $80000007,$0F00C27D,$00077688,$C37D8000,$43880F00,$80000007,$0F00C47D,$00071088,$C57D8000,$DD880F00
  DD $80000006,$0F00C67D,$0006AA88,$C77D8000,$77880F00,$80000006,$0F00C87D,$00064488,$C97D8000,$11880F00
  DD $80000006,$0F00CA7D,$0005DE88,$CB7D8000,$AB880F00,$80000005,$0F00CC7D,$00057888,$CD7D8000,$45880F00
  DD $80000005,$0F00CE7D,$00051288,$CF7D8000,$DF880F00,$89000004,$EC958BF0,$D1FFFFFE,$EC85C7F8,$00FFFFFE
  DD $01000000,$F08589C2,$89FFFFFE,$FA85C6D0,$00FFFFFE,$F71FFAC1,$F0958BFE,$88FFFFFE,$FFFEEC85,$89FA01FF
  DD $1FFAC1D0,$958BFEF7,$FFFFFEEC,$858BC688,$FFFFFEF4,$FEEC9589,$958BFFFF,$FFFFFEF0,$D089C201,$F71FFAC1
  DD $00FF25FE,$958B0000,$FFFFFEEC,$8110E0C1,$00FFFFE2,$8DC20900,$85890143,$FFFFFEFC,$BE0FF601,$9489C045
  DD $FFFF409D,$9D7489FF,$00CA8180,$39020000,$34840FC3,$0F000004,$39C145BE,$1D840FC3,$0F000004,$39C245BE
  DD $06840FC3,$0F000004,$39C345BE,$EF840FC3,$0F000003,$39C445BE,$D8840FC3,$0F000003,$39C545BE,$C1840FC3
  DD $0F000003,$39C645BE,$AA840FC3,$0F000003,$39C745BE,$93840FC3,$0F000003,$39C845BE,$7C840FC3,$0F000003
  DD $39C945BE,$65840FC3,$0F000003,$39CA45BE,$4E840FC3,$0F000003,$39CB45BE,$37840FC3,$0F000003,$39CC45BE
  DD $20840FC3,$0F000003,$39CD45BE,$09840FC3,$0F000003,$39CE45BE,$F2840FC3,$0F000002,$39CF45BE,$DB840FC3
  DD $8B000002,$FFFEFC9D,$FBF7E9FF,$BD80FFFF,$FFFFFED6,$800D7400,$FFFEFABD,$850F00FF,$00000253,$7D8BC031
  DD $40858A14,$31FFFFFF,$14578BF6,$888DDB31,$00000300,$048AC031,$D045894A,$448AC031,$4589014A,$8AC031D4
  DD $FFFF4185,$00888DFF,$31000002,$4A048AC0,$31D84589,$4A448AC0,$DC458901,$858AC031,$FFFFFF42,$0300888D
  DD $C0310000,$894A048A,$C031E045,$014A448A,$8BE44589,$558BE045,$05E2C1D8,$C1DC4D8B,$7D8B0BE0,$05E1C1D0
  DD $558BD009,$C1F809E4,$CA090BE2,$09D44D8B,$0FD039CA,$00021C84,$C1960F00,$00FFE181,$CF890000,$F739C931
  DD $C1890874,$CA89D089,$E2C101B1,$8BC20910,$FFFED085,$89DB84FF,$81840F10,$B8000001,$00000002,$000003BA
  DD $80D18900,$7500C27D,$89C18902,$C1D189CE,$7D8004E6,$027500C1,$1C8DC189,$0000008D,$09D18900,$C37D80F3
  DD $89027500,$89CE89C1,$06E6C1D1,$7D80DE09,$027500C4,$CB89C189,$E3C1D189,$80F30908,$7500C57D,$89C18902
  DD $C1D189CE,$DE090AE6,$00C67D80,$C1890275,$D189CB89,$090CE3C1,$C77D80F3,$89027500,$89CE89C1,$0EE6C1D1
  DD $7D80DE09,$027500C8,$CB89C189,$E3C1D189,$80F30910,$7500C97D,$89C18902,$C1D189CE,$DE0912E6,$00CA7D80
  DD $C1890275,$D189CB89,$0914E3C1,$CB7D80F3,$89027500,$89CE89C1,$16E6C1D1,$7D80DE09,$027500CC,$CB89C189
  DD $E3C1D189,$80F30918,$7500CD7D,$89C18902,$C1D189CE,$DE091AE6,$00CE7D80,$C1890275,$D189CB89,$091CE3C1
  DD $C07D80F3,$89027500,$80D909C1,$7500CF7D,$C1C28902,$CA091EE2,$FED08D8B,$5189FFFF,$FAB3E904,$458BFFFF
  DD $244C8914,$24448904,$D0958B08,$89FFFFFE,$10FF2414,$FFFA98E9,$F4BD80FF,$00FFFFFE,$7D8B2274,$0C478B14
  DD $1874C085,$08247C89,$890C558B,$8B042454,$0C89084D,$E9D0FF24,$FFFFF9E9,$8914758B,$8B082474,$7C890C7D
  DD $458B0424,$24048908,$E90456FF,$FFFFF9CD,$1901F980,$03C083C0,$7AE9C289,$8BFFFFFE,$01B8147D,$8D000000
  DD $5F8BD075,$83D23114,$548A02F8,$95894701,$FFFFFEF0,$038A840F,$958B0000,$FFFFFEF0,$0100C281,$D7890000
  DD $FEF09589,$D231FFFF,$897B148A,$31F8C654,$7B548AD2,$02F88301,$FCC65489,$EB40037F,$0001BEBC,$01B30000
  DD $FFFDB6E9,$31C289FF,$FDF4E9C9,$9589FFFF,$FFFFFF3C,$FFFD1AE9,$389589FF,$E9FFFFFF,$FFFFFD03,$FF349589
  DD $ECE9FFFF,$89FFFFFC,$FFFF3095,$FCD5E9FF,$9589FFFF,$FFFFFF2C,$FFFCBEE9,$289589FF,$E9FFFFFF,$FFFFFCA7
  DD $FF249589,$90E9FFFF,$89FFFFFC,$FFFF2095,$FC79E9FF,$9589FFFF,$FFFFFF1C,$FFFC62E9,$189589FF,$E9FFFFFF
  DD $FFFFFC4B,$FF149589,$34E9FFFF,$89FFFFFC,$FFFF1095,$FC1DE9FF,$9589FFFF,$FFFFFF0C,$FFFC06E9,$089589FF
  DD $E9FFFFFF,$FFFFFBEF,$FF049589,$D8E9FFFF,$89FFFFFB,$FFFF0095,$FBC1E9FF,$458BFFFF,$CF5D880C,$8A46D231
  DD $95013C50,$FFFFFEEC,$508AD231,$31D7013D,$3E508AD2,$FEF49501,$F8E9FFFF,$8BFFFFFA,$5D880C55,$46C031CE
  DD $0138428A,$FFFEEC85,$8AC031FF,$C7013942,$428AC031,$F485013A,$E9FFFFFE,$FFFFFAC5,$880C458B,$D231CD5D
  DD $34508A46,$FEEC9501,$D231FFFF,$0135508A,$8AD231D7,$95013650,$FFFFFEF4,$FFFA92E9,$0C558BFF,$31CC5D88
  DD $428A46C0,$EC850130,$31FFFFFE,$31428AC0,$C031C701,$0132428A,$FFFEF485,$FA5FE9FF,$458BFFFF,$CB5D880C
  DD $8A46D231,$95012C50,$FFFFFEEC,$508AD231,$31D7012D,$2E508AD2,$FEF49501,$2CE9FFFF,$8BFFFFFA,$5D880C55
  DD $46C031CA,$0128428A,$FFFEEC85,$8AC031FF,$C7012942,$428AC031,$F485012A,$E9FFFFFE,$FFFFF9F9,$880C458B
  DD $D231C95D,$24508A46,$FEEC9501,$D231FFFF,$0125508A,$8AD231D7,$95012650,$FFFFFEF4,$FFF9C6E9,$0C558BFF
  DD $31C85D88,$428A46C0,$EC850120,$31FFFFFE,$21428AC0,$C031C701,$0122428A,$FFFEF485,$F993E9FF,$458BFFFF
  DD $C75D880C,$8A46D231,$95011C50,$FFFFFEEC,$508AD231,$31D7011D,$1E508AD2,$FEF49501,$60E9FFFF,$8BFFFFF9
  DD $5D880C55,$46C031C6,$0118428A,$FFFEEC85,$8AC031FF,$C7011942,$428AC031,$F485011A,$E9FFFFFE,$FFFFF92D
  DD $880C458B,$D231C55D,$14508A46,$FEEC9501,$D231FFFF,$0115508A,$8AD231D7,$95011650,$FFFFFEF4,$FFF8FAE9
  DD $0C558BFF,$31C45D88,$428A46C0,$EC850110,$31FFFFFE,$11428AC0,$C031C701,$0112428A,$FFFEF485,$F8C7E9FF
  DD $458BFFFF,$C35D880C,$8A46D231,$95010C50,$FFFFFEEC,$508AD231,$31D7010D,$0E508AD2,$FEF49501,$94E9FFFF
  DD $8BFFFFF8,$5D880C55,$46C031C2,$0108428A,$FFFEEC85,$8AC031FF,$C7010942,$428AC031,$F485010A,$E9FFFFFE
  DD $FFFFF861,$880C458B,$D231C15D,$04508A46,$FEEC9501,$D231FFFF,$0105508A,$8AD231D7,$95010650,$FFFFFEF4
  DD $FFF82EE9,$0C558BFF,$31C05D88,$0001BEC0,$028A0000,$FEEC8589,$428AFFFF,$00FF2501,$C7890000,$428AC031
  DD $F4858902,$E9FFFFFE,$FFFFF7F5,$D231D789,$897B148A,$31F8C654,$7B548AD2,$C6548901,$46E940FC,$90FFFFFC
end;

procedure DxtCompress(var Dest: TDxt1Block; const BGRABlock: TBGRABlock);
asm
  mov ecx, 1   // Dxt1
  jmp DxtCompress
end;

procedure DxtCompress(var Dest: TDxt5Block; const BGRABlock: TBGRABlock);
asm
  xor ecx, ecx // Dxt5
  jmp DxtCompress
end;

procedure DxtCompressData(const Dest: pointer; const BGRABlock: TBGRABlock;
  const DXT1: boolean);
asm
  or ecx, 2    // DxtFlag or DataMode
  jmp DxtCompress
end;

procedure DxtCompressData(var Dest: TDxt1Block; const BGRABlock: TBGRABlock);
asm
  mov ecx, 3  // Dxt1 or DataMode
  jmp DxtCompress
end;

procedure DxtCompressData(var Dest: TDxt5Block; const BGRABlock: TBGRABlock);
asm
  mov ecx, 2 // Dxt5 or DataMode
  jmp DxtCompress
end;

procedure DXT_ColorsCompress;
asm
  // 6400 bytes dump
  DD $83E58955,$5657C0E4,$84EC8153,$8B000004,$C7660C75,$044C2484,$037F0000,$2484C766,$0000044E,$7E8B0000
  DD $24BCD904,$0000044E,$ACD9E2DB,$00044C24,$0FFF8500,$0018798E,$10458B00,$00BD148D,$8D000000,$01D0248C
  DD $94890000,$0001A424,$1840DD00,$64248C89,$31000001,$8BCA89C0,$C748064C,$01B42484,$00000000,$B60F0000
  DD $249C89D9,$000001B0,$B024ACDF,$D8000001,$DDB60FC9,$8110E9C1,$0000FFE1,$821CD900,$B0249C89,$C7000001
  DD $01B42484,$00000000,$ACDF0000,$0001B024,$31C9D800,$825CD9DB,$249C8904,$000001C4,$C0248C89,$DF000001
  DD $01C024AC,$C9D80000,$5CD9DB31,$8C8B0882,$00008806,$249C8900,$000001C4,$C0248C89,$DF000001,$01C024AC
  DD $C9D80000,$0C825CD9,$3B04C083,$01A42484,$850F0000,$FFFFFF5F,$5D8BD8DD,$C1F98910,$848D04E1,$0001DC24
  DD $2043D900,$C1D9C0D9,$0C8DC2D9,$DDC28908,$01902494,$90660000,$C3DC02D9,$D8F442D9,$D9C2DEC9,$C9D8F842
  DD $4AD8C5DE,$10C283FC,$C2DECA39,$558BE275,$3042D910,$9C2494D9,$DE000001,$DDCAD8F3,$0178249C,$CAD90000
  DD $9CDDC9D8,$00018024,$DDC9DE00,$0188249C,$EED90000,$94D9C0D9,$0001A424,$D9C0D900,$01A02494,$14EB0000
  DD $B02484D9,$DD000001,$0190249C,$C9D90000,$C9D9CAD9,$782484DD,$D8000001,$84DDF468,$00018024,$F868D800
  DD $882484DD,$D8000001,$9CDDFC68,$0001A824,$D900D900,$83C9D8C1,$C13910C0,$A82484DD,$D8000001,$D8CAD9CA
  DD $DCCCD8CC,$01902484,$9CD90000,$0001B024,$D8C3D900,$2484D8C9,$000001A0,$A0249CD9,$D9000001,$DEC9D8CB
  DD $D9CCD9C5,$01CC249C,$84D90000,$0001CC24,$D8CAD900,$2484D8C9,$000001A4,$A4249CD9,$D8000001,$D9C4DECB
  DD $249CD9CB,$000001CC,$CC2484D9,$DD000001,$01A82484,$CBDE0000,$C1DECAD9,$CC249CD9,$D9000001,$01CC2484
  DD $850F0000,$FFFFFF30,$A02484D9,$D9000001,$D9C9D8C0,$D9CDD8C4,$01B02484,$C8D80000,$C1D8C2D8,$CED8C5D9
  DD $B8249CD9,$D9000001,$01A42484,$C8D80000,$CAD9C3DE,$B82484D8,$D9000001,$01B82484,$C2DE0000,$CDD8C4D9
  DD $CAD9C2DE,$E0DFE2DD,$AF860F9E,$DD000015,$9EE0DFE9,$15AA860F,$D8DD0000,$84D9D8DD,$0001B024,$D9C9D900
  DD $01B82494,$CCD90000,$A82494D9,$D9000001,$01B02484,$CAD80000,$B82484D9,$D8000001,$D9C1DECE,$01A82484
  DD $CAD80000,$94D9C1DE,$00044824,$D8C2D900,$2484D9CE,$000001B8,$A4248CD8,$DE000001,$2484D9C1,$000001A8
  DD $C1DECED8,$442494D9,$D9000004,$D9CAD8CB,$01B82484,$CED80000,$84D9C1DE,$0001A824,$DECDD800,$2494D9C1
  DD $00000440,$E1DDCBD9,$0F9EE0DF,$0014FD86,$D9C0D900,$24948DCC,$00000444,$E0DFE4DD,$769EDCDD,$24948D07
  DD $00000440,$9C2484D9,$D8000001,$D9CCDC32,$2494D9CC,$000001A8,$CCD8C9D9,$B82494D9,$D9000001,$D9CADECC
  DD $01B02484,$CAD80000,$CFD8CCD9,$CAD8C4DE,$CAD9C3DE,$482494D9,$D9000004,$D9CBD8C5,$01A42484,$8CD80000
  DD $0001B824,$D9C1DE00,$01A82484,$CED80000,$94D9C1DE,$00044424,$DEC2D900,$2484D9CC,$000001B8,$C4DECED8
  DD $A82484D9,$D8000001,$D9C4DECD,$2494D9CB,$00000440,$E1DDCBD9,$0F9EE0DF,$00143D86,$D9C0D900,$24948DCC
  DD $00000444,$E0DFE4DD,$769EDCDD,$24948D07,$00000440,$9C2484D9,$D8000001,$D9CCDC32,$2494D9CC,$000001A8
  DD $CCD8C9D9,$B82494D9,$D9000001,$D9CADECC,$01B02484,$CAD80000,$CFD8CCD9,$CAD8C4DE,$CAD9C3DE,$482494D9
  DD $D9000004,$D9CBD8C5,$01A42484,$8CD80000,$0001B824,$D9C1DE00,$01A82484,$CED80000,$94D9C1DE,$00044424
  DD $DEC2D900,$2484D9CC,$000001B8,$C4DECED8,$A82484D9,$D8000001,$D9C4DECD,$2494D9CB,$00000440,$E1DDCBD9
  DD $0F9EE0DF,$00137D86,$D9C0D900,$24948DCC,$00000444,$E0DFE4DD,$769EDCDD,$24948D07,$00000440,$9C2484D9
  DD $D8000001,$D9CCDC32,$2494D9CC,$000001A8,$CCD8C9D9,$B82494D9,$D9000001,$D9CADECC,$01B02484,$CAD80000
  DD $CFD8CCD9,$CAD8C4DE,$CAD9C3DE,$482494D9,$D9000004,$D9CBD8C5,$01A42484,$8CD80000,$0001B824,$D9C1DE00
  DD $01A82484,$CED80000,$94D9C1DE,$00044424,$DEC2D900,$2484D9CC,$000001B8,$C4DECED8,$A82484D9,$D8000001
  DD $D9C4DECD,$2494D9CB,$00000440,$E1DDCBD9,$0F9EE0DF,$0012BD86,$D9C0D900,$24948DCC,$00000444,$E0DFE4DD
  DD $769EDCDD,$24948D07,$00000440,$9C2484D9,$D8000001,$D9CCDC32,$2494D9CC,$000001A8,$CCD8C9D9,$B82494D9
  DD $D9000001,$D9CADECC,$01B02484,$CAD80000,$CFD8CCD9,$CAD8C4DE,$CAD9C3DE,$482494D9,$D9000004,$D9CBD8C5
  DD $01A42484,$8CD80000,$0001B824,$D9C1DE00,$01A82484,$CED80000,$94D9C1DE,$00044424,$DEC2D900,$2484D9CC
  DD $000001B8,$C4DECED8,$A82484D9,$D8000001,$D9C4DECD,$2494D9CB,$00000440,$E1DDCBD9,$0F9EE0DF,$0011FD86
  DD $D9C0D900,$24948DCC,$00000444,$E0DFE4DD,$769EDCDD,$24948D07,$00000440,$9C2484D9,$D8000001,$D9CCDC32
  DD $2494D9CC,$000001A8,$CCD8C9D9,$B82494D9,$D9000001,$D9CADECC,$01B02484,$CAD80000,$CFD8CCD9,$CAD8C4DE
  DD $CAD9C3DE,$482494D9,$D9000004,$D9CBD8C5,$01A42484,$8CD80000,$0001B824,$D9C1DE00,$01A82484,$CED80000
  DD $94D9C1DE,$00044424,$DEC2D900,$2484D9CC,$000001B8,$C4DECED8,$A82484D9,$D8000001,$D9C4DECD,$2494D9CB
  DD $00000440,$E1DDCBD9,$0F9EE0DF,$00113D86,$D9C0D900,$24948DCC,$00000444,$E0DFE4DD,$769EDCDD,$24948D07
  DD $00000440,$9C2484D9,$D8000001,$D9CCDC32,$2494D9CC,$000001A8,$CCD8C9D9,$B82494D9,$D9000001,$D9CADECC
  DD $01B02484,$CAD80000,$CFD8CCD9,$CAD8C4DE,$CAD9C3DE,$482494D9,$D9000004,$D9CBD8C5,$01A42484,$8CD80000
  DD $0001B824,$D9C1DE00,$01A82484,$CED80000,$94D9C1DE,$00044424,$DEC2D900,$2484D9CC,$000001B8,$C4DECED8
  DD $A82484D9,$D8000001,$D9C4DECD,$2494D9CB,$00000440,$E1DDCBD9,$0F9EE0DF,$00107D86,$D9C0D900,$24948DCC
  DD $00000444,$E0DFE4DD,$769EDCDD,$24948D07,$00000440,$9C2484D9,$D8000001,$D9CCDC32,$249CD9CC,$000001B8
  DD $CBD9CBD8,$84D9C9DE,$0001B024,$D9C9D800,$DECCD8C6,$2484D9C1,$000001B8,$C1DECBD8,$482494D9,$D9000004
  DD $D9C9D8CE,$01A42484,$CCD80000,$84D9C1DE,$0001B824,$DECED800,$2494D9C1,$00000444,$C9DECAD9,$CADECCD9
  DD $C1DECBD9,$8CD8C9D9,$0001B824,$D9C1DE00,$04402494,$C9D90000,$E0DFE2DD,$CB860F9E,$D900000F,$8DCAD9C0
  DD $04442494,$E2DD0000,$DADDE0DF,$8D07769E,$04402494,$84D90000,$00019C24,$8532D800,$DCCADCFF,$0FCBDEC9
  DD $0000B78E,$24948B00,$00000164,$D189C031,$CBD802D9,$D80442D9,$D9C1DECA,$CBD80842,$00818489,$83000001
  DD $C1DE10C2,$40819CD9,$40000001,$D875F839,$D8DDD8DD,$DB31D8DD,$B024B489,$8D000001,$F939014B,$948B6574
  DD $00016424,$8A84D900,$00000140,$3C8A84D9,$DD000001,$F6E0DFE1,$357545C4,$008AB48B,$8D000001,$01009A94
  DD $5AD90000,$85028B44,$4052D9DB,$89044289,$D91D7E32,$834B3C42,$E1DD04EA,$C4F6E0DF,$DDDF7445,$EBD8DDD8
  DD $DDD8DD09,$9003EBD8,$CB89D8DD,$39014B8D,$8B9B75F9,$01B024B4,$06EB0000,$D8DDD8DD,$948BD8DD,$00016424
  DD $104D8B00,$08245489,$04244C89,$FF243489,$5D8B1051,$9083D910,$D9000000,$00009483,$B083D900,$D9000000
  DD $03502484,$CBD80000,$00B0838B,$9C8D0000,$00031024,$D9C1D800,$0350249C,$84D90000,$00035424,$29CAD800
  DD $03502484,$C1D80000,$54249CD9,$D9000003,$03582484,$CBD80000,$54248429,$D8000003,$249CD9C1,$00000358
  DD $602484D9,$D8000003,$248429CB,$00000358,$9CD9C1D8,$00036024,$D8C9D900,$0364248C,$84290000,$00036024
  DD $D9C1D800,$0364249C,$C9D90000,$68248CD8,$29000003,$03642484,$C1DE0000,$68249CD9,$29000003,$03682484
  DD $468A0000,$24848802,$00000107,$BC83C031,$00037024,$197E0000,$6424948B,$C6000001,$01400284,$40000000
  DD $70248439,$7F000003,$248439E7,$00000374,$148D117E,$0202C603,$84394240,$00037424,$8AF27F00,$0107248C
  DD $F1800000,$248C8801,$00000117,$148D1A74,$24843B03,$00000378,$02C60E7D,$39424003,$03782484,$F27F0000
  DD $8E0FC739,$000000E3,$0C8DFA89,$89C22903,$01B8248C,$94890000,$0001B024,$F7CA8900,$03E283DA,$B024943B
  DD $76000001,$24948B07,$000001B0,$3E74D285,$64248C8B,$89000001,$01A824B4,$C1010000,$A4248C89,$31000001
  DD $24B48BC9,$000001A4,$0E84C640,$00000140,$CA394101,$B48BEB77,$0001A824,$24943900,$000001B0,$8C8B7574
  DD $0001B024,$89D12900,$01B0248C,$E9C10000,$248C8902,$000001A8,$8902E1C1,$01A4248C,$43740000,$B8249403
  DD $89000001,$01B82494,$D2310000,$0026748D,$B8248C8B,$C7000001,$01019104,$3B420101,$01A82494,$E8720000
  DD $A424948B,$03000001,$01A42484,$94390000,$0001B024,$8D0C7400,$C6400314,$39420102,$8DF77FC7,$0320248C
  DD $FF850000,$8C248C89,$7E000000,$89C03134,$01B824B4,$948B0000,$00016424,$00768D00,$0082B48B,$8A000001
  DD $0140028C,$88400000,$0320348C,$F8390000,$B48BE675,$0001B824,$86BE0F00,$000000C8,$0FFFF883,$000C4D84
  DD $04848A00,$00000320,$BE0F0388,$0000C986,$FFF88300,$0C2B840F,$848A0000,$00032004,$01438800,$CA86BE0F
  DD $83000000,$840FFFF8,$00000C08,$2004848A,$88000003,$BE0F0243,$0000CB86,$FFF88300,$0BE5840F,$848A0000
  DD $00032004,$03438800,$CC86BE0F,$83000000,$840FFFF8,$00000BC2,$2004848A,$88000003,$BE0F0443,$0000CD86
  DD $FFF88300,$0B9F840F,$848A0000,$00032004,$05438800,$CE86BE0F,$83000000,$840FFFF8,$00000B7C,$2004848A
  DD $88000003,$BE0F0643,$0000CF86,$FFF88300,$0B59840F,$848A0000,$00032004,$07438800,$D086BE0F,$83000000
  DD $840FFFF8,$00000B36,$2004848A,$88000003,$BE0F0843,$0000D186,$FFF88300,$0B13840F,$848A0000,$00032004
  DD $09438800,$D286BE0F,$83000000,$840FFFF8,$00000AF0,$2004848A,$88000003,$BE0F0A43,$0000D386,$FFF88300
  DD $0ACD840F,$848A0000,$00032004,$0B438800,$D486BE0F,$83000000,$840FFFF8,$00000AAA,$2004848A,$88000003
  DD $BE0F0C43,$0000D586,$FFF88300,$0A87840F,$848A0000,$00032004,$0D438800,$D686BE0F,$83000000,$840FFFF8
  DD $00000A64,$2004848A,$88000003,$BE0F0E43,$0000D786,$FFF88300,$09FC840F,$848A0000,$00032004,$0F438800
  DD $8B087E8B,$F8891056,$C1144E8B,$BC8918E8,$0000C824,$C0AF0F00,$7E8BE8D1,$24848918,$000003F0,$890C468B
  DD $00CC2484,$94890000,$0000D024,$18E8C100,$D4248C89,$0F000000,$E8D1C0AF,$D824BC89,$89000000,$03F42484
  DD $D0890000,$8B18E8C1,$AF0F2056,$89E8D1C0,$00E02494,$84890000,$0003F824,$C1C88900,$4E8B18E8,$C0AF0F24
  DD $8C89E8D1,$0000E424,$24848900,$000003FC,$E8C1F889,$287E8B18,$D1C0AF0F,$24BC89E8,$000000E8,$00248489
  DD $8B000004,$84891C46,$0000DC24,$18E8C100,$D1C0AF0F,$248489E8,$00000404,$E8C1D089,$30568B18,$D1C0AF0F
  DD $249489E8,$000000F0,$08248489,$89000004,$18E8C1C8,$0F344E8B,$E8D1C0AF,$0C248489,$89000004,$18E8C1F8
  DD $D1C0AF0F,$248489E8,$00000410,$892C468B,$00EC2484,$E8C10000,$C0AF0F18,$8489E8D1,$00041424,$C1D08900
  DD $AF0F18E8,$89E8D1C0,$04182484,$C8890000,$8918E8C1,$00F4248C,$AF0F0000,$387E8BC0,$D140568B,$24BC89E8
  DD $000000F8,$1C248489,$89000004,$18E8C1F8,$00249489,$0F000001,$E8D1C0AF,$702484C7,$00000001,$89000000
  DD $04202484,$468B0000,$2484893C,$000000FC,$C144768B,$B48918E8,$0000C424,$C0AF0F00,$EEC1E8D1,$24848918
  DD $00000424,$E8C1D089,$C0AF0F18,$D1F6AF0F,$89EED1E8,$04282484,$B4890000,$00015824,$24B48900,$0000042C
  DD $70248C8B,$8B000001,$00C82484,$E1C10000,$24B48B03,$000000C4,$FF25E8D3,$89000000,$01182484,$84890000
  DD $0003B024,$24848B00,$000000CC,$FF25E8D3,$89000000,$03B42484,$848B0000,$0000D024,$25E8D300,$000000FF
  DD $B8248489,$8B000003,$00D42484,$E8D30000,$0000FF25,$24848900,$000003BC,$D824848B,$D3000000,$00FF25E8
  DD $84890000,$0003C024,$24848B00,$000000DC,$FF25E8D3,$89000000,$03C42484,$848B0000,$0000E024,$25E8D300
  DD $000000FF,$C8248489,$8B000003,$00E42484,$E8D30000,$0000FF25,$24848900,$000003CC,$E824848B,$D3000000
  DD $00FF25E8,$84890000,$0003D024,$24848B00,$000000EC,$FF25E8D3,$89000000,$03D42484,$848B0000,$0000F024
  DD $25E8D300,$000000FF,$D8248489,$8B000003,$00F42484,$E8D30000,$0000FF25,$24848900,$000003DC,$F824848B
  DD $D3000000,$00FF25E8,$84890000,$0003E024,$24848B00,$000000FC,$FF25E8D3,$D3000000,$248489EE,$000003E4
  DD $0024848B,$D3000001,$25F189E8,$000000FF,$00FFE181,$84890000,$0003E824,$248C8900,$0000011C,$7024BC8B
  DD $89000001,$03EC248C,$8C8B0000,$00016424,$24B48B00,$00000164,$80B9948B,$83000001,$017024BC,$8D010000
  DD $940FFF4A,$FF2542C0,$89000000,$01B82494,$948B0000,$000190BE,$48CF8900,$FFC1D7F7,$E0E0831F,$213FC083
  DD $24BC89CF,$00000160,$08248489,$3B000001,$01B82484,$0E7E0000,$B8248C8B,$89000001,$0108248C,$B48B0000
  DD $00010824,$24B43900,$00000160,$056B870F,$4A8D0000,$CF8942FF,$5C248489,$F7000001,$1FFFC1D7,$D039CF21
  DD $0C24BC89,$7E000001,$24948907,$0000015C,$682484C7,$FF000001,$907FFFFF,$0026748D,$5C24848B,$39000001
  DD $010C2484,$870F0000,$000004FE,$6024948B,$8B000001,$01602484,$E0C10000,$248C8B02,$00000160,$8B04EAC1
  DD $010C24B4,$C2090000,$6024848B,$C1000001,$BC8B03E0,$0003B424,$02E9C100,$10249489,$09000001,$24948BC1
  DD $000003BC,$B824848B,$89000003,$016C248C,$B4890000,$00017424,$248C8B00,$000003C0,$C424B48B,$89000003
  DD $014C24BC,$84890000,$00014824,$24BC8B00,$000003C8,$CC24848B,$89000003,$01442494,$948B0000,$0003D024
  DD $248C8900,$00000140,$3C24B489,$8B000001,$03D4248C,$BC890000,$00013824,$24848900,$00000134,$30249489
  DD $8B000001,$03D824B4,$BC8B0000,$0003DC24,$24848B00,$000003E0,$E424948B,$89000003,$012C248C,$B4890000
  DD $00012824,$24BC8900,$00000124,$20248489,$89000001,$01542494,$8C8B0000,$0003E824,$248C8900,$00000150
  DD $00035EE9,$50148D00,$AAAAABB8,$D1E2F7AA,$24B48BEA,$00000430,$38249489,$8B000004,$04342494,$ABB80000
  DD $8DAAAAAA,$E2F75614,$9489EAD1,$00043C24,$8BC03100,$01182494,$038A0000,$4C24B48B,$8B000001,$0430848C
  DD $C0310000,$8C8BCA29,$00014824,$24948900,$00000178,$8B01438A,$04308494,$C0310000,$2902438A,$F6AF0FD6
  DD $3084BC8B,$31000004,$89F929C0,$8B4C244C,$0144248C,$438A0000,$84948B03,$00000430,$438AC031,$8BD12904
  DD $01402494,$BC8B0000,$00043084,$8AC03100,$FA290543,$3C24BC8B,$2B000001,$043084BC,$C0310000,$B024BC89
  DD $8B000001,$013824BC,$438A0000,$84BC2B06,$00000430,$BC89C031,$0001A424,$24BC8B00,$00000134,$2B07438A
  DD $043084BC,$C0310000,$B824BC89,$8B000001,$013024BC,$438A0000,$84BC2B08,$00000430,$BC89C031,$0001A024
  DD $24BC8B00,$0000012C,$2B09438A,$043084BC,$C0310000,$9C24BC89,$8A000001,$BC8B0A43,$00012824,$84BC2B00
  DD $00000430,$BC89C031,$0001A824,$24BC8B00,$00000124,$2B0B438A,$043084BC,$C0310000,$9024BC89,$8B000001
  DD $012024BC,$438A0000,$84BC2B0C,$00000430,$BC89C031,$00018824,$24BC8B00,$00000154,$2B0D438A,$043084BC
  DD $C0310000,$8024BC89,$8B000001,$015024BC,$438A0000,$84BC2B0E,$00000430,$4C24448B,$89C0AF0F,$008824BC
  DD $BC8B0000,$0003F824,$C7AF0F00,$F424BC8B,$0F000003,$AF0FF7AF,$8BC601C9,$03FC2484,$AF0F0000,$C8AF0FD2
  DD $0024BC8B,$01000004,$D7AF0FCE,$B0248C8B,$8B000001,$01B824BC,$AF0F0000,$16148DC9,$0424B48B,$0F000004
  DD $B48BCEAF,$0001A424,$0FCA0100,$AF0FF6AF,$248C8BFF,$00000408,$0C24848B,$0F000004,$AF0FF1AF,$24848BF8
  DD $000001A0,$9C248C8B,$0F000001,$AF0FC0AF,$8BF201C9,$041424B4,$FA010000,$1024BC8B,$0F000004,$AF0FC7AF
  DD $24B48BCE,$000001A8,$9024BC8B,$0F000001,$AF0FF6AF,$8BC201FF,$041C2484,$CA010000,$18248C8B,$0F000004
  DD $AF0FF1AF,$24848BF8,$00000188,$80248C8B,$0F000001,$AF0FC0AF,$8BF201C9,$042424B4,$FA010000,$2024BC8B
  DD $0F000004,$AF0FC7AF,$8BC201CE,$008824B4,$CA010000,$28248C8B,$0F000004,$AF0FF6AF,$24BC8BF1,$00000178
  DD $1C248C8B,$0F000001,$048DFFAF,$24948B32,$000003F0,$31FAAF0F,$8AF801D2,$B48B0F53,$00015824,$94BC8B00
  DD $00000430,$AF0FF929,$CEAF0FC9,$8C3BC101,$00016824,$8B317D00,$01702484,$948B0000,$00016424,$24B48B00
  DD $00000160,$7424BC8B,$89000001,$018082B4,$BC890000,$00019082,$248C8900,$00000168,$7424948B,$8B000001
  DD $015C2484,$89420000,$01742494,$84390000,$00017424,$8F870F00,$83000000,$017024BC,$74010000,$24848B56
  DD $00000174,$7424948B,$C1000001,$BC8B03E2,$00016C24,$02E8C100,$3024BC89,$09000004,$89FA89D0,$04342484
  DD $BC800000,$00011724,$850F0000,$FFFFFC5D,$84C7D001,$00043C24,$00000000,$89E8D100,$04382484,$76E90000
  DD $8BFFFFFC,$01742484,$948B0000,$00017424,$02E2C100,$1024B48B,$C1000001,$B48904E8,$00043024,$89D00900
  DD $248489F2,$00000434,$848BA8EB,$00016024,$24948B00,$00000108,$24848940,$00000160,$60249439,$0F000001
  DD $FFFACB86,$2484FFFF,$00000170,$7024BC83,$03000001,$F88E850F,$948BFFFF,$00035424,$24848B00,$00000358
  DD $8B0BE0C1,$0368248C,$E2C10000,$0BE1C105,$848BC209,$00036424,$24940B00,$00000350,$0905E0C1,$24840BC8
  DD $00000360,$0724BC80,$00000001,$0089840F,$C2390000,$C1896D7F,$E1C1D089,$085D8B10,$C031C109,$8C8B0B89
  DD $00016424,$31DB3100,$819C8AD2,$00000141,$4281948A,$C1000001,$758B02E3,$04E2C108,$DB31DA09,$4081940A
  DD $8A000001,$0143819C,$E3C10000,$88DA0906,$40040654,$7504F883,$84C481C4,$5B000004,$EC895F5E,$03C6C35D
  DD $9C3B4301,$00008C24,$8A567400,$74C9840B,$75C9FEED,$0003C6EC,$43C6E7EB,$05E9030F,$39FFFFF6,$0F267CC2
  DD $FFFF7185,$8BC031FF,$0164248C,$84C60000,$00014001,$83400000,$EB7510F8,$D089D189,$FFFF55E9,$830B8AFF
  DD $E18301F1,$430B8803,$8C249C3B,$75000000,$E9D189EC,$FFFFFF3A,$030E43C6,$FFF59DE9,$0D43C6FF,$F57AE903
  DD $43C6FFFF,$57E9030C,$C6FFFFF5,$E9030B43,$FFFFF534,$030A43C6,$FFF511E9,$0943C6FF,$F4EEE903,$43C6FFFF
  DD $CBE90308,$C6FFFFF4,$E9030743,$FFFFF4A8,$030643C6,$FFF485E9,$0543C6FF,$F462E903,$43C6FFFF,$3FE90304
  DD $C6FFFFF4,$E9030343,$FFFFF41C,$030243C6,$FFF3F9E9,$0143C6FF,$F3D6E903,$03C6FFFF,$F3B4E903,$C2D9FFFF
  DD $948DCAD9,$00044824,$F030E900,$C1D9FFFF,$948DCCD9,$00044824,$EF7EE900,$C1D9FFFF,$948DCCD9,$00044824
  DD $EEBEE900,$C1D9FFFF,$948DCCD9,$00044824,$EDFEE900,$C1D9FFFF,$948DCCD9,$00044824,$ED3EE900,$C1D9FFFF
  DD $948DCCD9,$00044824,$EC7EE900,$C1D9FFFF,$948DCCD9,$00044824,$EBBEE900,$C1D9FFFF,$948DCCD9,$00044824
  DD $EAFEE900,$D8DDFFFF,$02EBC9D9,$E9DAC9D9,$779EE0DF,$D9C3D921,$2494D9CB,$000001B8,$94D9CAD9,$0001A824
  DD $D9CAD900,$D9C9D9CB,$EA49E9CC,$C0D9FFFF,$A42484D9,$D9000001,$01B8249C,$CBD90000,$A82494D9,$D9000001
  DD $D9C9D9CB,$EA25E9CC,$458BFFFF,$2040D910,$B0249CD9,$D9000001,$848D3040,$0001D024,$249CD900,$0000019C
  DD $C0D9EED9,$64248489,$D9000001,$01A42494,$C0D90000,$C9D9C1D9,$CAD9CBD9,$6CE9C9D9,$90FFFFE9,$0026748D
end;

procedure DXT_CompressSSE;
asm
  // 1424 bytes dump
  DD $57E58955,$EC815356,$0000028C,$8B0C4D8B,$280F0845,$280F3041,$290F5061,$FFFE9885,$04508BFF,$6069280F
  DD $68A5290F,$0FFFFFFD,$0F704128,$FE08AD29,$FA83FFFF,$A1280F00,$00000080,$1885290F,$0FFFFFFE,$0090A928
  DD $290F0000,$FFFD88A5,$81280FFF,$000000A0,$FD809589,$280FFFFF,$0000B0A1,$AD290F00,$FFFFFE48,$0258B60F
  DD $A885290F,$0FFFFFFE,$FEC8A529,$8E0FFFFF,$00000411,$51100FF3,$ED570F30,$FED88D89,$4D8BFFFF,$8DD68910
  DD $FFFEE895,$C1C031FF,$D78902E6,$8BCD280F,$01000194,$E2C10000,$100FF304,$0F0C1144,$140FE028,$E2140FC0
  DD $0FC4160F,$0F0A0459,$0F870413,$08874417,$3904C083,$C8580FF0,$8D8BCD75,$FFFFFED8,$D88D290F,$0FFFFFFD
  DD $0F404128,$FE788529,$D888FFFF,$34C0570F,$80BD8301,$00FFFFFD,$FD7B8588,$290FFFFF,$FFFE2885,$85290FFF
  DD $FFFFFDB8,$A885290F,$C7FFFFFD,$FFFDD485,$000000FF,$A485C700,$00FFFFFD,$C7000000,$FFFDA085,$000000FF
  DD $9C85C700,$00FFFFFD,$7D000000,$D4858B37,$FFFFFFFD,$FFFDD485,$A5280FFF,$FFFFFE28,$0F04E0C1,$E805A458
  DD $8BFFFFFE,$FFFDD485,$808539FF,$0FFFFFFD,$FE28A529,$8C0FFFFF,$00000355,$FEE8858D,$570FFFFF,$D4BD8BC0
  DD $89FFFFFD,$FFFD7C85,$D4858BFF,$8BFFFFFD,$FFFD7CB5,$85290FFF,$FFFFFE68,$0104E0C1,$848589F0,$88FFFFFD
  DD $FFFE679D,$00768DFF,$FE67BD80,$0F00FFFF,$0002B384,$A5280F00,$FFFFFDD8,$5C0FFE89,$FFFE68A5,$A55C0FFF
  DD $FFFFFE28,$B8A5290F,$0FFFFFFE,$FE68A528,$FA89FFFF,$E2C1F889,$A5590F04,$FFFFFD68,$68AD280F,$0FFFFFFE
  DD $FE18AD59,$290FFFFF,$FFFE88A5,$85280FFF,$FFFFFE68,$0885590F,$8BFFFFFE,$FFFD7C8D,$AD580FFF,$FFFFFE28
  DD $28A5580F,$01FFFFFE,$AD290FCA,$FFFFFE38,$7B9DB60F,$0FFFFFFD,$FDF88529,$290FFFFF,$FFFDE8A5,$0177E9FF
  DD $B68D0000,$00000000,$889D280F,$0FFFFFFE,$FE889528,$580FFFFF,$FFFEB89D,$A5280FFF,$FFFFFDE8,$FFD2C60F
  DD $0FC4280F,$0FFFC4C6,$C60FF328,$290FFFF3,$FFFED885,$E8280FFF,$0FC2280F,$280FCC28,$C2590FFC,$0FEE590F
  DD $590FCE59,$E85C0FFA,$0FC3280F,$530FC259,$AD590FED,$FFFFFE48,$0FC85C0F,$FED88528,$590FFFFF,$CD590FC3
  DD $0FC75C0F,$FEC88D58,$590FFFFF,$8D5C0FC5,$FFFFFEC8,$A88D590F,$0FFFFFFE,$580FED57,$FFFEC885,$855C0FFF
  DD $FFFFFEC8,$0FE95F0F,$FEA88559,$280FFFFF,$AD280FCD,$FFFFFE98,$0FE95D0F,$570FCD28,$E85F0FED,$0FE1590F
  DD $280FC528,$FFFE98AD,$E85D0FFF,$0FC5280F,$590FED59,$EE590FD8,$0FF1280F,$590FF159,$FFFED8B5,$F5580FFF
  DD $0FE9280F,$590FE859,$EC5C0FEA,$0FEB5C0F,$580FED58,$D5280FEE,$0FDD280F,$0FAAD5C6,$0F55DDC6,$0F00EDC6
  DD $580FEB58,$D5280FEA,$7895C20F,$01FFFFFE,$85CA500F,$8B2D74C9,$FFFDD48D,$AD290FFF,$FFFFFE78,$B885290F
  DD $0FFFFFFD,$FDA88D29,$8589FFFF,$FFFFFDA4,$FDA0BD89,$8D89FFFF,$FFFFFD9C,$1174DB84,$B885280F,$0FFFFFFE
  DD $290F0258,$FFFEB885,$C28340FF,$7FF03910,$67BD8074,$00FFFFFE,$FE82850F,$280FFFFF,$FFFDD885,$855C0FFF
  DD $FFFFFEB8,$68855C0F,$0FFFFFFE,$FEB89D28,$280FFFFF,$FFFEB8A5,$855C0FFF,$FFFFFE28,$189D590F,$0FFFFFFE
  DD $FE689528,$590FFFFF,$FFFE08A5,$95580FFF,$FFFFFEB8,$F885580F,$0FFFFFFD,$0FFFD2C6,$FE38A558,$580FFFFF
  DD $95590FD8,$FFFFFD88,$FFFE3BE9,$84858BFF,$47FFFFFD,$6885280F,$0FFFFFFE,$C0830058,$84858910,$39FFFFFD
  DD $FFFD80BD,$85290FFF,$FFFFFE68,$FD4E8D0F,$B60FFFFF,$FFFE679D,$FCCFE9FF,$9066FFFF,$FD80BD39,$BD7CFFFF
  DD $8BED570F,$FFFD80B5,$AD290FFF,$FFFFFEB8,$FFFD4EE9,$69280FFF,$AD290F40,$FFFFFE78,$570F7D74,$A485C7E4
  DD $00FFFFFD,$0F000000,$FDB8A529,$290FFFFF,$FFFDA8A5,$A085C7FF,$00FFFFFD,$C7000000,$FFFD9C85,$000000FF
  DD $10558B00,$A885280F,$0FFFFFFD,$01808229,$280F0000,$FFFDB8A5,$A2290FFF,$00000190,$FD9C858B,$8D8BFFFF
  DD $FFFFFDA0,$01A08289,$858B0000,$FFFFFDA4,$01A48A89,$82890000,$000001A8,$028CC481,$5E5B0000,$0FC35D5F
  DD $290FC057,$FFFDD885,$FBC4E9FF,$9090FFFF,$90909090,$90909090,$0001B855,$E5890000,$000CC25D,$0026748D
  DD $D231C931,$8955C031,$400D89E5,$316B28A0,$441589C9,$316B28A0,$A048A3D2,$C0316B28,$A04C0D89,$15896B28
  DD $6B28A050,$28A054A3,$8DC35D6B,$000000B6,$27BC8D00,$00000000
end;

procedure DXT_CompressFPU;
asm
  // 3088 bytes dump
  DD $57E58955,$EC815356,$0000022C,$8B0C5D8B,$4B8D0845,$04508B30,$00A0BB8D,$408A0000,$DC958902,$88FFFFFD
  DD $FFFE4F85,$E88D89FF,$89FFFFFD,$FFFDECBD,$9083D9FF,$8D000000,$9DD92043,$FFFFFEAC,$009483D9,$BD830000
  DD $FFFFFDDC,$709DD900,$D9FFFFFE,$0000B083,$B09DD900,$D9FFFFFE,$938B0840,$000000B0,$FE9095D9,$40D9FFFF
  DD $9495D904,$D9FFFFFE,$95D92043,$FFFFFE98,$0ADE8E0F,$95D90000,$FFFFFE34,$458DC9D9,$E0B58D98,$D9FFFFFE
  DD $FFFE309D,$D9C0D9FF,$FFFE3085,$1C8589FF,$31FFFFFE,$CC9589C9,$89FFFFFE,$FFFEC89D,$10458BFF,$CBD902EB
  DD $0088948B,$89000001,$04E3C1D3,$01127C8D,$0195148D,$D9000000,$D90C1844,$C9D8F804,$D89004D9,$1C958BCA
  DD $D9FFFFFE,$CBD80304,$16D9CAD9,$56D9C9D9,$D9CAD9FC,$C6DEF856,$C683CBD9,$D9C1DE0C,$D9C2DECD,$C2DE8A14
  DD $DC8D3B41,$75FFFFFD,$D9C9D9A5,$FFFE189D,$8BC9D9FF,$FFFEC89D,$CC958BFF,$D9FFFFFE,$FFFE349D,$D9C9D9FF
  DD $FFFE309D,$2C9DD9FF,$D9FFFFFE,$9DD94043,$FFFFFE5C,$FE9885D9,$858AFFFF,$FFFFFE4F,$FDDC8D8B,$95D9FFFF
  DD $FFFFFE38,$FE9085D9,$F083FFFF,$D8BD8D01,$D9FFFFFE,$FFFE1495,$88C9D9FF,$FFFE2B85,$E48D89FF,$D9FFFFFD
  DD $FFFE0895,$9485D9FF,$89FFFFFE,$FFFDE0BD,$2085C7FF,$00FFFFFE,$D9000000,$FFFE1095,$C7CAD9FF,$FFFDF885
  DD $000000FF,$F485C700,$00FFFFFD,$D9000000,$FFFDFC95,$0C85C7FF,$00FFFFFE,$D9000000,$FFFE409D,$D9C9D9FF
  DD $FFFE3C95,$D9C9D9FF,$FFFE0095,$D9C9D9FF,$FFFE049D,$609DD9FF,$8BFFFFFE,$FFFDE4BD,$0FFF85FF,$00084B88
  DD $9485D900,$8BFFFFFE,$FFFE208D,$2485C7FF,$00FFFFFE,$D9000000,$FFFE549D,$9885D9FF,$8DFFFFFE,$95D94904
  DD $FFFFFE50,$FE9085D9,$848DFFFF,$FFFED885,$589DD9FF,$89FFFFFE,$FFFDF085,$809DD9FF,$80FFFFFE,$FFFE4FBD
  DD $840F00FF,$000007B8,$FE1885D9,$A5D8FFFF,$FFFFFE60,$FE4885C7,$0000FFFF,$A5D80000,$FFFFFE80,$FE9C9DD9
  DD $43D9FFFF,$E8858B30,$8BFFFFFD,$FFFE20B5,$449DD9FF,$D9FFFFFE,$BD8B0840,$FFFFFE24,$9DD9C931,$FFFFFE84
  DD $010440D9,$889DD9F7,$D9FFFFFE,$EC858B00,$89FFFFFD,$FFFDD4BD,$8C9DD9FF,$D9FFFFFE,$749DD900,$D9FFFFFE
  DD $9DD90440,$FFFFFE78,$8D0840D9,$9DD97F04,$FFFFFE7C,$FE9085D9,$B48DFFFF,$FFFED885,$6C9DD9FF,$D9FFFFFE
  DD $FFFE9485,$689DD9FF,$D9FFFFFE,$FFFE9885,$649DD9FF,$E9FFFFFE,$00000596,$FE8085D9,$4BD8FFFF,$6085D95C
  DD $D8FFFFFE,$C89DD9C1,$D9FFFFFE,$FFFE9C85,$D9C1D8FF,$FFFECC9D,$5043D9FF,$FE5085D9,$C9D8FFFF,$FE3885D8
  DD $9DD9FFFF,$FFFFFEB4,$FE5485D9,$C9D8FFFF,$FE3C85D9,$C1DEFFFF,$FEB89DD9,$8DD8FFFF,$FFFFFE58,$FE4085D9
  DD $C1DEFFFF,$FEBC9DD9,$85D9FFFF,$FFFFFEC8,$FECC8DD8,$C1D9FFFF,$E9DECAD8,$FE44BDD8,$9DD9FFFF,$FFFFFED0
  DD $FE2C85D9,$A5D8FFFF,$FFFFFEBC,$FEA89DD9,$85D9FFFF,$FFFFFE30,$FEB8A5D8,$95D9FFFF,$FFFFFEA4,$FE3485D9
  DD $A5D8FFFF,$FFFFFEB4,$FEA09DD9,$85D9FFFF,$FFFFFECC,$FEBC8DD8,$85D9FFFF,$FFFFFEA8,$E9DECBD8,$FED08DD8
  DD $85D9FFFF,$FFFFFECC,$FEB88DD8,$C2D9FFFF,$E9DECCD8,$FED08DD8,$85D9FFFF,$FFFFFECC,$FEB48DD8,$85D9FFFF
  DD $FFFFFEA0,$E9DECDD8,$FED08DD8,$85D9FFFF,$FFFFFEC8,$FEA88DD8,$85D9FFFF,$FFFFFEBC,$E9DECED8,$FED08DD8
  DD $85D9FFFF,$FFFFFEC8,$85D9CDDE,$FFFFFEB8,$EDDECED8,$8DD8CCD9,$FFFFFED0,$FEC885D9,$8DD8FFFF,$FFFFFEA0
  DD $FEB485D9,$CFD8FFFF,$8DD8E9DE,$FFFFFED0,$FE9085D9,$CDD9FFFF,$E0DFE5DD,$7445C4F6,$D9D8DD0F,$D9CAD9C9
  DD $EBCCD9CB,$00768D0D,$C9D9DDDD,$CBD9CAD9,$85D9CCD9,$FFFFFE94,$E5DDCDD9,$C4F6E0DF,$DD0C7445,$D9C9D9D8
  DD $D9CBD9CA,$DD0AEBCC,$D9C9D9DD,$D9CBD9CA,$9885D9CC,$D9FFFFFE,$DFE5DDCD,$45C4F6E0,$D8DD0E74,$CAD9C9D9
  DD $CCD9CBD9,$90660CEB,$C9D9DDDD,$CBD9CAD9,$85D9CCD9,$FFFFFE84,$E0DFE3DD,$7445C4F6,$D9DBDD0C,$D9CAD9C9
  DD $EBC9D9CA,$D9D8DD02,$FFFE8885,$DFE2DDFF,$45C4F6E0,$DADD0A74,$C9D9C9D9,$906604EB,$85D9D8DD,$FFFFFE8C
  DD $E0DFE1DD,$7445C4F6,$EBD9DD04,$D9D8DD02,$FFFE9085,$DDCED9FF,$F6E0DFE6,$0E7445C4,$C9D9D8DD,$CBD9CAD9
  DD $CDD9CCD9,$DEDD0CEB,$CAD9C9D9,$CCD9CBD9,$85D9CDD9,$FFFFFE94,$E6DDCED9,$C4F6E0DF,$DD0E7445,$D9C9D9D8
  DD $D9CBD9CA,$EBCDD9CC,$D9DEDD0C,$D9CAD9C9,$D9CCD9CB,$9885D9CD,$D9FFFFFE,$DFE6DDCE,$45C4F6E0,$D8DD1274
  DD $CAD9C9D9,$CCD9CBD9,$10EBCDD9,$0026748D,$C9D9DEDD,$CBD9CAD9,$CDD9CCD9,$FE8485D9,$E3DDFFFF,$C4F6E0DF
  DD $DD0C7445,$D9C9D9DB,$D9CAD9CA,$DD02EBC9,$8885D9D8,$DDFFFFFE,$F6E0DFE2,$087445C4,$C9D9DADD,$02EBC9D9
  DD $85D9D8DD,$FFFFFE8C,$E0DFE1DD,$7445C4F6,$D9D9DD06,$DD04EBCB,$D8CBD9D8,$FFFEAC8D,$B085D8FF,$D9FFFFFE
  DD $CBD9D85D,$FE708DD8,$458BFFFF,$89D029D8,$FFFED485,$B085D8FF,$D9FFFFFE,$CBD9DC5D,$FEAC8DD8,$458BFFFF
  DD $D8D029DC,$FFFEB085,$E05DD9FF,$FED485DB,$8DD8FFFF,$FFFFFE74,$FED48589,$458BFFFF,$D9D029E0,$FFFEC09D
  DD $D485DBFF,$D8FFFFFE,$FFFE788D,$D48589FF,$D9FFFFFE,$FFFEC495,$D485DBFF,$D8FFFFFE,$FFFE7C8D,$D09DD9FF
  DD $D9FFFFFE,$AC8DD8C9,$D8FFFFFE,$FFFEB085,$D85DD9FF,$8DD8C9D9,$FFFFFE70,$29D8458B,$B085D8D0,$D9FFFFFE
  DD $C9D9DC5D,$FEAC8DD8,$85D8FFFF,$FFFFFEB0,$89E05DD9,$FFFED485,$DC458BFF,$FED485DB,$8DD8FFFF,$FFFFFE74
  DD $8589D029,$FFFFFED4,$29E0458B,$D485DBD0,$D8FFFFFE,$FFFE788D,$D48589FF,$DBFFFFFE,$FFFED485,$7C8DD8FF
  DD $D9FFFFFE,$FFFED085,$D8C9D8FF,$D085D9CD,$D8FFFFFE,$FFFEBC8D,$D9E9DEFF,$FFFEA885,$DECAD8FF,$D8C4D9E9
  DD $D9CED8CB,$FFFEB885,$DECEDEFF,$A485D9E5,$D8FFFFFE,$D9EDDECB,$FFFEC085,$DECCD8FF,$C085D9CE,$D8FFFFFE
  DD $FFFEB48D,$D9EEDEFF,$FFFEA085,$DECCD8FF,$D8C1D9EE,$CC8DD8CA,$D9FFFFFE,$FFFED085,$D8C8D8FF,$FFFEC88D
  DD $D8C1DEFF,$D9C1DEC1,$D8CCD8C3,$FFFECC8D,$C085D9FF,$D8FFFFFE,$C88DD8C8,$DEFFFFFE,$DEC6D8C1,$D8C2D9C6
  DD $CC8DD8CB,$D9FFFFFE,$FFFEC485,$D8C8D8FF,$FFFEC88D,$D8C1DEFF,$D9C5DEC5,$D9C4DECD,$D9C3DECC,$FFFE5C85
  DD $DFEBDDFF,$45C4F6E0,$CBD96675,$FE149DD9,$CAD9FFFF,$FE24858B,$BD8BFFFF,$FFFFFE20,$FE049DD9,$C9D9FFFF
  DD $FDF88D89,$8589FFFF,$FFFFFDF4,$FE009DD9,$85D9FFFF,$FFFFFED0,$FE0CBD89,$9DD9FFFF,$FFFFFDFC,$FEC485D9
  DD $9DD9FFFF,$FFFFFE10,$FEC085D9,$9DD9FFFF,$FFFFFE08,$FE5C9DD9,$0CEBFFFF,$0026748D,$D8DDD8DD,$D8DDD8DD
  DD $FE2BBD80,$7400FFFF,$6485D949,$D8FFFFFE,$D4858B06,$01FFFFFD,$1CBD8BC8,$D9FFFFFE,$FFFE649D,$6885D9FF
  DD $D8FFFFFE,$9DD90446,$FFFFFE68,$FE6C85D9,$46D8FFFF,$6C9DD908,$D9FFFFFE,$FFFE9C85,$8704D8FF,$FE9C9DD9
  DD $8341FFFF,$8D390CC6,$FFFFFE48,$00EC8C0F,$BD800000,$FFFFFE4F,$5D850F00,$D9FFFFFA,$FFFE8085,$9C85D8FF
  DD $D8FFFFFE,$0000808B,$7C43D900,$D96C43D9,$FFFE8085,$D8CAD8FF,$FFFE6085,$C89DD9FF,$D9FFFFFE,$FFFE9C85
  DD $D8C9D8FF,$FFFEC885,$C89DD9FF,$D9FFFFFE,$FFFE1885,$60A5D8FF,$D8FFFFFE,$FFFE80A5,$9CA5D8FF,$D9FFFFFE
  DD $9C8DD8CA,$DEFFFFFE,$808DD8C2,$DEFFFFFE,$CC9DD9C1,$D9FFFFFE,$43D96043,$6485D970,$D8FFFFFE,$5085D9CA
  DD $D8FFFFFE,$3885D8CA,$DEFFFFFE,$B49DD9C1,$D9FFFFFE,$FFFE6885,$D9CAD8FF,$FFFEB89D,$5485D9FF,$D8FFFFFE
  DD $3C85D8C9,$D8FFFFFE,$FFFEB885,$B89DD9FF,$D9FFFFFE,$6C8DD8C9,$D9FFFFFE,$588DD8C9,$D8FFFFFE,$FFFE4085
  DD $D9C1DEFF,$FFFEBC9D,$F9E4E9FF,$BD8BFFFF,$FFFFFDD4,$FDF0858B,$8D8BFFFF,$FFFFFE1C,$40D900D9,$0840D904
  DD $8BB904D9,$FFFE248D,$0CC083FF,$E4BD8B41,$89FFFFFD,$FFFE248D,$F08589FF,$39FFFFFD,$FFFE24BD,$D9757FFF
  DD $5085D8CB,$80FFFFFE,$FFFE4FBD,$9DD900FF,$FFFFFE50,$85D8C9D9,$FFFFFE54,$FE549DD9,$85D8FFFF,$FFFFFE58
  DD $FE589DD9,$85D8FFFF,$FFFFFE80,$FE809DD9,$850FFFFF,$FFFFF848,$FE248D8B,$858BFFFF,$FFFFFE20,$8B013C8D
  DD $FFFDE485,$89C829FF,$FFFE4885,$61880FFF,$D9FFFFFF,$FFFE9885,$9C9DD9FF,$E9FFFFFE,$FFFFF836,$D8DDD8DD
  DD $D8DDD8DD,$FDE0858B,$8D8BFFFF,$FFFFFE20,$FE1CBD8B,$00D9FFFF,$D90440D9,$04D90840,$E4BD8B8F,$41FFFFFD
  DD $0CC0834F,$FE208D89,$BD89FFFF,$FFFFFDE4,$FDE08589,$8D39FFFF,$FFFFFDDC,$CBD9397C,$FE3885D8,$9DD9FFFF
  DD $FFFFFE38,$85D8C9D9,$FFFFFE3C,$FE3C9DD9,$85D8FFFF,$FFFFFE40,$FE409DD9,$85D8FFFF,$FFFFFE60,$FE609DD9
  DD $2CE9FFFF,$DDFFFFF7,$DDD8DDD8,$8BD8DDD8,$FFFDF485,$F88D8BFF,$03FFFFFD,$FFFE0C85,$08148DFF,$8B105D8B
  DD $FFFE08BD,$80BB89FF,$89000001,$0001A483,$108D8B00,$89FFFFFE,$0001A893,$848B8900,$8B000001,$FFFDFCBD
  DD $88BB89FF,$D9000001,$FFFE9885,$8C93D9FF,$8B000001,$FFFE008D,$908B89FF,$8B000001,$FFFE04BD,$94BB89FF
  DD $8B000001,$FFFE148D,$9C9BD9FF,$89000001,$0001988B,$0CBD8B00,$89FFFFFE,$0001A0BB,$2CC48100,$5B000002
  DD $C35D5F5E,$D8DDD8DD,$43D9D8DD,$5C9DD940,$74FFFFFE,$9085D955,$C7FFFFFE,$FFFE0C85,$000000FF,$149DD900
  DD $D9FFFFFE,$FFFE9885,$14858BFF,$D9FFFFFE,$FFFE0895,$9485D9FF,$89FFFFFE,$FFFDFC85,$D9C031FF,$FFFE109D
  DD $10958BFF,$89FFFFFE,$FFFE0495,$D9D231FF,$FFFE009D,$FF1AE9FF,$85D9FFFF,$FFFFFE90,$D998458D,$FFFE2C9D
  DD $9885D9FF,$89FFFFFE,$FFFE1C85,$3495D9FF,$D9FFFFFE,$FFFE9485,$309DD9FF,$D9FFFFFE,$FFFE189D,$F54AE9FF
  DD $B68DFFFF,$00000000
end;

procedure DXT_AlphaCompress;
asm
  // 8560 bytes dump
  DD $89D23155,$C0E483E5,$5657C931,$81DB3153,$0002B4EC,$0C458B00,$482484C7,$00000002,$C7000000,$024C2484
  DD $00000000,$84C70000,$00025024,$FFFFFF00,$03508A0F,$8911E2C1,$00D82494,$94890000,$00014024,$07488A00
  DD $E1C1D231,$248C8911,$000000D4,$44248C89,$8A000001,$C9310B58,$8911E3C1,$00E4249C,$9C890000,$00014824
  DD $0F508A00,$E2C1DB31,$24948911,$000000E0,$4C249489,$8A000001,$D2311348,$8911E1C1,$00DC248C,$8C890000
  DD $00015024,$17588A00,$E3C1C931,$249C8911,$00000110,$54249C89,$8A000001,$DB311B50,$8911E2C1,$010C2494
  DD $94890000,$00015824,$1F488A00,$E1C1D231,$248C8911,$00000108,$5C248C89,$8A000001,$C9312358,$8911E3C1
  DD $0104249C,$9C890000,$00016024,$27508A00,$E2C1DB31,$24948911,$00000100,$64249489,$8A000001,$D2312B48
  DD $8911E1C1,$00FC248C,$8C890000,$00016824,$2F588A00,$E3C1C931,$249C8911,$000000F8,$6C249C89,$8A000001
  DD $DB313350,$8911E2C1,$00F42494,$94890000,$00017024,$37488A00,$E1C1D231,$248C8911,$000000F0,$74248C89
  DD $8A000001,$8C8B3B58,$0000D424,$11E3C100,$EC249C89,$89000000,$0178249C,$508A0000,$2444C73F,$FFFFFF70
  DD $11E2C10F,$742444C7,$00000000,$D0249489,$89000000,$017C2494,$E9C10000,$11EAC111,$A8248C89,$89000000
  DD $00902494,$548B0000,$848B7424,$0000D824,$0FD28500,$EB248494,$C1000000,$BC8011E8,$0000EB24,$13740000
  DD $0000FF3D,$14840F00,$8500000F,$0C840FC0,$8500000F,$6A8E0FC0,$8900001F,$00FE3DC2,$077E0000,$FFB8C289
  DD $80000000,$00EB24BC,$0F000000,$000EED85,$24943B00,$000000A8,$948B077D,$0000A824,$24843B00,$000000A8
  DD $848B077E,$0000A824,$248C8B00,$000000E4,$3911E9C1,$89027DCA,$7EC839CA,$8BC88902,$00E0248C,$E9C10000
  DD $7DCA3911,$39CA8902,$89027EC8,$248C8BC8,$000000DC,$3911E9C1,$89027DCA,$7EC839CA,$8BC88902,$0110248C
  DD $E9C10000,$7DCA3911,$39CA8902,$89027EC8,$248C8BC8,$0000010C,$3911E9C1,$89027DCA,$7EC839CA,$8BC88902
  DD $0108248C,$E9C10000,$7DCA3911,$39CA8902,$89027EC8,$248C8BC8,$00000104,$3911E9C1,$89027DCA,$7EC839CA
  DD $8BC88902,$0100248C,$E9C10000,$7DCA3911,$39CA8902,$89027EC8,$248C8BC8,$000000FC,$3911E9C1,$89027DCA
  DD $7EC839CA,$8BC88902,$00F8248C,$E9C10000,$7DCA3911,$39CA8902,$89027EC8,$248C8BC8,$000000F4,$3911E9C1
  DD $89027DCA,$7EC839CA,$8BC88902,$00F0248C,$E9C10000,$7DCA3911,$39CA8902,$89027EC8,$248C8BC8,$000000EC
  DD $3911E9C1,$89027DCA,$7EC839CA,$3BC88902,$00902494,$077D0000,$9024948B,$3B000000,$00902484,$077E0000
  DD $9024848B,$8A000000,$00EB249C,$84C70000,$00008824,$FFFFFF00,$FFE3810F,$C7000000,$00842484,$00000000
  DD $9C890000,$0000B424,$29D38900,$2484C7C3,$00000080,$00000000,$D989DE89,$C704E6C1,$008C2484,$00000000
  DD $E1C10000,$8DF12908,$020824B4,$D9010000,$38247489,$C103E1C1,$CA2910F9,$8901048D,$897C2454,$8B782444
  DD $008C2484,$C0850000,$160C850F,$BC800000,$0000EB24,$850F0000,$0000194C,$7C247C8B,$7824448B,$A424BC89
  DD $89000000,$03EA83FA,$89FD588D,$01202494,$8C8B0000,$0000A424,$24848900,$00000128,$D824B48B,$8B000000
  DD $00E424BC,$848B0000,$0000E024,$24948B00,$000000DC,$C104C183,$8C8911E8,$0000B024,$11EEC100,$98248489
  DD $C1000000,$84C711EF,$00013024,$FFFFFF00,$11EAC10F,$AC249C89,$89000000,$00A024B4,$BC890000,$00009C24
  DD $24948900,$00000094,$948BD889,$00012024,$FFFDB900,$BC81FFFF,$00012024,$0000FF00,$84970F00,$00011824
  DD $FFE28100,$890000FF,$8934244C,$10E3C1D3,$C085D309,$14249C89,$89000001,$1D880FC3,$80000009,$011824BC
  DD $0F000000,$00090F85,$00FF3D00,$8F0F0000,$00000904,$2024843B,$0F000001,$0008F784,$C29C0F00,$00FFE281
  DD $943B0000,$0000B424,$E1840F00,$89000008,$FFE681C6,$890000FF,$10E2C1F2,$248C6E0F,$00000114,$620FF209
  DD $D26E0FC9,$89C16F0F,$0F3C2454,$BC80D262,$0000EB24,$840F0000,$00000F82,$0F10558B,$6F0FDA6F,$0000C88A
  DD $92D50F00,$000000D0,$0FC8D50F,$00E09AD5,$D50F0000,$0000D882,$CAED0F00,$30926F0F,$0F000001,$E50FC3ED
  DD $82EB0FC2,$00000128,$0FCAE50F,$6F0FD96F,$D16F0FE0,$30D3730F,$10F4730F,$10F2730F,$0FE3EB0F,$ED0FD1ED
  DD $24BC80E0,$000000EB,$01840F00,$8B00000F,$EB0F1075,$0001088E,$86EB0F00,$00000110,$0FD96F0F,$610FDA69
  DD $9C7F0FCA,$0000C024,$D06F0F00,$248C7F0F,$000000C8,$C024B48B,$8B000000,$00C424BC,$610F0000,$947F0FD4
  DD $00013824,$24948B00,$000000C8,$89C4690F,$018824B4,$BC890000,$00018C24,$24B48B00,$00000138,$3C24BC8B
  DD $0F000001,$B824847F,$8B000000,$00CC248C,$94890000,$00018024,$24B48900,$00000190,$BC89F289,$00019424
  DD $24B48B00,$000000B8,$BC24BC8B,$89000000,$019824B4,$8C890000,$00018424,$24BC8900,$0000019C,$9439D689
  DD $0000D824,$5D820F00,$8B00000A,$019824BC,$BC390000,$0000D824,$B9820F00,$8B000011,$019C24B4,$B4390000
  DD $0000D824,$5D820F00,$9000000A,$0026748D,$BC8BF189,$0000A024,$08EEC100,$00FFE181,$29660000,$66F189CF
  DD $A024BC89,$88000001,$01C0248C,$D6890000,$D4249439,$0F000000,$0009CB82,$24BC8B00,$00000198,$D424BC39
  DD $0F000000,$00119782,$24B48B00,$0000019C,$D424B439,$0F000000,$0009CB82,$00768D00,$8C8BF789,$0000A824
  DD $08EEC100,$00FFE781,$29660000,$249439F9,$000000E4,$248C8966,$000001A2,$8C88F189,$0001C124,$0FD68900
  DD $00093B82,$24BC8B00,$00000198,$E424BC39,$0F000000,$00117782,$24B48B00,$0000019C,$E424B439,$0F000000
  DD $00093B82,$00768D00,$8C8BF789,$00009C24,$08EEC100,$00FFE781,$29660000,$249439F9,$000000E0,$248C8966
  DD $000001A4,$8C88F189,$0001C224,$0FD68900,$0008AB82,$24BC8B00,$00000198,$E024BC39,$0F000000,$00105782
  DD $24B48B00,$0000019C,$E024B439,$0F000000,$0008AB82,$00768D00,$8C8BF789,$00009824,$08EEC100,$00FFE781
  DD $29660000,$249439F9,$000000DC,$248C8966,$000001A6,$8C88F189,$0001C324,$0FD68900,$00081B82,$24BC8B00
  DD $00000198,$DC24BC39,$0F000000,$000FB782,$24B48B00,$0000019C,$DC24B439,$0F000000,$00081B82,$00768D00
  DD $8C8BF789,$00009424,$08EEC100,$00FFE781,$29660000,$249439F9,$00000110,$248C8966,$000001A8,$8C88F189
  DD $0001C424,$0FD68900,$00078B82,$24BC8B00,$00000198,$1024BC39,$0F000001,$000E9782,$24B48B00,$0000019C
  DD $1024B439,$0F000001,$00078B82,$00768D00,$1024BC8B,$C1000001,$BC8911EF,$00013824,$81F78900,$0000FFE7
  DD $248C8B00,$00000138,$C1F92966,$896608EE,$01AA248C,$F1890000,$0C249439,$88000001,$01C5248C,$D6890000
  DD $06EA820F,$BC8B0000,$00019824,$24BC3900,$0000010C,$0E66820F,$B48B0000,$00019C24,$24B43900,$0000010C
  DD $06EA820F,$90660000,$0C24BC8B,$C1000001,$BC8911EF,$00013824,$81F78900,$0000FFE7,$248C8B00,$00000138
  DD $C1F92966,$896608EE,$01AC248C,$F1890000,$08249439,$88000001,$01C6248C,$D6890000,$064A820F,$BC8B0000
  DD $00019824,$24BC3900,$00000108,$0E36820F,$B48B0000,$00019C24,$24B43900,$00000108,$064A820F,$90660000
  DD $0824BC8B,$C1000001,$BC8911EF,$00013824,$81F78900,$0000FFE7,$248C8B00,$00000138,$C1F92966,$896608EE
  DD $01AE248C,$F1890000,$04249439,$88000001,$01C7248C,$D6890000,$05AA820F,$BC8B0000,$00019824,$24BC3900
  DD $00000104,$0C86820F,$B48B0000,$00019C24,$24B43900,$00000104,$05AA820F,$90660000,$0424BC8B,$C1000001
  DD $BC8911EF,$00013824,$81F78900,$0000FFE7,$248C8B00,$00000138,$C1F92966,$896608EE,$01B0248C,$F1890000
  DD $00249439,$88000001,$01C8248C,$D6890000,$050A820F,$BC8B0000,$00019824,$24BC3900,$00000100,$0C56820F
  DD $B48B0000,$00019C24,$24B43900,$00000100,$050A820F,$90660000,$0024BC8B,$C1000001,$BC8911EF,$00013824
  DD $81F78900,$0000FFE7,$248C8B00,$00000138,$C1F92966,$896608EE,$01B2248C,$F1890000,$FC249439,$88000000
  DD $01C9248C,$D6890000,$046A820F,$BC8B0000,$00019824,$24BC3900,$000000FC,$0C26820F,$B48B0000,$00019C24
  DD $24B43900,$000000FC,$046A820F,$90660000,$FC24BC8B,$C1000000,$BC8911EF,$00013824,$81F78900,$0000FFE7
  DD $248C8B00,$00000138,$C1F92966,$896608EE,$01B4248C,$F1890000,$F8249439,$88000000,$01CA248C,$D6890000
  DD $03CA820F,$BC8B0000,$00019824,$24BC3900,$000000F8,$0AF6820F,$B48B0000,$00019C24,$24B43900,$000000F8
  DD $03CA820F,$90660000,$F824BC8B,$C1000000,$BC8911EF,$00013824,$81F78900,$0000FFE7,$248C8B00,$00000138
  DD $C1F92966,$896608EE,$01B6248C,$F1890000,$F4249439,$88000000,$01CB248C,$D6890000,$032A820F,$BC8B0000
  DD $00019824,$24BC3900,$000000F4,$0A46820F,$B48B0000,$00019C24,$24B43900,$000000F4,$032A820F,$90660000
  DD $F424BC8B,$C1000000,$BC8911EF,$00013824,$81F78900,$0000FFE7,$248C8B00,$00000138,$C1F92966,$896608EE
  DD $01B8248C,$F1890000,$F0249439,$88000000,$01CC248C,$D6890000,$025A820F,$BC8B0000,$00019824,$24BC3900
  DD $000000F0,$0956820F,$B48B0000,$00019C24,$24B43900,$000000F0,$025A820F,$90660000,$F024BC8B,$C1000000
  DD $BC8911EF,$00013824,$81F78900,$0000FFE7,$248C8B00,$00000138,$C1F92966,$896608EE,$01BA248C,$F1890000
  DD $EC249439,$88000000,$01CD248C,$D6890000,$021A820F,$BC8B0000,$00019824,$24BC3900,$000000EC,$0926820F
  DD $B48B0000,$00019C24,$24B43900,$000000EC,$021A820F,$90660000,$EC24BC8B,$C1000000,$BC8911EF,$00013824
  DD $81F78900,$0000FFE7,$248C8B00,$00000138,$C1F92966,$896608EE,$01BC248C,$F1890000,$D0249439,$88000000
  DD $01CE248C,$820F0000,$0000014C,$9824B48B,$39000001,$00D024B4,$820F0000,$0000085B,$9C24948B,$39000001
  DD $00D02494,$820F0000,$0000014C,$0026748D,$C1F2B60F,$BC8B08EA,$00009024,$F7296600,$CF249488,$66000001
  DD $BE24BC89,$0F000001,$B824846F,$0F000001,$F50FD06F,$846F0FD0,$0001B024,$C86F0F00,$0FC8F50F,$A824846F
  DD $0F000001,$6F0FCAFE,$D0F50FD0,$24846F0F,$000001A0,$0FC0F50F,$FE0FD0FE,$8C7F0FCA,$00013824,$24BC8B00
  DD $0000013C,$6C24BC89,$8B000002,$01382494,$8C8B0000,$00026C24,$24B48B00,$00000138,$B489CA01,$00026824
  DD $24943B00,$00000130,$B48B767D,$0001C024,$24BC8B00,$00000120,$1824B489,$8B000002,$01C424B4,$B4890000
  DD $00021C24,$24B48B00,$000001C8,$2024B489,$8B000002,$01CC24B4,$B4890000,$00022424,$24BC8900,$000000A4
  DD $34247C8B,$7C894740,$7C833424,$0F043424,$00069B84,$249C8900,$00000128,$30249489,$89000001,$0FC085C3
  DD $FFF6E689,$00768DFF,$28249C8B,$8B000001,$01302494,$C0EB0000,$8824948B,$39000001,$00D02494,$820F0000
  DD $0000090C,$8C24B48B,$39000001,$00D024B4,$820F0000,$FFFFFEB8,$B1E9F289,$90FFFFFE,$8824B48B,$39000001
  DD $00F024B4,$820F0000,$000009DC,$8C24BC8B,$39000001,$00F024BC,$820F0000,$FFFFFDA8,$A1E9FE89,$90FFFFFD
  DD $8824B48B,$39000001,$00EC24B4,$820F0000,$0000092C,$8C24BC8B,$39000001,$00EC24BC,$820F0000,$FFFFFDE8
  DD $E1E9FE89,$90FFFFFD,$8824B48B,$39000001,$00F424B4,$820F0000,$000008BC,$8C24BC8B,$39000001,$00F424BC
  DD $820F0000,$FFFFFCD8,$D1E9FE89,$90FFFFFC,$8824B48B,$39000001,$00F824B4,$820F0000,$000009CC,$8C24BC8B
  DD $39000001,$00F824BC,$820F0000,$FFFFFC38,$31E9FE89,$90FFFFFC,$8824B48B,$39000001,$00FC24B4,$820F0000
  DD $000008DC,$8C24BC8B,$39000001,$00FC24BC,$820F0000,$FFFFFB98,$91E9FE89,$90FFFFFB,$8824B48B,$39000001
  DD $010024B4,$820F0000,$0000076C,$8C24BC8B,$39000001,$010024BC,$820F0000,$FFFFFAF8,$F1E9FE89,$90FFFFFA
  DD $8824B48B,$39000001,$010424B4,$820F0000,$000008FC,$8C24BC8B,$39000001,$010424BC,$820F0000,$FFFFFA58
  DD $51E9FE89,$90FFFFFA,$8824B48B,$39000001,$010824B4,$820F0000,$0000068C,$8C24BC8B,$39000001,$010824BC
  DD $820F0000,$FFFFF9B8,$B1E9FE89,$90FFFFF9,$8824B48B,$39000001,$010C24B4,$820F0000,$0000069C,$8C24BC8B
  DD $39000001,$010C24BC,$820F0000,$FFFFF918,$11E9FE89,$90FFFFF9,$8824B48B,$39000001,$011024B4,$820F0000
  DD $000005AC,$8C24BC8B,$39000001,$011024BC,$820F0000,$FFFFF878,$71E9FE89,$90FFFFF8,$8824B48B,$39000001
  DD $00DC24B4,$820F0000,$000008BC,$8C24BC8B,$39000001,$00DC24BC,$820F0000,$FFFFF7E8,$E1E9FE89,$90FFFFF7
  DD $8824B48B,$39000001,$00E024B4,$820F0000,$0000058C,$8C24BC8B,$39000001,$00E024BC,$820F0000,$FFFFF758
  DD $51E9FE89,$90FFFFF7,$8824B48B,$39000001,$00E424B4,$820F0000,$0000047F,$8C24BC8B,$39000001,$00E424BC
  DD $820F0000,$FFFFF6C8,$C1E9FE89,$90FFFFF6,$8824B48B,$39000001,$00D424B4,$820F0000,$000004AC,$8C24BC8B
  DD $39000001,$00D424BC,$820F0000,$FFFFF638,$31E9FE89,$90FFFFF6,$8824B48B,$39000001,$00D824B4,$820F0000
  DD $000005FC,$8C24BC8B,$39000001,$00D824BC,$820F0000,$FFFFF5A8,$A1E9FE89,$B8FFFFF5,$000000FF,$BC81D231
  DD $0000A824,$0000FF00,$83437400,$00A824BC,$74000000,$24943B39,$000000A8,$948B077D,$0000A824,$24843B00
  DD $000000A8,$848B077E,$0000A824,$248C8B00,$000000E4,$8011E9C1,$00EB24BC,$75000000,$F0EDE90F,$8C8BFFFF
  DD $0000E424,$11E9C100,$00FFF981,$29740000,$2574C985,$027DCA39,$C839CA89,$C889027E,$E0248C8B,$C1000000
  DD $BC8011E9,$0000EB24,$0F750000,$FFF0C8E9,$248C8BFF,$000000E0,$8111E9C1,$0000FFF9,$85297400,$392574C9
  DD $89027DCA,$7EC839CA,$8BC88902,$00DC248C,$E9C10000,$24BC8011,$000000EB,$E90F7500,$FFFFF0A3,$DC248C8B
  DD $C1000000,$F98111E9,$000000FF,$C9852974,$CA392574,$CA89027D,$027EC839,$8C8BC889,$00011024,$11E9C100
  DD $EB24BC80,$00000000,$7EE90F75,$8BFFFFF0,$0110248C,$E9C10000,$FFF98111,$74000000,$74C98529,$7DCA3925
  DD $39CA8902,$89027EC8,$248C8BC8,$0000010C,$8011E9C1,$00EB24BC,$75000000,$F059E90F,$8C8BFFFF,$00010C24
  DD $11E9C100,$00FFF981,$29740000,$2574C985,$027DCA39,$C839CA89,$C889027E,$08248C8B,$C1000001,$BC8011E9
  DD $0000EB24,$0F750000,$FFF034E9,$248C8BFF,$00000108,$8111E9C1,$0000FFF9,$85297400,$392574C9,$89027DCA
  DD $7EC839CA,$8BC88902,$0104248C,$E9C10000,$24BC8011,$000000EB,$E90F7500,$FFFFF00F,$04248C8B,$C1000001
  DD $F98111E9,$000000FF,$C9852974,$CA392574,$CA89027D,$027EC839,$8C8BC889,$00010024,$11E9C100,$EB24BC80
  DD $00000000,$EAE90F75,$8BFFFFEF,$0100248C,$E9C10000,$FFF98111,$74000000,$74C98529,$7DCA3925,$39CA8902
  DD $89027EC8,$248C8BC8,$000000FC,$8011E9C1,$00EB24BC,$75000000,$EFC5E90F,$8C8BFFFF,$0000FC24,$11E9C100
  DD $00FFF981,$29740000,$2574C985,$027DCA39,$C839CA89,$C889027E,$F8248C8B,$C1000000,$BC8011E9,$0000EB24
  DD $0F750000,$FFEFA0E9,$248C8BFF,$000000F8,$8111E9C1,$0000FFF9,$85297400,$392574C9,$89027DCA,$7EC839CA
  DD $8BC88902,$00F4248C,$E9C10000,$24BC8011,$000000EB,$E90F7500,$FFFFEF7B,$F4248C8B,$C1000000,$F98111E9
  DD $000000FF,$C9852974,$CA392574,$CA89027D,$027EC839,$8C8BC889,$0000F024,$11E9C100,$EB24BC80,$00000000
  DD $56E90F75,$8BFFFFEF,$00F0248C,$E9C10000,$FFF98111,$74000000,$74C98529,$7DCA3925,$39CA8902,$89027EC8
  DD $248C8BC8,$000000EC,$8011E9C1,$00EB24BC,$75000000,$EF31E90F,$8C8BFFFF,$0000EC24,$11E9C100,$00FFF981
  DD $1E740000,$1A74C985,$027DCA39,$C839CA89,$C889027E,$EB24BC80,$00000000,$EF0D840F,$BC81FFFF,$00009024
  DD $0000FF00,$1C840F00,$83FFFFEF,$009024BC,$0F000000,$FFEEEE85,$EF09E9FF,$9066FFFF,$2024B48B,$8B000001
  DD $00B02484,$89460000,$012024B4,$84390000,$00012024,$6D840F00,$89000004,$0128249C,$94890000,$00013024
  DD $24848B00,$000000AC,$FFEFE5E9,$00768DFF,$0F107D8B,$01188FEB,$EB0F0000,$00012087,$F0FAE900,$4D8BFFFF
  DD $DA6F0F10,$E8896F0F,$0F000000,$00F891D5,$D50F0000,$99D50FC8,$00000100,$F081D50F,$0F000000,$6F0FCAED
  DD $00013891,$C3ED0F00,$0FCAE50F,$80E9C2E5,$8BFFFFF0,$018424B4,$B4390000,$0000E424,$49830F00,$8BFFFFF2
  DD $018024B4,$3DE90000,$8BFFFFF2,$019424B4,$B4390000,$0000D024,$A9820F00,$89FFFFF7,$F7A2E9F2,$9066FFFF
  DD $9424BC8B,$39000001,$00F024BC,$820F0000,$FFFFF6AC,$A5E9FE89,$90FFFFF6,$0026748D,$8424B48B,$39000001
  DD $00D424B4,$830F0000,$FFFFF18C,$8024B48B,$E9000001,$FFFFF180,$9424BC8B,$39000001,$00EC24BC,$820F0000
  DD $FFFFF6DC,$D5E9FE89,$90FFFFF6,$0026748D,$8424B48B,$39000001,$011024B4,$830F0000,$FFFFF2CC,$8024B48B
  DD $E9000001,$FFFFF2C0,$9424BC8B,$39000001,$00F424BC,$820F0000,$FFFFF5BC,$B5E9FE89,$90FFFFF5,$0026748D
  DD $8424B48B,$39000001,$00E024B4,$830F0000,$FFFFF1CC,$8024B48B,$E9000001,$FFFFF1C0,$9424BC8B,$39000001
  DD $00F824BC,$820F0000,$FFFFF50C,$05E9FE89,$90FFFFF5,$0026748D,$8424B48B,$39000001,$010824B4,$830F0000
  DD $FFFFF32C,$8024B48B,$E9000001,$FFFFF320,$9424BC8B,$39000001,$010424BC,$820F0000,$FFFFF37C,$75E9FE89
  DD $90FFFFF3,$0026748D,$8424B48B,$39000001,$010C24B4,$830F0000,$FFFFF27C,$8024B48B,$E9000001,$FFFFF270
  DD $9424BC8B,$39000001,$010024BC,$820F0000,$FFFFF3AC,$A5E9FE89,$90FFFFF3,$0026748D,$8424B48B,$39000001
  DD $010024B4,$830F0000,$FFFFF38C,$8024B48B,$E9000001,$FFFFF380,$9424BC8B,$39000001,$00FC24BC,$820F0000
  DD $FFFFF3DC,$D5E9FE89,$90FFFFF3,$0026748D,$8424B48B,$39000001,$00D824B4,$830F0000,$FFFFEFAC,$8024B48B
  DD $E9000001,$FFFFEFA0,$9424BC8B,$39000001,$011024BC,$820F0000,$FFFFF16C,$65E9FE89,$90FFFFF1,$0026748D
  DD $8424948B,$39000001,$00D02494,$830F0000,$FFFFF5AC,$8024948B,$E9000001,$FFFFF5A0,$9424BC8B,$39000001
  DD $010C24BC,$820F0000,$FFFFF19C,$95E9FE89,$90FFFFF1,$0026748D,$8424B48B,$39000001,$00F424B4,$830F0000
  DD $FFFFF41C,$8024B48B,$E9000001,$FFFFF410,$9424BC8B,$39000001,$010824BC,$820F0000,$FFFFF1CC,$C5E9FE89
  DD $90FFFFF1,$0026748D,$8424B48B,$39000001,$00EC24B4,$830F0000,$FFFFF4BC,$8024B48B,$E9000001,$FFFFF4B0
  DD $9424BC8B,$39000001,$00DC24BC,$820F0000,$FFFFF04C,$45E9FE89,$90FFFFF0,$0026748D,$8424B48B,$39000001
  DD $00FC24B4,$830F0000,$FFFFF2BC,$8024B48B,$E9000001,$FFFFF2B0,$9424BC8B,$39000001,$00E024BC,$820F0000
  DD $FFFFEFAC,$A5E9FE89,$90FFFFEF,$0026748D,$8424B48B,$39000001,$00F024B4,$830F0000,$FFFFF3CC,$8024B48B
  DD $E9000001,$FFFFF3C0,$9424BC8B,$39000001,$00D824BC,$820F0000,$FFFFEE4C,$45E9FE89,$90FFFFEE,$0026748D
  DD $8424B48B,$39000001,$010424B4,$830F0000,$FFFFF15C,$8024B48B,$E9000001,$FFFFF150,$9424BC8B,$39000001
  DD $00D424BC,$820F0000,$FFFFEE6C,$65E9FE89,$90FFFFEE,$0026748D,$8424B48B,$39000001,$00F824B4,$830F0000
  DD $FFFFF26C,$8024B48B,$E9000001,$FFFFF260,$9424BC8B,$39000001,$00E424BC,$820F0000,$FFFFEE8C,$85E9FE89
  DD $90FFFFEE,$0026748D,$8424B48B,$39000001,$00DC24B4,$830F0000,$FFFFEF2C,$8024B48B,$E9000001,$FFFFEF20
  DD $A424B48B,$39000000,$008424B4,$840F0000,$00000428,$8824943B,$7D000000,$24BC8B58,$000000A4,$10249489
  DD $89000002,$020824BC,$9C890000,$00020C24,$24BC8D00,$00000228,$000008B9,$24748B00,$8BA5F338,$02282484
  DD $948B0000,$00022C24,$248C8B00,$00000230,$84248489,$89000000,$00802494,$8C890000,$00008824,$249C8B00
  DD $0000008C,$249C8943,$0000008C,$8C24BC83,$09000000,$EA55850F,$5C8BFFFF,$9C397024,$00008824,$8B3F7D00
  DD $008424B4,$BC8B0000,$00008024,$24848B00,$00000088,$2824B489,$89000002,$022C24BC,$84890000,$00023024
  DD $24BC8D00,$00000248,$2824B48D,$B9000002,$00000008,$4C8BA5F3,$89417424,$8374244C,$0274247C,$066C840F
  DD $948B0000,$00025024,$24548900,$E7BCE970,$558BFFFF,$31DB3110,$0FC931F6,$00C0926F,$9C890000,$00012024
  DD $24B48900,$00000124,$18249C89,$89000001,$011C24B4,$C0310000,$0000EBE9,$FFFA8100,$0F000000,$0000D584
  DD $0FD28500,$0000CD84,$049C8A00,$00000218,$00FFE381,$DE890000,$83FA5B8D,$860F01FB,$000000B2,$840FF685
  DD $00000136,$0F01FE83,$00015284,$0006BB00,$84C70000,$00013024,$007FFF00,$8DF32900,$3C8D5B34,$C1FE89B3
  DD $FE2906E6,$3024BC8B,$8D000001,$DB01B31C,$9C89DF29,$00012824,$24BC8900,$00000130,$2824B48B,$8B000001
  DD $0130249C,$AF0F0000,$8C6E0FDE,$00012824,$00C38100,$C1000040,$620F0FFB,$0130248C,$6F0F0000,$C8F50FC1
  DD $0FDA6E0F,$620FCAFE,$E1720FDB,$C3F50F0F,$248CFE0F,$00000118,$2484FE0F,$00000120,$248C7F0F,$00000118
  DD $3C245489,$7F0FD901,$01202484,$83400000,$840F10F8,$000000D0,$4084948B,$C1000001,$BC8011EA,$0000EB24
  DD $850F0000,$FFFFFEFD,$18049C8A,$81000002,$0000FFE3,$83DE8900,$547E01FB,$000008BB,$2484C700,$00000130
  DD $00007FFF,$348DF329,$C1F789DB,$FE0106E7,$3024BC8B,$8D000001,$F729F334,$2824B489,$89000001,$013024BC
  DD $9C8B0000,$00013024,$DEAF0F00,$4000C381,$FBC10000,$CE6E0F0F,$FFFF35E9,$00768DFF,$84C72574,$00012824
  DD $007FFF00,$C7DB3100,$01302484,$00000000,$6E0F0000,$0128248C,$0BE90000,$C7FFFFFF,$01282484,$00000000
  DD $DB310000,$302484C7,$FF000001,$0F00007F,$28248C6E,$E9000001,$FFFFFEE6,$0026748D,$1C24B48B,$8B000001
  DD $01182484,$948B0000,$00012024,$24B48900,$0000027C,$18249C8B,$89000001,$01282484,$94890000,$00011824
  DD $8BC28900,$027C2484,$BC8B0000,$00012424,$249C8900,$00000278,$FAC1C389,$24BC891F,$00000274,$8B1FFBC1
  DD $012024B4,$84890000,$00013024,$24BC8B00,$00000274,$2C249489,$89000001,$0134249C,$948B0000,$00013024
  DD $249C8B00,$00000128,$7024B489,$89000002,$013824BC,$CE890000,$848BCF89,$00012C24,$248C8B00,$00000134
  DD $0FC2AF0F,$FFC1CBAF,$8BC1011F,$01302484,$A4F70000,$00012824,$24848900,$00000120,$9489CA01,$00012424
  DD $248C8B00,$00000120,$4000C181,$9C8B0000,$00012424,$89FA8900,$00D383F0,$01D6AF0F,$249489D2,$00000120
  DD $8489E6F7,$0000C824,$24848B00,$00000120,$848BC201,$0000C824,$24948900,$000000CC,$0FD9AC0F,$CC24948B
  DD $05000000,$00004000,$0F00D283,$290FD0AC,$C74875C1,$00A42484,$00000000,$C0310000,$A424948B,$E9000000
  DD $FFFFE6D9,$80249C39,$0F000000,$FFFBCB85,$FC44E9FF,$9066FFFF,$7824448B,$A4248489,$8B000000,$8B7C2444
  DD $00A42494,$AAE90000,$8BFFFFE6,$01182484,$C2890000,$20248489,$C1000001,$848B1FFA,$00013824,$24948900
  DD $00000124,$FAC1C289,$2484891F,$00000118,$1C249489,$BA000001,$40000000,$9C8BD089,$00012024,$1FFAC100
  DD $8489F9F7,$00013824,$8BC28900,$0124248C,$848B0000,$00013024,$C8AF0F00,$8B1FFAC1,$01342484,$AF0F0000
  DD $8BC101C3,$01202484,$94890000,$00013C24,$24A4F700,$00000130,$30248489,$01000001,$249489CA,$00000134
  DD $30248C8B,$81000001,$004000C1,$249C8B00,$00000134,$1824948B,$8B000001,$011C2484,$D3830000,$C6AF0F00
  DD $01D7AF0F,$24848BC2,$00000118,$30249489,$F7000001,$248489E6,$000000C8,$3024848B,$01000001,$24848BC2
  DD $000000C8,$CC249489,$0F000000,$8B0FD9AC,$00CC2494,$00050000,$83000040,$9C8B00D2,$00013C24,$D0AC0F00
  DD $8BC1290F,$01382484,$CA890000,$30248C89,$C1000001,$8C8B1FFA,$00013024,$24948900,$00000134,$3424948B
  DD $0F000001,$AF0FC2AF,$8BC301D9,$01382484,$A4F70000,$00013024,$89DA0100,$01302484,$94890000,$00013424
  DD $24948B00,$00000130,$4000C281,$8C8B0000,$00013424,$00D18300,$28249C8B,$0F000001,$C10FCAAC,$848B0FF9
  DD $00012C24,$24948900,$00000130,$34248C89,$8B000001,$01182494,$8C8B0000,$00011C24,$C2AF0F00,$01CBAF0F
  DD $24848BC1,$00000118,$2824A4F7,$89000001,$01282484,$CA010000,$2C249489,$8B000001,$01282494,$C2810000
  DD $00004000,$2C248C8B,$83000001,$848B00D1,$00012424,$CAAC0F00,$0FF9C10F,$28249489,$89000001,$012C248C
  DD $8C8B0000,$00012024,$C6AF0F00,$01CFAF0F,$24848BC1,$00000120,$8C89E6F7,$00011824,$89D38900,$24B48BC1
  DD $00000118,$C181F301,$00004000,$8B00D383,$01282494,$AC0F0000,$CA290FD9,$3C24B48B,$89000001,$24848BD3
  DD $00000138,$8B1FFBC1,$0130248C,$AF0F0000,$F2AF0FC3,$848BC601,$00013824,$05E2F700,$00004000,$89161C8D
  DD $00D283DA,$4000C181,$AC0F0000,$F9C10FD0,$00908D0F,$C1000040,$F9810FFA,$000000FF,$FFB9057E,$89000000
  DD $FFF083C8,$211FF8C1,$81D189C8,$0000FFFA,$B9057E00,$000000FF,$F283CA89,$1FFAC1FF,$D039CA21,$A4249489
  DD $0F000000,$E181C19F,$000000FF,$B4248C3B,$0F000000,$FFE3CC84,$248489FF,$000000A4,$948BD089,$0000A424
  DD $E3B7E900,$C031FFFF,$848AD231,$00026124,$24948A00,$00000262,$8B0EE2C1,$E0C1084D,$31D0090B,$24948AD2
  DD $00000260,$0908E2C1,$8AD231D0,$02632494,$E2C10000,$31D00911,$24948AD2,$00000264,$0914E2C1,$8AD231D0
  DD $02652494,$E2C10000,$31D00917,$24948AD2,$00000266,$091AE2C1,$8AD231D0,$02672494,$E2C10000,$31D0091D
  DD $044189D2,$848AC031,$00025924,$24948A00,$0000025A,$C10EE2C1,$D0090BE0,$948AD231,$00025824,$08E2C100
  DD $D231D009,$5B24948A,$C1000002,$D00911E2,$948AD231,$00025C24,$14E2C100,$D231D009,$5D24948A,$C1000002
  DD $D00917E2,$948AD231,$00025E24,$1AE2C100,$D231D009,$5F24948A,$C1000002,$D0091DE2,$8B014189,$02482484
  DD $01880000,$4C24848B,$88000002,$770F0141,$02B4C481,$5E5B0000,$5DEC895F,$E9D231C3,$FFFFE09F,$0026748D
end;


// ------------------  MULTI-THREAD ROUTINE  ----------------------

const
  // maximum size of the data buffer for multi-threaded compression
  DXT_MT_BUFFER_LENGTH = 256;

type
  // simple version of a critical section
  // perhaps for other OS to do other types and implementation methods
  TCompactCS = object
  private
    FSection: TRTLCriticalSection;
  public
    procedure Initialize;
    procedure Finalize;
    procedure Enter;
    procedure Leave;
  end;

  // some idea of the buffer, which is used for data storage
  // within the compression algorithm
  // to its threading panel for future use
  TDxtColourSet = record
    __: dword;
    flag: dword; // general count, but it will be 0 for Alpha mode.
    __data: array [1 .. (54 - 2)] of dword;
    // other internal data storage (including alpha)
  end;

  // Internal data buffer involved
  // The internal multi-threaded data compression: how color and alpha.
  // In the structure also adds a pointer to the location where to write the results of calculations
  TDxtMTData = record
    // basis - the buffer used in the compression algorithm
    ColourSet: TDxtColourSet;

    // pointer to the place where in the end it is necessary to write data: Color or Alpha.
    Destination: pointer;
  end;

  // fixed array of temporary data
  TDxtMTArray = array [0 .. DXT_MT_BUFFER_LENGTH - 1] of TDxtMTData;

  // Global manager DXT-specific data
  // Which is populated in the main thread
  // processed (and cleared) by the thread
  TDxtMTGlobalArray = object
  private
    // Main critical section to use all TDxtMTGlobalArray.
    // (Used in the compression of a single image)
    // Only through BeginUse / EndUse
    FMainCS: TCompactCS;

    // critical section for the interaction of the main and worker threads
    FCS: TCompactCS;

    // array of operating data, the number of elements (protected section)
    FArray: TDxtMTArray;
    FArrayCount: integer;
    FFinishMode: boolean; // If the flag is set, as soon as possible curving

    // Number of used threads
    // We have to first of all to monitor the situation, when all threads are freed
    FThreadCount: integer;

    // Data on threads
    // Needed for the finalization thread
    FThreadsData: array of THandle; { ToDo: for non-Windows - do something }
  private
    // Function is called from within a thread
    // Pick up the accumulated data buffer (that then processed)
    // -1 If it is necessary to finish the job thread
    function ReadFilled(var MT_BUFFER: TDxtMTArray): integer;

    // thread over, decrements
    procedure DecThreadCount;
  public
    // Initialization, finalization (in fact only the critical sections)
    // + Vanishing fields
    procedure Initialize;
    procedure Finalize;

    // use of a buffer in a single contraction of the whole image
    procedure BeginUse;
    procedure EndUse;

    // --------------------------------------------------------------

    // Add an item to buffer
    // (Only after Run)
    procedure AddFilled(const Destination: pointer;
      const ColourSet: TDxtColourSet);

    // Create the number of processing threads
    // Can even associate them with a specific nucleus
    // At the beginning they would just stand
    procedure run(const ThreadCount: integer);

    // Add data to the buffer is not planned (all image reads)
    // Set all necessary flags and wait for the removal of all threads
    procedure Finish();
  end;

  { TCompactCS }

procedure TCompactCS.Initialize;
begin
  InitializeCriticalSection(FSection);
end;

procedure TCompactCS.Finalize;
begin
  DeleteCriticalSection(FSection);
end;

procedure TCompactCS.Enter;
begin
  EnterCriticalSection(FSection);
end;

procedure TCompactCS.Leave;
begin
  LeaveCriticalSection(FSection);
end;

{ TDxtMTGlobalArray }

// critical sections and null fields
procedure TDxtMTGlobalArray.Initialize;
begin
  FMainCS.Initialize();
  FCS.Initialize();

  FArrayCount := 0;
  FFinishMode := false;
  FThreadCount := 0;
end;

// removal of critical sections
procedure TDxtMTGlobalArray.Finalize;
begin
  FCS.Finalize();
  FMainCS.Finalize();
end;

// Start to use the global buffer (manager)
// If it is free
procedure TDxtMTGlobalArray.BeginUse;
begin
  FMainCS.Enter;
end;

// Work with the buffer is complete, release
// So you can use it in future
procedure TDxtMTGlobalArray.EndUse;
begin
  FMainCS.Leave;
end;

// add data to an array
procedure TDxtMTGlobalArray.AddFilled(const Destination: pointer;
  const ColourSet: TDxtColourSet);
var
  Done: boolean;
begin
  while (true) do
  begin
    Done := false;

    // we try to add an element to the array
    FCS.Enter;
    try
      if (FArrayCount <> DXT_MT_BUFFER_LENGTH) then
      begin
        FArray[FArrayCount].ColourSet := ColourSet;
        FArray[FArrayCount].Destination := Destination;

        inc(FArrayCount);
        Done := true;
      end;
    finally
      FCS.Leave;
    end;

    // If we get to add (there is space in the buffer) - exit
    // If you did not add the - wait, and go to the next iteration
    if (Done) then
      break
    else
      Sleep(1);
  end;
end;

// Read the stored data in the buffer
// But if there is a flag FFinishMode, it is necessary (if the buffer is empty) return -1
function TDxtMTGlobalArray.ReadFilled(var MT_BUFFER: TDxtMTArray): integer;
begin
  FCS.Enter;
  try
    Result := FArrayCount;

    if (Result = 0) then
    begin
      // If it is necessary to give a signal that no data will no longer be
      // And the need to remove all the threads
      if (FFinishMode) then
        Result := -1;
    end
    else
    begin
      // read (Result) blocks in an array
      CopyMemory(@MT_BUFFER, @FArray, Result * sizeof(TDxtMTData));

      // zero counter
      FArrayCount := 0;
    end;
  finally
    FCS.Leave;
  end;
end;

// thread over, decrements
procedure TDxtMTGlobalArray.DecThreadCount;
begin
  FCS.Enter;
  dec(FThreadCount);
  FCS.Leave;
end;

// thread function to read and process data DXT
function DxtThreadProc(const DxtMTGlobalArray: TDxtMTGlobalArray): integer;
var
  i, Count: integer;
  MT_BUFFER: TDxtMTArray;
begin
  // machining cycle
  while (true) do
  begin
    // reading
    Count := DxtMTGlobalArray.ReadFilled(MT_BUFFER);
    if (Count < 0) then
      break;

    // either waiting or processing
    if (Count = 0) then
    begin
      // waiting
      Sleep(1);
    end
    else
    begin
      // Count blocks processing
      for i := 0 to Count - 1 do
        with MT_BUFFER[i] do
        begin
          if (ColourSet.flag <> 0) then
          begin
            // color compressing
            DxtConstsData.ColorsCompress(Destination, @ColourSet,
              DxtConstsData);
          end
          else
          begin
            // alpha compressing
            DxtConstsData.AlphaCompress(Destination, @ColourSet.__data,
              DxtConstsData);
          end;
        end;
    end;
  end;

  // thread count decrement
  DxtMTGlobalArray.DecThreadCount;

  // result
  Result := 0;
end;

// Create the number of processing threads
// Can even associate them with a specific nucleus
// At the beginning they would just stand
procedure TDxtMTGlobalArray.run(const ThreadCount: integer);
var
  i: integer;
  buf: cardinal;
  ParallelMode: boolean;
begin
  // This mode must be determined in order
  // To create thread each in its core
  ParallelMode := (ThreadCount = CPU_COUNT);

  // threading
  FCS.Enter;
  try
    // internal field control
    FThreadCount := ThreadCount;
    SetLength(FThreadsData, ThreadCount);

    // creation and initialization
    for i := 0 to ThreadCount - 1 do
    begin
{$IFDEF MSWINDOWS}
      FThreadsData[i] := System.BeginThread(nil, 0, TThreadFunc(@DxtThreadProc),
        pointer(@Self), 0, buf);

      if (ParallelMode) then
        Windows.SetThreadIdealProcessor(FThreadsData[i], i);
{$ELSE}
{$MESSAGE ERROR 'Thread creation not defined'}
{$ENDIF}
    end;
  finally
    FCS.Leave;
  end;
end;

// Read the image over
// Gives a signal and waits for the remote thread
procedure TDxtMTGlobalArray.Finish;
var
  i: integer;
  Done: boolean;
begin
  // putting a flag on the completion of the entire
  FCS.Enter;
  FFinishMode := true;
  FCS.Leave;

  // look forward to when all the threads are processed and deleted
  while (true) do
  begin
    Done := false;

    // Consider the number of threads
    FCS.Enter;
    if (FThreadCount = 0) then
      Done := true;
    FCS.Leave;

    // If all retired - ok
    // If in the process - then wait
    if (Done) then
      break
    else
      Sleep(1);
  end;

  // cleans up after itself (list of threads and FFinishMode)
  for i := 0 to Length(FThreadsData) - 1 do
  begin
{$IFDEF MSWINDOWS}
    Windows.CloseHandle(FThreadsData[i]);
{$ELSE}
{$MESSAGE ERROR 'Thread finalization not defined'}
{$ENDIF}
  end;
  FThreadsData := nil;
  FFinishMode := false;
end;

var
  DxtMTGlobalArray: TDxtMTGlobalArray;

  // Handler for the multi-threaded compression color
  // In fact add just ColourSet in a queue for processing threads
procedure DXT_MT_ColorsCompress(const Dest, ColourSet, DxtConstsData
  : pointer); cdecl;
begin
  DxtMTGlobalArray.AddFilled(Dest, TDxtColourSet(ColourSet^));
end;

// Handler for the multi-threaded compression alphachennel
// Add a fake web ColourSet for processing array
// Flag = 0 (for alphachennel)
procedure DXT_MT_AlphaCompress(const Dest, BGRA, DxtConstsData: pointer); cdecl;
var
  fake: TDxtColourSet;
begin
  fake.flag := 0;
  Move(BGRA^, fake.__data, 16 * sizeof(dword));
  DxtMTGlobalArray.AddFilled(Dest, fake);
end;

// parallel version of texture compression
procedure DxtImageCompressParallel(const DxtBlocks: pointer;
  const DXT1: boolean; const ImageWidth, ImageHeight: integer;
  const Handle: pointer; const Callback: TDxtCallback;
  const DataMode: boolean = false);
const
  MODE_PARALLEL = $70; // 01110 000
var
  Mask: integer;
  BlocksCount: integer;
begin
  // fill mask
  Mask := ord(DXT1);
  if (DataMode) then
    Mask := Mask or 2;

  // if a single core or a small number of blocks
  BlocksCount := ((ImageWidth + 3) shr 2) * ((ImageHeight + 3) shr 2);
  if (CPU_COUNT = 1) or (BlocksCount <= 10 * 10) then
  begin
    DxtInternalImageCompress(DxtBlocks, Mask, ImageWidth, ImageHeight, Handle,
      Callback);
    exit;
  end;

  // multi-threaded version
  Mask := Mask or MODE_PARALLEL;
  DxtMTGlobalArray.BeginUse;
  try
    // initialization threads
    DxtMTGlobalArray.run(CPU_COUNT);

    // call the compression function
    DxtInternalImageCompress(DxtBlocks, Mask, ImageWidth, ImageHeight, Handle,
      Callback);

    // complete threads
    DxtMTGlobalArray.Finish();
  finally
    DxtMTGlobalArray.EndUse;
  end;
end;

procedure CheckSSE;
asm
  push ebx
  mov eax, 1
  cpuid
  test edx, 02000000h
  setnz [SSE]
  // test edx, 04000000h
  // setnz [SSE2]
  pop ebx
end;

// ToDo Linux, MacOS
procedure CheckCPU;
var
{$IFDEF MSWINDOWS}
  Info: TSystemInfo;
{$ENDIF}
begin
  CheckSSE;

{$IFDEF MSWINDOWS}
  Windows.GetSystemInfo(Info);
  CPU_COUNT := integer(Info.dwNumberOfProcessors);
{$ENDIF}
end;

initialization

DxtConstsData := PDxtConstsData((integer(@DXT_CONSTS_DATA) + 15) and -16);
Move(DXT_CONSTS_DATA, DxtConstsData^, DXT_CONSTS_SIZE);
CheckCPU();
if (CPU_COUNT <> 1) then
  DxtMTGlobalArray.Initialize;

finalization

if (CPU_COUNT <> 1) then
  DxtMTGlobalArray.Finalize;

end.
