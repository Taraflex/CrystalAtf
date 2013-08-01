{
  2013-07-27
  Taratin Alexander
  Public domain
}

unit Main;

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
{$define FLIP_DLG}

interface

uses Windows, SysUtils, Classes, CrystalDxt, FastAlg, Vcl.Graphics, jpeg,
  PNGImage, TPSDGraphics, Math, TGA;

procedure GetBitmap32bitBlock(const Handle: pointer; var BGRABlock: TBGRABlock;
  const X, Y: integer);
procedure GetBitmap24bitBlock(const Handle: pointer; var BGRABlock: TBGRABlock;
  const X, Y: integer);

procedure SaveDDSImage(var Image: TBitmap; const name: string;
  Format: AtfFormat; isAlpha: boolean);
procedure ConvertToATF(Filename, Targetname: string; Format: AtfFormat);

var
  isDds: bool = false;
  nomip: bool = false;

implementation

const
  MAXTEXTURESIZE = 2048;
{$IFDEF DEBUG}
  DLLPATH = 'E:\Documents\RAD Studio\Projects\cpp_utf\Win32\Release\atf.dll';
{$ELSE}
  DLLPATH = 'atf.dll';
{$ENDIF}
function _convert(header, dxtdata: pointer; datalen: integer; name: AnsiString)
  : integer; cdecl; stdcall; external DLLPATH;

type
  TSizes = array [0 .. 12] of uint32;

var
  tempbitmap: TBitmap;
  temppng: TPNGImage;
  tempjpeg: TJpegimage;
  temppsd: TPSDGraphic;
  Extensions: TStringList;
  stream: TMemoryStream;
  //
  dxtdata: TBytes;
  selectedSize: uint32 = 0;

function log2(X: uint32): uint8; inline;
begin
  Result := Ceil(Ln(X) / Ln(2));
end;

function AtfHeader(w, h, filesize: uint32; Format, nummips: uint8): TAtfHeader;
begin
  Result[0] := byte('A');
  Result[1] := byte('T');
  Result[2] := byte('F');
  Result[3] := ((filesize shr 16) and $FF);
  Result[4] := ((filesize shr 8) and $FF);
  Result[5] := ((filesize shr 0) and $FF);
  Result[6] := (Format);
  Result[7] := (log2(w));
  Result[8] := (log2(h));
  Result[9] := nummips;
end;

//
// callback to fill bgra block from 24-bit bitmap
procedure GetBitmap24bitBlock(const Handle: pointer; var BGRABlock: TBGRABlock;
  const X, Y: integer);
var
  Bitmap: TBitmap;
  src, Dest: PBGRA;
  i, j, Width, Height: integer;
begin
  Bitmap := TBitmap(Handle);
  Width := Bitmap.Width;
  Height := Bitmap.Height;

  src := nil;
  Dest := @BGRABlock[0];
  for j := Y to Y + 3 do
  begin
    if (j < Height) then
      src := pointer(integer(Bitmap.ScanLine[j]) + sizeof(TBGRA) * X);

    for i := X to X + 3 do
    begin
      if (i >= Width) or (j >= Height) then
        Dest^.a := 0
      else
      begin
        Dest^ := src^;
        Dest^.a := 255;
      end;

      inc(Dest);
      inc(src);
    end;
  end;
end;

// callback to fill bgra block from 32-bit bitmap
procedure GetBitmap32bitBlock(const Handle: pointer; var BGRABlock: TBGRABlock;
  const X, Y: integer);
var
  Bitmap: TBitmap;
  src, Dest: PBGRA;
  i, j, Width, Height: integer;
begin
  Bitmap := TBitmap(Handle);
  Width := Bitmap.Width;
  Height := Bitmap.Height;

  src := nil;
  Dest := @BGRABlock[0];
  for j := Y to Y + 3 do
  begin
    if (j < Height) then
      src := pointer(integer(Bitmap.ScanLine[j]) + sizeof(TBGRA) * X);

    for i := X to X + 3 do
    begin
      if (i >= Width) or (j >= Height) then
        Dest^.a := 0
      else
        Dest^ := src^;

      inc(Dest);
      inc(src);
    end;
  end;
end;
{
procedure Dds2Atf(Filename, ext: string);
begin
  stream.LoadFromFile(Filename);
  _convert(stream.Memory, pointer(integer(stream.Memory) + 128), stream.size,
    AnsiString(copy(Filename, 0, Length(Filename) - Length(ext)) + '.atf'));
end;   }

function IsPowerOf2(X: integer): boolean; inline;
begin
  Result := ((X and (X - 1)) = 0) and (X <= MAXTEXTURESIZE);
end;

function toPowerOf2(X: integer): integer; inline;
begin
  Result := X - 1;
  Result := Result or (Result Shr 1);
  Result := Result or (Result Shr 2);
  Result := Result or (Result Shr 4);
  Result := Result or (Result Shr 8);
  Result := Result or (Result Shr 16);
  inc(Result);
  if Result > MAXTEXTURESIZE then
    Result := MAXTEXTURESIZE;
end;

procedure ConvertToATF(Filename, Targetname: string; Format: AtfFormat);
var
  ext: string;
  isAlpha: boolean;
begin
  isAlpha := false;
  if FileExists(Filename) then
  begin
    ext := ExtractFileExt(Filename);
    { if (ext = '.dds') and (not isDds) then
      Dds2Atf(Filename, ext)
      else } if (Extensions.IndexOf(ext) > -1) then
    begin
      case ext[3] of
        's': // 'psd'
          begin
            temppsd.LoadFromFile(Filename);
            tempbitmap.Assign(temppsd);
            tempbitmap.PixelFormat := pf24bit;
          end;
        'n': // 'png'
          begin
            temppng.LoadFromFile(Filename);
            tempbitmap.Assign(temppng);
            isAlpha := hasPngAlpha(tempbitmap);
          end;
        'g': // tga
          begin
            tempbitmap.Free;
            tempbitmap := LoadTgaImage(Filename);
            isAlpha := hasTgaAlpha(tempbitmap);
          end;
        'm': // 'bmp'
          begin
            tempbitmap.LoadFromFile(Filename);
            tempbitmap.PixelFormat := pf24bit;
          end;
        'p': // 'jpg' 'jpeg'
          begin
            tempjpeg.LoadFromFile(Filename);
            tempbitmap.Assign(tempjpeg);
            tempbitmap.PixelFormat := pf24bit;
          end;
      end;

      if Targetname.Length < 1 then
      begin
        Targetname := copy(Filename, 0, Length(Filename) - Length(ext));
        if isDds then
          Targetname := Targetname + '.dds'
        else
          Targetname := Targetname + '.atf';
      end;

      SaveDDSImage(tempbitmap, Targetname, Format, isAlpha);
    end;
  end;
end;

procedure SaveDDSImage(var Image: TBitmap; const name: string;
  Format: AtfFormat; isAlpha: boolean);
var
  w: integer;
  h: integer;
  nummips: integer;
  Sizes: TSizes;
  size: uint32;
  OriginalPF: TPixelFormat;
  Image32bit: TBitmap;
  header: TDxtHeader;
  i: integer;
  filestr: TFileStream;
  pos: uint32;
begin
  // if not(Image.PixelFormat in [pf24bit, pf32Bit]) then
  // Image.PixelFormat := pf24bit;
  if Format = Auto then
  begin
    if isAlpha then
      Format := DXT5
    else
      Format := DXT1
  end
  else if Format = RGB then
    if isAlpha then
      Format := RGBA
    else
    begin
      Format := RGB;
      Image.PixelFormat := pf24bit;
    end;

  Image32bit := TBitmap.Create;
  if (IsPowerOf2(Image.Width)) and (IsPowerOf2(Image.Height)) then
    Image32bit.Assign(Image)
  else
  begin
    Image32bit.PixelFormat := Image.PixelFormat;
    Image32bit.SetSize(toPowerOf2(Image.Width), toPowerOf2(Image.Height));
    case Image.PixelFormat of
      pf24bit:
        Bilinear24(Image, Image32bit);
      pf32Bit:
        Bilinear32(Image, Image32bit);
    end;
  end;

  OriginalPF := Image32bit.PixelFormat;
  if Format in [DXT1, DXT5] then
  begin
    Image32bit.PixelFormat := pf32Bit;
  end;

  nummips := 0;
  w := Image32bit.Width;
  h := Image32bit.Height;

  if nomip then
  begin
    size := DxtImageSize(Format, w, h);
    Sizes[0] := size;
  end
  else
  begin
    size := 0;
    while (w > 1) or (h > 1) do
    begin
      Sizes[nummips] := DxtImageSize(Format, w, h);
      size := size + Sizes[nummips];
      if w > 1 then
        w := w div 2;
      if h > 1 then
        h := h div 2;
      inc(nummips);
    end;
    Sizes[nummips] := DxtImageSize(Format, 1, 1);
    inc(size, Sizes[nummips]);
  end;

  if selectedSize < size then
  begin
    SetLength(dxtdata, size);
    selectedSize := size;
  end;

  header := DxtHeader(Image32bit.Width, Image32bit.Height, nummips + 1, Format);
  i := 0;
  pos := 0;

  repeat
    if Format in [RGBA, RGB] then
      GetColor(Image32bit, @dxtdata[pos], 3 + byte(Format = RGBA))
    else
      case OriginalPF of
        pf32Bit:
          DxtImageCompressParallel(@dxtdata[pos], Format = DXT1,
            Image32bit.Width, Image32bit.Height, pointer(Image32bit),
            GetBitmap32bitBlock);
        pf24bit:
          DxtImageCompressParallel(@dxtdata[pos], Format = DXT1,
            Image32bit.Width, Image32bit.Height, pointer(Image32bit),
            GetBitmap24bitBlock);
      end;
    inc(pos, Sizes[i]);
    inc(i);
    Compress(Image32bit);
  until (i > nummips);

  if isDds then
  begin
    filestr := TFileStream.Create(string(name), fmCreate);
    filestr.Write(header, 128);
    filestr.Write(dxtdata, pos);
    filestr.Free;
  end
  else
    _convert(@header, @dxtdata[0], pos, AnsiString(name));

  FreeAndNil(Image32bit);
  writeln(name);
end;

initialization

tempjpeg := TJpegimage.Create;
tempbitmap := TBitmap.Create;
temppng := TPNGImage.Create;
temppsd := TPSDGraphic.Create;
Extensions := TStringList.Create;
Extensions.Add('.psd');
Extensions.Add('.png');
Extensions.Add('.bmp');
Extensions.Add('.jpg');
Extensions.Add('.jpe');
Extensions.Add('.jpeg');
Extensions.Add('.tga');

finalization

SetLength(dxtdata, 0);
tempjpeg.Free;
tempbitmap.Free;
temppng.Free;
temppsd.Free;
Extensions.Free;

end.
