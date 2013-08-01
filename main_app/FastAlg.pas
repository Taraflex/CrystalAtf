{
  2013-07-27
  Taratin Alexander
  Public domain
}

unit FastAlg;

interface

uses Windows, Classes, Graphics;

type
  TFColor = packed record
    b, g, r: Byte;
  end;

  PFColor = ^TFColor;

  TFColorA = packed record
    case Integer of
      0:
        (i: DWord);
      1:
        (c: TFColor);
      2:
        (hi, lo: Word);
      3:
        (b, g, r, a: Byte;);
  end;

  PFColorA = ^TFColorA;

  TLine32 = array [Word] of TFColorA;
  PLine32 = ^TLine32;

  TLine24 = array [Word] of TFColor;
  PLine24 = ^TLine24;

function isMMX: Boolean;
procedure BitmapAntialias2X(var SrcBitmap, DstBitmap: TBitmap);
function DIBBits(var BMP: TBitmap): Pointer;
function ScanLineSize(var BMP: TBitmap): Integer;
procedure Compress(var Src: TBitmap);
procedure Bilinear32(var Src, Dst: TBitmap);
procedure Bilinear24(var Src, Dst: TBitmap);
function hasPngAlpha(var Src: TBitmap): Boolean;
function hasTgaAlpha(var Src: TBitmap): Boolean;
procedure GetColor(var Src: TBitmap; Dst: Pointer; ColorSize: Byte = 4);

implementation

{$I ctypes.inc}

var
  CPUisMMX: Boolean = false;

procedure Bilinear24(var Src, Dst: TBitmap);
var
  x, y, xp, yp, ypp, xpp, t1, t2, zx, zy, izy, w1, w2, w3, w4: Integer;
  y1, y2: PLine24;
  pc: PFColor;
begin
  xpp := (Src.Width shl 16) div Dst.Width;
  ypp := (Src.Height shl 16) div Dst.Height;
  yp := (ypp shr 1) - $8000;
  for y := 0 to Dst.Height - 1 do
  begin
    pc := Pointer(Dst.ScanLine[y]);
    if yp < 0 then
    begin
      t1 := 0;
      zy := 0;
      izy := $10000;
    end
    else
    begin
      t1 := yp shr 16;
      zy := yp and $FFFF;
      izy := ((not yp) and $FFFF) + 1;
    end;
    if t1 < Src.Height - 1 then
      t2 := t1 + 1
    else
      t2 := t1;
    y1 := Src.ScanLine[t1];
    y2 := Src.ScanLine[t2];
    xp := (xpp shr 1) - $8000;
    for x := 0 to Dst.Width - 1 do
    begin
      if xp < 0 then
      begin
        t1 := 0;
        zx := 0;
      end
      else
      begin
        t1 := xp shr 16;
        zx := xp and $FFFF;
      end;
      if t1 < Src.Width - 1 then
        t2 := t1 + 1
      else
        t2 := t1;
      w2 := (izy * zx) shr 16;
      w1 := izy - w2;
      w4 := (zy * zx) shr 16;
      w3 := zy - w4;
      pc.b := (y1[t1].b * w1 + y1[t2].b * w2 + y2[t1].b * w3 + y2[t2].b * w4 +
        $8000) shr 16;
      pc.g := (y1[t1].g * w1 + y1[t2].g * w2 + y2[t1].g * w3 + y2[t2].g * w4 +
        $8000) shr 16;
      pc.r := (y1[t1].r * w1 + y1[t2].r * w2 + y2[t1].r * w3 + y2[t2].r * w4 +
        $8000) shr 16;
      Inc(xp, xpp);
      Inc(pc);
    end;
    Inc(yp, ypp);
  end;
end;

procedure Bilinear32(var Src, Dst: TBitmap);
var
  x, y, xp, yp, ypp, xpp, t1, t2, zx, zy, izy, w1, w2, w3, w4: Integer;
  y1, y2: PLine32;
  pc: PFColorA;
begin
  xpp := (Src.Width shl 16) div Dst.Width;
  ypp := (Src.Height shl 16) div Dst.Height;
  yp := (ypp shr 1) - $8000;
  for y := 0 to Dst.Height - 1 do
  begin
    pc := Pointer(Dst.ScanLine[y]);
    if yp < 0 then
    begin
      t1 := 0;
      zy := 0;
      izy := $10000;
    end
    else
    begin
      t1 := yp shr 16;
      zy := yp and $FFFF;
      izy := ((not yp) and $FFFF) + 1;
    end;
    if t1 < Src.Height - 1 then
      t2 := t1 + 1
    else
      t2 := t1;
    y1 := Src.ScanLine[t1];
    y2 := Src.ScanLine[t2];
    xp := (xpp shr 1) - $8000;
    for x := 0 to Dst.Width - 1 do
    begin
      if xp < 0 then
      begin
        t1 := 0;
        zx := 0;
      end
      else
      begin
        t1 := xp shr 16;
        zx := xp and $FFFF;
      end;
      if t1 < Src.Width - 1 then
        t2 := t1 + 1
      else
        t2 := t1;
      w2 := (izy * zx) shr 16;
      w1 := izy - w2;
      w4 := (zy * zx) shr 16;
      w3 := zy - w4;
      pc.b := (y1[t1].b * w1 + y1[t2].b * w2 + y2[t1].b * w3 + y2[t2].b * w4 +
        $8000) shr 16;
      pc.g := (y1[t1].g * w1 + y1[t2].g * w2 + y2[t1].g * w3 + y2[t2].g * w4 +
        $8000) shr 16;
      pc.r := (y1[t1].r * w1 + y1[t2].r * w2 + y2[t1].r * w3 + y2[t2].r * w4 +
        $8000) shr 16;
      pc.a := (y1[t1].a * w1 + y1[t2].a * w2 + y2[t1].a * w3 + y2[t2].a * w4 +
        $8000) shr 16;
      Inc(xp, xpp);
      Inc(pc);
    end;
    Inc(yp, ypp);
  end;
end;

function hasTgaAlpha(var Src: TBitmap): Boolean;
var
  y: uint32;
  x: uint32;
  src_pointer: PFColorA;
  a: Byte;
begin
  if Src.PixelFormat = pf24bit then
    Exit(false);
  for y := 0 to Src.Height - 1 do
  begin
    src_pointer := Src.ScanLine[y];
    for x := 0 to Src.Width - 1 do
    begin
      if (src_pointer.a < 255) and (src_pointer.a > 0) then
        Exit(True);
      Inc(src_pointer);
    end;
  end;
  Result := false;
end;

function hasPngAlpha(var Src: TBitmap): Boolean;
var
  y: uint32;
  x: uint32;
  src_pointer: PFColorA;
  a: Byte;
begin
  Result := false;
  if Src.PixelFormat = pf24bit then
    Exit;
  for y := 0 to Src.Height - 1 do
  begin
    src_pointer := Src.ScanLine[y];
    for x := 0 to Src.Width - 1 do
    begin
      a := src_pointer.a;
      if a = 0 then
      begin
        src_pointer.r := 0;
        src_pointer.g := 0;
        src_pointer.b := 0;
      end
      else if a <> 255 then
      begin
        Result := True;
        src_pointer.r := src_pointer.r * 255 div a;
        src_pointer.g := src_pointer.g * 255 div a;
        src_pointer.b := src_pointer.b * 255 div a;
      end;
      Inc(src_pointer);
    end;
  end;
end;

procedure GetColor(var Src: TBitmap; Dst: Pointer; ColorSize: Byte = 4);
var
  y: uint32;
  w: uint32;
  src_pointer: Pointer;
begin
  w := Src.Width * ColorSize;
  for y := 0 to Src.Height - 1 do
  begin
    src_pointer := Src.ScanLine[y];
    Move(src_pointer^, Dst^, w);
    Dst := Pointer(uint32(Dst) + w);
  end;
end;

procedure Compress(var Src: TBitmap);
var
  Target: TBitmap;
  a: Integer;
  b: Integer;
begin
  a := Src.Width div 2;
  b := Src.Height div 2;
  if a < 1 then
    a := 1;
  if b < 1 then
    b := 1;

  if (a = Src.Width) and (b = Src.Height) then
    Exit;

  Target := TBitmap.Create;

  Target.PixelFormat := Src.PixelFormat;
  Target.SetSize(a, b);
  BitmapAntialias2X(Src, Target);
  Src.Free;
  Src := Target;
end;

function DIBBits(var BMP: TBitmap): Pointer;
var
  Section: TDIBSECTION;
begin
  BMP.HandleType := bmDIB;
  GetObject(BMP.Handle, sizeof(TDIBSECTION), @Section);
  Result := Section.dsBm.bmBits;
end;

function ScanLineSize(var BMP: TBitmap): Integer;
var
  Section: TDIBSECTION;
begin
  BMP.HandleType := bmDIB;
  GetObject(BMP.Handle, sizeof(TDIBSECTION), @Section);
  Result := ((Section.dsBmih.biBitCount * Section.dsBmih.biWidth + 31) shr 3)
    and $FFFFFFFC;;
end;

function isMMX: Boolean;
var
  i: Integer;
begin
  i := 0;
  Result := false;
  asm // check if bit 21 of EFLAGS can be set and reset
    PUSHFD
    POP     EAX
    OR      EAX, 1 shl 21
    PUSH    EAX
    POPFD
    PUSHFD
    POP     EAX
    TEST    EAX, 1 shl 21
    JZ      @@1
    AND     EAX, not( 1 shl 21 )
    PUSH    EAX
    POPFD
    PUSHFD
    POP     EAX
    TEST    EAX, 1 shl 21
    JNZ     @@1
    INC     [ I ]
  @@1:
  end;
  if i = 0 then
    Exit; // CPUID not supported
  asm // get CPU features flags using CPUID command
    MOV     EAX, 1
    PUSH    EDX
    PUSH    EBX
    PUSH    ECX
    DB $0F, $A2
    MOV     [ I ], EDX  // I := features information
    POP     ECX
    POP     EBX
    POP     EDX
  end;
  if (i and (1 shl 23)) <> 0 then
    Result := True;
end;

procedure BitmapAntialias2X(var SrcBitmap, DstBitmap: TBitmap);
var
  SrcBits: DWord;
  DstBits: DWord;
  dHeight: DWord;
  dWidth: DWord;
  Delta: DWord;
begin
  case SrcBitmap.PixelFormat of
    pf32bit:
      if CPUisMMX then
      begin
        SrcBits := DWord(DIBBits(SrcBitmap));
        DstBits := DWord(DIBBits(DstBitmap));
        dHeight := DstBitmap.Height;
        dWidth := DstBitmap.Width;
        Delta := ScanLineSize(SrcBitmap);
        asm
          pushad
          mov esi, SrcBits
          mov edi, DstBits
          // pxor mm2, mm2
          db $0f, $ef, $d2

          mov eax, dHeight
        @LM1:       push eax

          mov eax, dWidth
        @LM2:       /// //////
          mov ecx, esi

          // movd mm1, [ecx]
          db $0f, $6e, $09
          // punpcklbw mm1, mm2
          db $0f, $60, $ca
          // movd mm3, [ecx+4]
          db $0f, $6e, $59, $04
          // punpcklbw mm3, mm2
          db $0f, $60, $da
          // paddusw mm1, mm3
          db $0f, $dd, $cb

          add ecx, Delta

          // movd mm3, [ecx]
          db $0f, $6e, $19
          // punpcklbw mm3, mm2
          db $0f, $60, $da
          // paddusw mm1, mm3
          db $0f, $dd, $cb
          // movd mm3, [ecx+4]
          db $0f, $6e, $59, $04
          // punpcklbw mm3, mm2
          db $0f, $60, $da
          // paddusw mm1, mm3
          db $0f, $dd, $cb

          // psrlw mm1, 2
          db $0f, $71, $d1, $02
          // packuswb mm1, mm2
          db $0f, $67, $ca
          // movd [edi], mm1
          db $0f, $7e, $0f
          /// //////

          add edi, 4
          add esi, 8

          sub eax, 1
          jnz @LM2

          add esi, Delta

          pop eax
          sub eax, 1
          jnz @LM1

          // emms
          db $0f, $77

          popad
        end;
      end
      else
        Bilinear32(SrcBitmap, DstBitmap);
    pf24bit:
      Bilinear24(SrcBitmap, DstBitmap);
  end;
end;

initialization

CPUisMMX := isMMX();

end.
