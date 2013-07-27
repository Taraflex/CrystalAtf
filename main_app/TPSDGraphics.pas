unit TPSDGraphics;

interface

uses
  Windows, SysUtils, Classes, Graphics, Math, StdCtrls;

type
  PRGB = ^TRGB;

  TRGB = packed record
    R, G, B: Byte;
  end;

  PCMYK = ^TCMYK;

  TCMYK = packed record
    C, M, Y, K: Byte;
  end;

  PCMYK16 = ^TCMYK16;

  TCMYK16 = packed record
    C, M, Y, K: Word;
  end;

  PBGR = ^TBGR;

  TBGR = packed record
    B, G, R: Byte;
  end;

  PRGBWord = ^TRGBWord;

  TRGBWord = record
    R, G, B: Word;
  end;

  TPSDHeader = packed record
    Signature: array [0 .. 3] of ansichar;
    Version: Word;
    Reserved: array [0 .. 5] of Byte;
    Channels: Word;
    Rows, Columns: Cardinal;
    Depth: Word;
    Mode: Word;
  end;

  TPSDGraphic = class(TBitmap)
  private
    FPalette: array [0 .. 767] of Byte;
    procedure MakePalette(BPS: Byte; Mode: Integer);
  public
    procedure LoadFromStream(Stream: TStream); override;
  end;

  TPackbitsRLE = class
  public
    procedure Decode(var Source: Pointer; Dest: Pointer;
      PackedSize, UnpackedSize: Integer);
  end;

const
  PSD_BITMAP = 0;
  PSD_GRAYSCALE = 1;
  PSD_INDEXED = 2;
  PSD_RGB = 3;
  PSD_CMYK = 4;
  PSD_MULTICHANNEL = 7;
  PSD_DUOTONE = 8;
  PSD_LAB = 9;
  PSD_COMPRESSION_NONE = 0;
  PSD_COMPRESSION_RLE = 1;

implementation

procedure CIELAB2BGR(LSource, aSource, bSource: PByte; Target: Pointer;
  BitsPerSample: Byte; Count: Cardinal); overload;
var
  FinalR, FinalG, FinalB: Integer;
  l, a, B, X, Y, Z, T, YYn3: Double;
  TargetPtr: PByte;
  PixelCount: Cardinal;
begin
  TargetPtr := Target;
  PixelCount := Count div 3;

  while PixelCount > 0 do
  begin
    l := LSource^ / 2.55;
    Inc(LSource);
    a := ShortInt(aSource^);
    Inc(aSource);
    B := ShortInt(bSource^);
    Inc(bSource);

    YYn3 := (l + 16) / 116;
    if l < 7.9996 then
    begin
      Y := l / 903.3;
      X := a / 3893.5 + Y;
      Z := Y - B / 1557.4;
    end
    else
    begin
      T := YYn3 + a / 500;
      X := T * T * T;
      Y := YYn3 * YYn3 * YYn3;
      T := YYn3 - B / 200;
      Z := T * T * T;
    end;

    FinalR := Round(255 * (2.998 * X - 1.458 * Y - 0.541 * Z));
    FinalG := Round(255 * (-0.952 * X + 1.893 * Y + 0.059 * Z));
    FinalB := Round(255 * (0.099 * X - 0.198 * Y + 1.099 * Z));

    TargetPtr^ := Max(0, Min(255, FinalB));
    Inc(TargetPtr);
    TargetPtr^ := Max(0, Min(255, FinalG));
    Inc(TargetPtr);
    TargetPtr^ := Max(0, Min(255, FinalR));
    Inc(TargetPtr);

    Dec(PixelCount);
  end;
end;

procedure CMYK2BGR(C, M, Y, K, Target: Pointer; BitsPerSample: Byte;
  Count: Cardinal); overload;
var
  R, G, B: Integer;
  C8, M8, Y8, K8: PByte;
  C16, M16, Y16, K16: PWord;
  I: Integer;
  TargetPtr: PByte;
begin
  case BitsPerSample of
    8:
      begin
        C8 := C;
        M8 := M;
        Y8 := Y;
        K8 := K;
        TargetPtr := Target;
        Count := Count div 4;
        for I := 0 to Count - 1 do
        begin
          R := 255 - (C8^ - MulDiv(C8^, K8^, 255) + K8^);
          G := 255 - (M8^ - MulDiv(M8^, K8^, 255) + K8^);
          B := 255 - (Y8^ - MulDiv(Y8^, K8^, 255) + K8^);
          TargetPtr^ := Max(0, Min(255, B));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, G));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, R));
          Inc(TargetPtr);
          Inc(C8);
          Inc(M8);
          Inc(Y8);
          Inc(K8);
        end;
      end;
    16:
      begin
        C16 := C;
        M16 := M;
        Y16 := Y;
        K16 := K;
        TargetPtr := Target;
        Count := Count div 4;
        for I := 0 to Count - 1 do
        begin
          R := 255 - (C16^ - MulDiv(C16^, K16^, 65535) + K16^) shr 8;
          G := 255 - (M16^ - MulDiv(M16^, K16^, 65535) + K16^) shr 8;
          B := 255 - (Y16^ - MulDiv(Y16^, K16^, 65535) + K16^) shr 8;
          TargetPtr^ := Max(0, Min(255, B));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, G));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, R));
          Inc(TargetPtr);
          Inc(C16);
          Inc(M16);
          Inc(Y16);
          Inc(K16);
        end;
      end;
  end;
end;

procedure RGB2BGR(R, G, B, Target: Pointer; BitsPerSample: Byte;
  Count: Cardinal); overload;
var
  R8, G8, B8: PByte;
  R16, G16, B16: PWord;
  TargetRun: PByte;
begin
  Count := Count div 3;
  case BitsPerSample of
    8:
      begin
        R8 := R;
        G8 := G;
        B8 := B;
        TargetRun := Target;
        while Count > 0 do
        begin
          TargetRun^ := B8^;
          Inc(B8);
          Inc(TargetRun);
          TargetRun^ := G8^;
          Inc(G8);
          Inc(TargetRun);
          TargetRun^ := R8^;
          Inc(R8);
          Inc(TargetRun);
          Dec(Count);
        end;
      end;
    16:
      begin
        R16 := R;
        G16 := G;
        B16 := B;
        TargetRun := Target;
        while Count > 0 do
        begin
          TargetRun^ := B16^ shr 8;
          Inc(B16);
          Inc(TargetRun);
          TargetRun^ := G16^ shr 8;
          Inc(G16);
          Inc(TargetRun);
          TargetRun^ := R16^ shr 8;
          Inc(R16);
          Inc(TargetRun);
          Dec(Count);
        end;
      end;
  end;
end;

procedure SwapShort(P: PWord; Count: Cardinal);
asm
@@Loop:
  MOV CX, [EAX]
  XCHG CH, CL
  MOV [EAX], CX
  ADD EAX, 2
  DEC EDX
  JNZ @@Loop
end;

procedure SwapLong(P: PInteger; Count: Cardinal); overload;
asm
@@Loop:
  MOV ECX, [EAX]
  BSWAP ECX
  MOV [EAX], ECX
  ADD EAX, 4
  DEC EDX
  JNZ @@Loop
end;

function SwapLong(Value: Cardinal): Cardinal; overload;
asm
  BSWAP EAX
end;

procedure TPackbitsRLE.Decode(var Source: Pointer; Dest: Pointer;
  PackedSize, UnpackedSize: Integer);
var
  SourcePtr, TargetPtr: PByte;
  N: SmallInt;
begin
  TargetPtr := Dest;
  SourcePtr := Source;
  while PackedSize > 0 do
  begin
    N := ShortInt(SourcePtr^);
    Inc(SourcePtr);
    Dec(PackedSize);
    if N < 0 then
    begin
      if N = -128 then
        Continue;
      N := -N + 1;
      FillChar(TargetPtr^, N, SourcePtr^);
      Inc(SourcePtr);
      Inc(TargetPtr, N);
      Dec(PackedSize);
    end
    else
    begin
      Move(SourcePtr^, TargetPtr^, N + 1);
      Inc(TargetPtr, N + 1);
      Inc(SourcePtr, N + 1);
      Dec(PackedSize, N + 1);
    end;
  end;
end;

procedure TPSDGraphic.MakePalette(BPS: Byte; Mode: Integer);
var
  Pal: TMaxLogPalette;
  hpal: HPALETTE;
  I: Integer;
  EntryCount: Word;
begin
  case BPS of
    1:
      EntryCount := 1;
    4:
      EntryCount := 15;
  else
    EntryCount := 255;
  end;

  Pal.palVersion := $300;
  Pal.palNumEntries := 1 + EntryCount;
  case BPS of
    1:
      begin
        Pal.palPalEntry[0].peRed := 255;
        Pal.palPalEntry[0].peGreen := 255;
        Pal.palPalEntry[0].peBlue := 255;
        Pal.palPalEntry[0].peFlags := 0;
        Pal.palPalEntry[1].peRed := 0;
        Pal.palPalEntry[1].peGreen := 0;
        Pal.palPalEntry[1].peBlue := 0;
        Pal.palPalEntry[1].peFlags := 0;
      end;
  else
    case Mode of
      PSD_DUOTONE, PSD_GRAYSCALE:
        for I := 0 to EntryCount do
        begin
          Pal.palPalEntry[I].peRed := I;
          Pal.palPalEntry[I].peGreen := I;
          Pal.palPalEntry[I].peBlue := I;
          Pal.palPalEntry[I].peFlags := 0;
        end;
    else
      for I := 0 to EntryCount do
      begin
        Pal.palPalEntry[I].peRed := FPalette[0 * 256 + I];
        Pal.palPalEntry[I].peGreen := FPalette[1 * 256 + I];
        Pal.palPalEntry[I].peBlue := FPalette[2 * 256 + I];
        Pal.palPalEntry[I].peFlags := 0;
      end;
    end;
  end;
  hpal := CreatePalette(PLogPalette(@Pal)^);
  if hpal <> 0 then
    Palette := hpal;
end;

procedure TPSDGraphic.LoadFromStream(Stream: TStream);
var
  Header: TPSDHeader;
  Count: Integer;
  Compression: Word;
  Decoder: TPackbitsRLE;
  RLELength: array of Word;

  Y: Integer;
  BPS: Integer;
  ChannelSize: Integer;
  RawBuffer, Buffer: Pointer;
  Run1, Run2, Run3, Run4: PByte;
begin
  with Stream do
  begin
    ReadData(@Header, sizeof(TPSDHeader));
    if Header.Signature <> '8BPS' then
      raise Exception.Create('Bad file');
    with Header do
    begin
      Channels := Swap(Channels);
      Rows := SwapLong(Rows);
      Columns := SwapLong(Columns);
      Depth := Swap(Depth);
      Mode := Swap(Mode);
    end;

    case Header.Mode of
      PSD_BITMAP:
        PixelFormat := pf1Bit;
      PSD_DUOTONE, PSD_GRAYSCALE, PSD_INDEXED:
        PixelFormat := pf8Bit;
      PSD_RGB:
        PixelFormat := pf24Bit;
      PSD_CMYK:
        PixelFormat := pf24Bit;
      PSD_MULTICHANNEL:
        ;
      PSD_LAB:
        PixelFormat := pf24Bit;
    end;
    ReadBuffer(Count, sizeof(Count));
    Count := SwapLong(Count);

    if Header.Mode in [PSD_BITMAP, PSD_GRAYSCALE, PSD_INDEXED] then
    begin
      if Header.Mode = PSD_INDEXED then
        ReadBuffer(FPalette, Count);
      MakePalette(Header.Depth, Header.Mode);
    end;

    Width := Header.Columns;
    Height := Header.Rows;

    ReadBuffer(Count, sizeof(Count));
    Count := SwapLong(Count);
    Seek(Count, soFromCurrent);
    ReadBuffer(Count, sizeof(Count));
    Count := SwapLong(Count);
    Seek(Count, soFromCurrent);

    RawBuffer := nil;

    ReadBuffer(Compression, sizeof(Compression));
    Compression := Swap(Compression);
    if Compression = 1 then
    begin
      Decoder := TPackbitsRLE.Create;
      SetLength(RLELength, Header.Rows * Header.Channels);
      ReadBuffer(RLELength[0], 2 * Length(RLELength));
      SwapShort(@RLELength[0], Header.Rows * Header.Channels);
    end
    else
      Decoder := nil;

    try
      case Header.Mode of
        PSD_BITMAP, PSD_DUOTONE, PSD_GRAYSCALE, PSD_INDEXED:
          begin
            if Assigned(Decoder) then
            begin
              Count := 0;
              for Y := 0 to Height - 1 do
                Inc(Count, RLELength[Y]);
              GetMem(RawBuffer, Count);
              ReadBuffer(RawBuffer^, Count);
              Run1 := RawBuffer;
              for Y := 0 to Height - 1 do
              begin
                Count := RLELength[Y];
                Decoder.Decode(Pointer(Run1), ScanLine[Y], Count, Width);
                Inc(Run1, Count);
              end;
              FreeMem(RawBuffer);
            end
            else // Незапакованные данные
              for Y := 0 to Height - 1 do
                ReadBuffer(ScanLine[Y]^, Width);
          end;
        PSD_RGB, PSD_CMYK, PSD_LAB:
          begin
            BPS := Header.Depth div 8;
            ChannelSize := BPS * Width * Height;

            GetMem(Buffer, Header.Channels * ChannelSize);

            if Assigned(Decoder) then
            begin
              Count := 0;
              for Y := 0 to High(RLELength) do
                Inc(Count, RLELength[Y]);
              Count := Count * BPS;
              GetMem(RawBuffer, Count);
              Run1 := RawBuffer;
              ReadBuffer(RawBuffer^, Count);
              Decoder.Decode(RawBuffer, Buffer, Count,
                Header.Channels * ChannelSize);
              FreeMem(RawBuffer);
            end
            else
            begin
              ReadBuffer(Buffer^, Header.Channels * ChannelSize);
              if BPS = 2 then
                SwapShort(Buffer, Header.Channels * ChannelSize div 2);
            end;

            case Header.Mode of
              PSD_RGB:
                begin
                  Run1 := Buffer;
                  Run2 := Run1;
                  Inc(Run2, ChannelSize);
                  Run3 := Run2;
                  Inc(Run3, ChannelSize);
                  for Y := 0 to Height - 1 do
                  begin
                    RGB2BGR(Run1, Run2, Run3, ScanLine[Y], Header.Depth,
                      3 * Width);
                    Inc(Run1, BPS * Width);
                    Inc(Run2, BPS * Width);
                    Inc(Run3, BPS * Width);
                  end;
                end;
              PSD_CMYK:
                begin
                  Run1 := Buffer;
                  for Y := 1 to 4 * ChannelSize do
                  begin
                    Run1^ := 255 - Run1^;
                    Inc(Run1);
                  end;

                  Run1 := Buffer;
                  Run2 := Run1;
                  Inc(Run2, ChannelSize);
                  Run3 := Run2;
                  Inc(Run3, ChannelSize);
                  Run4 := Run3;
                  Inc(Run4, ChannelSize);
                  for Y := 0 to Height - 1 do
                  begin
                    CMYK2BGR(Run1, Run2, Run3, Run4, ScanLine[Y], Header.Depth,
                      4 * Width);
                    Inc(Run1, BPS * Width);
                    Inc(Run2, BPS * Width);
                    Inc(Run3, BPS * Width);
                    Inc(Run4, BPS * Width);
                  end;
                end;
              PSD_LAB:
                begin
                  Run1 := Buffer;
                  Inc(Run1, ChannelSize);
                  for Y := 1 to 2 * ChannelSize do
                  begin
                    Run1^ := Run1^ - 128;
                    Inc(Run1);
                  end;
                  Run1 := Buffer;
                  Run2 := Run1;
                  Inc(Run2, ChannelSize);
                  Run3 := Run2;
                  Inc(Run3, ChannelSize);
                  for Y := 0 to Height - 1 do
                  begin
                    CIELAB2BGR(Run1, Run2, Run3, ScanLine[Y], Header.Depth,
                      3 * Width);
                    Inc(Run1, BPS * Width);
                    Inc(Run2, BPS * Width);
                    Inc(Run3, BPS * Width);
                  end;
                end;
            end;
          end;
      end;
    finally
      Decoder.Free;
    end;
  end;
end;

end.
