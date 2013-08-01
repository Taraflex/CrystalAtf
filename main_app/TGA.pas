unit TGA;

interface

uses Windows, SysUtils, Classes, Graphics;

type
  TGA_Header = packed record
    FileType: Byte;
    ColorMapType: Byte;
    ImageType: Byte;
    ColorMapStart: WORD;
    ColorMapLength: WORD;
    ColorMapDepth: Byte;
    OrigX: WORD;
    OrigY: WORD;
    Width: WORD;
    Height: WORD;
    BPP: Byte;
    ImageInfo: Byte;
    Data: pointer;
  end;

function LoadTgaImage(const FileName: string): TBitmap;
procedure SaveTgaImage(const Image32bit: TBitmap; const FileName: string);

implementation

// --------------------------------------------------------------------->>>-----
// ---------------------------- TGA ROUTINE --------------------------->>>-----
// --------------------------------------------------------------------->>>-----

function TgaTest(const Header: TGA_Header): boolean;
begin
  Result := false;

  case Header.ImageType of
    1, 9:
      if (Header.ColorMapType = 1) and (Header.ColorMapDepth = 24) then
      begin
        // norm
      end
      else
        Exit;
  end;

  case Header.BPP of
    32, 24, 16:
      Result := (Header.ImageType = 2) or (Header.ImageType = 10);
    8:
      Result := ((Header.ColorMapType = 1) and (Header.ColorMapDepth = 24)) or
        (Header.ImageType = 3) or (Header.ImageType = 11);
  end;
end;

function inci(var i: integer): integer; inline;
begin
  Result := i;
  i := i + 1;
end;

procedure TGAFlipHorizontally(TGA: TGA_Header; Data: PByteArray);
var
  scanLine: PByteArray;
  i, j, x, w, h, pixelSize: integer;
begin
  w := TGA.Width;
  h := TGA.Height;
  pixelSize := TGA.BPP div 8;

  GetMem(scanLine, w * pixelSize);
  for i := 0 to h - 1 do
  begin
    Move(Data[i * w * pixelSize], scanLine[0], w * pixelSize);
    for x := 0 to w div 2 do
      for j := 0 to pixelSize - 1 do
        scanLine[x * pixelSize + j] := scanLine[(w - 1 - x) * pixelSize + j];
    Move(scanLine[0], Data[i * w * pixelSize], w * pixelSize);
  end;
  FreeMem(scanLine);
end;

procedure TGAFlipVertically(TGA: TGA_Header; Data: PByteArray);
var
  scanLine: PByteArray;
  i, w, h, pixelSize: integer;
begin
  w := TGA.Width;
  h := TGA.Height;
  pixelSize := TGA.BPP div 8;
  GetMem(scanLine, w * pixelSize);

  for i := 0 to h div 2 - 1 do
  begin
    Move(Data[i * w * pixelSize], scanLine[0], w * pixelSize);
    Move(Data[(h - i - 1) * w * pixelSize], Data[i * w * pixelSize],
      w * pixelSize);
    Move(scanLine[0], Data[(h - i - 1) * w * pixelSize], w * pixelSize);
  end;
  FreeMem(scanLine);
end;

procedure TGA_GetPackets(F: TStream; Data: PByteArray;
  Width, Height, depth: WORD);
var
  current_byte, run_length, i: integer;
  buffer8: array [0 .. 3] of Byte;
  buffer16: WORD;
  BPP: Byte;
  Header: Byte;
begin
  current_byte := 0;

  if depth = 16 then
    BPP := 3
  else
    BPP := depth div 8;

  while current_byte < Width * Height * BPP do
  begin
    // BlockRead(F, header, 1);
    F.Read(Header, 1);
    run_length := (Header and $7F) + 1;

    if (Header and $80) <> 0 then
    begin
      if depth = 32 then
        // BlockRead(F, buffer8[0], 4);
        F.Read(buffer8[0], 4);

      if depth = 24 then
        // BlockRead(F, buffer8[0], 3);
        F.Read(buffer8[0], 3);

      if depth = 16 then
        // BlockRead(F, buffer16, 2);
        F.Read(buffer16, 2);

      if depth = 8 then
        // BlockRead(F, buffer8[0], 1);
        F.Read(buffer8[0], 1);

      for i := 0 to run_length - 1 do
      begin
        if depth = 32 then
        begin
          Data[inci(current_byte)] := buffer8[0];
          Data[inci(current_byte)] := buffer8[1];
          Data[inci(current_byte)] := buffer8[2];
          Data[inci(current_byte)] := buffer8[3];
        end;

        if depth = 24 then
        begin
          Data[inci(current_byte)] := buffer8[0];
          Data[inci(current_byte)] := buffer8[1];
          Data[inci(current_byte)] := buffer8[2];
        end;

        if depth = 16 then
        begin
          Data[inci(current_byte)] := (buffer16 and $1F) shl 3;
          Data[inci(current_byte)] := ((buffer16 shr 5) and $1F) shl 3;
          Data[inci(current_byte)] := ((buffer16 shr 10) and $1F) shl 3;
        end;

        if depth = 8 then
          Data[inci(current_byte)] := buffer8[0];
      end;
    end;

    if (Header and $80) = 0 then
    begin
      for i := 0 to run_length - 1 do
      begin
        if depth = 32 then
        begin
          // BlockRead(F, buffer8[0], 4);
          F.Read(buffer8[0], 4);
          Data[inci(current_byte)] := buffer8[0];
          Data[inci(current_byte)] := buffer8[1];
          Data[inci(current_byte)] := buffer8[2];
          Data[inci(current_byte)] := buffer8[3];
        end;

        if depth = 24 then
        begin
          F.Read(buffer8[0], 3);
          Data[inci(current_byte)] := buffer8[0];
          Data[inci(current_byte)] := buffer8[1];
          Data[inci(current_byte)] := buffer8[2];
        end;

        if depth = 16 then
        begin
          // BlockRead(F, buffer16, 2);
          F.Read(buffer16, 2);
          Data[inci(current_byte)] := (buffer16 and $1F) shl 3;
          Data[inci(current_byte)] := ((buffer16 shr 5) and $1F) shl 3;
          Data[inci(current_byte)] := ((buffer16 shr 10) and $1F) shl 3;
        end;

        if depth = 8 then
        begin
          // BlockRead(F, buffer8[0], 1);
          F.Read(buffer8[0], 1);
          Data[inci(current_byte)] := buffer8[0];
        end;

      end;
    end;
  end;
end;

function TGA_GetData(F: TStream): TGA_Header;
var
  buffer1: PByteArray;
  buffer2: PWordArray;
  i: integer;
  ColorMap: PByteArray;
  TGA: TGA_Header;
begin
  // TGA :=@Result;

  F.Read(TGA, sizeof(TGA) - 4);
  F.Position := sizeof(TGA) - 4 + TGA.FileType;
  ColorMap := nil;

  case TGA.ImageType of
    1:
      if (TGA.ColorMapType = 1) and (TGA.ColorMapDepth = 24) then
      begin
        GetMem(ColorMap, TGA.ColorMapLength * (TGA.ColorMapDepth div 8));
        // BlockRead(F, ColorMap[0], TGA.ColorMapLength*(TGA.ColorMapDepth div 8));
        F.Read(ColorMap[0], TGA.ColorMapLength * (TGA.ColorMapDepth div 8));
      end
      else
      begin
        // CloseFile(F);
        Exit;
      end;

    9:
      if (TGA.ColorMapType = 1) and (TGA.ColorMapDepth = 24) then
      begin
        GetMem(ColorMap, TGA.ColorMapLength * (TGA.ColorMapDepth div 8));
        // BlockRead(F, ColorMap[0], TGA.ColorMapLength*(TGA.ColorMapDepth div 8));
        F.Read(ColorMap[0], TGA.ColorMapLength * (TGA.ColorMapDepth div 8));
      end
      else
      begin
        // CloseFile(F);
        Exit;
      end;
  end;

  case TGA.BPP of
    32:
      begin
        GetMem(TGA.Data, TGA.Width * TGA.Height * 4);

        if TGA.ImageType = 2 then
          // BlockRead(F, TGA.Data^, TGA.Width * TGA.Height * 4)
          F.Read(TGA.Data^, TGA.Width * TGA.Height * 4)
        else if TGA.ImageType = 10 then
          TGA_GetPackets(F, TGA.Data, TGA.Width, TGA.Height, TGA.BPP);
      end;

    24:
      begin
        GetMem(TGA.Data, TGA.Width * TGA.Height * 3);

        if TGA.ImageType = 2 then
          // BlockRead(F, TGA.Data^, TGA.Width*TGA.Height*3)
          F.Read(TGA.Data^, TGA.Width * TGA.Height * 3)
        else if TGA.ImageType = 10 then
          TGA_GetPackets(F, TGA.Data, TGA.Width, TGA.Height, TGA.BPP);
      end;

    16:
      begin
        GetMem(TGA.Data, TGA.Width * TGA.Height * 3);

        if TGA.ImageType = 2 then
        begin
          GetMem(buffer2, 2 * TGA.Width * TGA.Height);
          // BlockRead(F, buffer2[0], 2 * TGA.Width * TGA.Height);
          F.Read(buffer2[0], 2 * TGA.Width * TGA.Height);

          for i := 0 to TGA.Width * TGA.Height - 1 do
          begin
            PByteArray(TGA.Data)[3 * i] := (buffer2[i] and $1F) shl 3;
            PByteArray(TGA.Data)[3 * i + 1] :=
              ((buffer2[i] shr 5) and $1F) shl 3;
            PByteArray(TGA.Data)[3 * i + 2] :=
              ((buffer2[i] shr 10) and $1F) shl 3;
          end;

          FreeMem(buffer2);
          TGA.BPP := 24;
        end
        else if TGA.ImageType = 10 then
        begin
          TGA_GetPackets(F, TGA.Data, TGA.Width, TGA.Height, TGA.BPP);
          TGA.BPP := 24;
        end;
      end;

    8:
      begin
        GetMem(TGA.Data, TGA.Width * TGA.Height * 3);
        GetMem(buffer1, TGA.Width * TGA.Height);

        if (TGA.ColorMapType = 1) and (TGA.ColorMapDepth = 24) then
        begin
          if TGA.ImageType = 9 then
            TGA_GetPackets(F, buffer1, TGA.Width, TGA.Height, TGA.BPP)
          else
            // BlockRead(F, buffer1[0], TGA.Width * TGA.Height);
            F.Read(buffer1[0], TGA.Width * TGA.Height);

          For i := 0 to TGA.Width * TGA.Height - 1 do
          begin
            PByteArray(TGA.Data)[3 * i] := ColorMap[3 * buffer1[i]];
            PByteArray(TGA.Data)[3 * i + 1] := ColorMap[3 * buffer1[i] + 1];
            PByteArray(TGA.Data)[3 * i + 2] := ColorMap[3 * buffer1[i] + 2];
          end;

          FreeMem(ColorMap);
        end
        else
        begin
          if TGA.ImageType = 3 then
            // BlockRead(F, Buffer1[0], TGA.Width * TGA.Height)
            F.Read(buffer1[0], TGA.Width * TGA.Height)
          else if TGA.ImageType = 11 then
            TGA_GetPackets(F, buffer1, TGA.Width, TGA.Height, TGA.BPP);

          for i := 0 to TGA.Width * TGA.Height - 1 do
          begin
            PByteArray(TGA.Data)[3 * i] := buffer1[i];
            PByteArray(TGA.Data)[3 * i + 1] := buffer1[i];
            PByteArray(TGA.Data)[3 * i + 2] := buffer1[i];
          end;
        end;

        FreeMem(buffer1);
        TGA.BPP := 24;
      end;
  end;
  // CloseFile(F);

  if (TGA.ImageInfo and (1 shl 4)) <> 0 then
    TGAFlipHorizontally(TGA, TGA.Data);
  if (TGA.ImageInfo and (1 shl 5)) <> 0 then
    TGAFlipVertically(TGA, TGA.Data);

  // Result := true;
  Result := TGA;
end;

function TestAlpha(P: pointer; WidthHeight: DWORD): boolean;
asm
  push ecx
  mov ecx, edx
  mov edx, eax
  add edx, 3
@loop:
  mov  al, [edx]
  inc al
  jnz @true
  add edx, 4
  dec ecx
  jnz @loop

  // @false:
  pop ecx
  xor eax, eax
  ret

@true:
  pop ecx
  mov eax, 1
end;

function LoadTgaImage(const FileName: string): TBitmap;
var
  F: TFileStream;
  TGA: TGA_Header;
  Alpha: boolean;
  SelfData, TgaData: pointer;
  SelfWidthSize, TgaWidthSize, i, j, k: integer;
begin
  Result := nil;

  F := TFileStream.Create(FileName, fmOpenRead);
  try
    if (F.Read(TGA, sizeof(TGA)) <> sizeof(TGA)) then
      Exit;
    if (not TgaTest(TGA)) then
      Exit;
    F.Position := 0;

    try
      TGA.Data := nil;
      TGA := TGA_GetData(F);
    except
      TGA.Data := nil;
    end;

    if (TGA.Data = nil) then
      Exit;

    try
      Result := TBitmap.Create;
      Alpha := (TGA.BPP = 32) and TestAlpha(TGA.Data, TGA.Width * TGA.Height);
      if Alpha then
        Result.PixelFormat := pf32bit
      else
        Result.PixelFormat := pf24bit;
      Result.Width := TGA.Width;
      Result.Height := TGA.Height;

      SelfData := Result.scanLine[TGA.Height - 1];
      SelfWidthSize := (TGA.Width * (3 + ord(Alpha)) + 3) and -4;
      TgaData := TGA.Data;
      if (TGA.BPP = 32) then
        TgaWidthSize := TGA.Width * 4
      else
        TgaWidthSize := TGA.Width * 3;

      if Alpha or ((TGA.BPP = 24) and (SelfWidthSize = TgaWidthSize)) then
        CopyMemory(SelfData, TgaData, SelfWidthSize * TGA.Height)
      else if (TGA.BPP = 24) then
      begin
        // несочетаются offet-ы
        for i := 1 to TGA.Height do
        begin
          CopyMemory(SelfData, TgaData, SelfWidthSize);
          inc(integer(SelfData), SelfWidthSize);
          inc(integer(TgaData), TgaWidthSize);
        end;
      end
      else
      begin
        k := (4 - SelfWidthSize mod 4) mod 4;

        // самый сложный вариант: 32->24
        for i := 1 to TGA.Height - 1 do
        begin
          for j := 1 to TGA.Width do
          begin
            pdword(SelfData)^ := pdword(TgaData)^;
            inc(integer(SelfData), 3);
            inc(integer(TgaData), 4);
          end;

          inc(integer(SelfData), k);
        end;

        for j := 1 to TGA.Width - 1 do
        begin
          pdword(SelfData)^ := pdword(TgaData)^;
          inc(integer(SelfData), 3);
          inc(integer(TgaData), 4);
        end;

        CopyMemory(SelfData, TgaData, 3);
      end;

    finally
      FreeMem(TGA.Data);
    end;

  finally
    F.Free;
  end;

end;

procedure SaveTgaImage(const Image32bit: TBitmap; const FileName: string);
var
  F: TFileStream;
  Header: TGA_Header;
begin
  FillChar(Header, sizeof(Header), 0);
  Header.ImageType := 2;
  Header.BPP := 32;
  Header.Width := Image32bit.Width;
  Header.Height := Image32bit.Height;

  F := TFileStream.Create(FileName, fmCreate);
  try
    F.Write(Header, sizeof(Header) - 4);
    F.Write(Image32bit.scanLine[Header.Height - 1]^,
      Header.Width * Header.Height * 4 { sizeof(TBGRA) } );
  finally
    F.Free;
  end;
end;

// ------<<<--------------------------------------------------------------------
// -----<<<-------------------- TGA ROUTINE -----------------------------------
// ------<<<--------------------------------------------------------------------

end.
