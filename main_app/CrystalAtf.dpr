{
  2013-07-27
  Taratin Alexander
  Public domain
}

program CrystalAtf;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  ShareMem,
  System.SysUtils,
  Main in 'Main.pas',
  CrystalDXT in 'CrystalDXT.pas',
  FastAlg in 'FastAlg.pas',
  TPSDGraphics in 'TPSDGraphics.pas',
  TGA in 'TGA.pas';

var
  format: AtfFormat = Auto;
  j: uint32;
  curentdir: string;
  curentfile: string;
  sr: TSearchRec;
  sleeptime: uint32 = 1000000;
  slpos: integer = -1;
  targetfile: string;

procedure normalizePath(var str: string);
var
  i: uint32;
begin
  for i := 0 to Length(str) do
    if str[i] = '/' then
      str[i] := '\';
end;

begin
  try
    curentdir := ExtractFilePath(ParamStr(0));
    for j := 1 to ParamCount do
    begin
      if LowerCase(ParamStr(j)) = '-dxt1' then
        format := DXT1
      else if LowerCase(ParamStr(j)) = '-dxt5' then
        format := DXT5
      else if LowerCase(ParamStr(j)) = '-rgba' then
        format := RGB
      else if LowerCase(ParamStr(j)) = '-auto' then
        format := Auto
      else if LowerCase(ParamStr(j)) = '-dds' then
        isDds := true
      else if LowerCase(ParamStr(j)) = '-atf' then
        isDds := false
      else if LowerCase(ParamStr(j)) = '-nomip' then
        nomip := true
      else if LowerCase(ParamStr(j)) = '-mip' then
        nomip := false
      else
      begin
        curentfile := ParamStr(j);
        normalizePath(curentfile);

        if (pos(curentfile, ':') = 0) then
          curentfile := curentdir + curentfile;

        targetfile := '';
        slpos := pos('->', curentfile);
        if slpos > 3 then
        begin
          targetfile := copy(curentfile, slpos + 2,
            curentfile.Length - slpos - 1);

          if pos(targetfile, ':') = 0 then
            targetfile := curentdir + targetfile;

          curentfile := copy(curentfile, 0, slpos - 1);

          ConvertToATF(curentfile, targetfile, format);
        end
        else if FindFirst(curentfile, faNormal, sr) = 0 then
        begin
          repeat
            ConvertToATF(ExtractFilePath(curentfile) + sr.Name, '', format);
          until FindNext(sr) <> 0;
          FindClose(sr);
        end;
      end;
    end;
    writeln('Complete...');
    sleep(sleeptime);
  except
  end;

end.
