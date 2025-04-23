unit USound;
///
/// MineSweeperD12 (RAD Programmer Challenge #1))
///
/// Unit for paly sound
///
/// Alex Nevskiy 2025-04-20
///
///
interface

uses MMSystem, CLasses, UUTility;

type
  TSoundClass = class

  private
    FSounds: array[1..csndMax] of pointer;
    procedure FreeSoundMemory;
  public
    function LoadSounds(aStyle: string): boolean;
    function PlaySound(aSound: integer): boolean;
    constructor Create;
    destructor Destroy; override;
  end;


implementation
uses SysUtils;

{ TSoundClass }

constructor TSoundClass.Create;
begin
  for var Idx: integer := Low(FSounds) to High(FSounds) do
    FSounds[Idx] := nil;
end;


destructor TSoundClass.Destroy;
begin
  FreeSoundMemory;
  inherited;
end;


procedure TSoundClass.FreeSoundMemory;
begin
  for var Idx: integer := Low(FSounds) to High(FSounds) do
    if FSounds[Idx] <> nil then
    begin
      FreeMem(FSounds[Idx]);
      FSounds[Idx] := nil;
    end;
end;


function TSoundClass.LoadSounds(aStyle: string): boolean;
begin
  FreeSoundMemory;
  for var Idx: integer := Low(FSounds) to High(FSounds) do
  begin
    var Res: TResourceStream := TResourceStream.Create(HInstance, format(csResLoad, [aStyle, 'snd', Idx]), 'WAV');
    try
      Res.Position := 0;
      //var P: Pointer;
      GetMem(FSounds[Idx], Res.Size);
      Res.Read(FSounds[Idx]^, Res.Size);
    finally
      FreeAndNil(Res);
    end;
end;
end;


function TSoundClass.PlaySound(aSound: integer): boolean;
begin
  SndPlaySound(FSounds[aSound], SND_MEMORY or SND_ASYNC);
end;

end.
