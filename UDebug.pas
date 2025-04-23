unit UDebug;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, UUtility, UGameGrid;

type
  TDebugForm = class(TForm)
    memDebug: TMemo;
  private
    { Private declarations }
  public
    procedure ShowDebug(GameGrid:TGameGridClass; n: integer);
  end;

var
  DebugForm: TDebugForm;

implementation
uses StrUtils;

{$R *.dfm}

{ TDebugForm }

procedure TDebugForm.ShowDebug(GameGrid:TGameGridClass; n: integer);
const cspc = ' . ';
var s: string;
    m, f, c: integer;
begin
  memDebug.Clear;
  m := 0; f := 0; c := 0;
  for var y: integer := 0 to GameGrid.FGridData.Height - 1 do
  begin
    s := '';
    for var x: integer := 0 to GameGrid.FGridData.Width - 1 do
    begin
      case n of
        0: s := s + IfThen(GameGrid.MineField[x, y].Flag, '[F]', cspc);
        1: s := s + IfThen(GameGrid.MineField[x, y].Mine, 'M', cspc);
        2: s := s + IfThen(GameGrid.MineField[x, y].Covered, 'C', cspc);
        3: s := s + IfThen(GameGrid.MineField[x, y].DownCheck, ' D ', cspc);
      end;
      if GameGrid.MineField[x, y].Flag then inc(f);
      if GameGrid.MineField[x, y].Mine then inc(m);
      if GameGrid.MineField[x, y].Covered then inc(c);
      //s := s + IfThen(GameGrid.MineField[x, y].Flag, '[', '.');
      //s := s + IfThen(GameGrid.MineField[x, y].Mine, 'M', cspc);
      //s := s + IfThen(GameGrid.MineField[x, y].Covered, ']', ',');
    end;
    memDebug.Lines.Add(s);
  end;
  memDebug.Lines.Add('');
  memDebug.Lines.Add(format('Mines: %d', [m]));
  memDebug.Lines.Add(format('Flags: %d', [f]));
  memDebug.Lines.Add(format('Cover: %d', [c]));
  show;
end;

end.
