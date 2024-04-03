unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, csvdataset, DB, BufDataset, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, TAGraph, uni_graphic;

type

  { TForm1 }

  TForm1 = class(TForm)
    UniGraphic1: TUniGraphic;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

