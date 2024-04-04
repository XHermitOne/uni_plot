unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  uni_trend;

type

  { TForm1 }

  TForm1 = class(TForm)
    Timer1: TTimer;
    UniTrend1: TUniTrend;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FStartTime: LongInt;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Timer1Timer(Sender: TObject);
var
  cur_time: LongInt;
  y, last_y: Double;
begin
  cur_time := (DateTimeToTimeStamp(Now()).Time - FStartTime) div 1000;

  last_y := UniTrend1.Pens[0].GraphData^.points[UniTrend1.Pens[0].GraphData^.n_points - 1].y;
  y := Random(10) - 5 + last_y;
  UniTrend1.Pens[0].AddPoint(cur_time, y);

  last_y := UniTrend1.Pens[1].GraphData^.points[UniTrend1.Pens[1].GraphData^.n_points - 1].y;
  y := Random(10) - 5 + last_y;
  UniTrend1.Pens[1].AddPoint(cur_time, y);

  UniTrend1.Refresh;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  FStartTime := DateTimeToTimeStamp(Now()).Time;

  UniTrend1.Pens[0].AddPoint(0, 50);
  UniTrend1.Pens[1].AddPoint(0, 30);
end;

end.

