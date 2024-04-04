unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  uni_trend;

type

  { TForm1 }

  TForm1 = class(TForm)
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    Timer1: TTimer;
    UniTrend1: TUniTrend;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  FStartTime := DateTimeToTimeStamp(Now()).Time;

  UniTrend1.Pens[0].AddPoint(0, 50);
  UniTrend1.Pens[1].AddPoint(0, 30);
end;

procedure TForm1.SpeedButton10Click(Sender: TObject);
begin
  UniTrend1.SceneX2 := UniTrend1.SceneX1 + (UniTrend1.SceneX2 - UniTrend1.SceneX1) * 2;
  UniTrend1.Refresh;
end;

procedure TForm1.SpeedButton11Click(Sender: TObject);
begin
  UniTrend1.SceneX1 := UniTrend1.SceneX1 + UniTrend1.dX;
  UniTrend1.SceneX2 := UniTrend1.SceneX2 + UniTrend1.dX;
  UniTrend1.Refresh;
end;

procedure TForm1.SpeedButton12Click(Sender: TObject);
begin
  UniTrend1.SceneX2 := 1500;
  UniTrend1.SceneX1 := UniTrend1.SceneX2 - 1000;

  UniTrend1.Refresh;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  UniTrend1.SceneY2 := 150;
  UniTrend1.SceneY1 := UniTrend1.SceneY2 - 100;
  UniTrend1.Refresh;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  UniTrend1.SceneY1 := 0;
  UniTrend1.SceneY2 := UniTrend1.SceneY1 + 100;
  UniTrend1.Refresh;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  UniTrend1.SceneY1 := UniTrend1.SceneY1 + UniTrend1.dY;
  UniTrend1.SceneY2 := UniTrend1.SceneY2 + UniTrend1.dY;
  UniTrend1.Refresh;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
  UniTrend1.SceneY1 := UniTrend1.SceneY1 - UniTrend1.dY;
  UniTrend1.SceneY2 := UniTrend1.SceneY2 - UniTrend1.dY;
  UniTrend1.Refresh;
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
begin
  UniTrend1.SceneY2 := UniTrend1.SceneY1 + (UniTrend1.SceneY2 - UniTrend1.SceneY1) / 2;
  UniTrend1.Refresh;
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);
begin
  UniTrend1.SceneY2 := UniTrend1.SceneY1 + (UniTrend1.SceneY2 - UniTrend1.SceneY1) * 2;
  UniTrend1.Refresh;
end;

procedure TForm1.SpeedButton7Click(Sender: TObject);
begin
  UniTrend1.SceneX1 := 0;
  UniTrend1.SceneX2 := UniTrend1.SceneX1 + 1000;
  UniTrend1.Refresh;
end;

procedure TForm1.SpeedButton8Click(Sender: TObject);
begin
  UniTrend1.SceneX1 := UniTrend1.SceneX1 - UniTrend1.dX;
  UniTrend1.SceneX2 := UniTrend1.SceneX2 - UniTrend1.dX;
  UniTrend1.Refresh;
end;

procedure TForm1.SpeedButton9Click(Sender: TObject);
begin
  UniTrend1.SceneX2 := UniTrend1.SceneX1 + (UniTrend1.SceneX2 - UniTrend1.SceneX1) / 2;
  UniTrend1.Refresh;
end;

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

end.

