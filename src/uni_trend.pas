{
Компонент построения временных графиков/трендов.

Версия: 0.0.0.1
}
unit uni_trend;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, uni_graphic;

type
  TUniTrend = class(TUniGraphic)
  private

  protected

  public
    // Конструктор
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property XType;
    property YType;

    property SceneX1;
    property SceneY1;
    property SceneX2;
    property SceneY2;

    property dX;
    property dY;

    property TextColor;
    property GroundColor;
    property BorderColor;
    property GridColor;
    property AxisColor;

    property Pens;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I uni_trend_icon.lrs}
  RegisterComponents('Chart',[TUniTrend]);
end;

// ---------------- TUniTrend ----------------
constructor TUniTrend.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FXType := atTime;
end;

destructor TUniTrend.Destroy;
begin
  inherited;
end;

end.
