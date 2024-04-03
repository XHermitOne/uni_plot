{
Компонент построения графиков.

Версия: 0.0.0.1
}
unit uni_graphic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  FileUtil,
  graphfunc;

type
  TUniGraphPen = class(TCollectionItem)
  private
    // Массив точек
    FGraphData: graphfunc.PGraphData;

  protected
    // Файл - источник данных
    FSrcDataFileName: TFileName;
    // Цвет пера
    FColor: TColor;

    //procedure SetSrcDataFileName(ASrcDataFileName: String);
    procedure SetColor(AColor: TColor);

  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    //procedure Assign(aSource : TPersistent); override;

    // Прочитать файл данных и преобразовать данные в формате данных пера
    function ReadSrcDataFile(ASrcFileName: String = ''): Boolean;
    // Добавить точку в список
    procedure AddPoint(X, Y: Double);

    // Получить графические данные из данных о пере
    property GraphData: PGraphData read FGraphData;
  published
    property SrcDataFileName: TFileName read FSrcDataFileName write FSrcDataFileName;
    property Color: TColor read FColor write SetColor;
  end;

  TUniGraphPens = class(TOwnedCollection)
  private
    function GetDef(AIndex : Integer): TUniGraphPen;
    procedure SetDef(AIndex : Integer; APen: TUniGraphPen);
  protected
  public
    function AddPen(const AColor: TColor; const ASrcDataFileName: String = ''): TUniGraphPen;
    property Definitions[AIndex : Integer] : TUniGraphPen Read GetDef Write SetDef; default;
  end;

  TUniGraphAxisType = (atExp = 0, atTime = 1, atOptimal = 2);

  TUniGraphic = class(TImage)
  private

  protected
    FPrevFrameFileName: String;
    FNextFrameFileName: String;

    // Типы данных осей
    FXType: TUniGraphAxisType;
    FYType: TUniGraphAxisType;

    // Цвета
    FTextColor: TColor;
    FGroundColor: TColor;
    FBorderColor: TColor;
    FGridColor: TColor;
    FAxisColor: TColor;

    // Данные сцены
    FSceneX1: Double;
    FSceneY1: Double;
    FSceneX2: Double;
    FSceneY2: Double;
    Fdx: Double;
    Fdy: Double;

    FPens: TUniGraphPens;

    procedure SetTextColor(AColor: TColor);
    procedure SetGroundColor(AColor: TColor);
    procedure SetBorderColor(AColor: TColor);
    procedure SetGridColor(AColor: TColor);
    procedure SetAxisColor(AColor: TColor);

    procedure SetPens(APens: TUniGraphPens);

    // Отрисовать следующий кадр
    function DrawNextFrame(): String;

  public
    // Конструктор
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Refresh;
    procedure Paint; override;

  published
    property XType: TUniGraphAxisType read FXType write FXType;
    property YType: TUniGraphAxisType read FXType write FXType;

    property SceneX1: Double read FSceneX1 write FSceneX1;
    property SceneY1: Double read FSceneY1 write FSceneY1;
    property SceneX2: Double read FSceneX2 write FSceneX2;
    property SceneY2: Double read FSceneY2 write FSceneY2;

    property dX: Double read Fdx write Fdx;
    property dY: Double read Fdy write Fdy;

    property TextColor: TColor read FTextColor write SetTextColor;
    property GroundColor: TColor read FGroundColor write SetGroundColor;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property GridColor: TColor read FGridColor write SetGridColor;
    property AxisColor: TColor read FAxisColor write SetAxisColor;

    property Pens: TUniGraphPens read FPens write SetPens;
  end;

function GetCgaByColor(AColor: TColor): Byte;

procedure Register;

implementation

uses
  logfunc, filefunc, strfunc, toolfunc;

procedure Register;
begin
  {$I uni_graphic_icon.lrs}
  RegisterComponents('Chart',[TUniGraphic]);
end;

// ---------------- TUniGraphPen ----------------
constructor TUniGraphPen.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  New(FGraphData);
  graphfunc.InitGraphData(FGraphData);
  FColor := clYellow;
end;


destructor TUniGraphPen.Destroy;
begin
  Dispose(FGraphData);

  inherited;
end;


// Прочитать файл данных и преобразовать данные в формате данных пера
function TUniGraphPen.ReadSrcDataFile(ASrcFileName: String): Boolean;
var
  data_file: Text;
  line: String;
  x_str, y_str: String;
  x, y: Double;
begin
  Result := False;

  if strfunc.IsEmptyStr(ASrcFileName) then
    ASrcFileName := FSrcDataFileName;

  if strfunc.IsEmptyStr(ASrcFileName) then
    exit;

  if not FileExists(ASrcFileName) then
  begin
    logfunc.WarningMsgFmt('Файл данных <%s> не найден', [ASrcFileName]);
    exit;
  end;

  AssignFile(data_file, ASrcFileName);
  Reset(data_file);
  while not Eof(data_file) do
  begin
    ReadLn(data_file, line);
    if strfunc.IsStartsWith(line, '#') then
      continue;
    SScanf(line, '%s %s', [@x_str, @y_str]);

    // Парсинг значений X и Y
    if strfunc.IsWordInStr(':', x_str) then
      x := toolfunc.StrTimeToLong(Trim(x_str))
    else
      x := StrToFloat(Trim(x_str));
    y := StrToFloat(Trim(y_str));

    // После того как распарсили x и y добавляем точку
    AddPoint(x, y);
  end;
  Result := True;
end;

//procedure TUniGraphPen.SetSrcDataFileName(ASrcDataFileName: String);
//begin
//  FSrcDataFileName := ASrcDataFileName;
//  if FileExists(FSrcDataFileName) then
//    ReadSrcDataFile(FSrcDataFileName);
//end;


procedure TUniGraphPen.SetColor(AColor: TColor);
begin
  FGraphData^.line_color := GetCgaByColor(AColor);
end;


procedure TUniGraphPen.AddPoint(X, Y: Double);
var
  last_index: Integer;
begin
  last_index := Length(FGraphData^.points);
  SetLength(FGraphData^.points, last_index + 1);

  FGraphData^.points[last_index].x := X;
  FGraphData^.points[last_index].y := Y;
  Inc(FGraphData^.n_points);
end;

// ---------------- TUniGraphPens ----------------
function TUniGraphPens.GetDef(AIndex : Integer): TUniGraphPen;
begin
  Result := TUniGraphPen(Items[AIndex]);
end;

procedure TUniGraphPens.SetDef(AIndex : Integer; APen: TUniGraphPen);
begin
  Items[AIndex] := APen;
end;

function TUniGraphPens.AddPen(const AColor: TColor; const ASrcDataFileName: String = ''): TUniGraphPen;
begin
  Result := Add as TUniGraphPen;
  Result.Color := AColor;
  Result.SrcDataFileName := ASrcDataFileName;
end;


// ---------------- TUniGraphic ----------------
constructor TUniGraphic.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FXType := atOptimal;
  FYType := atOptimal;

  // Данные сцены
  FSceneX1 := 0.0;
  FSceneY1 := 0.0;
  FSceneX2 := 0.0;
  FSceneY2 := 0.0;
  Fdx := 0.0;
  Fdy := 0.0;

  FNextFrameFileName := filefunc.JoinPath([GetTempDir(), Format('%s_next.png', [Name])]);
  FPrevFrameFileName := filefunc.JoinPath([GetTempDir(), Format('%s_prev.png', [Name])]);

  FTextColor := clGreen;
  FGroundColor := clBlack;
  FBorderColor := clGray;
  FGridColor := clGray;
  FAxisColor := clGray;

  FPens := TUniGraphPens.Create(self, TUniGraphPen);
end;

destructor TUniGraphic.Destroy;
begin
  FreeAndNil(FPens);
  inherited;
end;


procedure TUniGraphic.SetTextColor(AColor: TColor);
begin
  if FTextColor <> AColor then
  begin
    FTextColor := AColor;
    Refresh;
  end;
end;


procedure TUniGraphic.SetGroundColor(AColor: TColor);
begin
  if FGroundColor <> AColor then
  begin
    FGroundColor := AColor;
    Refresh;
  end;
end;


procedure TUniGraphic.SetBorderColor(AColor: TColor);
begin
  if FBorderColor <> AColor then
  begin
    FBorderColor := AColor;
    Refresh;
  end;
end;


procedure TUniGraphic.SetGridColor(AColor: TColor);
begin
  if FGridColor <> AColor then
  begin
    FGridColor := AColor;
    Refresh;
  end;
end;


procedure TUniGraphic.SetAxisColor(AColor: TColor);
begin
  if FAxisColor <> AColor then
  begin
    FAxisColor := AColor;
    Refresh;
  end;
end;


procedure TUniGraphic.SetPens(APens: TUniGraphPens);
begin
  if FPens = APens then
    exit;
  FPens.Assign(APens);
end;


function TUniGraphic.DrawNextFrame(): String;
var
  i: Integer;
  pen_list: Array [0..9] of PGraphData;
begin
  Result := '';

  // Инициализировать массив
  for i := 0 to graphfunc.MAX_PEN_COUNT - 1 do
    pen_list[i] := nil;

  if csDesigning in ComponentState then
  begin
    //... код, работающий только в дизайне ...
    graphfunc.DrawPNG(FNextFrameFileName, Ord(FXType), Ord(FYType),
                      Width, Height,
                      FSceneX1, FSceneY1, FSceneX2, FSceneY2, FdX, FdY,
                      GetCgaByColor(FTextColor),
                      GetCgaByColor(FGroundColor),
                      GetCgaByColor(FBorderColor),
                      GetCgaByColor(FGridColor),
                      GetCgaByColor(FAxisColor),
                      pen_list);
  end
  else
  begin
    // Отрисовка в режиме работы программы
    for i := 0  to graphfunc.MAX_PEN_COUNT - 1 do
      if i < FPens.Count then
      begin
        FPens[i].ReadSrcDataFile;
        pen_list[i] := FPens[i].GraphData;
      end
      else
        break;
    graphfunc.DrawPNG(FNextFrameFileName, Ord(FXType), Ord(FYType),
                      Width, Height,
                      FSceneX1, FSceneY1, FSceneX2, FSceneY2, FdX, FdY,
                      GetCgaByColor(FTextColor),
                      GetCgaByColor(FGroundColor),
                      GetCgaByColor(FBorderColor),
                      GetCgaByColor(FGridColor),
                      GetCgaByColor(FAxisColor),
                      pen_list);
  end;

  if FileExists(FNextFrameFileName) then
    Result := FNextFrameFileName;
end;


procedure TUniGraphic.Refresh;
begin
  DrawNextFrame;

  if FileExists(FNextFrameFileName) then
  begin
    CopyFile(FNextFrameFileName, FPrevFrameFileName);
    self.Picture.LoadFromFile(FPrevFrameFileName);
  end;
end;


procedure TUniGraphic.Paint;
begin
  Refresh;
  inherited;
end;

//
function GetCgaByColor(AColor: TColor): Byte;
begin
  Result := 0;

  case AColor of
    Graphics.clBlack: Result := 0; 	// BLACK
    Graphics.clNavy: Result := 1;  	// BLUE
    Graphics.clGreen: Result := 2; 	// GREEN
    Graphics.clTeal: Result := 3;  	// CYAN
    Graphics.clMaroon: Result := 4;	// RED
    Graphics.clPurple: Result := 5;	// MAGENTA
    Graphics.clOlive: Result := 6; 	// BROWN
    Graphics.clSilver: Result := 7;	// LIGHTGRAY
    Graphics.clGray: Result := 8;  	// DARKGRAY
    Graphics.clBlue: Result := 9;  	// LIGHTBLUE
    Graphics.clLime: Result := 10; 	// LIGHTGREEN
    Graphics.clAqua: Result := 11; 	// LIGTHCYAN
    Graphics.clRed: Result := 12; 	    // LIGHTRED
    Graphics.clFuchsia: Result := 13;   // LIGHTMAGENTA
    Graphics.clYellow: Result := 14;	// YELLOW
    Graphics.clWhite: Result := 15; 	// WHITE
  else
    logfunc.WarningMsgFmt('Get default color: %d', [Result]);
  end;
end;

end.
