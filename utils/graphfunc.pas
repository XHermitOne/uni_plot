{
Модуль функций отрисовки графиков

Версия: 0.0.0.1
}
unit graphfunc;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Graphics, CairoCanvas,
  exttypes;

const
  // Axis status
  AXIS_OFF: Boolean = False;
  AXIS_ON: Boolean = True;

  // Numbers status
  NUMBER_OFF: Boolean = False;
  NUMBER_ON: Boolean = True;

  // Grid status
  GRID_OFF: Boolean = False;
  GRID_ON: Boolean = True;

  // Originate grid or NO
  GRID_FIXED: Boolean = False;
  GRID_ORIGIN: Boolean = True;

  // Grid numbers modes
  GM_EXP: Byte = 0;
  GM_TIME: Byte = 1;
  GM_OPTIMAL: Byte = 2;

  // Clear graphical screen before plotting or not
  CLEAR: Boolean = False;
  NOCLEAR: Boolean = True;

  // Data type
  NOSEQ: Boolean = False;
  SEQX: Boolean = True;

  MINX: Integer = 0;
  MINY: Integer = 0;
  MAXX: Integer = 1920;
  MAXY: Integer = 1080;

  // Размеры картинки по умолчанию
  DEFAULT_WIDTH: Integer = 640;
  DEFAULT_HEIGHT: Integer = 480;

  // Направление
  HORIZ_DIRECTION: Boolean = False;   // Горизонтальное направление 
  VERT_DIRECTION: Boolean = True;     // Вертикальное направление 

  // Имя результирующего файла по умолчанию
  DEFAULT_OUTPUT_PNG_FILENAME: AnsiString = './output.png';

type

  // Цвета элементов графика
  TGraphColor= record
    text: Byte;
    ground: Byte;
    border: Byte;
    grid: Byte;
    axis: Byte;
    line: Byte;
  end;
  PGraphColor = ^TGraphColor;

  // Состояния элементов графика
  TGraphStatus = record
    axis_x: Boolean;
    axis_y: Boolean;

    grid_x: Boolean;
    grid_y: Boolean;

    number_x: Boolean;
    number_y: Boolean;

    clear: Boolean; 

    x_type: Byte;
    y_type: Byte;

    origin: Boolean; 
    dtype: Boolean; 

    line: Byte; 
  end;
  PGraphStatus = ^TGraphStatus;

  // Точка графика
  TGraphPoint = record
    x: Double;
    y: Double;
  end;
  PGraphPoint = ^TGraphPoint;

  // Данные графика
  PGraphData = ^TGraphData;

  TGraphData = record
    status: PGraphStatus;
    color: PGraphColor;

    // Функция получения координат точки по ее индексу
    GetPoint: procedure(AGraphData: PGraphData; X: PDouble; Y: PDouble; AIndex: LongInt);

    n_points: LongInt; // Количество точек
    points: Array of TGraphPoint;  // Точки графика
    
    x1, y1, x2, y2: Double;  // Диапазон данных графика (Сцена)
    canvas_x1, canvas_y1, canvas_x2, canvas_y2: Integer;  // Графическая граница области графика
  end;

 
  PCairoPngCanvas = ^TCairoPngCanvas;
  // График
  TGraph = record
    dX: Double;
    dY: Double;
    canvas_x1, canvas_y1, canvas_x2, canvas_y2: Integer;  // Размеры самого графика
    graph_data: PGraphData;

    canvas: PCairoPngCanvas;
  end;
  PGraph = ^TGraph;

var
  LGStatus: TGraphStatus = (axis_x: True;
  			                axis_y: True;

			                grid_x: True;
			                grid_y: True;

                            number_x: True;
			                number_y: True;

			                clear: False;

			                x_type: 1;
			                y_type: 2;

			                origin: True;
			                dtype: False;

			                line: 2);

const
  // Цвета CGA
  BLACK_COLOR: Byte              = 0;
  BLUE_COLOR: Byte               = 1;
  GREEN_COLOR: Byte              = 2;
  CYAN_COLOR: Byte               = 3;
  RED_COLOR: Byte                = 4;
  MAGENTA_COLOR: Byte            = 5;
  BROWN_COLOR: Byte              = 6;
  LIGHTGRAY_COLOR: Byte          = 7;
  DARKGRAY_COLOR: Byte           = 8;
  LIGHTBLUE_COLOR: Byte          = 9;
  LIGHTGREEN_COLOR: Byte         = 10;
  LIGTHCYAN_COLOR: Byte          = 11;
  LIGHTRED_COLOR: Byte           = 12;
  LIGHTMAGENTA_COLOR: Byte       = 13;
  YELLOW_COLOR: Byte             = 14;
  WHITE_COLOR: Byte              = 15;

var
  // Цвета режима графического вывода
  LGColor: TGraphColor = (text: 3; ground: 0; border: 8; grid: 8; axis: 8; line: 14);

  // Цвета режима печати
  LPColor: TGraphColor = (text: 0; ground: 15; border: 0; grid: 0; axis: 0; line: 0);

var
  LGraphData: TGraphData = (status: @LGStatus;
		                    color: @LGColor;

			                GetPoint: nil;
                            n_points: 0;

			                points: nil;

			                x1: 0;
			                y1: 0;
			                x2: 320;
			                y2: 120;
			                canvas_x1: 0;
			                canvas_y1: 0;
			                canvas_x2: 639;
			                canvas_y2: 479);

// Функции обработки графика
procedure SetCgaColor(AGraph: PGraph; AColor: Byte);
procedure OutTextXY(AGraph: PGraph; x, y: Integer; AText: AnsiString; AOrient: Boolean);
procedure SetDashLineStyle(AGraph: PGraph);
procedure SetSolidLineStyle(AGraph: PGraph);

procedure GetPoint(AGraphData: PGraphData; X: PDouble; Y: PDouble; ACount: LongInt);
function Draw(AGraph: PGraph; AGraphData: PGraphData; AIsPrintMode: Boolean): Boolean;
procedure CheckGraph(AGraph: PGraph);
procedure CheckCoords(X1, X2: PInteger; AMinX, AMaxX, dX: Integer);
procedure DrawGrid(AGraph: PGraph);
procedure DrawAxis(AGraph: PGraph);
function DrawGraph(AGraph: PGraph): Boolean;
function StepX(AGraph: PGraph): Double;
function StepY(AGraph: PGraph): Double;
function Step(AGraph: PGraph; ASt: Double; AType: Byte): Double;
// function MostLeftPoint(AGraph: PGraph; X: PDouble; Y: PDouble): LongInt;
procedure OutGridNumber(AGraph: PGraph; X: Integer; Y: Integer; ANumber: Double; AOrient: Boolean);
function SplitGraph(AGraph: PGraph; x1, y1, x2, y2: PDouble): Boolean;
function CrossPoint(x1, y1, x2, y2: PDouble; x: Double; AMode: Integer): Boolean;

// Функции общего назначения
procedure SwapInteger(ASrc, ADst: PInteger);
procedure SwapDouble(ASrc, ADst: PDouble);

// Инициализация структуры графика
function InitGraph(AGraph: PGraph; AGraphData: PGraphData; ACAnvas: PCairoPngCanvas;
                   AWidth, AHeight: Integer;
                   DX, DY: Double): PGraph;

// Инициализация структуры данных графика
function InitGraphData(AGraphData: PGraphData; x1, y1, x2, y2: Double): PGraphData;

// Выполнить отрисовку графика в PNG файл
//  APNGFileName - Полное имя PNG файла
function DrawPNG(APNGFileName: AnsiString; AGraphData: PGraphData; AXType, AYType: Byte;
                 AWidth, AHeight: Integer;
                 ASceneX1, ASceneY1, ASceneX2, ASceneY2, dX, dY: Double): Boolean;


implementation

uses
  math, strfunc, logfunc;

// Установить текущий цвет отрисовки
procedure SetCgaColor(AGraph: PGraph; AColor: Byte);
begin
  logfunc.InfoMsgFmt('Set CGA color: %d', [AColor]);

  case AColor of
    0: AGraph^.canvas^.Pen.Color := Graphics.clBlack; 	// BLACK
    1: AGraph^.canvas^.Pen.Color := Graphics.clNavy;  	// BLUE
    2: AGraph^.canvas^.Pen.Color := Graphics.clGreen;  	// GREEN
    3: AGraph^.canvas^.Pen.Color := Graphics.clTeal;  	// CYAN
    4: AGraph^.canvas^.Pen.Color := Graphics.clMaroon; 	// RED
    5: AGraph^.canvas^.Pen.Color := Graphics.clPurple; 	// MAGENTA
    6: AGraph^.canvas^.Pen.Color := Graphics.clOlive;  	// BROWN
    7: AGraph^.canvas^.Pen.Color := Graphics.clSilver; 	// LIGHTGRAY
    8: AGraph^.canvas^.Pen.Color := Graphics.clGray;  	// DARKGRAY
    9: AGraph^.canvas^.Pen.Color := Graphics.clBlue;  	// LIGHTBLUE
    10: AGraph^.canvas^.Pen.Color := Graphics.clLime; 	// LIGHTGREEN
    11: AGraph^.canvas^.Pen.Color := Graphics.clAqua; 	// LIGTHCYAN
    12: AGraph^.canvas^.Pen.Color := Graphics.clRed; 	// LIGHTRED
    13: AGraph^.canvas^.Pen.Color := Graphics.clFuchsia;// LIGHTMAGENTA
    14: AGraph^.canvas^.Pen.Color := Graphics.clYellow;	// YELLOW
    15: AGraph^.canvas^.Pen.Color := Graphics.clWhite; 	// WHITE
  else
    logfunc.WarningMsgFmt('Incorrect CGA color: %d', [AColor]);
  end;
end;

// Вывод текста
procedure OutTextXY(AGraph: PGraph; x, y: Integer; AText: AnsiString; AOrient: Boolean);
begin
  //cairo_text_extents_t te;
  //cairo_text_extents(graph->cr, text, &te);

  if AOrient = HORIZ_DIRECTION then
    // cairo_move_to(graph->cr, x - te.width - te.x_bearing, y - te.height / 2 - te.y_bearing);
    AGraph^.canvas^.TextOut(x, y, AText)
  else
    if AOrient = VERT_DIRECTION then
    begin
      // cairo_move_to(graph->cr, x - te.height / 2 - te.y_bearing, y + te.width + te.x_bearing);

      // cairo_save(graph->cr);
      // cairo_rotate(graph->cr, - M_PI / 2);
      AGraph^.canvas^.EndDoc;
      // AGraph^.canvas^.Orientation := ;

      // cairo_show_text(graph->cr, text);
      AGraph^.canvas^.TextOut(x, y, AText);

      // cairo_restore(graph->cr);
    end;
end;


// Установить точечный стиль отрисовки линий
procedure SetDashLineStyle(AGraph: PGraph);
//var
//  dash_ink: Double = 1.0;
//  dash_skip: Double = 3.0;
//  ndash: Integer;
//  offset: Double = -50.0;
begin
  //ndash  = sizeof(dashes) / sizeof(dashes[0]);
  //
  //cairo_set_dash(graph->cr, dashes, ndash, offset);
  //cairo_set_line_width(graph->cr, 1.0);

  AGraph^.canvas^.Pen.Style := psDash;
  AGraph^.canvas^.Pen.Width := 1;
end;


// Установить нормальный стиль отрисовки линий
procedure SetSolidLineStyle(AGraph: PGraph);
//var
//  dash_ink: Double = 1.0;
//  dash_skip: Double = 3.0;
begin
  //cairo_set_dash(graph->cr, dashes, 0, 0);
  //cairo_set_line_width(graph->cr, 1.0);
  AGraph^.canvas^.Pen.Style := psSolid;
  AGraph^.canvas^.Pen.Width := 1;
end;


// Функция получения данных графика по умолчанию
procedure GetPoint(AGraphData: PGraphData; X,Y: PDouble; ACount: LongInt);
begin
  if AGraphData = nil then
  begin
    logfunc.WarningMsg('Don"t define graphic data');
    exit;
  end
  else
  begin
    if AGraphData^.n_points <= 0 then
    begin
      logfunc.WarningMsg('Points is empty');
      exit;
    end;

    if (ACount < 0) or (AGraphData^.n_points <= ACount) then
    begin
      logfunc.WarningMsg('Invalid index in function <GetPoint>');
      exit;
    end;

    X^ := AGraphData^.points[ACount].x;
    Y^ := AGraphData^.points[ACount].y;
    logfunc.InfoMsgFmt('Get point (%f, %f)', [X^, Y^]);
  end;
end;

// Функция полной отрисовки графика
function Draw(AGraph: PGraph; AGraphData: PGraphData; AIsPrintMode: Boolean): Boolean;
begin
  AGraph^.graph_data := AGraphData;

  if AGraph^.graph_data = nil then
    AGraph^.graph_data := @LGraphData;

  if AGraph^.graph_data^.status = nil then
    AGraph^.graph_data^.status := @LGStatus;

  if AGraph^.graph_data^.color = nil then
  begin
    if AIsPrintMode then
      AGraph^.graph_data^.color := @LPColor
    else
      AGraph^.graph_data^.color := @LGColor;
  end;

  logfunc.InfoMsg('Default graph options:');
  logfunc.InfoMsgFmt(#9'X1: %d Y1: %d', [AGraph^.canvas_x1, AGraph^.canvas_y1]);
  logfunc.InfoMsgFmt(#9'X2: %d Y2: %d', [AGraph^.canvas_x2, AGraph^.canvas_y2]);
  logfunc.InfoMsgFmt(#9'dX: %f dY: %f', [AGraph^.dX, AGraph^.dY]);

  // Переносим настройки на данные графика
  AGraph^.graph_data^.canvas_x1 := AGraph^.canvas_x1;
  AGraph^.graph_data^.canvas_y1 := AGraph^.canvas_y1;
  AGraph^.graph_data^.canvas_x2 := AGraph^.canvas_x2;
  AGraph^.graph_data^.canvas_y2 := AGraph^.canvas_y2;

  CheckGraph(AGraph);
  DrawGrid(AGraph);
  DrawAxis(AGraph);
  DrawGraph(AGraph);

  Result := True;
end;


// Функция проверки всех данных графика
procedure CheckGraph(AGraph: PGraph);
begin
  if AGraph^.graph_data^.x1 = AGraph^.graph_data^.x2 then
    AGraph^.graph_data^.x2 := AGraph^.graph_data^.x1 + 0.001;
  if AGraph^.graph_data^.y1 = AGraph^.graph_data^.y2 then
    AGraph^.graph_data^.y2 := AGraph^.graph_data^.y1 + 0.001;
  if AGraph^.graph_data^.x1 > AGraph^.graph_data^.x2 then
    SwapDouble(@AGraph^.graph_data^.x1, @AGraph^.graph_data^.x2);
  if AGraph^.graph_data^.y1 > AGraph^.graph_data^.y2 then
    SwapDouble(@AGraph^.graph_data^.y1, @AGraph^.graph_data^.y2);

  logfunc.InfoMsg('Check coord graph options:');
  logfunc.InfoMsgFmt(#9'X1: %d Y1: %d', [AGraph^.graph_data^.canvas_x1, AGraph^.graph_data^.canvas_y1]);
  logfunc.InfoMsgFmt(#9'X2: %d Y2: %d', [AGraph^.graph_data^.canvas_x2, AGraph^.graph_data^.canvas_y2]);

  CheckCoords(@AGraph^.graph_data^.canvas_x1, @AGraph^.graph_data^.canvas_x2, MINX, MAXX, 200);
  CheckCoords(@AGraph^.graph_data^.canvas_y1, @AGraph^.graph_data^.canvas_y2, MINY, MAXY, 200);

  if AGraph^.graph_data^.status^.number_y then
    AGraph^.canvas_x1 := AGraph^.graph_data^.canvas_x1 + 66
  else
    AGraph^.canvas_x1 := AGraph^.graph_data^.canvas_x1;

  AGraph^.canvas_y1 := AGraph^.graph_data^.canvas_y1;
  AGraph^.canvas_x2 := AGraph^.graph_data^.canvas_x2;

  if AGraph^.graph_data^.status^.number_x then
    AGraph^.canvas_y2 := AGraph^.graph_data^.canvas_y2 - 66
  else
    AGraph^.canvas_y2 := AGraph^.graph_data^.canvas_y2;

  AGraph^.dX := Double(AGraph^.canvas_x2 - AGraph^.canvas_x1) / (AGraph^.graph_data^.x2 - AGraph^.graph_data^.x1);
  AGraph^.dY := Double(AGraph^.canvas_y2 - AGraph^.canvas_y1) / (AGraph^.graph_data^.y2 - AGraph^.graph_data^.y1);

  logfunc.InfoMsg('Checked graph options:');
  logfunc.InfoMsgFmt(#9'X1: %d Y1: %d', [AGraph^.canvas_x1, AGraph^.canvas_y1]);
  logfunc.InfoMsgFmt(#9'X2: %d Y2: %d', [AGraph^.canvas_x2, AGraph^.canvas_y2]);
  logfunc.InfoMsgFmt(#9'dX: %f dY: %f', [AGraph^.dX, AGraph^.dY]);
  logfunc.InfoMsg('Graph data options:');
  logfunc.InfoMsgFmt(#9'x1: %f y1: %f', [AGraph^.graph_data^.x1, AGraph^.graph_data^.y1]);
  logfunc.InfoMsgFmt(#9'x2: %f y2: %f', [AGraph^.graph_data^.x2, AGraph^.graph_data^.y2]);
  logfunc.InfoMsgFmt(#9'X1: %d Y1: %d', [AGraph^.graph_data^.X1, AGraph^.graph_data^.Y1]);
  logfunc.InfoMsgFmt(#9'X2: %d Y2: %d', [AGraph^.graph_data^.X2, AGraph^.graph_data^.Y2]);

  if AGraph^.graph_data^.GetPoint = nil then
  begin
    AGraph^.graph_data^.GetPoint := @GetPoint;
    logfunc.InfoMsg('Set default <GetPoint> function');
  end;
end;


// Функция проверки координат графика
procedure CheckCoords(X1, X2: PInteger; AMinX, AMaxX, dX: Integer);
var
  _dx: Integer = 0;
  dx_max: Integer = 0;
begin
  if dX > 0 then
  begin
    if X2^ < X1^ then
      SwapInteger(X1, X2);
    _dx := X2^ - X1^;
    dx_max := AMaxX - AMinX;
    if dX > dx_max then
      dX := dx_max;
    if X1^ < AMinX then
      X1^ := AMinX;
    if X2^ > AMaxX then
      X2^ := AMaxX;
    if _dx < dX then
    begin
      X2^ := X1^ + dX;
      if X2^ > AMaxX then
      begin
        X2^ := AMaxX;
        X1^ := X2^ - dX;
      end;
    end;
  end;
end;


// Процедура меняет местами значения двух переменных
procedure SwapInteger(ASrc, ADst: PInteger);
var
  // Создаем временную переменную типа целое
  temp: Integer;
begin
  // Присваиваем temp значение первой переменной
  temp := ASrc^;
  // Присваиваем первой переменной значение второй
  ASrc^ := ADst^;
  // Присваиваем второй переменной значение temp
  ADst^ := temp;
end;


procedure SwapDouble(ASrc, ADst: PDouble);
var
  // Создаем временную переменную
  temp: Double;
begin
  // Присваиваем temp значение первой переменной
  temp := ASrc^;
  // Присваиваем первой переменной значение второй
  ASrc^ := ADst^;
  // Присваиваем второй переменной значение temp
  ADst^ := temp;
end;


// Вспомогательная функция свопирования двух элементов памяти
//procedure Swap(ASrc, ADst: Pointer; ASize: Integer)
//var
//  i: Integer = 0;
//  j: Byte = 0;
//  pS: PByte = PByte(ASrc);
//  pD: PByte = PByte(ADst);
//begin
//  for i := 0 to ASize do
//  begin
//    j := pS[i];
//    pS[i] := pD[i];
//    pD[i] := j;
//  end
//end;


// Отрисовка области под надписи
procedure DrawLabelArea(AGraph: PGraph);
begin
  // Отрисовка области под надписи
  SetCgaColor(AGraph, AGraph^.graph_data^.color^.ground);
  // Косяк с заливкой------------------------------------------------v
  // cairo_rectangle(AGraph^.cr, AGraph^.graph_data^.X1, AGraph^.Y2, AGraph^.graph_data^.X2 + 1, AGraph^.graph_data^.Y2);
  AGraph^.canvas^.Rectangle(AGraph^.graph_data^.canvas_x1, AGraph^.canvas_y2,
                            AGraph^.graph_data^.canvas_x2 + 1, AGraph^.graph_data^.canvas_y2);
  // Операция cairo_fill() используется вместо контура как шаблон закрашивания.
  // cairo_fill(AGraph^.cr);

  SetCgaColor(AGraph, AGraph^.graph_data^.color^.ground);
  //cairo_rectangle(AGraph^.cr, AGraph^.graph_data^.X1, AGraph^.graph_data^.Y1, AGraph^.X1, AGraph^.Y2);
  AGraph^.canvas^.Rectangle(AGraph^.graph_data^.canvas_x1, AGraph^.graph_data^.canvas_y1,
                            AGraph^.canvas_x1, AGraph^.canvas_y2);
  // Операция cairo_fill() используется вместо контура как шаблон закрашивания.
  //cairo_fill(AGraph^.cr);
end;


// Область поля графика
procedure DrawGraphArea(AGraph: PGraph);
begin
  // Область поля графика
  SetCgaColor(AGraph, AGraph^.graph_data^.color^.ground);
  //cairo_rectangle(AGraph^.cr, AGraph^.X1, AGraph^.graph_data^.Y1, AGraph^.graph_data^.X2, AGraph^.Y2);
  AGraph^.canvas^.Rectangle(AGraph^.canvas_x1, AGraph^.graph_data^.canvas_y1,
                            AGraph^.graph_data^.canvas_x2, AGraph^.canvas_y2);
  // Операция cairo_fill() используется вместо контура как шаблон закрашивания.
  //cairo_fill(AGraph^.cr);
end;


// Бордер
procedure DrawBorder(AGraph: PGraph);
begin
  // Бордер
  SetCgaColor(AGraph, AGraph^.graph_data^.color^.border);

  //cairo_rectangle(AGraph^.cr, AGraph^.X1, AGraph^.Y1, AGraph^.X2, AGraph^.Y2);
  AGraph^.canvas^.Rectangle(AGraph^.canvas_x1, AGraph^.canvas_y1,
                            AGraph^.canvas_x2, AGraph^.canvas_y2);
  // Дополнительная вертикальная линия (какой то косяк с cairo_rectangle)
  //cairo_move_to(AGraph^.cr, AGraph^.X2, AGraph^.Y1);
  AGraph^.canvas^.MoveTo(AGraph^.canvas_x2, AGraph^.canvas_y1);
  //cairo_line_to(AGraph^.cr, AGraph^.X2, AGraph^.Y2);
  AGraph^.canvas^.LineTo(AGraph^.canvas_x2, AGraph^.canvas_y2);
  // Операция cairo_stroke() применяет виртуальный карандаш вдоль контура.
  // cairo_stroke(AGraph^.cr);
end;


// Отрисовка сетки
procedure DrawGrid(AGraph: PGraph);
var
  stx: Double = 0.0;
  _stx: Double = 0.0;
  tmpx: Double = 0.0;
  _tmpx: Double = 0.0;
  _tmp0x: Double = 0.0;
  sty: Double = 0.0;
  _sty: Double = 0.0;
  tmpy: Double = 0.0;
  _tmpy: Double = 0.0;
  _tmp0y: Double = 0.0;
begin
  stx := StepX(AGraph);
  sty := StepY(AGraph);
  if AGraph^.graph_data^.status^.origin then
  begin
    tmpx := math.Ceil(AGraph^.graph_data^.x1 / stx) * stx;
    if (tmpx - AGraph^.graph_data^.x1) < (8 / AGraph^.dX) then
      tmpx := tmpx + stx;
    tmpy := math.Ceil(AGraph^.graph_data^.y1 / sty) * sty;
    if (tmpy - AGraph^.graph_data^.y1) < (8 / AGraph^.dY) then
      tmpy := tmpy + sty;
  end
  else
  begin
    tmpx := AGraph^.graph_data^.x1 + stx;
    tmpy := AGraph^.graph_data^.y1 + sty;
  end;

  _stx := stx * AGraph^.dX;
  _tmp0x := AGraph^.canvas_x1 + (tmpx - AGraph^.graph_data^.x1) * AGraph^.dX;
  _sty := sty * AGraph^.dY;
  _tmp0y := AGraph^.canvas_y2 - (tmpy - AGraph^.graph_data^.y1) * AGraph^.dY;

  // Отрисовка области под надписи
  DrawLabelArea(AGraph);

  // Область поля графика
  DrawGraphArea(AGraph);

  // Бордер
  DrawBorder(AGraph);

  // Сетка
  SetCgaColor(AGraph, AGraph^.graph_data^.color^.grid);
  SetDashLineStyle(AGraph);
  if AGraph^.graph_data^.status^.grid_x then
  begin
    _tmpx := _tmp0x;
    while AGraph^.canvas_x2 - _tmpx > 8 do
    begin
      // cairo_move_to(AGraph^.cr, _tmpx, AGraph^.Y1);
      AGraph^.canvas^.MoveTo(Round(_tmpx), AGraph^.canvas_y1);
      // cairo_line_to(AGraph^.cr, _tmpx, AGraph^.Y2);
      AGraph^.canvas^.LineTo(Round(_tmpx), AGraph^.canvas_y2);
      _tmpx := _tmpx + _stx;
    end;
  end;


  if AGraph^.graph_data^.status^.grid_y then
  begin
    _tmpy := _tmp0y;
    while _tmpy - AGraph^.canvas_y1 > 8 do
    begin
      // cairo_move_to(AGraph^.cr, AGraph^.X1, _tmpy);
      AGraph^.canvas^.MoveTo(AGraph^.canvas_x1, Round(_tmpy));
      // cairo_line_to(AGraph^.cr, AGraph^.X2, _tmpy);
      AGraph^.canvas^.LineTo(AGraph^.canvas_x2, Round(_tmpy));
      _tmpy := _tmpy - _sty;
    end;
  end;

  // Операция cairo_stroke() применяет виртуальный карандаш вдоль контура.
  // cairo_stroke(AGraph^.cr);

  // Шкала X
  SetCgaColor(AGraph, AGraph^.graph_data^.color^.text);
  if AGraph^.graph_data^.status^.number_x then
  begin
    _tmpx := _tmp0x;
    while AGraph^.canvas_x2 - _tmpx > 8 do
    begin
      OutGridNumber(AGraph, Round(_tmpx), AGraph^.canvas_y2, tmpx, VERT_DIRECTION);
      tmpx := tmpx + stx;
      _tmpx := _tmpx + _stx;
    end;
  end;

  // Шкала Y
  SetCgaColor(AGraph, AGraph^.graph_data^.color^.text);
  if AGraph^.graph_data^.status^.number_y then
  begin
    _tmpy := _tmp0y;
    while _tmpy - AGraph^.canvas_y1 > 8 do
    begin
      OutGridNumber(AGraph, AGraph^.canvas_x1, Round(_tmpy), tmpy, HORIZ_DIRECTION);
      tmpy := tmpy + sty;
      _tmpy := _tmpy + _sty;
    end;
  end;

end;


function Step(AGraph: PGraph; ASt: Double; AType: Byte): Double;
var
  fl: Double = 0.0;
  i: Integer = 0;
begin
  if AGraph^.graph_data^.status^.origin then
  begin
    i := 0;
    while ASt < 1.0 do 
    begin
      ASt := ASt * 10;
      Dec(i);
    end;
    while ASt > 10.0 do
    begin
      ASt := ASt / 10;
      Inc(i);
    end;

    fl := math.Floor(ASt);
    if (ASt - fl) > 0.5 then
      ASt := math.Ceil(ASt)
    else
      ASt := fl;
    ASt := ASt * math.Power(10.0, i);
    if (AType = 1) and (ASt < 1) then
      ASt := 1;
  end;
  Result := ASt;
end;


function StepX(AGraph: PGraph): Double;
begin
  Result := Step(AGraph, (AGraph^.graph_data^.x2 - AGraph^.graph_data^.x1) / ((AGraph^.canvas_x2 - AGraph^.canvas_x1) >> 5), AGraph^.graph_data^.status^.x_type);
end;


function StepY(AGraph: PGraph): Double;
begin
  Result := Step(AGraph, (AGraph^.graph_data^.y2 - AGraph^.graph_data^.y1) / ((AGraph^.canvas_y2 - AGraph^.canvas_y1) >> 5), AGraph^.graph_data^.status^.y_type);
end;


// Отрисовка надписей сетки
procedure OutGridNumber(AGraph: PGraph; x, y: Integer; ANumber: Double; AOrient: Boolean);
var
  mod_number: Double = 0.0;
  tmp: LongInt = 0;
  mode: LongInt = 0;
  i: Integer = 0;
  hh: Integer = 0;
  mm: Integer = 0;
  ss: Integer = 0;
  dd: Integer = 0;
  x1: Integer = 0;
  y1: Integer = 0;
  x2: Integer = 0;
  y2: Integer = 0;
  buffer: String;
begin
  mod_number := Abs(ANumber);

  if mod_number < 1E-14 then
    ANumber := 0;

  if AOrient then
    mode := AGraph^.graph_data^.status^.x_type
  else
    mode := AGraph^.graph_data^.status^.y_type;

  case mode of
    1:      // GM_TIME
    begin
      tmp := Round(ANumber);
      ss := tmp mod 60;
      mm := (tmp div 60) mod 60;
      hh := (tmp div 3600) mod 24;
      dd := tmp div 86400;
      if (ANumber < 0) or (ANumber >= 864000000) then
        buffer := '##:##:##'
      else
        buffer := Format('%02d:%02d:%02d', [hh, mm, ss]);
      if dd > 0 then
      begin
        OutTextXY(AGraph, x1, y1, buffer, AOrient);
        buffer := Format('(%d)', [dd]);
        OutTextXY(AGraph, x2, y2, buffer, AOrient);
       end
       else
         OutTextXY(AGraph, x, y, buffer, AOrient);
       // break;
    end;
    2:    // GM_OPTIMAL
    begin
      if (mod_number < 1000000.0) and (mod_number > 0.00001) or (ANumber = 0) then
      begin
        if ANumber <> 0 then
          buffer := Format('% lf', [ANumber])
        else
          buffer := '0';

        // buffer[8] := 0;
        //
        //i := 7;
        //while (i > 0) and (buffer[i] = '0') do
        //begin
        //   buffer[i] := 0;
        //  Dec(i);
        //end;
        //
        //if buffer[i] = '.' then
        //  buffer[i] := 0;
        OutTextXY(AGraph, x, y, buffer, AOrient);
        // break;
       end;
    end;
    0:     // GM_EXP
    begin
      buffer := Format('%12.5E', [ANumber]);
      OutTextXY(AGraph, x2, y2, &buffer[8], AOrient);

      //buffer[8] := 0;
      OutTextXY(AGraph, x1, y1, buffer, AOrient);
      //break;
    end;
  end;
end;


// Отрисовка осей
procedure DrawAxis(AGraph: PGraph);
var
  i: Integer = 0;
begin
  SetCgaColor(AGraph, AGraph^.graph_data^.color^.axis);
  SetSolidLineStyle(AGraph);

  if (AGraph^.graph_data^.y1 <= 0) and (AGraph^.graph_data^.y2 >= 0) and (AGraph^.graph_data^.status^.axis_x) then
  begin
    i := AGraph^.canvas_y2 + Round(AGraph^.graph_data^.y1 * AGraph^.dY);
    //cairo_move_to(AGraph^.cr, AGraph^.X1, i);
    AGraph^.canvas^.MoveTo(AGraph^.canvas_x1, i);
    //cairo_line_to(AGraph^.cr, AGraph^.X2, i);
    AGraph^.canvas^.LineTo(AGraph^.canvas_x2, i);
  end;

  if (AGraph^.graph_data^.x1 <= 0) and (AGraph^.graph_data^.x2 >= 0) and (AGraph^.graph_data^.status^.axis_y) then
  begin
    i := AGraph^.canvas_x1 - Round(AGraph^.graph_data^.x1 * AGraph^.dX);
    //cairo_move_to(AGraph^.cr, i, AGraph^.Y1);
    AGraph^.canvas^.MoveTo(i, AGraph^.canvas_y1);
    //cairo_line_to(AGraph^.cr, i, AGraph^.Y2);
    AGraph^.canvas^.LineTo(i, AGraph^.canvas_y2);
  end;

  // ВНИМАНИЕ! Для отображения ранее отрисованных
  //           линий необходимо вызвать cairo_stroke().
  //cairo_stroke(AGraph^.cr);
end;


// Отрисовка самого графика
function DrawGraph(AGraph: PGraph): Boolean;
var
  _y1: Double = 0.0;
  _x1: Double = 0.0;
  _y2: Double = 0.0;
  _x2: Double = 0.0;
  y1: Double = 0.0;
  x1: Double = 0.0;
  y2: Double = 0.0;
  x2: Double = 0.0;
  y: Double = 0.0;
  x: Double = 0.0;
  ix1: LongInt = 0;
  iy1: LongInt = 0;
  ix2: LongInt = 0;
  iy2: LongInt = 0;
  i: LongInt = 0;
  j: LongInt = 0;
  k: LongInt = 0;
  // unsigned Pattern, Pc, P;
begin
  if AGraph^.graph_data = nil then
  begin
    logfunc.WarningMsg('Don"t define graphic data');
    Result := False;
    exit;
  end;

  if AGraph^.graph_data^.n_points = 0 then
  begin
    logfunc.WarningMsg('Empty points');
    Result := False;
    exit;
  end;

  if AGraph^.graph_data^.n_points < 2 then
  begin
    logfunc.WarningMsgFmt('Point count < 2 %d', [AGraph^.graph_data^.n_points]);
    Result := False;
    exit;
  end;

  if AGraph^.graph_data^.GetPoint = nil then
  begin
    logfunc.WarningMsg('Not define <GetPoint> function');
    Result := False;
    exit;
  end;

  // Берем первую точку
  i := 0;
  AGraph^.graph_data^.GetPoint(AGraph^.graph_data, @x1, @y1, i);
  if AGraph^.graph_data^.status^.dtype then
  begin
      j := AGraph^.graph_data^.n_points - 1;
      AGraph^.graph_data^.GetPoint(AGraph^.graph_data, @x2, @y2, j);
      if (x2 > AGraph^.graph_data^.x1) and (x1 < AGraph^.graph_data^.x2) then
      begin
        if x1 < AGraph^.graph_data^.x1 then
          while (j - i) > 1 do
          begin
            k := (i + j) >> 1;
            AGraph^.graph_data^.GetPoint(AGraph^.graph_data, @x, @y, k);
            if (x > AGraph^.graph_data^.x1) then
            begin
              x2 := x;
              y2 := y;
              j := k;
            end
            else if (x < AGraph^.graph_data^.x1) then
            begin
              x1 := x;
              y1 := y;
              i := k;
             end
             else
               break;
          end;
      end
      else
          i := AGraph^.graph_data^.n_points;
  end;

  SetCgaColor(AGraph, AGraph^.graph_data^.color^.line);
  SetSolidLineStyle(AGraph);

  _x2 := x1;
  _y2 := y1;

  Inc(i);
  while (i < AGraph^.graph_data^.n_points) and ((not AGraph^.graph_data^.status^.dtype) or (x1 < AGraph^.graph_data^.x2)) do 
  begin
    AGraph^.graph_data^.GetPoint(AGraph^.graph_data, @x2, @y2, i);
    if not ((y1 <= AGraph^.graph_data^.y1) and (y2 <= AGraph^.graph_data^.y1)) or
           ((y1 >= AGraph^.graph_data^.y2) and (y2 >= AGraph^.graph_data^.y2)) or
           ((x1 <= AGraph^.graph_data^.x1) and (x2 <= AGraph^.graph_data^.x1)) or
           ((x1 >= AGraph^.graph_data^.x2) and (x2 >= AGraph^.graph_data^.x2)) then
    begin
          _x1 := _x2;
          _y1 := _y2;
          _x2 := x2;
          _y2 := y2;

          SplitGraph(AGraph, @_x1, @_y1, @_x2, @_y2);

          ix1 := Round((_x1 - AGraph^.graph_data^.x1) * AGraph^.dX) + AGraph^.canvas_x1;
          iy1 := AGraph^.canvas_y2 - Round((_y1 - AGraph^.graph_data^.y1) * AGraph^.dY);
          ix2 := Round((_x2 - AGraph^.graph_data^.x1) * AGraph^.dX) + AGraph^.canvas_x1;
          iy2 := AGraph^.canvas_y2 - Round((_y2 - AGraph^.graph_data^.y1) * AGraph^.dY);

          //cairo_move_to(AGraph^.cr, ix1, iy1);
          AGraph^.canvas^.MoveTo(ix1, iy1);
          //cairo_line_to(AGraph^.cr, ix2, iy2);
          AGraph^.canvas^.LineTo(ix2, iy2);
          logfunc.InfoMsgFmt('Draw line (%d, %d) - (%d, %d)', [ix1, iy1, ix2, iy2]);
    end;

    // memcpy(&x1, &x2, 16);
    x2 := x1;
    Inc(i);
  end;
  // ВНИМАНИЕ! Для отображения ранее отрисованных
  //           линий необходимо вызвать cairo_stroke().
  // cairo_stroke(AGraph^.cr);

  Result := True;
end;


// Обрезка линии
function SplitGraph(AGraph: PGraph; x1, y1, x2, y2: PDouble): Boolean;
begin
 CrossPoint(y1, x1, y2, x2, AGraph^.graph_data^.y1, 0);
 CrossPoint(y1, x1, y2, x2, AGraph^.graph_data^.y2, 1);
 CrossPoint(x1, y1, x2, y2, AGraph^.graph_data^.x1, 0);
 CrossPoint(x1, y1, x2, y2, AGraph^.graph_data^.x2, 1);
 Result := True;
end;


function CrossPoint(x1, y1, x2, y2: PDouble; x: Double; AMode: Integer): Boolean;
var
  y: Double = 0;
  dx1: Double = 0;
  dx2: Double = 0;
  dx: Double = 0;
  dy: Double = 0;
begin
  if x1^ > x2^ then
  begin
    SwapDouble(x1, x2);
    SwapDouble(y1, y2);
  end;

  if not ((x2^ > x) and (x1^ < x)) then
  begin
    Result := False;
    exit;
  end;
  dx1 := x - x1^;
  dx2 := x2^ - x;
  dx := x2^ - x1^;
  dy := y2^ - y1^;
  if dx2 > dx1 then
      y := dy / dx * dx1 + y1^
  else
      y := y2^ - dy / dx * dx2;

  if AMode > 0 then
  begin
    x2^ := x;
    y2^ := y;
  end
  else
  begin
    x1^ := x;
    y1^ := y;
  end;
  Result := True;
end;


// Инициализация структуры графика
function InitGraph(AGraph: PGraph; AGraphData: PGraphData; ACanvas: PCairoPngCanvas;
                   AWidth, AHeight: Integer;
                   DX, DY: Double): PGraph;
begin
  if AGraph = nil then
  begin
    logfunc.WarningMsg('Not define graph');
    Result := nil;
    exit;
  end;

  AGraph^.canvas_x1 := 0;
  AGraph^.canvas_y1 := 0;
  AGraph^.canvas_x2 := AWidth - 1;
  AGraph^.canvas_y2 := AHeight - 1;

  AGraph^.dX := DX;
  AGraph^.dY := DY;

  AGraph^.graph_data := AGraphData;

  AGraph^.canvas := ACanvas;

  Result := AGraph;
end;


// Инициализация структуры данных графика
function InitGraphData(AGraphData: PGraphData; x1, y1, x2, y2: Double): PGraphData;
begin
  if AGraphData = nil then
  begin
    logfunc.WarningMsg('Not define graph data');
    Result := nil;
    exit;
  end;

  AGraphData^.x1 := x1;
  AGraphData^.y1 := y1;
  AGraphData^.x2 := x2;
  AGraphData^.y2 := y2;

  AGraphData^.status := nil;
  AGraphData^.color := nil;

  AGraphData^.n_points := 0;

  //x1, y1, x2, y2 0, 0, 320, 120,
  AGraphData^.canvas_x1 := 0;
  AGraphData^.canvas_y1 := 0;
  AGraphData^.canvas_x2 := 639;
  AGraphData^.canvas_y2 := 479;

  logfunc.InfoMsg('Init graph data');

  Result := AGraphData;
end;


// Выполнить отрисовку графика в PNG файл
//  png_filename - Полное имя PNG файла
function DrawPNG(APNGFileName: AnsiString; AGraphData: PGraphData; AXType, AYType: Byte;
                 AWidth, AHeight: Integer;
                 ASceneX1, ASceneY1, ASceneX2, ASceneY2, dX, dY: Double): Boolean;
var
  cairo_canvas: TCairoPngCanvas;
  graphic: TGraph;
begin
  if strfunc.IsEmptyStr(APNGFileName) then
    APNGFileName := DEFAULT_OUTPUT_PNG_FILENAME;

  if AWidth = 0 then
    AWidth := graphfunc.DEFAULT_WIDTH;

  if AHeight = 0 then
    AHeight := graphfunc.DEFAULT_HEIGHT;

  logfunc.InfoMsgFmt('Draw PNG file: %s [%d x %d]', [APNGFileName, AWidth, AHeight]);

  //surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, width, height);
  //cairo_surface_set_device_offset(surface, 0.0, 1.0);
  //cr = cairo_create(surface);
  cairo_canvas := TCairoPNGCanvas.Create;

  try
    // OutputFileName must set before BeginDoc; Otherwise use stream property
    cairo_canvas.OutputFileName := APNGFileName;
    cairo_canvas.BeginDoc;

    // Значения по умолчанию
    if dX <= 0 then
      dX := 20;
    if dY <= 0 then
      dY := 2;

    graphfunc.InitGraph(@graphic, AGraphData, @cairo_canvas, AWidth, AHeight, dX, dY);

    if AGraphData <> nil then
    begin
      // Размер картинки
      AGraphData^.canvas_x2 := AWidth - 1;
      AGraphData^.canvas_y2 := AHeight - 1;
      // Параметры сцены графика
      if (ASceneX1 <> ASceneX2) and (ASceneY1 <> ASceneY2) then
      begin
        AGraphData^.x1 := ASceneX1;
        AGraphData^.y1 := ASceneY1;
        AGraphData^.x2 := ASceneX2;
        AGraphData^.y2 := ASceneY2;
        logfunc.InfoMsgFmt('Set graph data scene (%f, %f) - (%f, %f)', [ASceneX1, ASceneY1, ASceneX2, ASceneY2]);
      end
      else
        logfunc.InfoMsgFmt('Graph data scene (%f, %f) - (%f, %f)', [ASceneX1, ASceneY1, ASceneX2, ASceneY2]);
    end;

    graphfunc.Draw(@graphic, AGraphData, False);

    // cairo_fill(cr);

    // cairo_surface_write_to_png(surface, png_filename);

    // cairo_surface_destroy(surface);
    // graphic.canvas := nil;
    // cairo_destroy(cr);
    // graphic.cr = NULL;

    cairo_canvas.EndDoc;
    Result := True;
  finally
    graphic.canvas := nil;
    cairo_canvas.Free;
  end;
end;

end.
