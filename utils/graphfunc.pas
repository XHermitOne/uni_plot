{
Модуль функций отрисовки графиков

Версия: 0.0.0.1
}
unit graphfunc;

{$mode objfpc}{$H+}

interface

uses
  exttypes, cairocanvas;

const
  // Цвета CGA
  BLACK_COLOR: Byte = 0;
  BLUE_COLOR: Byte  = 1;
  GREEN_COLOR: Byte = 2;
  CYAN_COLOR: Byte  = 3;
  RED_COLOR: Byte = 4;
  MAGENTA_COLOR: Byte = 5;
  BROWN_COLOR: Byte = 6;
  LIGHTGRAY_COLOR: Byte = 7;
  DARKGRAY_COLOR: Byte = 8;
  LIGHTBLUE_COLOR: Byte = 9;
  LIGHTGREEN_COLOR: Byte = 10;
  LIGTHCYAN_COLOR: Byte = 11;
  LIGHTRED_COLOR: Byte = 12;
  LIGHTMAGENTA_COLOR: Byte = 13;
  YELLOW_COLOR: Byte = 14;
  WHITE_COLOR: Byte = 15;

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
  TGraphData = record
    status: PGraphStatus;
    color: PGraphColor;

    // Функция получения координат точки по ее индексу
    procedure GetPoint(AGraphData: PGraphData; X: Double; Y: Double; AIndex: LongInt);

    n_points: LongInt; // Количество точек

    points: PGraphPoint;  // Точки графика
    
    x1, y1, x2, y2: Double;  // Диапазон данных графика (Сцена)
    canvas_x1, canvas_y1, canvas_x2, canvas_y2: Integer;  // Графическая граница области графика
  end;
  PGraphData = ^TGraphData;

 
  PCairoPngCanvas = ^TCairoPngCanvas;
  // График
  TGraph = record
    dX: Double;
    dY: Double;
    X1, Y1, X2, Y2: Integer;  // Размеры самого графика
    graph_data: PDaraphData;

    canvas: PCairoPngCanvas;
  end;

  // Функции обработки графика
  procedure GetPoint(AGraphData: PGraphData; X: PDouble; Y: PDouble; ACount: LongInt);
  function Draw(AGraph: PGraph; AGraphData: PGraphData; AIsPrintMode: Boolean): Boolean;
  procedure CheckGraph(AGraph: PGraph);
  procedure DrawGrid(AGraph: PGraph);
  procedure DrawAxis(AGraph: PGraph);
  function DrawGraph(AGraph: PGraph): Integer;
  function StepX(AGraph: PGraph): Double;
  function StepY(AGraph: PGraph): Double;
  function Step(AGraph: PGraph; ASt: Double; AType: Integer): Double;
  function MostLeftPoint(AGraph: PGraph; X: PDouble; Y: PDouble): LongInt;
  procedure OutGridNumber(AGraph: PGraph; X: Integer; Y: Integer; ANumber: Double; AOrient: Integer);
  function SplitGraph(AGraph: PGraph; x1, y1, x2, y2: PDouble): Integer;

  // Функции общего назначения
  procedure CheckCoords(X1, X2: PInteger; AMinX, AMaxX, dX: Integer);
  procedure Swap(ASrc, ADst: Pointer; ASize: Integer);
  function CrossPoint(x1, y1, x2, y2: PDouble; x: Double; AMode: Integer): Integer;

  // Инициализация структуры графика
  function InitGraph(AGraph: PGraph; AGraphData: PGraphData; ACAnvas: TCairoPngCanvas;
                     AWidth, AHeight: Integer;
                     DX, DY: Double): PGraph;

  // Инициализация структуры данных графика
  function InitGraphData(AGraphData: PGraphData; x1, y1, x2, y2: Double): TGraphData;

var
  // Цвета режима графического вывода
  LGColor: TGraphColor = (text: CYAN_COLOR;
		         ground: BLACK_COLOR;
		         border: DARKGRAY_COLOR;
		         grid: DARKGRAY_COLOR;
			 axis: DARKGRAY_COLOR;
		         line: YELLOW_COLOR);

implementation

uses
  logfunc;

var
  // Цвета режима печати
  LPColor: TGraphColor = (text: BLACK_COLOR;
		         ground: WHATE_COLOR;
		         border: BLACK_COLOR;
		         grid: BLACK_COLOR;
			 axis: BLACK_COLOR;
		         line: BLACK_COLOR);

  LGStatus: TGraphStatus = (axis_x: AXIS_ON;
			    axis_y: AXIS_ON;

			    grid_x: GRID_ON;
			    grid_y: GRID_ON;
	
			    number_x: NUMBER_ON;
			    number_y: NUMBER_ON;

			    clear: CLEAR; 
	
			    x_type: GM_TIME;
			    y_type: GM_OPTIMAL;

			    origin: GRID_ORIGIN; 
			    dtype: NOSEQ; 

			    line: GM_OPTIMAL);

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


// Установить текущий цвет отрисовки
procedure SetCgaColor(AGraph: PGraph; AColor: Byte)
begin
  logfunc.InfoMsgFmt('Set CGA color: %d', [AColor]);

  case AColor of
    BLACK_COLOR: cairo_set_source_rgb(graph->cr, 0.0, 0.0, 0.0);
    BLUE_COLOR: cairo_set_source_rgb(graph->cr, 0.0, 0.0, 0.5);
    GREEN_COLOR: cairo_set_source_rgb(graph->cr, 0.0, 0.5, 0.0);
    CYAN_COLOR: cairo_set_source_rgb(graph->cr, 0.0, 0.5, 0.5);
    RED_COLOR: cairo_set_source_rgb(graph->cr, 0.5, 0.0, 0.0);
    MAGENTA_COLOR: cairo_set_source_rgb(graph->cr, 0.5, 0.0, 0.5);
    BROWN_COLOR: cairo_set_source_rgb(graph->cr, 1.0, 0.5, 0.5);
    LIGHTGRAY_COLOR: cairo_set_source_rgb(graph->cr, 0.8, 0.8, 0.8);
    DARKGRAY_COLOR: cairo_set_source_rgb(graph->cr, 0.5, 0.5, 0.5);
    LIGHTBLUE_COLOR: cairo_set_source_rgb(graph->cr, 0.0, 0.0, 2.0);
    LIGHTGREEN_COLOR: cairo_set_source_rgb(graph->cr, 0.0, 1.0, 0.0);
    LIGTHCYAN_COLOR: cairo_set_source_rgb(graph->cr, 0.0, 1.0, 1.0);
    LIGHTRED_COLOR: cairo_set_source_rgb(graph->cr, 1.0, 0.0, 0.0);
    LIGHTMAGENTA_COLOR: cairo_set_source_rgb(graph->cr, 1.0, 1.0, 0.0);
    YELLOW_COLOR: cairo_set_source_rgb(graph->cr, 1.0, 1.0, 0.0);
    WHITE_COLOR: cairo_set_source_rgb(graph->cr, 1.0, 1.0, 1.0);
  else
    logfunc.WarningMsgFmt('Incorrect CGA color: %d', [AColor]);
  end;
end;

// Вывод текста
procedure OutTextXY(AGraph: PGraph; x, y: Integer; AText: AnsiString; AOrient: Boolean)
begin
  cairo_text_extents_t te;
  cairo_text_extents(graph->cr, text, &te);

  if AOrient = HORIZ_DIRECTION then
    cairo_move_to(graph->cr, x - te.width - te.x_bearing, y - te.height / 2 - te.y_bearing);
  else if AOrient = VERT_DIRECTION then
    cairo_move_to(graph->cr, x - te.height / 2 - te.y_bearing, y + te.width + te.x_bearing);

  if AOrient = VERT_DIRECTION then
  begin
    cairo_save(graph->cr);
    cairo_rotate(graph->cr, - M_PI / 2);
  end;

  cairo_show_text(graph->cr, text);

  if AOrient = VERT_DIRECTION then
    cairo_restore(graph->cr);
end;


// Установить точечный стиль отрисовки линий
procedure SetDotLineStyle(AGraph: PGraph)
var
  dash_ink: Double = 1.0;
  dash_skip: Double = 3.0;
  ndash: Integer;
  offset: Double = -50.0;
begin
  ndash  = sizeof(dashes) / sizeof(dashes[0]);

  cairo_set_dash(graph->cr, dashes, ndash, offset);
  cairo_set_line_width(graph->cr, 1.0);
end;


// Установить нормальный стиль отрисовки линий
procedure SetSolidLineStyle(AGraph: PGraph)
var
  dash_ink: Double = 1.0;
  dash_skip: Double = 3.0;
begin
  cairo_set_dash(graph->cr, dashes, 0, 0);
  cairo_set_line_width(graph->cr, 1.0);
end;


// Функция получения данных графика по умолчанию
procedure GetPoint(AGraphData: PGraphData; X,Y: PDouble, ACount: LongInt)
begin
  if AGraphData = nil then
  begin
    logfunc.WarningMsg('Don"t define graphic data');
    exit;
  end
  else
  begin
    if graph_data->n_points then
    begin
      logfunc.WarningMsg('Points is empty');
      exit;
    end;

    if (count < 0) or (graph_data->n_points <= count) then
    begin
      logfunc.WarningMsg('Invalid index in function <GetPoint>');
      exit;
    end;

    *X = graph_data->points[count].x;
    *Y = graph_data->points[count].y;
    logfunc.InfoMsgFmt('Get point (%f, %f)', [*X, *Y]);
  end;
end;

// Функция полной отрисовки графика
function Draw(AGraph: PGraph; AGraphData: PGraphData; AIsPrintMode: Boolean): Boolean
begin
  graph->graph_data = graph_data;

  if graph->graph_data = nil then
    graph->graph_data = &LGraph;

  if graph->graph_data->status = nil then
    graph->graph_data->status = &LGStatus;

  if graph->graph_data->color = nil then
  begin
    if is_print_mode then
      graph->graph_data->color = &LPColor;
    else
      graph->graph_data->color = &LGColor;
  end;

  logfunc.InfoMsg('Default graph options:');
  logfunc.InfoMsgFmt('\tX1: %d Y1: %d', [graph->X1, graph->Y1]);
  logfunc.InfoMsgFmt('\tX2: %d Y2: %d', [graph->X2, graph->Y2]);
  logfunc.InfoMsgFmt('\tdX: %f dY: %f', [graph->dX, graph->dY]);

  // Переносим настройки на данные графика
  graph->graph_data->X1 = graph->X1;
  graph->graph_data->Y1 = graph->Y1;
  graph->graph_data->X2 = graph->X2;
  graph->graph_data->Y2 = graph->Y2;

  CheckGraph(graph);
  DrawGrid(graph);
  DrawAxis(graph);
  DrawGraph(graph);

  Result :=True;
end;


// Функция проверки всех данных графика
procedure CheckGraph(AGraph: PGraph)
begin
  if graph->graph_data->x1 = graph->graph_data->x2 then
    graph->graph_data->x2 := graph->graph_data->x1 + 0.001;
  if graph->graph_data->y1 = graph->graph_data->y2 then
    graph->graph_data->y2 := graph->graph_data->y1 + 0.001;
  if graph->graph_data->x1 > graph->graph_data->x2 then
    Swap(&graph->graph_data->x1, &graph->graph_data->x2, sizeof(double));
  if graph->graph_data->y1 > graph->graph_data->y2 then
    Swap(&graph->graph_data->y1, &graph->graph_data->y2, sizeof(double));

  logfunc.InfoMsg('Check coord graph options:');
  logfunc.InfoMsgFmt('\tX1: %d Y1: %d', [graph->graph_data->X1, graph->graph_data->Y1]);
  logfunc.InfoMsgFmt('\tX2: %d Y2: %d', [graph->graph_data->X2, graph->graph_data->Y2]);

  CheckCoords(&graph->graph_data->X1, &graph->graph_data->X2, MINX, MAXX, 200);
  CheckCoords(&graph->graph_data->Y1, &graph->graph_data->Y2, MINY, MAXY, 200);

  if graph->graph_data->status->number_y then
    graph->X1 := graph->graph_data->X1 + 66;
  else
    graph->X1 := graph->graph_data->X1;

  graph->Y1 := graph->graph_data->Y1;

  graph->X2 := graph->graph_data->X2;

  if graph->graph_data->status->number_x then
    graph->Y2 := graph->graph_data->Y2 - 66;
  else
    graph->Y2 := graph->graph_data->Y2;

  graph->dX := ((double)(graph->X2 - graph->X1)) / (graph->graph_data->x2 - graph->graph_data->x1);
  graph->dY := ((double)(graph->Y2 - graph->Y1)) / (graph->graph_data->y2 - graph->graph_data->y1);

  logfunc.InfoMsg('Checked graph options:');
  logfunc.InfoMsgFmt('\tX1: %d Y1: %d', [graph->X1, graph->Y1]);
  logfunc.InfoMsgFmt('\tX2: %d Y2: %d', [graph->X2, graph->Y2]);
  logfunc.InfoMsgFmt('\tdX: %f dY: %f', [graph->dX, graph->dY]);
  logfunc.InfoMsg('Graph data options:');
  logfunc.InfoMsgFmt('\tx1: %f y1: %f', [graph->graph_data->x1, graph->graph_data->y1]);
  logfunc.InfoMsgFmt('\tx2: %f y2: %f', [graph->graph_data->x2, graph->graph_data->y2]);
  logfunc.InfoMsgFmt('\tX1: %d Y1: %d', [graph->graph_data->X1, graph->graph_data->Y1]);
  logfunc.InfoMsgFmt('\tX2: %d Y2: %d', [graph->graph_data->X2, graph->graph_data->Y2]);

  if graph->graph_data->get_point = nil then
  begin
    graph->graph_data->get_point := GetPoint;
    logfunc.InfoMsg('Set default <GetPoint> function');
  end;
end;


// Функция проверки координат графика
procedure CheckCoords(X1, X2: PInteger; AMinX, AMaxX, dX: Integer)
var
  dx: Integer = 0;
  dx_max: Integer = 0;
begin
  if dX > 0 then
  begin
    if X2^ < X1^ then
      Swap(X1, X2, sizeof(int));
    dx = X2^ - X1^;
    dx_max := max_x - min_x;
    if dX > dx_max then
      dX := dx_max;
    if X1^ < min_x then
      X1^ := min_x;
    if X2^ > max_x then
      X2^ := max_x;
    if dx < dX then
    begin
      X2^ := X1^ + dX;
      if X2^ > max_x then
      begin
        X2^ := max_x;
        X1^ := X2^ - dX;
      end;
    end;
  end;
end;


// Вспомогательная функция свопирования двух элементов памяти
procedure Swap(ASrc, ADst: Pointer; ASize: Integer)
var
  i: Integer = 0;
  j: Byte = 0;
  pS: PByte = PByte(ASrc);
  pD: PByte = PByte(ADst);
begin
  for i := 0 to ASize do
  begin
    j := pS[i];
    pS[i] := pD[i];
    pD[i] := j;
  end
end;


// Отрисовка области под надписи
procedure DrawLabelArea(AGraph: PGraph)
begin
  // Отрисовка области под надписи
  SetCgaColor(AGraph, graph->graph_data->color->ground);
  // Косяк с заливкой------------------------------------------------v
  cairo_rectangle(graph->cr, graph->graph_data->X1, graph->Y2, graph->graph_data->X2 + 1, graph->graph_data->Y2);
  // Операция cairo_fill() используется вместо контура как шаблон закрашивания.
  cairo_fill(graph->cr);

  SetCgaColor(AGraph, graph->graph_data->color->ground);
  cairo_rectangle(graph->cr, graph->graph_data->X1, graph->graph_data->Y1, graph->X1, graph->Y2);
  // Операция cairo_fill() используется вместо контура как шаблон закрашивания.
  cairo_fill(graph->cr);
end;


// Область поля графика
procedure DrawGraphArea(AGraph: PGraph)
begin
  // Область поля графика
  SetCgaColor(AGraph, graph->graph_data->color->ground);
  cairo_rectangle(graph->cr, graph->X1, graph->graph_data->Y1, graph->graph_data->X2, graph->Y2);
  // Операция cairo_fill() используется вместо контура как шаблон закрашивания.
  cairo_fill(graph->cr);
end;


// Бордер
procedure DrawBorder(AGraph: PGraph)
begin
  // Бордер
  SetCgaColor(AGraph, graph->graph_data->color->border);

  cairo_rectangle(graph->cr, graph->X1, graph->Y1, graph->X2, graph->Y2);
  // Дополнительная вертикальная линия (какой то косяк с cairo_rectangle)
  cairo_move_to(graph->cr, graph->X2, graph->Y1);
  cairo_line_to(graph->cr, graph->X2, graph->Y2);
  // Операция cairo_stroke() применяет виртуальный карандаш вдоль контура.
  cairo_stroke(graph->cr);
end;


// Отрисовка сетки
procedure DrawGrid(AGraph: PGraph)
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
  if graph->graph_data->status->origin then
  begin
    tmpx := Ceil(graph->graph_data->x1 / stx) * stx;
    if (tmpx - graph->graph_data->x1) < (8 / graph->dX) then
      tmpx := tmpx + stx;
    tmpy := Ceil(graph->graph_data->y1 / sty) * sty;
    if (tmpy - graph->graph_data->y1) < (8 / graph->dY) then
      tmpy := tmpy + sty;
  end
  else
  begin
    tmpx := graph->graph_data->x1 + stx;
    tmpy := graph->graph_data->y1 + sty;
  end;

  _stx := stx * graph->dX;
  _tmp0x := graph->X1 + (tmpx - graph->graph_data->x1) * graph->dX;
  _sty := sty * graph->dY;
  _tmp0y := graph->Y2 - (tmpy - graph->graph_data->y1) * graph->dY;

  // Отрисовка области под надписи
  DrawLabelArea(AGraph);

  // Область поля графика
  DrawGraphArea(AGraph);

  // Бордер
  DrawBorder(AGraph);

  // Сетка
  SetCgaColor(AGraph, graph->graph_data->color->grid);
  SetDotLineStyle(AGraph);
  if graph->graph_data->status->grid_x then
  begin
    _tmpx := _tmp0x;
    while graph->X2 - _tmpx > 8 do
    begin
      cairo_move_to(graph->cr, _tmpx, graph->Y1);
      cairo_line_to(graph->cr, _tmpx, graph->Y2);
      _tmpx := _tmpx + _stx;
    end;
  end;


  if graph->graph_data->status->grid_y then
  begin
    _tmpy := _tmp0y;
    while _tmpy - graph->Y1 > 8 do 
    begin
      cairo_move_to(graph->cr, graph->X1, _tmpy);
      cairo_line_to(graph->cr, graph->X2, _tmpy);
      _tmpy := _tmpy - _sty;
    end;
  end;

  // Операция cairo_stroke() применяет виртуальный карандаш вдоль контура.
  cairo_stroke(graph->cr);

  // Шкала X
  SetCgaColor(AGraph, graph->graph_data->color->text);
  if graph->graph_data->status->number_x then
  begin
    _tmpx = _tmp0x;
    while graph->X2 - _tmpx > 8 do 
    begin
      OutGridNumber(AGraph, _tmpx, graph->Y2, tmpx, VERT_DIRECTION);
      tmpx := tmpx + stx;
      _tmpx := _tmpx + _stx;
    end;
  end;

  // Шкала Y
  SetCgaColor(AGraph, graph->graph_data->color->text);
  if graph->graph_data->status->number_y then
  begin
    _tmpy = _tmp0y;
    while _tmpy - graph->Y1 > 8 do
    begin
      OutGridNumber(AGraph, graph->X1, _tmpy, tmpy, HORIZ_DIRECTION);
      tmpy := tmpy + sty;
      _tmpy := _tmpy + _sty;
    end;
  end;

end;


function Step(AGraph: PGraph; ASt: Double; AType: Integer): Double
var
  fl: Double = 0.0;
  i: Integer = 0;
begin
  if graph->graph_data->status->origin then
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

    fl := floor(ASt);
    if (ASt - fl) > 0.5 then
      ASt := Ceil(ASt);
    else
      ASt := fl;
    ASt *= pow(10.0, i);
    if (AType = 1) and (ASt < 1) then
      ASt := 1;
  end;
  Result := ASt;
end;


function StepX(AGraph: PGraph): Double
begin
  Result := Step(AGraph, (graph->graph_data->x2 - graph->graph_data->x1) / ((graph->X2 - graph->X1) >> 5), graph->graph_data->status->x_type);
end;


function StepY(AGraph: PGraph): Double
begin
  Result := Step(AGraph, (graph->graph_data->y2 - graph->graph_data->y1) / ((graph->Y2 - graph->Y1) >> 5), graph->graph_data->status->y_type);
end;


// Отрисовка надписей сетки
procedure OutGridNumber(AGraph: PGraph; x, y: Integer; ANumber: Double, AOrient: Integer)
var
  mod_number: Double = fabs(ANumber);
  tmp: LongInt = 0L;
  mode: LongInt = 0L;
  i: Integer = 0;
  hh: Integer = 0;
  mm: Integer = 0;
  ss: Integer = 0;
  dd: Integer = 0;
  x1: Integer = 0;
  y1: Integer = 0;
  x2: Integer = 0;
  y2: Integer = 0;
  char    buffer[16];
begin
  if mod_number < 1E-14 then
    ANumber := 0;

  if AOrient then
    mode := graph->graph_data->status->x_type;
  else
    mode := graph->graph_data->status->y_type;

  case mode of
    GM_TIME:
      tmp := number;
      ss := tmp % 60;
      mm := (tmp / 60) % 60;
      hh := (tmp / 3600) % 24;
      dd := tmp / 86400;
      if (number < 0) or (number >= 864000000) then
        sprintf(buffer, '##:##:##');
      else
        sprintf(buffer, '%02d:%02d:%02d', hh, mm, ss);
      if dd then
      begin
        OutTextXY(AGraph, x1, y1, buffer, AOrient);
        sprintf(buffer, '(%d)', dd);
        OutTextXY(AGraph, x2, y2, buffer, AOrient);
       end;
       else
         OutTextXY(AGraph, x, y, buffer, AOrient);
       // break;

    GM_OPTIMAL:
      if (mod_number < 1000000.0) and (mod_number > 0.00001) or (not ANumber) then
      begin
        if ANumber then
          sprintf(buffer, '% lf', number);
        else
          sprintf(buffer, '0');

        buffer[8] := 0;

        i := 7;
        while (i > 0) and (buffer[i] == '0') do
        begin
          buffer[i] := 0;
          Dec(i);
        end;

        if buffer[i] = '.' then
          buffer[i] := 0;
        OutTextXY(AGraph, x, y, buffer, AOrient);
        // break;
       end;

    GM_EXP:
      sprintf(buffer, '%12.5E', ANumber);
      OutTextXY(AGraph, x2, y2, &buffer[8], AOrient);

      buffer[8] := 0;
      OutTextXY(AGraph, x1, y1, buffer, AOrient);
      break;
  end;
end;


// Отрисовка осей
procedure DrawAxis(AGraph: PGraph)
var
  i: Integer = 0;
begin
  SetCgaColor(AGraph, graph->graph_data->color->axis);
  SetSolidLineStyle(AGraph);

  if (graph->graph_data->y1 <= 0) and (graph->graph_data->y2 >= 0) and (graph->graph_data->status->axis_x) then
  begin
    i := graph->Y2 + (int)(graph->graph_data->y1 * graph->dY);
    cairo_move_to(graph->cr, graph->X1, i);
    cairo_line_to(graph->cr, graph->X2, i);
  end

  if (graph->graph_data->x1 <= 0) and (graph->graph_data->x2 >= 0) and (graph->graph_data->status->axis_y) then
  begin
    i := graph->X1 - (int)(graph->graph_data->x1 * graph->dX);
    cairo_move_to(graph->cr, i, graph->Y1);
    cairo_line_to(graph->cr, i, graph->Y2);
  end;

  // ВНИМАНИЕ! Для отображения ранее отрисованных
  //           линий необходимо вызвать cairo_stroke().
  cairo_stroke(graph->cr);
end;


// Отрисовка самого графика
function DrawGraph(nix_graph_t *graph): Boolean
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
  ix1: LongInt = 0L;
  iy1: LongInt = 0L;
  ix2: LongInt = 0L;
  iy2: LongInt = 0L;
  i: LongInt = 0L;
  j: LongInt = 0L;
  k: LongInt = 0L;
  // unsigned Pattern, Pc, P;
begin
  if graph->graph_data = nil then
  begin
    logfunc.WarningMsg('Don"t define graphic data');
    Result := False;
    exit;
  end;

  if graph->graph_data->n_points = nil then
  begin
    logfunc.WarningMsg('Empty points);
    Result := False;
    exit;
  end;

  if graph->graph_data->n_points < 2 then
  begin
    logfunc.WarningMsgFmt('Point count < 2 %d', [graph->graph_data->n_points]);
    Result := False;
    exit;
  end;

  if graph->graph_data->get_point = nil then
  begin
    logfunc.Warning('Not define <GetPoint> function');
    Result := False;
    exit;
  end;

  // Берем первую точку
  graph->graph_data->GetPoint(graph->graph_data, &x1, &y1, i=0);
  if graph->graph_data->status->dtype then
  begin
      graph->graph_data->GetPoint(graph->graph_data, &x2, &y2, j=graph->graph_data->n_points - 1);
      if (x2 > graph->graph_data->x1) and (x1 < graph->graph_data->x2) then
      begin
        if (x1 < graph->graph_data->x1)
          while((j - i) > 1)
          begin
            k := (i + j) >> 1;
            graph->graph_data->GetPoint(graph->graph_data, &x, &y, k);
            if (x > graph->graph_data->x1) then
            begin
              x2 := x;
              y2 := y;
              j := k;
            end
            else if (x < graph->graph_data->x1) then
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
          i := graph->graph_data->n_points;
  end;

  SetCgaColor(AGraph, graph->graph_data->color->line);
  SetSolidLineStyle(AGraph);

  _x2 := x1;
  _y2 := y1;

  Inc(i);
  while (i < graph->graph_data->n_points) and ((not graph->graph_data->status->dtype) or (x1 < graph->graph_data->x2)) do 
  begin
    graph->graph_data->GetPoint(graph->graph_data, &x2, &y2, i);
    if not ((y1 <= graph->graph_data->y1 && y2 <= graph->graph_data->y1) or 
            (y1 >= graph->graph_data->y2 && y2 >= graph->graph_data->y2) or 
            (x1 <= graph->graph_data->x1 && x2 <= graph->graph_data->x1) or 
            (x1 >= graph->graph_data->x2 && x2 >= graph->graph_data->x2)) then
    begin
          _x1 := _x2;
          _y1 := _y2;
          _x2 := x2;
          _y2 := y2;

          SplitGraph(AGraph, &_x1, &_y1, &_x2, &_y2);

          ix1 := (long)((_x1 - graph->graph_data->x1) * graph->dX) + graph->X1;
          iy1 := graph->Y2 - (long)((_y1 - graph->graph_data->y1) * graph->dY);
          ix2 := (long)((_x2 - graph->graph_data->x1) * graph->dX) + graph->X1;
          iy2 := graph->Y2 - (long)((_y2 - graph->graph_data->y1) * graph->dY);

          cairo_move_to(graph->cr, ix1, iy1);
          cairo_line_to(graph->cr, ix2, iy2);
          logfunc.InfoFmt('Draw line (%d, %d) - (%d, %d)', [(int)ix1, (int)iy1, (int)ix2, (int)iy2]);
    end;

    memcpy(&x1, &x2, 16);
    Inc(i);
  end;
  // ВНИМАНИЕ! Для отображения ранее отрисованных
  //           линий необходимо вызвать cairo_stroke().
  cairo_stroke(graph->cr);

  Result := True;
end;


// Обрезка линии
function SplitGraph(AGraph: PGraph; x1, y1, x2, y2: PDouble): Boolean
begin
 CrossPoint(y1, x1, y2, x2, graph->graph_data->y1, 0);
 CrossPoint(y1, x1, y2, x2, graph->graph_data->y2, 1);
 CrossPoint(x1, y1, x2, y2, graph->graph_data->x1, 0);
 CrossPoint(x1, y1, x2, y2, graph->graph_data->x2, 1);
 Result := True;
end


function CrossPoint(x1, y1, x2, y2: PDouble, x: Double, AMode: Integer): Boolean
var
  y: Double = 0;
  dx1: Double = 0;
  dx2: Double = 0;
  dx: Double = 0;
  dy: Double = 0;
begin
  if x1^ > x2^ then
  begin
    Swap(x1, x2, sizeof(double));
    Swap(y1, y2, sizeof(double));
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
      y := dy / dx * dx1 + y1^;
  else
      y := y2^ - dy / dx * dx2;

  if AMode then
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
                   DX, DY: Double): PGraph
begin
  if AGraph = nil then
  begin
    logfunc.WarningMsg('Not define graph');
    Result := nil;
    exit;
  end;

  graph->X1 := 0;
  graph->Y1 := 0;
  graph->X2 := AWidth - 1;
  graph->Y2 := AHeight - 1;

  graph->dX := DX;
  graph->dY := DY;

  graph->graph_data := graph_data;

  graph->canvas = ACanvas;

  Result := AGraph;
end;


// Инициализация структуры данных графика
function InitGraphData(AGraphData: PGraphData; x1, y1, x2, y2: Double): PGraphData
{
  if AGraphData = nil then
  begin
    logfunc.WarningMsg('Not define graph data');
    Result := nil;
    exit;
  end;

  graph_data->x1 := x1;
  graph_data->y1 := y1;
  graph_data->x2 := x2;
  graph_data->y2 := y2;

  graph_data->status := nil;
  graph_data->color := nil;

  graph_data->n_points := 0;

  //x1, y1, x2, y2 0, 0, 320, 120,
  graph_data->X1 := 0;
  graph_data->Y1 := 0;
  graph_data->X2 := 639;
  graph_data->Y2 := 479;

  log_func.InfoMsg('Init graph data');

  Result := AGraphData;
}

end.
