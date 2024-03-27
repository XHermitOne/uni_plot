{
Модуль функций движка

Версия: 0.0.0.1
}

unit engine;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Crt;

const
  // Имя результирующего файла по умолчанию
  DEFAULT_OUTPUT_PNG_FILENAME: AnsiString = './output.png';
  // По умолчанию вывод графиков в PNG
  DEFAULT_OUTPUT_FILE_FORMAT: AnsiString = 'PNG';

type

// Функция запуска основного алгоритма
//   AOutputFileName - Имя результирующего файла
//   AXType - Тип оси X 
//   AYType - Тип оси Y
// APen0 - Данные пера 1
// APen1 - Данные пера 2
// APen2 - Данные пера 3
// APen3 - Данные пера 4
// APen4 - Данные пера 5
// APen5 - Данные пера 6
// APen6 - Данные пера 7
// APen7 - Данные пера 8
// APen8 - Данные пера 9
// APen9 - Данные пера 10
// ASceneX1, ASceneY1, ASceneX2, ASceneY2 - Сцена
function Run(AOutputFileName: AnsiString; AXType, AYType: Byte; 
	     APen0, APen1, APen2, APen3, APen4, APen5, APen6, APen7, APen8, APen9: PGraphData,
	     AImgWidth, AImgHeight: Integer,
	     ASceneX1, ASceneY1, ASceneX2, ASceneY2, dX, dY: Double): Boolean;

implementation

uses
  strfunc, logfunc, graphfunc;

// Выполнить отрисовку графика в PNG файл
//  png_filename - Полное имя PNG файла
function DrawPNG(APNGFileName: AnsiString; AGraphData: PGraphData; AXType, AYType: Byte,
                 AWidth, AHeight: Integer,
                 ASceneX1, ASceneY1, ASceneX2, ASceneY2, dX, dY: Double): Boolean
var
  cairo_canvas: TCairoPngCanvas;
  graphic: TGraph;
begin
  if strfunc.IsEmpty(APNGFileName) then
    png_filename := DEFAULT_OUTPUT_PNG_FILENAME;

  if AWidth = 0 then
    AWidth := graphfunc.DEFAULT_WIDTH;

  if AHeight = 0 then
    AHeight := graphfunc.DEFAULT_HEIGHT;

  logfunc.InfoMsgFmt('Draw PNG file: %s [%d x %d]', [png_filename, width, height]);

  surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, width, height);
  //cairo_surface_set_device_offset(surface, 0.0, 1.0);
  cr = cairo_create(surface);

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
    if (scene_x1 <> scene_x2) and (scene_y1 <> scene_y2) then
    begin
      AGraphData^.x1 := scene_x1;
      AGraphData^.y1 := scene_y1;
      AGraphData^.x2 := scene_x2;
      AGraphData^.y2 := scene_y2;
      logfunc.InfoMsgFmt('Set graph data scene (%f, %f) - (%f, %f)', [scene_x1, scene_y1, scene_x2, scene_y2]);
    end
    else
      logfunc.InfoMsgFmt('Graph data scene (%f, %f) - (%f, %f)', [scene_x1, scene_y1, scene_x2, scene_y2]);
  end;

  graphfunc.Draw(@graphic, AGraphData, False);

  cairo_fill(cr);

  cairo_surface_write_to_png(surface, png_filename);

  cairo_surface_destroy(surface);
  graphic.canvas := nil;
  cairo_destroy(cr);
  // graphic.cr = NULL;

  Result := True;
end;


// Функция запуска основного алгоритма
function Run(AOutputFileName: AnsiString; AXType, AYType: Byte; 
	     APen0, APen1, APen2, APen3, APen4, APen5, APen6, APen7, APen8, APen9: PGraphData;
	     APen0Color, APen1Color, APen2Color, APen3Color, APen4Color, APen5Color, APen6Color, APen7Color, APen8Color, APen9Color: Byte;
	     ATextColor, AGroundColor, ABorderColor, AGridColor, AAxisColor: Byte;
	     AImgWidth, AImgHeight: Integer;
	     ASceneX1, ASceneY1, ASceneX2, ASceneY2, dX, dY: Double): Boolean;
var
begin
  Result := False;

  if DEFAULT_OUTPUT_FILE_FORMAT = 'PNG' then
    Result := DrawPNG(AOutputFileName, APen0, AXType, AYType, AImgWidth, AImgHeight,
                      ASceneX1, ASceneY1, ASceneX2, ASceneY2, dX, dY);
end;


//  Определить тип значений оси
function ParseAxisType(AAxisType: AnsiString): Byte
begin
  // По умолчанию
  Result := GM_OPTIMAL;
  if AAxisType = 'N' then
    Result := GM_OPTIMAL;
  if AAxisType = 'T' then
    Result := GM_TIME;
end;


//  Определить данные пера
//  Данные в командной строке определяются как x1/y1,x2/y2,...
function ParsePenData(AStrPenData: AnsiString): PGraphData
var
  str_points: TArrayOfString;
  i: Integer = 0;
  str_point: String;
  str_count: Integer;

  prev_time: LongInt = 0;
  prev_data: Double = 0.0;
  i_time: LongInt = 0;
  y_data: Double = 0.0;

  min_time: LongInt = 0;
  min_data: Double = 0.0;
  max_time: LongInt = 0;
  max_data: Double = 0.0;

  pen_data: PGraphData;

  point: TArrayOfString;
  point_data: PGraphPoint;
begin
  logfunc.DebugMsg('Points parse');

  if strfunc.IsEmpty(AStrPenData) then
  begin
    logfunc.WarningMsg('Not define pen data');
    Result := nil;
    exit;
  end;

  str_points := strfunc.SplitStr(AStrPenData, ',');
  str_count := Length(str_points);

  New(pen_data);
  graphfunc.InitGraphData(pen_data, prev_time, prev_data, i_time, y_data);
  SetLength(pen_data^.points, str_count);
  pen_data^.n_points := str_count;

  repeat
    str_point := str_points[i];
    if strfunc.IsEmpty(str_point) then
        break;

    point := strfunc.SplitStr(str_point, '/');
    i_time = toolfunc.StrTimeToLong(point[0]);
    y_data = StrToFloat(point[1]);

    New(point_data);
    point_data^.x := i_time;
    point_data^.y := y_data;

    pen_data^.points[i].x := point_data^.x;
    pen_data^.points[i].y := point_data^.y;

    logfunc.DebugMsgFmt(#9'Point data: [%d : %f] - [%d : %f]', [prev_time, prev_data, i_time, y_data]);
    Dispose(point_data);

    prev_time := i_time;
    prev_data := y_data;

    // Определяем максимаотные и минимальные значения диапазона графика
    if prev_time < min_time then
      min_time := prev_time;
    if prev_data < min_data then
      min_data := prev_data;
    if prev_time > max_time then
      max_time := prev_time;
    if prev_data > max_data then
      max_data := prev_data;

    Inc(i);
  until str_point;

  // Указать границы графика
  pen_data^.x1 := min_time;
  pen_data^.y1 := min_data;
  pen_data^.x2 := max_time;
  pen_data^.y2 := max_data;
  logfunc.DebugMsgFmt('Graphic range: [%d : %f] - [%d : %f]', [min_time, min_data, max_time, max_data]);

  Result := pen_data;
end;


//  Определить данные сцены
//  Данные в командной строке определяются как x1/y1,x2/y2
function ParseSceneData(AStrSceneData: String; x1, y1, x2, y2: PDouble): Boolean
var
  str_points: TArrayOfString;  
  i: Integer= 0;
  str_point: String;
  str_count: Integer;

  prev_time: LongInt = 0;
  prev_data: Double = 0.0;
  i_time: LongInt = 0;
  y_data: Double = 0.0;

  point: TArrayOfString;
begin
  logfunc.DebugMsg('Scene parse');
  if strfunc.IsEmpty(AStrSceneData) then
  begin
    logfunc.WarningMsg('Not define scene data');
    Result := False;
    exit;
  end;

  str_points := strfunc.SplitStr(AStrSceneData, ',');
  str_count := Length(str_points);

  if str_count <> 2 then
  begin
    logfunc.WarningMsg('Not valid scene coordinates');
    Result := False;
    exit;
  end;

  // Определяем первую точку сцены
  str_point := str_points[0];
  if not strfunc.IsEmpty(str_point) then
  begin
    point := strfunc.SplitStr(str_point, '/');
    prev_time := toolfunc.StrTimeToLong(point[0]);
    prev_data := StrToFloat(point[1]);
  end;

  // Определяем вторую точку сцены
  str_point := str_points[1];
  if not strfunc.IsEmpty(str_point) then
  begin
    point := strfunc.SplitStr(str_point, '/');
    i_time := toolfunc.StrTimeToLong(point[0]);
    y_data := StrToFloat(point[1]);
  end;

  logfunc.DebugMsgFmt(#9'Scene data: [%d : %f] - [%d : %f]', [prev_time, prev_data, i_time, y_data]);
  x1^ := prev_time;
  y1^ := prev_data;
  x2^ := i_time;
  y2^ := y_data;

  Result := True;
end;


// Функция определения кода цвета по его имени
function ParseColorByName(AColorName): Byte
begin
  AColorName := UpperCase(AColorName);
  case AColorName do
    'BLACK': 	Result := BLACK_COLOR;
    'BLUE': 	Result := BLUE_COLOR;
    'GREEN': 	Result := GREEN_COLOR
    'CYAN':     Result := CYAN_COLOR;
    'RED':      Result := RED_COLOR;
    'MAGENTA':  Result := MAGENTA_COLOR;
    'BROWN': 	Result := BROWN_COLOR;
    'YELLOW':	Result := YELLOW_COLOR;
    'WHITE':	Result := WHITE_COLOR;

    'LIGHTGRAY':	Result := LIGHTGRAY_COLOR;
    'DARKGRAY':		Result := DARKGRAY_COLOR;
    'LIGHTBLUE':	Result := LIGHTBLUE_COLOR;
    'LIGHTGREEN':	Result := LIGHTGREEN_COLOR;
    'LIGTHCYAN':	Result := LIGTHCYAN_COLOR;
    'LIGHTRED':		Result := LIGHTRED_COLOR;
    'LIGHTMAGENTA':	Result := LIGHTMAGENTA_COLOR;
  else
    logfunc.WarningMsgFmt('Not define color <%s>', [AColorName]);
    // По умолчанию белый цвет
    Result := WHITE_COLOR;
  end;  
end;


end.