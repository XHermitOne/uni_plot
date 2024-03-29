{
Модуль функций движка

Версия: 0.0.0.1
}

unit engine;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Crt,
    exttypes, graphfunc;

const
  // По умолчанию вывод графиков в PNG
  DEFAULT_OUTPUT_FILE_FORMAT: AnsiString = 'PNG';

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
	     APen0, APen1, APen2, APen3, APen4, APen5, APen6, APen7, APen8, APen9: PGraphData;
	     APen0Color, APen1Color, APen2Color, APen3Color, APen4Color, APen5Color, APen6Color, APen7Color, APen8Color, APen9Color: Byte;
	     ATextColor, AGroundColor, ABorderColor, AGridColor, AAxisColor: Byte;
	     AImgWidth, AImgHeight: Integer;
	     ASceneX1, ASceneY1, ASceneX2, ASceneY2, dX, dY: Double): Boolean;

//  Определить тип значений оси
function ParseAxisType(AAxisType: AnsiString): Byte;
//  Определить данные пера
//  Данные в командной строке определяются как x1/y1,x2/y2,...
function ParsePenData(AStrPenData: AnsiString): PGraphData;
// Функция определения кода цвета по его имени
function ParseColorByName(AColorName: String): Byte;
//  Определить данные сцены
//  Данные в командной строке определяются как x1/y1,x2/y2
function ParseSceneData(AStrSceneData: String; x1, y1, x2, y2: PDouble): Boolean;


implementation

uses
  strfunc, logfunc, toolfunc;



// Функция запуска основного алгоритма
function Run(AOutputFileName: AnsiString; AXType, AYType: Byte; 
	     APen0, APen1, APen2, APen3, APen4, APen5, APen6, APen7, APen8, APen9: PGraphData;
	     APen0Color, APen1Color, APen2Color, APen3Color, APen4Color, APen5Color, APen6Color, APen7Color, APen8Color, APen9Color: Byte;
	     ATextColor, AGroundColor, ABorderColor, AGridColor, AAxisColor: Byte;
	     AImgWidth, AImgHeight: Integer;
	     ASceneX1, ASceneY1, ASceneX2, ASceneY2, dX, dY: Double): Boolean;
begin
  Result := False;

  if DEFAULT_OUTPUT_FILE_FORMAT = 'PNG' then
    try
      Result := DrawPNG(AOutputFileName, APen0, AXType, AYType, AImgWidth, AImgHeight,
                        ASceneX1, ASceneY1, ASceneX2, ASceneY2, dX, dY);
    except
      logfunc.FatalMsg('Ошибка выполнения');
    end;
end;


//  Определить тип значений оси
function ParseAxisType(AAxisType: AnsiString): Byte;
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
function ParsePenData(AStrPenData: AnsiString): PGraphData;
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
  logfunc.DebugMsgFmt('Points parse <%s>:', [AStrPenData]);

  if strfunc.IsEmptyStr(AStrPenData) then
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
    if strfunc.IsEmptyStr(str_point) then
        break;

    point := strfunc.SplitStr(str_point, '/');
    i_time := toolfunc.StrTimeToLong(point[0]);
    y_data := StrToFloat(point[1]);

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
  until not strfunc.IsEmptyStr(str_point);

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
function ParseSceneData(AStrSceneData: String; x1, y1, x2, y2: PDouble): Boolean;
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
  if strfunc.IsEmptyStr(AStrSceneData) then
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
  if not strfunc.IsEmptyStr(str_point) then
  begin
    point := strfunc.SplitStr(str_point, '/');
    prev_time := toolfunc.StrTimeToLong(point[0]);
    prev_data := StrToFloat(point[1]);
  end;

  // Определяем вторую точку сцены
  str_point := str_points[1];
  if not strfunc.IsEmptyStr(str_point) then
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
function ParseColorByName(AColorName: String): Byte;
begin
  AColorName := UpperCase(AColorName);
  case AColorName of
    'BLACK': 	Result := BLACK_COLOR;
    'BLUE': 	Result := BLUE_COLOR;
    'GREEN': 	Result := GREEN_COLOR;
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
