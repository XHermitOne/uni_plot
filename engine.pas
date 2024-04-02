{
Модуль функций движка

Версия: 0.0.2.1
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
	     APens: Array of PGraphData;
	     ATextColor, AGroundColor, ABorderColor, AGridColor, AAxisColor: Byte;
	     AImgWidth, AImgHeight: Integer;
	     ASceneX1, ASceneY1, ASceneX2, ASceneY2, dX, dY: Double): Boolean;

//  Определить тип значений оси
function ParseAxisType(AAxisType: AnsiString): Byte;
//  Определить данные пера
//  Данные в командной строке определяются как x1/y1,x2/y2,...
function ParsePenData(AStrPenData: AnsiString; XAxisType, YAxisType: Byte): PGraphData;
// Функция определения кода цвета по его имени
function ParseColorByName(AColorName: String): Byte;
//  Определить данные сцены
//  Данные в командной строке определяются как x1/y1,x2/y2
function ParseSceneData(AStrSceneData: String; x1, y1, x2, y2: PDouble; XAxisType, YAxisType: Byte): Boolean;

// Прочитать файл данных и преобразовать данные в формате данных пера
function ReadSrcDataFile(ASrcFileName: String): String;

implementation

uses
  strfunc, logfunc, toolfunc;

// Функция запуска основного алгоритма
function Run(AOutputFileName: AnsiString; AXType, AYType: Byte; 
	     APens: Array of PGraphData;
	     ATextColor, AGroundColor, ABorderColor, AGridColor, AAxisColor: Byte;
	     AImgWidth, AImgHeight: Integer;
	     ASceneX1, ASceneY1, ASceneX2, ASceneY2, dX, dY: Double): Boolean;
begin
  Result := False;

  if DEFAULT_OUTPUT_FILE_FORMAT = 'PNG' then
    try
      Result := DrawPNG(AOutputFileName, AXType, AYType, AImgWidth, AImgHeight,
                        ASceneX1, ASceneY1, ASceneX2, ASceneY2, dX, dY,
                        ATextColor, AGroundColor, ABorderColor, AGridColor, AAxisColor,
                        APens);
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
function ParsePenData(AStrPenData: AnsiString; XAxisType, YAxisType: Byte): PGraphData;
var
  str_points: TArrayOfString;
  i: Integer = 0;
  str_point: String;
  str_count: Integer;

  x_data: Double = 0.0;
  y_data: Double = 0.0;

  pen_data: PGraphData;

  point: TArrayOfString;
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
  graphfunc.InitGraphData(pen_data);
  SetLength(pen_data^.points, str_count);
  pen_data^.n_points := str_count;

  for i := 0 to str_count - 1 do
  begin
    str_point := str_points[i];
    if strfunc.IsEmptyStr(str_point) then
        break;

    point := strfunc.SplitStr(str_point, '/');
    if XAxisType = GM_TIME then
      x_data := toolfunc.StrTimeToLong(point[0])
    else
      x_data := StrToFloat(point[0]);
    y_data := StrToFloat(point[1]);

    pen_data^.points[i].x := x_data;
    pen_data^.points[i].y := y_data;

    logfunc.DebugMsgFmt(#9'Point data: [%f : %f]', [x_data, y_data]);

  end;

  Result := pen_data;
end;


//  Определить данные сцены
//  Данные в командной строке определяются как x1/y1,x2/y2
function ParseSceneData(AStrSceneData: String; x1, y1, x2, y2: PDouble; XAxisType, YAxisType: Byte): Boolean;
var
  str_points: TArrayOfString;  
  str_point: String;
  str_count: Integer;

  prev_x: Double = 0.0;
  prev_y: Double = 0.0;
  x_data: Double = 0;
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
    if XAxisType = GM_TIME then
      prev_x := toolfunc.StrTimeToLong(point[0])
    else
      prev_x := StrToFloat(point[0]);
    prev_y := StrToFloat(point[1]);
  end;

  // Определяем вторую точку сцены
  str_point := str_points[1];
  if not strfunc.IsEmptyStr(str_point) then
  begin
    point := strfunc.SplitStr(str_point, '/');
    if XAxisType = GM_TIME then
      x_data := toolfunc.StrTimeToLong(point[0])
    else
      x_data := StrToFloat(point[0]);
    y_data := StrToFloat(point[1]);
  end;

  logfunc.DebugMsgFmt(#9'Scene data: [%f : %f] - [%f : %f]', [prev_x, prev_y, x_data, y_data]);
  x1^ := prev_x;
  y1^ := prev_y;
  x2^ := x_data;
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


// Прочитать файл данных и преобразовать данные в формате данных пера
function ReadSrcDataFile(ASrcFileName: String): String;
var
  data_file: Text;
  line: String;
  x_str, y_str: String;
begin
  Result := '';

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

    if strfunc.IsEmptyStr(Result) then
      Result := Format('%s/%s', [x_str, y_str])
    else
      Result := Result + Format(',%s/%s', [x_str, y_str])
  end;
end;

end.
