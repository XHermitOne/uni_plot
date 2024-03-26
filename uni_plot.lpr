{
Консольный режим работы программы UniPlot.

Версия 0.0.1.1

@bold(Поиск утечек памяти)

Включение поиска утечек:
Меню Lazarus -> Проект -> Параметры проекта... ->
Параметры проекта -> Отладка -> Выставить галки для ключей -gl и -gh

Вывод делаем в текстовый файл *.mem в:

@longcode(#
***********************************************************
if UseHeapTrace then     // Test if reporting is on
   SetHeapTraceOutput(ChangeFileExt(ParamStr(0), '.mem'));
***********************************************************
#)

Допустим, имеем код, который заведомо без утечек:

@longcode(#
***********************************************************
uses heaptrc;
var
  p1, p2, p3: pointer;

begin
  getmem(p1, 100);
  getmem(p2, 200);
  getmem(p3, 300);

  // ...

  freemem(p3);
  freemem(p2);
  freemem(p1);
end.
***********************************************************
#)

, после запуска и завершения работы программы, в консоли наблюдаем отчет:

@longcode(#
***********************************************************
Running "f:\programs\pascal\tst.exe "
Heap dump by heaptrc unit
3 memory blocks allocated : 600/608
3 memory blocks freed     : 600/608
0 unfreed memory blocks : 0
True heap size : 163840 (80 used in System startup)
True free heap : 163760
***********************************************************
#)

Утечек нет, раз "0 unfreed memory blocks"
Теперь внесем утечку, "забудем" вернуть память выделенную под p2:

@longcode(#
***********************************************************
uses heaptrc;
var
  p1, p2, p3: pointer;

begin
  getmem(p1, 100);
  getmem(p2, 200);
  getmem(p3, 300);

  // ...

  freemem(p3);
  // freemem(p2);
  freemem(p1);
end.
***********************************************************
#)

и смотрим на результат:

@longcode(#
***********************************************************
Running "f:\programs\pascal\tst.exe "
Heap dump by heaptrc unit
3 memory blocks allocated : 600/608
2 memory blocks freed     : 400/408
1 unfreed memory blocks : 200
True heap size : 163840 (80 used in System startup)
True free heap : 163488
Should be : 163496
Call trace for block $0005D210 size 200
  $00408231
***********************************************************
#)

200 байт - утечка...
Если будешь компилировать еще и с ключом -gl,
то ко всему прочему получишь и место, где была выделена "утекающая" память.

ВНИМАНИЕ! Если происходят утечки памяти в модулях Indy
необходимо в C:\lazarus\fpc\3.0.4\source\packages\indy\IdCompilerDefines.inc
добавить @code($DEFINE IDFREEONFINAL) в секции FPC (2+)
и перекомпилировать проект.
}

program uni_plot;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  logfunc;

const
  VERSION: AnsiString = '0.0.0.1';

type

  { TUniPlotApplication }

  TUniPlotApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure WriteHelp; virtual;
    procedure WriteVersion; virtual;
  end;

{ TUniPlotApplication }

procedure TUniPlotApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('hvdl', 'help version debug log');
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

    if HasOption('v', 'version') then
  begin
    WriteVersion;
    Terminate;
    Exit;
  end;

  if HasOption('d', 'debug') then
    DEBUG_MODE := True;

  // Очень странно, но при  отключении режима логирования
  // пропадает ошибка Access violation при  вызове ReadRaw

  if HasOption('l', 'log') then
    LOG_MODE := True;

  if LOG_MODE then
    OpenLog(ChangeFileExt(ParamStr(0), '.log'));


  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TUniPlotApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TUniPlotApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TUniPlotApplication.WriteHelp;
begin
  { add your help code here }
  PrintColorTxt('uni_plot - Программа формирования графиков из коммандной строки в виде PNG файла', CYAN_COLOR_TEXT);
  PrintColorTxt(Format(#9'Версия: %s', [VERSION]), CYAN_COLOR_TEXT);
  PrintColorTxt('Параметры командной строки:', CYAN_COLOR_TEXT);
  PrintColorTxt('Вызов команды: ./uni_plot [Помощь и отладка] [Параметры запуска]', CYAN_COLOR_TEXT);
  PrintColorTxt(#9'[Помощь и отладка]', CYAN_COLOR_TEXT);
  PrintColorTxt(Format(#9#9'Напечатать строки помощи: %s --help|-h', [ExeName]), CYAN_COLOR_TEXT);
  PrintColorTxt(Format(#9#9'Напечатать версию программы: %s --version|-v', [ExeName]), CYAN_COLOR_TEXT);
  PrintColorTxt(Format(#9#9'Включить режим отладки: %s --debug|-d', [ExeName]), CYAN_COLOR_TEXT);
  PrintColorTxt(Format(#9#9'Включить режим журналирования: %s --log|-l', [ExeName]), CYAN_COLOR_TEXT);
  // PrintColorTxt(Format('    Файл настройки: %s --settings=имя_файла_настройки.ini', [ExeName]), CYAN_COLOR_TEXT);
  PrintColorTxt(#9'[Параметры запуска]', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Имя результирующего файла: --out=имя_файла.png', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Тип данных оси X [N, T, D, DT, E]: --xtype=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Тип данных оси Y [N, T, D, DT, E]: --ytype=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Данные пера 1 [x1/y1,x2/y2,...]: --pen0=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Данные пера 2 [x1/y1,x2/y2,...]: --pen1=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Данные пера 3 [x1/y1,x2/y2,...]: --pen2=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Данные пера 4 [x1/y1,x2/y2,...]: --pen3=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Данные пера 5 [x1/y1,x2/y2,...]: --pen4=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Данные пера 6 [x1/y1,x2/y2,...]: --pen5=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Данные пера 7 [x1/y1,x2/y2,...]: --pen6=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Данные пера 8 [x1/y1,x2/y2,...]: --pen7=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Данные пера 9 [x1/y1,x2/y2,...]: --pen8=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Данные пера 10 [x1/y1,x2/y2,...]: --pen9=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Цвет пера 1 (Цвета задаются как BLACK, BLUE, GREEN, CYAN, RED, MAGENTA, BROWN, LIGHTGRAY, DARKGRAY, LIGHTBLUE, LIGHTGREEN, LIGTHCYAN, LIGHTRED, LIGHTMAGENTA, YELLOW, WHITE): --pen0_color=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Цвет пера 2: --pen1_color=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Цвет пера 3: --pen2_color=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Цвет пера 4: --pen3_color=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Цвет пера 5: --pen4_color=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Цвет пера 6: --pen5_color=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Цвет пера 7: --pen6_color=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Цвет пера 8: --pen7_color=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Цвет пера 9: --pen8_color=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Цвет пера 10: --pen9_color=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Цвет текста шкал: --text_color=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Цвет фона: --ground_color=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Цвет окантовки: --border_color=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Цвет сетки: --grid_color=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Цвет осей: --axis_color=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Ширина в точках: --width=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Высота в точках: --height=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Данные точек видимой сцены: --scene=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Цена деления сетки по оси X: --dx=', CYAN_COLOR_TEXT);
  PrintColorTxt(#9#9'Цена деления сетки по оси Y: --dy=', CYAN_COLOR_TEXT);
end;


procedure TUniPlotApplication.WriteVersion;
begin
  PrintColorTxt(Format('uni_plot. Версия: %s', [VERSION]), CYAN_COLOR_TEXT);
end;

var
  Application: TUniPlotApplication;
begin
  Application:=TUniPlotApplication.Create(nil);
  Application.Title:='UniPlot';
  Application.Run;
  Application.Free;

  // Учет утечек памяти. Вывод делаем в текстовый файл *.mem
  {$if declared(UseHeapTrace)}
  if UseHeapTrace then // Test if reporting is on
     SetHeapTraceOutput(ChangeFileExt(ParamStr(0), '.mem'));
  {$ifend}

end.
