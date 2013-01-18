{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit t_MarksSQLDb;

interface

type
  TMarksSQLiteOptions = record
  public
    const c_Current_Version = 1;
  public
    IdUserStr: AnsiString;
    IdVersion: Integer;
    // для SQLite некоторые опции игнорируются
    // соответственно хранятся только нужные
    MonitoringModeValue: Integer;
    MultiuserModeValue: Integer;
    CoordStorageModeValue: Integer;
  public
    procedure Clear;
    function ActualVersion: Boolean; inline;
  end;

  // опции из g_option
  Tg_option_ID = (
    // monitoring mode: 0 - disabled, 1 - by triggers, 2 - by stored procedures, 3 - by host
    // SQLite: only 0 or 3
    mgo_MonitoringMode         = 1,

    // referential integrity: 0 - disabled, 1 - by triggers, 2 - by stored procedures, 3 - by host
    // SQLite: ignore (use only triggers)
    mgo_ReferentialIntegrity   = 2,

    // monitoring timeout: seconds to keep log rows (0 - clear by host)
    // SQLite: ignore (monitoring by host without timeouts)
    mgo_MonitoringTimeout      = 3,

    // multiuser mode: 0 - all connections use single user with id=0 (by default)
    mgo_MultiuserMode          = 4,

    // coordinates storage mode: 0 - as double, 1 - use c_COORD_to_INT, else - treat as int fraction
    mgo_CoordStorageMode       = 5
  );

const
  // $B60000  = 11927552
  // максимум = 2146959360 = 7FF80000
  // квант    = 0.000000083839500343406
  // при длине экватора 40075км одному кванту соответствует 9мм на экваторе
  c_COORD_to_INT = $B60000;

type
  // простые однопоточные быстрые микрокэшики "на стеке"
  // должны "работать" при массовой вставке (или обновлении) меток
  TIdShowCache1 = packed record
  private
    FKey1: Integer;
    FResult: Integer;
  public
    procedure Init;
    function GetInCache(const AKey1: Integer; out AResult: Integer): Boolean;
    procedure SetInCache(const AKey1, AResult: Integer);
  end;

  // указатели на массивчики - чтобы удобнее параметры передавать
  TIdShowData2 = array [0..1] of Integer;
  PIdShowData2 = ^TIdShowData2;
  TIdShowCache2 = packed record
  private
    FKeys: TIdShowData2;
    FResult: Integer;
  public
    procedure Init;
    function GetInCache(const AKeys: PIdShowData2; out AResult: Integer): Boolean;
    procedure SetInCache(const AKeys: PIdShowData2; const AResult: Integer);
  end;

  TIdShowData4 = array [0..3] of Integer;
  PIdShowData4 = ^TIdShowData4;
  TIdShowCache4 = packed record
  private
    FKeys: TIdShowData4;
    FResult: Integer;
  public
    procedure Init;
    function GetInCache(const AKeys: PIdShowData4; out AResult: Integer): Boolean;
    procedure SetInCache(const AKeys: PIdShowData4; const AResult: Integer);
  end;

  TIdShowCacheStr = record
  private
    FKey: String;
    FResult: Integer;
  public
    procedure Init;
    function GetInCache(const AKey: String; out AResult: Integer): Boolean;
    procedure SetInCache(const AKey: String; const AResult: Integer);
  end;
  
implementation

{ TMarksSQLiteOptions }

function TMarksSQLiteOptions.ActualVersion: Boolean;
begin
  Result := (c_Current_Version = IdVersion)
end;

procedure TMarksSQLiteOptions.Clear;
begin
  IdUserStr := '';
  FillChar(Self, SizeOf(Self), 0);
end;

{ TIdShowCache1 }

function TIdShowCache1.GetInCache(const AKey1: Integer; out AResult: Integer): Boolean;
begin
  Result := (AKey1=FKey1);
  if Result then begin
    AResult := FResult;
    if (0=AResult) then
      Result := FALSE;
  end;
end;

procedure TIdShowCache1.Init;
begin
  FKey1 := 0;
  FResult := 0;
end;

procedure TIdShowCache1.SetInCache(const AKey1, AResult: Integer);
begin
  FKey1 := AKey1;
  FResult := AResult;
end;

{ TIdShowCache2 }

function TIdShowCache2.GetInCache(const AKeys: PIdShowData2; out AResult: Integer): Boolean;
begin
  Result := (FResult<>0) and (FKeys[0]=AKeys^[0]) and (FKeys[1]=AKeys^[1]);
  if Result then
    AResult := FResult;
end;

procedure TIdShowCache2.Init;
begin
  FKeys[0] := 0;
  FKeys[1] := 0;
  FResult  := 0;
end;

procedure TIdShowCache2.SetInCache(const AKeys: PIdShowData2; const AResult: Integer);
begin
  FKeys := AKeys^;
  FResult := AResult;
end;

{ TIdShowCache4 }

function TIdShowCache4.GetInCache(const AKeys: PIdShowData4; out AResult: Integer): Boolean;
begin
  Result := (FResult<>0) and (FKeys[0]=AKeys^[0]) and (FKeys[1]=AKeys^[1])
                         and (FKeys[2]=AKeys^[2]) and (FKeys[3]=AKeys^[3]);
  if Result then
    AResult := FResult;
end;

procedure TIdShowCache4.Init;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

procedure TIdShowCache4.SetInCache(const AKeys: PIdShowData4; const AResult: Integer);
begin
  FKeys := AKeys^;
  FResult := AResult;
end;

{ TIdShowCacheStr }

function TIdShowCacheStr.GetInCache(const AKey: String; out AResult: Integer): Boolean;
begin
  Result := (FResult<>0) and (AKey=FKey);
  if Result then
    AResult := FResult;
end;

procedure TIdShowCacheStr.Init;
begin
  FKey := '';
  FResult := 0;
end;

procedure TIdShowCacheStr.SetInCache(const AKey: String; const AResult: Integer);
begin
  FKey := AKey;
  FResult := AResult;
end;

end.
