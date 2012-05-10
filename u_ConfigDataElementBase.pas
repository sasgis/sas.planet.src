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

unit u_ConfigDataElementBase;

interface

uses
  Windows,
  SysUtils,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ConfigDataElement,
  u_ChangeableBase;

type
  TConfigDataElementBase = class(TChangeableBase, IConfigDataElement)
  private
    FLock: TMultiReadExclusiveWriteSynchronizer;
    FStopNotifyCounter: Longint;
    FNeedNotify: Longint;
  protected
    procedure SetChanged;
    function CheckIsChangedAndReset: Boolean;
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); virtual; abstract;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); virtual; abstract;
  protected
    procedure LockRead; virtual;
    procedure LockWrite; virtual;
    procedure UnlockRead; virtual;
    procedure UnlockWrite; virtual;
    procedure ReadConfig(const AConfigData: IConfigDataProvider);
    procedure WriteConfig(const AConfigData: IConfigDataWriteProvider);
    procedure StopNotify; virtual;
    procedure StartNotify; virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    constructor Create();
    destructor Destroy; override;
  end;

  TConfigDataElementWithStaticBase = class(TConfigDataElementBase)
  private
    FStatic: IInterface;
    FStaticCS: IReadWriteSync;
  protected
    function CreateStatic: IInterface; virtual; abstract;
  protected
    procedure DoBeforeChangeNotify; override;
    function GetStaticInternal: IInterface; virtual;
  public
    procedure AfterConstruction; override;
  public
    constructor Create();
    destructor Destroy; override;
  end;

type
  TConfigDataElementBaseEmptySaveLoad = class(TConfigDataElementBase)
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  end;

  TConfigDataElementWithStaticBaseEmptySaveLoad = class(TConfigDataElementWithStaticBase)
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  end;

implementation

uses
  u_Synchronizer;

{ TConfigDataElementBase }

constructor TConfigDataElementBase.Create;
begin
  inherited;
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
  FStopNotifyCounter := 0;
end;

destructor TConfigDataElementBase.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

procedure TConfigDataElementBase.AfterConstruction;
begin
  inherited;
  FNeedNotify := 0;
end;

procedure TConfigDataElementBase.BeforeDestruction;
begin
  inherited;
  StopNotify;
end;

function TConfigDataElementBase.CheckIsChangedAndReset: Boolean;
begin
  Result := InterlockedExchange(FNeedNotify, 0) <> 0;
end;

procedure TConfigDataElementBase.LockRead;
begin
  FLock.BeginRead;
end;

procedure TConfigDataElementBase.LockWrite;
begin
  StopNotify;
  FLock.BeginWrite;
end;

procedure TConfigDataElementBase.ReadConfig(const AConfigData: IConfigDataProvider);
begin
  LockWrite;
  try
    DoReadConfig(AConfigData);
  finally
    UnlockWrite;
  end;
end;

procedure TConfigDataElementBase.SetChanged;
begin
  InterlockedIncrement(FNeedNotify);
end;

procedure TConfigDataElementBase.StartNotify;
var
  VCouner: Longint;
begin
  VCouner := InterlockedDecrement(FStopNotifyCounter);
  if VCouner = 0 then begin
    if CheckIsChangedAndReset then begin
      DoChangeNotify;
    end;
  end;
end;

procedure TConfigDataElementBase.StopNotify;
begin
  InterlockedIncrement(FStopNotifyCounter);
end;

procedure TConfigDataElementBase.UnlockRead;
begin
  FLock.EndRead;
end;

procedure TConfigDataElementBase.UnlockWrite;
begin
  FLock.EndWrite;
  StartNotify;
end;

procedure TConfigDataElementBase.WriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  LockRead;
  try
    DoWriteConfig(AConfigData);
  finally
    UnlockRead;
  end;
end;

{ TConfigDataElementWithStaticBase }

procedure TConfigDataElementWithStaticBase.AfterConstruction;
begin
  inherited;
  FStatic := CreateStatic;
end;

constructor TConfigDataElementWithStaticBase.Create;
begin
  inherited;
  FStaticCS := MakeSyncRW_Var(Self);
end;

destructor TConfigDataElementWithStaticBase.Destroy;
begin
  FStaticCS := nil;
  FStatic := nil;
  inherited;
end;

procedure TConfigDataElementWithStaticBase.DoBeforeChangeNotify;
var
  VStatic: IInterface;
begin
  inherited;
  LockWrite;
  try
    VStatic := CreateStatic;
    FStaticCS.BeginWrite;
    try
      FStatic := VStatic;
    finally
      FStaticCS.EndWrite;
    end;
  finally
    UnlockWrite;
  end;
end;

function TConfigDataElementWithStaticBase.GetStaticInternal: IInterface;
begin
  FStaticCS.BeginRead;
  try
    Result := FStatic;
  finally
    FStaticCS.EndRead;
  end;
end;

{ TConfigDataElementBaseEmptySaveLoad }

procedure TConfigDataElementBaseEmptySaveLoad.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
end;

procedure TConfigDataElementBaseEmptySaveLoad.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
end;

{ TConfigDataElementWithStaticBaseEmptySaveLoad }

procedure TConfigDataElementWithStaticBaseEmptySaveLoad.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
end;

procedure TConfigDataElementWithStaticBaseEmptySaveLoad.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
end;

end.
