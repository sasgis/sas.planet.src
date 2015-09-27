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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_BitmapLayerProviderChangeableBase;

interface

uses
  SysUtils,
  i_SimpleFlag,
  i_BitmapLayerProvider,
  i_BitmapLayerProviderChangeable,
  i_ListenerNotifierLinksList,
  u_ChangeableBase;

type
  TBitmapLayerProviderChangeableBase = class(TChangeableBase, IBitmapLayerProviderChangeable)
  private
    FLock: IReadWriteSync;
    FChangedFlag: ISimpleFlag;
    FLinksList: IListenerNotifierLinksList;
    FStatic: IBitmapTileUniProvider;
    FLockCounter: Integer;
  private
    function GetStatic: IBitmapTileUniProvider;

  protected
    property LinksList: IListenerNotifierLinksList read FLinksList;
    procedure SetChanged;
    procedure LockRead;
    procedure LockWrite;
    procedure UnlockRead;
    procedure UnlockWrite;
    function CreateStatic: IBitmapTileUniProvider; virtual; abstract;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    constructor Create;
  end;

implementation

uses
  u_SimpleFlagWithInterlock,
  u_ListenerNotifierLinksList,
  u_Synchronizer;

{ TBitmapLayerProviderChangeableBase }

constructor TBitmapLayerProviderChangeableBase.Create;
begin
  inherited Create(GSync.SyncVariable.Make(Self.ClassName + 'Notifiers'));
  FLock := GSync.SyncVariableRecursive.Make(Self.ClassName);
  FChangedFlag := TSimpleFlagWithInterlock.Create;

  FLinksList := TListenerNotifierLinksList.Create;
  FLockCounter := 0;
end;

procedure TBitmapLayerProviderChangeableBase.AfterConstruction;
begin
  inherited;
  FStatic := CreateStatic;
  FLinksList.ActivateLinks;
end;

procedure TBitmapLayerProviderChangeableBase.BeforeDestruction;
begin
  inherited;
  FLinksList.DeactivateLinks;
end;

function TBitmapLayerProviderChangeableBase.GetStatic: IBitmapTileUniProvider;
begin
  FLock.BeginRead;
  try
    Result := FStatic;
  finally
    FLock.EndRead;
  end;
end;

procedure TBitmapLayerProviderChangeableBase.LockRead;
begin
  FLock.BeginRead;
end;

procedure TBitmapLayerProviderChangeableBase.LockWrite;
begin
  FLock.BeginRead;
  Inc(FLockCounter);
end;

procedure TBitmapLayerProviderChangeableBase.SetChanged;
begin
  FChangedFlag.SetFlag;
end;

procedure TBitmapLayerProviderChangeableBase.UnlockRead;
begin
  FLock.EndRead;
end;

procedure TBitmapLayerProviderChangeableBase.UnlockWrite;
var
  VNeedNotify: Boolean;
begin
  Dec(FLockCounter);

  VNeedNotify := False;
  if FLockCounter = 0 then begin
    VNeedNotify := FChangedFlag.CheckFlagAndReset;
    if VNeedNotify then begin
      FStatic := CreateStatic;
    end;
  end;
  Assert(FLockCounter >= 0);
  FLock.EndWrite;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

end.
