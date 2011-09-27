{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_MainFormMainConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ContentTypeManager,
  i_MainFormConfig,
  u_ConfigDataElementBase;

type
  TMainFormMainConfig = class(TConfigDataElementBase, IMainFormMainConfig)
  private
    FContentTypeManager: IContentTypeManager;
    FShowMapName: Boolean;
    FMouseScrollInvert: Boolean;
    FShowHintOnMarks: Boolean;

    FRullerFileName: string;
    FRuller: TCustomBitmap32;
    FTumblerFileName: string;
    FTumbler: TCustomBitmap32;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetShowMapName: Boolean;
    procedure SetShowMapName(AValue: Boolean);

    function GetMouseScrollInvert: Boolean;
    procedure SetMouseScrollInvert(AValue: Boolean);

    function GetShowHintOnMarks: Boolean;
    procedure SetShowHintOnMarks(AValue: Boolean);

    function GetRuller: TCustomBitmap32;
    function GetTumbler: TCustomBitmap32;
  public
    constructor Create(
      AContentTypeManager: IContentTypeManager
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_ConfigProviderHelpers;

{ TMainFormMainConfig }

constructor TMainFormMainConfig.Create(
  AContentTypeManager: IContentTypeManager
);
begin
  inherited Create;
  FContentTypeManager := AContentTypeManager;
  FShowMapName := True;
  FMouseScrollInvert := False;
  FShowHintOnMarks := True;
  FRuller := TCustomBitmap32.Create;
  FTumbler := TCustomBitmap32.Create;

  FRullerFileName := 'sas:\Resource\VRULLER.png';
  FTumblerFileName := 'sas:\Resource\VTUMBLER.png';
end;

destructor TMainFormMainConfig.Destroy;
begin
  FreeAndNil(FRuller);
  FreeAndNil(FTumbler);
  inherited;
end;

procedure TMainFormMainConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FShowMapName := AConfigData.ReadBool('ShowMapNameOnPanel', FShowMapName);
    FMouseScrollInvert := AConfigData.ReadBool('MouseScrollInvert', FMouseScrollInvert);
    FShowHintOnMarks := AConfigData.ReadBool('ShowHintOnMarks', FShowHintOnMarks);

    ReadBitmapByFileRef(AConfigData, FRullerFileName, FContentTypeManager, FRuller);
    ReadBitmapByFileRef(AConfigData, FTumblerFileName, FContentTypeManager, FTumbler);

    SetChanged;
  end;
end;

procedure TMainFormMainConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('ShowMapNameOnPanel', FShowMapName);
  AConfigData.WriteBool('MouseScrollInvert', FMouseScrollInvert);
  AConfigData.WriteBool('ShowHintOnMarks', FShowHintOnMarks);
end;

function TMainFormMainConfig.GetMouseScrollInvert: Boolean;
begin
  LockRead;
  try
    Result := FMouseScrollInvert;
  finally
    UnlockRead;
  end;
end;

function TMainFormMainConfig.GetRuller: TCustomBitmap32;
begin
  LockRead;
  try
    Result := FRuller;
  finally
    UnlockRead;
  end;
end;

function TMainFormMainConfig.GetShowHintOnMarks: Boolean;
begin
  LockRead;
  try
    Result := FShowHintOnMarks;
  finally
    UnlockRead;
  end;
end;

function TMainFormMainConfig.GetShowMapName: Boolean;
begin
  LockRead;
  try
    Result := FShowMapName;
  finally
    UnlockRead;
  end;
end;

function TMainFormMainConfig.GetTumbler: TCustomBitmap32;
begin
  LockRead;
  try
    Result := FTumbler;
  finally
    UnlockRead;
  end;
end;

procedure TMainFormMainConfig.SetMouseScrollInvert(AValue: Boolean);
begin
  LockWrite;
  try
    if FMouseScrollInvert <> AValue then begin
      FMouseScrollInvert := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainFormMainConfig.SetShowHintOnMarks(AValue: Boolean);
begin
  LockWrite;
  try
    if FShowHintOnMarks <> AValue then begin
      FShowHintOnMarks := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainFormMainConfig.SetShowMapName(AValue: Boolean);
begin
  LockWrite;
  try
    if FShowMapName <> AValue then begin
      FShowMapName := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
