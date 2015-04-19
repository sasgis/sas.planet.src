{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_MarksDrawConfig;

interface

uses
  Types,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MarksDrawConfig,
  u_ConfigDataElementBase,
  u_ConfigDataElementComplexBase;

type
  TCaptionDrawConfig = class(TConfigDataElementWithStaticBase, ICaptionDrawConfig)
  private
    FShowPointCaption: Boolean;
    FUseSolidCaptionBackground: Boolean;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetShowPointCaption: Boolean;
    procedure SetShowPointCaption(AValue: Boolean);

    function GetUseSolidCaptionBackground: Boolean;
    procedure SetUseSolidCaptionBackground(AValue: Boolean);

    function GetStatic: ICaptionDrawConfigStatic;
  public
    constructor Create;
  end;

  TMarksDrawOrderConfig = class(TConfigDataElementWithStaticBase, IMarksDrawOrderConfig)
  private
    FOverSizeRect: TRect;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetOverSizeRect: TRect;
    procedure SetOverSizeRect(AValue: TRect);

    function GetStatic: IMarksDrawOrderConfigStatic;
  public
    constructor Create;
  end;

  TMarksDrawConfig = class(TConfigDataElementComplexBase, IMarksDrawConfig)
  private
    FCaptionDrawConfig: ICaptionDrawConfig;
    FDrawOrderConfig: IMarksDrawOrderConfig;
  private
    function GetCaptionDrawConfig: ICaptionDrawConfig;
    function GetDrawOrderConfig: IMarksDrawOrderConfig;
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_MarksDrawConfigStatic;

{ TMarksDrawConfig }

constructor TMarksDrawOrderConfig.Create;
begin
  inherited Create;

  FOverSizeRect := Rect(256, 128, 64, 128);
end;

function TMarksDrawOrderConfig.CreateStatic: IInterface;
var
  VStatic: IMarksDrawOrderConfigStatic;
begin
  VStatic :=
    TMarksDrawOrderConfigStatic.Create(
      FOverSizeRect
    );
  Result := VStatic;
end;

procedure TMarksDrawOrderConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FOverSizeRect.Left := AConfigData.ReadInteger('OverSizeRect.Left', FOverSizeRect.Left);
    FOverSizeRect.Top := AConfigData.ReadInteger('OverSizeRect.Top', FOverSizeRect.Top);
    FOverSizeRect.Right := AConfigData.ReadInteger('OverSizeRect.Right', FOverSizeRect.Right);
    FOverSizeRect.Bottom := AConfigData.ReadInteger('OverSizeRect.Bottom', FOverSizeRect.Bottom);
    SetChanged;
  end;
end;

procedure TMarksDrawOrderConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('OverSizeRect.Left', FOverSizeRect.Left);
  AConfigData.WriteInteger('OverSizeRect.Top', FOverSizeRect.Top);
  AConfigData.WriteInteger('OverSizeRect.Right', FOverSizeRect.Right);
  AConfigData.WriteInteger('OverSizeRect.Bottom', FOverSizeRect.Bottom);
end;

function TMarksDrawOrderConfig.GetOverSizeRect: TRect;
begin
  LockRead;
  try
    Result := FOverSizeRect;
  finally
    UnlockRead;
  end;
end;

function TMarksDrawOrderConfig.GetStatic: IMarksDrawOrderConfigStatic;
begin
  Result := IMarksDrawOrderConfigStatic(GetStaticInternal);
end;

procedure TMarksDrawOrderConfig.SetOverSizeRect(AValue: TRect);
begin
  LockWrite;
  try
    if not EqualRect(FOverSizeRect, AValue) then begin
      FOverSizeRect := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

{ TMarksDrawConfig }

constructor TMarksDrawConfig.Create;
begin
  inherited Create;
  FCaptionDrawConfig := TCaptionDrawConfig.Create;
  Add(FCaptionDrawConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);

  FDrawOrderConfig := TMarksDrawOrderConfig.Create;
  Add(FDrawOrderConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

function TMarksDrawConfig.GetCaptionDrawConfig: ICaptionDrawConfig;
begin
  Result := FCaptionDrawConfig;
end;

function TMarksDrawConfig.GetDrawOrderConfig: IMarksDrawOrderConfig;
begin
  Result := FDrawOrderConfig;
end;

{ TCaptionDrawConfig }

constructor TCaptionDrawConfig.Create;
begin
  inherited Create;

  FShowPointCaption := True;
  FUseSolidCaptionBackground := False;
end;

function TCaptionDrawConfig.CreateStatic: IInterface;
var
  VStatic: ICaptionDrawConfigStatic;
begin
  VStatic :=
    TCaptionDrawConfigStatic.Create(
      FShowPointCaption,
      FUseSolidCaptionBackground
    );
  Result := VStatic;
end;

procedure TCaptionDrawConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FShowPointCaption := AConfigData.ReadBool('ShowPointCaption', FShowPointCaption);
    FUseSolidCaptionBackground := AConfigData.ReadBool('UseSolidCaptionBackground', FUseSolidCaptionBackground);
    SetChanged;
  end;
end;

procedure TCaptionDrawConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('ShowPointCaption', FShowPointCaption);
  AConfigData.WriteBool('UseSolidCaptionBackground', FUseSolidCaptionBackground);
end;

function TCaptionDrawConfig.GetStatic: ICaptionDrawConfigStatic;
begin
  Result := ICaptionDrawConfigStatic(GetStaticInternal);
end;

function TCaptionDrawConfig.GetShowPointCaption: Boolean;
begin
  LockRead;
  try
    Result := FShowPointCaption;
  finally
    UnlockRead;
  end;
end;

function TCaptionDrawConfig.GetUseSolidCaptionBackground: Boolean;
begin
  LockRead;
  try
    Result := FUseSolidCaptionBackground;
  finally
    UnlockRead;
  end;
end;

procedure TCaptionDrawConfig.SetShowPointCaption(AValue: Boolean);
begin
  LockWrite;
  try
    if FShowPointCaption <> AValue then begin
      FShowPointCaption := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TCaptionDrawConfig.SetUseSolidCaptionBackground(AValue: Boolean);
begin
  LockWrite;
  try
    if FUseSolidCaptionBackground <> AValue then begin
      FUseSolidCaptionBackground := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
