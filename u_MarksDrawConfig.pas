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

unit u_MarksDrawConfig;

interface

uses
  Types,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MarksDrawConfig,
  u_ConfigDataElementBase;

type
  TMarksDrawConfig = class(TConfigDataElementWithStaticBase, IMarksDrawConfig)
  private
    FShowPointCaption: Boolean;
    FUseSolidCaptionBackground: Boolean;
    FUseSimpleDrawOrder: Boolean;
    FOverSizeRect: TRect;
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

    function GetUseSimpleDrawOrder: Boolean;
    procedure SetUseSimpleDrawOrder(AValue: Boolean);

    function GetOverSizeRect: TRect;
    procedure SetOverSizeRect(AValue: TRect);

    function GetStatic: IMarksDrawConfigStatic;
  public
    constructor Create;
  end;

implementation

uses
  u_MarksDrawConfigStatic;

{ TMarksDrawConfig }

constructor TMarksDrawConfig.Create;
begin
  inherited Create;

  FShowPointCaption := True;
  FUseSimpleDrawOrder := false;
  FUseSolidCaptionBackground := False;
  FOverSizeRect := Rect(256, 128, 64, 128);
end;

function TMarksDrawConfig.CreateStatic: IInterface;
var
  VStatic: IMarksDrawConfigStatic;
begin
  VStatic :=
    TMarksDrawConfigStatic.Create(
      TCaptionDrawConfigStatic.Create(FShowPointCaption, FUseSolidCaptionBackground),
      FUseSimpleDrawOrder,
      FOverSizeRect
    );
  Result := VStatic;
end;

procedure TMarksDrawConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FShowPointCaption := AConfigData.ReadBool('ShowPointCaption', FShowPointCaption);
    FUseSolidCaptionBackground := AConfigData.ReadBool('UseSolidCaptionBackground', FUseSolidCaptionBackground);
    FUseSimpleDrawOrder := AConfigData.ReadBool('UseSimpleDrawOrder', FUseSimpleDrawOrder);
    FOverSizeRect.Left := AConfigData.ReadInteger('OverSizeRect.Left', FOverSizeRect.Left);
    FOverSizeRect.Top := AConfigData.ReadInteger('OverSizeRect.Top', FOverSizeRect.Top);
    FOverSizeRect.Right := AConfigData.ReadInteger('OverSizeRect.Right', FOverSizeRect.Right);
    FOverSizeRect.Bottom := AConfigData.ReadInteger('OverSizeRect.Bottom', FOverSizeRect.Bottom);
    SetChanged;
  end;
end;

procedure TMarksDrawConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('ShowPointCaption', FShowPointCaption);
  AConfigData.WriteBool('UseSolidCaptionBackground', FUseSolidCaptionBackground);
  AConfigData.WriteBool('UseSimpleDrawOrder', FUseSimpleDrawOrder);
  AConfigData.WriteInteger('OverSizeRect.Left', FOverSizeRect.Left);
  AConfigData.WriteInteger('OverSizeRect.Top', FOverSizeRect.Top);
  AConfigData.WriteInteger('OverSizeRect.Right', FOverSizeRect.Right);
  AConfigData.WriteInteger('OverSizeRect.Bottom', FOverSizeRect.Bottom);
end;

function TMarksDrawConfig.GetOverSizeRect: TRect;
begin
  LockRead;
  try
    Result := FOverSizeRect;
  finally
    UnlockRead;
  end;
end;

function TMarksDrawConfig.GetShowPointCaption: Boolean;
begin
  LockRead;
  try
    Result := FShowPointCaption;
  finally
    UnlockRead;
  end;
end;

function TMarksDrawConfig.GetUseSimpleDrawOrder: Boolean;
begin
  LockRead;
  try
    Result := FUseSimpleDrawOrder;
  finally
    UnlockRead;
  end;
end;

function TMarksDrawConfig.GetUseSolidCaptionBackground: Boolean;
begin
  LockRead;
  try
    Result := FUseSolidCaptionBackground;
  finally
    UnlockRead;
  end;
end;

function TMarksDrawConfig.GetStatic: IMarksDrawConfigStatic;
begin
  Result := IMarksDrawConfigStatic(GetStaticInternal);
end;

procedure TMarksDrawConfig.SetOverSizeRect(AValue: TRect);
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

procedure TMarksDrawConfig.SetShowPointCaption(AValue: Boolean);
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

procedure TMarksDrawConfig.SetUseSimpleDrawOrder(AValue: Boolean);
begin
  LockWrite;
  try
    if FUseSimpleDrawOrder <> AValue then begin
      FUseSimpleDrawOrder := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMarksDrawConfig.SetUseSolidCaptionBackground(AValue: Boolean);
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
