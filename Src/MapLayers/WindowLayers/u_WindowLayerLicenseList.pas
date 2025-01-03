{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_WindowLayerLicenseList;

interface

uses
  Types,
  Graphics,
  GR32,
  GR32_Image,
  i_StringListChangeable,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  u_WindowLayerWithBitmapBase;

type
  TWindowLayerLicenseList = class(TWindowLayerWithBitmapBase)
  private
    FLicenseList: IStringListChangeable;

    procedure OnListChange;
  protected
    function GetNewBitmapSize: TPoint; override;
    function GetNewLayerLocation: TFloatRect; override;
    procedure DoUpdateBitmapDraw; override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const ALicenseList: IStringListChangeable
    );
  end;

implementation

uses
  GR32_Layers,
  i_StringListStatic,
  u_ListenerByEvent;

{ TLayerLicenseList }

constructor TWindowLayerLicenseList.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier,
  AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const ALicenseList: IStringListChangeable
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    TBitmapLayer.Create(AParentMap.Layers)
  );
  FLicenseList := ALicenseList;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnListChange),
    FLicenseList.ChangeNotifier
  );
end;

procedure TWindowLayerLicenseList.DoUpdateBitmapDraw;
var
  VStrings: IStringListStatic;
  VRowsCount: Integer;
  i: Integer;
  VString: string;
  VFontHight: Integer;
begin
  inherited;
  VStrings := FLicenseList.GetStatic;
  if VStrings.Count > 0 then begin
    VRowsCount := VStrings.Count;
    if VRowsCount > 5 then begin
      VRowsCount := 5;
    end;
    Layer.Bitmap.Clear(0);
    VFontHight := Abs(Layer.Bitmap.Font.Height);
    for i := 0 to VRowsCount - 1 do begin
      VString := VStrings.Items[i];
      Layer.Bitmap.RenderText(5, VFontHight * i + 5, VString, clWhite32, False);
    end;
  end;
end;

function TWindowLayerLicenseList.GetNewBitmapSize: TPoint;
var
  VStrings: IStringListStatic;
  VRowsCount: Integer;
begin
  VStrings := FLicenseList.GetStatic;
  if VStrings.Count > 0 then begin
    VRowsCount := VStrings.Count;
    if VRowsCount > 5 then begin
      VRowsCount := 5;
    end;

    Result := Types.Point(1000, Abs(Layer.Bitmap.Font.Height) * VRowsCount + 10);
  end else begin
    Result := Types.Point(0, 0);
  end;
end;

function TWindowLayerLicenseList.GetNewLayerLocation: TFloatRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Result.Left + Layer.Bitmap.Width;
  Result.Bottom := Result.Top + Layer.Bitmap.Height;
end;

procedure TWindowLayerLicenseList.OnListChange;
var
  VList: IStringListStatic;
begin
  ViewUpdateLock;
  try
    VList := FLicenseList.GetStatic;
    Visible := Assigned(VList) and (VList.Count > 0);
    SetNeedUpdateBitmapSize;
    SetNeedUpdateBitmapDraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWindowLayerLicenseList.StartThreads;
begin
  inherited;
  OnListChange;
end;

end.
