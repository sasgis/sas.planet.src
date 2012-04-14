{******************************************************************************}
{* SAS.Planet (SAS.œÎ‡ÌÂÚ‡)                                                   *}
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

unit u_CenterScaleConfig;

interface

uses
  i_BitmapMarker,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_CenterScaleConfig,
  u_ConfigDataElementBase;

type
  TCenterScaleConfig = class(TConfigDataElementBase, ICenterScaleConfig)
  private
    FVisible: Boolean;
    FBitmap: IBitmapMarker;
    procedure CreateBitmap;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);

    function GetBitmap: IBitmapMarker;
    procedure SetBitmap(const AValue: IBitmapMarker);
  public
    constructor Create;
  end;

implementation

uses
  Types,
  SysUtils,
  GR32,
  i_Bitmap32Static,
  u_GeoFun,
  u_BitmapMarker,
  u_Bitmap32Static;

{ TCenterScaleConfig }

constructor TCenterScaleConfig.Create;
begin
  inherited;
  FVisible := False;
  CreateBitmap;
end;

procedure TCenterScaleConfig.CreateBitmap;
var
  VBitmap: TBitmap32;
  VHalfSize: TPoint;
  i: integer;
  r: Double;
  xy, xy1: TPoint;
  VSize: TPoint;
  VTextWdth: integer;
  VRadius: Integer;
  VFontSize: Integer;
  VDigitsOffset: Integer;
  VBitmapStatic: IBitmap32Static;
begin
  VBitmap := TBitmap32.Create;
  try
    VRadius := 115;
    VDigitsOffset := 20;
    VFontSize := 12;
    VBitmap.Font.Size := VFontSize;
    VTextWdth := VBitmap.TextWidth('270∞');
    VSize := Point((VRadius * 2) + (VDigitsOffset * 2) + (VTextWdth * 2), (VRadius * 2) + (VDigitsOffset * 2) + (VTextWdth * 2));
    VHalfSize := Point(VSize.X div 2, VSize.Y div 2);
    VBitmap.SetSize(VSize.X, VSize.Y);
    VBitmap.Clear(0);
    VBitmap.Font.Size := VFontSize - 3;

    i := 0;
    While i < 360 do begin
      VBitmap.Font.Size := VFontSize - 3;
      if (i mod 90) = 0 then begin
        r := 0;
        VBitmap.Font.Size := VFontSize;
      end else if (i mod 45) = 0 then begin
        r := VRadius - 40;
        VBitmap.Font.Size := VFontSize - 1;
      end else begin
        r := VRadius - 10;
      end;
      xy.x := round(VHalfSize.X + VRadius * cos(i * (Pi / 180)));
      xy.y := round(VHalfSize.Y + VRadius * sin(i * (Pi / 180)));
      xy1.x := round(VHalfSize.X + r * cos(i * (Pi / 180)));
      xy1.y := round(VHalfSize.Y + r * sin(i * (Pi / 180)));
      VBitmap.LineFS(xy.x, xy.y, xy1.x, xy1.y, SetAlpha(clRed32, 180));
      if (i mod 15) = 0 then begin
        xy1.x := round(VHalfSize.X + (VRadius + VDigitsOffset) * cos(i * (Pi / 180))) - VBitmap.TextWidth(inttostr((i + 90) mod 360) + '∞') div 2;
        xy1.y := round(VHalfSize.X + (VRadius + VDigitsOffset) * sin(i * (Pi / 180))) - 2 - VBitmap.Font.size div 2;
        VBitmap.RenderText(xy1.x + 1, xy1.y + 1, inttostr((i + 90) mod 360) + '∞', 3, SetAlpha(clWhite32, 150));
        VBitmap.RenderText(xy1.x, xy1.y, inttostr((i + 90) mod 360) + '∞', 3, SetAlpha(clBlue32, 210));
      end;
      inc(i, 5);
    end;
    VBitmapStatic := TBitmap32Static.CreateWithCopy(VBitmap);
    FBitmap := TBitmapMarker.Create(VBitmapStatic, DoublePoint(VHalfSize));
  finally
    VBitmap.Free;
  end;
end;

procedure TCenterScaleConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVisible := AConfigData.ReadBool('Visible', FVisible);
    CreateBitmap;
    SetChanged;
  end;
end;

procedure TCenterScaleConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('Visible', FVisible);
end;

function TCenterScaleConfig.GetBitmap: IBitmapMarker;
begin
  LockRead;
  try
    Result := FBitmap;
  finally
    UnlockRead;
  end;
end;

function TCenterScaleConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

procedure TCenterScaleConfig.SetBitmap(const AValue: IBitmapMarker);
begin
  LockWrite;
  try
    if FBitmap <> AValue then begin
      FBitmap := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TCenterScaleConfig.SetVisible(const AValue: Boolean);
begin
  LockWrite;
  try
    if FVisible <> AValue then begin
      FVisible := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
