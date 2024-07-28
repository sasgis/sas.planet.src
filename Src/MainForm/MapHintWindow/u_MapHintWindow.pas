{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_MapHintWindow;

interface

uses
  Types,
  Classes,
  Controls,
  GR32_Image,
  i_VectorItemSubset;

type
  TMapHintWindow = class
  private
    FOwner: TComponent;
    FMap: TImage32;
    FHintWindow: THintWindow;
    function MakeHintText(
      const AItems: IVectorItemSubset
    ): string;
  public
    procedure ShowHint(
      const AMousePos: TPoint;
      const AVectorItems: IVectorItemSubset
    );
    procedure HideHint;
  public
    constructor Create(
      const AOwner: TComponent;
      const AMap: TImage32
    );
    destructor Destroy; override;
  end;

implementation

uses
  Forms,
  SysUtils,
  Graphics;

{ TMapHintWindow }

constructor TMapHintWindow.Create(
  const AOwner: TComponent;
  const AMap: TImage32
);
begin
  inherited Create;
  FOwner := AOwner;
  FMap := AMap;
end;

destructor TMapHintWindow.Destroy;
begin
  HideHint;
  inherited Destroy;
end;

procedure TMapHintWindow.ShowHint(
  const AMousePos: TPoint;
  const AVectorItems: IVectorItemSubset
);
var
  VHintText: string;
  VHintRect: TRect;
begin
  VHintText := MakeHintText(AVectorItems);

  if VHintText = '' then begin
    HideHint;
    Exit;
  end;

  if FMap.Cursor = crDefault then begin
    FMap.Cursor := crHandPoint;
  end;

  if FHintWindow = nil then begin
    FHintWindow := THintWindow.Create(FOwner);
    FHintWindow.Brush.Color := clInfoBk;
  end;

  VHintRect := FHintWindow.CalcHintRect(Screen.Width, VHintText, nil);

  FHintWindow.ActivateHint(
    Bounds(
      AMousePos.X + 13,
      AMousePos.Y - 13,
      Abs(VHintRect.Right - VHintRect.Left),
      Abs(VHintRect.Top - VHintRect.Bottom)
    ),
    VHintText
  );

  FHintWindow.Repaint;
end;

procedure TMapHintWindow.HideHint;
begin
  if FHintWindow <> nil then begin
    FHintWindow.ReleaseHandle;
    FreeAndNil(FHintWindow);
    if FMap.Cursor = crHandPoint then begin
      FMap.Cursor := crDefault;
    end;
  end;
end;

function TMapHintWindow.MakeHintText(
  const AItems: IVectorItemSubset
): string;
const
  CSep = #13#10'----------------'#13#10;
var
  I: Integer;
begin
  Result := '';
  if AItems <> nil then begin
    for I := 0 to AItems.Count - 1 do begin
      if Result = '' then begin
        Result := AItems[I].GetHintText;
      end else begin
        Result := Result + CSep + AItems[I].GetHintText;
      end;
    end;
  end;
end;

end.
