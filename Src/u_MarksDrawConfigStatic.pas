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

unit u_MarksDrawConfigStatic;

interface

uses
  Types,
  t_Hash,
  i_MarksDrawConfig,
  u_BaseInterfacedObject;

type
  TCaptionDrawConfigStatic = class(TBaseInterfacedObject, ICaptionDrawConfigStatic)
  private
    FFontName: string;
    FShowPointCaption: Boolean;
    FUseSolidCaptionBackground: Boolean;

    FHash: THashValue;
    function CalcHash: THashValue;
  private
    { ICaptionDrawConfigStatic }
    function GetHash: THashValue;
    function GetFontName: string;
    function GetShowPointCaption: Boolean;
    function GetUseSolidCaptionBackground: Boolean;
  public
    constructor Create(
      const AFontName: string;
      const AShowPointCaption: Boolean;
      const AUseSolidCaptionBackground: Boolean
    );
  end;

  TMarksDrawOrderConfigStatic = class(TBaseInterfacedObject, IMarksDrawOrderConfigStatic)
  private
    FOverSizeRect: TRect;
  private
    function GetOverSizeRect: TRect;
  public
    constructor Create(
      const AOverSizeRect: TRect
    );
  end;

implementation

uses
  libcrc32;

{ TMarksDrawConfigStatic }

constructor TMarksDrawOrderConfigStatic.Create(
  const AOverSizeRect: TRect
);
begin
  inherited Create;
  FOverSizeRect := AOverSizeRect;
end;

function TMarksDrawOrderConfigStatic.GetOverSizeRect: TRect;
begin
  Result := FOverSizeRect;
end;

{ TCaptionDrawConfigStatic }

constructor TCaptionDrawConfigStatic.Create(
  const AFontName: string;
  const AShowPointCaption: Boolean;
  const AUseSolidCaptionBackground: Boolean
);
begin
  inherited Create;

  FFontName := AFontName;
  FShowPointCaption := AShowPointCaption;
  FUseSolidCaptionBackground := AUseSolidCaptionBackground;

  FHash := CalcHash;
end;

function TCaptionDrawConfigStatic.CalcHash: THashValue;
var
  VHash: THashValue;
begin
  VHash := $01;
  if not FShowPointCaption then begin
    VHash := $02;
  end;
  Result := VHash;

  VHash := $04;
  if not FUseSolidCaptionBackground then begin
    VHash := $08;
  end;
  Result := Result or VHash;

  if FFontName <> '' then begin
    Result := (Result shl 32) +
      crc32(0, Pointer(FFontName), Length(FFontName) * SizeOf(Char));
  end;
end;

function TCaptionDrawConfigStatic.GetHash: THashValue;
begin
  Result := FHash;
end;

function TCaptionDrawConfigStatic.GetFontName: string;
begin
  Result := FFontName;
end;

function TCaptionDrawConfigStatic.GetShowPointCaption: Boolean;
begin
  Result := FShowPointCaption;
end;

function TCaptionDrawConfigStatic.GetUseSolidCaptionBackground: Boolean;
begin
  Result := FUseSolidCaptionBackground;
end;

end.
