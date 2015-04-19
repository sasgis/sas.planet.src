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
    FShowPointCaption: Boolean;
    FUseSolidCaptionBackground: Boolean;
  private
    function GetHash: THashValue;
    function GetShowPointCaption: Boolean;
    function GetUseSolidCaptionBackground: Boolean;
  public
    constructor Create(
      AShowPointCaption: Boolean;
      AUseSolidCaptionBackground: Boolean
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
  AShowPointCaption,
  AUseSolidCaptionBackground: Boolean
);
begin
  inherited Create;
  FShowPointCaption := AShowPointCaption;
  FUseSolidCaptionBackground := AUseSolidCaptionBackground;
end;

function TCaptionDrawConfigStatic.GetHash: THashValue;
var
  VHash: THashValue;
begin
  VHash := $162e192b2957163d;
  if not FShowPointCaption then begin
    VHash := not VHash;
  end;
  Result := VHash;
  VHash := $f3786a4b25827c1;
  if not FUseSolidCaptionBackground then begin
    VHash := not VHash;
  end;
  Result := Result xor VHash;
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
