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

unit u_ContentConverterKml2Kmz;

interface

uses
  Classes,
  u_ContentConverterBase;

type
  TContentConverterKml2Kmz = class(TContentConverterBase)
  protected
    procedure ConvertStream(ASource, ATarget: TStream); override;
  end;

implementation

uses
  KAZip;

{ TContentConverterKmz2Kml }

procedure TContentConverterKml2Kmz.ConvertStream(ASource, ATarget: TStream);
var
  VZip:TKAZip;
begin
  inherited;
  VZip:=TKAZip.Create(nil);
  try
    VZip.CreateZip(ATarget);
    VZip.Open(ATarget);
    VZip.CompressionType := ctNormal;
    VZip.AddStream('doc.kml', ASource);
  finally
    VZip.Free;
  end;
end;

end.
