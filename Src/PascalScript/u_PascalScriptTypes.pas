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

unit u_PascalScriptTypes;

interface
uses
  uPSRuntime,
  uPSCompiler;

procedure CompileTimeReg_CommonTypes(const APSComp: TPSPascalCompiler);
procedure CompileTimeReg_ProjConverter(const APSComp: TPSPascalCompiler);
procedure CompileTimeReg_ProjConverterFactory(const APSComp: TPSPascalCompiler);
procedure CompileTimeReg_CoordConverterSimple(const APSComp: TPSPascalCompiler);
procedure CompileTimeReg_SimpleHttpDownloader(const APSComp: TPSPascalCompiler);

implementation

uses
  i_ProjConverter,
  i_CoordConverterSimple,
  i_SimpleHttpDownloader;

procedure CompileTimeReg_CommonTypes(const APSComp: TPSPascalCompiler);
begin
  APSComp.AddTypeS('TPoint', 'record X, Y: Integer; end;');
  APSComp.AddTypeS('TDoublePoint', 'record X, Y: Double; end;');
  APSComp.AddTypeS('TRect', 'record Left, Top, Right, Bottom: Integer; end;');
end;

procedure CompileTimeReg_ProjConverter(const APSComp: TPSPascalCompiler);
begin
  with APSComp.AddInterface(APSComp.FindInterface('IUnknown'), IProjConverter, 'IProjConverter') do begin
    RegisterMethod('function LonLat2XY(const AProjLP: TDoublePoint): TDoublePoint', cdRegister);
    RegisterMethod('function XY2LonLat(const AProjXY: TDoublePoint): TDoublePoint', cdRegister);
  end;
end;

procedure CompileTimeReg_ProjConverterFactory(const APSComp: TPSPascalCompiler);
begin
  with APSComp.AddInterface(APSComp.FindInterface('IUnknown'), IProjConverterFactory, 'IProjConverterFactory') do begin
    RegisterMethod('function GetByEPSG(const AEPSG: Integer): IProjConverter', cdRegister);
    RegisterMethod('function GetByInitString(const AArgs: AnsiString): IProjConverter', cdRegister);
  end;
end;

procedure CompileTimeReg_CoordConverterSimple(const APSComp: TPSPascalCompiler);
begin
  with APSComp.AddInterface(APSComp.FindInterface('IUnknown'), ICoordConverterSimple, 'ICoordConverter') do begin
    RegisterMethod('function Pos2LonLat(const XY: TPoint; Azoom: byte): TDoublePoint', cdStdCall);
    RegisterMethod('function LonLat2Pos(const Ll: TDoublePoint; Azoom: byte): Tpoint', cdStdCall);

    RegisterMethod('function LonLat2Metr(const Ll: TDoublePoint): TDoublePoint', cdStdCall);
    RegisterMethod('function Metr2LonLat(const Mm: TDoublePoint): TDoublePoint', cdStdCall);

    RegisterMethod('function TilesAtZoom(AZoom: byte): Longint', cdStdCall);
    RegisterMethod('function PixelsAtZoom(AZoom: byte): Longint', cdStdCall);

    RegisterMethod('function TilePos2PixelPos(const XY: TPoint; Azoom: byte): TPoint', cdStdCall);
    RegisterMethod('function TilePos2PixelRect(const XY: TPoint; Azoom: byte): TRect', cdStdCall);
  end;
end;

procedure CompileTimeReg_SimpleHttpDownloader(const APSComp: TPSPascalCompiler);
begin
  with APSComp.AddInterface(APSComp.FindInterface('IUnknown'), ISimpleHttpDownloader, 'ISimpleHttpDownloader') do begin
    RegisterMethod('function DoHttpRequest(const ARequestUrl, ARequestHeader, APostData: AnsiString; out AResponseHeader, AResponseData: AnsiString): Cardinal', cdRegister);
  end;
end;

end.
