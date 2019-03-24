{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit i_WinInetConfig;

interface

uses
  i_ConfigDataElement;

type
  TConnsPerServerRec = record
    IsAvailable: Boolean;
    Value: Cardinal;
    Def: Cardinal;
    Min: Cardinal;
    Max: Cardinal;
    class operator Equal(const A, B: TConnsPerServerRec): Boolean; inline;
    class operator NotEqual(const A, B: TConnsPerServerRec): Boolean; inline;
  end;

  IWinInetConfigStatic = interface
    ['{61BA4EA7-448C-4012-AA2C-DA758CD30451}']

    // available for IE5 and later
    function GetMaxConnsPerServer: TConnsPerServerRec;
    property MaxConnsPerServer: TConnsPerServerRec read GetMaxConnsPerServer;

    // available for IE8 and later
    function GetMaxConnsPerProxy: TConnsPerServerRec;
    property MaxConnsPerProxy: TConnsPerServerRec read GetMaxConnsPerProxy;
  end;

  IWinInetConfig = interface(IConfigDataElement)
    ['{EE25B7A4-CB08-4C38-8D87-1954A7F0FF04}']
    function GetMaxConnsPerServer: TConnsPerServerRec;
    procedure SetMaxConnsPerServer(const AValue: TConnsPerServerRec);
    property MaxConnsPerServer: TConnsPerServerRec read GetMaxConnsPerServer write SetMaxConnsPerServer;

    function GetMaxConnsPerProxy: TConnsPerServerRec;
    procedure SetMaxConnsPerProxy(const AValue: TConnsPerServerRec);
    property MaxConnsPerProxy: TConnsPerServerRec read GetMaxConnsPerProxy write SetMaxConnsPerProxy;

    function GetStatic: IWinInetConfigStatic;
  end;

implementation

{ TConnsPerServerRec }

class operator TConnsPerServerRec.Equal(const A, B: TConnsPerServerRec): Boolean;
begin
  Result :=
    (A.IsAvailable = B.IsAvailable) and
    (A.Value = B.Value) and
    (A.Def = B.Def) and
    (A.Min = B.Min) and
    (A.Max = B.Max);
end;

class operator TConnsPerServerRec.NotEqual(const A, B: TConnsPerServerRec): Boolean;
begin
  Result := not (A = B);
end;

end.
