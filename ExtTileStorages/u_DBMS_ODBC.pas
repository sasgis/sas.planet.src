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

unit u_DBMS_ODBC;

interface

uses
  t_ETS_Result,
  u_DBMS_Basic;

type
  TDBMS_Environment_ODBC = class(TDBMS_Environment_Basic)
  
  end;

  TDBMS_Connection_ODBC = class(TDBMS_Connection_Basic)
  
  end;

  TDBMS_Link_ODBC = class(TDBMS_Link_Basic)
  protected
    procedure Internal_Underlaying_Disconnect; override;
    function Internal_Underlaying_Connect: LongInt; override;
  end;

implementation

{ TDBMS_Link_ODBC }

function TDBMS_Link_ODBC.Internal_Underlaying_Connect: LongInt;
begin
  Result:=ETSR_NOT_IMPLEMENTED;
end;

procedure TDBMS_Link_ODBC.Internal_Underlaying_Disconnect;
begin
  inherited;

end;

end.