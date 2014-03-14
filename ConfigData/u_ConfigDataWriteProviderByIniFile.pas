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

unit u_ConfigDataWriteProviderByIniFile;

interface

uses
  IniFiles,
  i_BinaryData,
  i_ConfigDataWriteProvider,
  u_ConfigDataProviderByIniFile;

type
  TConfigDataWriteProviderByIniFile = class(TConfigDataProviderByIniFile, IConfigDataWriteProvider)
  protected
    function GetOrCreateSubItem(const AIdent: string): IConfigDataWriteProvider;
    procedure DeleteSubItem(const AIdent: string);
    procedure DeleteValue(const AIdent: string);
    procedure DeleteValues;
    procedure WriteBinary(
      const AIdent: string;
      const AValue: IBinaryData
    );
    procedure WriteString(
      const AIdent: string;
      const AValue: string
    );
    procedure WriteAnsiString(
      const AIdent: string;
      const AValue: AnsiString
    );
    procedure WriteInteger(
      const AIdent: string;
      const AValue: Longint
    );
    procedure WriteBool(
      const AIdent: string;
      const AValue: Boolean
    );
    procedure WriteDate(
      const AIdent: string;
      const AValue: TDateTime
    );
    procedure WriteDateTime(
      const AIdent: string;
      const AValue: TDateTime
    );
    procedure WriteFloat(
      const AIdent: string;
      const AValue: Double
    );
    procedure WriteTime(
      const AIdent: string;
      const AValue: TDateTime
    );
  public
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_ConfigDataWriteProviderByIniFileSection;

{ TConfigDataWriteProviderByIniFile }

procedure TConfigDataWriteProviderByIniFile.DeleteSubItem(const AIdent: string);
begin
  IniFile.EraseSection(AIdent);
end;

procedure TConfigDataWriteProviderByIniFile.DeleteValue(const AIdent: string);
begin
  raise Exception.Create('Not expected');
end;

procedure TConfigDataWriteProviderByIniFile.DeleteValues;
begin
  raise Exception.Create('Not expected');
end;

destructor TConfigDataWriteProviderByIniFile.Destroy;
begin
  if Assigned(IniFile) then begin
    try
      IniFile.UpdateFile;
    except
    end;
  end;
  inherited;
end;

function TConfigDataWriteProviderByIniFile.GetOrCreateSubItem(
  const AIdent: string): IConfigDataWriteProvider;
begin
  Result := TConfigDataWriteProviderByIniFileSection.Create(IniFile, AIdent, Self);
end;

procedure TConfigDataWriteProviderByIniFile.WriteAnsiString(
  const AIdent: string;
  const AValue: AnsiString
);
begin
  raise Exception.Create('Not expected');
end;

procedure TConfigDataWriteProviderByIniFile.WriteBinary(
  const AIdent: string;
  const AValue: IBinaryData
);
begin
  raise Exception.Create('Not expected');
end;

procedure TConfigDataWriteProviderByIniFile.WriteBool(
  const AIdent: string;
  const AValue: Boolean
);
begin
  raise Exception.Create('Not expected');
end;

procedure TConfigDataWriteProviderByIniFile.WriteDate(
  const AIdent: string;
  const AValue: TDateTime
);
begin
  raise Exception.Create('Not expected');
end;

procedure TConfigDataWriteProviderByIniFile.WriteDateTime(
  const AIdent: string;
  const AValue: TDateTime
);
begin
  raise Exception.Create('Not expected');
end;

procedure TConfigDataWriteProviderByIniFile.WriteFloat(
  const AIdent: string;
  const AValue: Double
);
begin
  raise Exception.Create('Not expected');
end;

procedure TConfigDataWriteProviderByIniFile.WriteInteger(
  const AIdent: string;
  const AValue: Integer
);
begin
  raise Exception.Create('Not expected');
end;

procedure TConfigDataWriteProviderByIniFile.WriteString(const AIdent,
  AValue: string);
begin
  raise Exception.Create('Not expected');
end;

procedure TConfigDataWriteProviderByIniFile.WriteTime(
  const AIdent: string;
  const AValue: TDateTime
);
begin
  raise Exception.Create('Not expected');
end;

end.
