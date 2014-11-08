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

unit u_ConfigDataWriteProviderWithGlobal;

interface

uses
  i_BinaryData,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataProviderWithGlobal;

type
  TConfigDataWriteProviderWithGlobal = class(TConfigDataProviderWithGlobal, IConfigDataWriteProvider)
  private
    FProviderMain: IConfigDataWriteProvider;
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
    constructor Create(
      const AProviderMain: IConfigDataWriteProvider;
      const AProviderGlobalPrefix: string;
      const AProviderGlobal: IConfigDataProvider
    );
  end;

implementation

uses
  SysUtils;

{ TConfigDataWriteProviderWithGlobal }

constructor TConfigDataWriteProviderWithGlobal.Create(
  const AProviderMain: IConfigDataWriteProvider;
  const AProviderGlobalPrefix: string;
  const AProviderGlobal: IConfigDataProvider
);
begin
  inherited Create(AProviderMain, AProviderGlobalPrefix, AProviderGlobal);
  FProviderMain := AProviderMain;
end;

procedure TConfigDataWriteProviderWithGlobal.DeleteSubItem(
  const AIdent: string);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.DeleteSubItem(VIdent);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.DeleteValue(const AIdent: string);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.DeleteValue(VIdent);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.DeleteValues;
begin
  if FProviderMain <> nil then begin
    FProviderMain.DeleteValues;
  end;
end;

function TConfigDataWriteProviderWithGlobal.GetOrCreateSubItem(
  const AIdent: string): IConfigDataWriteProvider;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  Result := nil;
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      Result := TConfigDataWriteProviderWithGlobal.Create(FProviderMain.GetOrCreateSubItem(VIdent), ProviderGlobalPrefix, ProviderGlobal);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.WriteBinary(
  const AIdent: string;
  const AValue: IBinaryData
);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.WriteBinary(VIdent, AValue);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.WriteBool(
  const AIdent: string;
  const AValue: Boolean
);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.WriteBool(VIdent, AValue);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.WriteDate(
  const AIdent: string;
  const AValue: TDateTime
);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.WriteDate(VIdent, AValue);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.WriteDateTime(
  const AIdent: string;
  const AValue: TDateTime
);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.WriteDateTime(VIdent, AValue);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.WriteFloat(
  const AIdent: string;
  const AValue: Double
);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.WriteFloat(VIdent, AValue);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.WriteInteger(
  const AIdent: string;
  const AValue: Integer
);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.WriteInteger(VIdent, AValue);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.WriteString(const AIdent,
  AValue: string);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.WriteString(VIdent, AValue);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.WriteAnsiString(
  const AIdent: string;
  const AValue: AnsiString
);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.WriteAnsiString(VIdent, AValue);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.WriteTime(
  const AIdent: string;
  const AValue: TDateTime
);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.WriteTime(VIdent, AValue);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

end.
