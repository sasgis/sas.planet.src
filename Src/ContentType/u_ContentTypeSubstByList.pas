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

unit u_ContentTypeSubstByList;

interface

uses
  u_AnsiStr,
  i_ConfigDataProvider,
  i_ContentTypeSubst,
  u_BaseInterfacedObject;

type
  TContentTypeSubstByList = class(TBaseInterfacedObject, IContentTypeSubst)
  private
    FSource: TStringListA;
    FTarget: TStringListA;
    procedure ParseSubstList(const ASubstListText: AnsiString);
    procedure ParseSubstListItem(const ASubstListItemText: AnsiString);
  private
    function GetContentType(const ASource: AnsiString): AnsiString;
  public
    constructor Create(const AConfig: IConfigDataProvider);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TContentTypeSubstByList }

constructor TContentTypeSubstByList.Create(const AConfig: IConfigDataProvider);
var
  VSubstListText: AnsiString;
begin
  inherited Create;
  FSource := TStringListA.Create;
  FTarget := TStringListA.Create;
  VSubstListText := AConfig.ReadAnsiString('MimeTypeSubst', '');
  if VSubstListText <> '' then begin
    ParseSubstList(VSubstListText);
  end;
  FSource.Sorted := True;
end;

destructor TContentTypeSubstByList.Destroy;
begin
  FreeAndNil(FSource);
  FreeAndNil(FTarget);
  inherited;
end;

function TContentTypeSubstByList.GetContentType(const ASource: AnsiString): AnsiString;
var
  VSourceIndex: Integer;
  VTargetIndex: Integer;
begin
  if FSource.Find(ASource, VSourceIndex) then begin
    VTargetIndex := Integer(FSource.Objects[VSourceIndex]);
    Result := FTarget.Strings[VTargetIndex];
  end else begin
    Result := ASource;
  end;
end;

procedure TContentTypeSubstByList.ParseSubstList(const ASubstListText: AnsiString);
var
  VTempList: TStringListA;
  i: Integer;
  VSubstItemText: AnsiString;
begin
  VTempList := TStringListA.Create;
  try
    VTempList.QuoteChar := '"';
    VTempList.Delimiter := ';';
    VTempList.DelimitedText := ASubstListText;
    for i := 0 to VTempList.Count - 1 do begin
      VSubstItemText := VTempList.Strings[i];
      if VSubstItemText <> '' then begin
        ParseSubstListItem(VSubstItemText);
      end;
    end;
  finally
    VTempList.Free;
  end;
end;

procedure TContentTypeSubstByList.ParseSubstListItem(
  const ASubstListItemText: AnsiString);
var
  VTempList: TStringListA;
  VSource: AnsiString;
  VTarget: AnsiString;
  VTargetIndex: Integer;
begin
  VTempList := TStringListA.Create;
  try
    VTempList.QuoteChar := '"';
    VTempList.Delimiter := '=';
    VTempList.DelimitedText := ASubstListItemText;
    if VTempList.Count = 2 then begin
      VSource := VTempList.Strings[0];
      VTarget := VTempList.Strings[1];
      if (VSource <> '') and (VTarget <> '') then begin
        VTargetIndex := FTarget.Add(VTarget);
        FSource.AddObject(VSource, TObject(VTargetIndex));
      end;
    end;
  finally
    VTempList.Free;
  end;
end;

end.
