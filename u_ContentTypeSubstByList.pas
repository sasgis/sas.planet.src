{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_ContentTypeSubstByList;

interface

uses
  Classes,
  i_ConfigDataProvider,
  i_ContentTypeSubst;

type
  TContentTypeSubstByList = class(TInterfacedObject, IContentTypeSubst)
  private
    FSource: TStringList;
    FTarget: TStringList;
    procedure ParseSubstList(const ASubstListText: string);
    procedure ParseSubstListItem(const ASubstListItemText: string);
  private
    function GetContentType(const ASource: string): string;
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
  VSubstListText: string;
begin
  inherited Create;
  FSource := TStringList.Create;
  FTarget := TStringList.Create;
  VSubstListText := AConfig.ReadString('MimeTypeSubst', '');
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

function TContentTypeSubstByList.GetContentType(const ASource: string): string;
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

procedure TContentTypeSubstByList.ParseSubstList(const ASubstListText: string);
var
  VTempList: TStringList;
  i: Integer;
  VSubstItemText: string;
begin
  VTempList := TStringList.Create;
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
  const ASubstListItemText: string);
var
  VTempList: TStringList;
  VSource: string;
  VTarget: string;
  VTargetIndex: Integer;
begin
  VTempList := TStringList.Create;
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
