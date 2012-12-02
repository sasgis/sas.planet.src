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

unit u_MapAttachmentsInfo;

interface

uses
  Classes,
  SysUtils,
  i_MapAttachmentsInfo,
  u_BaseInterfacedObject;

type
  TMapAttachmentsInfo = class(TBaseInterfacedObject, IMapAttachmentsInfo)
  private
    FMaxSubIndex: Integer;
    FParseNumberAfter: String;
    FGUID: TGUID;
    FSL_NameInCache: TStringList;
    FSL_Ext: TStringList;
    FSL_DefUrlBase: TStringList;
    FSL_ContentType: TStringList;
    FSL_Names: TStringList;
    FUseDwn, FUseDel: Boolean;
  protected
    { IMapAttachmentsInfo }
    function GetGUID: TGUID; stdcall;
    function GetMaxSubIndex: Integer; stdcall;
    function GetParseNumberAfter: String; stdcall;
    function GetNameInCache(const AIndex: Integer): String; stdcall;
    function GetExt(const AIndex: Integer): String; stdcall;
    function GetEnabled(const AIndex: Integer): Boolean; stdcall;
    function GetDefURLBase(const AIndex: Integer): String; stdcall;
    function GetContentType(const AIndex: Integer): String; stdcall;
    function GetUseDwn: Boolean; stdcall;
    function GetUseDel: Boolean; stdcall;
    { IStringByLanguage }
    function GetString(ALangIndex: Integer): string;
    function GetDefault: string;
  public
    constructor Create(
      const AGUID: TGUID;
      AMaxSubIndex: Integer;
      const AParseNumberAfter: String;
      ASL_NameInCache: TStringList;
      ASL_Ext: TStringList;
      ASL_Names: TStringList;
      ASL_DefUrlBase: TStringList;
      ASL_ContentType: TStringList;
      AUseDwn, AUseDel: Boolean
    );
    destructor Destroy; override;
  end;

implementation

{ TMapAttachmentsInfo }

constructor TMapAttachmentsInfo.Create(
  const AGUID: TGUID;
  AMaxSubIndex: Integer;
  const AParseNumberAfter: String;
  ASL_NameInCache: TStringList;
  ASL_Ext: TStringList;
  ASL_Names: TStringList;
  ASL_DefUrlBase: TStringList;
  ASL_ContentType: TStringList;
  AUseDwn, AUseDel: Boolean
);
begin
  inherited Create;
  FMaxSubIndex := AMaxSubIndex;
  FParseNumberAfter := AParseNumberAfter;
  FGUID := AGUID;
  FSL_NameInCache := ASL_NameInCache;
  FSL_Ext := ASL_Ext;
  FSL_Names := ASL_Names;
  FSL_DefUrlBase := ASL_DefUrlBase;
  FSL_ContentType := ASL_ContentType;
  FUseDwn := AUseDwn;
  FUseDel := AUseDel;
end;

destructor TMapAttachmentsInfo.Destroy;
begin
  FreeAndNil(FSL_NameInCache);
  FreeAndNil(FSL_Ext);
  FreeAndNil(FSL_Names);
  FreeAndNil(FSL_DefUrlBase);
  FreeAndNil(FSL_ContentType);
  inherited;
end;

function TMapAttachmentsInfo.GetContentType(const AIndex: Integer): String;
begin
  if (FSL_ContentType <> nil) and (AIndex >= 0) and (AIndex < FSL_ContentType.Count) then begin
    Result := FSL_ContentType[AIndex];
  end else begin
    Result := '';
  end;
end;

function TMapAttachmentsInfo.GetDefault: string;
begin
  if FSL_Names = nil then begin
    Result := '';
  end else begin
    Result := FSL_Names[0];
  end;
end;

function TMapAttachmentsInfo.GetDefURLBase(const AIndex: Integer): String;
begin
  if (FSL_DefUrlBase <> nil) and (AIndex >= 0) and (AIndex < FSL_DefUrlBase.Count) then begin
    Result := FSL_DefUrlBase[AIndex];
  end else begin
    Result := '';
  end;
end;

function TMapAttachmentsInfo.GetEnabled(const AIndex: Integer): Boolean;
begin
  if (FSL_NameInCache <> nil) and (AIndex >= 0) and (AIndex < FSL_NameInCache.Count) then begin
    Result := (FSL_NameInCache.Objects[AIndex] <> nil);
  end else begin
    Result := FALSE;
  end;
end;

function TMapAttachmentsInfo.GetExt(const AIndex: Integer): String;
begin
  if (FSL_Ext <> nil) and (AIndex >= 0) and (AIndex < FSL_Ext.Count) then begin
    Result := FSL_Ext[AIndex];
  end else begin
    Result := '';
  end;
end;

function TMapAttachmentsInfo.GetGUID: TGUID;
begin
  Result := FGUID;
end;

function TMapAttachmentsInfo.GetMaxSubIndex: Integer;
begin
  Result := FMaxSubIndex;
end;

function TMapAttachmentsInfo.GetNameInCache(const AIndex: Integer): String;
begin
  if (FSL_NameInCache <> nil) and (AIndex >= 0) and (AIndex < FSL_NameInCache.Count) then begin
    Result := FSL_NameInCache[AIndex];
  end else begin
    Result := '';
  end;
end;

function TMapAttachmentsInfo.GetParseNumberAfter: String;
begin
  Result := FParseNumberAfter;
end;

function TMapAttachmentsInfo.GetString(ALangIndex: Integer): string;
begin
  if (FSL_Names = nil) then begin
    Result := '';
  end else if (ALangIndex <= 0) or (ALangIndex >= FSL_Names.Count) then begin
    Result := GetDefault;
  end else begin
    Result := FSL_Names[ALangIndex];
  end;
end;

function TMapAttachmentsInfo.GetUseDel: Boolean;
begin
  Result := FUseDel;
end;

function TMapAttachmentsInfo.GetUseDwn: Boolean;
begin
  Result := FUseDwn;
end;

end.
