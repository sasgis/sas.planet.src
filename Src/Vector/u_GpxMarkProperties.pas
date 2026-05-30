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

unit u_GpxMarkProperties;

interface

uses
  i_VectorDataItemSimple;

type
  TGpxMarkProperties = record
  private
    FDesc: string;
    FCmt: string;
    FTime: TDateTime;
    FType: string;
    FSym: string;
    FNotes: string;

    function ExtractDesc(const ADesc: string): string;
    function ExtractCmt(var ADesc: string): string;
    function ExtractTime(var ADesc: string; const AName: string): TDateTime;
    function ExtractType(var ADesc: string): string;
    function ExtractSym(var ADesc: string): string;
  public
    class function Read(const AMark: IVectorDataItem): TGpxMarkProperties; static;
  public
    property Desc: string read FDesc;
    property Cmt: string read FCmt;
    property Time: TDateTime read FTime;
    property TypeId: string read FType;
    property Sym: string read FSym;
    property Notes: string read FNotes;
  end;

implementation

uses
  SysUtils;

{ TGpxMarkProperties }

class function TGpxMarkProperties.Read(const AMark: IVectorDataItem): TGpxMarkProperties;
begin
  with Result do begin
    // Order of extraction is important
    FDesc := ExtractDesc(AMark.Desc);
    FCmt  := ExtractCmt(FDesc);
    FTime := ExtractTime(FDesc, AMark.Name);
    FType := ExtractType(FDesc);
    FSym  := ExtractSym(FDesc);

    // Google Earth ignore "desc"? And shows "cmt" only
    if (FCmt = '') and (FDesc <> '') then begin
      FCmt := FDesc;
      FDesc := '';
    end;

    FNotes := Trim(FDesc + sLineBreak + FCmt);
  end;
end;

function TGpxMarkProperties.ExtractDesc(const ADesc: string): string;

  procedure RemoveField(var AStr: string; const AFieldName: string);
  var
    X: Integer;
    Prefix: string;
    Pre: string;
  begin
    Prefix := AFieldName + ': ';
    X := Pos(Prefix, AStr);
    if X > 0 then begin
      Pre := Trim(Copy(AStr, 1, X - 1));
      AStr := Trim(Copy(AStr, X + Length(Prefix), MaxInt));
      X := Pos(#10, AStr);
      if X > 0 then
        AStr := Trim(Copy(AStr, X + 1, MaxInt));
      AStr := Trim(Pre + sLineBreak + AStr);
    end;
  end;

begin
  Result := Trim(AdjustLineBreaks(ADesc));

  // Remove BR-s
  Result := StringReplace(Result, '<br>' + sLineBreak,  sLineBreak,  [rfReplaceAll]);
  Result := StringReplace(Result, '<br />' + sLineBreak,  sLineBreak,  [rfReplaceAll]);
  Result := StringReplace(Result, '<br/>' + sLineBreak,  sLineBreak,  [rfReplaceAll]);
  Result := StringReplace(Result, '<br>',  '',  [rfReplaceAll]);
  Result := StringReplace(Result, '<br />',  '',  [rfReplaceAll]);
  Result := StringReplace(Result, '<br/>',  '',  [rfReplaceAll]);

  RemoveField(Result, 'number');
  RemoveField(Result, 'type');
  RemoveField(Result, 'kind');
  RemoveField(Result, 'GPS Coordinates');
end;

function TGpxMarkProperties.ExtractCmt(var ADesc: string): string;
var
  X: Integer;
  Pre: String;
begin
  // Extract "cmt:" field
  X := Pos('cmt: ', ADesc);
  if X > 0 then begin
    Pre := Trim(Copy(ADesc, 1, X - 1));
    ADesc := Trim(Copy(ADesc, X + Length('cmt: '), MaxInt));
    X := Pos(#10, ADesc);
    if X > 0 then begin
      Result := Trim(Copy(ADesc, 1, X - 1));
      ADesc := Trim(Copy(ADesc, X + 1, MaxInt));
      ADesc := Trim(Pre + sLineBreak + ADesc);
    end else begin
      Result := ADesc;
      ADesc := Pre;
    end;
  end else
    Result := '';
end;

function TGpxMarkProperties.ExtractTime(var ADesc: string; const AName: string): TDateTime;
var
  X: Integer;
  VPre: string;
  VDesc: string;
begin
  Result := 0;
  if TryStrToDateTime(ADesc, Result) then begin
    ADesc := '';
    Exit;
  end;
  VDesc := LowerCase(ADesc);

  // Extract "time:" field
  X := Pos('time: ', VDesc);
  if X > 0 then begin
    VPre := Trim(Copy(ADesc, 1, X - 1));
    ADesc := Trim(Copy(ADesc, X + Length('time: '), MaxInt));
    X := Pos(#10, ADesc);
    if X > 0 then begin
      if not TryStrToDateTime(Trim(Copy(ADesc, 1, X - 1)), Result) then Result := 0;
      ADesc := Trim(Copy(ADesc, X + 1, MaxInt));
      ADesc := Trim(VPre + sLineBreak + ADesc);
    end else begin
      if not TryStrToDateTime(ADesc, Result) then Result := 0;
      ADesc := VPre;
    end;
  end;

  if Result <> 0 then
    Exit;

  // Extract "DateTime:" field
  X := Pos('datetime: ', VDesc);
  if X > 0 then begin
    VPre := Trim(Copy(ADesc, 1, X - 1));
    ADesc := Trim(Copy(ADesc, X + Length('time: '), MaxInt));
    X := Pos(#10, ADesc);
    if X > 0 then begin
      if not TryStrToDateTime(Trim(Copy(ADesc, 1, X - 1)), Result) then Result := 0;
      ADesc := Trim(Copy(ADesc, X + 1, MaxInt));
      ADesc := Trim(VPre + sLineBreak + ADesc);
    end else begin
      if not TryStrToDateTime(ADesc, Result) then Result := 0;
      ADesc := VPre;
    end;
  end;

  if Result <> 0 then
    Exit;

  // Extract "Date:" field
  X := Pos('date: ', VDesc);
  if X > 0 then begin
    VPre := Trim(Copy(ADesc, 1, X - 1));
    ADesc := Trim(Copy(ADesc, X + Length('Date: '), MaxInt));
    X := Pos(#10, ADesc);
    if X > 0 then begin
      if not TryStrToDateTime(Trim(Copy(ADesc, 1, X - 1)), Result) then Result := 0;
      ADesc := Trim(Copy(ADesc, X + 1, MaxInt));
      ADesc := Trim(VPre + sLineBreak + ADesc);
    end else begin
      if not TryStrToDateTime(ADesc, Result) then Result := 0;
      ADesc := VPre;
    end;
  end;

  if Result = 0 then
    Result := StrToDateTimeDef(AName, 0);
end;

function TGpxMarkProperties.ExtractType(var ADesc: String): String;
var
  X: Integer;
  Pre: String;
begin
  // Extract "type:" field
  X := Pos('type: ', ADesc);
  if X > 0 then begin
    Pre := Trim(Copy(ADesc, 1, X - 1));
    ADesc := Trim(Copy(ADesc, X + Length('type: '), MaxInt));
    X := Pos(#10, ADesc);
    if X > 0 then begin
      Result := Trim(Copy(ADesc, 1, X - 1));
      ADesc := Trim(Copy(ADesc, X + 1, MaxInt));
      ADesc := Trim(Pre + sLineBreak + ADesc);
    end else begin
      Result := ADesc;
      ADesc := Pre;
    end;
  end else
    Result := '';
end;

function TGpxMarkProperties.ExtractSym(var ADesc: String): String;
var
  X: Integer;
  Pre: String;
begin
  // Extract "sym:" field
  X := Pos('sym: ', ADesc);
  if X > 0 then begin
    Pre := Trim(Copy(ADesc, 1, X - 1));
    ADesc := Trim(Copy(ADesc, X + Length('sym: '), MaxInt));
    X := Pos(#10, ADesc);
    if X > 0 then begin
      Result := Trim(Copy(ADesc, 1, X - 1));
      ADesc := Trim(Copy(ADesc, X + 1, MaxInt));
      ADesc := Trim(Pre + sLineBreak + ADesc);
    end else begin
      Result := ADesc;
      ADesc := Pre;
    end;
  end else
    Result := '';
end;

end.
