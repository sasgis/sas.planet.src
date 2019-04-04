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

unit u_InternalDomainPhotoUrlHandler;

interface

uses
  i_PathConfig,
  i_InternalDomainUrlHandler,
  u_BaseInterfacedObject;

type
  TInternalDomainPhotoUrlHandler = class(TBaseInterfacedObject, IInternalDomainUrlHandler)
  private
    FMediaDataPath: IPathConfig;
  private
    function Process(const AUrl: string): Boolean;
  public
    constructor Create(const AMediaDataPath: IPathConfig);
  end;

implementation

uses
  SysUtils,
  c_InternalBrowser,
  u_InetFunc;

{ TInternalDomainPhotoUrlHandler }

constructor TInternalDomainPhotoUrlHandler.Create(
  const AMediaDataPath: IPathConfig
);
begin
  Assert(AMediaDataPath <> nil);

  inherited Create;
  FMediaDataPath := AMediaDataPath;
end;

function TInternalDomainPhotoUrlHandler.Process(const AUrl: string): Boolean;
var
  I: Integer;
  VFileName: string;
begin
  I := Pos(CPhotoInternalUrl, AUrl);

  Result := I > 0;
  if not Result then begin
    Exit;
  end;

  VFileName := Trim(Copy(AUrl, I + Length(CPhotoInternalUrl)));
  if VFileName = '' then begin
    raise Exception.CreateFmt('Url "%s" process error. File name is empty!', [AUrl]);
  end;

  I := Pos(CMediaDataInternalURL, VFileName);
  if I > 0 then begin
    VFileName := FMediaDataPath.FullPath + Copy(VFileName, I + Length(CMediaDataInternalURL));
  end;

  if FileExists(VFileName) then begin
    // ToDo: add option to open photo in a user program
    OpenFileInDefaultProgram(VFileName);
  end else begin
    raise Exception.CreateFmt('Url "%s" process error. File not exists: %s', [AUrl, VFileName]);
  end;
end;

end.
