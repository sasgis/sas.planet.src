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

unit u_ImageResamplerFactoryListStatic;

interface

uses
  SyncObjs,
  Classes,
  i_ImageResamplerFactory;

type
  TImageResamplerFactoryListStatic = class(TInterfacedObject, IImageResamplerFactoryList)
  private
    FList: TStringList;
    FCS: TCriticalSection;
  protected
    procedure Add(AFactory: IImageResamplerFactory; ACaption: string);
  protected
    function Count: Integer;
    function Get(AIndex: Integer): IImageResamplerFactory;
    function GetCaption(AIndex: Integer): string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TImageResamplerFactoryListStatic }

constructor TImageResamplerFactoryListStatic.Create;
begin
  inherited;
  FCS := TCriticalSection.Create;
  FList := TStringList.Create;
end;

destructor TImageResamplerFactoryListStatic.Destroy;
var
  i: Integer;
begin
  if FList <> nil then begin
    for i := 0 to FList.Count - 1 do begin
      if FList.Objects[i] <> nil then begin
        IInterface(Pointer(FList.Objects[i]))._Release;
        FList.Objects[i] := nil;
      end;
    end;
  end;
  FreeAndNil(FList);
  FreeAndNil(FCS);
  inherited;
end;

procedure TImageResamplerFactoryListStatic.Add(AFactory: IImageResamplerFactory;
  ACaption: string);
begin
  FCS.Acquire;
  try
    FList.AddObject(ACaption, TObject(Pointer(AFactory)));
    AFactory._AddRef;
  finally
    FCS.Release;
  end;
end;

function TImageResamplerFactoryListStatic.Count: Integer;
begin
  FCS.Acquire;
  try
    Result := FList.Count;
  finally
    FCS.Release;
  end;
end;

function TImageResamplerFactoryListStatic.Get(
  AIndex: Integer): IImageResamplerFactory;
begin
  FCS.Acquire;
  try
    Result := IImageResamplerFactory(Pointer(FList.Objects[AIndex]));
  finally
    FCS.Release;
  end;
end;

function TImageResamplerFactoryListStatic.GetCaption(AIndex: Integer): string;
begin
  FCS.Acquire;
  try
    Result := FList.Strings[AIndex];
  finally
    FCS.Release;
  end;
end;

end.
