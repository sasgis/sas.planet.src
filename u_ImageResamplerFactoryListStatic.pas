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

unit u_ImageResamplerFactoryListStatic;

interface

uses
  SysUtils,
  Classes,
  i_ImageResamplerFactory,
  u_BaseInterfacedObject;

type
  TImageResamplerFactoryListStatic = class(TBaseInterfacedObject, IImageResamplerFactoryList)
  private
    FList: TStringList;
    FCS: IReadWriteSync;
  protected
    procedure Add(
      const AFactory: IImageResamplerFactory;
      const ACaption: string
    );
  private
    function Count: Integer;
    function Get(AIndex: Integer): IImageResamplerFactory;
    function GetCaption(AIndex: Integer): string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer;

{ TImageResamplerFactoryListStatic }

constructor TImageResamplerFactoryListStatic.Create;
begin
  inherited;
  FCS := MakeSyncRW_Std(Self, TRUE);
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
  FCS := nil;
  inherited;
end;

procedure TImageResamplerFactoryListStatic.Add(
  const AFactory: IImageResamplerFactory;
  const ACaption: string
);
begin
  FCS.BeginWrite;
  try
    FList.AddObject(ACaption, TObject(Pointer(AFactory)));
    AFactory._AddRef;
  finally
    FCS.EndWrite;
  end;
end;

function TImageResamplerFactoryListStatic.Count: Integer;
begin
  FCS.BeginRead;
  try
    Result := FList.Count;
  finally
    FCS.EndRead;
  end;
end;

function TImageResamplerFactoryListStatic.Get(
  AIndex: Integer): IImageResamplerFactory;
begin
  FCS.BeginRead;
  try
    Result := IImageResamplerFactory(Pointer(FList.Objects[AIndex]));
  finally
    FCS.EndRead;
  end;
end;

function TImageResamplerFactoryListStatic.GetCaption(AIndex: Integer): string;
begin
  FCS.BeginRead;
  try
    Result := FList.Strings[AIndex];
  finally
    FCS.EndRead;
  end;
end;

end.
