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

unit u_ImportMpSimple;

interface

uses
  Classes,
  t_GeoTypes,
  i_VectorItemsFactory,
  i_DoublePointsAggregator,
  i_ImportFile,
  i_ImportConfig,
  i_MarksSystem,
  u_BaseInterfacedObject;

type
  TImportMpSimple = class(TBaseInterfacedObject, IImportFile)
  private
    FFactory: IVectorItemsFactory;
    procedure ParseCoordinates(
      const AData: string;
      const APointsAggregator: IDoublePointsAggregator
    );
  private
    function ProcessImport(
      const AMarksSystem: IMarksSystem;
      const AFileName: string;
      const AConfig: IImportConfig
    ): IInterfaceList;
  public
    constructor Create(
      const AFactory: IVectorItemsFactory
    );
  end;

implementation

uses
  SysUtils,
  StrUtils,
  i_MarksSimple,
  u_DoublePointsAggregator,
  u_GeoFun;

const
  CPoligonHeader = '[POLYGON]';
  CDataHeader = 'Data0=';

{ TImportMpSimple }

constructor TImportMpSimple.Create(const AFactory: IVectorItemsFactory);
begin
  inherited Create;
  FFactory := AFactory;
end;

procedure TImportMpSimple.ParseCoordinates(
  const AData: string;
  const APointsAggregator: IDoublePointsAggregator
);
var
  VCoordList: TStringList;
  VString: string;
  i: Integer;
  VPos: Integer;
  VXStr: string;
  VYStr: string;
  VPoint: TDoublePoint;
  VFormatSettings: TFormatSettings;
begin
  VCoordList := TStringList.Create;
  try
    VFormatSettings.DecimalSeparator := '.';
    VCoordList.Delimiter := '(';
    VCoordList.DelimitedText := AData;
    for i := 0 to VCoordList.Count - 1 do begin
      VString := VCoordList[i];
      if VString <> '' then begin
        VPoint := CEmptyDoublePoint;
        VPos := Pos(',', VString);
        if VPos > 0 then begin
          VYStr := LeftStr(VString, VPos - 1);
          VXStr := MidStr(VString, VPos + 1, Length(VString));
          VPos := Pos(')', VXStr);
          if VPos > 0 then begin
            VXStr := LeftStr(VXStr, VPos - 1);
          end;
          VPoint.X := StrToFloatDef(VXStr, VPoint.X, VFormatSettings);
          VPoint.Y := StrToFloatDef(VYStr, VPoint.Y, VFormatSettings);
        end;
        if not PointIsEmpty(VPoint) then begin
          APointsAggregator.Add(VPoint);
        end;
      end;
    end;
  finally
    VCoordList.Free;
  end;
end;

function TImportMpSimple.ProcessImport(
  const AMarksSystem: IMarksSystem;
  const AFileName: string;
  const AConfig: IImportConfig
): IInterfaceList;
var
  VFile: TStringList;
  i: integer;
  VPointsAggregator: IDoublePointsAggregator;
  VMark: IMark;
  VString: string;
  VPoligonLine: Integer;
  VDataLine: Integer;
begin
  Result := nil;
  if AConfig.TemplateNewPoly <> nil then begin
    VPointsAggregator := TDoublePointsAggregator.Create;
    VFile := TStringList.Create;
    try
      VFile.LoadFromFile(AFileName);
      VPoligonLine := -1;
      for i := 0 to VFile.Count - 1 do begin
        VString := VFile[i];
        if LeftStr(VString, Length(CPoligonHeader)) = CPoligonHeader then begin
          VPoligonLine := i;
          Break;
        end;
      end;
      if VPoligonLine >= 0 then begin
        VDataLine := -1;
        for i := VPoligonLine + 1 to VFile.Count - 1 do begin
          VString := VFile[i];
          if LeftStr(VString, Length(CDataHeader)) = CDataHeader then begin
            VDataLine := i;
            Break;
          end;
        end;
        if VDataLine >= 0 then begin
          VString := MidStr(VString, Length(CDataHeader) + 1, Length(VString));
          if VString <> '' then begin
            ParseCoordinates(VString, VPointsAggregator);
          end;
        end;
      end;
    finally
      FreeAndNil(VFile);
    end;
    if VPointsAggregator.Count > 2 then begin
      VMark := AMarksSystem.MarksDb.Factory.CreateNewPoly(
        FFactory.CreateLonLatPolygon(VPointsAggregator.Points, VPointsAggregator.Count),
        ExtractFileName(AFileName),
        '',
        AConfig.TemplateNewPoly
      );
      if VMark <> nil then begin
        VMark := AMarksSystem.MarksDb.UpdateMark(nil, VMark);
        if VMark <> nil then begin
          Result := TInterfaceList.Create;
          Result.Add(VMark);
        end;
      end;
    end;
  end;
end;

end.
