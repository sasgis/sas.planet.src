{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_MarkPictureConfigByFolder;

interface

uses
  Classes,
  t_GeoTypes,
  i_PathConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MarkPictureConfig,
  u_GeoFunc,
  u_ConfigDataElementBase;

type
  TMarkPictureConfigByFolder = class(TConfigDataElementBase, IMarkPictureConfig)
  private
    type
      TItemRec = record
        Name: string;
        Anchor: TDoublePoint;
      end;
      PItemRec = ^TItemRec;
  private
    FItemsList: TStringList;
    FItemsListChanged: Boolean;
    FDefaultAnchor: TDoublePoint;
    FMarkPicturePath: string;
    procedure Clear;
    function IsValidAnchor(const AAnchor: TDoublePoint): Boolean; inline;
    function GetConfigDataProvider: IConfigDataProvider;
    function GetConfigDataWriteProvider: IConfigDataWriteProvider;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    { IMarkPictureConfig }
    function GetDefaultAnchor(const APicName: string): TDoublePoint;
    function GetAnchor(const APicName: string): TDoublePoint;
    procedure SetAnchor(const APicName: string; const AAnchor: TDoublePoint);
  public
    constructor Create(const AMarkPicturePath: string);
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  SysUtils,
  IniFiles,
  {$IFNDEF UNICODE}
  Compatibility,
  CompatibilityIniFiles,
  {$ENDIF}
  c_MarkPictureAnchor,
  i_StringListStatic,
  u_MarkPictureAnchorFunc,
  u_ConfigDataProviderByIniFile,
  u_ConfigDataWriteProviderByIniFile;

const
  cMarkPictureIniFileName = 'MarkPicture.ini';

{ TMarkPictureConfigByFolder }

constructor TMarkPictureConfigByFolder.Create(
  const AMarkPicturePath: string
);
begin
  inherited Create;
  FMarkPicturePath := IncludeTrailingPathDelimiter(AMarkPicturePath);
  FDefaultAnchor := cPicAnchorDefault;

  FItemsList := TStringList.Create;
  FItemsList.Sorted := False;
  FItemsList.CaseSensitive := False;
  FItemsList.Duplicates := dupIgnore;

  FItemsListChanged := False;
end;

destructor TMarkPictureConfigByFolder.Destroy;
begin
  Clear;
  FreeAndNil(FItemsList);
  inherited Destroy;
end;

procedure TMarkPictureConfigByFolder.Clear;
var
  I: Integer;
begin
  if Assigned(FItemsList) then begin
    for I := 0 to FItemsList.Count - 1 do begin
      Dispose(Pointer(FItemsList.Objects[I]));
    end;
    FItemsList.Clear;
  end;
end;

function TMarkPictureConfigByFolder.GetConfigDataProvider: IConfigDataProvider;
var
  VIniFile: TMeminifile;
  VIniFileName: string;
begin
  VIniFileName := FMarkPicturePath + cMarkPictureIniFileName;

  if not FileExists(VIniFileName) then begin
    Result := nil;
    Exit;
  end;

  VIniFile := TMeminiFile.Create(VIniFileName);
  try
    VIniFile.Encoding := TEncoding.UTF8;
    Result := TConfigDataProviderByIniFile.CreateWithOwn(VIniFile);
    VIniFile := nil;
  finally
    VIniFile.Free;
  end;
end;

function TMarkPictureConfigByFolder.GetConfigDataWriteProvider: IConfigDataWriteProvider;
var
  VIniFile: TMeminifile;
begin
  VIniFile := TMeminiFile.Create(FMarkPicturePath + cMarkPictureIniFileName);
  try
    VIniFile.Encoding := TEncoding.UTF8;
    Result := TConfigDataWriteProviderByIniFile.CreateWithOwn(VIniFile);
    VIniFile := nil;
  finally
    VIniFile.Free;
  end;
end;

function TMarkPictureConfigByFolder.IsValidAnchor(
  const AAnchor: TDoublePoint
): Boolean;
begin
  Result :=
    not PointIsEmpty(AAnchor) and
    (AAnchor.X >= 0) and (AAnchor.X <= 1) and
    (AAnchor.Y >= 0) and (AAnchor.Y <= 1);
end;

procedure TMarkPictureConfigByFolder.DoReadConfig(
  const AConfigData: IConfigDataProvider
);

  function _ReadAnchor(const AConfig: IConfigDataProvider; var AAnchor: TDoublePoint): Boolean;
  var
    VAnchor: TDoublePoint;
    VAnchorReadable: string;
  begin
    VAnchorReadable := AConfig.ReadString('Anchor', '');
    if VAnchorReadable <> '' then begin
      VAnchor := GetAnchorFromName(VAnchorReadable);
    end else begin
      VAnchor.X := AConfig.ReadFloat('AnchorX', NAN);
      VAnchor.Y := AConfig.ReadFloat('AnchorY', NAN);
    end;
    Result := IsValidAnchor(VAnchor);
    if Result then begin
      AAnchor := VAnchor;
    end;
  end;

var
  I: Integer;
  VCount: Integer;
  VConfig: IConfigDataProvider;
  VSection: IConfigDataProvider;
  VItem: PItemRec;
  VName: string;
  VAnchor: TDoublePoint;
begin
  inherited;
  VConfig := GetConfigDataProvider;
  if VConfig <> nil then begin
    VSection := VConfig.GetSubItem('Main');
    if VSection <> nil then begin
      // read default anchor
      _ReadAnchor(VSection, FDefaultAnchor);

      // read items with custom anchors
      VCount := VSection.ReadInteger('Count', 0);
      for I := 0 to VCount - 1 do begin
        VSection := VConfig.GetSubItem('Pic' + IntToStr(I+1));
        if VSection <> nil then begin
          VName := VSection.ReadString('Name', '');
          if (VName <> '') and _ReadAnchor(VSection, VAnchor) then begin
            if FItemsList.IndexOf(VName) >= 0 then begin
              // ignore duplicated items
              Continue;
            end;
            New(VItem);
            VItem.Name := VName;
            VItem.Anchor := VAnchor;
            FItemsList.AddObject(VItem.Name, TObject(VItem));
          end;
        end;
      end;
    end;
  end;
end;

procedure TMarkPictureConfigByFolder.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
var
  I: Integer;
  VItem: PItemRec;
  VSubItemsList: IStringListStatic;
  VConfig: IConfigDataWriteProvider;
  VSection: IConfigDataWriteProvider;
  VAnchorReadable: string;
begin
  inherited;
  if FItemsListChanged then begin
    VConfig := GetConfigDataWriteProvider;
    if VConfig <> nil then begin
      VSubItemsList := VConfig.ReadSubItemsList;
      // erase all items sections
      if VSubItemsList <> nil then begin
        for I := 0 to VSubItemsList.Count - 1 do begin
          if not SameText(VSubItemsList.Items[I], 'Main')  then begin
            VConfig.DeleteSubItem(VSubItemsList.Items[I]);
          end;
        end;
      end;
      // write items
      VSection := VConfig.GetOrCreateSubItem('Main');
      if VSection <> nil then begin
        VSection.WriteString('Count', IntToStr(FItemsList.Count));
        for I := 0 to FItemsList.Count - 1 do begin
          VItem := PItemRec(FItemsList.Objects[I]);
          VSection := VConfig.GetOrCreateSubItem('Pic' + IntToStr(I+1));
          if VSection <> nil then begin
            VSection.WriteString('Name', VItem.Name);
            if IsKnownAnchor(VItem.Anchor, VAnchorReadable) then begin
              VSection.WriteString('Anchor', VAnchorReadable);
            end else begin
              VSection.WriteFloat('AnchorX', VItem.Anchor.X);
              VSection.WriteFloat('AnchorY', VItem.Anchor.Y);
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TMarkPictureConfigByFolder.GetDefaultAnchor(
  const APicName: string
): TDoublePoint;
begin
  Result := FDefaultAnchor;
end;

function TMarkPictureConfigByFolder.GetAnchor(
  const APicName: string
): TDoublePoint;
var
  I: Integer;
  VName: string;
begin
  LockRead;
  try
    VName := ExtractFileName(APicName);
    Assert(VName <> '');
    I := FItemsList.IndexOf(VName);
    if I >= 0 then begin
      Result := PItemRec(FItemsList.Objects[I]).Anchor;
    end else begin
      Result := FDefaultAnchor;
    end;
  finally
    UnlockRead;
  end;
end;

procedure TMarkPictureConfigByFolder.SetAnchor(
  const APicName: string;
  const AAnchor: TDoublePoint
);
var
  I: Integer;
  VName: string;
  VItem: PItemRec;
begin
  if not IsValidAnchor(AAnchor) then begin
    raise Exception.CreateFmt(
      'Invalid anchor value [%.2f, %.2f]',
      [AAnchor.X, AAnchor.Y]
    );
  end;
  LockWrite;
  try
    VName := ExtractFileName(APicName);
    Assert(VName <> '');
    I := FItemsList.IndexOf(VName);
    if I >= 0 then begin
      VItem := PItemRec(FItemsList.Objects[I]);
      if not DoublePointsEqual(VItem.Anchor, AAnchor) then begin
        VItem.Anchor := AAnchor;
        FItemsListChanged := True;
        SetChanged;
      end;
    end else begin
      New(VItem);
      VItem.Name := VName;
      VItem.Anchor := AAnchor;
      FItemsList.AddObject(VItem.Name, TObject(VItem));
      FItemsListChanged := True;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
