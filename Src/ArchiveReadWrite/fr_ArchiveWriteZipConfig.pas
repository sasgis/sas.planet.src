{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit fr_ArchiveWriteZipConfig;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  StdCtrls,
  i_LanguageManager,
  i_ArchiveReadWriteConfig,
  u_CommonFormAndFrameParents;

type
  TfrArchiveWriteZipConfig = class(TFrame, IArchiveWriteConfigFrame)
    cbbCompressLevel: TComboBox;
    lblCompressLevel: TLabel;
    cbbCompressMethod: TComboBox;
    lblCompressMethod: TLabel;
    lblSplitToVolumes: TLabel;
    cbbVolumeSize: TComboBox;
    procedure cbbCompressLevelChange(Sender: TObject);
  private
    { IArchiveWriteConfigFrame }
    function GetWriteConfig: IArchiveWriteConfig;
    procedure Reset(const AWriteConfig: IArchiveWriteConfig);
  protected
    procedure OnShow(const AIsFirstTime: Boolean); override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager
    ); reintroduce;
  end;

implementation

uses
  gnugettext,
  u_ArchiveReadWriteConfig,
  u_Dialogs;

{$R *.dfm}

const
  cMegabyte: Int64 = 1024 * 1024;
  cCompressMethodStr: array [0..2] of string = ('Deflate', 'BZip2', 'LZMA');
  cVolumeSizeStr: array [0..6] of string = (
    '100M', '200M', '650M - CD', '700M - CD', '1000M', '4092M - FAT', '4480M - DVD'
  );
  cVolumeSizeVal: array [0..6] of Integer = (100, 200, 650, 700, 1000, 4092, 4480);

{ TfrArchiveWriteZipConfig }

constructor TfrArchiveWriteZipConfig.Create(
  const ALanguageManager: ILanguageManager
);
var
  I: Integer;
begin
  inherited Create(ALanguageManager);

  cbbCompressLevel.Items.Add( _('Store') );
  cbbCompressLevel.Items.Add( _('Fast') );
  cbbCompressLevel.Items.Add( _('Normal') );
  cbbCompressLevel.Items.Add( _('Best') );

  for I := 0 to Length(cCompressMethodStr) - 1 do begin
    cbbCompressMethod.Items.Add(cCompressMethodStr[I]);
  end;

  for I := 0 to Length(cVolumeSizeStr) - 1 do begin
    cbbVolumeSize.Items.Add(cVolumeSizeStr[I]);
  end;

  FPropertyState := CreateComponentPropertyState(
    Self, [], [], True, False, True, True
  );

  FPropertyState.Include('cbbVolumeSize', ['Text']);
end;

procedure TfrArchiveWriteZipConfig.OnShow(const AIsFirstTime: Boolean);
begin
  inherited;
  if AIsFirstTime then begin
    if cbbCompressLevel.ItemIndex < 0 then begin
      cbbCompressLevel.ItemIndex := 0; // Store
    end;
    cbbCompressLevelChange(Self);
  end;
end;

procedure TfrArchiveWriteZipConfig.cbbCompressLevelChange(Sender: TObject);
begin
  cbbCompressMethod.Enabled := cbbCompressLevel.ItemIndex > 0;
  if cbbCompressMethod.Enabled then begin
    if cbbCompressMethod.ItemIndex < 0 then begin
      cbbCompressMethod.ItemIndex := 0; // Deflate
    end;
  end else begin
    cbbCompressMethod.ItemIndex := -1;
  end;
end;

function TfrArchiveWriteZipConfig.GetWriteConfig: IArchiveWriteConfig;
var
  I: Integer;
  VStr: string;
  VLevel: TZipCompressionLevel;
  VMethod: TZipCompressionMethod;
begin
  Result := nil;

  I := 0;

  VStr := Trim(cbbVolumeSize.Text);

  if VStr <> '' then begin
    I := Pos('M', VStr);
    if I > 0 then begin
      VStr := Copy(VStr, 1, I - 1);
    end;
    if not TryStrToInt(VStr, I) then begin
      ShowErrorMessage(Format(_('Invalid Volume Size value: %s'), [cbbVolumeSize.Text]));
      Exit;
    end;
  end;

  if cbbCompressLevel.ItemIndex = 0 then begin
    VLevel := zclFast; // not used
    VMethod := zcmStore;
  end else begin
    VLevel := TZipCompressionLevel(cbbCompressLevel.ItemIndex - 1);
    VMethod := TZipCompressionMethod(cbbCompressMethod.ItemIndex + 1);
  end;

  Result :=
    TArchiveWriteZipConfig.Create(
      VLevel,
      VMethod,
      I * cMegabyte
    ) as IArchiveWriteZipConfig;
end;

procedure TfrArchiveWriteZipConfig.Reset(
  const AWriteConfig: IArchiveWriteConfig
);
var
  I: Integer;
  VSize: Int64;
  VConfig: IArchiveWriteZipConfig;
begin
  cbbVolumeSize.Text := '';
  if AWriteConfig = nil then begin
    cbbCompressLevel.ItemIndex := 0;
    cbbCompressLevelChange(Self);
  end else
  if Supports(AWriteConfig, IArchiveWriteZipConfig, VConfig) then begin
    Assert(Integer(zcmStore) = 0);
    if VConfig.CompressionMethod = zcmStore then begin
      cbbCompressLevel.ItemIndex := 0;
    end else begin
      cbbCompressMethod.ItemIndex := Integer(VConfig.CompressionMethod) - 1;
      cbbCompressLevel.ItemIndex := Integer(VConfig.CompressionLevel) + 1;
    end;
    cbbCompressLevelChange(Self);

    VSize := VConfig.VolumeSize;
    if VSize > 0 then begin
      for I := 0 to Length(cVolumeSizeVal) - 1 do begin
        if VSize = cVolumeSizeVal[I] * cMegabyte then begin
          cbbVolumeSize.Text := cVolumeSizeStr[I];
          Break;
        end;
      end;
      if cbbVolumeSize.Text = '' then begin
        cbbVolumeSize.Text := IntToStr(VSize div cMegabyte);
      end;
    end;
  end else begin
    raise Exception.Create('Unsupported interface type!');
  end;
end;

end.
