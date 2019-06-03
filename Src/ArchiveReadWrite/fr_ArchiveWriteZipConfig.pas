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
  Dialogs,
  StdCtrls,
  UITypes,
  TBX,
  TBXExtItems,
  TB2Item,
  TB2ExtItems,
  TB2Dock,
  TB2Toolbar,
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
    tbxToolbar: TTBXToolbar;
    tbxcbbVolumeSize: TTBXComboBoxItem;
    procedure cbbCompressLevelChange(Sender: TObject);
  private
    { IArchiveWriteConfigFrame }
    function GetWriteConfig: IArchiveWriteConfig;
    procedure Reset(const AWriteConfig: IArchiveWriteConfig);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager
    ); reintroduce;
  end;

implementation

uses
  gnugettext,
  u_ArchiveReadWriteConfig;

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

  cbbCompressLevel.ItemIndex := 0; // Store

  for I := 0 to Length(cCompressMethodStr) - 1 do begin
    cbbCompressMethod.Items.Add(cCompressMethodStr[I]);
  end;

  cbbCompressLevelChange(Self);

  for I := 0 to Length(cVolumeSizeStr) - 1 do begin
    tbxcbbVolumeSize.Lines.Add(cVolumeSizeStr[I]);
  end;
end;

procedure TfrArchiveWriteZipConfig.cbbCompressLevelChange(Sender: TObject);
begin
  cbbCompressMethod.Enabled := cbbCompressLevel.ItemIndex > 0;
  if cbbCompressMethod.Enabled then begin
    cbbCompressMethod.ItemIndex := 0;
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

  VStr := Trim(tbxcbbVolumeSize.Text);

  if VStr <> '' then begin
    I := Pos('M', VStr);
    if I > 0 then begin
      VStr := Copy(VStr, 1, I - 1);
    end;
    if not TryStrToInt(VStr, I) then begin
      MessageDlg(
        Format(_('Invalid Volume Size value: %s'), [tbxcbbVolumeSize.Text]),
        mtError,
        [mbOk],
        -1
      );
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
  if AWriteConfig = nil then begin
    cbbCompressLevel.ItemIndex := 0;
    cbbCompressLevelChange(Self);
    tbxcbbVolumeSize.Text := '';
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

    tbxcbbVolumeSize.Text := '';
    VSize := VConfig.VolumeSize;
    if VSize > 0 then begin
      for I := 0 to Length(cVolumeSizeVal) - 1 do begin
        if VSize = cVolumeSizeVal[I] * cMegabyte then begin
          tbxcbbVolumeSize.Text := cVolumeSizeStr[I];
          Break;
        end;
      end;
      if tbxcbbVolumeSize.Text = '' then begin
        tbxcbbVolumeSize.Text := IntToStr(VSize div cMegabyte);
      end;
    end;
  end else begin
    raise Exception.Create('Unsupported interface type!');
  end;
end;

end.
