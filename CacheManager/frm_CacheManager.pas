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

unit frm_CacheManager;

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
  ExtCtrls,
  ComCtrls,
  i_Notifier, 
  i_NotifierOperation,
  i_LanguageManager,
  i_NotifierTTLCheck,
  i_ContentTypeManager,
  i_InternalPerformanceCounter,
  i_ValueToStringConverter,
  u_CommonFormAndFrameParents;

type
  TfrmCacheManager = class(TFormWitghLanguageManager)
    PageControl1: TPageControl;
    tsConverter: TTabSheet;
    pnlBottomButtons: TPanel;
    btnStart: TButton;
    btnCansel: TButton;
    chkCloseWithStart: TCheckBox;
    grpSrc: TGroupBox;
    lblPath: TLabel;
    edtPath: TEdit;
    cbbCacheTypes: TComboBox;
    lblCacheType: TLabel;
    chkIgnoreTNE: TCheckBox;
    chkRemove: TCheckBox;
    edtDefExtention: TEdit;
    lblDefExtention: TLabel;
    grpDestCache: TGroupBox;
    lblDestPath: TLabel;
    lblDestFormat: TLabel;
    edtDestPath: TEdit;
    cbbDestCacheTypes: TComboBox;
    chkOverwrite: TCheckBox;
    btnSelectSrcPath: TButton;
    btnSelectDestPath: TButton;
    procedure btnStartClick(Sender: TObject);
    procedure btnSelectSrcPathClick(Sender: TObject);
    procedure btnSelectDestPathClick(Sender: TObject);
    procedure btnCanselClick(Sender: TObject);
  private
    FLanguageManager: ILanguageManager;
    FAppClosingNotifier: INotifierOneOperation;
    FTimerNoifier: INotifier;
    FGCList: INotifierTTLCheck;
    FContentTypeManager: IContentTypeManager;
    FPerfCounterList: IInternalPerformanceCounterList;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    procedure ProcessCacheConverter;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifier;
      const AGCList: INotifierTTLCheck;
      const AContentTypeManager: IContentTypeManager;
      const APerfCounterList: IInternalPerformanceCounterList;
      const AValueToStringConverterConfig: IValueToStringConverterConfig
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  c_CacheTypeCodes,
  frm_ProgressCacheConvrter,   
  i_CacheConverterProgressInfo,
  u_NotifierOperation,
  u_GlobalCahceConfig,
  u_ThreadCacheConverter,
  u_CacheConverterProgressInfo;

{$R *.dfm}

{TfrmCacheManager}

constructor TfrmCacheManager.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifier;
  const AGCList: INotifierTTLCheck;
  const AContentTypeManager: IContentTypeManager;
  const APerfCounterList: IInternalPerformanceCounterList;
  const AValueToStringConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(ALanguageManager);
  FLanguageManager := ALanguageManager;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
  FGCList := AGCList;
  FContentTypeManager := AContentTypeManager;
  FPerfCounterList := APerfCounterList;
  FValueToStringConverterConfig := AValueToStringConverterConfig;

  cbbCacheTypes.ItemIndex := 1; // SAS.Planet
  cbbDestCacheTypes.ItemIndex := 5; // BerkeleyDB
end;

destructor TfrmCacheManager.Destroy;
begin
  FAppClosingNotifier := nil;
  inherited Destroy;
end;

procedure TfrmCacheManager.btnSelectSrcPathClick(Sender: TObject);
var
  VPath: string;
begin
  if SelectDirectory('', '', VPath) then begin
    edtPath.Text := IncludeTrailingPathDelimiter(VPath);
  end;
end;

procedure TfrmCacheManager.btnCanselClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmCacheManager.btnSelectDestPathClick(Sender: TObject);
var
  VPath: string;
begin
  if SelectDirectory('', '', VPath) then begin
    edtDestPath.Text := IncludeTrailingPathDelimiter(VPath);
  end;
end;

procedure TfrmCacheManager.btnStartClick(Sender: TObject);
begin
  if PageControl1.ActivePageIndex = 0 then begin
    ProcessCacheConverter;
  end;
  if chkCloseWithStart.Checked then begin
    Self.Close;
  end;
end;

procedure TfrmCacheManager.ProcessCacheConverter;

  function GetCacheFormatFromIndex(const AIndex: Integer): Byte;
  begin
    case AIndex of
      0: Result := c_File_Cache_Id_GMV;
      1: Result := c_File_Cache_Id_SAS;
      2: Result := c_File_Cache_Id_ES;
      3: Result := c_File_Cache_Id_GM;
      4: Result := c_File_Cache_Id_GM_Aux;
      5: Result := c_File_Cache_Id_BDB;
    else
      Result := c_File_Cache_Id_SAS;
    end;
  end;
var
  VProgressInfo: ICacheConverterProgressInfo;
  VCancelNotifierInternal: INotifierOperationInternal;
  VOperationID: Integer;
  VConverterThread: TThreadCacheConverter;
begin
  VProgressInfo := TCacheConverterProgressInfo.Create;

  VCancelNotifierInternal := TNotifierOperation.Create;
  VOperationID := VCancelNotifierInternal.CurrentOperation;

  VConverterThread := TThreadCacheConverter.Create(
    VCancelNotifierInternal,
    VOperationID,
    edtPath.Text,
    edtDestPath.Text,
    edtDefExtention.Text,
    GetCacheFormatFromIndex(cbbCacheTypes.ItemIndex),
    GetCacheFormatFromIndex(cbbDestCacheTypes.ItemIndex),
    chkIgnoreTNE.Checked,
    chkRemove.Checked,
    chkOverwrite.Checked,
    FGCList,
    FContentTypeManager,
    FPerfCounterList,
    VProgressInfo
  );

  TfrmProgressCacheConverter.Create(
    VConverterThread,
    FLanguageManager,
    FAppClosingNotifier,
    FTimerNoifier,
    VCancelNotifierInternal,
    VProgressInfo,
    FValueToStringConverterConfig
  );
end;

end.


