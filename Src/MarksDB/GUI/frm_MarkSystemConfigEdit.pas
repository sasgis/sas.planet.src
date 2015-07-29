unit frm_MarkSystemConfigEdit;

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
  i_MarkSystemConfig,
  i_MarkSystemImplConfig,
  i_MarkSystemImplConfigSML,
  i_MarkSystemImplConfigORM,
  i_MarkSystemImplFactory,
  u_CommonFormAndFrameParents;

type
  TfrmMarkSystemConfigEdit = class(TCommonFormParent)
    grpDBType: TGroupBox;
    cbbDbType: TComboBox;
    grpFile: TGroupBox;
    btnOpenFile: TButton;
    grpDisplayName: TGroupBox;
    edtDisplayName: TEdit;
    edtFileName: TEdit;
    grpOptions: TGroupBox;
    edtUserName: TEdit;
    lblUserName: TLabel;
    chkReadOnly: TCheckBox;
    btnOk: TButton;
    btnCancel: TButton;
    dlgOpenDb: TOpenDialog;
    lblPass: TLabel;
    edtPass: TEdit;
    chkShowPass: TCheckBox;
    procedure btnCancelClick(Sender: TObject);
    procedure cbbDbTypeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure chkShowPassClick(Sender: TObject);
  private
    FGUIDList: array of TGUID;
    FConfig: IMarkSystemConfigStatic;
    FImpl: IMarkSystemImplConfigStatic;
    FImplSML: IMarkSystemImplConfigSML;
    FImplORM: IMarkSystemImplConfigORM;

    FMarkSystemConfig: IMarkSystemConfigListChangeable;
    FMarkSystemFactoryList: IMarkSystemImplFactoryListStatic;

    procedure FillDbTypeList;
    procedure Clear;
    procedure PrepareControls;
    procedure UpdateControlsStateByActiveDB;
  public
    procedure AddNewDatabaseConfig;
    procedure EditActiveDatabaseConfig;
  public
    constructor Create(
      const AOwner: TComponent;
      const AMarkSystemFactoryList: IMarkSystemImplFactoryListStatic;
      const AMarkSystemConfig: IMarkSystemConfigListChangeable
    ); reintroduce;
  end;

implementation

uses
  ActiveX,
  gnugettext,
  c_MarkSystem,
  u_MarkSystemImplConfigSML,
  U_MarkSystemImplConfigORM;

{$R *.dfm}

constructor TfrmMarkSystemConfigEdit.Create(
  const AOwner: TComponent;
  const AMarkSystemFactoryList: IMarkSystemImplFactoryListStatic;
  const AMarkSystemConfig: IMarkSystemConfigListChangeable
);
begin
  inherited Create(AOwner);
  FMarkSystemFactoryList := AMarkSystemFactoryList;
  FMarkSystemConfig := AMarkSystemConfig;
  FillDbTypeList;
  Clear;
end;

procedure TfrmMarkSystemConfigEdit.Clear;
begin
  FConfig := nil;
  FImpl := nil;
  FImplSML := nil;
  FImplORM := nil;
end;

procedure TfrmMarkSystemConfigEdit.FillDbTypeList;
var
  I: Cardinal;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  VCount: Integer;
  VDefItem: Integer;
  VItem: IMarkSystemImplFactoryListElement;
begin
  VCount := 0;
  VDefItem := 0;
  SetLength(FGUIDList, VCount);
  VEnum := FMarkSystemFactoryList.GetGUIDEnum;
  while VEnum.Next(1, VGUID, I) = S_OK do begin
    if IsEqualGUID(VGUID, cORMSQLiteMarksDbGUID) then begin
      VDefItem := VCount;
    end;
    VItem := FMarkSystemFactoryList.Get(VGUID);
    SetLength(FGUIDList, VCount+1);
    FGUIDList[VCount] := VGUID;
    Inc(VCount);
    cbbDbType.Items.Add(VItem.Caption);
  end;
  if VCount > 0 then begin
    cbbDbType.ItemIndex := VDefItem;
  end;
end;

procedure TfrmMarkSystemConfigEdit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Clear;
end;

procedure TfrmMarkSystemConfigEdit.AddNewDatabaseConfig;
begin
  Clear;
  PrepareControls;
  ShowModal;
end;

procedure TfrmMarkSystemConfigEdit.EditActiveDatabaseConfig;
begin
  Clear;
  FConfig := FMarkSystemConfig.GetActiveConfig;
  if Assigned(FConfig) then begin
    FImpl := FConfig.ImplConfig;
    Supports(FImpl, IMarkSystemImplConfigSML, FImplSML);
    Supports(FImpl, IMarkSystemImplConfigORM, FImplORM);
    PrepareControls;
    ShowModal;
  end else begin
    Assert(False);
  end;
end;

procedure TfrmMarkSystemConfigEdit.PrepareControls;
var
  I: Integer;
begin
  if Assigned(FConfig) and Assigned(FImpl) then begin
    for I := 0 to Length(FGUIDList) do begin
      if IsEqualGUID(FGUIDList[I], FConfig.DatabaseGUID) then begin
        cbbDbType.ItemIndex := I;
        Break;
      end;
    end;
    cbbDbType.Enabled := False;

    if Assigned(FImplORM) then begin
      edtUserName.Text := FImplORM.UserName;
      edtPass.Text := FImplORM.PasswordPlain;
    end else begin
      edtUserName.Text := '';
      edtPass.Text := '';
    end;

    edtDisplayName.Text := FConfig.DisplayName;
    edtFileName.Text := FImpl.FileName;
    chkReadOnly.Checked := FImpl.IsReadOnly;

    Self.Caption := _('Edit Marks Database');
  end else begin
    cbbDbType.Enabled := True;

    edtDisplayName.Text := _('My Marks');

    edtFileName.Text := '';
    edtFileName.Enabled := True;
    btnOpenFile.Enabled := True;

    edtUserName.Text := '';
    edtPass.Text := '';

    chkReadOnly.Checked := False;

    Self.Caption := _('Add Marks Database');
  end;

  UpdateControlsStateByActiveDB;
end;

procedure TfrmMarkSystemConfigEdit.UpdateControlsStateByActiveDB;
var
  VSelectedGUID: TGUID;
  VUserEnabled: Boolean;
  VPassEnabled: Boolean;
  VIsSML, VIsSQLite: Boolean;
begin
  if cbbDbType.ItemIndex >= 0 then begin
    VSelectedGUID := FGUIDList[cbbDbType.ItemIndex];

    VIsSML := IsEqualGUID(VSelectedGUID, cSMLMarksDbGUID);
    VIsSQLite := IsEqualGUID(VSelectedGUID, cORMSQLiteMarksDbGUID);

    VUserEnabled := not VIsSML;
    VPassEnabled := VUserEnabled and not VIsSQLite;

    edtUserName.Enabled := VUserEnabled;
    lblUserName.Enabled := VUserEnabled;

    edtPass.Enabled := VPassEnabled;
    lblPass.Enabled := VPassEnabled;
    chkShowPass.Enabled := VPassEnabled;

    if VIsSML or VIsSQLite then begin
      grpFile.Caption := _('File name');
      btnOpenFile.Enabled := True;
    end else begin
      grpFile.Caption := _('Connection string');
      btnOpenFile.Enabled := False;
    end;
  end;
end;

function GetFilterByDB(const ADB: TGUID): string;
begin
  if IsEqualGUID(ADB, cORMSQLiteMarksDbGUID) then begin
    Result := '*' + cORMSQLiteMarksDbFileExt;
    Result := 'SQLite3 (' + Result + ')|' + Result;
  end else if IsEqualGUID(ADB, cSMLMarksDbGUID) then begin
    Result := '*' + cSMLMarksDbFileExt;
    Result := 'SML (' + Result + ')|' + Result;
  end else begin
    Result := '';
  end;
end;

procedure TfrmMarkSystemConfigEdit.btnOpenFileClick(Sender: TObject);
begin
  dlgOpenDb.Filter := GetFilterByDB(FGUIDList[cbbDbType.ItemIndex]);
  if dlgOpenDb.Execute then begin
    edtFileName.Text := dlgOpenDb.FileName;
  end;
end;

procedure TfrmMarkSystemConfigEdit.cbbDbTypeChange(Sender: TObject);
begin
  UpdateControlsStateByActiveDB;
  edtFileName.Text := '';
end;

procedure TfrmMarkSystemConfigEdit.chkShowPassClick(Sender: TObject);
begin
  if chkShowPass.Checked then begin
    edtPass.PasswordChar := #0;
  end else begin
    edtPass.PasswordChar := '*';
  end;
end;

procedure TfrmMarkSystemConfigEdit.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMarkSystemConfigEdit.btnOkClick(Sender: TObject);
var
  VDatabase: TGUID;
  VMsgText: string;
  VFileName: string;
  VDisplayName: string;
  VIsReadOnly: Boolean;
  VIsSML, VIsSQLite: Boolean;
  VImpl: IMarkSystemImplConfigStatic;
begin
  VDatabase := FGUIDList[cbbDbType.ItemIndex];

  VIsSML := IsEqualGUID(VDatabase, cSMLMarksDbGUID);
  VIsSQLite := IsEqualGUID(VDatabase, cORMSQLiteMarksDbGUID);

  VFileName := edtFileName.Text;
  if VFileName = '' then begin
    if VIsSML or VIsSQLite then begin
      VMsgText := _('Set File name first!');
    end else begin
      VMsgText := _('Set Connection string first!');
    end;
    MessageDlg(VMsgText, mtError, [mbOK], 0);
    Exit;
  end;

  VDisplayName := edtDisplayName.Text;
  if VDisplayName = '' then begin
    VDisplayName := _('My Marks');
  end;

  VIsReadOnly := chkReadOnly.Checked;

  if VIsSML then begin
    VImpl :=
      TMarkSystemImplConfigSML.Create(
        VFileName,
        VIsReadOnly
      );
  end
  else
  if
    VIsSQLite or
    IsEqualGUID(VDatabase, cORMMongoDbMarksDbGUID) or
    IsEqualGUID(VDatabase, cORMODBCMarksDbGUID) or
    IsEqualGUID(VDatabase, cORMZDBCMarksDbGUID) then
  begin
    VImpl :=
      TMarkSystemImplConfigORM.Create(
        VFileName,
        VIsReadOnly,
        edtUserName.Text,
        edtPass.Text,
        ''
      );
  end else begin
    Assert(False);
    Exit;
  end;

  if Assigned(FConfig) then begin
    FMarkSystemConfig.Update(FConfig.ID, VDatabase, VDisplayName, VImpl);
  end else begin
    FMarkSystemConfig.Add(VDatabase, VDisplayName, VImpl, True);
  end;

  Close;
end;

end.
