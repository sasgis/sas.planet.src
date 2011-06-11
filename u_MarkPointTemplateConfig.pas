unit u_MarkPointTemplateConfig;

interface

uses
  Classes,
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MarkPicture,
  i_MarkTemplate,
  i_MarkCategory,
  i_MarksFactoryConfig,
  i_MarkCategoryDBSmlInternal,
  u_MarkTemplateConfigBase;

type
  TMarkPointTemplateConfig = class(TMarkTemplateConfigBase, IMarkPointTemplateConfig)
  private
    FDefaultTemplate: IMarkTemplatePoint;
    FMarkPictureList: IMarkPictureList;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function CreateTemplate(
      APic: IMarkPicture;
      ACategory: IMarkCategory;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer
    ): IMarkTemplatePoint;

    function GetMarkPictureList: IMarkPictureList;

    function GetDefaultTemplate: IMarkTemplatePoint;
    procedure SetDefaultTemplate(AValue: IMarkTemplatePoint);
  public
    constructor Create(
      ACategoryDb: IMarkCategoryDBSmlInternal;
      AMarkPictureList: IMarkPictureList
    );
  end;

implementation

uses
  SysUtils,
  i_MarksDbSmlInternal,
  u_ConfigProviderHelpers,
  u_ResStrings,
  u_MarkTemplates;

{ TMarkPointTemplateConfig }

constructor TMarkPointTemplateConfig.Create(
  ACategoryDb: IMarkCategoryDBSmlInternal;
  AMarkPictureList: IMarkPictureList
);
var
  VPic: IMarkPicture;
begin
  inherited Create(ACategoryDb, SAS_STR_NewMark);

  FMarkPictureList := AMarkPictureList;
  if FMarkPictureList.Count > 0 then begin
    VPic := FMarkPictureList.Get(0);
  end else begin
    VPic := nil;
  end;

  FDefaultTemplate := CreateTemplate(
    VPic,
    nil,
    SetAlpha(clYellow32, 166),
    SetAlpha(clBlack32, 166),
    11,
    32
  );
end;

function TMarkPointTemplateConfig.CreateTemplate(
  APic: IMarkPicture;
  ACategory: IMarkCategory;
  AColor1, AColor2: TColor32;
  AScale1, AScale2: Integer
): IMarkTemplatePoint;
var
  VCategoryId: Integer;
  VCategoryInternal: IMarkCategorySMLInternal;
begin
  VCategoryId := -1;
  if ACategory <> nil then begin
    if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
      VCategoryId := VCategoryInternal.Id;
    end;
  end;
  Result := TMarkTemplatePoint.Create(
    CategoryDb,
    NameGenerator,
    VCategoryId,
    AColor1,
    AColor2,
    AScale1,
    AScale2,
    APic
  );
end;

procedure TMarkPointTemplateConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
var
  VPicName: string;
  VPic: IMarkPicture;
  VPicIndex: Integer;
  VCategoryId: Integer;
  VColor1, VColor2: TColor32;
  VScale1, VScale2: Integer;
  VTemplateInternal: IMarkTemplateSMLInternal;
begin
  inherited;
  VCategoryID := -1;
  if Supports(FDefaultTemplate, IMarkTemplateSMLInternal, VTemplateInternal) then begin
    VCategoryId := VTemplateInternal.CategoryId;
  end;
  VColor1 := FDefaultTemplate.Color1;
  VColor2 := FDefaultTemplate.Color2;
  VScale1 := FDefaultTemplate.Scale1;
  VScale2 := FDefaultTemplate.Scale2;
  VPic := FDefaultTemplate.Pic;
  if VPic = nil then begin
    VPic := FMarkPictureList.GetDefaultPicture;
  end;
  if VPic <> nil then begin
    VPicName := VPic.GetName;
  end;
  if AConfigData <> nil then begin
    VPicName := AConfigData.ReadString('IconName', VPicName);
    if VPicName = '' then begin
      VPic := nil;
    end else begin
      VPicIndex := FMarkPictureList.GetIndexByName(VPicName);
      if VPicIndex < 0 then begin
        VPic := nil;
      end else begin
        VPic := FMarkPictureList.Get(VPicIndex);
      end;
    end;
    VCategoryId := AConfigData.ReadInteger('CategoryId', VCategoryId);
    VColor1 := ReadColor32(AConfigData, 'TextColor', VColor1);
    VColor2 := ReadColor32(AConfigData, 'ShadowColor', VColor2);
    VScale1 := AConfigData.ReadInteger('FontSize', VScale1);
    VScale2 := AConfigData.ReadInteger('IconSize', VScale2);
  end;
  SetDefaultTemplate(
    TMarkTemplatePoint.Create(
      CategoryDb,
      NameGenerator,
      VCategoryId,
      VColor1,
      VColor2,
      VScale1,
      VScale2,
      VPic
    )
  );
end;

procedure TMarkPointTemplateConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
var
  VCategoryId: Integer;
  VPicName: string;
  VTemplateInternal: IMarkTemplateSMLInternal;
  VPic: IMarkPicture;
begin
  inherited;
  VCategoryID := -1;
  if Supports(FDefaultTemplate, IMarkTemplateSMLInternal, VTemplateInternal) then begin
    VCategoryId := VTemplateInternal.CategoryId;
  end;
  VPicName := '';
  VPic := FDefaultTemplate.Pic;
  if VPic <> nil then begin
    VPicName := VPic.GetName;
  end;
  AConfigData.WriteString('IconName', VPicName);
  AConfigData.WriteInteger('CategoryId', VCategoryId);
  WriteColor32(AConfigData, 'TextColor', FDefaultTemplate.Color1);
  WriteColor32(AConfigData, 'ShadowColor', FDefaultTemplate.Color2);
  AConfigData.WriteInteger('FontSize', FDefaultTemplate.Scale1);
  AConfigData.WriteInteger('IconSize', FDefaultTemplate.Scale2);
end;

function TMarkPointTemplateConfig.GetDefaultTemplate: IMarkTemplatePoint;
begin
  LockRead;
  try
    Result := FDefaultTemplate;
  finally
    UnlockRead;
  end;
end;

function TMarkPointTemplateConfig.GetMarkPictureList: IMarkPictureList;
begin
  Result := FMarkPictureList;
end;

procedure TMarkPointTemplateConfig.SetDefaultTemplate(
  AValue: IMarkTemplatePoint);
begin
  if AValue <> nil then begin
    LockWrite;
    try
      if (FDefaultTemplate = nil) or (not FDefaultTemplate.IsSame(AValue)) then begin
        FDefaultTemplate := AValue;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

end.
