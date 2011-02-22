unit u_MarkPointTemplateConfig;

interface
uses
  Classes,
  GR32,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IMarkPicture,
  i_MarksSimple,
  i_IMarksFactoryConfig,
  u_ConfigDataElementBase;

type
  TMarkPointTemplateConfig = class(TConfigDataElementBase, IMarkPointTemplateConfig)
  private
    FDefaultTemplate: IMarkTemplatePoint;
    FMarkPictureList: IMarkPictureList;
    function IsSameTempalte(lhs, rhs: IMarkTemplatePoint): Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function CreateTemplate(
      APicName: string;
      APic: IMarkPicture;
      ACategoryId: Integer;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer
    ): IMarkTemplatePoint; overload;
    function CreateTemplate(
      ASource: IMarkFull
    ): IMarkTemplatePoint; overload;

    function GetMarkPictureList: IMarkPictureList;

    function GetDefaultTemplate: IMarkTemplatePoint;
    procedure SetDefaultTemplate(AValue: IMarkTemplatePoint);
  public
    constructor Create(AMarkPictureList: IMarkPictureList);
  end;

implementation

uses
  u_ConfigProviderHelpers,
  u_MarkTemplates;

{ TMarkPointTemplateConfig }

constructor TMarkPointTemplateConfig.Create(AMarkPictureList: IMarkPictureList);
var
  VPicName: string;
  VPic: IMarkPicture;
begin
  inherited Create;
  FMarkPictureList := AMarkPictureList;
  if FMarkPictureList.Count > 0 then begin
    VPicName := FMarkPictureList.GetName(0);
    VPic := FMarkPictureList.Get(0);
  end else begin
    VPicName := '';
    VPic := nil;
  end;

  FDefaultTemplate := CreateTemplate(
    VPicName,
    VPic,
    -1,
    SetAlpha(clYellow32, 166),
    SetAlpha(clBlack32, 166),
    11,
    32
  );
end;

function TMarkPointTemplateConfig.CreateTemplate(APicName: string;
  APic: IMarkPicture; ACategoryId: Integer; AColor1, AColor2: TColor32; AScale1,
  AScale2: Integer): IMarkTemplatePoint;
begin
  Result := TMarkTemplatePoint.Create(
    ACategoryId,
    AColor1,
    AColor2,
    AScale1,
    AScale2,
    APicName,
    APic
  );
end;

function TMarkPointTemplateConfig.CreateTemplate(
  ASource: IMarkFull): IMarkTemplatePoint;
begin
  Result := CreateTemplate(
    ASource.PicName,
    ASource.Pic,
    ASource.CategoryId,
    ASource.Color1,
    ASource.Color2,
    ASource.Scale1,
    ASource.Scale2
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
begin
  inherited;
  VCategoryId := FDefaultTemplate.CategoryId;
  VColor1 := FDefaultTemplate.Color1;
  VColor2 := FDefaultTemplate.Color2;
  VScale1 := FDefaultTemplate.Scale1;
  VScale2 := FDefaultTemplate.Scale2;
  VPicName := FDefaultTemplate.PicName;
  VPic := FDefaultTemplate.Pic;
  if VPicName = '' then begin
    if FMarkPictureList.Count > 0 then begin
      VPicName := FMarkPictureList.GetName(0);
      VPic := FMarkPictureList.Get(0);
    end;
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
    VColor1 := LoadColor32(AConfigData, 'TextColor', VColor1);
    VColor2 := LoadColor32(AConfigData, 'ShadowColor', VColor2);
    VScale1 := AConfigData.ReadInteger('FontSize', VScale1);
    VScale2 := AConfigData.ReadInteger('IconSize', VScale2);
  end;
  SetDefaultTemplate(
    CreateTemplate(
      VPicName,
      VPic,
      VCategoryId,
      VColor1,
      VColor2,
      VScale1,
      VScale2
    )
  );
end;

procedure TMarkPointTemplateConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteString('IconName', FDefaultTemplate.PicName);
  AConfigData.WriteInteger('CategoryId', FDefaultTemplate.CategoryId);
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

function TMarkPointTemplateConfig.IsSameTempalte(lhs,
  rhs: IMarkTemplatePoint): Boolean;
begin
  Result :=
    (lhs.CategoryId = rhs.CategoryId) and
    (lhs.Color1 = rhs.Color1) and
    (lhs.Color2 = rhs.Color2) and
    (lhs.Scale1 = rhs.Scale1) and
    (lhs.Scale2 = rhs.Scale2) and
    (lhs.PicName = rhs.PicName) and
    (lhs.Pic = rhs.Pic);
end;

procedure TMarkPointTemplateConfig.SetDefaultTemplate(
  AValue: IMarkTemplatePoint);
begin
  LockWrite;
  try
    if not IsSameTempalte(FDefaultTemplate, AValue) then begin
      FDefaultTemplate := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
