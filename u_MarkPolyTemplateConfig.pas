unit u_MarkPolyTemplateConfig;

interface

uses
  Classes,
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MarksSimple,
  i_IMarkCategory,
  i_IMarkNameGenerator,
  i_IMarksFactoryConfig,
  u_ConfigDataElementComplexBase;

type
  TMarkPolyTemplateConfig = class(TConfigDataElementComplexBase, IMarkPolyTemplateConfig)
  private
    FDefaultTemplate: IMarkTemplatePoly;
    FNameGenerator: IMarkNameGenerator;

    function IsSameTempalte(lhs, rhs: IMarkTemplatePoly): Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function CreateTemplate(
      ACategory: IMarkCategory;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer
    ): IMarkTemplatePoly;

    function GetDefaultTemplate: IMarkTemplatePoly;
    procedure SetDefaultTemplate(AValue: IMarkTemplatePoly);

    function GetNameGenerator: IMarkNameGenerator;
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_ConfigProviderHelpers,
  u_MarkNameGenerator,
  UResStrings,
  u_MarkTemplates;

{ TMarkPolyTemplateConfig }

constructor TMarkPolyTemplateConfig.Create;
begin
  inherited Create;

  FNameGenerator := TMarkNameGenerator.Create(SAS_STR_NewPoly);
  Add(FNameGenerator, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Name'), False, False, False, False);

  FDefaultTemplate := CreateTemplate(
    nil,
    SetAlpha(clBlack32, 166),
    SetAlpha(clWhite32, 51),
    2
  );
end;

function TMarkPolyTemplateConfig.CreateTemplate(
  ACategory: IMarkCategory;
  AColor1: TColor32;
  AColor2: TColor32;
  AScale1: Integer
): IMarkTemplatePoly;
var
  VCategoryId: Integer;
begin
  if ACategory <> nil then begin
    VCategoryId := ACategory.Id;
  end else begin
    VCategoryId := -1;
  end;
  Result := TMarkTemplatePoly.Create(
    FNameGenerator,
    VCategoryId,
    AColor1,
    AColor2,
    AScale1
  );
end;

procedure TMarkPolyTemplateConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
var
  VCategoryId: Integer;
  VColor1, VColor2: TColor32;
  VScale1: Integer;
begin
  inherited;
  VCategoryId := FDefaultTemplate.CategoryId;
  VColor1 := FDefaultTemplate.Color1;
  VColor2 := FDefaultTemplate.Color2;
  VScale1 := FDefaultTemplate.Scale1;
  if AConfigData <> nil then begin
    VCategoryId := AConfigData.ReadInteger('CategoryId', VCategoryId);
    VColor1 := ReadColor32(AConfigData, 'LineColor', VColor1);
    VColor2 := ReadColor32(AConfigData, 'FillColor', VColor2);
    VScale1 := AConfigData.ReadInteger('LineWidth', VScale1);
  end;
  SetDefaultTemplate(
    TMarkTemplatePoly.Create(
      FNameGenerator,
      VCategoryId,
      VColor1,
      VColor2,
      VScale1
    )
  );
end;

procedure TMarkPolyTemplateConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('CategoryId', FDefaultTemplate.CategoryId);
  WriteColor32(AConfigData, 'LineColor', FDefaultTemplate.Color1);
  WriteColor32(AConfigData, 'FillColor', FDefaultTemplate.Color2);
  AConfigData.WriteInteger('LineWidth', FDefaultTemplate.Scale1);
end;

function TMarkPolyTemplateConfig.GetDefaultTemplate: IMarkTemplatePoly;
begin
  LockRead;
  try
    Result := FDefaultTemplate;
  finally
    UnlockRead;
  end;
end;

function TMarkPolyTemplateConfig.GetNameGenerator: IMarkNameGenerator;
begin
  Result := FNameGenerator;
end;

function TMarkPolyTemplateConfig.IsSameTempalte(lhs,
  rhs: IMarkTemplatePoly): Boolean;
begin
  Result :=
    (lhs.CategoryId = rhs.CategoryId) and
    (lhs.Color1 = rhs.Color1) and
    (lhs.Color2 = rhs.Color2) and
    (lhs.Scale1 = rhs.Scale1)
end;

procedure TMarkPolyTemplateConfig.SetDefaultTemplate(
  AValue: IMarkTemplatePoly);
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

