unit u_MarkLineTemplateConfig;

interface

uses
  Classes,
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MarksSimple,
  i_MarkCategory,
  i_MarkNameGenerator,
  i_MarksFactoryConfig,
  u_ConfigDataElementComplexBase;

type
  TMarkLineTemplateConfig = class(TConfigDataElementComplexBase, IMarkLineTemplateConfig)
  private
    FDefaultTemplate: IMarkTemplateLine;
    FNameGenerator: IMarkNameGenerator;

    function IsSameTempalte(lhs, rhs: IMarkTemplateLine): Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function CreateTemplate(
      ACategory: IMarkCategory;
      AColor1: TColor32;
      AScale1: Integer
    ): IMarkTemplateLine;

    function GetDefaultTemplate: IMarkTemplateLine;
    procedure SetDefaultTemplate(AValue: IMarkTemplateLine);

    function GetNameGenerator: IMarkNameGenerator;
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_ConfigProviderHelpers,
  u_MarkNameGenerator,
  u_ResStrings,
  u_MarkTemplates;

{ TMarkLineTemplateConfig }

constructor TMarkLineTemplateConfig.Create;
begin
  inherited;

  FNameGenerator := TMarkNameGenerator.Create(SAS_STR_NewPath);
  Add(FNameGenerator, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Name'), False, False, False, False);

  FDefaultTemplate := CreateTemplate(
    nil,
    SetAlpha(clRed32, 166),
    2
  );
end;

function TMarkLineTemplateConfig.CreateTemplate(
  ACategory: IMarkCategory;
  AColor1: TColor32;
  AScale1: Integer
): IMarkTemplateLine;
var
  VCategoryId: Integer;
begin
  if ACategory <> nil then begin
    VCategoryId := ACategory.Id;
  end else begin
    VCategoryId := -1;
  end;
  Result := TMarkTemplateLine.Create(
    FNameGenerator,
    VCategoryId,
    AColor1,
    AScale1
  );
end;

procedure TMarkLineTemplateConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
var
  VCategoryId: Integer;
  VColor1: TColor32;
  VScale1: Integer;
begin
  inherited;
  VCategoryId := FDefaultTemplate.CategoryId;
  VColor1 := FDefaultTemplate.Color1;
  VScale1 := FDefaultTemplate.Scale1;
  if AConfigData <> nil then begin
    VCategoryId := AConfigData.ReadInteger('CategoryId', VCategoryId);
    VColor1 := ReadColor32(AConfigData, 'LineColor', VColor1);
    VScale1 := AConfigData.ReadInteger('LineWidth', VScale1);
  end;
  SetDefaultTemplate(
    TMarkTemplateLine.Create(
      FNameGenerator,
      VCategoryId,
      VColor1,
      VScale1
    )
  );
end;

procedure TMarkLineTemplateConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('CategoryId', FDefaultTemplate.CategoryId);
  WriteColor32(AConfigData, 'LineColor', FDefaultTemplate.Color1);
  AConfigData.WriteInteger('LineWidth', FDefaultTemplate.Scale1);
end;

function TMarkLineTemplateConfig.GetDefaultTemplate: IMarkTemplateLine;
begin
  LockRead;
  try
    Result := FDefaultTemplate;
  finally
    UnlockRead;
  end;
end;

function TMarkLineTemplateConfig.GetNameGenerator: IMarkNameGenerator;
begin
  Result := FNameGenerator;
end;

function TMarkLineTemplateConfig.IsSameTempalte(lhs,
  rhs: IMarkTemplateLine): Boolean;
begin
  Result :=
    (lhs.CategoryId = rhs.CategoryId) and
    (lhs.Color1 = rhs.Color1) and
    (lhs.Scale1 = rhs.Scale1);
end;

procedure TMarkLineTemplateConfig.SetDefaultTemplate(
  AValue: IMarkTemplateLine);
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

