unit u_MarkPolyTemplateConfig;

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
  i_MarkCategoryDBSmlInternal,
  u_MarkTemplateConfigBase;

type
  TMarkPolyTemplateConfig = class(TMarkTemplateConfigBase, IMarkPolyTemplateConfig)
  private
    FDefaultTemplate: IMarkTemplatePoly;

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
  public
    constructor Create(
      ACategoryDb: IMarkCategoryDBSmlInternal
    );
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_ConfigProviderHelpers,
  u_MarkNameGenerator,
  u_ResStrings,
  u_MarkTemplates;

{ TMarkPolyTemplateConfig }

constructor TMarkPolyTemplateConfig.Create(
  ACategoryDb: IMarkCategoryDBSmlInternal
);
begin
  inherited Create(ACategoryDb, SAS_STR_NewPoly);

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
  VCategoryId := -1;
  if ACategory <> nil then begin
    VCategoryId := ACategory.Id;
  end;
  Result := TMarkTemplatePoly.Create(
    CategoryDb,
    NameGenerator,
    VCategoryId,
    AColor1,
    AColor2,
    AScale1
  );
end;

procedure TMarkPolyTemplateConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
var
  VCategory: IMarkCategory;
  VCategoryId: Integer;
  VColor1, VColor2: TColor32;
  VScale1: Integer;
begin
  inherited;
  VCategoryID := -1;
  VCategory := FDefaultTemplate.Category;
  if VCategory <> nil then begin
    VCategoryID := VCategory.Id;
  end;
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
      CategoryDb,
      NameGenerator,
      VCategoryId,
      VColor1,
      VColor2,
      VScale1
    )
  );
end;

procedure TMarkPolyTemplateConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
var
  VCategory: IMarkCategory;
  VCategoryId: Integer;
begin
  inherited;
  VCategoryID := -1;
  VCategory := FDefaultTemplate.Category;
  if VCategory <> nil then begin
    VCategoryID := VCategory.Id;
  end;
  AConfigData.WriteInteger('CategoryId', VCategoryId);
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

function TMarkPolyTemplateConfig.IsSameTempalte(lhs,
  rhs: IMarkTemplatePoly): Boolean;
var
  VlhsCategory: IMarkCategory;
  VrhsCategory: IMarkCategory;
begin
  VlhsCategory := lhs.Category;
  VrhsCategory := rhs.Category;
  Result :=
    (
      (
        (VlhsCategory <> nil) and
        (VrhsCategory <> nil) and
        (VlhsCategory.Id = VrhsCategory.Id)
      ) or
        (VlhsCategory = nil) and
        (VrhsCategory = nil)
    )and
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

