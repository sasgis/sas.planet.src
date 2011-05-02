unit u_MarkLineTemplateConfig;

interface

uses
  Classes,
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MarksSimple,
  i_MarkCategory,
  i_MarksFactoryConfig,
  i_MarkCategoryDBSmlInternal,
  u_MarkTemplateConfigBase;

type
  TMarkLineTemplateConfig = class(TMarkTemplateConfigBase, IMarkLineTemplateConfig)
  private
    FDefaultTemplate: IMarkTemplateLine;

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
  public
    constructor Create(
      ACategoryDb: IMarkCategoryDBSmlInternal
    );
  end;

implementation

uses
  u_ConfigProviderHelpers,
  u_ResStrings,
  u_MarkTemplates;

{ TMarkLineTemplateConfig }

constructor TMarkLineTemplateConfig.Create(
  ACategoryDb: IMarkCategoryDBSmlInternal
);
begin
  inherited Create(ACategoryDb, SAS_STR_NewPath);

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
  VCategoryId := -1;
  if ACategory <> nil then begin
    VCategoryId := ACategory.Id;
  end;
  Result := TMarkTemplateLine.Create(
    CategoryDb,
    NameGenerator,
    VCategoryId,
    AColor1,
    AScale1
  );
end;

procedure TMarkLineTemplateConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
var
  VCategoryId: Integer;
  VCategory: IMarkCategory;
  VColor1: TColor32;
  VScale1: Integer;
begin
  inherited;
  VCategoryID := -1;
  VCategory := FDefaultTemplate.Category;
  if VCategory <> nil then begin
    VCategoryID := VCategory.Id;
  end;
  VColor1 := FDefaultTemplate.Color1;
  VScale1 := FDefaultTemplate.Scale1;
  if AConfigData <> nil then begin
    VCategoryId := AConfigData.ReadInteger('CategoryId', VCategoryId);
    VColor1 := ReadColor32(AConfigData, 'LineColor', VColor1);
    VScale1 := AConfigData.ReadInteger('LineWidth', VScale1);
  end;
  SetDefaultTemplate(
    TMarkTemplateLine.Create(
      CategoryDb,
      NameGenerator,
      VCategoryId,
      VColor1,
      VScale1
    )
  );
end;

procedure TMarkLineTemplateConfig.DoWriteConfig(
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

function TMarkLineTemplateConfig.IsSameTempalte(lhs,
  rhs: IMarkTemplateLine): Boolean;
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

