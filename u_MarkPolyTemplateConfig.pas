unit u_MarkPolyTemplateConfig;

interface

uses
  Classes,
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MarkTemplate,
  i_MarkCategory,
  i_MarksFactoryConfig,
  i_MarkCategoryDBSmlInternal,
  u_MarkTemplateConfigBase;

type
  TMarkPolyTemplateConfig = class(TMarkTemplateConfigBase, IMarkPolyTemplateConfig)
  private
    FDefaultTemplate: IMarkTemplatePoly;
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
  SysUtils,
  i_MarksDbSmlInternal,
  u_ConfigProviderHelpers,
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
  VCategoryInternal: IMarkCategorySMLInternal;
begin
  VCategoryId := -1;
  if ACategory <> nil then begin
    if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
      VCategoryId := VCategoryInternal.Id;
    end;
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
  VCategoryId: Integer;
  VColor1, VColor2: TColor32;
  VScale1: Integer;
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
  VCategoryId: Integer;
  VTemplateInternal: IMarkTemplateSMLInternal;
begin
  inherited;
  VCategoryID := -1;
  if Supports(FDefaultTemplate, IMarkTemplateSMLInternal, VTemplateInternal) then begin
    VCategoryId := VTemplateInternal.CategoryId;
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

procedure TMarkPolyTemplateConfig.SetDefaultTemplate(
  AValue: IMarkTemplatePoly);
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

