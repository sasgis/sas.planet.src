unit u_MarksFactoryConfig;

interface

uses
  Classes,
  i_MarkPicture,
  i_MarksFactoryConfig,
  i_MarkCategoryDBSmlInternal,
  u_ConfigDataElementComplexBase;

type
  TMarksFactoryConfig = class(TConfigDataElementComplexBase, IMarksFactoryConfig)
  private
    FPointTemplateConfig: IMarkPointTemplateConfig;
    FLineTemplateConfig: IMarkLineTemplateConfig;
    FPolyTemplateConfig: IMarkPolyTemplateConfig;
  protected
    function GetPointTemplateConfig: IMarkPointTemplateConfig;
    function GetLineTemplateConfig: IMarkLineTemplateConfig;
    function GetPolyTemplateConfig: IMarkPolyTemplateConfig;
  public
    constructor Create(
      ACategoryDb: IMarkCategoryDBSmlInternal;
      AMarkPictureList: IMarkPictureList
    );
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_MarkPointTemplateConfig,
  u_MarkLineTemplateConfig,
  u_MarkPolyTemplateConfig;

{ TMarksFactoryConfig }

constructor TMarksFactoryConfig.Create(
  ACategoryDb: IMarkCategoryDBSmlInternal;
  AMarkPictureList: IMarkPictureList
);
begin
  inherited Create;

  FPointTemplateConfig := TMarkPointTemplateConfig.Create(ACategoryDb, AMarkPictureList);
  Add(FPointTemplateConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarkNewPoint'));

  FLineTemplateConfig := TMarkLineTemplateConfig.Create(ACategoryDb);
  Add(FLineTemplateConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarkNewLine'));

  FPolyTemplateConfig := TMarkPolyTemplateConfig.Create(ACategoryDb);
  Add(FPolyTemplateConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarkNewPoly'));
end;

function TMarksFactoryConfig.GetLineTemplateConfig: IMarkLineTemplateConfig;
begin
  Result := FLineTemplateConfig;
end;

function TMarksFactoryConfig.GetPointTemplateConfig: IMarkPointTemplateConfig;
begin
  Result := FPointTemplateConfig;
end;

function TMarksFactoryConfig.GetPolyTemplateConfig: IMarkPolyTemplateConfig;
begin
  Result := FPolyTemplateConfig;
end;

end.
