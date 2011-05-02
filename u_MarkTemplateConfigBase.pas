unit u_MarkTemplateConfigBase;

interface
uses
  Classes,
  GR32,
  i_MarkNameGenerator,
  i_MarkCategoryDBSmlInternal,
  u_ConfigDataElementComplexBase;

type
  TMarkTemplateConfigBase = class(TConfigDataElementComplexBase)
  private
    FCategoryDb: IMarkCategoryDBSmlInternal;
    FNameGenerator: IMarkNameGenerator;
  protected
    property CategoryDb: IMarkCategoryDBSmlInternal read FCategoryDb;
    property NameGenerator: IMarkNameGenerator read FNameGenerator;
  protected
    function GetNameGenerator: IMarkNameGenerator;
  public
    constructor Create(
      ACategoryDb: IMarkCategoryDBSmlInternal;
      AFormatStringDefault: string
    );
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_MarkNameGenerator;

  { TMarkTemplateConfigBase }

constructor TMarkTemplateConfigBase.Create(
  ACategoryDb: IMarkCategoryDBSmlInternal;
  AFormatStringDefault: string
);
begin
  inherited Create;
  FCategoryDb := ACategoryDb;

  FNameGenerator := TMarkNameGenerator.Create(AFormatStringDefault);
  Add(FNameGenerator, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Name'), False, False, False, False);
end;

function TMarkTemplateConfigBase.GetNameGenerator: IMarkNameGenerator;
begin
  Result := FNameGenerator;
end;

end.
