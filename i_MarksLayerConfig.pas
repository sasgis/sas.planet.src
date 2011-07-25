unit i_MarksLayerConfig;

interface

uses
  i_ConfigDataElement,
  i_UsedMarksConfig,
  i_MarksDrawConfig;

type
  IMarksLayerConfig = interface(IConfigDataElement)
    function GetMarksShowConfig: IUsedMarksConfig;
    property MarksShowConfig: IUsedMarksConfig read GetMarksShowConfig;

    function GetMarksDrawConfig: IMarksDrawConfig;
    property MarksDrawConfig: IMarksDrawConfig read GetMarksDrawConfig;
  end;

implementation

end.
