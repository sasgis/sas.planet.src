unit i_ShortCutSingleConfig;

interface

uses
  Classes,
  Graphics,
  i_ConfigDataElement;

type
  IShortCutSingleConfig = interface(IConfigDataElement)
    ['{B8B92915-98D2-4254-ACE7-92ACFC081513}']
    function GetCaption: String;
    property Caption: String read GetCaption;

    function GetIconBitmap: TBitmap;
    property IconBitmap: TBitmap read GetIconBitmap;

    function GetShortCut: TShortCut;
    procedure SetShortCut(AValue: TShortCut);
    property ShortCut: TShortCut read GetShortCut write SetShortCut;

    procedure ResetToDefault;

    procedure ResetShortCut;
    procedure ApplyShortCut;
  end;

implementation

end.
