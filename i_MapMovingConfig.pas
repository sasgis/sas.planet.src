unit i_MapMovingConfig;

interface

uses
  i_ConfigDataElement;

type
  IMapMovingConfig = interface(IConfigDataElement)
    ['{A322104E-A247-4EB3-83F6-C897F64E764C}']
    //Анимация инерции
    function GetAnimateMove: Boolean;
    procedure SetAnimateMove(AValue: Boolean);
    property AnimateMove: Boolean read GetAnimateMove write SetAnimateMove;

    function GetAnimateMoveTime: Cardinal;
    procedure SetAnimateMoveTime(AValue: Cardinal);
    property AnimateMoveTime: Cardinal read GetAnimateMoveTime write SetAnimateMoveTime;

    function GetAnimateMaxStartSpeed: Cardinal;
    procedure SetAnimateMaxStartSpeed(AValue: Cardinal);
    property AnimateMaxStartSpeed: Cardinal read GetAnimateMaxStartSpeed write SetAnimateMaxStartSpeed;

    function GetAnimateMinStartSpeed: Cardinal;
    procedure SetAnimateMinStartSpeed(AValue: Cardinal);
    property AnimateMinStartSpeed: Cardinal read GetAnimateMinStartSpeed write SetAnimateMinStartSpeed;
  end;

implementation

end.
