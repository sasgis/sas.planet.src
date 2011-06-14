unit i_MessageHandler;

interface

uses
  Windows;

type
  IMessageHandler = interface
    ['{6EEDFD42-5FCC-42EF-B236-57E83DDA567C}']
    procedure DoMessageEvent(var Msg: TMsg; var Handled: Boolean);
  end;

implementation

end.
