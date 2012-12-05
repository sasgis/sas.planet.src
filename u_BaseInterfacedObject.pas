unit u_BaseInterfacedObject;

interface

uses
  u_BaseInterfacedObjectDebug;

{$IFDEF DEBUG}
type
  TBaseInterfacedObject = TBaseInterfacedObjectDebug;
{$ELSE}
type
  TBaseInterfacedObject = TInterfacedObject;
{$ENDIF}

implementation

end.
