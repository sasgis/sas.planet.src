{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclNotify.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel Bestebroer.                                 }
{ Portions created by Marcel Bestebroer are Copyright Marcel Bestebroer. All rights reserved.      }
{                                                                                                  }
{ Contributors:                                                                                    }
{   -                                                                                              }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains generic JCL notification/listener pattern interfaces and base implementations }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2009-07-02 15:24:19 +0200 (jeu. 02 juil. 2009)                          $ }
{ Revision:      $Rev:: 2841                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit u_JclNotify;

interface

uses
  Classes,
  SysUtils,
  i_JclNotify;

  { The following classes provide a basic notifier/listener implementation. Note that using one of these classes does
    not imply the usage of the related classes; the notifier can be used in conjection with any class implementing
    IListener and vice versa. }
type
  TJclBaseListener = class (TInterfacedObject, IListener)
  protected
    procedure Notification(const msg: IInterface); virtual; stdcall;
  end;

  TJclBaseNotifier = class (TInterfacedObject, INotifier)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FListeners: TInterfaceList;
    FSynchronizer: TMultiReadExclusiveWriteSynchronizer;
  protected
    procedure Add(const listener: IListener); stdcall;
    procedure Notify(const msg: IInterface); stdcall;
    procedure Remove(const listener: IListener); stdcall;
  end;

  TJclBaseNotifierFaked = class (TInterfacedObject, INotifier)
  protected
    procedure Add(const listener: IListener); stdcall;
    procedure Notify(const msg: IInterface); stdcall;
    procedure Remove(const listener: IListener); stdcall;
  end;

implementation

{ TJclBaseNotifier }

constructor TJclBaseNotifier.Create;
begin
  inherited Create;
  FListeners := TInterfaceList.Create;
  FSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TJclBaseNotifier.Destroy;
begin
  FSynchronizer.BeginWrite;
  try
    FreeAndNil(FListeners);
  finally
    FSynchronizer.EndWrite;
    FreeAndNil(FSynchronizer);
  end;
  inherited Destroy;
end;

procedure TJclBaseNotifier.Add(const listener: IListener);
begin
  FSynchronizer.BeginWrite;
  try
    if FListeners.IndexOf(listener) < 0 then
      FListeners.Add(listener);
  finally
    FSynchronizer.EndWrite;
  end;
end;

procedure TJclBaseNotifier.Notify(const msg: IInterface);
var
  idx: Integer;
begin
  FSynchronizer.BeginRead;
  try
    for idx := 0 to FListeners.Count - 1 do
      IListener(FListeners[idx]).Notification(msg);
  finally
    FSynchronizer.EndRead;
  end;
end;

procedure TJclBaseNotifier.Remove(const listener: IListener);
var
  idx: Integer;
begin
  FSynchronizer.BeginWrite;
  try
    idx := FListeners.IndexOf(listener);
    if idx >= 0 then
      FListeners.Delete(idx);
  finally
    FSynchronizer.EndWrite;
  end;
end;

{ TJclBaseListener }

procedure TJclBaseListener.Notification(const msg: IInterface);
begin
  // do nothing; descendants should override this method to process incoming notifications
end;

{ TJclBaseNotifierFaked }

procedure TJclBaseNotifierFaked.Add(const listener: IListener);
begin
  // do nothing;
end;

procedure TJclBaseNotifierFaked.Notify(const msg: IInterface);
begin
  // do nothing;
end;

procedure TJclBaseNotifierFaked.Remove(const listener: IListener);
begin
  // do nothing;
end;

end.


