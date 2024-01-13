unit uSpaceShipNavigator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform, CastleScene, CastleViewport, CastleCameras;

type
  TSpaceShipNavigator = class(TCastleWalkNavigation)
  private

  public
    constructor Create(AOwner: TComponent); override;
	end;

implementation

{ TSpaceShipNavigator }

constructor TSpaceShipNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Gravity := False;
  MoveSpeed := 10;
end;

end.

