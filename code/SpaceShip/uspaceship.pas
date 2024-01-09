unit uSpaceShip;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform, CastleScene, CastleViewport;

type
  TSpaceShip = class(TCastleScene)
  private
    //FSolar: TCastleScene;
    FSpaceShip: TCastleSphere;
  public
    constructor Create(AOwner: TComponent); override;
	end;

implementation

{ TSpaceShip }

constructor TSpaceShip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSpaceShip := TCastleSphere.Create(Self);
  Self.Add(FSpaceShip);
  FSpaceShip.Translation := Vector3(0, 0, 0);
  FSpaceShip.Scale := Vector3(0.2, 0.2, 0.2);
end;

end.

