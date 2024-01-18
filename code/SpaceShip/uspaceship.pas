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
    FViewport: TCastleViewport;
  public
    constructor Create(AOwner: TComponent; AViewport: TCastleViewport); overload;
	end;

implementation

{ TSpaceShip }

constructor TSpaceShip.Create(AOwner: TComponent; AViewport: TCastleViewport);
begin
  inherited Create(AOwner);

  FViewport := AViewport;

  Self.Translation := Vector3(3, 1, 4);

  FSpaceShip := TCastleSphere.Create(Self);
  Self.Add(FSpaceShip);
  FSpaceShip.Translation := Vector3(0, 0, 0);
  FSpaceShip.Scale := Vector3(0.05, 0.05, 0.05);

  // перемещаем камеру как дочернюю
  FViewport.Camera.Parent.Remove(FViewport.Camera);
  Self.Add(FViewport.Camera);
  FViewport.Camera.Translation := Vector3(0, 0, 1);
  FViewport.Camera.Rotation := Vector4(0, 0, 0, 0);
  FViewport.Camera.Direction := Vector3(0, 0, -1);
end;

end.

