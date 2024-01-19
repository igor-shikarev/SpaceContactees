unit uSpaceShip;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform, CastleScene, CastleViewport;

type
  TSpaceShip = class(TCastleTransform)
  private
    //FSolar: TCastleScene;
    FSpaceShip: TCastleBox;
    FViewport: TCastleViewport;
  public
    constructor Create(AOwner: TComponent; AViewport: TCastleViewport); overload;
	end;

implementation

uses
  Math;

{ TSpaceShip }

constructor TSpaceShip.Create(AOwner: TComponent; AViewport: TCastleViewport);
begin
  inherited Create(AOwner);

  FViewport := AViewport;

  Self.Translation := Vector3(3, 1, -6);

  FSpaceShip := TCastleBox.Create(Self);
  Self.Add(FSpaceShip);
  FSpaceShip.Translation := Vector3(0, 0, 0);
  FSpaceShip.Scale := Vector3(1, 1, 3);
  FSpaceShip.Size := Vector3(0.2, 0.2, 0.2);
  FSpaceShip.Material := pmUnlit;

  // перемещаем камеру как дочернюю
  FViewport.Camera.Parent.Remove(FViewport.Camera);
  Self.Add(FViewport.Camera);
  FViewport.Camera.Translation := Vector3(0, 0, -1.27);
  FViewport.Camera.Rotation := Vector4(0, 1, 0, DegToRad(180));
  FViewport.Camera.Direction := Vector3(0, 0, 1);
end;

end.

