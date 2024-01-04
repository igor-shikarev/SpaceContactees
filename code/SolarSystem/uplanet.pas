unit uPlanet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform, CastleScene, CastleViewport;

type

	{ TPlanet }

  TPlanet = class(TCastleTransform)
  private
    FPlanet: TCastleScene;
    FSphere: TCastleSphere;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    // установка радиуса орбиты
    procedure SetOrbitRadius(const AValue: Single);
	end;

implementation

uses
  CastleColors;

{ TPlanet }

constructor TPlanet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPlanet := TCastleScene.Create(Self);

  FSphere := TCastleSphere.Create(Self);
  FSphere.Color := Yellow;

  FPlanet.Add(FSphere);
  Self.Add(FPlanet);

  FPlanet.Translation := Vector3(0, 0, 0);
  FSphere.Translation := Vector3(0, 0, 0);
end;

procedure TPlanet.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited Update(SecondsPassed, RemoveMe);
  Self.Rotation := Vector4(Self.Rotation.XYZ, Self.Rotation.W + 0.001);
end;

procedure TPlanet.SetOrbitRadius(const AValue: Single);
begin
  FSphere.Translation := Vector3(AValue, 0, 0);
end;

end.

