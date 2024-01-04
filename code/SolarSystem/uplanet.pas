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
    FSpeed: Single;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    {
    Установка параметров планеты
    AOrbitalRadius - радиус орбиты планеты
    ASpeed - скорость вращения вокруг солнца
    }
    procedure SetParams(const AOrbitalRadius, ASpeed: Single);
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
  Self.Rotation := Vector4(Self.Rotation.XYZ, Self.Rotation.W + (FSpeed * SecondsPassed));
end;

procedure TPlanet.SetParams(const AOrbitalRadius, ASpeed: Single);
begin
  FSphere.Translation := Vector3(AOrbitalRadius, 0, 0);
  FSpeed := ASpeed;
end;

end.

