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
    FSpeed: Single;
  public
    {
    AOrbitalRadius - радиус орбиты планеты
    ASpeed - угловая скорость вращения вокруг солнца
    AModelUrl - URL модели
    }
    constructor Create(AOwner: TComponent; const AOrbitalRadius,
      ASpeed: Single; const AModelUrl: String); overload;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    property Planet: TCastleScene read FPlanet;
	end;

implementation

uses
  CastleColors;

{ TPlanet }

constructor TPlanet.Create(AOwner: TComponent; const AOrbitalRadius,
  ASpeed: Single; const AModelUrl: String);
begin
  inherited Create(AOwner);
  FPlanet := TCastleScene.Create(Self);
  Self.Add(FPlanet);
  FPlanet.Translation := Vector3(AOrbitalRadius, 0, 0);
  FSpeed := ASpeed;
  FPlanet.Url := AModelUrl;
  FPlanet.ReloadUrl;
end;

procedure TPlanet.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited Update(SecondsPassed, RemoveMe);
  Self.Rotation := Vector4(Self.Rotation.XYZ, Self.Rotation.W + (FSpeed * SecondsPassed));
end;

end.

