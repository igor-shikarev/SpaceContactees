unit uSolarSystem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform, CastleScene, CastleViewport,
  Generics.Collections, uSolar, uPlanet;

type
  TPlanetList = {$ifdef FPC}specialize{$endif} TObjectList<TPlanet>;

	{ TSolarSystem }

  TSolarSystem = class(TCastleTransform)
  private
    FSolar: TSolar;
    FPlanetList: TPlanetList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
	end;

implementation

uses
  Math;

{ TSolarSystem }

constructor TSolarSystem.Create(AOwner: TComponent);
var
  i: Integer;
  vPlanet: TPlanet;
begin
  inherited Create(AOwner);

  // создание солнца
  FSolar := TSolar.Create(Self);
  Self.Add(FSolar);
  FSolar.Translation := Vector3(0, 0, 0);

  // создание планет и начальное их расположение в пространтсве
  FPlanetList := TPlanetList.Create(True);
  for i := 1 to 9 do
  begin
    vPlanet := TPlanet.Create(Self);
    Self.Add(vPlanet);
    vPlanet.Translation := Vector3(0, 0, 0);
    vPlanet.SetOrbitRadius(i * 10);
    vPlanet.Rotation := Vector4(RandomRange(-100, 100) / 100, RandomRange(-100, 100) / 100, RandomRange(-100, 100) / 100, RandomRange(0, Ceil((1.9 * PI * 100))) / 100);

    FPlanetList.Add(vPlanet);
	end;
end;

destructor TSolarSystem.Destroy;
begin
  FreeAndNil(FPlanetList);
  inherited Destroy;
end;

procedure TSolarSystem.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
begin
  inherited Update(SecondsPassed, RemoveMe);

  FSolar.Update(SecondsPassed, RemoveMe);
end;

end.

