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

const
  {
  Радиус орбит планет (ае)
  Меркурий  - 0.4
  Венера    - 0.7
  Земля     - 1
  Марс      - 1.6
  Юпитер    - 5.2
  Сатурн    - 10
  Уран      - 19.6
  Нептун    - 30
  Плутон    - 38.8
  }
  cOrbitalRadius: array[0..8] of Single = (0.4, 0.7, 1, 1.6, 5.2, 10, 19.6, 30, 38.8);
  {
  Угловая скорость планет (рад/сек)
  Земля вращается за 365дн., пусть день в игре равен 8ч, тогда скорость будет 0,0057рад/сек
  Меркурий  - 0.241
  Венера    - 0.615
  Земля     - 1
  Марс      - 1.88
  Юпитер    - 11.857
  Сатурн    - 29.4
  Уран      - 84.02
  Нептун    - 164.79
  Плутон    - 242.92
  }
  cSpeed: array[0..8] of Single = (0.241, 0.615, 1, 1.88, 11.857, 29.4, 84.02, 164.79, 242.92);
  // Астрономическая единица (ае)
  cAE = 100;
  // Угловая скорость условной Земли (рад/сек)
  cDefSpeed = 0.0057;


{ TSolarSystem }

constructor TSolarSystem.Create(AOwner: TComponent);
var
  i, j: Integer;
  vPlanet: TPlanet;
  vRot: TVector4;
  vFlag: Boolean;
begin
  inherited Create(AOwner);

  // создание солнца
  FSolar := TSolar.Create(Self);
  Self.Add(FSolar);
  FSolar.Translation := Vector3(0, 0, 0);

  // создание планет и начальное их расположение в пространстве
  FPlanetList := TPlanetList.Create(True);
  for i := Low(cOrbitalRadius) to High(cOrbitalRadius) do
  begin
    vPlanet := TPlanet.Create(Self);
    Self.Add(vPlanet);
    vPlanet.Translation := Vector3(0, 0, 0);
    vPlanet.SetParams(cOrbitalRadius[i] * cAE, SimpleRoundTo(cSpeed[i] * cDefSpeed, -4));

    // подбор уникального начального положения
    vFlag := True;
    while vFlag do
    begin
      vRot := Vector4(RandomRange(-100, 100) / 100, RandomRange(-100, 100) / 100, RandomRange(-100, 100) / 100, RandomRange(0, Ceil((1.9 * PI * 100))) / 100);
      if FPlanetList.Count > 0 then
      begin
        for j := 0 to FPlanetList.Count - 1 do
        begin
          vFlag := TVector4.Equals(vRot, FPlanetList[j].Rotation);
          if vFlag then
            Break;
        end;
        if vFlag then
          Continue;
			end;
      vPlanet.Rotation := vRot;
      Break;
		end;

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
end;

end.

