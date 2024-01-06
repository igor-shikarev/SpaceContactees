unit uSolar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform, CastleScene, CastleViewport;

type
  TSolar = class(TCastleTransform)
  private
    FSolar: TCastleScene;
    FLight: TCastlePointLight;
  public
    {
    ALightRadius - радиус света от солнца
    }
    constructor Create(AOwner: TComponent; const ALightRadius: Single); overload;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
	end;

implementation

uses
  CastleColors;

{ TSolar }

constructor TSolar.Create(AOwner: TComponent; const ALightRadius: Single);
begin
  inherited Create(AOwner);
  FSolar := TCastleScene.Create(Self);
  Self.Add(FSolar);
  FSolar.Translation := Vector3(0, 0, 0);
  FSolar.Scale := Vector3(0.5, 0.5, 0.5);
  FSolar.Url := 'castle-data:/models/star-sun/scene.gltf';
  FSolar.AnimateOnlyWhenVisible := True;
  FSolar.AutoAnimation := 'Take 001';
  FSolar.TimePlayingSpeed := 0.05;

  FLight := TCastlePointLight.Create(Self);
  Self.Add(FLight);
  FLight.Color := WhiteRGB;
  FLight.Intensity := 8;
  FLight.Attenuation := Vector3(0, 0, 1);
  FLight.Translation := Vector3(0, 0, 0);
  FLight.Radius := ALightRadius;
end;

procedure TSolar.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited Update(SecondsPassed, RemoveMe);
end;

end.

