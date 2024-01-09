{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform, CastleScene, CastleViewport, CastleThirdPersonNavigation, CastleCameras,
  uSolarSystem, uSpaceShip;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    Viewport: TCastleViewport;
  private
    FDefCameraTranslation: TVector3;
    FDefCameraDirection: TVector3;
    FSolarSystem: TSolarSystem;
    FCameraPlanetIdx: Integer;
    FSpaceShip: TSpaceShip;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, CastleColors;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
var
  vNavigation: TCastleThirdPersonNavigation;
begin
  inherited;
  vNavigation := TCastleThirdPersonNavigation.Create(Self);
  Viewport.InsertBack(vNavigation);

  FDefCameraTranslation := Viewport.Camera.Translation;
  FDefCameraDirection := Viewport.Camera.Direction;
  FCameraPlanetIdx := 0;

  FSolarSystem := TSolarSystem.Create(Self);
  Viewport.Items.Add(FSolarSystem);
  FSolarSystem.Translation := Vector3(0, 0, 0);

  FSpaceShip := TSpaceShip.Create(Self);
  FSolarSystem.Add(FSpaceShip);
  vNavigation.Avatar := FSpaceShip;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyHome) then
  begin
    FCameraPlanetIdx := 0;
    Viewport.Camera.Parent.Remove(Viewport.Camera);
    Viewport.Items.Add(Viewport.Camera);
    Viewport.Camera.Translation := FDefCameraTranslation;
    Viewport.Camera.Direction := FDefCameraDirection;
    Exit(True); // key was handled
  end;

  if Event.IsKey(keyP) then
  begin
    if FCameraPlanetIdx > FSolarSystem.PlanetList.Count - 1 then
      FCameraPlanetIdx := 0;

    Viewport.Camera.Parent.Remove(Viewport.Camera);
    FSolarSystem.PlanetList[FCameraPlanetIdx].Add(Viewport.Camera);
    Viewport.Camera.Translation := FSolarSystem.PlanetList[FCameraPlanetIdx].Planet.Translation + Vector3(-10, 0, 0);
    Viewport.Camera.Direction := Viewport.Camera.Translation;
    Inc(FCameraPlanetIdx);
    Exit(True); // key was handled
  end;

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TViewMain.Press method should be used to handle keys
    not handled in children controls.
  }

  // Use this to handle keys:
  {
  if Event.IsKey(keyXxx) then
  begin
    // DoSomething;
    Exit(true); // key was handled
  end;
  }
end;

end.
