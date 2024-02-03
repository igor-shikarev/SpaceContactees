  { Navigation by walking or flying (classic first-person shooter navigation)
    in a 3D scene.
    User can rotate and move camera using various keys, like arrows or AWSD.
    Mouse dragging and mouse look are also supported. }
  TCastleWalkNavigation = class(TCastleMouseLookNavigation)
  strict private
    FRotationHorizontalSpeed, FRotationVerticalSpeed: Single;
    FRotationHorizontalPivot: Single;
    FPreferGravityUpForRotations: boolean;
    FPreferGravityUpForMoving: boolean;
    FIsAbove: boolean;
    FAboveHeight: Single;
    FAboveGround: PTriangle;
    FMouseDragMode: TMouseDragMode;
    FInput_Forward: TInputShortcut;
    FInput_Backward: TInputShortcut;
    FInput_RightRotate: TInputShortcut;
    FInput_LeftRotate: TInputShortcut;
    FInput_RightStrafe: TInputShortcut;
    FInput_LeftStrafe: TInputShortcut;
    FInput_UpRotate: TInputShortcut;
    FInput_DownRotate: TInputShortcut;
    FInput_IncreasePreferredHeight: TInputShortcut;
    FInput_DecreasePreferredHeight: TInputShortcut;
    FInput_GravityUp: TInputShortcut;
    FInput_MoveSpeedInc: TInputShortcut;
    FInput_MoveSpeedDec: TInputShortcut;
    FInput_Jump: TInputShortcut;
    FInput_Crouch: TInputShortcut;
    FInput_Run: TInputShortcut;

    FAllowSlowerRotations: boolean;
    FCheckModsDown: boolean;

    FMinAngleFromGravityUp: Single;

    { This is initally false. It's used by MoveHorizontal while head bobbing,
      to avoid updating HeadBobbingPosition more than once in the same Update call.

      Updating it more than once is bad --- try e.g. holding Input_Forward
      with one of the strafe keys: you move and it's very noticeable
      that HeadBobbing seems faster. That's because
      when holding both Input_Forward and Input_StrafeRight, you shouldn't
      do HeadBobbing twice in one Update --- you should do it only Sqrt(2).
      When you will also hold Input_RotateRight at the same time --- situation
      gets a little complicated...

      The good solution seems to just do head bobbing only once.
      In some special cases this means that head bobbing will be done
      *less often* than it should be, but this doesn't hurt. }
    HeadBobbingAlreadyDone: boolean;

    { MoveHorizontal call sets this to @true to indicate that some
      horizontal move was done. }
    MoveHorizontalDone: boolean;

    FMoveForward, FMoveBackward: boolean;
    FMoveHorizontalSpeed, FMoveVerticalSpeed, FMoveSpeed: Single;
    FMoveSpeedMin, FMoveSpeedMax: Single;
    FPreferredHeight: Single;
    FHeadBobbing: Single;
    FHeadBobbingTime: Single;
    FClimbHeight: Single;
    FCrouchHeight: Single;

    { React to Input_MoveSpeedInc. }
    procedure MoveSpeedInc(const SecondsPassed: Single);
    { React to Input_MoveSpeedDec. }
    procedure MoveSpeedDec(const SecondsPassed: Single);

    procedure RotateAroundGravityUp(const Angle: Single);
    procedure RotateAroundUp(const Angle: Single);
    procedure RotateHorizontal(const Angle: Single);
    procedure RotateVertical(AngleRad: Single);

    { Move horizontally.
      Dir is in camera parent coordinates, like Camera.Direction.
      It will be automatically adjusted to be parallel to gravity plane,
      if PreferGravityUpForMoving. }
    procedure MoveHorizontal(Dir: TVector3; const SecondsPassed: Single);

    { Up or down move, only when flying (ignored when @link(Gravity) is @true). }
    procedure MoveVertical(const SecondsPassed: Single; const Multiply: Integer);

    { Call always after horizontal rotation change.
      This will return new Position, applying effect of RotationHorizontalPivot.
      The OldPosition, OldDirection, NewDirection must be in world coordinates,
      so is the result. }
    function AdjustPositionForRotationHorizontalPivot(
      const OldPosition: TVector3;
      const OldDirection, NewDirection: TVector3): TVector3;

    { Jump.

      Returns if a jump was actually done. For example, you cannot
      jump when there's no gravity, or you're already in the middle
      of the jump. Can be useful to determine if key was handled and such. }
    function Jump: boolean;

    { Camera.GravityUp expressed in camera parent coordinate system. }
    function GravityUpLocal: TVector3;

    { Direction to strafe left.
      In camera parent coordinate space, just like Camera.Direction.
      This is not adjusted using PreferGravityUpForMoving/Rotations. }
    function DirectionLeft: TVector3;

    { Direction to strafe right.
      In camera parent coordinate space, just like Camera.Direction.
      This is not adjusted using PreferGravityUpForMoving/Rotations. }
    function DirectionRight: TVector3;
  private
    { Private things related to gravity ---------------------------- }

    FFalling: boolean;
    FFallingStartPosition: TVector3;
    FFallSpeedStart: Single;
    FFallSpeed: Single;
    FFallSpeedIncrease: Single;
    FGravity: boolean;
    FGrowSpeed: Single;
    { This is used by FallingEffect to temporary modify Matrix result
      by rotating Up around Direction. In degress. }
    Fde_UpRotate: Single;
    { This is used by FallingEffect to consistently rotate us.
      This is either -1, 0 or +1. }
    Fde_RotateHorizontal: Integer;
    FFallingEffect: boolean;

    FJumpMaxHeight: Single;
    FIsJumping: boolean;
    FJumpHeight: Single;
    FJumpTime: Single;
    FJumpHorizontalSpeedMultiply: Single;

    HeadBobbingPosition: Single;
    function UseHeadBobbing: boolean;
    class procedure CreateComponentFly(Sender: TObject);
  private
    FIsCrouching: boolean;

    FFallingOnTheGround: boolean;
    FFallingOnTheGroundAngleIncrease: boolean;

    FIsOnTheGround: boolean;
    FIsWalkingOnTheGround: boolean;

    FMouseDraggingHorizontalRotationSpeed, FMouseDraggingVerticalRotationSpeed: Single;
    FMouseDraggingMoveSpeed: Single;

    function RealPreferredHeightNoHeadBobbing: Single;
    function RealPreferredHeightMargin: Single;
  protected
    function ReallyEnableMouseDragging: boolean; override;
    procedure ProcessMouseLookDelta(const Delta: TVector2); override;
    function MoveAllowed(
      const OldPos: TVector3; ProposedNewPos: TVector3; out NewPos: TVector3;
      const BecauseOfGravity, CheckClimbHeight: Boolean): Boolean; override;
  public
    const
      DefaultFallSpeedStart = 0.5;
      DefaultGrowSpeed = 1.0;
      DefaultJumpMaxHeight = 1.0;
      DefaultMinAngleFromGravityUp = Pi * 10 / 180;
      DefaultRotationHorizontalSpeed = Pi * 150 / 180;
      DefaultRotationVerticalSpeed = Pi * 100 / 180;
      DefaultFallSpeedIncrease = 13/12;
      DefaultJumpHorizontalSpeedMultiply = 2.0;
      DefaultJumpTime = 1.0 / 8.0;
      DefaultMouseDraggingHorizontalRotationSpeed = Pi * 0.1 / 180;
      DefaultMouseDraggingVerticalRotationSpeed = Pi * 0.1 / 180;
      DefaultMouseDraggingMoveSpeed = 0.01;
      DefaultMoveSpeedMin = 0.01;
      DefaultMoveSpeedMax = 10000.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function AllowSuspendForInput: boolean; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function SensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean; override;
    function SensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean; override;

    { If PreferGravityUpForRotations or PreferGravityUpForMoving
      then various operations are done with respect
      to GravityUp, otherwise they are done with
      respect to current @link(TCastleTransform.Up Camera.Up).

      With PreferGravityUpForRotations, this affects rotations:
      horizontal rotations (Input_LeftRotate and Input_RightRotate)
      and rotations caused by MouseLook.
      Also vertical rotations are bounded by MinAngleFromGravityUp
      when PreferGravityUpForRotations.

      Note that you can change it freely at runtime,
      and when you set PreferGravityUpForRotations from @false to @true
      then in nearest Update
      the @link(TCastleTransform.Up Camera.Up) will be gradually fixed,
      so that @link(TCastleTransform.Direction Camera.Direction) and @link(TCastleTransform.Up Camera.Up)
      and GravityUp are on the same plane. Also @link(TCastleTransform.Direction Camera.Direction) may be adjusted
      to honour MinAngleFromGravityUp.

      With PreferGravityUpForMoving, this affects moving:
      horizontal moving (forward, backward, strafe),
      and vertical moving (Input_Jump and Input_Crouch when @link(Gravity) is @false).
      E.g. when PreferGravityUpForMoving then forward/backward keys are tied
      to horizontal plane defined by GravityUp.
      When not PreferGravityUpForMoving then forward/backward try to move
      you just in the @link(TCastleTransform.Direction Camera.Direction). Which is usually more handy when
      e.g. simulating flying.

      @unorderedList(
        @item(
          When there is no "natural" up-or-down feeling in the scene,
          e.g. outer space environment without any gravity,
          then you @bold(may) set
          PreferGravityUpForRotations as @false and you @bold(should)
          leave PreferGravityUpForMoving and @link(Gravity) to @false.
        )

        @item(
          With PreferGravityUpForRotations the "feeling" of GravityUp
          is stronger. Raising/bowing the head doesn't mess with "the general
          sense that there's some vertical axis independent of my movement,
          that doesn't change, and affects how I move".

          Without PreferGravityUpForRotations, we quickly start to do rotations
          in an awkward way --- once you do some vertical rotation,
          you changed @link(TCastleTransform.Up Camera.Up), and next horizontal rotation will be
          done versus new @link(TCastleTransform.Up Camera.Up).

          If your GravityUp is good, then you generally should
          leave PreferGravityUpForRotations to @true. Unless you really @bold(want)
          the player to feel movements as "awkward", e.g. when you
          want to simulate this "outer space without any gravity" feeling.)

        @item(
          If your GravityUp is good, then you generally should set
          PreferGravityUpForMoving just like Gravity.

          E.g. when the player is flying / swimming etc. he will probably prefer
          PreferGravityUpForMoving = @false, because this way he will not have to
          press Input_Jump and Input_Crouch. Simply pressing Input_Forward
          and Input_Backward and doing rotations will be enough to move
          freely in 3D space.

          When gravity works, PreferGravityUpForMoving = @true is better,
          otherwise player would unnecessarily try to jump when looking up.)
      )

      @groupBegin }
    property PreferGravityUpForRotations: boolean
      read FPreferGravityUpForRotations write FPreferGravityUpForRotations default true;

    property PreferGravityUpForMoving: boolean
      read FPreferGravityUpForMoving write FPreferGravityUpForMoving default true;
    { @groupEnd }

    { Set the most important properties of this navigation, in one call.
      Sets camera properties (Translation, Direction, Up).

      Given here AInitialDirection, AInitialUp, AGravityUp will be normalized,
      and AInitialUp will be adjusted to be orthogonal to AInitialDirection
      (see @link(TCastleTransform.SetWorldView Camera.SetWorldView)).

      Sets also PreferredHeight and Radius.
      PreferredHeight may be adjusted to be sensible
      (by calling CorrectPreferredHeight(ARadius)).
      You can pass ARadius = 0.0 if you really don't want this
      PreferredHeight adjustment. }
    procedure Init(const AInitialPosition, AInitialDirection,
      AInitialUp: TVector3;
      const AGravityUp: TVector3;
      const APreferredHeight: Single;
      const ARadius: Single); overload;
      deprecated 'use Viewport.Camera.SetWorldView, and set GravityUp, PreferredHeight, Radius and call CorrectPreferredHeight manually';

    { Alternative Init that sets camera properties such that
      an object inside Box is more or less "visible good".
      Sets InitialCameraXxx properties to make it look right,
      sets current CameraXxx properties to InitialCameraXxx.
      Sets GravityUp to the same thing as InitialUp.
      Sets also PreferredHeight to make it behave "sensibly". }
    procedure Init(const box: TBox3D; const ARadius: Single); overload;
      deprecated 'use Viewport.Camera.SetWorldView, and set GravityUp, PreferredHeight, Radius and call CorrectPreferredHeight manually';

    { This sets the minimal angle (in radians) between GravityUp
      and @link(TCastleTransform.Direction Camera.Direction), and also between -GravityUp and
      @link(TCastleTransform.Direction Camera.Direction).
      This way vertical rotations (like Input_UpRotate,
      Input_DownRotate) are "bounded" to not allow player to do something
      strange, i.e. bow your head too much and raise your head too much.

      This is used only when PreferGravityUpForRotations
      is @true and when it's <> 0.0.

      This must be always between 0 and Pi/2. Value of Pi/2 will effectively
      disallow vertical rotations (although you should rather do this in
      a "cleaner way" by calling MakeClear on Input_UpRotate and Input_DownRotate). }
    property MinAngleFromGravityUp: Single
      read FMinAngleFromGravityUp write FMinAngleFromGravityUp
      {$ifdef FPC}default DefaultMinAngleFromGravityUp{$endif};

    function DirectionInGravityPlane: TVector3; deprecated 'avoid using it, as it inherently has difficult cases: it is in TCastleCamera local coordinate space, it cannot be correct when Direction is parallel to gravity';

    function Motion(const Event: TInputMotion): boolean; override;

    { Initial speed of falling down.
      Of course this is used only when @link(Gravity) is true.

      Note that while falling down,
      the camera will actually fall with greater and greated speed
      (this adds more realism to the gravity effect...).
      Note that this is always relative to @link(TCastleTransform.Direction Camera.Direction) length.
      @link(TCastleTransform.Direction Camera.Direction) determines moving speed --- and so it determines
      also falling speed. The default DefaultFallSpeedStart
      is chosen to be something sensible, to usually get nice effect
      of falling.

      You can change it at any time, but note that if you change this
      while Falling is @true, then you will not change the
      "current falling down speed". You will change only the falling down
      speed used the next time. }
    property FallSpeedStart: Single
      read FFallSpeedStart write FFallSpeedStart
      {$ifdef FPC}default DefaultFallSpeedStart{$endif};

    { When falling down, the speed increases.
      Set this to 1.0 to fall down with constant speed
      (taken from FallSpeedStart). }
    property FallSpeedIncrease: Single
      read FFallSpeedIncrease write FFallSpeedIncrease
      {$ifdef FPC}default DefaultFallSpeedIncrease{$endif};

    { Are we currently falling down because of gravity. }
    property Falling: boolean read FFalling write FFalling;

    { If Falling, then this will force Falling to false
      @bold(without calling OnFallenDown). It's much like forcing
      the opinion that "camera is not falling down right now".

      Note that if we will find out (e.g. in nearest @link(Update))
      that camera is still too high above the ground,
      then we will start falling down again, setting @link(Falling)
      back to true. (but then we will start falling down from the beginning
      with initial falling down speed).

      This is useful to call if you just changed @link(TCastleTransform.Translation Camera.Translation) because
      e.g. the player teleported somewhere (or e.g. game levels changed).
      In this case you just want to forget the fact that camera
      was falling down --- no consequences (like lowering player's
      health, fadeout etc.). }
    procedure CancelFalling;

    { Make a nice dizzying camera effect when falling down.
      This adds temporary camera rotations simulating that you
      rotate randomly and helplessly when falling down.

      Of course this is meaningfull only when @link(Gravity) works.

      Note that changing it from @true to @false doesn't immediately
      "cancel out" this effect if it's currently in progress.
      It only prevents this effect from starting again. }
    property FallingEffect: boolean
      read FFallingEffect write FFallingEffect default true;

    { When @link(Gravity) works and camera height above the ground
      is less than PreferredHeight, then we try to "grow",
      i.e. camera position increases along the GravityUp
      so that camera height above the ground is closer to
      PreferredHeight. This property (together with length of
      @link(TCastleTransform.Direction Camera.Direction), that always determines every moving speed)
      determines the speed of this growth. }
    property GrowSpeed: Single
      read FGrowSpeed write FGrowSpeed
      {$ifdef FPC}default DefaultGrowSpeed{$endif};

    { How high can you jump ?
      The max jump distance is calculated as
      JumpMaxHeight * PreferredHeight, see MaxJumpDistance. }
    property JumpMaxHeight: Single
      read FJumpMaxHeight write FJumpMaxHeight
      {$ifdef FPC}default DefaultJumpMaxHeight{$endif};

    { Returns just JumpMaxHeight * PreferredHeight,
      see JumpMaxHeight for explanation. }
    function MaxJumpDistance: Single;

    { We are in the middle of a "jump" move right now. }
    property IsJumping: boolean read FIsJumping;

    { Scales the speed of horizontal moving during jump. }
    property JumpHorizontalSpeedMultiply: Single
      read FJumpHorizontalSpeedMultiply write FJumpHorizontalSpeedMultiply
      {$ifdef FPC}default DefaultJumpHorizontalSpeedMultiply{$endif};

    { How fast do you jump up. This is the time, in seconds, in takes
      to reach MaxJumpDistance height when jumping. }
    property JumpTime: Single read FJumpTime write FJumpTime
      {$ifdef FPC}default DefaultJumpTime{$endif};

    { Is player crouching right now. }
    property IsCrouching: boolean read FIsCrouching;

    { The PreferredHeight slightly modified by head bobbing
      and crouch. It can be useful for collision detection
      between camera and something. }
    function RealPreferredHeight: Single;

    { This makes a visual effect of camera falling down horizontally
      on the ground. Nice to use when player died, and you want to show
      that it's body falled on the ground.
      This works by gradually changing @link(TCastleTransform.Up Camera.Up) such that
      it gets orthogonal to GravityUp. }
    procedure FallOnTheGround;

    { @true when the effect caused by FallOnTheGround is stil in motion. }
    property FallingOnTheGround: boolean read FFallingOnTheGround;

    { This is @true when gravity works (that is @link(Gravity) is @true),
      and player is standing stable on the ground. This is set in every Update.

      You can use this e.g. to make some effects when player is on some
      special ground (standing or walking), e.g. hurt player when he's
      standing on some toxical ground.

      @seealso IsWalkingOnTheGround }
    property IsOnTheGround: boolean read FIsOnTheGround;

    { This is @true when gravity works (that is @link(Gravity) is @true),
      and player is standing stable on the ground, and player is moving
      horizontally. In other words, this is like "IsOnTheGround and (s)he's
      walking". This is set in every Update.

      The intention is that you can use this to make
      some "footsteps" sound for the player. }
    property IsWalkingOnTheGround: boolean read FIsWalkingOnTheGround;

    { Change up vector, keeping the direction unchanged.
      If necessary, the up vector provided here will be fixed to be orthogonal
      to direction.
      See TCastleTransform.UpPrefer for detailed documentation what this does. }
    procedure UpPrefer(const AUp: TVector3); deprecated 'use Viewport.Camera.UpPrefer';

    { Last known information about whether camera is over the ground.

      These are updated continuously only when @link(Gravity) is @true.

      We do not (and, currently, cannot) track here if
      AboveGround pointer will be eventually released (which may happen
      if you release your 3D scene, or rebuild scene causing octree rebuild).
      This is not a problem for navigation class, since we do not use this
      pointer for anything. But if you use this pointer,
      then you may want to take care to eventually set it to @nil when
      your octree or such is released.

      AboveHeight is in world coordinates (not camera coordinates).

      @groupBegin }
    property IsAbove: boolean read FIsAbove;
    property AboveHeight: Single read FAboveHeight;
    property AboveGround: PTriangle read FAboveGround write FAboveGround;
    { @groupEnd }

    { TODO: Input_Xxx not published. See TCastleExamineNavigation Input_Xxx notes
      for reasoning. }
    { }
    property Input_Forward: TInputShortcut read FInput_Forward;
    property Input_Backward: TInputShortcut read FInput_Backward;
    property Input_LeftRotate: TInputShortcut read FInput_LeftRotate;
    property Input_RightRotate: TInputShortcut read FInput_RightRotate;
    {$ifdef FPC}
    property Input_LeftRot: TInputShortcut read FInput_LeftRotate; deprecated 'use Input_LeftRotate';
    property Input_RightRot: TInputShortcut read FInput_RightRotate; deprecated 'use Input_RightRotate';
    {$endif}
    property Input_LeftStrafe: TInputShortcut read FInput_LeftStrafe;
    property Input_RightStrafe: TInputShortcut read FInput_RightStrafe;
    property Input_UpRotate: TInputShortcut read FInput_UpRotate;
    property Input_DownRotate: TInputShortcut read FInput_DownRotate;
    property Input_IncreasePreferredHeight: TInputShortcut read FInput_IncreasePreferredHeight;
    property Input_DecreasePreferredHeight: TInputShortcut read FInput_DecreasePreferredHeight;
    property Input_GravityUp: TInputShortcut read FInput_GravityUp;
    property Input_Run: TInputShortcut read FInput_Run;

    { Change the MoveSpeed.
      @groupBegin }
    property Input_MoveSpeedInc: TInputShortcut read FInput_MoveSpeedInc;
    property Input_MoveSpeedDec: TInputShortcut read FInput_MoveSpeedDec;
    { @groupEnd }

    { Jumping and crouching (when @link(Gravity) = @true) or flying up / down
      (when @link(Gravity) = @false).
      @groupBegin }
    property Input_Jump: TInputShortcut read FInput_Jump;
    property Input_Crouch: TInputShortcut read FInput_Crouch;
    { @groupEnd }

    { Move forward, just like Input_Forward would be pressed. }
    property MoveForward: boolean read FMoveForward write FMoveForward;
    { Move backward, just like Input_Backward would be pressed. }
    property MoveBackward: boolean read FMoveBackward write FMoveBackward;

    { If @true then all rotation keys
      (Input_RightRotate, Input_LeftRotate, Input_UpRotate, Input_DownRotate)
      will work 10x slower when Ctrl modified is pressed. }
    property AllowSlowerRotations: boolean
      read FAllowSlowerRotations write FAllowSlowerRotations
      default true;

    { @abstract(Do we check what key modifiers are pressed and do something
      differently based on it?)

      If @true then all keys work only when no modifiers or only shift are
      pressed. Additionally when Ctrl is pressed (and AllowSlowerRotations) then
      rotation keys work 10x slower. Also Increase/DecreasePreferredHeight
      work only when Ctrl pressed.
      Other keys with other modifiers
      don't work. We allow shift, because to press character "+" on non-numpad
      keyboard (useful on laptops, where numpad is difficult) you
      probably need to press shift.

      If @false then all keys work as usual, no matter what
      modifiers are pressed. And rotation keys never work 10x slower
      (AllowSlowerRotations is ignored),
      also Increase/DecreasePreferredHeight are ignored. }
    property CheckModsDown: boolean
      read FCheckModsDown write FCheckModsDown
      default true;

    { Horizontal rotation can rotate around a vector that is RotationHorizontalPivot units
      forward before the camera. This is a poor-mans way to implement some 3rd camera game.
      Note that when non-zero this may (for now) move the camera without actually checking
      OnMoveAllowed. }
    property RotationHorizontalPivot: Single
      read FRotationHorizontalPivot write FRotationHorizontalPivot
      {$ifdef FPC}default 0{$endif};
      {$ifdef FPC}deprecated 'use TCastleThirdPersonNavigation for real 3rd-person navigation';{$endif}

    { Correct PreferredHeight based on @link(Radius)
      and on current @link(HeadBobbing).

      Exactly what and why is done: if you do any kind of collision
      detection with some Radius, then
      you should make sure that RealPreferredHeight is always >= of your
      Radius, otherwise strange effects may happen when crouching
      or when head bobbing forces camera to go down.

      Exactly, the required equation is

      @preformatted(
        MinimumRealPreferredHeight :=
          PreferredHeight * CrouchHeight * (1 - HeadBobbing);
      )

      and always must be

      @preformatted(
        MinimumRealPreferredHeight >= RealPreferredHeight
      )

      Reasoning: otherwise this class would "want camera to fall down"
      (because we will always be higher than RealPreferredHeight)
      but your OnMoveAllowed would not allow it (because Radius
      would not allow it).

      This method will make sure that this condition
      holds by eventually adjusting (making larger) PreferredHeight.
      Note that for Radius = 0.0 this will always leave
      PreferredHeight as it is. }
    procedure CorrectPreferredHeight;

    { We may make a "head bobbing" effect,
      by moving the camera a bit up and down.

      This property mutiplied by PreferredHeight
      says how much head bobbing can move you along GravityUp.
      Set this to 0 to disable head bobbing.
      This must always be < 1.0. For sensible effects, this should
      be rather close to 0.0, for example 0.02.

      This is meaningfull only when @link(TCastleWalkNavigation.Gravity) works. }
    property HeadBobbing: Single
      read FHeadBobbing write FHeadBobbing {$ifdef FPC}default DefaultHeadBobbing{$endif};

    { Controls head bobbing frequency. In the time of HeadBobbingTime seconds,
      we do full head bobbing sequence (camera swing up, then down again).

      Note that if you do a footsteps sound in your game (see
      stPlayerFootstepsDefault or TMaterialProperty.FootstepsSound)
      then you will want this property to match your footsteps sound length,
      things feel and sound natural then.
      Also, often it sounds better to record two footsteps inside
      a single sound file, in which case the footstep sound length should be twice
      as long as this property. For example, record 2 steps inside a 1-second long
      footstep sound, and set this property to 0.5 a second (which is a default
      in fact). }
    property HeadBobbingTime: Single
      read FHeadBobbingTime write FHeadBobbingTime
      {$ifdef FPC}default DefaultHeadBobbingTime{$endif};

    { The tallest height that you can climb only used
      when @link(TCastleWalkNavigation.Gravity) is @true.
      This is checked in each single horizontal move when @link(TCastleWalkNavigation.Gravity) works.
      Must be >= 0. Value 0 means there is no limit (and makes a small speedup).

      This is reliable to prevent user from climbing stairs and such,
      when vertical walls are really vertical (not just steep-almost-vertical).

      It's not 100% reliable to prevent player from climbing steep hills.
      That's because, depending on how often an event processing occurs,
      you actually climb using less or more steps.
      So even a very steep hill can be always
      climbed on a computer with very fast speed, because with large FPS you
      effectively climb it using a lot of very small steps (assuming that
      FPS limit is not enabled, that is CastleWindow.TCastleApplication.LimitFPS
      or CastleControl.LimitFPS is zero).

      Remember that user can still try jumping to climb on high obstactes.
      See @link(TCastleWalkNavigation.JumpMaxHeight) for a way to control jumping.

      For a 100% reliable way to prevent user from reaching some point,
      that does not rely on specific navigation settings,
      you should build actual walls in 3D (invisible walls
      can be created by Collision.proxy in VRML/X3D). }
    property ClimbHeight: Single read FClimbHeight write FClimbHeight;
  published
    property MouseLook;
    property MouseLookHorizontalSensitivity;
    property MouseLookVerticalSensitivity;
    property InvertVerticalMouseLook;

    { Rotation keys speed, in radians per second.
      @groupBegin }
    property RotationHorizontalSpeed: Single
      read FRotationHorizontalSpeed write FRotationHorizontalSpeed
      {$ifdef FPC}default DefaultRotationHorizontalSpeed{$endif};

    property RotationVerticalSpeed: Single
      read FRotationVerticalSpeed write FRotationVerticalSpeed
      {$ifdef FPC}default DefaultRotationVerticalSpeed{$endif};
    { @groupEnd }

    { Moving speeds.
      MoveHorizontalSpeed is only for horizontal movement,
      MoveVerticalSpeed is only for vertical, and MoveSpeed simply affects
      both types of movement. Effectively, we always scale the speed
      of movement by either @code(MoveHorizontalSpeed * MoveSpeed) or
      @code(MoveVerticalSpeed * MoveSpeed).

      We move by distance @code(MoveSpeed * MoveHorizontalSpeed (or MoveVerticalSpeed))
      during one second.
      So if you leave MoveHorizontalSpeed = MoveVerticalSpeed = 1 (as default),
      MoveSpeed expresses the speed in nice units / per second.

      Default values for all these speed properties is 1.0,
      so you simply move by 1 unit per second.

      @groupBegin }
    property MoveHorizontalSpeed: Single
      read FMoveHorizontalSpeed write FMoveHorizontalSpeed {$ifdef FPC}default 1.0{$endif};
    property MoveVerticalSpeed: Single
      read FMoveVerticalSpeed write FMoveVerticalSpeed {$ifdef FPC}default 1.0{$endif};
    property MoveSpeed: Single read FMoveSpeed write FMoveSpeed {$ifdef FPC}default 1.0{$endif};
    { @groupEnd }

    { Minimum and maximum values for possible @link(MoveSpeed) that user can make,
      using Input_MoveSpeedInc, Input_MoveSpeedInc.
      Note that code can still set @link(MoveSpeed) to any value, however small or large,
      these limits only apply to user changing speed by Input_MoveSpeedInc, Input_MoveSpeedInc.
      @groupBegin }
    property MoveSpeedMin: Single read FMoveSpeedMin write FMoveSpeedMin {$ifdef FPC}default DefaultMoveSpeedMin{$endif};
    property MoveSpeedMax: Single read FMoveSpeedMax write FMoveSpeedMax {$ifdef FPC}default DefaultMoveSpeedMax{$endif};
    { @groupEnd }

    { Speed (radians per pixel delta) of rotations by mouse dragging.
      Relevant only if niMouseDragging in @link(Input), and MouseDragMode is mdRotate or mdWalk.
      Separate for horizontal and vertical, this way you can e.g. limit
      (or disable) vertical rotations, useful for games where you mostly
      look horizontally and accidentally looking up/down is more confusing
      than useful.
      @groupBegin }
    property MouseDraggingHorizontalRotationSpeed: Single
      read FMouseDraggingHorizontalRotationSpeed write FMouseDraggingHorizontalRotationSpeed
      {$ifdef FPC}default DefaultMouseDraggingHorizontalRotationSpeed{$endif};
    property MouseDraggingVerticalRotationSpeed: Single
      read FMouseDraggingVerticalRotationSpeed write FMouseDraggingVerticalRotationSpeed
      {$ifdef FPC}default DefaultMouseDraggingVerticalRotationSpeed{$endif};
    { @groupEnd }

    { Moving speed when mouse dragging.
      Relevant only when @code((MouseDragMode is mdWalk) and (niMouseDragging in UsingInput)). }
    property MouseDraggingMoveSpeed: Single
      read FMouseDraggingMoveSpeed write FMouseDraggingMoveSpeed
      {$ifdef FPC}default DefaultMouseDraggingMoveSpeed{$endif};

    { What mouse dragging does. Used only when niMouseDragging in @link(Input). }
    property MouseDragMode: TMouseDragMode
      read FMouseDragMode write FMouseDragMode default mdWalk;

    { This unlocks a couple of features and automatic behaviors
      related to gravity. Gravity always drags the camera down to
      -GravityUp.

      Summary of things done by gravity:
      @unorderedList(
        @item(It performs collision detection with parent TCastleViewport to get camera height above the ground.)
        @item(It allows player to jump. See Input_Jump, IsJumping, JumpMaxHeight,
          JumpHorizontalSpeedMultiply.)
        @item(It allows player to crouch. See Input_Crouch, CrouchHeight.)
        @item(It tries to keep @link(TCastleTransform.Translation Camera.Translation) above the ground on
          PreferredHeight height.)
        @item(When current height is too small --- @link(TCastleTransform.Translation Camera.Translation) is moved up.
          See GrowSpeed.)
        @item(When current height is too large --- we're falling down.
          See Falling, OnFall, FallSpeedStart,
          FallSpeedIncrease, FallingEffect.)
        @item(It does head bobbing. See HeadBobbing, HeadBobbingTime.)
      )

      While there are many properties allowing you to control
      gravity behavior, most of them have initial values that should be
      sensible in all cases. The most important property you need to set yourself is PreferredHeight.
      Everything else should basically work auto-magically.

      Note that Gravity setting is independent from
      PreferGravityUpForRotations or PreferGravityUpForMoving settings ---
      PreferGravityUpXxx say how the player controls work,
      Gravity says what happens to player due to ... well, due to gravity. }
    property Gravity: boolean
      read FGravity write FGravity default true;

    { Height above the ground, only used when @link(TCastleWalkNavigation.Gravity) is @true.
      The @link(TCastleTransform.Translation Camera.Translation) tries to stay PreferredHeight above the ground.
      Temporarily it may still be lower (e.g. player can
      shortly "duck" when he falls from high).

      This must always be >= 0.
      You should set this to something greater than zero to get sensible
      behavior of some things related to @link(TCastleWalkNavigation.Gravity).

      See CorrectPreferredHeight for important property
      of PreferredHeight that you should keep. }
    property PreferredHeight: Single
      read FPreferredHeight write FPreferredHeight {$ifdef FPC}default DefaultPreferredHeight{$endif};

    { Preferred height when crouching.
      This is always mutiplied to PreferredHeight.
      This should always be <= 1 (CrouchHeight = 1 effectively disables
      crouching, although it's better to do this by calling MakeClear
      on Input_Crouch). }
    property CrouchHeight: Single
      read FCrouchHeight write FCrouchHeight {$ifdef FPC}default DefaultCrouchHeight{$endif};

    property Radius;
  end;
  
  
{ TCastleWalkNavigation ---------------------------------------------------------------- }

constructor TCastleWalkNavigation.Create(AOwner: TComponent);
begin
  inherited;

  FRotationHorizontalSpeed := DefaultRotationHorizontalSpeed;
  FRotationVerticalSpeed := DefaultRotationVerticalSpeed;
  FFallSpeedStart := DefaultFallSpeedStart;
  FFallSpeedIncrease := DefaultFallSpeedIncrease;
  FPreferGravityUpForRotations := true;
  FPreferGravityUpForMoving := true;
  FGravity := true;
  FGrowSpeed := DefaultGrowSpeed;
  FFallingEffect := true;
  FIsJumping := false;
  FJumpMaxHeight := DefaultJumpMaxHeight;
  FMinAngleFromGravityUp := DefaultMinAngleFromGravityUp;
  FAllowSlowerRotations := true;
  FCheckModsDown := true;
  FJumpHorizontalSpeedMultiply := DefaultJumpHorizontalSpeedMultiply;
  FJumpTime := DefaultJumpTime;
  FMouseDraggingHorizontalRotationSpeed := DefaultMouseDraggingHorizontalRotationSpeed;
  FMouseDraggingVerticalRotationSpeed := DefaultMouseDraggingVerticalRotationSpeed;
  FMouseDraggingMoveSpeed := DefaultMouseDraggingMoveSpeed;
  FMoveHorizontalSpeed := 1;
  FMoveVerticalSpeed := 1;
  FMoveSpeed := 1;
  FMoveSpeedMin := DefaultMoveSpeedMin;
  FMoveSpeedMax := DefaultMoveSpeedMax;
  FPreferredHeight := DefaultPreferredHeight;
  FHeadBobbing := DefaultHeadBobbing;
  FHeadBobbingTime := DefaultHeadBobbingTime;
  FCrouchHeight := DefaultCrouchHeight;

  FInput_Forward                 := TInputShortcut.Create(Self);
  FInput_Backward                := TInputShortcut.Create(Self);
  FInput_LeftRotate              := TInputShortcut.Create(Self);
  FInput_RightRotate             := TInputShortcut.Create(Self);
  FInput_LeftStrafe              := TInputShortcut.Create(Self);
  FInput_RightStrafe             := TInputShortcut.Create(Self);
  FInput_UpRotate                := TInputShortcut.Create(Self);
  FInput_DownRotate              := TInputShortcut.Create(Self);
  FInput_IncreasePreferredHeight := TInputShortcut.Create(Self);
  FInput_DecreasePreferredHeight := TInputShortcut.Create(Self);
  FInput_GravityUp               := TInputShortcut.Create(Self);
  FInput_MoveSpeedInc            := TInputShortcut.Create(Self);
  FInput_MoveSpeedDec            := TInputShortcut.Create(Self);
  FInput_Jump                    := TInputShortcut.Create(Self);
  FInput_Crouch                  := TInputShortcut.Create(Self);
  FInput_Run                     := TInputShortcut.Create(Self);

  Input_Forward                 .Assign(keyW, keyArrowUp);
  Input_Backward                .Assign(keyS, keyArrowDown);
  Input_LeftRotate              .Assign(keyArrowLeft);
  Input_RightRotate             .Assign(keyArrowRight);
  Input_LeftStrafe              .Assign(keyA);
  Input_RightStrafe             .Assign(keyD);
  Input_UpRotate                .Assign(keyNone);
  Input_DownRotate              .Assign(keyNone);
  Input_IncreasePreferredHeight .Assign(keyNone);
  Input_DecreasePreferredHeight .Assign(keyNone);
  Input_GravityUp               .Assign(keyNone);
  { For move speed we use also character codes +/-, as numpad
    may be hard to reach on some keyboards (e.g. on laptops). }
  Input_MoveSpeedInc            .Assign(keyNumpadPlus , keyNone, '+');
  Input_MoveSpeedDec            .Assign(keyNumpadMinus, keyNone, '-');
  Input_Jump                    .Assign(keySpace);
  Input_Crouch                  .Assign(keyC);
  Input_Run                     .Assign(keyShift);

  Input_Forward                .SetSubComponent(true);
  Input_Backward               .SetSubComponent(true);
  Input_LeftRotate             .SetSubComponent(true);
  Input_RightRotate            .SetSubComponent(true);
  Input_LeftStrafe             .SetSubComponent(true);
  Input_RightStrafe            .SetSubComponent(true);
  Input_UpRotate               .SetSubComponent(true);
  Input_DownRotate             .SetSubComponent(true);
  Input_IncreasePreferredHeight.SetSubComponent(true);
  Input_DecreasePreferredHeight.SetSubComponent(true);
  Input_GravityUp              .SetSubComponent(true);
  Input_MoveSpeedInc           .SetSubComponent(true);
  Input_MoveSpeedDec           .SetSubComponent(true);
  Input_Jump                   .SetSubComponent(true);
  Input_Crouch                 .SetSubComponent(true);
  Input_Run                    .SetSubComponent(true);

  Input_Forward                .Name := 'Input_Forward';
  Input_Backward               .Name := 'Input_Backward';
  Input_LeftRotate             .Name := 'Input_LeftRotate';
  Input_RightRotate            .Name := 'Input_RightRotate';
  Input_LeftStrafe             .Name := 'Input_LeftStrafe';
  Input_RightStrafe            .Name := 'Input_RightStrafe';
  Input_UpRotate               .Name := 'Input_UpRotate';
  Input_DownRotate             .Name := 'Input_DownRotate';
  Input_IncreasePreferredHeight.Name := 'Input_IncreasePreferredHeight';
  Input_DecreasePreferredHeight.Name := 'Input_DecreasePreferredHeight';
  Input_GravityUp              .Name := 'Input_GravityUp';
  Input_MoveSpeedInc           .Name := 'Input_MoveSpeedInc';
  Input_MoveSpeedDec           .Name := 'Input_MoveSpeedDec';
  Input_Jump                   .Name := 'Input_Jump';
  Input_Crouch                 .Name := 'Input_Crouch';
  Input_Run                    .Name := 'Input_Run';
end;

destructor TCastleWalkNavigation.Destroy;
begin
  inherited;
end;

function TCastleWalkNavigation.MoveAllowed(
  const OldPos: TVector3; ProposedNewPos: TVector3; out NewPos: TVector3;
  const BecauseOfGravity, CheckClimbHeight: Boolean): Boolean;
var
  NewIsAbove: boolean;
  NewAboveHeight, OldAbsoluteHeight, NewAbsoluteHeight: Single;
  NewAboveGround: PTriangle;
begin
  Result := inherited;

  if Result and Gravity and CheckClimbHeight and (ClimbHeight <> 0) and IsAbove and
    { if we're already below ClimbHeight then do not check if new position
      satisfies ClimbHeight requirement. This may prevent camera blocking
      in weird situations, e.g. if were forcefully pushed into some position
      (e.g. because player is hit by a missile with a knockback, or teleported
      or such). }
    (AboveHeight > ClimbHeight) then
  begin
    Height(NewPos, NewIsAbove, NewAboveHeight, NewAboveGround);
    if NewIsAbove then
    begin
      OldAbsoluteHeight := TVector3.DotProduct(Camera.GravityUp, OldPos);
      NewAbsoluteHeight := TVector3.DotProduct(Camera.GravityUp, NewPos);
      Result := not (
        AboveHeight - NewAboveHeight - (OldAbsoluteHeight - NewAbsoluteHeight) >
        ClimbHeight );
      // useful log to test ClimbHeight, but too spammy to be enabled by default
      // if Log and not Result then
      //   WritelnLog('Camera', 'Blocked move because of ClimbHeight (%f).', [ClimbHeight]);
    end;
  end;
end;

procedure TCastleWalkNavigation.CorrectPreferredHeight;
begin
  CastleCameras.CorrectPreferredHeight(
    FPreferredHeight, Radius, CrouchHeight, HeadBobbing);
end;

function TCastleWalkNavigation.UseHeadBobbing: boolean;
begin
  Result := Gravity and (HeadBobbing <> 0.0);
end;

function TCastleWalkNavigation.RealPreferredHeightNoHeadBobbing: Single;
begin
  Result := PreferredHeight;

  if IsCrouching then
    Result := Result * CrouchHeight;
end;

function TCastleWalkNavigation.RealPreferredHeight: Single;
var
  BobbingModifier: Single;
begin
  Result := RealPreferredHeightNoHeadBobbing;

  if UseHeadBobbing then
  begin
    { HeadBobbingPosition = 0 means that head is at lowest position.
      HeadBobbingPosition = 0.5 means that head is at highest position.
      HeadBobbingPosition = 1.0 means that head is at lowest position again.

      Larger HeadBobbingPosition work like Frac(HeadBobbingPosition)
      (i.e. function HeadBobbingPosition -> BobbingModifier
      is periodic with period = 1.0). }

    BobbingModifier := Frac(HeadBobbingPosition);

    if BobbingModifier <= 0.5 then
      BobbingModifier := MapRange(BobbingModifier, 0.0, 0.5, -1, +1) else
      BobbingModifier := MapRange(BobbingModifier, 0.5, 1.0, +1, -1);

    { Most game tutorials and codes advice that head bobbing be done with sinus,
      as below. But actually I found that the visual difference between
      sin-based head bobbing and linear-based (like above) head bobbing
      is not noticeable, so I'm using linear-based right now (as it's
      a little faster --- no trig calculation needed, although this
      could be avoided with sinus lookup table).

      If however you prefer sin-based head bobbing, uncomment line below
      and comment out 3 lines "if BobbingModifier <= 0.5 then ...." above.

    BobbingModifier := Sin(BobbingModifier * 2 * Pi);
    }

    BobbingModifier := BobbingModifier * (Result * HeadBobbing);
    Result := Result + BobbingModifier;
  end;
end;

function TCastleWalkNavigation.RealPreferredHeightMargin: Single;
begin
  { I tried using here something smaller like
    SingleEpsilon, but this was not good. }
  Result := RealPreferredHeight * 0.01;
end;

function TCastleWalkNavigation.AdjustPositionForRotationHorizontalPivot(
  const OldPosition: TVector3;
  const OldDirection, NewDirection: TVector3): TVector3;
var
  Pivot, OldDirectionInGravityPlane, NewDirectionInGravityPlane: TVector3;
begin
  Result := OldPosition;
  {$warnings off} // using deprecated RotationHorizontalPivot to keep it working
  if RotationHorizontalPivot <> 0 then
  begin
    if PreferGravityUpForRotations then
    begin
      Pivot := OldPosition + OldDirection * RotationHorizontalPivot;
      Result := Pivot - NewDirection * RotationHorizontalPivot;
    end else
    begin
      OldDirectionInGravityPlane := OldDirection;
      if not VectorsParallel(OldDirectionInGravityPlane, Camera.GravityUp) then
        MakeVectorsOrthoOnTheirPlane(OldDirectionInGravityPlane, Camera.GravityUp);
      NewDirectionInGravityPlane := NewDirection;
      if not VectorsParallel(NewDirectionInGravityPlane, Camera.GravityUp) then
        MakeVectorsOrthoOnTheirPlane(NewDirectionInGravityPlane, Camera.GravityUp);
      Pivot := OldPosition + OldDirectionInGravityPlane * RotationHorizontalPivot;
      Result := Pivot - NewDirectionInGravityPlane * RotationHorizontalPivot;
    end;
  end;
  {$warnings on}
end;

procedure TCastleWalkNavigation.RotateAroundGravityUp(const Angle: Single);
var
  GravityAxis,
    OldPosition, OldDirection, OldUp,
    NewPosition, NewDirection, NewUp: TVector3;
begin
  Camera.GetWorldView(OldPosition, OldDirection, OldUp);

  { We need to sometimes use GravityUp, sometimes -GravityUp,
    to intuitively keep rotations to left -> going left even
    when looking upside-down. }
  if AngleRadBetweenVectors(OldUp, Camera.GravityUp) > Pi/2 then
    GravityAxis := -Camera.GravityUp
  else
    GravityAxis := Camera.GravityUp;

  NewUp        := RotatePointAroundAxisRad(Angle,        OldUp, GravityAxis);
  NewDirection := RotatePointAroundAxisRad(Angle, OldDirection, GravityAxis);

  NewPosition := AdjustPositionForRotationHorizontalPivot(OldPosition, OldDirection, NewDirection);

  Camera.SetWorldView(NewPosition, NewDirection, NewUp);
end;

procedure TCastleWalkNavigation.RotateAroundUp(const Angle: Single);
var
  OldPosition, OldDirection, OldUp, NewPosition, NewDirection: TVector3;
begin
  Camera.GetWorldView(OldPosition, OldDirection, OldUp);

  { We know that RotatePointAroundAxisRad below doesn't change the length
    of the NewDirection (so it will remain normalized, like OldDirection)
    and it will keep NewDirection and OldUp vectors orthogonal. }
  NewDirection := RotatePointAroundAxisRad(Angle, OldDirection, OldUp);

  NewPosition := AdjustPositionForRotationHorizontalPivot(OldPosition, OldDirection, NewDirection);

  Camera.SetWorldView(NewPosition, NewDirection, OldUp);
end;

procedure TCastleWalkNavigation.RotateHorizontal(const Angle: Single);
begin
  if PreferGravityUpForRotations then
    RotateAroundGravityUp(Angle)
  else
    RotateAroundUp(Angle);
end;

procedure TCastleWalkNavigation.RotateVertical(AngleRad: Single);
var
  Side: TVector3;
  GravityAxis, OldPosition, OldDirection, OldUp, NewDirection, NewUp: TVector3;
  AngleRadBetween: Single;
begin
  Camera.GetWorldView(OldPosition, OldDirection, OldUp);

  { We need to sometimes use GravityUp, sometimes -GravityUp,
    to intuitively keep rotations to left -> going left even
    when looking upside-down. }
  if AngleRadBetweenVectors(OldUp, Camera.GravityUp) > Pi/2 then
    GravityAxis := -Camera.GravityUp
  else
    GravityAxis := Camera.GravityUp;

  if PreferGravityUpForRotations then
  begin
    Side := TVector3.CrossProduct(OldDirection, GravityAxis);
    if Side.IsZero then
    begin
      { if OldDirection is parallel to GravityAxis,
        then realizing PreferGravityUpForRotations is not really possible.
        Allow rotation as if PreferGravityUpForRotations = false.
        This is important to do right, to allow in CGE editor to use mouse look
        right after pressing Top (7). }
      Side := TVector3.CrossProduct(OldDirection, OldUp);
    end else
    if MinAngleFromGravityUp <> 0 then
    begin
      { Calculate AngleRadBetween, and possibly adjust AngleRad. }
      AngleRadBetween := AngleRadBetweenVectors(OldDirection, GravityAxis);

      if AngleRadBetween - AngleRad < MinAngleFromGravityUp then
        AngleRad := AngleRadBetween - MinAngleFromGravityUp
      else
      if AngleRadBetween - AngleRad > Pi - MinAngleFromGravityUp then
        AngleRad := AngleRadBetween - (Pi - MinAngleFromGravityUp);
    end;
  end else
  begin
    Side := TVector3.CrossProduct(OldDirection, OldUp);
  end;

  { Rotate NewUp around Side }
  NewUp        := RotatePointAroundAxisRad(AngleRad, OldUp       , Side);
  { Rotate NewDirection around Side }
  NewDirection := RotatePointAroundAxisRad(AngleRad, OldDirection, Side);

  Camera.SetWorldView(OldPosition, NewDirection, NewUp);
end;

procedure TCastleWalkNavigation.MoveHorizontal(Dir: TVector3;
  const SecondsPassed: Single);
var
  Multiplier: Single;
  Grav: TVector3;
begin
  Multiplier := MoveSpeed * MoveHorizontalSpeed * SecondsPassed;
  if IsJumping then
    Multiplier := Multiplier * JumpHorizontalSpeedMultiply;
  if Input_Run.IsPressed(Container) then
    Multiplier := Multiplier * 2;

  { Update HeadBobbingPosition }
  if (not IsJumping) and UseHeadBobbing and (not HeadBobbingAlreadyDone) then
  begin
    HeadBobbingPosition := HeadBobbingPosition + (SecondsPassed / HeadBobbingTime);
    HeadBobbingAlreadyDone := true;
  end;

  if PreferGravityUpForMoving then
  begin
    Grav := GravityUpLocal;
    if not VectorsParallel(Dir, Grav) then
      MakeVectorsOrthoOnTheirPlane(Dir, Grav)
    else
      { Do not move at all, if Dir and Grav parallel.
        This avoids moving vertically in such case. }
      EXit;
  end;

  MoveHorizontalDone := true;
  Move(Dir * Multiplier, false, true);
end;

procedure TCastleWalkNavigation.MoveVertical(const SecondsPassed: Single; const Multiply: Integer);

  { Provided PreferredUpVector must be already normalized. }
  procedure MoveVerticalCore(const PreferredUpVector: TVector3);
  var
    Multiplier: Single;
  begin
    Multiplier := MoveSpeed * MoveVerticalSpeed * SecondsPassed * Multiply;
    if Input_Run.IsPressed(Container) then
      Multiplier := Multiplier * 2;
    Move(PreferredUpVector * Multiplier, false, false);
  end;

begin
  if not Gravity then
  begin
    if PreferGravityUpForMoving then
      MoveVerticalCore(GravityUpLocal)
    else
      MoveVerticalCore(Camera.Up);
  end;
end;

function TCastleWalkNavigation.ReallyEnableMouseDragging: boolean;
begin
  Result := (inherited ReallyEnableMouseDragging) and not InternalUsingMouseLook;
end;

procedure TCastleWalkNavigation.Update(const SecondsPassed: Single;
  var HandleInput: boolean);

  { Check are keys for left/right/down/up rotations are pressed, and handle them.
    SpeedScale = 1 indicates a normal rotation speed, you can use it to scale
    the rotation speed to specific purposes. }
  procedure CheckRotates(SpeedScale: Single);
  begin
    if Input_RightRotate.IsPressed(Container) then
      RotateHorizontal(-RotationHorizontalSpeed * SecondsPassed * SpeedScale);
    if Input_LeftRotate.IsPressed(Container) then
      RotateHorizontal(+RotationHorizontalSpeed * SecondsPassed * SpeedScale);
    if Input_UpRotate.IsPressed(Container) then
      RotateVertical(+RotationVerticalSpeed * SecondsPassed * SpeedScale);
    if Input_DownRotate.IsPressed(Container) then
      RotateVertical(-RotationVerticalSpeed * SecondsPassed * SpeedScale);
  end;

  { Things related to gravity --- jumping, taking into account
    falling down and keeping RealPreferredHeight above the ground. }
  procedure GravityUpdate;

    function TryJump: boolean;
    var
      ThisJumpHeight: Single;
    begin
      Result := IsJumping;

      if Result then
      begin
        { jump. This means:
          1. update FJumpHeight and move Position
          2. or set FIsJumping to false when jump ends }
        ThisJumpHeight := MaxJumpDistance * SecondsPassed / FJumpTime;
        FJumpHeight := FJumpHeight + ThisJumpHeight;

        if FJumpHeight > MaxJumpDistance then
          FIsJumping := false else
          { do jumping }
          Move(GravityUpLocal * ThisJumpHeight, false, false);
      end;
    end;

   function TryFde_Stabilize: boolean; forward;

    { If our height above the ground is < RealPreferredHeight
      then we try to "grow".

      (this may happen because of many things --- e.g. user code
      just changed PreferredHeight to something larger
      (because e.g. "duck mode" ended), or we just ended falling dowm
      from high). }
    function TryGrow: boolean;
    var
      GrowingVectorLength: Single;
    begin
      Result := AboveHeight < RealPreferredHeight - RealPreferredHeightMargin;

      if Result then
      begin
        { calculate GrowingVectorLength }
        GrowingVectorLength := Min(
          MoveSpeed * MoveVerticalSpeed * GrowSpeed * SecondsPassed,
          RealPreferredHeight - AboveHeight);

        Move(GravityUpLocal * GrowingVectorLength, true, false);

        { When growing, TryFde_Stabilize also must be done.
          Otherwise when player walks horizontally on the flat surface
          for some time then "Falling down effect" activates --- because
          player is always in TryGrow or TryFalling. So one of them
          (TryGrow or TryFalling) *must* allow "Falling down effect"
          to stabilize itself. Obviously TryFalling can't (this would
          be against the idea of this effect) so TryGrow does it... }
        TryFde_Stabilize;
      end;
    end;

    function TryFalling: boolean;
    const
      Fde_VerticalRotateDeviation = 50.0;
      Fde_HorizontalRotateDeviation = 15.0;
    var
      PositionBefore: TVector3;
      FallingVectorLength: Single;
    begin
      Result := false;

      { Note that if we got here, then TryGrow returned false,
        which means that (assuming OnInternalHeight is correctly assigned)
        we are not above the ground, or
          AboveHeight >=
            RealPreferredHeight - RealPreferredHeightMargin
        However we require something stronger to continue:
          AboveHeight >
            RealPreferredHeight + RealPreferredHeightMargin

        This is important, because this way we avoid the unpleasant
        "bouncing" effect when in one Update we decide that camera
        is falling down, in next Update we decide that it's growing,
        in next Update it falls down again etc. In TryGrow we try
        to precisely set our Position, so that it hits exactly
        at RealPreferredHeight -- which means that after TryGrow,
        in next Update TryGrow should not cause growing and TryFalling
        should not cause falling down. }
      if AboveHeight <=
           RealPreferredHeight + RealPreferredHeightMargin then
      begin
        FFalling := false;
        Exit;
      end;

      { Make sure that FallSpeed is initialized.
        When Falling, we know it's initialized (because setting
        "FFalling := true;" is done only in the piece of code below...),
        otherwise we make sure it's set to it's starting value. }
      if not FFalling then
        FFallSpeed := FallSpeedStart;

      { try to fall down }
      PositionBefore := Camera.Translation;

      { calculate FallingVectorLength.

        Note that we make sure that FallingVectorLength is no longer
        than AboveHeight --- this way we avoid the problem
        that when FFallSpeed would get very big,
        we couldn't fall down any more (while in fact we should then fall down
        very quickly).

        Actually, we even do more. We make sure that
        FallingVectorLength is no longer than
        (AboveHeight - RealPreferredHeight).
        Initially I wanted to do here
          MinVar(FallingVectorLength, AboveHeight);
        i.e. to allow camera to fall below RealPreferredHeight.

        But this didn't work like it should. Why ?
        See above for the trick that I have to do with
        RealPreferredHeightMargin above (to not cause
        "unpleasant bouncing" when swapping Falling and TryGrow).
        If I could fall down here below RealPreferredHeight then

        1. It *will not* cause the desired "nice" effect (of automatically
           "ducking" when falling down from high), because of comparison
           (the one with RealPreferredHeightMargin) above.

        2. It *will* cause the undesired unpleasant swapping between
           Falling and TryGrow.

        So it's totally bad thing to do.

        This means that I should limit myself to not fall down
        below RealPreferredHeight. And that's what I'm doing. }
      FallingVectorLength :=
        MoveSpeed * MoveVerticalSpeed * FFallSpeed * SecondsPassed;
      MinVar(FallingVectorLength, AboveHeight - RealPreferredHeight);

      if Move(GravityUpLocal * (- FallingVectorLength), true, false) and
        (not TVector3.PerfectlyEquals(Camera.Translation, PositionBefore)) then
      begin
        if not Falling then
        begin
          FFallingStartPosition := PositionBefore;

          { Why do I init here FFallSpeed ? A few lines above I did
              if not FFalling then
                FFallSpeed := FallSpeedStart;
            to init FFallSpeed (I had to do it to calculate
            FallingVectorLength). So why initing it again here ?

            Answer: Because Move above called MoveTo, that set Position
            that actually called VisibleChange that possibly
            called OnVisibleChange.
            And OnVisibleChange is user callback and user could do there
            things like
            - Changing FallSpeedStart (but still it's unspecified
              whether we have to apply this change, right ?)
            - Calling CancelFalling and *then* changing FallSpeedStart.
              And in this case, we *must* honour it, because here user
              expects that we will use FallSpeedStart if we want
              to fall down. (of course, one call to "Move" with old
              "FallSpeedStart" was already done, that's unavoidable...).

            TODO: Is the above reasoning still valid? Now only TCastleCamera
            calls VisibleChange.
          }
          FFallSpeed := FallSpeedStart;

          FFalling := true;
        end;

        Result := true;

        if AboveHeight < RealPreferredHeight * 1.1 then
        begin
          { This check is needed, otherwise when you're walking down even from
            the most slight hill then you get

            1. FallingEffect
            2. OnFall is called seldom and with large heights.

            Why ? Because MoveHorizontal calls are done between GravityUpdate
            calls, and the move can be quite fast. So even though the player is
            actually quite closely following the terrain, we would constantly
            have Falling := true. Consider a large hill that is almost
            flat --- when walking down the hill, we would get Falling
            := true, FallSpeed and FallingEffect would raise,
            and at the end OnFall would be called with parameters
            like player fell down from the top of the hill to the ground
            (which can cause e.g. player losing life).

            The check for RealPreferredHeight * 1.1 above and
            setting FFalling cure the situation. OnFall will
            be called more often indicating very small fallen down heights,
            and FallSpeed and FallingEffect will not be able
            to raise high as long as player follows terrain closely.

            Of course we're setting here FFalling := false even though
            the player is not exactly on the terrain --- but he's very close.
            In the next GravityUpdate call we will again bring him a little
            down, set FFalling to @true, and then set it back to @false
            by line below. }
          FFalling := false;
        end else
        begin
          { This is where we do FallingEffect.

            Note that I do FallingEffect *before* increasing
            FFallSpeed below.

            1. reason (ideological, not really that important...) is that
               FallingEffect is a penalty equivalent to FFallSpeed that
               was already used --- not to the future FFallSpeed.

            2. reason (practical, and real :) is that when the program
               was in some non-3d drawing state (e.g. displaying menu, or
               displaying progress bar because the VRML model was just loaded)
               then SecondsPassed indicates (truly) that a lot of time elapsed
               since last Update. This means that it's common that at the same moment
               when Falling changed suddenly to @true, SecondsPassed may be large
               and we're better not using this too much... A practical bug demo:
               open in view3dscene (it does progress bar in OpenGL, so will cause
               large SecondsPassed) any model with gravity on and camera slightly
               higher then PreferredHeight (we want to trigger Falling
               right when the model is loaded). E.g. run
               "view3dscene demo_models/navigation/speed_2.wrl".
               If FallSpeedIncrease will be done before FallingEffect,
               then you'll see that at the very first frame FFallSpeed
               was increased so much (because SecondsPassed was large) that it triggered
               FallingEffect. Even though the falling down distance was really small...

               Maybe in the future I'll workaround it differently.
               One idea is that FFallSpeed should be made smaller if the
               falled down distance is small. Or just don't call GravityUpdate after the first
               model load, to avoid using large SecondsPassed ?

               LATER NOTE: note that the (2.) problem above may be non-existing
               now, since we use SecondsPassed and we have ZeroNextSecondsPassed to
               set SecondsPassed to zero in such cases. }
          if FallingEffect and
             (FFallSpeed > FallSpeedStart * 3) then
          begin
            if FFallSpeed > FallSpeedStart * 5 then
            begin
              if Fde_RotateHorizontal = 0 then
                Fde_RotateHorizontal := RandomPlusMinus;
              RotateAroundGravityUp(DegToRad(Fde_RotateHorizontal *
                Fde_HorizontalRotateDeviation * SecondsPassed));
            end;

            if Fde_UpRotate < 0 then
              Fde_UpRotate := Fde_UpRotate - (Fde_VerticalRotateDeviation * SecondsPassed) else
            if Fde_UpRotate > 0 then
              Fde_UpRotate := Fde_UpRotate + (Fde_VerticalRotateDeviation * SecondsPassed) else
              Fde_UpRotate := RandomPlusMinus *
                              Fde_VerticalRotateDeviation * SecondsPassed;
          end;

          { Note that when changing FFallSpeed below I'm using SecondsPassed * 50.
            And also above when using FFallSpeed, I multipled
            FFallSpeed * SecondsPassed * 50. This is correct:
            - changing position based on FallSpeed is a "velocity"
            - changing FallSpeed below is "acceleration"
            And both acceleration and velocity must be time-based. }
          if FallSpeedIncrease <> 1.0 then
            FFallSpeed := FFallSpeed * (Power(FallSpeedIncrease, SecondsPassed * 50));
        end;
      end else
        FFalling := false;
    end;

    function TryFde_Stabilize: boolean;
    const
      Fde_VerticalRotateNormalization = 7 * 50;
    var
      Change: Single;
    begin
      Result := (Fde_RotateHorizontal <> 0) or (Fde_UpRotate <> 0);

      { Bring Fde_Xxx vars back to normal (zero) values. }

      Fde_RotateHorizontal := 0;

      if Fde_UpRotate <> 0.0 then
      begin
        { Note that we try to immediately bring UpRotate to
          range (-360, 360) here. E.g. no need to gradually bring back
          UpRotate from 360.0 to 0.0 --- this doesn't cause
          any interesting visual effect (and the only reason for
          UpRotate is a visual effect)... }
        Change := Trunc(Abs(Fde_UpRotate) / 360.0) * 360.0 +
          Fde_VerticalRotateNormalization * SecondsPassed;

        if Fde_UpRotate < 0 then
          Fde_UpRotate := Min(Fde_UpRotate + Change, 0.0)
        else
          Fde_UpRotate := Max(Fde_UpRotate - Change, 0.0);
      end;
    end;

    function TryFallingOnTheGround: boolean;
    var
      Grav: TVector3;

      { Return @link(TCastleTransform.Direction Camera.Direction) vector rotated such that it is
        orthogonal to GravityUp. This way it returns
        @link(TCastleTransform.Direction Camera.Direction) projected
        on the gravity horizontal plane, which neutralizes such things
        like raising / bowing your head.

        Result is always normalized (length 1).

        Note that when @link(TCastleTransform.Direction Camera.Direction) and GravityUp are parallel,
        this just returns current @link(TCastleTransform.Direction Camera.Direction) --- because in such case
        we can't project @link(TCastleTransform.Direction Camera.Direction) on the horizontal plane.

        Note that the result is in TCastleCamera parent coordinate space, just like Direction.
        We automatically account for the fact that GravityUp is specified in world coordinate space. }
      function DirInGravityPlane: TVector3;
      begin
        Result := Camera.Direction;
        MakeVectorsOrthoOnTheirPlane(Result, Grav);
      end;

    var
      Angle, AngleRotate: Single;
    begin
      Grav := GravityUpLocal;

      Result := FFallingOnTheGround;
      if not Result then
        Exit;

      Angle := AngleRadBetweenVectors(Camera.Up, Grav);
      if SameValue(Angle, HalfPi, 0.01) then
      begin
        { FallingOnTheGround effect stops here. }
        FFallingOnTheGround := false;
        Exit;
      end;

      { Our DirInGravityPlane doesn't work when Camera.Direction and Grav are parallel }
      if VectorsParallel(Camera.Direction, Grav) then
        Exit;

      AngleRotate := SecondsPassed * 5;
      MinVar(AngleRotate, Abs(Angle - HalfPi));
      if not FFallingOnTheGroundAngleIncrease then
        AngleRotate := -AngleRotate;

      Camera.Up := RotatePointAroundAxisRad(AngleRotate, Camera.Up, DirInGravityPlane);
    end;

    procedure DoFall;
    var
      BeginPos, EndPos, FallVector: TVector3;
    begin
      if Assigned(OnFall) then
      begin
        { Project Position and FFallingStartPosition
          onto GravityUp vector to calculate fall height. }
        BeginPos := PointOnLineClosestToPoint(TVector3.Zero, GravityUpLocal, FFallingStartPosition);
        EndPos   := PointOnLineClosestToPoint(TVector3.Zero, GravityUpLocal, Camera.Translation);
        FallVector := BeginPos - EndPos;

        { Because of various growing and jumping effects (imagine you jump up
          onto a taller pillar) it may turn out that we're higher at the end
          at the end of fall. Do not report it to OnFall event in this case. }
        if TVector3.DotProduct(GravityUpLocal, FallVector.Normalize) <= 0 then
          Exit;

        OnFall(Self, FallVector.Length);
      end;
    end;

    procedure HeadBobbingGoesDown;
    const
      HeadBobbingGoingDownSpeed = 5;
    var
      FracHeadBobbingPosition: Single;
    begin
      if UseHeadBobbing and (not HeadBobbingAlreadyDone) then
      begin
        { If head bobbing is active, but player did not move during
          this Update call, and no gravity effect is in work
          then player is standing still on the ground.

          This means that his head bobbing should go down as far as
          possible. This means that HeadBobbingPosition should
          go to nearest integer value.

          Note that we avoid changing HeadBobbingPosition by less
          than SingleEpsilon, just to be on the safe side
          and avoid any "corner cases", when HeadBobbingPosition
          would switch between going up and down repeatedly. }
        FracHeadBobbingPosition := Frac(HeadBobbingPosition);
        if FracHeadBobbingPosition > 0.5 then
        begin
          if 1 - FracHeadBobbingPosition > SingleEpsilon then
            HeadBobbingPosition := HeadBobbingPosition +
              Min(HeadBobbingGoingDownSpeed * SecondsPassed,
                  1 - FracHeadBobbingPosition);
        end else
        begin
          if FracHeadBobbingPosition > SingleEpsilon then
            HeadBobbingPosition := HeadBobbingPosition -
              Min(HeadBobbingGoingDownSpeed * SecondsPassed,
                  FracHeadBobbingPosition);
        end;
      end;
    end;

    function GetIsOnTheGround: boolean;
    var
      MinAboveHeight, MaxAboveHeight, H: Single;
    begin
      H := RealPreferredHeightNoHeadBobbing;
      MinAboveHeight := (H - H * HeadBobbing) * 0.99;
      MaxAboveHeight := (H + H * HeadBobbing) * 1.01;
      Result := IsAbove and
        (MinAboveHeight <= AboveHeight) and
        (AboveHeight <= MaxAboveHeight);
    end;

  var
    OldFalling: boolean;
  begin
    OldFalling := Falling;

    if Gravity then
    begin
      { update IsAbove, AboveHeight }
      Height(Camera.WorldTranslation, FIsAbove, FAboveHeight, FAboveGround);

      FIsOnTheGround := GetIsOnTheGround;
      FIsWalkingOnTheGround := MoveHorizontalDone and FIsOnTheGround;

      if not TryJump then
        if not TryGrow then
          if not TryFalling then
            if not TryFde_Stabilize then
              { Note that we don't do FallingOnTheGround effect until all
                other effects (jumping, growing, falling on the ground
                and stabilizing after falling on the ground) will finish
                their work. }
              if not TryFallingOnTheGround then
                HeadBobbingGoesDown;
    end else
    begin
      FFalling := false;
      TryFde_Stabilize;
    end;

    if OldFalling and (not Falling) then
      DoFall;
  end;

  procedure PreferGravityUpForRotationsUpdate;
  (* This is a good piece of work and seemed to work OK,
     but it's too much untested right now to let it work.

     It's needed only when you'll start to change
     PreferGravityUpForRotations from false to true in runtime,
     to avoid making player feel "awkward" rotations.

     Temporary I don't need it.

  var
    TargetPlane: TVector4;
    TargetPlaneDir: TVector3 absolute TargetPlane;
    TargetUp: TVector3;
    AngleRadBetweenTargetAndGravity: Single;
    AngleRadBetweenTarget, AngleRadBetweenTargetChange: Single;
    NewUp: TVector3;
  begin
    if PreferGravityUp then
    begin
      { TODO: Correcting MinAngleFromGravityUp }

      { Correct Up such that GravityUp, Direction and Up
        are on the same plane.

        Math:
          TargetPlane := common plane of GravityUp and Direction,
          given by (A, B, C) = TVector3.CrossProduct(GravityUp, Direction)
          and D = 0 (because point (0, 0, 0) is part of this plane).

          We check whether Up is on this TargetPlane too.

          If not, we find TargetUp = nearest point to Up
          lying on this TargetPlane. We want our Up be pointing
          like GravityUp, not in the other way, so if the angle between
          GravityUp and TargetUp is > 90 degress we negate
          TargetUp. If the angle is exactly 90 degress then
          TargetUp is simply equal to GravityUp.

          And then we make the angle between TargetUp and Up
          smaller. }

      TargetPlaneDir := TVector3.CrossProduct(GravityUp, Direction);
      if not Zero(
         (TargetPlaneDir[0] * FUp[0]) +
         (TargetPlaneDir[1] * FUp[1]) +
         (TargetPlaneDir[2] * FUp[2])) then
      begin
        TargetPlane[3] := 0;

        Writeln('corrrecting');

        { calculate TargetUp }
        TargetUp := PointOnPlaneClosestToPoint(TargetPlane, FUp);
        AngleRadBetweenTargetAndGravity :=
          AngleRadBetweenVectors(TargetUp, GravityUp);
        if SameValue(AngleRadBetweenTargetAndGravity, HalfPi) then
          TargetUp := GravityUp else
        if AngleRadBetweenTargetAndGravity > HalfPi then
          TargetUp := -TargetUp;

        AngleRadBetweenTarget := AngleRadBetweenVectors(TargetUp, FUp);
        AngleRadBetweenTargetChange := 0.5 * SecondsPassed;
        if AngleRadBetweenTarget > AngleRadBetweenTargetChange then
        begin
          NewUp := FUp;
          MakeVectorsAngleRadOnTheirPlane(NewUp, TargetUp,
            AngleRadBetweenTarget - AngleRadBetweenTargetChange, NewUp);
          Up := NewUp;
        end else
          Up := TargetUp;
      end;
    end;
    *)
  begin
  end;

  procedure ChangePreferredHeight(const Increase: Integer);
  begin
    PreferredHeight := PreferredHeight +
      { It's best to scale PreferredHeight changes by MoveSpeed,
        to make it faster/slower depending on scene size
        (which usually corresponds to move speed). }
      Increase * MoveSpeed * SecondsPassed * 0.2;

    CorrectPreferredHeight;
  end;

  procedure MoveViaMouseDragging(Delta: TVector2);
  var
    MoveSizeX, MoveSizeY: Single;
  const
    Tolerance = 5;  { 5px tolerance for not-moving }
  begin
    MoveSizeX := 0;
    MoveSizeY := 0;

    if Abs(Delta.X) < Tolerance then
      Delta.X := 0
    else
      MoveSizeX := (Abs(Delta.X) - Tolerance) * MouseDraggingMoveSpeed;

    if Abs(Delta.Y) < Tolerance then
      Delta.Y := 0
    else
      MoveSizeY := (Abs(Delta.Y) - Tolerance) * MouseDraggingMoveSpeed;

    if buttonLeft in Container.MousePressed then
    begin
      if Delta.Y < -Tolerance then
        MoveHorizontal( Camera.Direction, -MoveSizeY * SecondsPassed); // forward
      if Delta.Y > Tolerance then
        MoveHorizontal(-Camera.Direction, -MoveSizeY * SecondsPassed); // backward

      if Abs(Delta.X) > Tolerance then
        RotateHorizontal(-Delta.X * SecondsPassed * MouseDraggingHorizontalRotationSpeed); { rotate }
    end
    else if buttonRight in Container.MousePressed then
    begin
      if Delta.X < -Tolerance then
        MoveHorizontal(DirectionLeft, MoveSizeX * SecondsPassed);
      if Delta.X > Tolerance then
        MoveHorizontal(DirectionRight, MoveSizeX * SecondsPassed);

      if Delta.Y < -5 then
        MoveVertical(-MoveSizeY * SecondsPassed, 1);    { fly up }
      if Delta.Y > 5 then
        MoveVertical(-MoveSizeY * SecondsPassed, -1);   { fly down }
    end;
  end;

var
  ModsDown: TModifierKeys;
begin
  inherited;

  { update Cursor every frame, in case InternalViewport.Paused changed
    (which changes UsingInput and InternalUsingMouseLook) }
  if InternalUsingMouseLook then
    Cursor := mcForceNone
  else
    Cursor := mcDefault;

  if (not Valid) then Exit;

  ModsDown := ModifiersDown(Container.Pressed);

  HeadBobbingAlreadyDone := false;
  MoveHorizontalDone := false;

  if HandleInput then
  begin
    if niNormal in UsingInput then
    begin
      HandleInput := false;
      FIsCrouching := Gravity and Input_Crouch.IsPressed(Container);

      if (not CheckModsDown) or
         (ModsDown - Input_Run.Modifiers = []) then
      begin
        CheckRotates(1.0);

        if Input_Forward.IsPressed(Container) or MoveForward then
          MoveHorizontal( Camera.Direction, SecondsPassed);
        if Input_Backward.IsPressed(Container) or MoveBackward then
          MoveHorizontal(-Camera.Direction, SecondsPassed);
        if Input_RightStrafe.IsPressed(Container) then
          MoveHorizontal(DirectionRight, SecondsPassed);
        if Input_LeftStrafe.IsPressed(Container) then
          MoveHorizontal(DirectionLeft , SecondsPassed);

        { A simple implementation of Input_Jump was
            RotateVertical(HalfPi); Move(MoveVerticalSpeed * MoveSpeed * SecondsPassed); RotateVertical(-HalfPi)
          Similarly, simple implementation of Input_Crouch was
            RotateVertical(-HalfPi); Move(MoveVerticalSpeed * MoveSpeed * SecondsPassed); RotateVertical(HalfPi)
          But this is not good, because when PreferGravityUp, we want to move
          along the GravityUp. (Also later note: RotateVertical is now bounded by
          MinAngleFromGravityUp). }

        if Input_Jump.IsPressed(Container) then
          MoveVertical(SecondsPassed, 1);
        if Input_Crouch.IsPressed(Container) then
          MoveVertical(SecondsPassed, -1);
        if Input_MoveSpeedInc.IsPressed(Container) then
          MoveSpeedInc(SecondsPassed);
        if Input_MoveSpeedDec.IsPressed(Container) then
          MoveSpeedDec(SecondsPassed);
        if Input_IncreasePreferredHeight.IsPressed(Container) then
          ChangePreferredHeight(+1);
        if Input_DecreasePreferredHeight.IsPressed(Container) then
          ChangePreferredHeight(-1);
      end else
      if ModsDown = [mkCtrl] then
      begin
        if AllowSlowerRotations then
          CheckRotates(0.1);
      end;
    end;

    { mouse dragging navigation }
    if (MouseDraggingStarted <> -1) and
       ReallyEnableMouseDragging and
       ((buttonLeft in Container.MousePressed) or (buttonRight in Container.MousePressed)) and
       { Enable dragging only when no modifiers (except Input_Run,
         which must be allowed to enable running) are pressed.
         This allows application to handle e.g. ctrl + dragging
         in some custom ways (like view3dscene selecting a triangle). }
       (Container.Pressed.Modifiers - Input_Run.Modifiers = []) and
       (MouseDragMode = mdWalk) then
    begin
      HandleInput := false;
      MoveViaMouseDragging(Container.MousePosition - MouseDraggingStart);
    end;
  end;

  PreferGravityUpForRotationsUpdate;

  { These may be set to @true only inside GravityUpdate }
  FIsWalkingOnTheGround := false;
  FIsOnTheGround := false;

  { Disable gravity in design mode (in the future we may add optional way to enable them) }
  if not CastleDesignMode then
    GravityUpdate;
end;

function TCastleWalkNavigation.Jump: boolean;
begin
  Result := false;

  if IsJumping or Falling or (not Gravity) then Exit;

  { Merely checking for Falling is not enough, because Falling
    may be triggered with some latency. E.g. consider user that holds
    Input_Jump key down: whenever jump will end (in GravityUpdate),
    Input_Jump.IsKey = true will cause another jump to be immediately
    (before Falling will be set to true) initiated.
    This is of course bad, because user holding Input_Jump key down
    would be able to jump to any height. The only good thing to do
    is to check whether player really has some ground beneath his feet
    to be able to jump. }

  { update IsAbove, AboveHeight }
  Height(Camera.WorldTranslation, FIsAbove, FAboveHeight, FAboveGround);

  if AboveHeight > RealPreferredHeight + RealPreferredHeightMargin then
    Exit;

  FIsJumping := true;
  FJumpHeight := 0.0;
  Result := true;
end;

function TCastleWalkNavigation.AllowSuspendForInput: boolean;
begin
  Result := false;
end;

function TCastleWalkNavigation.Press(const Event: TInputPressRelease): boolean;

  procedure SetUpToGravityUp;
  var
    OldPosition, OldDirection, OldUp, NewDirection, NewUp: TVector3;
  begin
    Camera.GetWorldView(OldPosition, OldDirection, OldUp);

    if VectorsParallel(OldDirection, Camera.GravityUp) then
    begin
      { We can't carelessly set NewUp to something parallel to GravityUp
        in this case.

        Yes, this situation can happen: for example open a model with
        no viewpoint in VRML in view3dscene (so default viewpoint,
        both gravity and Up = +Y is used). Then change GravityUp
        by menu and press Home (Input_GravityUp). }

      NewUp := Camera.GravityUp;
      NewDirection := AnyOrthogonalVector(NewUp);
    end else
    begin
      NewUp := Camera.GravityUp;
      NewDirection := OldDirection; // we use AdjustUp = false with SetWorldView, it will already adjust direction
    end;

    Camera.SetWorldView(OldPosition, NewDirection, NewUp, false);
  end;

  procedure HandleMouseWheelPress;
  const
    PretendSecondsPassed = 1 / 30;
  var
    RotationsSpeedScale: Single;
  begin
    { Inputs below are handled also in TCastleWalkNavigation.Update.
      But the mouse wheel is never in a pressed state, so it cannot be handled in Update.
      So we handle pressing on mouse wheel in a special way here.

      TODO: A more generic mechanism for handling mouse wheel would be nice, so we don't
      need to double handling of everything for mouse wheel?  }

    if Input_MoveSpeedInc.IsEvent(Event) then
    begin
      MoveSpeedInc(PretendSecondsPassed);
      Result := true;
    end;

    if Input_MoveSpeedDec.IsEvent(Event) then
    begin
      MoveSpeedDec(PretendSecondsPassed);
      Result := true;
    end;

    if Container.Pressed.Modifiers = [mkCtrl] then
      RotationsSpeedScale := 0.1
    else
      RotationsSpeedScale := 1.0;

    if Input_RightRotate.IsEvent(Event) then
    begin
      RotateHorizontal(-RotationHorizontalSpeed * PretendSecondsPassed * RotationsSpeedScale);
      Result := true;
    end;

    if Input_LeftRotate.IsEvent(Event) then
    begin
      RotateHorizontal(+RotationHorizontalSpeed * PretendSecondsPassed * RotationsSpeedScale);
      Result := true;
    end;

    if Input_UpRotate.IsEvent(Event) then
    begin
      RotateVertical(+RotationVerticalSpeed * PretendSecondsPassed * RotationsSpeedScale);
      Result := true;
    end;

    if Input_DownRotate.IsEvent(Event) then
    begin
      RotateVertical(-RotationVerticalSpeed * PretendSecondsPassed * RotationsSpeedScale);
      Result := true;
    end;
  end;

begin
  Result := inherited;
  if Result then Exit;

  if (Event.EventType = itKey) and
     CheckModsDown and
     (ModifiersDown(Container.Pressed) - Input_Run.Modifiers <> []) then
    Exit;

  if (Event.EventType = itMouseButton) and
     ReallyEnableMouseDragging and
     (MouseDragMode = mdNone) then
  begin
    MouseDraggingStarted := -1;
    Result := false;
    Exit;
  end;

  if (not (niNormal in UsingInput)) or
     (not Valid) then
    Exit(false);

  if Input_GravityUp.IsEvent(Event) then
  begin
    SetUpToGravityUp;
    Result := true;
  end;

  if Input_Jump.IsEvent(Event) then
  begin
    if Jump then
      Result := true;
  end;

  if Event.EventType = itMouseWheel then
    HandleMouseWheelPress;
end;

procedure TCastleWalkNavigation.MoveSpeedInc(const SecondsPassed: Single);
begin
  { Time-based animation for multiplication: use SecondsPassed as an exponent.

    We want to do "MoveSpeed *= 10", intuitively.

    To do it correctly (accounting that each system has different FPS)
    we want to apply SecondsPassed such that the effect is the same, regardless
    if we apply it in 10 steps over 1 second, or 1 step over 1 second.
    Using SecondsPassed as the exponent is the solution.
  }
  if MoveSpeed < MoveSpeedMax then
    MoveSpeed := Min(MoveSpeedMax, MoveSpeed * Power(10, SecondsPassed));
end;

procedure TCastleWalkNavigation.MoveSpeedDec(const SecondsPassed: Single);
begin
  if MoveSpeed > MoveSpeedMin then
    MoveSpeed := Max(MoveSpeedMin, MoveSpeed / Power(10, SecondsPassed));
end;

function TCastleWalkNavigation.SensorTranslation(const X, Y, Z, Length: Double;
  const SecondsPassed: Single): boolean;
var
  MoveSize: Double;
begin
  if not (ni3dMouse in UsingInput) then Exit(false);
  Result := true;

  MoveSize := Length * SecondsPassed / 5000;

  if Z > 5 then
    MoveHorizontal(-Camera.Direction, Z * MoveSize); { backward }
  if Z < -5 then
    MoveHorizontal( Camera.Direction, -Z * MoveSize); { forward }

  if X > 5 then
    MoveHorizontal(DirectionRight, X * MoveSize);  { right }
  if X < -5 then
    MoveHorizontal(DirectionLeft, -X * MoveSize); { left }

  if Y > 5 then
    MoveVertical(Y * MoveSize, 1);    { up }
  if Y < -5 then
    MoveVertical(-Y * MoveSize, -1);  { down }
end;

function TCastleWalkNavigation.SensorRotation(const X, Y, Z, Angle: Double;
  const SecondsPassed: Single): boolean;
const
  SpeedSensor = 2;
begin
  if not (ni3dMouse in UsingInput) then Exit(false);
  Result := true;

  if Abs(X) > 0.4 then      { tilt forward / backward }
    RotateVertical(X * Angle * SpeedSensor * SecondsPassed);
  if Abs(Y) > 0.4 then      { rotate }
    RotateHorizontal(Y * Angle * SpeedSensor * SecondsPassed);
  {if Abs(Z) > 0.4 then ?} { tilt sidewards }
end;

procedure TCastleWalkNavigation.Init(
  const AInitialPosition, AInitialDirection, AInitialUp: TVector3;
  const AGravityUp: TVector3;
  const APreferredHeight: Single;
  const ARadius: Single);
begin
  PreferredHeight := APreferredHeight;
  Radius := ARadius;
  CorrectPreferredHeight;

  Camera.SetWorldView(AInitialPosition, AInitialDirection, AInitialUp);
  Camera.GravityUp := AGravityUp;
  Camera.ProjectionNear := Radius * RadiusToProjectionNear;
end;

procedure TCastleWalkNavigation.Init(const Box: TBox3D; const ARadius: Single);
var
  Pos: TVector3;
  AvgSize: Single;
begin
  if Box.IsEmptyOrZero then
  begin
    Radius := ARadius;
    PreferredHeight := Max(DefaultPreferredHeight, RadiusToPreferredHeightMin * ARadius);
    CorrectPreferredHeight;

    Camera.ProjectionNear := Radius * RadiusToProjectionNear;

    Camera.SetWorldView(TVector3.Zero,
      DefaultCameraDirection,
      DefaultCameraUp);
    Camera.GravityUp := DefaultCameraUp;
  end else
  begin
    Radius := ARadius;
    AvgSize := Box.AverageSize;
    PreferredHeight := AvgSize * 5;
    CorrectPreferredHeight;

    Camera.ProjectionNear := Radius * RadiusToProjectionNear;

    Pos := Vector3(
      Box.Data[0].X - AvgSize,
      (Box.Data[0].Y + Box.Data[1].Y) / 2,
      (Box.Data[0].Z + Box.Data[1].Z) / 2
    );
    Camera.SetWorldView(Pos,
      DefaultCameraDirection,
      DefaultCameraUp);
    Camera.GravityUp := DefaultCameraUp;
  end;
end;

procedure TCastleWalkNavigation.UpPrefer(const AUp: TVector3);
begin
  Camera.UpPrefer(AUp);
end;

function TCastleWalkNavigation.MaxJumpDistance: Single;
begin
  Result := JumpMaxHeight * PreferredHeight;
end;

function TCastleWalkNavigation.GravityUpLocal: TVector3;
begin
  if (Camera.Parent <> nil) and
     (Camera.World <> nil) then
    Result := Camera.Parent.WorldToLocalDirection(Camera.GravityUp).Normalize
  else
    Result := Camera.GravityUp;
end;

function TCastleWalkNavigation.DirectionLeft: TVector3;
begin
  Result := TVector3.CrossProduct(Camera.Up, Camera.Direction);
end;

function TCastleWalkNavigation.DirectionRight: TVector3;
begin
  Result := TVector3.CrossProduct(Camera.Direction, Camera.Up);
end;

procedure TCastleWalkNavigation.FallOnTheGround;
begin
  FFallingOnTheGround := true;

  { Mathematically reasoning, this should be smarter.
    I mean that we should randomize FFallingOnTheGroundAngleIncrease
    *only* if Up is parallel to GravityUp ?
    Otherwise Up could change through some strange path ?

    But current effect seems to behave good in all situations...
    In any case, Up going through some strange path will only
    be noticeable for a very short time, so I don't think that's a real
    problem... unless I see some example when it looks bad. }

  FFallingOnTheGroundAngleIncrease := RandomBoolean;
end;

procedure TCastleWalkNavigation.CancelFalling;
begin
  { Fortunately implementation of this is brutally simple right now. }
  FFalling := false;
end;

procedure TCastleWalkNavigation.ProcessMouseLookDelta(const Delta: TVector2);
begin
  inherited;
  RotateHorizontal(-Delta.X);
  RotateVertical(Delta.Y);
end;

function TCastleWalkNavigation.Motion(const Event: TInputMotion): boolean;

  procedure HandleMouseDrag;
  var
    MouseChange: TVector2;
  begin
    MouseChange := Event.Position - Container.MousePosition;
    if MouseChange[0] <> 0 then
      RotateHorizontal(-MouseChange[0] * MouseDraggingHorizontalRotationSpeed);
    if MouseChange[1] <> 0 then
      RotateVertical(MouseChange[1] * MouseDraggingVerticalRotationSpeed);
  end;

begin
  Result := inherited;
  if Result or (Event.FingerIndex <> 0) then Exit;

  if (MouseDraggingStarted <> -1) and
    // Not need to check here ReallyEnableMouseDragging, as MouseDraggingStarted is already <> -1
    // ReallyEnableMouseDragging and
    (MouseDragMode = mdRotate) and
    Valid and
    (not InternalUsingMouseLook) then
  begin
    HandleMouseDrag;
    Result := true;
  end;
end;

function TCastleWalkNavigation.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Gravity', 'MoveSpeed', 'Radius', 'CrouchHeight', 'PreferredHeight',
       'MoveHorizontalSpeed', 'MoveVerticalSpeed',
       'MouseDraggingHorizontalRotationSpeed', 'MouseDraggingVerticalRotationSpeed',
       'MouseDraggingMoveSpeed', 'MouseDragMode', 'RotationHorizontalSpeed',
       'RotationVerticalSpeed'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

class procedure TCastleWalkNavigation.CreateComponentFly(Sender: TObject);
begin
  (Sender as TCastleWalkNavigation).Gravity := false;
end;

function TCastleWalkNavigation.DirectionInGravityPlane: TVector3;
var
  Grav: TVector3;
begin
  Result := Camera.Direction;

  Grav := GravityUpLocal;
  if not VectorsParallel(Result, Grav) then
    MakeVectorsOrthoOnTheirPlane(Result, Grav);
end;   