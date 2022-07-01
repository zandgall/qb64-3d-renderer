Type vector
    x As Double
    y As Double
    z As Double
    w As Double
End Type

Type light
    position As vector
    intensity As Double
End Type

Dim Shared As Integer Width, Height
Dim Shared tri_data(29) As Double ' Store vertice position data, 1/z1-z3, area, 1/area, uvw, normal, vertice phyiscal position
Dim Shared vector(4) As Double ' Store xyzw for a vector used for transformations
Dim Shared matrix(16) As Double ' Store a matrix that can be used to transform vector
Dim Shared projection_data(4) As Double ' Store FOV * 0.5, FOV * Width * 0.5, Zm, and Za
Dim Shared render_data(14) As Double 'left inverse slope, right inverse slope, left line x, right line x, starting y, ending y, uvw derivatives, index, depth
Dim Shared pixel_x As Integer
Dim Shared pixel_y As Integer
Dim Shared As vector shader_color, physical_position
Dim Shared current_frame As Integer
current_frame = 1

Dim Shared As Integer NUM_OF_ATTRIBUTES, NUM_OF_IMAGES, NUM_OF_LIGHTS
NUM_OF_ATTRIBUTES = 2
NUM_OF_IMAGES = 16
NUM_OF_LIGHTS = 16
Dim Shared verticeAttributes(3 * NUM_OF_ATTRIBUTES) As Double ' Values that will be interpolated in a triangle
Dim Shared pixelAttributes(NUM_OF_ATTRIBUTES) As Double ' The resulting interpolated attributes
Dim Shared images&(NUM_OF_IMAGES) ' Shared images that can be used as textures and so forth, images&(0) is always screen
Dim Shared lights(NUM_OF_LIGHTS) As light

Width = 1200
Height = 800

Dim Shared pixel_depth(Width * Height) As Double
Dim Shared pixel_time(Width * Height) As Double
GoSub init_set_up_display
images&(1) = _LoadImage("metal\color.jpg")
images&(2) = _LoadImage("metal\normal.jpg")
images&(3) = _LoadImage("metal\metal.jpg")
images&(4) = _LoadImage("metal\ao.jpg")
Call setProjection(90, 10000, 0.01)
'Do

'Loop
'verticeAttributes(0) = 1: verticeAttributes(1) = 0: verticeAttributes(2) = 0: verticeAttributes(3) = 1: verticeAttributes(4) = 0: verticeAttributes(5) = 0
'verticeAttributes(6) = 0: verticeAttributes(7) = 1: verticeAttributes(8) = 0: verticeAttributes(9) = 1: verticeAttributes(10) = 1: verticeAttributes(11) = 1
'verticeAttributes(12) = 0: verticeAttributes(13) = 0: verticeAttributes(14) = 1: verticeAttributes(15) = 1: verticeAttributes(16) = 1: verticeAttributes(17) = 0
lights(0).intensity = 1
lights(0).position.x = -100
lights(0).position.y = -200
lights(0).position.z = 50
lights(0).position.w = 1
' Call tri(-50, -50, -100, 50, 50, -100, 50, -85, -200)
Dim As vector position, rotation, size, lightrot, lightsize
position.x = 0
position.y = 0
position.z = -200
position.w = 1

size.x = 50
size.y = 50
size.z = 50
size.w = 1

lightrot.x = 0
lightrot.y = 0
lightrot.z = 0
lightrot.w = 0

lightsize.x = 10
lightsize.y = 10
lightsize.z = 10

rotation.x = 0.7
rotation.y = 0.4

Do
    Cls
    'rotation.z = current_frame * 0.05
    rotation.w = 1
    If _KeyDown(119) Then lights(0).position.z = lights(0).position.z + 10
    If _KeyDown(115) Then lights(0).position.z = lights(0).position.z - 10
    If _KeyDown(97) Then lights(0).position.x = lights(0).position.x + 10
    If _KeyDown(100) Then lights(0).position.x = lights(0).position.x - 10
    If _KeyDown(101) Then lights(0).position.y = lights(0).position.y + 10
    If _KeyDown(113) Then lights(0).position.y = lights(0).position.y - 10

    If _KeyDown(19200) Then rotation.y = rotation.y - 0.03
    If _KeyDown(19712) Then rotation.y = rotation.y + 0.03
    If _KeyDown(18432) Then rotation.x = rotation.x - 0.03
    If _KeyDown(20480) Then rotation.x = rotation.x + 0.03
    Color _RGB(255, 255, 255)
    Print lights(0).position.x; lights(0).position.y; lights(0).position.z
    Call cube(position, size, rotation)
    'Color _RGB(255, 255, 255)
    'Print matrix(0); matrix(1); matrix(2); matrix(3)
    'Print matrix(4); matrix(5); matrix(6); matrix(7)
    'Print matrix(8); matrix(9); matrix(10); matrix(11)
    'Print matrix(12); matrix(13); matrix(14); matrix(15)

    'Call cube(lights(0).position, lightsize, lightrot)
    current_frame = current_frame + 1
    _Display
Loop

Color _RGB(255, 255, 255)
End

init_set_up_display:
display = _NewImage(Width, Height, 32)
images&(0) = display
Screen display
Return

Sub cube (position As vector, size As vector, rotation As vector)
    Dim As vector lft, rft, lbt, rbt, lfb, rfb, lbb, rbb

    setVector -size.x, size.y, -size.z, 0
    rotateVector rotation.x, rotation.y, rotation.z
    lft.x = vector(0) + position.x: lft.y = vector(1) + position.y: lft.z = vector(2) + position.z

    setVector size.x, size.y, -size.z, 0
    rotateVector rotation.x, rotation.y, rotation.z
    rft.x = vector(0) + position.x: rft.y = vector(1) + position.y: rft.z = vector(2) + position.z

    setVector -size.x, size.y, size.z, 0
    rotateVector rotation.x, rotation.y, rotation.z
    lbt.x = vector(0) + position.x: lbt.y = vector(1) + position.y: lbt.z = vector(2) + position.z

    setVector size.x, size.y, size.z, 0
    rotateVector rotation.x, rotation.y, rotation.z
    rbt.x = vector(0) + position.x: rbt.y = vector(1) + position.y: rbt.z = vector(2) + position.z

    setVector -size.x, -size.y, -size.z, 0
    rotateVector rotation.x, rotation.y, rotation.z
    lfb.x = vector(0) + position.x: lfb.y = vector(1) + position.y: lfb.z = vector(2) + position.z

    setVector size.x, -size.y, -size.z, 0
    rotateVector rotation.x, rotation.y, rotation.z
    rfb.x = vector(0) + position.x: rfb.y = vector(1) + position.y: rfb.z = vector(2) + position.z

    setVector -size.x, -size.y, size.z, 0
    rotateVector rotation.x, rotation.y, rotation.z
    lbb.x = vector(0) + position.x: lbb.y = vector(1) + position.y: lbb.z = vector(2) + position.z

    setVector size.x, -size.y, size.z, 0
    rotateVector rotation.x, rotation.y, rotation.z
    rbb.x = vector(0) + position.x: rbb.y = vector(1) + position.y: rbb.z = vector(2) + position.z

    'Print lbt.x; lbt.y; lbt.z; lft.x; lft.y; lft.z; lfb.x; lfb.y; lfb.z, lbb.x; lbb.y; lbb.z; lfb.x; lfb.y; lfb.z; lbt.x; lbt.y; lbt.z

    verticeAttributes(0) = 0: verticeAttributes(1) = 0
    verticeAttributes(2) = 1: verticeAttributes(3) = 0
    verticeAttributes(4) = 1: verticeAttributes(5) = 1
    tri_vector lft, rft, rfb
    verticeAttributes(0) = 1: verticeAttributes(1) = 1
    verticeAttributes(2) = 0: verticeAttributes(3) = 1
    verticeAttributes(4) = 0: verticeAttributes(5) = 0
    tri_vector rfb, lfb, lft
    verticeAttributes(0) = 0: verticeAttributes(1) = 0
    verticeAttributes(2) = 1: verticeAttributes(3) = 0
    verticeAttributes(4) = 1: verticeAttributes(5) = 1
    tri_vector rbt, lbt, lbb
    verticeAttributes(0) = 1: verticeAttributes(1) = 1
    verticeAttributes(2) = 0: verticeAttributes(3) = 1
    verticeAttributes(4) = 0: verticeAttributes(5) = 0
    tri_vector lbb, rbb, rbt
    verticeAttributes(0) = 0: verticeAttributes(1) = 0
    verticeAttributes(2) = 1: verticeAttributes(3) = 0
    verticeAttributes(4) = 1: verticeAttributes(5) = 1
    tri_vector lbt, lft, lfb
    verticeAttributes(0) = 1: verticeAttributes(1) = 1
    verticeAttributes(2) = 0: verticeAttributes(3) = 1
    verticeAttributes(4) = 0: verticeAttributes(5) = 0
    tri_vector lfb, lbb, lbt
    verticeAttributes(0) = 0: verticeAttributes(1) = 0
    verticeAttributes(2) = 1: verticeAttributes(3) = 0
    verticeAttributes(4) = 1: verticeAttributes(5) = 1
    tri_vector rft, rbt, rbb
    verticeAttributes(0) = 1: verticeAttributes(1) = 1
    verticeAttributes(2) = 0: verticeAttributes(3) = 1
    verticeAttributes(4) = 0: verticeAttributes(5) = 0
    tri_vector rbb, rfb, rft
    verticeAttributes(0) = 0: verticeAttributes(1) = 0
    verticeAttributes(2) = 1: verticeAttributes(3) = 0
    verticeAttributes(4) = 1: verticeAttributes(5) = 1
    tri_vector rft, lbt, rbt
    verticeAttributes(0) = 1: verticeAttributes(1) = 1
    verticeAttributes(2) = 0: verticeAttributes(3) = 1
    verticeAttributes(4) = 0: verticeAttributes(5) = 0
    tri_vector lbt, rft, lft
    verticeAttributes(0) = 0: verticeAttributes(1) = 0
    verticeAttributes(2) = 1: verticeAttributes(3) = 0
    verticeAttributes(4) = 1: verticeAttributes(5) = 1
    tri_vector lfb, rbb, lbb
    verticeAttributes(0) = 1: verticeAttributes(1) = 1
    verticeAttributes(2) = 0: verticeAttributes(3) = 1
    verticeAttributes(4) = 0: verticeAttributes(5) = 0
    tri_vector rbb, lfb, rfb

End Sub

Sub shade
    Dim As vector diffuseMap, normalMap, tolight, reflect, normal_pos
    Dim As Single reflectMag, roughness, ao
    Call texture2D(images&(1), pixelAttributes(0), pixelAttributes(1))
    copyVectorTo diffuseMap
    Call texture2D(images&(2), pixelAttributes(0), pixelAttributes(1))
    vector(0) = vector(0) * 2 - 1: vector(1) = vector(1) * 2 - 1: vector(2) = vector(2) * 2 - 1
    vector(3) = 0
    applyMatrix
    copyVectorTo normalMap
    Call texture2D(images&(3), pixelAttributes(0), pixelAttributes(1))
    roughness = vector(0)
    Call texture2D(images&(4), pixelAttributes(0), pixelAttributes(1))
    ao = vector(0)

    ' Apply normal map to normals
    'initVector shader_color, normalMap.x, normalMap.y, normalMap.z, 1

    tolight.x = lights(0).position.x - physical_position.x
    tolight.y = lights(0).position.y - physical_position.y
    tolight.z = lights(0).position.z - physical_position.z
    normalizeVector tolight
    diffuse = max(tolight.x * normalMap.x + tolight.y * normalMap.y + tolight.z * normalMap.z, 0) * 0.5

    reflectMag = 2.0 * (-tolight.x * normalMap.x - tolight.y * normalMap.y - tolight.z * normalMap.z)
    reflect.x = -tolight.x - reflectMag * normalMap.x: reflect.y = -tolight.y - reflectMag * normalMap.y: reflect.z = -tolight.z - reflectMag * normalMap.z
    normalizeVector reflect
    normal_pos.x = physical_position.x: normal_pos.y = physical_position.y: normal_pos.z = physical_position.z
    normalizeVector normal_pos
    specular = max(-normal_pos.x * reflect.x - normal_pos.y * reflect.y - normal_pos.z * reflect.z, 0) ^ 32

    diffuseMap.x = diffuseMap.x * (diffuse + specular * roughness + 0.2) * ao
    diffuseMap.y = diffuseMap.y * (diffuse + specular * roughness + 0.2) * ao
    diffuseMap.z = diffuseMap.z * (diffuse + specular * roughness + 0.2) * ao

    'initVector shader_color, 0.1 + diffuse + specular, 0.1 + diffuse + specular, 0.1 + diffuse + specular, 1
    initVector shader_color, diffuseMap.x, diffuseMap.y, diffuseMap.z, 1
    'initVector shader_color, specular, specular, specular, 1
    'initVector shader_color, reflect.x, reflect.y, reflect.z, 1
    ' Print vector(2)
End Sub

' Returns the max between two numbers
Function max (ma_x, ma_y)
    max = ma_x
    If ma_y > ma_x Then max = ma_y
End Function

' Returns the min between two numbers
Function min (mi_x, mi_y)
    min = mi_x
    If mi_y < mi_x Then min = mi_y
End Function


Function cap (in As Double, minimum As Double, maximum As Double)
    cap = min(max(in, minimum), maximum)
End Function

Sub applyMatrix
    vector(0) = vector(0) * matrix(0) + vector(1) * matrix(1) + vector(2) * matrix(2) + vector(3) * matrix(3)
    vector(1) = vector(0) * matrix(4) + vector(1) * matrix(5) + vector(2) * matrix(6) + vector(3) * matrix(7)
    vector(2) = vector(0) * matrix(8) + vector(1) * matrix(9) + vector(2) * matrix(10) + vector(3) * matrix(11)
    vector(3) = vector(0) * matrix(12) + vector(1) * matrix(13) + vector(2) * matrix(14) + vector(3) * matrix(15)
End Sub

Sub setVector (x As Double, y As Double, z As Double, w As Double)
    vector(0) = x
    vector(1) = y
    vector(2) = z
    vector(3) = w
End Sub

Sub initVector (invector As vector, x As Double, y As Double, z As Double, w As Double)
    invector.x = x
    invector.y = y
    invector.z = z
    invector.w = w
End Sub

Sub setProjection (FOVAngle As Double, Far As Double, Near As Double)
    projection_data(0) = 0.5 / Tan(FOVAngle * 3.14159267 / 360.0)
    projection_data(1) = Width * projection_data(0)
    projection_data(2) = -Far / (Far - Near)
    projection_data(3) = -(Far * Near) / (Far - Near)
End Sub

Sub projectVector ' Leaves z as 1/z
    vector(2) = 1 / (vector(2) * projection_data(2) + vector(3) * projection_data(3))
    vector(0) = vector(0) * vector(2) * projection_data(1) + vector(3) * 0.5 * Width
    vector(1) = vector(1) * vector(2) * projection_data(1) + vector(3) * 0.5 * Height
End Sub

Sub normalizeVector (invector As vector)
    mag = Sqr(invector.x ^ 2 + invector.y ^ 2 + invector.z ^ 2)
    invector.x = invector.x / mag: invector.y = invector.y / mag: invector.z = invector.z / mag
End Sub

Sub rotateVector (rx As Double, ry As Double, rz As Double)
    Dim nvector As vector
    Call initVector(nvector, 0, 0, 0, 0)
    nvector.x = (vector(0) * Cos(rz) * Cos(ry)) + (vector(1) * Sin(rz) * Cos(ry)) - vector(2) * Sin(ry)
    nvector.y = (vector(0) * (Cos(rz) * Sin(ry) * Sin(rx) - Sin(rz) * Cos(rx))) + (vector(1) * (Sin(rz) * Sin(ry) * Sin(rx) + Cos(rz) * Cos(rx))) + (vector(2) * Cos(ry) * Sin(rx))
    nvector.z = (vector(0) * (Cos(rz) * Sin(ry) * Cos(rx) + Sin(rz) * Sin(rx))) + (vector(1) * (Sin(rz) * Sin(ry) * Cos(rx) - Cos(rz) * Sin(rx))) + (vector(2) * Cos(ry) * Cos(rx))
    nvector.w = 1
    copyToVector nvector
End Sub

Sub copyVectorTo (invector As vector)
    invector.x = vector(0)
    invector.y = vector(1)
    invector.z = vector(2)
    invector.w = vector(3)
End Sub

Sub copyToVector (outvector As vector)
    vector(0) = outvector.x
    vector(1) = outvector.y
    vector(2) = outvector.z
    vector(3) = outvector.w
End Sub

' Gravs color
Sub texture2D (image&, u As Double, v As Double)
    _Source image&
    GRABBED_COLOR~& = Point(Fix(cap(u, 0, 1) * (_Width(image&) - 1)), Fix(cap(v, 0, 1) * (_Height(image&) - 1)))
    vector(0) = (Fix(GRABBED_COLOR~& / 65536) Mod 256) / 256.0
    vector(1) = (Fix(GRABBED_COLOR~& / 256) Mod 256) / 256.0
    vector(2) = (Fix(GRABBED_COLOR~&) Mod 256) / 256.0
    vector(3) = (Fix(GRABBED_COLOR~& / 16777216) Mod 256) / 256.0
End Sub

Sub tri_flat_term (x1, y1, x2, x3, term)
    'render_data(5) = Sgn(y1 - term)
    render_data(2) = x2 ' Set left x that will be walking line  (x2,term)-(x1,y1)
    render_data(3) = x3 ' Set right x that will be walking line (x3,term)-(x1,y1)
    If (x2 > x3) Then render_data(3) = x2: render_data(2) = x3
    render_data(0) = (render_data(2) - x1) / (term - y1) ' Negative left inverse slope depending on if y will be increasing or decreasing
    render_data(1) = (render_data(3) - x1) / (term - y1) ' Negative right inverse slope for same reason

    render_data(4) = term
    render_data(5) = y1
    If (y1 < term) Then render_data(4) = y1: render_data(5) = term: render_data(2) = x1: render_data(3) = x1
    'render_data(4) = Sgn(render_data(1) - render_data(0)) ' Which direction x will need to increment in, + or -

    ' Set uvw
    tri_data(14) = ((tri_data(6) - render_data(2)) * (tri_data(4) - render_data(4)) - (tri_data(7) - render_data(4)) * (tri_data(3) - render_data(2))) * 0.5 * tri_data(13) 'u = ((x3-starting x)*(y2-starting y) - (y3-starting y)*(x2-starting x)) * 0.5 * 1/tri area
    tri_data(15) = ((tri_data(0) - render_data(2)) * (tri_data(7) - render_data(4)) - (tri_data(1) - render_data(4)) * (tri_data(6) - render_data(2))) * 0.5 * tri_data(13) 'v = ((x1-starting x)*(y3-starting y) - (y1-starting y)*(x3-starting x)) * 0.5 * 1/tri area
    tri_data(16) = ((tri_data(3) - render_data(2)) * (tri_data(1) - render_data(4)) - (tri_data(4) - render_data(4)) * (tri_data(0) - render_data(2))) * 0.5 * tri_data(13) 'w = ((x2-starting x)*(y1-starting y) - (y2-starting y)*(x1-starting x)) * 0.5 * 1/tri area
    ' Set duvw/dx
    render_data(6) = (tri_data(7) - tri_data(4)) * 0.5 * tri_data(13) ' du/dx = y3 - y2
    render_data(7) = (tri_data(1) - tri_data(7)) * 0.5 * tri_data(13) ' du/dx = y1 - y3
    render_data(8) = (tri_data(4) - tri_data(1)) * 0.5 * tri_data(13) ' du/dx = y2 - y1
    ' Set duvw/dy
    render_data(9) = (tri_data(3) - tri_data(6)) * 0.5 * tri_data(13) ' du/dy = x2 - x3
    render_data(10) = (tri_data(6) - tri_data(0)) * 0.5 * tri_data(13) ' du/dy = x3 - x1
    render_data(11) = (tri_data(0) - tri_data(3)) * 0.5 * tri_data(13) ' du/dy = x1 - x2
    'Print render_data(9); render_data(6); tri_data(14), render_data(10); render_data(7); tri_data(15), render_data(11); render_data(8); tri_data(16)
    For pixel_y = render_data(4) To render_data(5) - 1
        uvwxinctimes = 0
        If pixel_y < 0 Or pixel_y >= Height Then GoTo tri_flat_term_yskip
        'pixel_x = render_data(2) ' Loop until pixel_x = render_data(3)

        'tri_data(14) = ((tri_data(6) - pixel_x) * (tri_data(4) - pixel_y) - (tri_data(7) - pixel_y) * (tri_data(3) - pixel_x)) * 0.5 * tri_data(13) 'u = ((x3-starting x)*(y2-starting y) - (y3-starting y)*(x2-starting x)) * 0.5 * 1/tri area
        'tri_data(15) = ((tri_data(0) - pixel_x) * (tri_data(7) - pixel_y) - (tri_data(1) - pixel_y) * (tri_data(6) - pixel_x)) * 0.5 * tri_data(13) 'v = ((x1-starting x)*(y3-starting y) - (y1-starting y)*(x3-starting x)) * 0.5 * 1/tri area
        'tri_data(16) = ((tri_data(3) - pixel_x) * (tri_data(1) - pixel_y) - (tri_data(4) - pixel_y) * (tri_data(0) - pixel_x)) * 0.5 * tri_data(13) 'w = ((x2-starting x)*(y1-starting y) - (y2-starting y)*(x1-starting x)) * 0.5 * 1/tri area
        For pixel_x = render_data(2) To render_data(3)
            'If uvwxinctimes = 0 Then Print pixel_x; render_data(2)
            tri_data(14) = tri_data(14) + render_data(6)
            tri_data(15) = tri_data(15) + render_data(7)
            tri_data(16) = tri_data(16) + render_data(8)

            If pixel_x < 0 Or pixel_x >= Width Then GoTo tri_flat_term_xskip

            render_data(12) = pixel_y * Width + pixel_x
            render_data(13) = 1 / (tri_data(14) * tri_data(9) + tri_data(15) * tri_data(10) + tri_data(16) * tri_data(11))
            If pixel_depth(render_data(12)) > render_data(13) Or pixel_time(render_data(12)) < current_frame Then
                pixel_depth(render_data(12)) = render_data(13)
                pixel_time(render_data(12)) = current_frame
                For i = 0 To (NUM_OF_ATTRIBUTES - 1)
                    pixelAttributes(i) = verticeAttributes(0 * NUM_OF_ATTRIBUTES + i) * tri_data(14) * tri_data(9) * render_data(13) + verticeAttributes(1 * NUM_OF_ATTRIBUTES + i) * tri_data(15) * tri_data(10) * render_data(13) + verticeAttributes(2 * NUM_OF_ATTRIBUTES + i) * tri_data(16) * tri_data(11) * render_data(13)
                Next
                physical_position.x = tri_data(20) * tri_data(14) * tri_data(9) * render_data(13) + tri_data(23) * tri_data(15) * tri_data(10) * render_data(13) + tri_data(26) * tri_data(16) * tri_data(11) * render_data(13)
                physical_position.y = tri_data(21) * tri_data(14) * tri_data(9) * render_data(13) + tri_data(24) * tri_data(15) * tri_data(10) * render_data(13) + tri_data(27) * tri_data(16) * tri_data(11) * render_data(13)
                physical_position.z = -render_data(13)
                physical_position.w = 1
                shade
                Color _RGBA(shader_color.x * 256, shader_color.y * 256, shader_color.z * 256, shader_color.w * 256)
                PSet (pixel_x, pixel_y)
            Else
                'Print "Depth bypass"; pixel_x; pixel_y; render_data(13)
            End If
            tri_flat_term_xskip:
        Next

        tri_flat_term_yskip:
        tri_data(14) = tri_data(14) + render_data(9) + render_data(0) * render_data(6) - (CInt(render_data(3)) - CInt(render_data(2)) + 1) * render_data(6)
        tri_data(15) = tri_data(15) + render_data(10) + render_data(0) * render_data(7) - (CInt(render_data(3)) - CInt(render_data(2)) + 1) * render_data(7)
        tri_data(16) = tri_data(16) + render_data(11) + render_data(0) * render_data(8) - (CInt(render_data(3)) - CInt(render_data(2)) + 1) * render_data(8)

        render_data(2) = render_data(2) + render_data(0) 'left x = left x + left inverse slope
        render_data(3) = render_data(3) + render_data(1) 'right x = right x + right inverse slope
    Next
End Sub

Sub tri (x1, y1, z1, x2, y2, z2, x3, y3, z3)
    'Project all 3 vertices to pixel-coordinates
    tri_data(20) = x1: tri_data(21) = y1: tri_data(22) = z1
    tri_data(23) = x2: tri_data(24) = y2: tri_data(25) = z2
    tri_data(26) = x3: tri_data(27) = y3: tri_data(28) = z3
    setVector x1, y1, z1, 1
    projectVector
    tri_data(0) = vector(0): tri_data(1) = vector(1): tri_data(9) = vector(2)
    setVector x2, y2, z2, 1
    projectVector
    tri_data(3) = vector(0): tri_data(4) = vector(1): tri_data(10) = vector(2)
    setVector x3, y3, z3, 1
    projectVector
    tri_data(6) = vector(0): tri_data(7) = vector(1): tri_data(11) = vector(2)

    ' Compute extra triangle data
    tri_data(12) = ((tri_data(6) - tri_data(0)) * (tri_data(4) - tri_data(1)) - (tri_data(7) - tri_data(1)) * (tri_data(3) - tri_data(0))) * 0.5 'Area = ((x3 - x1) * (y2 - y1) - (y3 - y1) * (x2 - x1)) * 0.5
    tri_data(13) = 1 / tri_data(12) '1/Area

    ' if any of the y coordinates are the same, draw a flat terminating triangle instead of splitting it
    ' if y1=y2 then tri_flat_term x3, y3, x1, x2, terminating at y1
    ' if y2=y3 then tri_flat_term x1, y1, x2, x3, terminating at y2
    ' if y3=y1 then tri_flat_term x2, y2, x3, x1, terminating at y3
    If tri_data(1) = tri_data(4) Then tri_flat_term tri_data(6), tri_data(7), tri_data(0), tri_data(3), tri_data(1): GoTo tri_skip
    If tri_data(4) = tri_data(7) Then tri_flat_term tri_data(0), tri_data(1), tri_data(3), tri_data(6), tri_data(4): GoTo tri_skip
    If tri_data(7) = tri_data(1) Then tri_flat_term tri_data(3), tri_data(4), tri_data(6), tri_data(0), tri_data(7): GoTo tri_skip
    Dim m
    ' Check which vertice is in the middle of the triangle in terms of y
    If (tri_data(1) < tri_data(4) And tri_data(1) > tri_data(7)) Or (tri_data(1) > tri_data(4) And tri_data(1) < tri_data(7)) Then
        m = tri_data(3) + (tri_data(1) - tri_data(4)) * (tri_data(6) - tri_data(3)) / (tri_data(7) - tri_data(4)) 'x2+(y1-y2)*(x3-x2)/(y3-y2)
        ' triangle (x3, y3, x1, m, y1) + triangle(x2, y2, x1, m, y1)
        tri_flat_term tri_data(6), tri_data(7), tri_data(0), m, tri_data(1): tri_flat_term tri_data(3), tri_data(4), tri_data(0), m, tri_data(1): GoTo tri_skip
    ElseIf (tri_data(4) < tri_data(1) And tri_data(4) > tri_data(7)) Or (tri_data(4) > tri_data(1) And tri_data(4) < tri_data(7)) Then
        m = tri_data(0) + (tri_data(4) - tri_data(1)) * (tri_data(6) - tri_data(0)) / (tri_data(7) - tri_data(1)) 'x1+(y2-y1)*(x3-x1)/(y3-y1)
        ' triangle (x3, y3, x2, m, y2) + triangle(x1, y1, x2, m, y2)
        tri_flat_term tri_data(6), tri_data(7), tri_data(3), m, tri_data(4): tri_flat_term tri_data(0), tri_data(1), tri_data(3), m, tri_data(4): GoTo tri_skip
    Else
        m = tri_data(0) + (tri_data(7) - tri_data(1)) * (tri_data(3) - tri_data(0)) / (tri_data(4) - tri_data(1)) 'x1+(y3-y1)*(x2-x1)/(y2-y1)
        ' triangle (x2, y2, x3, m, y3) + triangle(x1, y1, x3, m, y3)
        tri_flat_term tri_data(3), tri_data(4), tri_data(6), m, tri_data(7): tri_flat_term tri_data(0), tri_data(1), tri_data(6), m, tri_data(7)
    End If

    tri_skip:
End Sub

Sub tri_vector (a As vector, b As vector, c As vector)
    tri_data(17) = ((b.y - a.y) * (c.z - a.z) - (b.z - a.z) * (c.y - a.y))
    tri_data(18) = ((b.z - a.z) * (c.x - a.x) - (b.x - a.x) * (c.z - a.z))
    tri_data(19) = ((b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x))
    mag = Sqr(tri_data(17) ^ 2 + tri_data(18) ^ 2 + tri_data(19) ^ 2)
    tri_data(17) = tri_data(17) / mag
    tri_data(18) = tri_data(18) / mag
    tri_data(19) = tri_data(19) / mag
    deltaUV1x = verticeAttributes(2) - verticeAttributes(0): deltaUV1y = verticeAttributes(3) - verticeAttributes(1)
    deltaUV2x = verticeAttributes(4) - verticeAttributes(0): deltaUV2y = verticeAttributes(5) - verticeAttributes(1)
    f = 1.0 / (deltaUV1x * deltaUV2y - deltaUV2x * deltaUV1y)

    Dim As vector tang, bitan
    tang.x = f * (deltaUV2y * (b.x - a.x) - deltaUV1y * (c.x - a.x))
    tang.y = f * (deltaUV2y * (b.y - a.y) - deltaUV1y * (c.y - a.y))
    tang.z = f * (deltaUV2y * (b.z - a.z) - deltaUV1y * (c.z - a.z))
    normalizeVector tang
    bitan.x = f * (-deltaUV2x * (b.x - a.x) + deltaUV1x * (c.x - a.x))
    bitan.y = f * (-deltaUV2x * (b.y - a.y) + deltaUV1x * (c.y - a.y))
    bitan.z = f * (-deltaUV2x * (b.z - a.z) + deltaUV1x * (c.z - a.z))
    normalizeVector bitan
    matrix(0) = tang.x: matrix(1) = tang.y: matrix(2) = tang.z
    matrix(4) = bitan.x: matrix(5) = bitan.y: matrix(6) = bitan.z
    matrix(8) = tri_data(17): matrix(9) = tri_data(18): matrix(10) = tri_data(19)
    tri a.x, a.y, a.z, b.x, b.y, b.z, c.x, c.y, c.z
End Sub
