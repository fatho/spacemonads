#version 130
uniform mat3 cam;
in vec2 vertexCoord;
in vec2 texCoord;
out vec2 texCoordFrag;

void main() {
  texCoordFrag = texCoord;
  vec3 tmp = cam * vec3(vertexCoord, 1);
  gl_Position = vec4(tmp.xy, 0, 1);
}
