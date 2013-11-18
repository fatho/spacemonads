#version 130
uniform sampler2D tex;
in vec2 texCoordFrag;
out vec4 fragColor;

void main() {
  fragColor = texture2D(tex, texCoordFrag);
}
