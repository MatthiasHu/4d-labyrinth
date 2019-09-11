#version 120

varying vec2 vPosition;

void main() {
  gl_Position = gl_Vertex;
  vPosition = gl_Vertex.xy;
}
