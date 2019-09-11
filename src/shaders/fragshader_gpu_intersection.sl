#version 120

uniform vec3 uHyperplaneNormals[24];
uniform float uHyperplaneValues[24];

varying vec2 vPosition;

void main() {
  gl_FragColor = vec4(vPosition, 0, 1);
}
