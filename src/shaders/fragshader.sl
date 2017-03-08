#version 120

varying vec4 vColor;
varying vec3 vEyeCoords;

void main() {
  float dist = length(vEyeCoords);
  float bright = min(1.0, 0.5/dist);
  gl_FragColor = bright * vColor;
}
