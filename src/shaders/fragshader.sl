#version 120

varying vec4 vColor;
varying vec3 vEyeCoords;
varying vec3 vNormal;

void main() {
  float dist = length(vEyeCoords);
  float frontality =
    max(0.0, dot(normalize(vEyeCoords), -vNormal));
  float bright = 0.3/(1.0+0.3*dist) + 0.7*frontality/(1.0+0.6*dist);
  gl_FragColor = bright * vColor;
}
