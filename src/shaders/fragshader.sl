#version 120

uniform vec3 uObjectCenter;
uniform float uObjectInnerRadius;

varying vec4 vColor;
varying vec3 vEyeCoords;
varying vec3 vNormal;

float lighting(vec3 eyeCoords, vec3 normal);

void main() {
  float centerDist = length(vEyeCoords-uObjectCenter);
  float texture = centerDist < uObjectInnerRadius ? 1.0 : 0.9;

  gl_FragColor = texture * lighting(vEyeCoords, vNormal) * vColor;
}
