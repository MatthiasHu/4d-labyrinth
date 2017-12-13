#version 120

uniform vec3 uObjectCenter;
uniform float uObjectInnerRadius;

varying vec4 vColor;
varying vec3 vEyeCoords;
varying vec3 vNormal;

void main() {
  float eyeDist = length(vEyeCoords);
  float frontality =
    max(0.0, dot(normalize(vEyeCoords), -vNormal));
  float bright = 0.3/(1.0+0.3*eyeDist)
               + 0.7*frontality/(1.0+0.6*eyeDist);

  float centerDist = length(vEyeCoords-uObjectCenter);
  float bright2 = centerDist < uObjectInnerRadius ? 1.0 : 0.9;

  gl_FragColor = bright2 * bright * vColor;
}
