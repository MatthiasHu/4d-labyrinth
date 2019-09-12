#version 120

#define max_hyperplanes 24

uniform vec3 uHyperplaneNormals[max_hyperplanes];
uniform float uHyperplaneValues[max_hyperplanes];

varying vec3 vEyeCoords;

void main() {
  vec3 ray = normalize(vEyeCoords);

  float t_0 = -1; // time entering the polytope
  float t_1 = 100000; // time leaving the polytope
  int k = 0;
  for (int i = 0; i < max_hyperplanes; i++) {
    float d = dot(ray, uHyperplaneNormals[i]);
    float t = uHyperplaneValues[i] / d;
    if (d < 0) { // entering
      if (t > t_0) {
        t_0 = t;
        k = i;
      }
    }
    else { // leaving
      t_1 = min(t_1, t);
    }
  }

  if (t_0 <= t_1 && t_0 > 0) {
    gl_FragColor = vec4(float(k)/max_hyperplanes, 1-float(k)/max_hyperplanes, 1, 1);
    gl_FragDepth = 1 - 1/(1+t_0);
  }
  else {
    gl_FragColor = vec4(0, 0, 0, 1);
    gl_FragDepth = 1;
  }
}
