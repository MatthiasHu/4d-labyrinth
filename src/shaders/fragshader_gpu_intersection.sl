#version 120

#define max_hyperplanes 24

uniform vec3 uHyperplaneNormals[max_hyperplanes];
uniform float uHyperplaneValues[max_hyperplanes];
uniform vec3 uHyperplaneColors[max_hyperplanes];

varying vec3 vEyeCoordsBoundingBox;

float lighting(vec3 eyeCoords, vec3 normal);

void main() {
  vec3 ray = normalize(vEyeCoordsBoundingBox);

  float t_0 = -1; // time entering the polytope
  float t_1 = 100000; // time leaving the polytope
  int k = 0; // hyperplane being hit
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
    gl_FragColor =
      lighting(t_0 * ray, normalize(uHyperplaneNormals[k]))
      * vec4(uHyperplaneColors[k], 1);
    gl_FragDepth = 1 - 1/(1+t_0);
  }
  else {
    // put a different color here to see covered area
    gl_FragColor = vec4(0, 0, 0, 1);
    gl_FragDepth = 1;
  }
}
