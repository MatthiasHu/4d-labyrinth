float lighting(vec3 eyeCoords, vec3 normal) {
  float eyeDist = length(eyeCoords);
  float frontality =
    max(0.0, dot(normalize(eyeCoords), -normal));
  float bright = 0.3/(1.0+0.3*eyeDist)
               + 0.7*frontality/(1.0+0.6*eyeDist);
  return bright;
}
