#version 120

varying vec3 vEyeCoordsBoundingBox;

void main() {
  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
  vEyeCoordsBoundingBox = (gl_ModelViewMatrix * gl_Vertex).xyz;
}
