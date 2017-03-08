#version 120

varying vec4 vColor;
varying vec3 vEyeCoords;

void main() {
  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
  vEyeCoords = (gl_ModelViewMatrix * gl_Vertex).xyz;
  vColor = gl_Color;
}
