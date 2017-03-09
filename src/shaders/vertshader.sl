#version 120

attribute vec3 aNormal;

varying vec4 vColor;
varying vec3 vEyeCoords;
varying vec3 vNormal;

void main() {
  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
  vEyeCoords = (gl_ModelViewMatrix * gl_Vertex).xyz;
  vNormal = (gl_ModelViewMatrix * vec4(aNormal, 0.0)).xyz;
  vColor = gl_Color;
}
