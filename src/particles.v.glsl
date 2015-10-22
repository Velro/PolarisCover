#version 150

attribute vec3 coord3d;
uniform mat4 mv;

void main(void)
{
  gl_Position = mv * vec4(coord3d, 1.0);
  //gl_Position = vec4(coord3d, 1.0);
}
