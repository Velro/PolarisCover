#version 150

layout (points) in;
layout (triangle_strip) out;
layout (max_vertices = 4 ) out;

uniform mat4 projection;

void main()
{
  vec4 p = gl_in[0].gl_Position;

  vec2 a = p.xy + vec2(-0.5,-0.5);
  gl_Position = projection * vec4(a,p.zw);
  EmitVertex();

  vec2 b = p.xy + vec2(-0.5,0.5);
  gl_Position = projection * vec4(b,p.zw);
  EmitVertex();

  vec2 c = p.xy + vec2(0.5,-0.5);
  gl_Position = projection * vec4(c,p.zw);
  EmitVertex();

  vec2 d = p.xy + vec2(0.5,0.5);

  gl_Position = projection * vec4(d,p.zw);
  EmitVertex();

  EndPrimitive();
}
