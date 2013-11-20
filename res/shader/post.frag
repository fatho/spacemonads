#version 130
uniform sampler2D tex;
uniform float time;

uniform float bloom = 0.2;
uniform float bloomRnd = 0.1;

uniform int nScanLines = 400;

in vec2 texCoordFrag;
out vec4 fragColor;

const float pi = 3.1415926; 
const float m_pi_2 = 1.570963; 

vec4 pixelate(sampler2D tex, vec2 uv, float new_w, float new_h)
{
  vec2 coord = vec2( ceil(uv.x * new_w) / new_w,
  ceil(uv.y * new_h) / new_h );
  return texture2D(tex, coord);
}

vec4 reduce_palette(vec4 color, float max_colors_per_channel)
{
  if(max_colors_per_channel < 0) {
          return color;
  }
  
  return ceil(color * max_colors_per_channel) / max_colors_per_channel;
}

///////////////////////////////////////////////////////////////////////////////
// RANDOM NUMBER GENERATOR (FOUND ON THE INTERNET)
///////////////////////////////////////////////////////////////////////////////
float rand(vec2 co){
    return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}


///////////////////////////////////////////////////////////////////////////////
// CURVATURE
///////////////////////////////////////////////////////////////////////////////
// Apply radial distortion to the given coordinate.
vec2 radialDistortion(vec2 coord)
{
  const float distortion = 0.1;
  vec2 cc = coord - 0.5;
  float dist = dot(cc, cc) * distortion;
  return (coord + cc * (1.0 + dist) * dist);
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

vec2 randomSineDistortion(vec2 texCoord)
{
    float stime = time * 1.5;

    float ty = 1 - (stime - floor(stime));
    vec2 distortion = vec2(0,0);

    if(rand(vec2(floor(stime), floor(stime))) < 0.1)
    {
      float w = exp(-pow(texCoord.y - ty,2.0) / (2*pow(0.05,2.0)) );

      texCoord.x += sin((texCoord.y - ty)*10) * 0.05 * w;
    }
    return texCoord;
}

///////////////////////////////////////////////////////////////////////////////
// SCANLINES
///////////////////////////////////////////////////////////////////////////////

vec4 tex2DScan(sampler2D tex, vec2 texCoord) 
{
  float scanLineY = round(texCoord.y * nScanLines) / nScanLines;
  float wScan = exp(-pow(texCoord.y - scanLineY,2.0) / (2*pow(0.0005,2.0)) );
  vec4 col = texture2D(tex, texCoord);
  return (col * 0.95 + 0.05) * wScan;
}


void main() {
  vec2 texCoord = texCoordFrag;

  //texCoord = (texCoord - vec2(0.5, 0.5)) * 1 + vec2(0.5, 0.5);

  // fish eye 
  texCoord = radialDistortion(texCoord);//fishEye(texCoord);

  if(texCoord.x < 0 || texCoord.y < 0 || texCoord.x > 1 || texCoord.y > 1) 
  {
    fragColor = vec4(0.5,0.5,0.5,1);
  }
  else
  {
    texCoord = randomSineDistortion(texCoord);
    // bloom
    vec4 sum = vec4(0);
    int j;
    int i;
    float bloomFactor = (rand(vec2(time, time)) * bloomRnd + bloom);

    for( i= -4 ;i <= 4; i++)
    {
        for (j = -3; j <= 3; j++)
        {
            sum += tex2DScan(tex, texCoord + vec2(j, i)*0.0015) * bloomFactor;
        }
    }
    vec4 curColor = tex2DScan(tex, texCoord);
    if (curColor.r < 0.3)
    {
       fragColor = sum*sum*0.012 + curColor;
    }
    else
    {
        if (curColor.r < 0.5)
        {
            fragColor = sum*sum*0.009 + curColor;
        }
        else
        {
            fragColor = sum*sum*0.0075 + curColor;
        }
    }
  }


  //vec4 texcol = texture2D(tex, texCoord);
  //vec4 texcol = pixelate(tex, texCoord, 400, 300);
  //fragColor = reduce_palette(fragColor, 16);
}
