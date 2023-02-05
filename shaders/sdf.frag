#version 430 core

uniform vec2 iResolution;

float distance_from_sphere(in vec3 p, in vec3 c, float r)
{
	return length(p - c) - r;
}

float map_the_world(in vec3 p)
{
    float sphere_0 = distance_from_sphere(p, vec3(0.0), 1.0);

    // Later we might have sphere_1, sphere_2, cube_3, etc...

    return sphere_0;
}

vec3 calculate_normal(in vec3 p)
{
    const vec3 small_step = vec3(0.001, 0.0, 0.0);

    float gradient_x = map_the_world(p + small_step.xyy) - map_the_world(p - small_step.xyy);
    float gradient_y = map_the_world(p + small_step.yxy) - map_the_world(p - small_step.yxy);
    float gradient_z = map_the_world(p + small_step.yyx) - map_the_world(p - small_step.yyx);

    vec3 normal = vec3(gradient_x, gradient_y, gradient_z);

    return normalize(normal);
}

vec3 ray_march(in vec3 ro, in vec3 rd)
{
    float total_distance_traveled = 0.0;
    const int NUMBER_OF_STEPS = 32;
    const float MINIMUM_HIT_DISTANCE = 0.001;
    const float MAXIMUM_TRACE_DISTANCE = 1000.0;

    for (int i = 0; i < NUMBER_OF_STEPS; ++i)
    {
        // Calculate our current position along the ray
        vec3 current_position = ro + total_distance_traveled * rd;

        // We wrote this function earlier in the tutorial -
        // assume that the sphere is centered at the origin
        // and has unit radius
        float distance_to_closest = map_the_world(current_position);

        if (distance_to_closest < MINIMUM_HIT_DISTANCE) // hit
        {
            vec3 normal = calculate_normal(current_position);
            vec3 light_position = vec3(3.0, -3.0, 5.0);
            vec3 direction_to_light = normalize(current_position - light_position);
            float diffuse_intensity = max(0.0, dot(normal, direction_to_light));
            
            return vec3(1.0, 0.0, 0.0) * diffuse_intensity;
        }

        if (total_distance_traveled > MAXIMUM_TRACE_DISTANCE) // miss
        {
            break;
        }

        // accumulate the distance traveled thus far
        total_distance_traveled += distance_to_closest;
    }

    // If we get here, we didn't hit anything so just
    // return a background color (black)
    return vec3(1.3 - (ro + rd.y + 0.3).y - 0.2, 1.3 - (ro + rd.y + 0.3).y - 0.2, 1.3 - (ro + rd.y + 0.3).y + 0.3);
}

out vec4 fColor;
void main()
{
    vec2 fragCoord = gl_FragCoord.xy;
    // Normalized pixel coordinates (from 0 to 1)
    vec2 uv = fragCoord / iResolution.xy;
    uv = fragCoord - iResolution.xy*0.5;
    uv /= iResolution.x;
    
    vec3 camera_position = vec3(0.0, 0.0, -5.0);
    vec3 ro = camera_position;
    vec3 rd = vec3(uv, 1.0);
    vec3 shaded_color = ray_march(ro, rd);

    // Output to screen
    fColor = vec4(shaded_color, 1.0);
}