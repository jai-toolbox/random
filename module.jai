#import "Random";
#import "Basic";
#import "Math";


random_choice :: (arr: [] $T) -> T {
    assert(arr.count > 0, "Cannot pick from an empty array.");
    index := random_get() % cast(u64) arr.count;
    return arr[index];
}

random_seed_from_time :: () {
    now := current_time_monotonic();
    seed: Random_State;
    seed.low  = cast(u64) now.low | 1;  // Must be odd
    seed.high = cast(u64) now.high;
    random_seed(seed);
}

random_seed_from_time :: (state: *Random_State) {
    now := current_time_monotonic();
    seed: Random_State;
    seed.low  = cast(u64) now.low | 1;
    seed.high = cast(u64) now.high;
    random_seed(state, seed);
}

random_get_within_negative_one_to_one :: () -> float {
    return random_get_within_range(-1.0, 1.0);
}

random_get_within_negative_one_to_one :: (state: *Random_State) -> float {
    return random_get_within_range(state, -1.0, 1.0);
}

random_get_within_unit_cube :: () -> Vector3 {
    return .{
        random_get_within_negative_one_to_one(),
        random_get_within_negative_one_to_one(),
        random_get_within_negative_one_to_one()
    };
}

random_get_within_unit_cube :: (state: *Random_State) -> Vector3 {
    return .{
        random_get_within_negative_one_to_one(state),
        random_get_within_negative_one_to_one(state),
        random_get_within_negative_one_to_one(state)
    };
}

/*
Returns a random point inside the unit cube [-1,1]^3, but rejected if
its length falls outside the range [min_len, max_len].
*/
random_get_within_unit_cube_length_constrained :: (min_len: float = 0.01, max_len: float = 1.0) -> Vector3 {
    min_len_sq := min_len * min_len;
    max_len_sq := max_len * max_len;

    while true {
        v := random_get_within_unit_cube();

        len_sq := length_squared(v);

        if len_sq < min_len_sq || len_sq > max_len_sq continue;

        return v;
    }

    return .{0.5, 0.5, 0.5};
}

random_get_within_unit_cube_length_constrained :: (state: *Random_State, min_len: float = 0.01, max_len: float = 1.0) -> Vector3 {
    min_len_sq := min_len * min_len;
    max_len_sq := max_len * max_len;

    while true {
        v := random_get_within_unit_cube(state);

        len_sq := length_squared(v);

        if len_sq < min_len_sq || len_sq > max_len_sq continue;

        return v;
    }

    return .{0.5, 0.5, 0.5};
}

/*
Returns a random point inside the positive unit cube [0,1]^3, but rejected if
its length falls outside the range [min_len, max_len].
*/
random_get_within_positive_unit_cube :: () -> Vector3 {
    return .{
        random_get_zero_to_one(),
        random_get_zero_to_one(),
        random_get_zero_to_one()
    };
}

random_get_within_positive_unit_cube :: (state: *Random_State) -> Vector3 {
    return .{
        random_get_zero_to_one(state),
        random_get_zero_to_one(state),
        random_get_zero_to_one(state)
    };
}

random_get_within_positive_unit_cube_length_constrained :: (min_len: float = 0.01, max_len: float = 1.0) -> Vector3 {
    min_len_sq := min_len * min_len;
    max_len_sq := max_len * max_len;

    while true {
        v := random_get_within_positive_unit_cube();

        len_sq := length_squared(v);

        if len_sq < min_len_sq || len_sq > max_len_sq continue;

        return v;
    }

    return .{0.5, 0.5, 0.5};
}

random_get_within_positive_unit_cube_length_constrained :: (state: *Random_State, min_len: float = 0.01, max_len: float = 1.0) -> Vector3 {
    min_len_sq := min_len * min_len;
    max_len_sq := max_len * max_len;

    while true {
        v := random_get_within_positive_unit_cube(state);

        len_sq := length_squared(v);

        if len_sq < min_len_sq || len_sq > max_len_sq continue;

        return v;
    }

    return .{0.5, 0.5, 0.5};
}


/*
We want a uniformly distributed random direction on the unit sphere.

If we just picked a random point in the cube [-1,1]^3 and normalized it,
directions toward the corners of the cube would be overrepresented,
because the corners are farther from the origin, meaning more volume
of the cube exists in those directions.

To fix this, we use rejection sampling: we pick random points in the cube,
but throw away any that fall outside the unit sphere. What remains are
points uniformly distributed inside a sphere, which is symmetric in all
directions. When we normalize those points onto the surface, every
direction is equally likely.

We also reject points too close to the origin, because normalizing
a near-zero vector is numerically unstable.
*/
random_unit_vector :: () -> Vector3 {
    while true {
        v := random_get_within_unit_cube();

        len_sq := length_squared(v);

        // as mentioned, we do this to reject corner bias
        if len_sq > 1.0 continue;

        // reject to avoid numerical instability
        if len_sq < 0.0001 continue;

        return unit_vector(v);
    }

    // unreachable, but just in case:
    return .{0, 0, 1};
}

random_unit_vector :: (state: *Random_State) -> Vector3 {
    while true {
        v := random_get_within_unit_cube(state);

        len_sq := length_squared(v);

        if len_sq > 1.0 || len_sq < 0.0001 continue;

        return unit_vector(v);
    }

    return .{0, 0, 1};
}

// 2D version for convenience
random_unit_vector2 :: () -> Vector2 {
    while true {
        v := Vector2.{
            random_get_within_negative_one_to_one(),
            random_get_within_negative_one_to_one()
        };

        len_sq := length_squared(v);

        if len_sq > 1.0 || len_sq < 0.0001 continue;

        return unit_vector(v);
    }

    return .{1, 0};
}

random_unit_vector2 :: (state: *Random_State) -> Vector2 {
    while true {
        v := Vector2.{
            random_get_within_negative_one_to_one(state),
            random_get_within_negative_one_to_one(state)
        };

        len_sq := length_squared(v);

        if len_sq > 1.0 || len_sq < 0.0001 continue;

        return unit_vector(v);
    }

    return .{1, 0};
}
