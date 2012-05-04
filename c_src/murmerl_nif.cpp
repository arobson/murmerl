#include "murmerl_nif.h"
#include "erl_nif_compat.h"
#include "murmurhash3.h"

#define N32 624

extern "C" {
	ERL_NIF_TERM murmurhash_x86_32_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
	ERL_NIF_TERM murmurhash_x86_128_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
	ERL_NIF_TERM murmurhash_x86_128_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);


	static ErlNifFunc funcs[] =
	{
		{"hash_x86_32_impl", 2, murmurhash_x86_32_impl},
		{"hash_x86_128_impl", 2, murmurhash_x86_128_impl},
		{"hash_x64_128_impl", 2, murmurhash_x86_128_impl}
	};

	ERL_NIF_INIT(murmerl, funcs, NULL, NULL, NULL, NULL);
};

static bool check_and_unpack_data(ErlNifEnv* env, ERL_NIF_TERM bin_term,
    ErlNifBinary *bin);

ERL_NIF_TERM murmurhash_x86_32_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

	ErlNifBinary 	binary;
	uint32_t		hash;
	uint32_t		seed;

	// check for valid key arg
	if( !check_and_unpack_data(env, argv[0], &binary) ) {
		return enif_make_badarg(env);
	}

	// check for valid seed arg
	if( !enif_get_uint(env, argv[1], &seed) ) {
		return enif_make_badarg(env);
	}

	MurmurHash3_x86_32(binary.data, binary.size, seed, &hash);
	return enif_make_uint(env, hash);
}

ERL_NIF_TERM murmurhash_x86_128_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

	ErlNifBinary 	binary;
	uint32_t		hash[4] = { 0, 0, 0, 0 };
	uint32_t		seed;
	ERL_NIF_TERM	parts[4];

	// check for valid key arg
	if( !check_and_unpack_data(env, argv[0], &binary) ) {
		return enif_make_badarg(env);
	}

	// check for valid seed arg
	if( !enif_get_uint(env, argv[1], &seed) ) {
		return enif_make_badarg(env);
	}

	MurmurHash3_x86_128(binary.data, binary.size, seed, &hash);
	for( int i = 0; i < 4; i++ ) {
		parts[i] = enif_make_uint(env, hash[i]);
	}
	return enif_make_list_from_array(env, parts, 4);
}

ERL_NIF_TERM murmurhash_x64_128_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

	ErlNifBinary 	binary;
	uint64_t		hash[2];
	uint32_t		seed;
	ERL_NIF_TERM	parts[2];

	// check for valid key arg
	if( !check_and_unpack_data(env, argv[0], &binary) ) {
		return enif_make_badarg(env);
	}

	// check for valid seed arg
	if( !enif_get_uint(env, argv[1], &seed) ) {
		return enif_make_badarg(env);
	}

	MurmurHash3_x64_128(binary.data, binary.size, seed, &hash);
	parts[0] = hash[0];
	parts[1] = hash[1];
	return enif_make_list_from_array(env, parts, 2);
}

bool check_and_unpack_data(ErlNifEnv* env, ERL_NIF_TERM bin_term, ErlNifBinary *bin)
{
    if (!enif_is_binary(env, bin_term)) {
        return false;
    }

    if (!enif_inspect_binary(env, bin_term, bin)) {
        return false;
    }

    return true;
}