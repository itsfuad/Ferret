package numeric

import (
	"math/big"
	"testing"
)

// Benchmark to show big.Int overhead for compile-time operations is negligible
func BenchmarkStringToBigInt(b *testing.B) {
	testCases := []string{
		"42",
		"12345678901234567890",
		"170141183460469231731687303715884105727",
		"0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
	}

	for _, tc := range testCases {
		b.Run(tc, func(b *testing.B) {
			for i := 0; i < b.N; i++ {
				_, _ = StringToBigInt(tc)
			}
		})
	}
}

func BenchmarkFitsInBitSize(b *testing.B) {
	smallValue, _ := new(big.Int).SetString("100", 10)
	largeValue, _ := new(big.Int).SetString("170141183460469231731687303715884105727", 10)

	testCases := []struct {
		name    string
		value   *big.Int
		bitSize int
		signed  bool
	}{
		{"small_8bit", smallValue, 8, true},
		{"small_64bit", smallValue, 64, true},
		{"large_128bit", largeValue, 128, true},
		{"large_256bit", largeValue, 256, true},
	}

	for _, tc := range testCases {
		b.Run(tc.name, func(b *testing.B) {
			for i := 0; i < b.N; i++ {
				_ = FitsInBitSize(tc.value, tc.bitSize, tc.signed)
			}
		})
	}
}
