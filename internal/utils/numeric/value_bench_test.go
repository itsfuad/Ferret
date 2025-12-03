package numeric

import (
	"testing"
)

// BenchmarkNewNumericValue_Int64Path tests the fast path (values that fit in int64)
func BenchmarkNewNumericValue_Int64Path(b *testing.B) {
	testCases := []string{
		"42",
		"127",
		"-128",
		"9223372036854775807",  // max int64
		"-9223372036854775808", // min int64
		"0xFF",
		"0x7FFFFFFFFFFFFFFF", // max int64 in hex
	}

	for _, tc := range testCases {
		b.Run(tc, func(b *testing.B) {
			for i := 0; i < b.N; i++ {
				_, _ = NewNumericValue(tc)
			}
		})
	}
}

// BenchmarkNewNumericValue_BigIntPath tests the big.Int path (values that overflow int64)
func BenchmarkNewNumericValue_BigIntPath(b *testing.B) {
	testCases := []string{
		"9223372036854775808",                                                           // int64 max + 1
		"170141183460469231731687303715884105727",                                       // i128 max
		"340282366920938463463374607431768211455",                                       // u128 max
		"0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",                                            // u128 max in hex
		"57896044618658097711785492504343953926634992332820282019728792003956564819967", // i256 max
	}

	for _, tc := range testCases {
		b.Run(tc, func(b *testing.B) {
			for i := 0; i < b.N; i++ {
				_, _ = NewNumericValue(tc)
			}
		})
	}
}

// BenchmarkInt64ValueFitsInBitSize tests the optimized FitsInBitSize for int64
func BenchmarkInt64ValueFitsInBitSize(b *testing.B) {
	val := &Int64Value{value: 127}

	b.Run("8-bit", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			_ = val.FitsInBitSize(8, true)
		}
	})

	b.Run("16-bit", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			_ = val.FitsInBitSize(16, true)
		}
	})

	b.Run("32-bit", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			_ = val.FitsInBitSize(32, true)
		}
	})

	b.Run("64-bit", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			_ = val.FitsInBitSize(64, true)
		}
	})
}

// BenchmarkBigIntValueFitsInBitSize tests FitsInBitSize for big.Int
func BenchmarkBigIntValueFitsInBitSize(b *testing.B) {
	bigInt, _ := StringToBigInt("170141183460469231731687303715884105727") // i128 max
	val := &BigIntValue{value: bigInt}

	b.Run("128-bit", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			_ = val.FitsInBitSize(128, true)
		}
	})

	b.Run("256-bit", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			_ = val.FitsInBitSize(256, true)
		}
	})
}

// BenchmarkComparison_FullWorkflow compares the complete workflow from string to validation
func BenchmarkComparison_FullWorkflow(b *testing.B) {
	b.Run("Int64Path-Small", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			val, _ := NewNumericValue("127")
			_ = val.FitsInBitSize(8, true)
		}
	})

	b.Run("Int64Path-Large", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			val, _ := NewNumericValue("9223372036854775807")
			_ = val.FitsInBitSize(64, true)
		}
	})

	b.Run("BigIntPath-i128", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			val, _ := NewNumericValue("170141183460469231731687303715884105727")
			_ = val.FitsInBitSize(128, true)
		}
	})

	b.Run("BigIntPath-i256", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			val, _ := NewNumericValue("57896044618658097711785492504343953926634992332820282019728792003956564819967")
			_ = val.FitsInBitSize(256, true)
		}
	})
}
