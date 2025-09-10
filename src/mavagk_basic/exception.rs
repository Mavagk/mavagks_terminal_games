#[derive(Debug, Clone)]
pub enum Exception {
	DivisionByZero = 3001,
	NegativeNumberRaisedToNonIntegerPower = 3002,
	ZeroRaisedToNegativePower = 3003,
	LogOfNonPositive = 3004,
	SquareRootOfNegative = 3005,
	ModOrRemainderByZero = 3006,
	ACosOrASinOutOfRange = 3007,
	AngleOfZeroZero = 3008,
}