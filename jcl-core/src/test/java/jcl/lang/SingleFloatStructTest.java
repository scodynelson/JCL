package jcl.lang;

import jcl.lang.internal.SingleFloatStructImpl;
import jcl.lang.number.DecodeFloatResult;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;

public class SingleFloatStructTest {

	private static final SingleFloatStruct FLOAT = SingleFloatStruct.toLispFloat(2.5F);
	private static final SingleFloatStruct NEG_FLOAT = SingleFloatStruct.toLispFloat(-2.5F);


	@Test
	public void test_decodeFloat() {
		final DecodeFloatResult result = FLOAT.decodeFloat();
		Assert.assertThat(result.getSignificand(), instanceOf(SingleFloatStruct.class));
		Assert.assertThat(((SingleFloatStruct) result.getSignificand()).toJavaPFloat(), is(0.625F));
		Assert.assertThat(result.getExponent(), is(IntegerStruct.toLispInteger(2)));
		Assert.assertThat(result.getSign(), instanceOf(SingleFloatStruct.class));
		Assert.assertThat(result.getSign(), is(SingleFloatStruct.ONE));
	}

	@Test
	public void test_decodeFloat_Neg() {
		final DecodeFloatResult result = NEG_FLOAT.decodeFloat();
		Assert.assertThat(result.getSignificand(), instanceOf(SingleFloatStruct.class));
		Assert.assertThat(((SingleFloatStruct) result.getSignificand()).toJavaPFloat(), is(0.625F));
		Assert.assertThat(result.getExponent(), is(IntegerStruct.toLispInteger(2)));
		Assert.assertThat(result.getSign(), instanceOf(SingleFloatStruct.class));
		Assert.assertThat(result.getSign(), is(SingleFloatStruct.MINUS_ONE));
	}

	@Test
	@Ignore
	public void test_decodeFloat_Zero() {
		final DecodeFloatResult result = SingleFloatStruct.ZERO.decodeFloat();
		Assert.assertThat(result.getSignificand(), instanceOf(SingleFloatStruct.class));
		Assert.assertThat(result.getSignificand(), is(SingleFloatStruct.ZERO));
		Assert.assertThat(result.getExponent(), is(IntegerStruct.toLispInteger(0)));
		Assert.assertThat(result.getSign(), instanceOf(SingleFloatStruct.class));
		Assert.assertThat(result.getSign(), is(SingleFloatStruct.ONE));
	}

	@Test
	@Ignore
	public void test_decodeFloat_NegZero() {
		final DecodeFloatResult result = SingleFloatStruct.MINUS_ZERO.decodeFloat();
		Assert.assertThat(result.getSignificand(), instanceOf(SingleFloatStruct.class));
		Assert.assertThat(result.getSignificand(), is(SingleFloatStruct.ZERO));
		Assert.assertThat(result.getExponent(), is(IntegerStruct.toLispInteger(0)));
		Assert.assertThat(result.getSign(), instanceOf(SingleFloatStruct.class));
		Assert.assertThat(result.getSign(), is(SingleFloatStruct.MINUS_ONE));
	}

	@Test
	public void test_integerDecodeFloat() {
		final DecodeFloatResult result = FLOAT.integerDecodeFloat();
		Assert.assertThat(result.getSignificand(), instanceOf(IntegerStruct.class));
		Assert.assertThat(result.getSignificand(), is(IntegerStruct.toLispInteger(10485760)));
		Assert.assertThat(result.getExponent(), is(IntegerStruct.toLispInteger(-22)));
		Assert.assertThat(result.getSign(), instanceOf(IntegerStruct.class));
		Assert.assertThat(result.getSign(), is(IntegerStruct.toLispInteger(1)));
	}

	@Test
	public void test_integerDecodeFloat_Neg() {
		final DecodeFloatResult result = NEG_FLOAT.integerDecodeFloat();
		Assert.assertThat(result.getSignificand(), instanceOf(IntegerStruct.class));
		Assert.assertThat(result.getSignificand(), is(IntegerStruct.toLispInteger(10485760)));
		Assert.assertThat(result.getExponent(), is(IntegerStruct.toLispInteger(-22)));
		Assert.assertThat(result.getSign(), instanceOf(IntegerStruct.class));
		Assert.assertThat(result.getSign(), is(IntegerStruct.toLispInteger(-1)));
	}

	@Test
	public void test_integerDecodeFloat_Zero() {
		final DecodeFloatResult result = SingleFloatStruct.ZERO.integerDecodeFloat();
		Assert.assertThat(result.getSignificand(), instanceOf(IntegerStruct.class));
		Assert.assertThat(result.getSignificand(), is(IntegerStruct.toLispInteger(0)));
		Assert.assertThat(result.getExponent(), is(IntegerStruct.toLispInteger(-150)));
		Assert.assertThat(result.getSign(), instanceOf(IntegerStruct.class));
		Assert.assertThat(result.getSign(), is(IntegerStruct.toLispInteger(1)));
	}

	@Test
	public void test_integerDecodeFloat_NegZero() {
		final DecodeFloatResult result = SingleFloatStruct.MINUS_ZERO.integerDecodeFloat();
		Assert.assertThat(result.getSignificand(), instanceOf(IntegerStruct.class));
		Assert.assertThat(result.getSignificand(), is(IntegerStruct.toLispInteger(0)));
		Assert.assertThat(result.getExponent(), is(IntegerStruct.toLispInteger(-150)));
		Assert.assertThat(result.getSign(), instanceOf(IntegerStruct.class));
		Assert.assertThat(result.getSign(), is(IntegerStruct.toLispInteger(-1)));
	}

	@Test
	public void test_floatPrecision() {
		final IntegerStruct result = FLOAT.floatPrecision();
		Assert.assertThat(result, is(IntegerStruct.toLispInteger(SingleFloatStructImpl.FLOAT_PRECISION)));
	}

	@Test
	public void test_floatSign() {
	}

	@Test
	public void test_toJavaPFloat() {
	}

	@Test
	public void test_toJavaFloat() {
	}

	@Test
	public void test_toJavaPDouble() {
	}

	@Test
	public void test_toJavaDouble() {
	}

	@Test
	public void test_toJavaBigDecimal() {
	}

	@Test
	public void test_isLessThan() {
	}

	@Test
	public void test_isGreaterThan() {
	}

	@Test
	public void test_isLessThanOrEqualTo() {
	}

	@Test
	public void test_isGreaterThanOrEqualTo() {
	}

	@Test
	public void test_plusp() {
	}

	@Test
	public void test_minusp() {
	}

	@Test
	public void test_rational() {
	}

	@Test
	public void test_floatingPoint() {
	}

	@Test
	public void test_floor() {
	}

	@Test
	public void test_floor1() {
	}

	@Test
	public void test_ffloor() {
	}

	@Test
	public void test_ffloor1() {
	}

	@Test
	public void test_ceiling() {
	}

	@Test
	public void test_ceiling1() {
	}

	@Test
	public void test_fceiling() {
	}

	@Test
	public void test_fceiling1() {
	}

	@Test
	public void test_truncate() {
	}

	@Test
	public void test_truncate1() {
	}

	@Test
	public void test_ftruncate() {
	}

	@Test
	public void test_ftruncate1() {
	}

	@Test
	public void test_round() {
	}

	@Test
	public void test_round1() {
	}

	@Test
	public void test_fround() {
	}

	@Test
	public void test_fround1() {
	}

	@Test
	public void test_atan() {
	}

	@Test
	public void test_ap() {
	}

	@Test
	public void test_abs() {
	}

	@Test
	public void test_zerop() {
	}

	@Test
	public void test_add() {
	}

	@Test
	public void test_subtract() {
	}

	@Test
	public void test_multiply() {
	}

	@Test
	public void test_divide() {
	}

	@Test
	public void test_isEqualTo() {
	}

	@Test
	public void test_isNotEqualTo() {
	}

	@Test
	public void test_signum() {
	}

	@Test
	public void test_negation() {
	}

	@Test
	public void test_reciprocal() {
	}

	@Test
	public void test_exp() {
	}

	@Test
	public void test_expt() {
	}

	@Test
	public void test_log() {
	}

	@Test
	public void test_log1() {
	}

	@Test
	public void test_sqrt() {
	}

	@Test
	public void test_sin() {
	}

	@Test
	public void test_cos() {
	}

	@Test
	public void test_tan() {
	}

	@Test
	public void test_asin() {
	}

	@Test
	public void test_acos() {
	}

	@Test
	public void test_atan1() {
	}

	@Test
	public void test_sinh() {
	}

	@Test
	public void test_cosh() {
	}

	@Test
	public void test_tanh() {
	}

	@Test
	public void test_asinh() {
	}

	@Test
	public void test_acosh() {
	}

	@Test
	public void test_atanh() {
	}

	@Test
	public void test_test_toString() {
	}

}