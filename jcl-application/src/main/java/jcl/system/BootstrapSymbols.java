package jcl.system;

import jcl.lang.DoubleFloatStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.RandomStateStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.SingleFloatStruct;
import jcl.lang.SynonymStreamStruct;
import jcl.lang.TStruct;
import jcl.lang.TwoWayStreamStruct;
import jcl.lang.internal.stream.EmptyStreamStructImpl;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.GlobalPackageStruct;

final class BootstrapSymbols {

	static void bootstrap() {
		bootstrapConstants();
		bootstrapVariables();
	}

	static void bootstrapStreams() {
		final TwoWayStreamStruct terminalIo = TwoWayStreamStruct.toTwoWayStream(
				EmptyStreamStructImpl.INSTANCE, EmptyStreamStructImpl.INSTANCE
		);
		terminalIo.setInteractive(true);
		CommonLispSymbols.TERMINAL_IO.setValue(terminalIo);

		CommonLispSymbols.DEBUG_IO.setValue(SynonymStreamStruct.toSynonymStream(CommonLispSymbols.TERMINAL_IO));
		CommonLispSymbols.ERROR_OUTPUT.setValue(SynonymStreamStruct.toSynonymStream(CommonLispSymbols.TERMINAL_IO));
		CommonLispSymbols.QUERY_IO.setValue(SynonymStreamStruct.toSynonymStream(CommonLispSymbols.TERMINAL_IO));
		CommonLispSymbols.STANDARD_INPUT.setValue(SynonymStreamStruct.toSynonymStream(CommonLispSymbols.TERMINAL_IO));
		CommonLispSymbols.STANDARD_OUTPUT.setValue(SynonymStreamStruct.toSynonymStream(CommonLispSymbols.TERMINAL_IO));
		CommonLispSymbols.TRACE_OUTPUT.setValue(SynonymStreamStruct.toSynonymStream(CommonLispSymbols.TERMINAL_IO));
	}

	static void bootstrapConstants() {
		CommonLispSymbols.CALL_ARGUMENTS_LIMIT.initializeConstant(IntegerStruct.toLispInteger(Short.MAX_VALUE));
		CommonLispSymbols.INTERNAL_TIME_UNITS_PER_SECOND.initializeConstant(IntegerStruct.toLispInteger(1000000));
		CommonLispSymbols.LAMBDA_LIST_KEYWORDS.initializeConstant(
				ListStruct.toLispList(
						CommonLispSymbols.AND_ALLOW_OTHER_KEYS,
						CommonLispSymbols.AND_AUX,
						CommonLispSymbols.AND_BODY,
						CommonLispSymbols.AND_ENVIRONMENT,
						CommonLispSymbols.AND_KEY,
						CommonLispSymbols.AND_OPTIONAL,
						CommonLispSymbols.AND_REST,
						CommonLispSymbols.AND_WHOLE
				));
		CommonLispSymbols.LAMBDA_PARAMETERS_LIMIT.initializeConstant(IntegerStruct.toLispInteger(Short.MAX_VALUE));
		CommonLispSymbols.MULTIPLE_VALUES_LIMIT.initializeConstant(IntegerStruct.toLispInteger(Short.MAX_VALUE));

		CommonLispSymbols.ARRAY_DIMENSION_LIMIT.initializeConstant(IntegerStruct.toLispInteger(Integer.MAX_VALUE));
		CommonLispSymbols.ARRAY_RANK_LIMIT.initializeConstant(IntegerStruct.EIGHT);
		CommonLispSymbols.ARRAY_TOTAL_SIZE_LIMIT.initializeConstant(IntegerStruct.toLispInteger(Integer.MAX_VALUE));

		CommonLispSymbols.CHAR_CODE_LIMIT.initializeConstant(IntegerStruct.toLispInteger(Character.MAX_VALUE));

		CommonLispSymbols.BOOLE_1.initializeConstant(IntegerStruct.ZERO);
		CommonLispSymbols.BOOLE_2.initializeConstant(IntegerStruct.ONE);
		CommonLispSymbols.BOOLE_AND.initializeConstant(IntegerStruct.TWO);
		CommonLispSymbols.BOOLE_ANDC1.initializeConstant(IntegerStruct.THREE);
		CommonLispSymbols.BOOLE_ANDC2.initializeConstant(IntegerStruct.FOUR);
		CommonLispSymbols.BOOLE_C1.initializeConstant(IntegerStruct.FIVE);
		CommonLispSymbols.BOOLE_C2.initializeConstant(IntegerStruct.SIX);
		CommonLispSymbols.BOOLE_CLR.initializeConstant(IntegerStruct.SEVEN);
		CommonLispSymbols.BOOLE_EQV.initializeConstant(IntegerStruct.EIGHT);
		CommonLispSymbols.BOOLE_IOR.initializeConstant(IntegerStruct.NINE);
		CommonLispSymbols.BOOLE_NAND.initializeConstant(IntegerStruct.TEN);
		CommonLispSymbols.BOOLE_NOR.initializeConstant(IntegerStruct.ELEVEN);
		CommonLispSymbols.BOOLE_ORC1.initializeConstant(IntegerStruct.TWELVE);
		CommonLispSymbols.BOOLE_ORC2.initializeConstant(IntegerStruct.THIRTEEN);
		CommonLispSymbols.BOOLE_SET.initializeConstant(IntegerStruct.FOURTEEN);
		CommonLispSymbols.BOOLE_XOR.initializeConstant(IntegerStruct.FIFTEEN);

		CommonLispSymbols.MOST_POSITIVE_FIXNUM.initializeConstant(IntegerStruct.toLispInteger(Integer.MAX_VALUE));
		CommonLispSymbols.MOST_NEGATIVE_FIXNUM.initializeConstant(IntegerStruct.toLispInteger(Integer.MIN_VALUE));

//		Object EPSILON_PLACEHOLDER = Precision.EPSILON;

		CommonLispSymbols.MOST_POSITIVE_SHORT_FLOAT.initializeConstant(SingleFloatStruct.toLispFloat(Float.MAX_VALUE));
		CommonLispSymbols.LEAST_POSITIVE_SHORT_FLOAT.initializeConstant(SingleFloatStruct.toLispFloat(Float.MIN_VALUE));
		CommonLispSymbols.LEAST_POSITIVE_NORMALIZED_SHORT_FLOAT.initializeConstant(SingleFloatStruct.toLispFloat(Float.MIN_NORMAL));
		CommonLispSymbols.MOST_NEGATIVE_SHORT_FLOAT.initializeConstant(SingleFloatStruct.toLispFloat(-Float.MAX_VALUE));
		CommonLispSymbols.LEAST_NEGATIVE_SHORT_FLOAT.initializeConstant(SingleFloatStruct.toLispFloat(-Float.MIN_VALUE));
		CommonLispSymbols.LEAST_NEGATIVE_NORMALIZED_SHORT_FLOAT.initializeConstant(SingleFloatStruct.toLispFloat(-Float.MIN_NORMAL));
		CommonLispSymbols.SHORT_FLOAT_EPSILON.initializeConstant(SingleFloatStruct.toLispFloat(5.960465E-8F));
		CommonLispSymbols.SHORT_FLOAT_NEGATIVE_EPSILON.initializeConstant(SingleFloatStruct.toLispFloat(2.9802326e-8F));

		CommonLispSymbols.MOST_POSITIVE_SINGLE_FLOAT.initializeConstant(SingleFloatStruct.toLispFloat(Float.MAX_VALUE));
		CommonLispSymbols.LEAST_POSITIVE_SINGLE_FLOAT.initializeConstant(SingleFloatStruct.toLispFloat(Float.MIN_VALUE));
		CommonLispSymbols.LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT.initializeConstant(SingleFloatStruct.toLispFloat(Float.MIN_NORMAL));
		CommonLispSymbols.MOST_NEGATIVE_SINGLE_FLOAT.initializeConstant(SingleFloatStruct.toLispFloat(-Float.MAX_VALUE));
		CommonLispSymbols.LEAST_NEGATIVE_SINGLE_FLOAT.initializeConstant(SingleFloatStruct.toLispFloat(-Float.MIN_VALUE));
		CommonLispSymbols.LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT.initializeConstant(SingleFloatStruct.toLispFloat(-Float.MIN_NORMAL));
		CommonLispSymbols.SINGLE_FLOAT_EPSILON.initializeConstant(SingleFloatStruct.toLispFloat(5.960465E-8F));
		CommonLispSymbols.SINGLE_FLOAT_NEGATIVE_EPSILON.initializeConstant(SingleFloatStruct.toLispFloat(2.9802326e-8F));

		CommonLispSymbols.MOST_POSITIVE_DOUBLE_FLOAT.initializeConstant(DoubleFloatStruct.toLispFloat(Double.MAX_VALUE));
		CommonLispSymbols.LEAST_POSITIVE_DOUBLE_FLOAT.initializeConstant(DoubleFloatStruct.toLispFloat(Double.MIN_VALUE));
		CommonLispSymbols.LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT.initializeConstant(DoubleFloatStruct.toLispFloat(Double.MIN_NORMAL));
		CommonLispSymbols.MOST_NEGATIVE_DOUBLE_FLOAT.initializeConstant(DoubleFloatStruct.toLispFloat(-Double.MAX_VALUE));
		CommonLispSymbols.LEAST_NEGATIVE_DOUBLE_FLOAT.initializeConstant(DoubleFloatStruct.toLispFloat(-Double.MIN_VALUE));
		CommonLispSymbols.LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT.initializeConstant(DoubleFloatStruct.toLispFloat(-Double.MIN_NORMAL));
		CommonLispSymbols.DOUBLE_FLOAT_EPSILON.initializeConstant(DoubleFloatStruct.toLispFloat(1.1102230246251568E-16D));
		CommonLispSymbols.DOUBLE_FLOAT_NEGATIVE_EPSILON.initializeConstant(DoubleFloatStruct.toLispFloat(5.551115123125784E-17D));

		CommonLispSymbols.MOST_POSITIVE_LONG_FLOAT.initializeConstant(DoubleFloatStruct.toLispFloat(Double.MAX_VALUE));
		CommonLispSymbols.LEAST_POSITIVE_LONG_FLOAT.initializeConstant(DoubleFloatStruct.toLispFloat(Double.MIN_VALUE));
		CommonLispSymbols.LEAST_POSITIVE_NORMALIZED_LONG_FLOAT.initializeConstant(DoubleFloatStruct.toLispFloat(Double.MIN_NORMAL));
		CommonLispSymbols.MOST_NEGATIVE_LONG_FLOAT.initializeConstant(DoubleFloatStruct.toLispFloat(-Double.MAX_VALUE));
		CommonLispSymbols.LEAST_NEGATIVE_LONG_FLOAT.initializeConstant(DoubleFloatStruct.toLispFloat(-Double.MIN_VALUE));
		CommonLispSymbols.LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT.initializeConstant(DoubleFloatStruct.toLispFloat(-Double.MIN_NORMAL));
		CommonLispSymbols.LONG_FLOAT_EPSILON.initializeConstant(DoubleFloatStruct.toLispFloat(1.1102230246251568E-16D));
		CommonLispSymbols.LONG_FLOAT_NEGATIVE_EPSILON.initializeConstant(DoubleFloatStruct.toLispFloat(5.551115123125784E-17D));

		CommonLispSymbols.PI.initializeConstant(DoubleFloatStruct.toLispFloat(Math.PI));
	}

	static void bootstrapVariables() {

		// Done in Bootstrap Functions for now
//		CommonLispSymbols.MACROEXPAND_HOOK_VAR.setValue(funcallFunction);
		CommonLispSymbols.COMPILE_FILE_PATHNAME_VAR.setValue(NILStruct.INSTANCE);
		CommonLispSymbols.COMPILE_FILE_TRUENAME_VAR.setValue(NILStruct.INSTANCE);
		CommonLispSymbols.LOAD_PATHNAME_VAR.setValue(NILStruct.INSTANCE);
		CommonLispSymbols.LOAD_TRUENAME_VAR.setValue(NILStruct.INSTANCE);
		CommonLispSymbols.COMPILE_PRINT_VAR.setValue(NILStruct.INSTANCE);
		CommonLispSymbols.COMPILE_VERBOSE_VAR.setValue(NILStruct.INSTANCE);
		CommonLispSymbols.LOAD_PRINT_VAR.setValue(NILStruct.INSTANCE);
		CommonLispSymbols.LOAD_VERBOSE_VAR.setValue(NILStruct.INSTANCE);
		CommonLispSymbols.MODULES_VAR.setValue(NILStruct.INSTANCE);

		CommonLispSymbols.GENSYM_COUNTER_VAR.setValue(IntegerStruct.ZERO);

		CommonLispSymbols.PACKAGE_VAR.setValue(GlobalPackageStruct.COMMON_LISP_USER);

		CommonLispSymbols.PRINT_ARRAY_VAR.setValue(TStruct.INSTANCE);
		CommonLispSymbols.PRINT_BASE_VAR.setValue(IntegerStruct.TEN);
		CommonLispSymbols.PRINT_CASE_VAR.initMatches();
		CommonLispSymbols.PRINT_CASE_VAR.setValue(CommonLispSymbols.UPCASE_KEYWORD);
		CommonLispSymbols.PRINT_RADIX_VAR.setValue(NILStruct.INSTANCE);
		CommonLispSymbols.PRINT_CIRCLE_VAR.setValue(NILStruct.INSTANCE);
		CommonLispSymbols.PRINT_ESCAPE_VAR.setValue(TStruct.INSTANCE);
		CommonLispSymbols.PRINT_GENSYM_VAR.setValue(TStruct.INSTANCE);
		CommonLispSymbols.PRINT_PRETTY_VAR.setValue(NILStruct.INSTANCE);
		CommonLispSymbols.PRINT_READABLY_VAR.setValue(NILStruct.INSTANCE);

		CommonLispSymbols.DEFAULT_PATHNAME_DEFAULTS_VAR.setValue(PathnameStruct.toPathname(""));

		CommonLispSymbols.RANDOM_STATE_VAR.setValue(RandomStateStruct.toLispRandomState());

		CommonLispSymbols.READ_BASE_VAR.setValue(IntegerStruct.TEN);
		CommonLispSymbols.READ_DEFAULT_FLOAT_FORMAT_VAR.setValue(CommonLispSymbols.SINGLE_FLOAT);
		CommonLispSymbols.READ_EVAL_VAR.setValue(TStruct.INSTANCE);
		CommonLispSymbols.READ_SUPPRESS_VAR.setValue(NILStruct.INSTANCE);
		CommonLispSymbols.READTABLE_VAR.setValue(ReadtableStruct.toReadtable());
	}
}
