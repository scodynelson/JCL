package jcl.functions;

import jcl.compiler.function.CompiledFunctionStruct;
import jcl.lang.ArrayStruct;
import jcl.lang.BitVectorStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.ComplexStruct;
import jcl.lang.ConsStruct;
import jcl.lang.FloatStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.HashTableStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.NumberStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.RandomStateStruct;
import jcl.lang.RationalStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.RealStruct;
import jcl.lang.StreamStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.VectorStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.type.SimpleBitVectorType;
import jcl.type.SimpleStringType;
import jcl.type.SimpleVectorType;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class PredicateFunctions {

	abstract static class AbstractPredicateFunction extends CommonLispBuiltInFunctionStructBase {

		protected static final String OBJECT_ARGUMENT = "OBJECT";

		protected final Class<? extends LispStruct> classType;

		AbstractPredicateFunction(final String functionName, final String type,
		                                 final Class<? extends LispStruct> classType) {
			super("Returns true if object is of type " + type + "; otherwise, returns false.",
			      functionName,
			      Parameters.forFunction(functionName)
			                .requiredParameter(OBJECT_ARGUMENT)
			);
			this.classType = classType;
		}

		@Override
		public LispStruct apply(final Arguments arguments) {
			final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
			return BooleanStruct.toLispBoolean(classType.isAssignableFrom(object.getClass()));
		}
	}

	public static final class ArrayPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "ARRAYP";
		private static final String TYPE = "ARRAY";

		public ArrayPFunction() {
			super(FUNCTION_NAME, TYPE, ArrayStruct.class);
		}
	}

	public static final class AtomFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "ATOM";
		private static final String TYPE = "ATOM";

		public AtomFunction() {
			super(FUNCTION_NAME, TYPE, ArrayStruct.class);
		}

		@Override
		public LispStruct apply(final Arguments arguments) {
			final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
			return BooleanStruct.toLispBoolean(!(object instanceof ConsStruct));
		}
	}

	public static final class BitVectorPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "BIT-VECTOR-P";
		private static final String TYPE = "BIT-VECTOR";

		public BitVectorPFunction() {
			super(FUNCTION_NAME, TYPE, BitVectorStruct.class);
		}
	}

	public static final class CharacterPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "CHARACTERP";
		private static final String TYPE = "CHARACTER";

		public CharacterPFunction() {
			super(FUNCTION_NAME, TYPE, CharacterStruct.class);
		}
	}

	public static final class CompiledFunctionPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "COMPILE-FUNCTION-P";
		private static final String TYPE = "COMPILE-FUNCTION";

		public CompiledFunctionPFunction() {
			super(FUNCTION_NAME, TYPE, CompiledFunctionStruct.class);
		}
	}

	public static final class ComplexPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "COMPLEXP";
		private static final String TYPE = "COMPLEX";

		public ComplexPFunction() {
			super(FUNCTION_NAME, TYPE, ComplexStruct.class);
		}
	}

	public static final class ConsPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "CONSP";
		private static final String TYPE = "CONS";

		public ConsPFunction() {
			super(FUNCTION_NAME, TYPE, ConsStruct.class);
		}
	}

	public static final class FloatPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "FLOATP";
		private static final String TYPE = "FLOAT";

		public FloatPFunction() {
			super(FUNCTION_NAME, TYPE, FloatStruct.class);
		}
	}

	public static final class FunctionPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "FUNCTIONP";
		private static final String TYPE = "FUNCTION";

		public FunctionPFunction() {
			super(FUNCTION_NAME, TYPE, FunctionStruct.class);
		}
	}

	public static final class HashTablePFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "HASH-TABLE-P";
		private static final String TYPE = "HASH-TABLE";

		public HashTablePFunction() {
			super(FUNCTION_NAME, TYPE, HashTableStruct.class);
		}
	}

	public static final class IntegerPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "INTEGERP";
		private static final String TYPE = "INTEGER";

		public IntegerPFunction() {
			super(FUNCTION_NAME, TYPE, IntegerStruct.class);
		}
	}

	public static final class ListPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "LISTP";
		private static final String TYPE = "LIST";

		public ListPFunction() {
			super(FUNCTION_NAME, TYPE, ListStruct.class);
		}
	}

	public static final class NullFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "NULL";
		private static final String TYPE = "NIL";

		public NullFunction() {
			super(FUNCTION_NAME, TYPE, NILStruct.class);
		}

		@Override
		public LispStruct apply(final Arguments arguments) {
			final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
			return BooleanStruct.toLispBoolean(NILStruct.INSTANCE.eq(object));
		}
	}

	public static final class NumberPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "NUMBERP";
		private static final String TYPE = "NUMBER";

		public NumberPFunction() {
			super(FUNCTION_NAME, TYPE, NumberStruct.class);
		}
	}

	public static final class PackagePFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "PACKAGEP";
		private static final String TYPE = "PACKAGE";

		public PackagePFunction() {
			super(FUNCTION_NAME, TYPE, PackageStruct.class);
		}
	}

	public static final class PathnamePFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "PATHNAMEP";
		private static final String TYPE = "PATHNAME";

		public PathnamePFunction() {
			super(FUNCTION_NAME, TYPE, PathnameStruct.class);
		}
	}

	public static final class RationalPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "RATIONALP";
		private static final String TYPE = "RATIONAL";

		public RationalPFunction() {
			super(FUNCTION_NAME, TYPE, RationalStruct.class);
		}
	}

	public static final class ReadtablePFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "READTABLEP";
		private static final String TYPE = "READTABLE";

		public ReadtablePFunction() {
			super(FUNCTION_NAME, TYPE, ReadtableStruct.class);
		}
	}

	public static final class RealPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "REALP";
		private static final String TYPE = "REAL";

		public RealPFunction() {
			super(FUNCTION_NAME, TYPE, RealStruct.class);
		}
	}

	public static final class RandomStatePFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "RANDOM-STATE-P";
		private static final String TYPE = "RANDOM-STATE";

		public RandomStatePFunction() {
			super(FUNCTION_NAME, TYPE, RandomStateStruct.class);
		}
	}

	public static final class SimpleBitVectorPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "SIMPLE-BIT-VECTOR-P";
		private static final String TYPE = "SIMPLE-BIT-VECTOR";

		public SimpleBitVectorPFunction() {
			super(FUNCTION_NAME, TYPE, BitVectorStruct.class);
		}

		@Override
		public LispStruct apply(final Arguments arguments) {
			final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
			return BooleanStruct.toLispBoolean(
					classType.isAssignableFrom(object.getClass()) && object.getType().eq(SimpleBitVectorType.INSTANCE)
			);
		}
	}

	public static final class SimpleStringPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "SIMPLE-STRING-P";
		private static final String TYPE = "SIMPLE-STRING";

		public SimpleStringPFunction() {
			super(FUNCTION_NAME, TYPE, StringStruct.class);
		}

		@Override
		public LispStruct apply(final Arguments arguments) {
			final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
			return BooleanStruct.toLispBoolean(
					classType.isAssignableFrom(object.getClass()) && object.getType().eq(SimpleStringType.INSTANCE)
			);
		}
	}

	public static final class SimpleVectorPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "SIMPLE-VECTOR-P";
		private static final String TYPE = "SIMPLE-VECTOR";

		public SimpleVectorPFunction() {
			super(FUNCTION_NAME, TYPE, VectorStruct.class);
		}

		@Override
		public LispStruct apply(final Arguments arguments) {
			final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
			return BooleanStruct.toLispBoolean(
					classType.isAssignableFrom(object.getClass()) && object.getType().eq(SimpleVectorType.INSTANCE)
			);
		}
	}

	public static final class StringPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "STRINGP";
		private static final String TYPE = "STRING";

		public StringPFunction() {
			super(FUNCTION_NAME, TYPE, StringStruct.class);
		}
	}

	public static final class StreamPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "STREAMP";
		private static final String TYPE = "STREAM";

		public StreamPFunction() {
			super(FUNCTION_NAME, TYPE, StreamStruct.class);
		}
	}

	public static final class SymbolPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "SYMBOLP";
		private static final String TYPE = "SYMBOL";

		public SymbolPFunction() {
			super(FUNCTION_NAME, TYPE, SymbolStruct.class);
		}
	}

	public static final class VectorPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "VECTORP";
		private static final String TYPE = "VECTOR";

		public VectorPFunction() {
			super(FUNCTION_NAME, TYPE, VectorStruct.class);
		}
	}

	// Non-Standard

	public static final class KeywordPFunction extends AbstractPredicateFunction {

		private static final String FUNCTION_NAME = "KEYWORDP";
		private static final String TYPE = "KEYWORD";

		public KeywordPFunction() {
			super(FUNCTION_NAME, TYPE, KeywordStruct.class);
		}
	}
}
