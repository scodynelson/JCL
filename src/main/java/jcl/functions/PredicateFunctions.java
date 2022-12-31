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
import jcl.lang.LogicalPathnameStruct;
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
import jcl.lang.statics.CommonLispSymbols;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class PredicateFunctions {

	abstract static class AbstractPredicateFunction extends BuiltInFunctionStructImpl {

		protected static final String OBJECT_ARGUMENT = "OBJECT";

		protected final Class<? extends LispStruct> classType;

		private final SymbolStruct functionSymbol;

		AbstractPredicateFunction(final SymbolStruct functionSymbol, final String type,
		                          final Class<? extends LispStruct> classType) {
			super("Returns true if object is of type " + type + "; otherwise, returns false.",
			      functionSymbol.getName(),
			      Parameters.forFunction(functionSymbol.getName())
			                .requiredParameter(OBJECT_ARGUMENT)
			);
			this.classType = classType;
			this.functionSymbol = functionSymbol;
		}

		@Override
		public SymbolStruct getFunctionSymbol() {
			return functionSymbol;
		}

		@Override
		public LispStruct apply(final Arguments arguments) {
			final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
			return BooleanStruct.toLispBoolean(classType.isAssignableFrom(object.getClass()));
		}
	}

	public static final class ArrayPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "ARRAY";

		public ArrayPFunction() {
			super(CommonLispSymbols.ARRAYP, TYPE, ArrayStruct.class);
		}
	}

	public static final class AtomFunction extends AbstractPredicateFunction {

		private static final String TYPE = "ATOM";

		public AtomFunction() {
			super(CommonLispSymbols.ATOM, TYPE, ArrayStruct.class);
		}

		@Override
		public LispStruct apply(final Arguments arguments) {
			final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
			return BooleanStruct.toLispBoolean(!(object instanceof ConsStruct));
		}
	}

	public static final class BitVectorPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "BIT-VECTOR";

		public BitVectorPFunction() {
			super(CommonLispSymbols.BIT_VECTOR_P, TYPE, BitVectorStruct.class);
		}
	}

	public static final class CharacterPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "CHARACTER";

		public CharacterPFunction() {
			super(CommonLispSymbols.CHARACTERP, TYPE, CharacterStruct.class);
		}
	}

	public static final class CompiledFunctionPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "COMPILE-FUNCTION";

		public CompiledFunctionPFunction() {
			super(CommonLispSymbols.COMPILED_FUNCTION_P, TYPE, CompiledFunctionStruct.class);
		}
	}

	public static final class ComplexPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "COMPLEX";

		public ComplexPFunction() {
			super(CommonLispSymbols.COMPLEXP, TYPE, ComplexStruct.class);
		}
	}

	public static final class ConsPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "CONS";

		public ConsPFunction() {
			super(CommonLispSymbols.CONSP, TYPE, ConsStruct.class);
		}
	}

	public static final class FloatPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "FLOAT";

		public FloatPFunction() {
			super(CommonLispSymbols.FLOATP, TYPE, FloatStruct.class);
		}
	}

	public static final class FunctionPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "FUNCTION";

		public FunctionPFunction() {
			super(CommonLispSymbols.FUNCTIONP, TYPE, FunctionStruct.class);
		}
	}

	public static final class HashTablePFunction extends AbstractPredicateFunction {

		private static final String TYPE = "HASH-TABLE";

		public HashTablePFunction() {
			super(CommonLispSymbols.HASH_TABLE_P, TYPE, HashTableStruct.class);
		}
	}

	public static final class IntegerPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "INTEGER";

		public IntegerPFunction() {
			super(CommonLispSymbols.INTEGERP, TYPE, IntegerStruct.class);
		}
	}

	public static final class KeywordPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "KEYWORD";

		public KeywordPFunction() {
			super(CommonLispSymbols.KEYWORDP, TYPE, KeywordStruct.class);
		}
	}

	public static final class ListPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "LIST";

		public ListPFunction() {
			super(CommonLispSymbols.LISTP, TYPE, ListStruct.class);
		}
	}

	public static final class LogicalPathnamePFunction extends AbstractPredicateFunction {

		private static final String TYPE = "PATHNAME";

		public LogicalPathnamePFunction() {
			super(CommonLispSymbols.LOGICAL_PATHNAME_P, TYPE, LogicalPathnameStruct.class);
		}
	}

	public static final class NullFunction extends AbstractPredicateFunction {

		private static final String TYPE = "NIL";

		public NullFunction() {
			super(CommonLispSymbols.NULL, TYPE, NILStruct.class);
		}

		@Override
		public LispStruct apply(final Arguments arguments) {
			final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
			return BooleanStruct.toLispBoolean(NILStruct.INSTANCE.eq(object));
		}
	}

	public static final class NumberPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "NUMBER";

		public NumberPFunction() {
			super(CommonLispSymbols.NUMBERP, TYPE, NumberStruct.class);
		}
	}

	public static final class PackagePFunction extends AbstractPredicateFunction {

		private static final String TYPE = "PACKAGE";

		public PackagePFunction() {
			super(CommonLispSymbols.PACKAGEP, TYPE, PackageStruct.class);
		}
	}

	public static final class PathnamePFunction extends AbstractPredicateFunction {

		private static final String TYPE = "PATHNAME";

		public PathnamePFunction() {
			super(CommonLispSymbols.PATHNAMEP, TYPE, PathnameStruct.class);
		}
	}

	public static final class RandomStatePFunction extends AbstractPredicateFunction {

		private static final String TYPE = "RANDOM-STATE";

		public RandomStatePFunction() {
			super(CommonLispSymbols.RANDOM_STATE_P, TYPE, RandomStateStruct.class);
		}
	}

	public static final class RationalPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "RATIONAL";

		public RationalPFunction() {
			super(CommonLispSymbols.RATIONALP, TYPE, RationalStruct.class);
		}
	}

	public static final class ReadtablePFunction extends AbstractPredicateFunction {

		private static final String TYPE = "READTABLE";

		public ReadtablePFunction() {
			super(CommonLispSymbols.READTABLEP, TYPE, ReadtableStruct.class);
		}
	}

	public static final class RealPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "REAL";

		public RealPFunction() {
			super(CommonLispSymbols.REALP, TYPE, RealStruct.class);
		}
	}

	public static final class SimpleBitVectorPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "SIMPLE-BIT-VECTOR";

		public SimpleBitVectorPFunction() {
			super(CommonLispSymbols.SIMPLE_BIT_VECTOR_P, TYPE, BitVectorStruct.class);
		}

		@Override
		public LispStruct apply(final Arguments arguments) {
			final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
			return BooleanStruct.toLispBoolean(
					classType.isAssignableFrom(object.getClass()) && typep(CommonLispSymbols.SIMPLE_BIT_VECTOR).toJavaPBoolean()
			);
		}
	}

	public static final class SimpleStringPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "SIMPLE-STRING";

		public SimpleStringPFunction() {
			super(CommonLispSymbols.SIMPLE_STRING_P, TYPE, StringStruct.class);
		}

		@Override
		public LispStruct apply(final Arguments arguments) {
			final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
			return BooleanStruct.toLispBoolean(
					classType.isAssignableFrom(object.getClass()) && typep(CommonLispSymbols.SIMPLE_STRING).toJavaPBoolean()
			);
		}
	}

	public static final class SimpleVectorPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "SIMPLE-VECTOR";

		public SimpleVectorPFunction() {
			super(CommonLispSymbols.SIMPLE_VECTOR_P, TYPE, VectorStruct.class);
		}

		@Override
		public LispStruct apply(final Arguments arguments) {
			final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
			return BooleanStruct.toLispBoolean(
					classType.isAssignableFrom(object.getClass()) && typep(CommonLispSymbols.SIMPLE_VECTOR).toJavaPBoolean()
			);
		}
	}

	public static final class StreamPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "STREAM";

		public StreamPFunction() {
			super(CommonLispSymbols.STREAMP, TYPE, StreamStruct.class);
		}
	}

	public static final class StringPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "STRING";

		public StringPFunction() {
			super(CommonLispSymbols.STRINGP, TYPE, StringStruct.class);
		}
	}

	public static final class SymbolPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "SYMBOL";

		public SymbolPFunction() {
			super(CommonLispSymbols.SYMBOLP, TYPE, SymbolStruct.class);
		}
	}

	public static final class VectorPFunction extends AbstractPredicateFunction {

		private static final String TYPE = "VECTOR";

		public VectorPFunction() {
			super(CommonLispSymbols.VECTORP, TYPE, VectorStruct.class);
		}
	}
}
